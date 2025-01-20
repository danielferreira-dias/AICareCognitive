from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy import select, insert
import numpy as np
from fastapi import HTTPException
from tables.activities import activities_table
from tables.criteria import criteria_table
from tables.weights import weights_table
from tables.decision_matrix import decision_matrix_table
from tables.user import user_table
from tables.user_weights import user_weights_table
from tables.results import results_table


async def get_results(auth0_id: str, db_session: AsyncSession):
    # Step 1: Get user ID based on auth0_id
    user_result = await db_session.execute(
        select(user_table.c.id).where(user_table.c.auth0_user_id == auth0_id)
    )
    user_id = user_result.scalar_one_or_none()
    if not user_id:
        raise HTTPException(status_code=404, detail=f"User not found with auth0 ID: {auth0_id}")

    # Step 2: Fetch user-specific weights
    user_weights_result = await db_session.execute(
        select(user_weights_table.c.criterion_id, user_weights_table.c.weight)
        .where(user_weights_table.c.user_id == user_id)
    )
    user_weights = {criterion_id: weight for criterion_id, weight in user_weights_result.fetchall()}

    # Step 3: Fetch all activities
    activities_result = await db_session.execute(select(activities_table))
    activities = activities_result.fetchall()
    if not activities:
        raise HTTPException(status_code=404, detail="No activities found")

    # Step 4: Fetch all criteria
    criteria_result = await db_session.execute(select(criteria_table))
    criteria = criteria_result.fetchall()
    if not criteria:
        raise HTTPException(status_code=404, detail="No criteria found")

    # Step 5: Fetch the decision matrix scores
    decision_matrix_result = await db_session.execute(
        select(
            decision_matrix_table.c.activity_id,
            decision_matrix_table.c.criterion_id,
            decision_matrix_table.c.score,
        )
    )
    decision_matrix = decision_matrix_result.fetchall()
    if not decision_matrix:
        raise HTTPException(status_code=404, detail="No decision matrix data found")

    # Step 6: Construct the decision matrix
    # Rows: Activities, Columns: Criteria
    activity_ids = [activity[0] for activity in activities]
    criterion_ids = [criterion[0] for criterion in criteria]

    decision_matrix_np = np.zeros((len(activity_ids), len(criterion_ids)))

    activity_id_to_index = {activity_id: idx for idx, activity_id in enumerate(activity_ids)}
    criterion_id_to_index = {criterion_id: idx for idx, criterion_id in enumerate(criterion_ids)}

    for row in decision_matrix:
        activity_idx = activity_id_to_index[row[0]]
        criterion_idx = criterion_id_to_index[row[1]]
        decision_matrix_np[activity_idx][criterion_idx] = row[2]

    # Step 7: Find weights according to disease
    user_weight_vector = np.array([user_weights.get(c_id, 0) for c_id in criterion_ids])

    # Step 8: Define indices for maximizing and minimizing
    indices_max = [0, 1, 2, 3, 4, 9, 10, 11]  # Columns to MAXIMIZE
    indices_min = [5, 6, 7, 8, 12]  # Columns to MINIMIZE

    #  Step 9: Calculate the ideal best (positive) and worst (negative) solutions
    ideal_positive = []
    ideal_negative = []

    #################################################
    #                TOPSIS METHOD
    ################################################

    # STEP 1: Normalize the decision matrix
    normalized_matrix = decision_matrix_np / np.sqrt((decision_matrix_np ** 2).sum(axis=0))

    # STEP 2: Weight to normalized matrix
    weighted_matrix = normalized_matrix * user_weight_vector

    # STEP 3: Determine the ideal positive and the ideal negative options
    # Atribuir os máximos e mínimos de acordo com os índices
    for j in range(weighted_matrix.shape[1]):
        if j in indices_max:  # Maximize
            ideal_positive.append(weighted_matrix[:, j].max())
            ideal_negative.append(weighted_matrix[:, j].min())
        elif j in indices_min:  # Minimize
            ideal_positive.append(weighted_matrix[:, j].min())
            ideal_negative.append(weighted_matrix[:, j].max())

    ideal_positive = np.array(ideal_positive)
    ideal_negative = np.array(ideal_negative)

    # STEP 4: Calculate the distances of each alternative from negative ideal and positive solution (Eucledian distance)
    distances_to_positive = np.sqrt(((weighted_matrix - ideal_positive) ** 2).sum(axis=1))
    distances_to_negative = np.sqrt(((weighted_matrix - ideal_negative) ** 2).sum(axis=1))

    # STEP 5: Calculate the relative distance / TOPSIS scores
    topsis_scores = distances_to_negative / (distances_to_positive + distances_to_negative)

    # STEP 6: Rank the activities based on the TOPSIS scores
    ranked_activities = sorted(
        zip(activity_ids, topsis_scores), key=lambda x: x[1], reverse=True
    )

    # -END TOPSIS METHOD

    # Map activity IDs to their names
    activity_id_to_name = {activity[0]: activity[1] for activity in activities}
    ranked_activities_named = [
        {"activity": activity_id_to_name[activity_id], "score": score}
        for activity_id, score in ranked_activities
    ]

    # Step 13: Save or Update Results in the `results_table`
    insert_data = [
        {"user_id": user_id, "activity_id": activity_id, "score": score}
        for activity_id, score in ranked_activities
    ]

    for data in insert_data:
        # Check if a result already exists for this user and activity
        existing_result = await db_session.execute(
            select(results_table.c.id)
            .where(results_table.c.user_id == data["user_id"])
            .where(results_table.c.activity_id == data["activity_id"])
        )
        existing_result_id = existing_result.scalar_one_or_none()

        if existing_result_id:
            # Update the existing row with the new score
            await db_session.execute(
                results_table.update()
                .where(results_table.c.id == existing_result_id)
                .values(score=data["score"])
            )
        else:
            # Insert a new row
            await db_session.execute(insert(results_table).values(data))

    # Commit all the database changes
    await db_session.commit()

    # Step 14: Return the ranked activities
    return ranked_activities_named