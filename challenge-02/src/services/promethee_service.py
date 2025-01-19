from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy import select, insert, update
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
    activity_ids = [activity[0] for activity in activities]
    criterion_ids = [criterion[0] for criterion in criteria]

    decision_matrix_np = np.zeros((len(activity_ids), len(criterion_ids)))

    activity_id_to_index = {activity_id: idx for idx, activity_id in enumerate(activity_ids)}
    criterion_id_to_index = {criterion_id: idx for idx, criterion_id in enumerate(criterion_ids)}

    for row in decision_matrix:
        activity_idx = activity_id_to_index[row[0]]
        criterion_idx = criterion_id_to_index[row[1]]
        decision_matrix_np[activity_idx][criterion_idx] = row[2]

    # PROMETHEE II Implementation
    # Step 7: Normalize decision matrix
    min_vals = decision_matrix_np.min(axis=0)
    max_vals = decision_matrix_np.max(axis=0)

    # Prevent division by zero
    ranges = np.where(max_vals == min_vals, 1, max_vals - min_vals)
    normalized_matrix = (decision_matrix_np - min_vals) / ranges

    # Fetch user weight vector for criteria
    user_weight_vector = np.array([user_weights.get(c_id, 0) for c_id in criterion_ids])

    # Step 8: Apply user weights
    weighted_matrix = normalized_matrix * user_weight_vector

    # Step 9: Calculate pairwise preference indices for each pair of activities
    def preference_function(value1, value2):
        return max(0, value1 - value2)  # Example: linear preference function

    n_activities = len(activity_ids)
    preference_matrix = np.zeros((n_activities, n_activities))

    for i in range(n_activities):
        for j in range(n_activities):
            if i != j:
                preference_indices = [
                    preference_function(weighted_matrix[i, k], weighted_matrix[j, k])
                    for k in range(weighted_matrix.shape[1])
                ]
                preference_matrix[i, j] = np.dot(preference_indices, user_weight_vector)

    # Step 10: Calculate net flows (leaving flow - entering flow)
    leaving_flows = preference_matrix.sum(axis=1) / (n_activities - 1)
    entering_flows = preference_matrix.sum(axis=0) / (n_activities - 1)
    net_flows = leaving_flows - entering_flows

    # Step 11: Rank activities based on net flows
    ranked_activities = sorted(zip(activity_ids, net_flows), key=lambda x: x[1], reverse=True)

    # Map activity IDs to their names
    activity_id_to_name = {activity[0]: activity[1] for activity in activities}
    ranked_activities_named = [
        {"activity": activity_id_to_name[activity_id], "score": score}
        for activity_id, score in ranked_activities
    ]

    # Step 12: Save or Update results in the `results_table`
    for activity_id, score in ranked_activities:
        # Check if the result exists for the user and activity
        existing_result = await db_session.execute(
            select(results_table.c.id)
            .where(results_table.c.user_id == user_id)
            .where(results_table.c.activity_id == activity_id)
        )
        existing_result_id = existing_result.scalar_one_or_none()

        if existing_result_id:
            # Update the existing record
            await db_session.execute(
                results_table.update()
                .where(results_table.c.id == existing_result_id)
                .values(score=score)
            )
        else:
            # Insert a new record
            await db_session.execute(
                insert(results_table).values(
                    {"user_id": user_id, "activity_id": activity_id, "score": score}
                )
            )

    # Commit the changes
    await db_session.commit()

    # Step 13: Return ranked activities
    return ranked_activities_named