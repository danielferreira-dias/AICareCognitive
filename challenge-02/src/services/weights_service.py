from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy import select, insert, update

from schemas.weights import WeightUpdateRequest
from tables.disease_predictions import disease_predictions_table
from tables.weights import weights_table
from tables.criteria import criteria_table
from tables.user_weights import user_weights_table
from tables.user import user_table


async def get_weights(auth0_id: str, db_session: AsyncSession):
    # Step 1: Fetch user ID based on auth0_id
    result = await db_session.execute(
        select(user_table.c.id).where(user_table.c.auth0_user_id == auth0_id)
    )
    user = result.scalar_one_or_none()

    if not user:
        return {"error": "User not found"}

    user_id = user

    # Step 2: Check if there are user-specific weights in `user_weights_table`
    result = await db_session.execute(
        select(user_weights_table.c.criterion_id, user_weights_table.c.weight)
        .where(user_weights_table.c.user_id == user_id)
    )
    user_weights = result.fetchall()

    if not user_weights:
        # Step 3: If no user-specific weights are found, fetch disease prediction
        result = await db_session.execute(
            select(disease_predictions_table.c.prediction).where(disease_predictions_table.c.user_id == user_id)
        )
        prediction = result.scalar_one_or_none()

        if not prediction:
            return {"error": "No disease prediction found for the user"}

        # Step 4: Fetch default weights for the disease from `weights_table`
        result = await db_session.execute(
            select(weights_table.c.criterion_id, weights_table.c.weight)
            .where(weights_table.c.disease == prediction)
        )
        default_weights = result.fetchall()

        if not default_weights:
            return {"error": f"No default weights found for the disease: {prediction}"}

        # Step 5: Initialize user-specific weights with default values
        new_user_weights = [
            {"user_id": user_id, "criterion_id": weight[0], "weight": weight[1]}
            for weight in default_weights
        ]
        await db_session.execute(insert(user_weights_table).values(new_user_weights))
        await db_session.commit()

        # Use the default weights for the final return
        user_weights = default_weights

    # Step 6: Fetch criteria names for the weights
    criteria_ids = [weight[0] for weight in user_weights]
    result = await db_session.execute(
        select(criteria_table.c.id, criteria_table.c.name).where(criteria_table.c.id.in_(criteria_ids))
    )
    criteria = {row[0]: row[1] for row in result.fetchall()}

    # Combine weights with criteria names
    weights_with_criteria = [
        {"criterion_id": weight[0], "criterion": criteria[weight[0]], "weight": weight[1]}
        for weight in user_weights
    ]

    # Step 7: Return prediction and user-specific weights
    return {
        "weights": weights_with_criteria,
    }

async def update_weights(auth0_id: str, weights: list[WeightUpdateRequest], db_session: AsyncSession):
    """
    Update a batch of weights for a user.

    Parameters:
    - auth0_id: The Auth0 user ID of the user.
    - weights: List of weights to update, each containing:
        - criterion_id: The ID of the criterion.
        - new_weight: The new weight value (between 0 and 1).
    - db_session: The active database session.
    """
    # Step 1: Fetch user ID based on auth0_id
    result = await db_session.execute(
        select(user_table.c.id).where(user_table.c.auth0_user_id == auth0_id)
    )
    user = result.scalar_one_or_none()

    if not user:
        return {"error": "User not found"}

    user_id = user

    # Step 2: Iterate through the weights list and update the user-specific weights
    for weight_data in weights:
        criterion_id = weight_data.criterion_id  # Access as attribute
        new_weight = weight_data.new_weight  # Access as attribute

        if not criterion_id or new_weight is None:
            return {"error": "Invalid weight data in input list"}

        # Step 3: Check that the criterion exists in the user_weights_table for the user
        result = await db_session.execute(
            select(user_weights_table.c.id)
            .where(
                user_weights_table.c.user_id == user_id,
                user_weights_table.c.criterion_id == criterion_id
            )
        )
        user_weight_record = result.scalar_one_or_none()

        if not user_weight_record:
            return {"error": f"Criterion {criterion_id} not found for the user"}

        # Step 4: Update the weight in the user_weights_table
        await db_session.execute(
            update(user_weights_table)
            .where(
                user_weights_table.c.user_id == user_id,
                user_weights_table.c.criterion_id == criterion_id
            )
            .values(weight=new_weight)
        )

    # Step 5: Commit all updates
    await db_session.commit()

    return {"message": f"Successfully updated {len(weights)} weights"}