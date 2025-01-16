from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy import select, insert
from src.tables.disease_predictions import disease_predictions_table
from src.tables.weights import weights_table
from src.tables.criteria import criteria_table
from src.tables.user_weights import user_weights_table
from src.tables.user import user_table


async def get_weights(auth0_id: str, db_session: AsyncSession):
    # Step 1: Fetch user ID based on auth0_id
    result = await db_session.execute(
        select(user_table.c.id).where(user_table.c.auth0_user_id == auth0_id)
    )
    user = result.scalar_one_or_none()

    if not user:
        return {"error": "User not found"}

    user_id = user

    # Step 2: Fetch disease prediction for the user
    result = await db_session.execute(
        select(disease_predictions_table.c.prediction).where(disease_predictions_table.c.user_id == user_id)
    )
    prediction = result.scalar_one_or_none()

    if not prediction:
        return {"error": "No disease prediction found for the user"}

    result = await db_session.execute(
        select(weights_table.c.id, weights_table.c.criterion_id, weights_table.c.weight)
        .where(weights_table.c.disease == prediction)
    )

    weights = result.fetchall()

    if not weights:
        return {"error": f"No weights found for the disease prediction: {prediction}"}

    # Step 4: Save weights in the `user_weights_table`
    user_weights = [
        {"user_id": user_id, "weight_id": weight[0]} for weight in weights
    ]
    await db_session.execute(insert(user_weights_table), user_weights)
    await db_session.commit()

    # Step 5: Return the weights along with their criteria names
    criteria_ids = [weight[1] for weight in weights]
    result = await db_session.execute(
        select(criteria_table.c.id, criteria_table.c.name).where(criteria_table.c.id.in_(criteria_ids))
    )
    criteria = {row[0]: row[1] for row in result.fetchall()}

    # Combine weights with criteria names
    weights_with_criteria = [
        {"criterion": criteria[weight[1]], "weight": weight[2]} for weight in weights
    ]

    return {
        "prediction": prediction,
        "weights": weights_with_criteria,
    }