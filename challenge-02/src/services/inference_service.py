from concurrent.futures import ThreadPoolExecutor
from sqlite3 import IntegrityError
from typing import Any, Dict, Tuple

from sklearn.ensemble import RandomForestClassifier

from classes.alzheimer import AlzheimerData
from classes.base_model import BaseModelData
from classes.parkinson import ParkinsonData
from classes.request import RequestData
from sqlalchemy import insert, select
from sqlalchemy.ext.asyncio import AsyncSession

from tables.disease_predictions import disease_predictions_table
from tables.user import user_table


def inference(data: BaseModelData, model: Any) -> Dict[str, Any]:
    """
    Performs model inference on the provided input data.

    <p>
    This function preprocesses the input data into a format suitable for the model,
    performs the prediction using the specified trained machine learning model,
    and returns the prediction result.
    </p>

    @param data An instance of a class derived from BaseModelData, containing the input features.
    @param model A trained machine learning model (e.g., RandomForestClassifier) used for prediction.
    @return A dictionary with the prediction results, where the key "prediction" maps to a list of predicted values.
    """

    input_data = data.to_numpy_array().reshape(1, -1)

    # Make predictions using the model
    prediction = model.predict(input_data)

    return {"prediction": int(prediction[0])}

async def process(request: RequestData, models: Dict[str, RandomForestClassifier], auth0_id: str,
                  db_session: AsyncSession):
    """
    Processes a prediction request by running inference on multiple machine learning models in parallel.
    It saves the user's input data to the `users` table and prediction results to the `disease_predictions` table.
    """
    # Step 1: Extract Parkinson and Alzheimer data from the request
    parkinson_data, alzheimer_data = map_to_models(request)

    # Step 2: Perform predictions in parallel
    with ThreadPoolExecutor() as executor:
        parkinson_result = executor.submit(inference, parkinson_data, models["parkinson"])
        alzheimer_result = executor.submit(inference, alzheimer_data, models["alzheimer"])

    parkinson_prediction = parkinson_result.result()["prediction"]
    alzheimer_prediction = alzheimer_result.result()["prediction"]

    # Determine prediction overview (e.g., Alzheimer's, Parkinson's, Both, or None)
    if alzheimer_prediction == 1 and parkinson_prediction == 1:
        disease_prediction = "Both"
    elif alzheimer_prediction == 1:
        disease_prediction = "Alzheimer"
    elif parkinson_prediction == 1:
        disease_prediction = "Parkinson"
    else:
        disease_prediction = "None"

    # Step 3: Save or Update User Data
    user_data = request.model_dump()  # Serialize RequestData into a dictionary
    user_data["auth0_user_id"] = auth0_id  # Add the Auth0 user ID to the data

    # Check if the user already exists
    existing_user_stmt = select(user_table.c.id).where(user_table.c.auth0_user_id == auth0_id)
    result = await db_session.execute(existing_user_stmt)
    user_id = result.scalar()

    if user_id:
        # If the user exists, update their data using `update` statement
        user_update_stmt = (
            user_table.update().
            where(user_table.c.id == user_id).
            values(**user_data)
        )
        await db_session.execute(user_update_stmt)
    else:
        # If the user does not exist, insert new user data into the `users` table
        user_insert_stmt = insert(user_table).values(**user_data).returning(user_table.c.id)
        result = await db_session.execute(user_insert_stmt)
        user_id = result.scalar()  # Fetch the generated ID of the user

    # Step 4: Save Predictions in disease_predictions_table
    existing_prediction = await db_session.execute(
        select(disease_predictions_table.c.id).where(disease_predictions_table.c.user_id == user_id)
    )
    existing_prediction_id = existing_prediction.scalar_one_or_none()

    if existing_prediction_id:
        # Update the existing prediction
        await db_session.execute(
            disease_predictions_table.update()
            .where(disease_predictions_table.c.id == existing_prediction_id)
            .values(prediction=disease_prediction)
        )
    else:
        # Insert a new prediction
        prediction_data = {
            "user_id": user_id,
            "prediction": disease_prediction
        }
        prediction_insert_stmt = insert(disease_predictions_table).values(**prediction_data)
        await db_session.execute(prediction_insert_stmt)

    # Commit both inserts/updates to persist them in the database
    await db_session.commit()

    # Step 5: Return the predictions without database fields
    return {
        "parkinson_model": parkinson_result.result(),
        "alzheimer_model": alzheimer_result.result(),
        "overall_prediction": disease_prediction
    }

def map_to_models(request_data: RequestData) -> Tuple[ParkinsonData, AlzheimerData]:
    """
    Converts raw input data into domain-specific data objects for model inference.

    <p>
    This function takes the incoming RequestData object and maps it into
    ParkinsonData and AlzheimerData classes for use in their respective models.
    </p>

    @param request_data A RequestData object containing the input data.
    @return A tuple containing:
            - ParkinsonData: The deserialized data for the Parkinson model.
            - AlzheimerData: The deserialized data for the Alzheimer model.
    """
    parkinson_data = ParkinsonData(**request_data.model_dump())
    alzheimer_data = AlzheimerData(**request_data.model_dump())

    return parkinson_data, alzheimer_data

