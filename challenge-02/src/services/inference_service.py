from concurrent.futures import ThreadPoolExecutor
from typing import Any, Dict, Tuple

from sklearn.ensemble import RandomForestClassifier

from classes.alzheimer import AlzheimerData
from classes.base_model import BaseModelData
from classes.parkinson import ParkinsonData
from classes.request import RequestData


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

def process(request: RequestData, models: Dict[str, RandomForestClassifier]):
    """
    Processes a prediction request by running inference on multiple machine learning models in parallel.

    <p>
    This function maps the incoming data from the RequestData object into domain-specific data objects
    (e.g., ParkinsonData and AlzheimerData). It then uses a ThreadPoolExecutor to perform inference
    on both models (Parkinson and Alzheimer) concurrently.
    </p>

    @param request A RequestData object containing the input data for the models.
    @param models A dictionary that maps model names (e.g., "parkinson", "alzheimer") to trained machine learning models.
    @return A dictionary containing the prediction results:
            - "parkinson_model": Prediction results from the Parkinson model.
            - "alzheimer_model": Prediction results from the Alzheimer model.
    """
    parkinson_data, alzheimer_data = map_to_models(request)

    with ThreadPoolExecutor() as executor:
        parkinson_result = executor.submit(inference, parkinson_data, models["parkinson"])
        alzheimer_result = executor.submit(inference, alzheimer_data, models["parkinson"])

    return {
        "parkinson_model": parkinson_result.result(),
        "alzheimer_model": alzheimer_result.result(),
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

