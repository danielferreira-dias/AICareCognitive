from fastapi import FastAPI
import joblib

from classes.alzheimer import AlzheimerData
from classes.parkinson import ParkinsonData
import uvicorn
import services.inference_service as inference_service
from concurrent.futures import ThreadPoolExecutor

from classes.request import RequestData

models = {
    "parkinson": joblib.load('models/parkinson_model.joblib'),
    "alzheimer": joblib.load('models/alzheimer_model.joblib')
}
print(type(models))  # Example output: <class 'sklearn.ensemble._forest.RandomForestClassifier'>


app = FastAPI()

# Define the root endpoint
@app.get("/health")
def read_root():
    return {"status": "Server is up and running!"}


@app.post("/predict")
def predict(data: RequestData):
    return inference_service.process(data, models)


if __name__ == "__main__":
    uvicorn.run(app, host="127.0.0.1", port=8000)