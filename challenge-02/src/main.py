from fastapi import FastAPI
import joblib

import uvicorn
from fastapi.middleware.cors import CORSMiddleware

import services.inference_service as inference_service

from classes.request import RequestData

models = {
    "parkinson": joblib.load('models/parkinson_model.joblib'),
    "alzheimer": joblib.load('models/alzheimer_model.joblib')
}

app = FastAPI()

app.add_middleware(
    CORSMiddleware,
    allow_origins=["http://localhost:4200"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Define the root endpoint
@app.get("/health")
def read_root():
    return {"status": "Server is up and running!"}


@app.post("/predict")
def predict(data: RequestData):
    return inference_service.process(data, models)


if __name__ == "__main__":
    uvicorn.run(app, host="127.0.0.1", port=8000)