from fastapi import FastAPI
from contextlib import asynccontextmanager
import joblib
import uvicorn
from database import database, engine, metadata
from tables.user import user_table
from tables.criteria import criteria_table
from tables.activities import activities_table
from tables.results import results_table
from tables.decision_matrix import decision_matrix_table
from tables.disease_predictions import disease_predictions_table
from tables.weights import weights_table
from tables.user_weights import user_weights_table
import services.inference_service as inference_service
from classes.request import RequestData

# Load models
models = {
    "parkinson": joblib.load('models/parkinson_model.joblib'),
    "alzheimer": joblib.load('models/alzheimer_model.joblib')
}

@asynccontextmanager
async def lifespan(app: FastAPI):
    # Connect to the database
    await database.connect()

    # Create tables (sync operation in async context)
    metadata.create_all(engine)

    yield  # Application runs here

    # Disconnect from the database
    await database.disconnect()


# Add lifespan to the app
app = FastAPI(lifespan=lifespan)


@app.get("/health")
def read_root():
    return {"status": "Server is up and running!"}


@app.post("/predict")
def predict(data: RequestData):
    return inference_service.process(data, models)

@app.get("/weights")
def get_weights():
    return weights_table.select().execute().fetchall()


if __name__ == "__main__":
    uvicorn.run(app, host="127.0.0.1", port=8000)
