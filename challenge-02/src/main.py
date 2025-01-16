from typing import Optional
from fastapi import FastAPI, Depends, HTTPException
from contextlib import asynccontextmanager
import joblib
import uvicorn
from pydantic import BaseModel
from sqlalchemy.ext.asyncio import AsyncSession
from enum import Enum

from database import database, engine, metadata, get_db_session
from src.security import extract_sub_from_token
from src.services import promethee_service, topsis_service
from src.services.weights_service import get_weights
from services.inference_service import process as inference_process
from classes.request import RequestData


# Enum for selecting algorithms
class AlgorithmType(Enum):
    TOPSIS = "TOPSIS"
    PROMETHEE_II = "PROMETHEE_II"


# Request model to update algorithm
class UpdateAlgorithmRequest(BaseModel):
    algorithm: AlgorithmType


# In-memory store for the selected algorithm
class AlgorithmStore:
    _algorithm: Optional[AlgorithmType] = AlgorithmType.TOPSIS

    @classmethod
    def get_algorithm(cls) -> AlgorithmType:
        return cls._algorithm

    @classmethod
    def set_algorithm(cls, algorithm: AlgorithmType):
        cls._algorithm = algorithm


# Load pre-trained models
models = {
    "parkinson": joblib.load("models/parkinson_model.joblib"),
    "alzheimer": joblib.load("models/alzheimer_model.joblib"),
}


# Application lifespan setup
@asynccontextmanager
async def lifespan(app: FastAPI):
    await database.connect()
    async with engine.begin() as conn:
        await conn.run_sync(metadata.create_all)
    yield
    await database.disconnect()


# Initialize the FastAPI app
app = FastAPI(lifespan=lifespan)


@app.get("/health")
def health_check():
    """Health check endpoint."""
    return {"status": "Server is up and running!"}


@app.post("/predict")
async def predict(
        data: RequestData,
        auth0_id: str = Depends(extract_sub_from_token),
        db_session: AsyncSession = Depends(get_db_session),
):
    """Process inference using pre-trained models."""
    return await inference_process(data, models, auth0_id, db_session)


@app.get("/weights")
async def get_weights_endpoint(
        auth0_id: str = Depends(extract_sub_from_token),
        db_session: AsyncSession = Depends(get_db_session),
):
    """Fetch user-specific weights."""
    return await get_weights(auth0_id, db_session)


@app.get("/results")
async def get_results(
        auth0_id: str = Depends(extract_sub_from_token),
        db_session: AsyncSession = Depends(get_db_session),
):
    """Fetch activity results based on the selected algorithm."""
    current_algorithm = AlgorithmStore.get_algorithm()

    if current_algorithm == AlgorithmType.TOPSIS:
        return await topsis_service.get_results(auth0_id, db_session)
    elif current_algorithm == AlgorithmType.PROMETHEE_II:
        return await promethee_service.get_results(auth0_id, db_session)
    else:
        raise HTTPException(status_code=500, detail="Invalid algorithm type selected.")


@app.put("/algorithm")
async def change_algorithm(request: UpdateAlgorithmRequest):
    """Change the algorithm being used."""
    AlgorithmStore.set_algorithm(request.algorithm)
    return {"message": f"Algorithm successfully changed to {request.algorithm.value}"}


@app.get("/secure-data")
def secure_data(auth0_id: str = Depends(extract_sub_from_token)):
    """Fetch secure data for the authenticated user."""
    return {"message": "This is secure data!", "user_id": auth0_id}


# Run the application
if __name__ == "__main__":
    uvicorn.run(app, host="127.0.0.1", port=8000)