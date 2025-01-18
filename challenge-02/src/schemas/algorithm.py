# Enum for selecting algorithms
from enum import Enum

from pydantic import BaseModel

class AlgorithmType(Enum):
    TOPSIS = "TOPSIS"
    PROMETHEE_II = "PROMETHEE_II"

# Request model to update algorithm
class UpdateAlgorithmRequest(BaseModel):
    algorithm: AlgorithmType