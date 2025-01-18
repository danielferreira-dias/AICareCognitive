from pydantic import BaseModel, confloat, conlist


# Request model for updating weights
class WeightUpdateRequest(BaseModel):
    criterion_id: int
    new_weight: confloat(ge=0, le=1)  # Weight between 0 and 1


# Request schema for the endpoint
class UpdateWeightsRequest(BaseModel):
    weights: conlist(WeightUpdateRequest)