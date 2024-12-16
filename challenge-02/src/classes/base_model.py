from pydantic import BaseModel, Field
import numpy as np


class BaseModelData(BaseModel):
    age: int = Field(alias="Age", description="Age of the patient")
    gender: int = Field(alias="Gender", description="Gender of the patient (e.g., 0=Male, 1=Female)")
    ethnicity: int = Field(alias="Ethnicity", description="Ethnicity of the patient")
    bmi: float = Field(alias="BMI", description="Body Mass Index of the patient")
    smoking: int = Field(alias="Smoking", description="Smoking status (e.g., 0=Non-smoker, 1=Smoker)")
    alcohol_consumption: float = Field(alias="AlcoholConsumption", description="Alcohol consumption frequency")
    physical_activity: float = Field(alias="PhysicalActivity", description="Physical activity level")
    diet_quality: float = Field(alias="DietQuality", description="Diet quality score")
    sleep_quality: float = Field(alias="SleepQuality", description="Sleep quality score")
    hypertension: int = Field(alias="Hypertension", description="Presence of hypertension (0 or 1)")
    diabetes: int = Field(alias="Diabetes", description="Presence of diabetes (0 or 1)")
    depression: int = Field(alias="Depression", description="Presence of depression (0 or 1)")
    systolic_bp: int = Field(alias="SystolicBP", description="Systolic blood pressure")
    diastolic_bp: int = Field(alias="DiastolicBP", description="Diastolic blood pressure")
    cholesterol_total: float = Field(alias="CholesterolTotal", description="Total cholesterol level")
    cholesterol_ldl: float = Field(alias="CholesterolLDL", description="LDL cholesterol level")
    cholesterol_hdl: float = Field(alias="CholesterolHDL", description="HDL cholesterol level")
    cholesterol_triglycerides: float = Field(alias="CholesterolTriglycerides",
                                             description="Triglycerides cholesterol level")
    functional_assessment: float = Field(alias="FunctionalAssessment", description="Functional assessment score")

    class Config:
        populate_by_name = True

    def to_numpy_array(self) -> np.ndarray:
        """
        Converts the base model fields into a NumPy array.
        """
        return np.array([
            self.age, self.gender, self.ethnicity, self.bmi, self.smoking,
            self.alcohol_consumption, self.physical_activity, self.diet_quality,
            self.sleep_quality, self.hypertension, self.diabetes, self.depression,
            self.systolic_bp, self.diastolic_bp, self.cholesterol_total,
            self.cholesterol_ldl, self.cholesterol_hdl, self.cholesterol_triglycerides,
            self.functional_assessment,
        ])
