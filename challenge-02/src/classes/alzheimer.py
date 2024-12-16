from pydantic import Field
from .base_model import BaseModelData
import numpy as np


class AlzheimerData(BaseModelData):
    family_history_alzheimers: int = Field(alias="FamilyHistoryAlzheimers")
    cardiovascular_disease: int = Field(alias="CardiovascularDisease")
    head_injury: int = Field(alias="HeadInjury")
    mmse: float = Field(alias="MMSE")
    memory_complaints: int = Field(alias="MemoryComplaints")
    behavioral_problems: int = Field(alias="BehavioralProblems")
    adl: float = Field(alias="ADL")
    confusion: int = Field(alias="Confusion")
    disorientation: int = Field(alias="Disorientation")
    personality_changes: int = Field(alias="PersonalityChanges")
    difficulty_completing_tasks: int = Field(alias="DifficultyCompletingTasks")
    forgetfulness: int = Field(alias="Forgetfulness")

    def to_numpy_array(self) -> np.ndarray:
        """
        Converts the Alzheimer data into a NumPy array, appending specific fields.
        """
        base_array = super().to_numpy_array()
        return np.hstack([
            base_array,
            np.array([
                self.family_history_alzheimers, self.cardiovascular_disease,
                self.head_injury, self.mmse, self.memory_complaints,
                self.behavioral_problems, self.adl, self.confusion,
                self.disorientation, self.personality_changes,
                self.difficulty_completing_tasks, self.forgetfulness
            ])
        ])
