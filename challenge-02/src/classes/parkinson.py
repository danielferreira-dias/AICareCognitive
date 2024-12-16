from pydantic import Field
from .base_model import BaseModelData
import numpy as np


class ParkinsonData(BaseModelData):
    family_history_parkinsons: int = Field(
        alias="FamilyHistoryParkinsons", description="Family history of Parkinson's disease (0 or 1)"
    )
    traumatic_brain_injury: int = Field(
        alias="TraumaticBrainInjury", description="History of traumatic brain injury (0 or 1)"
    )
    stroke: int = Field(alias="Stroke", description="History of stroke (0 or 1)")
    updrs: float = Field(alias="UPDRS", description="Unified Parkinson's Disease Rating Scale score")
    moca: float = Field(alias="MoCA", description="Montreal Cognitive Assessment score")
    tremor: int = Field(alias="Tremor", description="Presence of tremor (0 or 1)")
    rigidity: int = Field(alias="Rigidity", description="Presence of rigidity (0 or 1)")
    bradykinesia: int = Field(alias="Bradykinesia", description="Presence of bradykinesia (0 or 1)")
    postural_instability: int = Field(alias="PosturalInstability",
                                      description="Presence of postural instability (0 or 1)")
    speech_problems: int = Field(alias="SpeechProblems", description="Speech problems status (0 or 1)")
    sleep_disorders: int = Field(alias="SleepDisorders", description="Presence of sleep disorders (0 or 1)")
    constipation: int = Field(alias="Constipation", description="Presence of constipation (0 or 1)")

    def to_numpy_array(self) -> np.ndarray:
        """
        Converts the Parkinson data into a NumPy array, extending the base fields.
        """
        base_array = super().to_numpy_array()
        return np.hstack([
            base_array,
            np.array([
                self.family_history_parkinsons, self.traumatic_brain_injury,
                self.stroke, self.updrs, self.moca, self.tremor,
                self.rigidity, self.bradykinesia, self.postural_instability,
                self.speech_problems, self.sleep_disorders, self.constipation
            ])
        ])
