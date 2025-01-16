from sqlalchemy import Table, Column, Integer, Float, Boolean, String
from database import metadata

# Define the user table
user_table = Table(
    "users",  # Table name
    metadata,
    # BaseModelData fields
    Column("id", Integer, primary_key=True),
    Column("auth0_user_id", String, unique=True, nullable=False),
    Column("age", Integer, nullable=False),  # Age of the patient
    Column("gender", Integer, nullable=False),  # Gender (0=Male, 1=Female)
    Column("ethnicity", Integer, nullable=False),  # Ethnicity
    Column("bmi", Float, nullable=False),  # Body Mass Index
    Column("smoking", Integer, nullable=False),  # Smoking status (0=Non-smoker, 1=Smoker)
    Column("alcohol_consumption", Float, nullable=False),  # Alcohol consumption frequency
    Column("physical_activity", Float, nullable=False),  # Physical activity level
    Column("diet_quality", Float, nullable=False),  # Diet quality score
    Column("sleep_quality", Float, nullable=False),  # Sleep quality score
    Column("hypertension", Integer, nullable=False),  # Hypertension (0 or 1)
    Column("diabetes", Integer, nullable=False),  # Diabetes (0 or 1)
    Column("depression", Integer, nullable=False),  # Depression (0 or 1)
    Column("systolic_bp", Integer, nullable=False),  # Systolic blood pressure
    Column("diastolic_bp", Integer, nullable=False),  # Diastolic blood pressure
    Column("cholesterol_total", Float, nullable=False),  # Total cholesterol
    Column("cholesterol_ldl", Float, nullable=False),  # LDL cholesterol
    Column("cholesterol_hdl", Float, nullable=False),  # HDL cholesterol
    Column("cholesterol_triglycerides", Float, nullable=False),  # Triglycerides cholesterol
    Column("functional_assessment", Float, nullable=False),  # Functional assessment score

    # AlzheimerData fields
    Column("family_history_alzheimers", Integer, nullable=True),  # Family history of Alzheimer's
    Column("cardiovascular_disease", Integer, nullable=True),  # Cardiovascular disease
    Column("head_injury", Integer, nullable=True),  # History of head injury
    Column("mmse", Float, nullable=True),  # Mini-Mental State Examination (MMSE) score
    Column("memory_complaints", Integer, nullable=True),  # Memory complaints (0 or 1)
    Column("behavioral_problems", Integer, nullable=True),  # Behavioral problems (0 or 1)
    Column("adl", Float, nullable=True),  # Activities of Daily Living (ADL) score
    Column("confusion", Integer, nullable=True),  # Presence of confusion (0 or 1)
    Column("disorientation", Integer, nullable=True),  # Presence of disorientation (0 or 1)
    Column("personality_changes", Integer, nullable=True),  # Personality changes (0 or 1)
    Column("difficulty_completing_tasks", Integer, nullable=True),  # Task completion difficulty (0 or 1)
    Column("forgetfulness", Integer, nullable=True),  # Forgetfulness (0 or 1)

    # ParkinsonData fields
    Column("family_history_parkinsons", Integer, nullable=True),  # Family history of Parkinson's (0 or 1)
    Column("traumatic_brain_injury", Integer, nullable=True),  # History of traumatic brain injury (0 or 1)
    Column("stroke", Integer, nullable=True),  # History of stroke (0 or 1)
    Column("updrs", Float, nullable=True),  # Unified Parkinson's Disease Rating Scale (UPDRS) score
    Column("moca", Float, nullable=True),  # Montreal Cognitive Assessment (MoCA) score
    Column("tremor", Integer, nullable=True),  # Presence of tremor (0 or 1)
    Column("rigidity", Integer, nullable=True),  # Presence of rigidity (0 or 1)
    Column("bradykinesia", Integer, nullable=True),  # Presence of bradykinesia (0 or 1)
    Column("postural_instability", Integer, nullable=True),  # Postural instability (0 or 1)
    Column("speech_problems", Integer, nullable=True),  # Speech problems (0 or 1)
    Column("sleep_disorders", Integer, nullable=True),  # Sleep disorders (0 or 1)
    Column("constipation", Integer, nullable=True),  # Constipation (0 or 1)xx\
    extend_existing=True # Parkinson's flag (default is NULL)
)