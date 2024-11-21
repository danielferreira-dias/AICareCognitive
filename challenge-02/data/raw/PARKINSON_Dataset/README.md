# Patient Information Dataset

This dataset contains detailed information about patients, including their demographic details, lifestyle factors, medical history, clinical measurements, cognitive and functional assessments, symptoms, and diagnosis information related to Parkinson's Disease. Below is a detailed description of the dataset's structure and attributes.

---

## Table of Contents
1. [Patient Information](#patient-information)
    - [Patient ID](#patient-id)
2. [Demographic Details](#demographic-details)
3. [Lifestyle Factors](#lifestyle-factors)
4. [Medical History](#medical-history)
5. [Clinical Measurements](#clinical-measurements)
6. [Cognitive and Functional Assessments](#cognitive-and-functional-assessments)
7. [Symptoms](#symptoms)
8. [Diagnosis Information](#diagnosis-information)
9. [Confidential Information](#confidential-information)

---

## Patient Information

### Patient ID
- **PatientID**: A unique identifier assigned to each patient. Values range from **3058 to 5162**.

---

## Demographic Details
- **Age**: Age of the patients, ranging from **50 to 90 years**.
- **Gender**: Gender of the patients:
  - `0`: Male
  - `1`: Female
- **Ethnicity**: Ethnicity of the patients, coded as follows:
  - `0`: Caucasian
  - `1`: African American
  - `2`: Asian
  - `3`: Other
- **EducationLevel**: Education level of the patients, coded as follows:
  - `0`: None
  - `1`: High School
  - `2`: Bachelor's
  - `3`: Higher

---

## Lifestyle Factors
- **BMI**: Body Mass Index of the patients, ranging from **15 to 40**.
- **Smoking**: Smoking status:
  - `0`: No
  - `1`: Yes
- **AlcoholConsumption**: Weekly alcohol consumption in units, ranging from **0 to 20**.
- **PhysicalActivity**: Weekly physical activity in hours, ranging from **0 to 10**.
- **DietQuality**: Diet quality score, ranging from **0 to 10**.
- **SleepQuality**: Sleep quality score, ranging from **4 to 10**.

---

## Medical History
- **FamilyHistoryParkinsons**: Family history of Parkinson's Disease:
  - `0`: No
  - `1`: Yes
- **TraumaticBrainInjury**: History of traumatic brain injury:
  - `0`: No
  - `1`: Yes
- **Hypertension**: Presence of hypertension:
  - `0`: No
  - `1`: Yes
- **Diabetes**: Presence of diabetes:
  - `0`: No
  - `1`: Yes
- **Depression**: Presence of depression:
  - `0`: No
  - `1`: Yes
- **Stroke**: History of stroke:
  - `0`: No
  - `1`: Yes

---

## Clinical Measurements
- **SystolicBP**: Systolic blood pressure, ranging from **90 to 180 mmHg**.
- **DiastolicBP**: Diastolic blood pressure, ranging from **60 to 120 mmHg**.
- **CholesterolTotal**: Total cholesterol levels, ranging from **150 to 300 mg/dL**.
- **CholesterolLDL**: Low-density lipoprotein cholesterol levels, ranging from **50 to 200 mg/dL**.
- **CholesterolHDL**: High-density lipoprotein cholesterol levels, ranging from **20 to 100 mg/dL**.
- **CholesterolTriglycerides**: Triglycerides levels, ranging from **50 to 400 mg/dL**.

---

## Cognitive and Functional Assessments
- **UPDRS**: Unified Parkinson's Disease Rating Scale score, ranging from **0 to 199** (higher scores indicate greater severity of the disease).
- **MoCA**: Montreal Cognitive Assessment score, ranging from **0 to 30** (lower scores indicate cognitive impairment).
- **FunctionalAssessment**: Functional assessment score, ranging from **0 to 10** (lower scores indicate greater impairment).

---

## Symptoms
- **Tremor**: Presence of tremor:
  - `0`: No
  - `1`: Yes
- **Rigidity**: Presence of muscle rigidity:
  - `0`: No
  - `1`: Yes
- **Bradykinesia**: Presence of bradykinesia (slowness of movement):
  - `0`: No
  - `1`: Yes
- **PosturalInstability**: Presence of postural instability:
  - `0`: No
  - `1`: Yes
- **SpeechProblems**: Presence of speech problems:
  - `0`: No
  - `1`: Yes
- **SleepDisorders**: Presence of sleep disorders:
  - `0`: No
  - `1`: Yes
- **Constipation**: Presence of constipation:
  - `0`: No
  - `1`: Yes

---

## Diagnosis Information
- **Diagnosis**: Diagnosis status for Parkinson's Disease:
  - `0`: No
  - `1`: Yes

---

## Confidential Information
- **DoctorInCharge**: This column contains confidential information about the doctor in charge, represented as `"DrXXXConfid"` for all patients.

---

### Notes
- This dataset is for educational and research purposes.
- Ensure compliance with data protection regulations when handling this data.
