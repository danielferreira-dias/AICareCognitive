# Patient Information Dataset

This dataset contains detailed information about patients, including their demographic details, lifestyle factors, medical history, clinical measurements, cognitive and functional assessments, symptoms, and diagnosis information related to Alzheimer's Disease. Below is a detailed description of the dataset's structure and attributes.

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
- **PatientID**: A unique identifier assigned to each patient. Values range from **4751 to 6900**.

---

## Demographic Details
- **Age**: Age of the patients, ranging from **60 to 90 years**.
- **Gender**: Gender of the patients.
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
- **FamilyHistoryAlzheimers**: Family history of Alzheimer's Disease:
  - `0`: No
  - `1`: Yes
- **CardiovascularDisease**: Presence of cardiovascular disease:
  - `0`: No
  - `1`: Yes
- **Diabetes**: Presence of diabetes:
  - `0`: No
  - `1`: Yes
- **Depression**: Presence of depression:
  - `0`: No
  - `1`: Yes
- **HeadInjury**: History of head injury:
  - `0`: No
  - `1`: Yes
- **Hypertension**: Presence of hypertension:
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
- **MMSE**: Mini-Mental State Examination score, ranging from **0 to 30** (lower scores indicate cognitive impairment).
- **FunctionalAssessment**: Functional assessment score, ranging from **0 to 10** (lower scores indicate greater impairment).
- **MemoryComplaints**: Presence of memory complaints:
  - `0`: No
  - `1`: Yes
- **BehavioralProblems**: Presence of behavioral problems:
  - `0`: No
  - `1`: Yes
- **ADL**: Activities of Daily Living score, ranging from **0 to 10** (lower scores indicate greater impairment).

---

## Symptoms
- **Confusion**: Presence of confusion:
  - `0`: No
  - `1`: Yes
- **Disorientation**: Presence of disorientation:
  - `0`: No
  - `1`: Yes
- **PersonalityChanges**: Presence of personality changes:
  - `0`: No
  - `1`: Yes
- **DifficultyCompletingTasks**: Presence of difficulty completing tasks:
  - `0`: No
  - `1`: Yes
- **Forgetfulness**: Presence of forgetfulness:
  - `0`: No
  - `1`: Yes

---

## Diagnosis Information
- **Diagnosis**: Diagnosis status for Alzheimer's Disease:
  - `0`: No
  - `1`: Yes

---

## Confidential Information
- **DoctorInCharge**: This column contains confidential information about the doctor in charge, represented as `"XXXConfid"` for all patients.

---

### Notes
- This dataset is for educational and research purposes.
- Ensure compliance with data protection regulations when handling this data.
