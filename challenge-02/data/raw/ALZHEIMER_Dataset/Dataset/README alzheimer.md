# Patient Information Dataset

Este conjunto de datos contiene información detallada sobre pacientes, incluyendo datos demográficos, factores de estilo de vida, historial médico, mediciones clínicas, evaluaciones cognitivas y funcionales, síntomas e información de diagnóstico relacionada con la Enfermedad de ALZHEIMER.

A continuación, se describe detalladamente la estructura y los atributos del conjunto de datos.

---

## Table of Contents

1. [Patient Information](#patient-information) Información del Paciente
   - [Patient ID](#patient-id) ID del Paciente
2. [Demographic Details](#demographic-details) Detalles Demográficos
3. [Lifestyle Factors](#lifestyle-factors) Factores de Estilo de Vida
4. [Medical History](#medical-history) Historial Médico
5. [Clinical Measurements](#clinical-measurements) Mediciones Clínicas
6. [Cognitive and Functional Assessments](#cognitive-and-functional-assessments) Evaluaciones Cognitivas y Funcionales
7. [Symptoms](#symptoms) Síntomas
8. [Diagnosis Information](#diagnosis-information) Información de Diagnóstico
9. [Confidential Information](#confidential-information) Información Confidencial

---

## Patient Information

### Patient ID

- **PatientID**: Un identificador único asignado a cada paciente. Los valores varían entre **4751 to 6900**.

---

## Demographic Details

- **Age**: Edad de los pacientes, entre 60 y 90 años. **60 to 90 years**.
- **Gender**: Género de los pacientes.
  - `0`: Male
  - `1`: Female
- **Ethnicity**: Etnicidad de los pacientes, codificada de la siguiente manera:
  - `0`: Caucasian
  - `1`: African American
  - `2`: Asian
  - `3`: Other
- **EducationLevel**: Nivel educativo de los pacientes, codificado de la siguiente manera:
  - `0`: None
  - `1`: High School
  - `2`: Bachelor's
  - `3`: Higher

---

## Lifestyle Factors

- **BMI**: Índice de Masa Corporal de los pacientes, entre **15 to 40**.
- **Smoking**: Estado de fumador:
  - `0`: No
  - `1`: Yes
- **AlcoholConsumption**: Consumo semanal de alcohol en unidades, entre **0 to 20**.
- **PhysicalActivity**: Actividad física semanal en horas, entre **0 to 10**.
- **DietQuality**: Puntaje de calidad de la dieta, entre **0 to 10**.
- **SleepQuality**: Puntaje de calidad del sueño, entre **4 to 10**.

---

## Medical History

- **FamilyHistoryAlzheimers**: Historial familiar de la Enfermedad de Alzheimer:
  - `0`: No
  - `1`: Yes
- **CardiovascularDisease**: Presencia de enfermedad cardiovascular:
  - `0`: No
  - `1`: Yes
- **Diabetes**: resencia de diabetes:
  - `0`: No
  - `1`: Yes
- **Depression**: Presencia de depresión:
  - `0`: No
  - `1`: Yes
- **HeadInjury**: Historial de lesiones en la cabeza:
  - `0`: No
  - `1`: Yes
- **Hypertension**: Presencia de hipertensión:
  - `0`: No
  - `1`: Yes

---

## Mediciones Clinicas

- **SystolicBP**: Presión arterial sistólica, entre **90 to 180 mmHg**.
- **DiastolicBP**: Presión arterial diastólica, entre **60 to 120 mmHg**.
- **CholesterolTotal**: Niveles de colesterol total, entre **150 to 300 mg/dL**.
- **CholesterolLDL**: Niveles de colesterol de lipoproteínas de baja densidad, entre **50 to 200 mg/dL**.
- **CholesterolHDL**: Niveles de colesterol de lipoproteínas de alta densidad, entre **20 to 100 mg/dL**.
- **CholesterolTriglycerides**: Niveles de triglicéridos, entre **50 to 400 mg/dL**.

---

## Cognitive and Functional Assessments

- **MMSE**: Puntaje del Mini-Examen del Estado Mental, entre **0 to 30** (los puntajes más bajos indican deterioro cognitivo).
- **FunctionalAssessment**: Puntaje de evaluación funcional, entre **0 to 10** (los puntajes más bajos indican mayor deterioro).
- **MemoryComplaints**: Presencia de quejas de memoria:
  - `0`: No
  - `1`: Yes
- **BehavioralProblems**: Presencia de problemas de conducta:
  - `0`: No
  - `1`: Yes
- **ADL**: Puntaje de Actividades de la Vida Diaria, entre **0 to 10** (los puntajes más bajos indican mayor deterioro).

---

## Symptoms

- **Confusion**: Presencia de confusión:
  - `0`: No
  - `1`: Yes
- **Disorientation**: Presencia de desorientación:
  - `0`: No
  - `1`: Yes
- **PersonalityChanges**: Presencia de cambios de personalidad:
  - `0`: No
  - `1`: Yes
- **DifficultyCompletingTasks**: Presencia de dificultad para completar tareas:
  - `0`: No
  - `1`: Yes
- **Forgetfulness**: Presencia de olvidos:
  - `0`: No
  - `1`: Yes

---

## Diagnosis Information

- **Diagnosis**: Estado de diagnóstico de la Enfermedad de Alzheimer:
  - `0`: No
  - `1`: Yes

---

## Confidential Information

- **DoctorInCharge**: This column contains confidential information about the doctor in charge, represented as `"XXXConfid"` for all patients.

---

### Notes

- This dataset is for educational and research purposes.
- Ensure compliance with data protection regulations when handling this data.
