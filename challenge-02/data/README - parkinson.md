# Patient Information Dataset

Este conjunto de datos contiene información detallada sobre la salud de 2,105 pacientes diagnosticados con la Enfermedad de Parkinson, cada uno identificado de forma única con IDs que van desde 3058 hasta 5162. Incluye detalles demográficos, factores de estilo de vida, historial médico, mediciones clínicas, evaluaciones cognitivas y funcionales, síntomas, e indicadores de diagnóstico. Es valioso para investigadores y científicos de datos que deseen explorar factores asociados con la Enfermedad de Parkinson, desarrollar modelos predictivos y realizar análisis estadísticos.

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

### Patient ID \*\*\* nao por

- **PatientID**: Un identificador único asignado a cada paciente. Los valores varían entre **3058 to 5162.**.

---

## Demographic Details

- **Age**: Edad de los pacientes, entre 50 y 90 años. **60 to 90 years**.
- **Gender**: Género de los pacientes.
  - `0`: Male
  - `1`: Female
- **Ethnicity**: Etnicidad de los pacientes, codificada de la siguiente manera:
  - `0`: Caucasian
  - `1`: African American
  - `2`: Asian
  - `3`: Other

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

- **FamilyHistoryParkinsons**: Historial familiar de la Enfermedad de Parkinson:
  - `0`: No
  - `1`: Yes
- **TraumaticBrainInjury**: Historial de lesiones cerebrales traumáticas:
  - `0`: No
  - `1`: Yes
- **Stroke**: Historial de accidente cerebrovascular:

  - `0`: No
  - `1`: Yes

- **Diabetes**: resencia de diabetes:\*
  - `0`: No
  - `1`: Yes
- **Depression**: Presencia de depresión:\*
  - `0`: No
  - `1`: Yes
- **Hypertension**: Presencia de hipertensión:\*
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

- **MoCA**: Puntuación de la Evaluación Cognitiva de Montreal, que varía entre **0 y 30** (Las puntuaciones más bajas indican deterioro cognitivo)
- **UPDRS**: Puntuación de la Escala Unificada de Clasificación de la Enfermedad de Parkinson, que varía entre **0 y 199** (los puntajes más bajos indican deterioro cognitivo).

- **FunctionalAssessment**: Puntaje de evaluación funcional, entre **0 to 10** (los puntajes más bajos indican mayor deterioro).

---

## Symptoms

- **Tremor**: Presencia de temblores:
  - `0`: No
  - `1`: Yes
- **Rigidity**: Presencia de rigidez muscular:
  - `0`: No
  - `1`: Yes
- **Bradykinesia**: Presencia de bradicinesia (lentitud de movimiento):
  - `0`: No
  - `1`: Yes
- **PosturalInstability**:Presencia de inestabilidad postural:
  - `0`: No
  - `1`: Yes
- **SpeechProblems**: Presencia de problemas del habla:
  - `0`: No
  - `1`: Yes
- **SleepDisorders**: Presencia de trastornos del sueño:
  - `0`: No
  - `1`: Yes
- **Constipation**: Presencia de estreñimiento:
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
