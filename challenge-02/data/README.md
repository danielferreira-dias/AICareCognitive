# Data Folder

This folder contains all datasets used in the project, organized into subdirectories based on their purpose and processing stage.

## Folder Structure

- **`raw/`**: Contains the original, unprocessed datasets as received from the source. These files should remain unchanged to preserve data integrity.
- **`processed/`**: Contains cleaned and transformed datasets that are ready for analysis or model training.
- **`external/`**: Contains any external datasets, such as pre-trained models, public datasets, or third-party data used in the project.

## Guidelines for Data Handling

1. **Do Not Modify Raw Data**:
   - Always keep the original datasets in the `raw/` folder unchanged.
   - Perform all cleaning and preprocessing in scripts, saving outputs to the `processed/` folder.

2. **Data Versioning**:
   - If datasets are updated, create a new version (e.g., `dataset_v2.csv`) and move the previous version to an archive folder (if applicable).

3. **Data Documentation**:
   - Document the source of each dataset in this README or in a separate metadata file.
   - Include details such as:
     - Data source
     - Acquisition date
     - Format (e.g., CSV, JSON)
     - Description of fields (e.g., column names and their meanings)

4. **Privacy and Security**:
   - If the data contains sensitive or personal information, ensure compliance with applicable privacy regulations (e.g., GDPR).
   - Do not upload sensitive data to public repositories.

## Example Data Workflow

1. **Place Raw Data**:
   - Add all new datasets to the `raw/` folder.

2. **Run Preprocessing Scripts**:
   - Use scripts in `src/preprocessing/` to clean and transform data.
   - Save outputs to the `processed/` folder.

3. **Use Processed Data**:
   - Use datasets from the `processed/` folder for analysis, machine learning, or optimization tasks.

## Sources and References

- TIHM_Dataset:
  - **Source**: https://zenodo.org/records/7622128
  - **License**: Creative Commons Attribution 4.0 International
  - **Notes**: [Any important notes about usage, restrictions, or limitations]

## Template

- [Dataset Name or Description]:
  - **Source**: [URL or Name of Source]
  - **License**: [License Type (e.g., CC-BY, MIT, Proprietary)]
  - **Notes**: [Any important notes about usage, restrictions, or limitations]

## Notes

- If you encounter any issues with data files (e.g., missing fields or errors), report them to the team and document the issue in this README or the issue tracker.
- For large datasets, consider using a `.gitignore` file to avoid committing raw data files to version control.


## ------------------------------

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
