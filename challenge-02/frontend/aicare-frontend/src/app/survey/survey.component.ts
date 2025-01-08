import { Component } from '@angular/core';
import { FormsModule } from '@angular/forms'; 
import { CommonModule } from '@angular/common';
import { HttpClient, HttpHeaders } from '@angular/common/http';

@Component({
  selector: 'app-survey',
  standalone: true,
  imports: [FormsModule, CommonModule], 
  templateUrl: './survey.component.html',
  styleUrls: ['./survey.component.css'],
})
export class SurveyComponent {
  constructor(private http: HttpClient) {} // Injeção do HttpClient

  questions = [
    // # Demographic and General Information
    [
        "Age",
        "Gender",
        "Ethnicity",
    ],
    // # Lifestyle
    [
        "What is your weight and height?",
        "Do you smoke currently or have you smoked in the past?",
        "How many alcoholic drinks do you consume per week?",
        "How many hours of exercise do you do per week?",
        "How would you describe your diet? (1-5, with 5 being excellent)",
        "How would you rate your sleep quality? (1-5, with 5 being excellent)",
    ],
    // # Family History
    [
      "Is there a family history of Parkinson's disease?",
      "Is there a family history of Alzheimer's disease?",
      "Has anyone in your family had a stroke?",
    ],
    // # Medical Conditions
    [
        "Have you been diagnosed with hypertension (high blood pressure)?",
        "Do you have diabetes?",
        "Have you been diagnosed with depression?",
        "What was your last blood pressure measurement (systolic and diastolic)?",
        "What was your last cholesterol measurement (total, LDL, HDL, and triglycerides)?",
        "How would you rate your overall functional capacity? (1-5, with 5 being excellent)",
    ],
    // # Neurological
    [
        "Have you experienced a severe traumatic brain injury?",
        "How would you classify your motor symptoms using a UPDRS scale (1-100)?",
        "What was your score on the last MoCA (Cognitive Scale) test?",
        "Do you frequently experience tremors?",
        "Do you experience muscle stiffness?",
        "Do you notice slowness in your movements?",
        "Do you have difficulty maintaining balance?",
        "Do you have difficulty speaking?",
        "Do you have trouble sleeping?",
        "Do you have problems with constipation?",
    ],
    // # Cognitive and Behavioral
    [
        "What was your score on the last MMSE (Mini-Mental State Examination)?",
        "Do you have trouble remembering things?",
        "Have you noticed changes in your behavior?",
        "Do you have difficulty performing daily tasks?",
        "Do you often feel confused?",
        "Do you feel disoriented?",
        "Have you noticed personality changes?",
        "Do you have difficulty completing tasks?",
        "Do you frequently experience forgetfulness?",
    ],
]


  // Arreglo de títulos para cada página
  pageTitles = [
    'Personal Information',
    'Lifestyle Questions',
    'Family History',
    'Medical Conditions',
    'Neurogical Conditions',
    'Cognitive and Behavioral'
  ];

  currentPage = 0; 
  formData: { [key: string]: string } = {}; 

  nextPage() {
    if (this.currentPage < this.questions.length - 1) {
      this.currentPage++;
    }
  }

  
  prevPage() {
    if (this.currentPage > 0) {
      this.currentPage--;
    }
  }

  
  async submitForm() {
    const jsonToSend = {
      "Age": 65,
      "Gender": 1,
      "Ethnicity": 2,
      "BMI": 24.5,
      "Smoking": 0,
      "AlcoholConsumption": 1.8,
      "PhysicalActivity": 3.5,
      "DietQuality": 4.0,
      "SleepQuality": 3.0,
      "Hypertension": 1,
      "Diabetes": 0,
      "Depression": 1,
      "SystolicBP": 130,
      "DiastolicBP": 85,
      "CholesterolTotal": 220.0,
      "CholesterolLDL": 140.0,
      "CholesterolHDL": 50.0,
      "CholesterolTriglycerides": 180.0,
      "FunctionalAssessment": 3.0,
    
      "FamilyHistoryParkinsons": 1,
      "TraumaticBrainInjury": 0,
      "Stroke": 0,
      "UPDRS": 32.0,
      "MoCA": 28.0,
      "Tremor": 1,
      "Rigidity": 1,
      "Bradykinesia": 1,
      "PosturalInstability": 0,
      "SpeechProblems": 0,
      "SleepDisorders": 1,
      "Constipation": 1,
    
      "FamilyHistoryAlzheimers": 1,
      "CardiovascularDisease": 1,
      "HeadInjury": 0,
      "MMSE": 26.0,
      "MemoryComplaints": 1,
      "BehavioralProblems": 0,
      "ADL": 4.0,
      "Confusion": 0,
      "Disorientation": 0,
      "PersonalityChanges": 1,
      "DifficultyCompletingTasks": 1,
      "Forgetfulness": 1
    };

    const headers = new HttpHeaders().set('Content-Type', 'application/json');

    this.http.post('http://127.0.0.1:8000/predict', jsonToSend, { headers })
      .subscribe({
        next: (response) => {
          console.log('Response:', response);
          alert('Form submitted successfully!');
        },
        error: (error) => {
          console.error('Error:', error);
          alert('An error occurred while submitting the form.');
        }
      });

  }
}
