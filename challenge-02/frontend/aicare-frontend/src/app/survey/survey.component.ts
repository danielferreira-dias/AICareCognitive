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
      "What was your last blood pressure measurement (systolic)?",
      "What was your last blood pressure measurement (diastolic)?",
      "What was your last cholesterol measurement (tota)?",
      "What was your last cholesterol measurement (LDL)?",
      "What was your last cholesterol measurement (HDL)?",
      "What was your last cholesterol measurement (CholesterolTriglycerides)?",
      "Do you have any Cardio Vascular Disease ?",
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
      Age: parseInt(this.formData['page1-question0']),
      Gender: parseInt(this.formData['page1-question1']),
      Ethnicity: parseInt(this.formData['page1-question2']),

      BMI: parseFloat(this.formData['page2-question0']),
      Smoking: parseInt(this.formData['page2-question1']),
      AlcoholConsumption: parseFloat(this.formData['page2-question2']),
      PhysicalActivity: parseFloat(this.formData['page2-question3']),
      DietQuality: parseInt(this.formData['page2-question4']),
      SleepQuality: parseInt(this.formData['page2-question5']),

      FamilyHistoryParkinsons: parseInt(this.formData['page3-question0']),
      FamilyHistoryAlzheimers: parseInt(this.formData['page3-question1']),
      Stroke: parseInt(this.formData['page3-question2']),

      Hypertension: parseInt(this.formData['page4-question0']),
      Diabetes: parseInt(this.formData['page4-question1']),
      Depression: parseInt(this.formData['page4-question2']),
      SystolicBP: parseInt(this.formData['page4-question3']),
      DiastolicBP: parseInt(this.formData['page4-question4']),
      CholesterolTotal: parseFloat(this.formData['page4-question5']),
      CholesterolLDL: parseFloat(this.formData['page4-question6']),
      CholesterolHDL: parseFloat(this.formData['page4-question7']),
      CholesterolTriglycerides: parseFloat(this.formData['page4-question8']),
      CardiovascularDisease : parseInt(this.formData['page4-question9']),
      FunctionalAssessment: parseFloat(this.formData['page4-question10']),
      
      TraumaticBrainInjury: parseInt(this.formData['page5-question0']),
      HeadInjury : parseFloat(this.formData['page5-question0']),
      UPDRS: parseFloat(this.formData['page5-question1']),
      MoCA: parseFloat(this.formData['page5-question2']),
      Tremor: parseInt(this.formData['page5-question3']),
      Rigidity: parseInt(this.formData['page5-question4']),
      Bradykinesia: parseInt(this.formData['page5-question5']),
      PosturalInstability: parseInt(this.formData['page5-question6']),
      SpeechProblems: parseInt(this.formData['page5-question7']),
      SleepDisorders: parseInt(this.formData['page5-question8']),
      Constipation: parseInt(this.formData['page5-question9']),
      
      MMSE: parseFloat(this.formData['page6-question0']),
      MemoryComplaints: parseInt(this.formData['page6-question1']),
      BehavioralProblems: parseInt(this.formData['page6-question2']),
      ADL: parseFloat(this.formData['page6-question3']),
      Confusion: parseInt(this.formData['page6-question4']),
      Disorientation: parseInt(this.formData['page6-question5']),
      PersonalityChanges: parseInt(this.formData['page6-question6']),
      DifficultyCompletingTasks: parseInt(this.formData['page6-question7']),
      Forgetfulness: parseInt(this.formData['page6-question8'])
  
    };

    const headers = new HttpHeaders().set('Content-Type', 'application/json');
    console.log(jsonToSend)
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
