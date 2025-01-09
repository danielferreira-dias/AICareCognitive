import { Component } from '@angular/core';
import { FormsModule } from '@angular/forms'; 
import { CommonModule } from '@angular/common';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { SurveynavbarComponent } from '../surveynavbar/surveynavbar.component';

@Component({
  selector: 'app-survey',
  standalone: true,
  imports: [FormsModule, CommonModule, SurveynavbarComponent], 
  templateUrl: './survey.component.html',
  styleUrls: ['./survey.component.css'],
})
export class SurveyComponent {
  constructor(private http: HttpClient) {} // Injeção do HttpClient

  categories = [
    {
      "category": "Demographic and General Information",
      "questions": [
        { "label": "Age", "type": "text" },
        { "label": "Gender", "type": "one-hot" },
        { "label": "Ethnicity", "type": "text" }
      ]
    },
    {
      "category": "Lifestyle",
      "questions": [
        { "label": "What is your weight and height?", "type": "text" },
        { "label": "Do you smoke currently or have you smoked in the past?", "type": "one-hot" },
        { "label": "How many alcoholic drinks do you consume per week?", "type": "text" },
        { "label": "How many hours of exercise do you do per week?", "type": "text" },
        { "label": "How would you describe your diet? (1-5, with 5 being excellent)", "type": "text" },
        { "label": "How would you rate your sleep quality? (1-5, with 5 being excellent)", "type": "text" }
      ]
    },
    {
      "category": "Family History",
      "questions": [
        { "label": "Is there a family history of Parkinson's disease?", "type": "one-hot" },
        { "label": "Is there a family history of Alzheimer's disease?", "type": "one-hot" },
        { "label": "Has anyone in your family had a stroke?", "type": "one-hot" }
      ]
    },
    {
      "category": "Medical Conditions",
      "questions": [
        { "label": "Have you been diagnosed with hypertension (high blood pressure)?", "type": "one-hot" },
        { "label": "Do you have diabetes?", "type": "one-hot" },
        { "label": "Have you been diagnosed with depression?", "type": "one-hot" },
        { "label": "What was your last blood pressure measurement (systolic)?", "type": "text" },
        { "label": "What was your last blood pressure measurement (diastolic)?", "type": "text" },
        { "label": "What was your last cholesterol measurement (total)?", "type": "text" },
        { "label": "What was your last cholesterol measurement (LDL)?", "type": "text" },
        { "label": "What was your last cholesterol measurement (HDL)?", "type": "text" },
        { "label": "What was your last cholesterol measurement (CholesterolTriglycerides)?", "type": "text" },
        { "label": "How would you rate your overall functional capacity? (1-5, with 5 being excellent)", "type": "text" }
      ]
    },
    {
      "category": "Neurological",
      "questions": [
        { "label": "Have you experienced a severe traumatic brain injury?", "type": "one-hot" },
        { "label": "How would you classify your motor symptoms using a UPDRS scale (1-100)?", "type": "text" },
        { "label": "What was your score on the last MoCA (Cognitive Scale) test?", "type": "text" },
        { "label": "Do you frequently experience tremors?", "type": "one-hot" },
        { "label": "Do you experience muscle stiffness?", "type": "one-hot" },
        { "label": "Do you notice slowness in your movements?", "type": "one-hot" },
        { "label": "Do you have difficulty maintaining balance?", "type": "one-hot" },
        { "label": "Do you have difficulty speaking?", "type": "one-hot" },
        { "label": "Do you have trouble sleeping?", "type": "one-hot" },
        { "label": "Do you have problems with constipation?", "type": "one-hot" }
      ]
    },
    {
      "category": "Cognitive and Behavioral",
      "questions": [
        { "label": "What was your score on the last MMSE (Mini-Mental State Examination)?", "type": "text" },
        { "label": "Do you have trouble remembering things?", "type": "one-hot" },
        { "label": "Have you noticed changes in your behavior?", "type": "one-hot" },
        { "label": "Do you have difficulty performing daily tasks?", "type": "one-hot" },
        { "label": "Do you often feel confused?", "type": "one-hot" },
        { "label": "Do you feel disoriented?", "type": "one-hot" },
        { "label": "Have you noticed personality changes?", "type": "one-hot" },
        { "label": "Do you have difficulty completing tasks?", "type": "one-hot" },
        { "label": "Do you frequently experience forgetfulness?", "type": "one-hot" }
      ]
    }
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

  currentCategory = 0;
  formData: any = {};

  prevCategory() {
    if (this.currentCategory > 0) {
      this.currentCategory--;
    }
  }

  nextCategory() {
    if (this.currentCategory < this.categories.length - 1) {
      this.currentCategory++;
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
      FunctionalAssessment: parseFloat(this.formData['page4-question9']),
      
      TraumaticBrainInjury: parseInt(this.formData['page5-question0']),
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
