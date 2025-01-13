import { Component } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { CommonModule } from '@angular/common';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Router } from '@angular/router';

type Question =
  | { label: string; type: 'text' }
  | { label: string; type: 'number'; min?: number; max?: number }
  | {
      label: string;
      type: 'one-hot';
      options: { value: string; label: string }[];
    }
  | { label: string; type: 'number-radio'; min: number; max: number };

type Category = {
  category: string;
  questions: Question[];
};

@Component({
  selector: 'app-survey',
  standalone: true,
  imports: [FormsModule, CommonModule],
  templateUrl: './survey.component.html',
  styleUrls: ['./survey.component.css'],
})
export class SurveyComponent {
  constructor(private http: HttpClient, private router: Router) {}

  categories: Category[] = [
    {
      category: 'Demographic and General Information',
      questions: [
        { label: 'Age', type: 'number' },
        {
          label: 'Gender',
          type: 'one-hot',
          options: [
            { value: '0', label: 'Male' },
            { value: '1', label: 'Female' },
          ],
        },
        {
          label: 'Ethnicity',
          type: 'one-hot',
          options: [
            { value: '0', label: 'Caucasian' },
            { value: '1', label: 'African American' },
            { value: '2', label: 'Asian' },
            { value: '3', label: 'Other' },
          ],
        },
      ],
    },
    {
      category: 'Lifestyle',
      questions: [
        {
          label: 'What is your BMY (body mass index)? (15-40)',
          type: 'number',
          min: 15,
          max: 40,
        },
        {
          label: 'Do you smoke currently or have you smoked in the past?',
          type: 'one-hot',
          options: [],
        },
        {
          label: 'How many alcoholic drinks do you consume per week? (0-20)',
          type: 'number',
          min: 0,
          max: 20,
        },
        {
          label: 'How many hours of exercise do you do per week? (0-10)',
          type: 'number',
          min: 0,
          max: 10,
        },
        {
          label:
            'How would you describe your diet? (0-10, with 10 being excellent)',
          type: 'number-radio',
          min: 0,
          max: 10,
        },
        {
          label:
            'How would you rate your sleep quality? (0-10, with 10 being excellent)',
          type: 'number-radio',
          min: 0,
          max: 10,
        },
      ],
    },
    {
      category: 'Family History',
      questions: [
        {
          label: "Is there a family history of Parkinson's disease?",
          type: 'one-hot',
          options: [],
        },
        {
          label: "Is there a family history of Alzheimer's disease?",
          type: 'one-hot',
          options: [],
        },
        {
          label: 'Has anyone in your family had a stroke?',
          type: 'one-hot',
          options: [],
        },
      ],
    },
    {
      category: 'Medical Conditions',
      questions: [
        {
          label:
            'Have you been diagnosed with hypertension (high blood pressure)?',
          type: 'one-hot',
          options: [],
        },
        {
          label: 'Do you have diabetes?',
          type: 'one-hot',
          options: [],
        },
        {
          label: 'Have you been diagnosed with depression?',
          type: 'one-hot',
          options: [],
        },
        {
          label: 'What was your last blood pressure measurement (systolic)?',
          type: 'number',
          min: 0,
        },
        {
          label: 'What was your last blood pressure measurement (diastolic)?',
          type: 'number',
          min: 0,
        },
        {
          label: 'What was your last cholesterol measurement (total)?',
          type: 'number',
          min: 0,
        },
        {
          label: 'What was your last cholesterol measurement (LDL)?',
          type: 'number',
          min: 0,
        },
        {
          label: 'What was your last cholesterol measurement (HDL)?',
          type: 'number',
          min: 0,
        },
        {
          label:
            'What was your last cholesterol measurement (CholesterolTriglycerides)?',
          type: 'number',
          min: 0,
        },
        {
          label: 'Do you have any Cardio Vascular Disease ?',
          type: 'one-hot',
          options: [],
        },
        {
          label:
            'How would you rate your overall functional capacity? (1-5, with 5 being excellent)',
          type: 'number-radio',
          min: 1,
          max: 5,
        },
      ],
    },
    {
      category: 'Neurological',
      questions: [
        {
          label: 'Have you experienced a severe traumatic brain injury?',
          type: 'one-hot',
          options: [],
        },
        {
          label:
            'How would you classify your motor symptoms using a UPDRS scale (1-100)?',
          type: 'number',
          min: 1,
          max: 100,
        },
        {
          label: 'What was your score on the last MoCA (Cognitive Scale) test?',
          type: 'number',
          min: 0,
        },
        {
          label: 'Do you frequently experience tremors?',
          type: 'one-hot',
          options: [],
        },
        {
          label: 'Do you experience muscle stiffness?',
          type: 'one-hot',
          options: [],
        },
        {
          label: 'Do you notice slowness in your movements?',
          type: 'one-hot',
          options: [],
        },
        {
          label: 'Do you have difficulty maintaining balance?',
          type: 'one-hot',
          options: [],
        },
        {
          label: 'Do you have difficulty speaking?',
          type: 'one-hot',
          options: [],
        },
        {
          label: 'Do you have trouble sleeping?',
          type: 'one-hot',
          options: [],
        },
        {
          label: 'Do you have problems with constipation?',
          type: 'one-hot',
          options: [],
        },
      ],
    },
    {
      category: 'Cognitive and Behavioral',
      questions: [
        {
          label:
            'What was your score on the last MMSE (Mini-Mental State Examination)?',
          type: 'number',
          min: 0,
        },
        {
          label: 'Do you have trouble remembering things?',
          type: 'one-hot',
          options: [],
        },
        {
          label: 'Have you noticed changes in your behavior?',
          type: 'one-hot',
          options: [],
        },
        {
          label: 'Do you have difficulty performing daily tasks?',
          type: 'one-hot',
          options: [],
        },
        {
          label: 'Do you often feel confused?',
          type: 'one-hot',
          options: [],
        },
        {
          label: 'Do you feel disoriented?',
          type: 'one-hot',
          options: [],
        },
        {
          label: 'Have you noticed personality changes?',
          type: 'one-hot',
          options: [],
        },
        {
          label: 'Do you have difficulty completing tasks?',
          type: 'one-hot',
          options: [],
        },
        {
          label: 'Do you frequently experience forgetfulness?',
          type: 'one-hot',
          options: [],
        },
      ],
    },
  ];

  // Array of titles for each page
  pageTitles = [
    'Personal Information',
    'Lifestyle Questions',
    'Family History',
    'Medical Conditions',
    'Neurogical Conditions',
    'Cognitive and Behavioral',
  ];

  currentCategory = 0;
  formData: any = {};

  sanitizeNumberInput(event: Event, index: number) {
    const input = event.target as HTMLInputElement;
    let value = input.value;

    const question = this.categories[this.currentCategory]?.questions[index];

    // Remove invalid characters
    value = value.replace(/[^0-9.-]/g, '');

    // Prevent negative values if min is >= 0
    if (
      question.type === 'number' &&
      question.min !== undefined &&
      question.min >= 0
    ) {
      value = value.replace(/-/g, ''); // Remove negative sign
    }

    // Update the input value
    input.value = value;

    // Update the model
    const fieldName = `category${this.currentCategory}-question${index}`;
    this.formData[fieldName] = value ? parseFloat(value) : null;
  }

  navigateToPage(pageIndex: number) {
    // // Optional validation to check if the user can leave the current page
    // const currentPageQuestions = this.categories;

    // const allFieldsFilled = currentPageQuestions.every((_, index) => {
    //     const fieldValue = this.formData[`page${this.currentCategory + 1}-question${index}`];
    //     return fieldValue !== undefined && fieldValue !== null && fieldValue !== '';
    // });

    // if (!allFieldsFilled) {
    //     alert('Please fill in all the fields before navigating.');
    //     return;
    // }

    // Navigate to the selected page

    this.currentCategory = pageIndex;
    const questions = this.categories[pageIndex]?.questions || [];
    questions.forEach((_, i) => {
      const fieldName = `category${pageIndex}-question${i}`;
      if (!this.formData[fieldName]) {
        this.formData[fieldName] = null; // Set default value
      }
    });
  }

  prevCategory() {
    if (this.currentCategory > 0) {
      this.currentCategory--;
    }
  }

  nextCategory() {
    // // Optional validation to check if the user can leave the current page
    // const currentPageQuestions = this.categories;

    // const allFieldsFilled = currentPageQuestions.every((_, index) => {
    //   const fieldValue = this.formData[`page${this.currentCategory + 1}-question${index}`];
    //   return fieldValue !== undefined && fieldValue !== null && fieldValue !== '';
    // });

    // if (!allFieldsFilled) {
    //     alert('Please fill in all the fields before navigating.');
    //     return;
    // }

    if (this.currentCategory < this.categories.length - 1) {
      this.currentCategory++;
    }
  }

  async submitForm() {
    for (const key in this.formData) {
      if (this.formData[key] === null || this.formData[key] === undefined) {
        alert(`Please complete all fields before submitting.`);
        return;
      }
    }

    const jsonToSend = {
      Age: parseInt(this.formData['category0-question0']),
      Gender: parseInt(this.formData['category0-question1']),
      Ethnicity: parseInt(this.formData['category0-question2']),

      BMI: parseFloat(this.formData['category1-question0']),
      Smoking: parseInt(this.formData['category1-question1']),
      AlcoholConsumption: parseFloat(this.formData['category1-question2']),
      PhysicalActivity: parseFloat(this.formData['category1-question3']),
      DietQuality: parseInt(this.formData['category1-question4']),
      SleepQuality: parseInt(this.formData['category1-question5']),

      FamilyHistoryParkinsons: parseInt(this.formData['category2-question0']),
      FamilyHistoryAlzheimers: parseInt(this.formData['category2-question1']),
      Stroke: parseInt(this.formData['category2-question2']),

      Hypertension: parseInt(this.formData['category3-question0']),
      Diabetes: parseInt(this.formData['category3-question1']),
      Depression: parseInt(this.formData['category3-question2']),
      SystolicBP: parseInt(this.formData['category3-question3']),
      DiastolicBP: parseInt(this.formData['category3-question4']),
      CholesterolTotal: parseFloat(this.formData['category3-question5']),
      CholesterolLDL: parseFloat(this.formData['category3-question6']),
      CholesterolHDL: parseFloat(this.formData['category3-question7']),
      CholesterolTriglycerides: parseFloat(
        this.formData['category3-question8']
      ),
      CardiovascularDisease: parseInt(this.formData['category3-question9']),
      FunctionalAssessment: parseFloat(this.formData['category3-question10']),

      TraumaticBrainInjury: parseInt(this.formData['category4-question0']),
      HeadInjury: parseFloat(this.formData['category4-question0']),
      UPDRS: parseFloat(this.formData['category4-question1']),
      MoCA: parseFloat(this.formData['category4-question2']),
      Tremor: parseInt(this.formData['category4-question3']),
      Rigidity: parseInt(this.formData['category4-question4']),
      Bradykinesia: parseInt(this.formData['category4-question5']),
      PosturalInstability: parseInt(this.formData['category4-question6']),
      SpeechProblems: parseInt(this.formData['category4-question7']),
      SleepDisorders: parseInt(this.formData['category4-question8']),
      Constipation: parseInt(this.formData['category4-question9']),

      MMSE: parseFloat(this.formData['category5-question0']),
      MemoryComplaints: parseInt(this.formData['category5-question1']),
      BehavioralProblems: parseInt(this.formData['category5-question2']),
      ADL: parseFloat(this.formData['category5-question3']),
      Confusion: parseInt(this.formData['category5-question4']),
      Disorientation: parseInt(this.formData['category5-question5']),
      PersonalityChanges: parseInt(this.formData['category5-question6']),
      DifficultyCompletingTasks: parseInt(this.formData['category5-question7']),
      Forgetfulness: parseInt(this.formData['category5-question8']),
    };
    console.log(JSON.stringify(jsonToSend));

    const headers = new HttpHeaders().set('Content-Type', 'application/json');

    this.http
      .post('http://127.0.0.1:8000/predict', jsonToSend, { headers })
      .subscribe({
        next: (response) => {
          this.router.navigate(['/prediction-result'], {
            state: { results: response },
          });
        },
        error: (error) => {
          console.error('Error:', error);
          alert('An error occurred while submitting the form.');
        },
      });
  }
}
