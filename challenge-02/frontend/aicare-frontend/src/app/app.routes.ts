import { Routes } from '@angular/router';
import { SurveyComponent } from './survey/survey.component'; // Adicione o caminho para o SurveyComponent
import { PredictionResultComponent } from './prediction-result/prediction-result.component';

export const routes: Routes = [
  { path: '', redirectTo: '/survey', pathMatch: 'full' }, // Default route
  { path: 'survey', component: SurveyComponent },
  { path: 'prediction-result', component: PredictionResultComponent }, // New route
];
