import { Routes } from '@angular/router';
import { SurveyComponent } from './survey/survey.component';
import { PredictionResultComponent } from './prediction-result/prediction-result.component';
import { AuthGuard } from '@auth0/auth0-angular';
import { CallbackComponent } from './callback/callback.component';
import { AlgorithmSettingsComponent } from './algorithm-settings/algorithm-settings.component';

export const routes: Routes = [
  {
    path: 'callback',
    component: CallbackComponent,
  },
  {
    path: '',
    redirectTo: '/survey',
    pathMatch: 'full',
  },
  { path: 'survey', component: SurveyComponent, canActivate: [AuthGuard] },
  {
    path: 'prediction-result',
    component: PredictionResultComponent,
    canActivate: [AuthGuard],
  },
  {
    path: 'algorithm-settings',
    component: AlgorithmSettingsComponent,
    canActivate: [AuthGuard],
  },
];
