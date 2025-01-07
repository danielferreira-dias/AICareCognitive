import { Routes } from '@angular/router';
import { SurveyComponent } from './survey/survey.component'; // Adicione o caminho para o SurveyComponent
import { AppComponent } from './app.component';
import { BodyComponentComponent } from './body-component/body-component.component';

export const routes: Routes = [
  { path: 'survey', component: SurveyComponent },
  { path: '', component: BodyComponentComponent },
];
