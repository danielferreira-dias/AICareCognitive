import { Component } from '@angular/core';
import { Router } from '@angular/router';
import { TranslateModule } from '@ngx-translate/core';

@Component({
  selector: 'app-body-component',
  imports: [TranslateModule],
  standalone: true, // Marca o componente como standalone
  templateUrl: './body-component.component.html',
  styleUrls: ['./body-component.component.css'],
})
export class BodyComponentComponent {
  constructor(private router: Router) {}

  navigateToSurvey() {
    this.router.navigate(['/survey']); // Navega para a página de survey
  }
}
