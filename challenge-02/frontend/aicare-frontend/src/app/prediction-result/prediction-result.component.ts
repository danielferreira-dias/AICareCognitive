import { CommonModule } from '@angular/common';
import { Component } from '@angular/core';
import { Router } from '@angular/router';

@Component({
  selector: 'app-prediction-result',
  imports: [CommonModule],
  templateUrl: './prediction-result.component.html',
  styleUrls: ['./prediction-result.component.css'],
})
export class PredictionResultComponent {
  results: {
    parkinson_model?: { prediction: number };
    alzheimer_model?: { prediction: number };
  } = {};

  constructor(private router: Router) {
    const navigation = this.router.getCurrentNavigation();
    this.results = navigation?.extras.state?.['results'] || {};
    console.log('results', this.results);
  }

  goBack() {
    this.router.navigate(['/survey']); // Adjust the route if necessary
  }

  navigateToAlgorithm() {
    this.router.navigate(['/algorithm-settings']);
  }
}
