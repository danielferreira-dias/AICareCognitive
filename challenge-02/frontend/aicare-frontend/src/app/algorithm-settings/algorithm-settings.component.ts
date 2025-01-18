import { Component } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { AuthService } from '@auth0/auth0-angular';
import { FormsModule } from '@angular/forms';
import { CommonModule } from '@angular/common';

@Component({
  selector: 'app-algorithm-settings',
  imports: [FormsModule, CommonModule],
  templateUrl: './algorithm-settings.component.html',
  styleUrl: './algorithm-settings.component.css',
})
export class AlgorithmSettingsComponent {
  selectedAlgorithm = 'TOPSIS';
  showWeightsPopup = false;
  weights: any[] = [];
  activities: any[] = [];
  maxPositive: number = 0;
  minPositive: number = 0;
  maxNegative: number = 0;
  minNegative: number = 0;

  constructor(private http: HttpClient, private auth: AuthService) {}

  onAlgorithmChange() {
    const payload = { algorithm: this.selectedAlgorithm };
    this.auth.getAccessTokenSilently().subscribe((token) => {
      const headers = new HttpHeaders().set('Authorization', `Bearer ${token}`);
      this.http
        .put('http://localhost:8000/algorithm', payload, { headers })
        .subscribe({
          next: () => console.log('Algorithm updated'),
          error: (err) => console.error('Error updating algorithm', err),
        });
    });
  }

  openWeightsPopup() {
    this.auth.getAccessTokenSilently().subscribe((token) => {
      const headers = new HttpHeaders().set('Authorization', `Bearer ${token}`);
      this.http.get('http://localhost:8000/weights', { headers }).subscribe({
        next: (response: any) => {
          this.weights = response.weights;
          this.showWeightsPopup = true;
        },
        error: (err) => console.error('Error fetching weights', err),
      });
    });
  }

  saveWeights() {
    this.auth.getAccessTokenSilently().subscribe((token) => {
      const headers = new HttpHeaders().set('Authorization', `Bearer ${token}`);
      this.http
        .put(
          'http://localhost:8000/weights',
          { weights: this.weights },
          { headers }
        )
        .subscribe({
          next: () => {
            this.showWeightsPopup = false;
            console.log('Weights updated');
          },
          error: (err) => console.error('Error updating weights', err),
        });
    });
  }

  closeWeightsPopup() {
    this.showWeightsPopup = false;
  }

  getActivities() {
    this.auth.getAccessTokenSilently().subscribe((token) => {
      const headers = new HttpHeaders().set('Authorization', `Bearer ${token}`);
      this.http.get('http://localhost:8000/results', { headers }).subscribe({
        next: (response: any) => {
          this.activities = response.map((activity: any) => ({
            ...activity,
            showBadge: false, // Add `showBadge` property for the badge
          }));

          // Calculate ranges for dynamic colors
          const positiveScores = this.activities
            .filter((a) => a.score > 0)
            .map((a) => a.score);
          const negativeScores = this.activities
            .filter((a) => a.score < 0)
            .map((a) => a.score);
          console.log(this.activities);
          console.log(positiveScores);
          console.log(negativeScores);

          this.maxPositive = Math.max(...positiveScores, 0);
          this.minPositive = Math.min(...positiveScores, 0);
          this.maxNegative = Math.max(...negativeScores, 0);
          this.minNegative = Math.min(...negativeScores, 0);
          console.log(
            'minPositive:',
            this.minPositive,
            'maxPositive:',
            this.maxPositive
          );
          console.log(
            'minNegative:',
            this.minNegative,
            'maxNegative:',
            this.maxNegative
          );
        },
        error: (err) => console.error('Error fetching activities', err),
      });
    });
  }

  getPillBackground(activity: any): string {
    const score = activity.score;

    console.log(score);

    if (score > 0) {
      const normalized =
        (score - this.minPositive) / (this.maxPositive - this.minPositive);
      return `rgba(0, 200, 0, ${normalized})`; // Gradient Green
    } else if (score < 0) {
      const normalized =
        (score - this.maxNegative) / (this.minNegative - this.maxNegative);
      return `rgba(200, 0, 0, ${normalized})`; // Gradient Red
    }

    return '#808080'; // Neutral Grey
  }

  toggleBadge(activity: any) {
    activity.showBadge = true; // Show the badge
    setTimeout(() => {
      activity.showBadge = false; // Hide the badge after 1 second
    }, 1000);
  }
}
