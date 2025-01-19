import { Component } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { AuthService } from '@auth0/auth0-angular';
import { FormsModule } from '@angular/forms';
import { CommonModule } from '@angular/common';
import { TranslateModule } from '@ngx-translate/core';

@Component({
  selector: 'app-algorithm-settings',
  imports: [FormsModule, CommonModule, TranslateModule],
  templateUrl: './algorithm-settings.component.html',
  styleUrl: './algorithm-settings.component.css',
})
export class AlgorithmSettingsComponent {
  selectedAlgorithm = 'TOPSIS';
  showWeightsPopup = false;
  weights: any[] = [];
  activities: any[] = [];

  constructor(private http: HttpClient, private auth: AuthService) {}

  onAlgorithmChange() {
    // Clear current state
    this.activities = [];

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
          console.log(this.weights);
        },
        error: (err) => console.error('Error fetching weights', err),
      });
    });
  }

  saveWeights() {
    // Transformar los datos al formato requerido
    const transformedWeights = this.weights.map((item: any) => ({
      criterion_id: item.criterion_id,
      new_weight: item.weight, // Cambiar "weight" a "new_weight"
    }));

    this.auth.getAccessTokenSilently().subscribe((token) => {
      console.log('Transformed weights', transformedWeights);
      const headers = new HttpHeaders().set('Authorization', `Bearer ${token}`);
      this.http
        .put(
          'http://localhost:8000/weights',
          { weights: transformedWeights }, // Enviar el objeto transformado
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
        },
        error: (err) => console.error('Error fetching activities', err),
      });
    });
  }

  getPillBackground(activity: any): string {
    const score = activity.score;

    const max = Math.max(...this.activities.map((a) => a.score));
    const min = Math.min(...this.activities.map((a) => a.score));
    const avg = (max + min) / 2; // Calculate the midpoint (average)

    if (score > avg) {
      const normalized = (max - score) / (max - avg);
      const greenValue = Math.round(0 + normalized * (128 - 0));
      return `rgb(${greenValue}, ${
        200 - normalized * (200 - 128)
      }, ${greenValue})`;
    } else if (score < avg) {
      // Invert the gradient logic for negatives
      const normalized = (score - avg) / (min - avg);
      const redValue = Math.round(128 + normalized * (200 - 128));
      return `rgb(${redValue}, ${128 - normalized * (128 - 0)}, ${
        128 - normalized * (128 - 0)
      })`;
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
