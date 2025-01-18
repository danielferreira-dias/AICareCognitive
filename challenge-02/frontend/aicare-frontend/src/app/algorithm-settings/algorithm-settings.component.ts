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
          this.activities = response;
        },
        error: (err) => console.error('Error fetching activities', err),
      });
    });
  }
}
