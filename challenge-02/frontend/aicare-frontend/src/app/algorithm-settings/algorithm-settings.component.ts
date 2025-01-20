import { Component } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { AuthService } from '@auth0/auth0-angular';
import { FormsModule } from '@angular/forms';
import { CommonModule } from '@angular/common';
import { TranslateModule } from '@ngx-translate/core';
import { firstValueFrom } from 'rxjs';

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
  totalWeight: number = 0;
  isLoading: boolean | undefined;

  constructor(private http: HttpClient, private auth: AuthService) {
    this.updateTotalWeight();
    this.onAlgorithmChange();
  }

  async ngOnInit(): Promise<void> {
    await this.getWeights();
  }

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

  async getWeights(): Promise<void> {
    this.auth.getAccessTokenSilently().subscribe((token) => {
      const headers = new HttpHeaders().set('Authorization', `Bearer ${token}`);
      this.http.get('http://localhost:8000/weights', { headers }).subscribe({
        next: (response: any) => {
          this.weights = response.weights;
          console.log('WEIGHTS FIRST:', this.weights);
        },
        error: (err) => console.error('Error fetching weights', err),
      });
    });
  }

  openWeightsPopup() {
    this.auth.getAccessTokenSilently().subscribe((token) => {
      const headers = new HttpHeaders().set('Authorization', `Bearer ${token}`);
      this.http.get('http://localhost:8000/weights', { headers }).subscribe({
        next: (response: any) => {
          this.weights = response.weights;
          console.log(this.weights);
          this.showWeightsPopup = true;

          // Actualizar el total después de cargar los pesos
          this.updateTotalWeight();
        },
        error: (err) => console.error('Error fetching weights', err),
      });
    });
  }

  setDefaultWeights() {
    this.isLoading = true; // Inicia el estado de carga
    this.auth.getAccessTokenSilently().subscribe((token) => {
      const headers = new HttpHeaders().set('Authorization', `Bearer ${token}`);

      // Solicitud DELETE
      this.http.delete('http://localhost:8000/weights', { headers }).subscribe({
        next: (response: any) => {
          console.log(response.message); // Mensaje del backend
          this.weights = []; // Limpiar pesos locales
          this.updateTotalWeight(); // Actualizar el total

          // Solicitud GET
          this.http
            .get('http://localhost:8000/weights', { headers })
            .subscribe({
              next: (response: any) => {
                this.weights = response.weights; // Actualizar pesos
                this.updateTotalWeight(); // Recalcular total
                console.log('Pesos actualizados:', this.weights);
                this.isLoading = false; // Termina la carga
              },
              error: (err) => {
                console.error('Error recuperando los pesos', err);
                this.isLoading = false; // Termina la carga en caso de error
              },
            });
        },
        error: (err) => {
          console.error('Error reseteando los pesos', err);
          this.isLoading = false; // Termina la carga en caso de error
        },
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

  /*toggleBadge(activity: any) {
    activity.showBadge = true; // Show the badge
    setTimeout(() => {
      activity.showBadge = false; // Hide the badge after 1 second
    }, 1000);
  }*/

  showBadge(activity: any): void {
    activity.showBadge = true; // Mostrar el badge
  }

  hideBadge(activity: any): void {
    activity.showBadge = false; // Ocultar el badge
  }

  updateTotalWeight() {
    console.log('entre');
    console.log(this.totalWeight);
    // Sumar todos los pesos redondeados a un decimal
    this.totalWeight = parseFloat(
      this.weights
        .reduce((sum, weight) => sum + parseFloat(weight.weight || 0), 0)
        .toFixed(2) // Redondear y convertir a número
    );
  }

  validateDecimal(event: Event, weight: any) {
    const inputElement = event.target as HTMLInputElement;
    let value = inputElement.value;

    // Guardar la posición actual del cursor
    const cursorPosition = inputElement.selectionStart;

    // Permitir "0" o "0." temporalmente
    if (value === '0' || value === '0.') {
      weight.weight = value; // Actualiza el modelo sin cambios
      return;
    }

    // Eliminar cualquier carácter no numérico excepto la coma o el punto
    value = value.replace(/[^0-9,\.]/g, '');

    // Reemplazar la coma por punto
    value = value.replace(',', '.');

    // Si ya tiene un punto decimal, asegurarse de que no haya más de 2 decimales
    const [integerPart, decimalPart] = value.split('.');

    if (decimalPart && decimalPart.length > 2) {
      value = `${integerPart}.${decimalPart.slice(0, 2)}`;
    }

    // Asegurarse de que esté dentro del rango permitido (0 - 1)
    const numericValue = parseFloat(value);
    if (numericValue < 0) {
      value = '0';
    } else if (numericValue > 1) {
      value = '1';
    }

    // Actualizar el valor en el input y en el modelo
    inputElement.value = value;
    weight.weight = value;

    // Restaurar el cursor al final del campo
    setTimeout(() => {
      inputElement.setSelectionRange(value.length, value.length);
    }, 0);

    this.updateTotalWeight();
  }
}
