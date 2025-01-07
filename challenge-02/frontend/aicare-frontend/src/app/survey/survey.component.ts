import { Component } from '@angular/core';
import { FormsModule } from '@angular/forms'; // Para ngModel
import { CommonModule } from '@angular/common'; // Para *ngFor y *ngIf

@Component({
  selector: 'app-survey',
  standalone: true,
  imports: [FormsModule, CommonModule], // Importar FormsModule y CommonModule
  templateUrl: './survey.component.html',
  styleUrls: ['./survey.component.css'],
})
export class SurveyComponent {
  // Arreglo de preguntas dividido en páginas
  questions = [
    [
      'What is your name?',
      'What is your age?',
      'What is your gender?',
      'What is your occupation?',
    ],
    [
      'What is your favorite color?',
      'Do you have pets?',
      'What is your hobby?',
      'Where do you live?',
    ],
    [
      'What is your dream job?',
      'Do you exercise regularly?',
      'What is your favorite food?',
      'Do you travel often?',
    ],
  ];

  // Arreglo de títulos para cada página
  pageTitles = [
    'Personal Information',
    'Lifestyle Questions',
    'Future Aspirations',
  ];

  currentPage = 0; // Página actual del formulario
  formData: { [key: string]: string } = {}; // Respuestas del formulario

  // Método para avanzar a la siguiente página
  nextPage() {
    if (this.currentPage < this.questions.length - 1) {
      this.currentPage++;
    }
  }

  // Método para retroceder a la página anterior
  prevPage() {
    if (this.currentPage > 0) {
      this.currentPage--;
    }
  }

  // Método para enviar el formulario
  submitForm() {
    console.log('Formulario enviado:', this.formData);
    alert('Formulario enviado con éxito!');
  }
}
