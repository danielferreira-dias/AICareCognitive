import { Component } from '@angular/core';
import { RouterModule } from '@angular/router'; // Importação do RouterModule
import { NavbarComponent } from './navbar/navbar.component';
import { BodyComponentComponent } from './body-component/body-component.component';
import { RouterOutlet } from '@angular/router'; // Importação do RouterOutlet para renderizar as rotas

@Component({
  selector: 'app-root',
  standalone: true,  // Marca o componente como standalone
  imports: [         // Importa os outros componentes e módulos necessários
    NavbarComponent, 
    RouterModule,    // Importa o RouterModule para usar as rotas
  ],
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css']
})
export class AppComponent {
  title = 'aicare-frontend';
}
