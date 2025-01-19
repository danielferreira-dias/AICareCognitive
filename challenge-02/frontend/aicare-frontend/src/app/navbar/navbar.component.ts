import { Component } from '@angular/core';
import { LoginButtonComponent } from '../login-button/login-button.component';
import { LogoutButtonComponent } from '../logout-button/logout-button.component';
import { CommonModule } from '@angular/common';
import { AuthService } from '@auth0/auth0-angular';
import { TranslateModule, TranslateService } from '@ngx-translate/core';

@Component({
  selector: 'app-navbar',
  standalone: true,
  imports: [
    LoginButtonComponent,
    LogoutButtonComponent,
    CommonModule,
    TranslateModule,
  ],
  templateUrl: './navbar.component.html',
  styleUrl: './navbar.component.css',
})
export class NavbarComponent {
  menuOpen = false;

  constructor(public auth: AuthService, private translate: TranslateService) {
    this.translate.addLangs(['en', 'pt', 'es']);
    this.translate.setDefaultLang('en');
  }

  // Method to change the language
  switchLanguage(lang: string) {
    this.translate.use(lang);
    console.log(`Language changed to ${lang}`);
  }
}
