import { Component } from '@angular/core';
import { LoginButtonComponent } from '../login-button/login-button.component';
import { LogoutButtonComponent } from '../logout-button/logout-button.component';
import { CommonModule } from '@angular/common';
import { AuthService } from '@auth0/auth0-angular';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { Router } from '@angular/router';

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
  selectedLanguage: string = 'en';

  constructor(
    public auth: AuthService,
    private translate: TranslateService,
    private router: Router
  ) {
    this.translate.addLangs(['en', 'pt', 'es']);
    this.translate.setDefaultLang('en');
  }

  // Method to change the language
  switchLanguage(lang: string) {
    this.translate.use(lang);
    this.selectedLanguage = lang;
    console.log(`Language changed to ${lang}`);
  }

  goBack() {
    console.log('entre');
    this.router.navigate(['/']); // Adjust the route if necessary
  }
}
