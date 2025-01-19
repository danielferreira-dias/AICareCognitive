import { Component, inject } from '@angular/core';
import { AuthService } from '@auth0/auth0-angular';
import { TranslateModule } from '@ngx-translate/core';

@Component({
  selector: 'app-login-button',
  imports: [TranslateModule],
  templateUrl: './login-button.component.html',
  styleUrl: './login-button.component.css',
})
export class LoginButtonComponent {
  private auth = inject(AuthService);

  login() {
    this.auth.loginWithRedirect({ appState: { target: '/protected' } });
  }
}
