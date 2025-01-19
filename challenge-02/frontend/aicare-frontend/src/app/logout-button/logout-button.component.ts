import { Component, inject } from '@angular/core';
import { AuthService } from '@auth0/auth0-angular';
import { DOCUMENT } from '@angular/common';
import { TranslateModule } from '@ngx-translate/core';

@Component({
  selector: 'app-logout-button',
  imports: [TranslateModule],
  templateUrl: './logout-button.component.html',
  styleUrl: './logout-button.component.css',
})
export class LogoutButtonComponent {
  private auth = inject(AuthService);
  private document = inject(DOCUMENT);

  constructor() {}

  logout() {
    this.auth.logout({
      logoutParams: { returnTo: this.document.location.origin },
    });
  }
}
