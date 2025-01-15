import { ApplicationConfig, provideZoneChangeDetection } from '@angular/core';
import { provideRouter } from '@angular/router';
import { provideAuth0 } from '@auth0/auth0-angular';
import {
  provideClientHydration,
  withEventReplay,
} from '@angular/platform-browser';
import { provideHttpClient, withFetch } from '@angular/common/http';
import { routes } from './app.routes';
import { environment } from '../environments/environment';

export const appConfig: ApplicationConfig = {
  providers: [
    provideZoneChangeDetection({ eventCoalescing: true }),
    provideHttpClient(withFetch()),
    provideRouter(routes),
    provideClientHydration(withEventReplay()),
    provideAuth0({
      domain: environment.auth0.domain,
      clientId: environment.auth0.clientId,
      authorizationParams: {
        audience: environment.auth0.authorizationParams.audience,
        redirect_uri: environment.auth0.authorizationParams.redirect_uri,
      },
      httpInterceptor: {
        allowedList: [
          `${environment.auth0.authorizationParams.audience}/`,
          `${environment.auth0.authorizationParams.audience}/survey`,
          `${environment.auth0.authorizationParams.audience}/prediction-result`,
          {
            uri: `${environment.api.serverUrl}/predict`,
            tokenOptions: {
              authorizationParams: {
                audience: environment.auth0.authorizationParams.audience,
              },
            },
          },
        ],
      },
    }),
  ],
};
