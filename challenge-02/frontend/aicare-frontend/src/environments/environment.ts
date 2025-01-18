export const environment = {
  production: false,
  auth0: {
    domain: 'aicare.eu.auth0.com',
    clientId: 'cxz3z9gQhOVPnHGLbntpnfyje6AIuEFE',
    authorizationParams: {
      redirect_uri: 'http://localhost:4200/callback',
      audience: 'aicare-api',
    }
  },
  api: {
    serverUrl: 'http://localhost:8000',
  },
};
