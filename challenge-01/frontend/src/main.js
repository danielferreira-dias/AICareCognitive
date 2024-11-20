import { createApp } from 'vue'
import './style.css'
import './index.css'
import { library } from '@fortawesome/fontawesome-svg-core';
import App from './App.vue'
import { faTrashAlt, faComments } from '@fortawesome/free-solid-svg-icons';
import { FontAwesomeIcon } from '@fortawesome/vue-fontawesome';
import router from './router';
import i18n from './i18n';
import { createAuth0 } from '@auth0/auth0-vue';

library.add(faTrashAlt, faComments);

createApp(App)
  .component('font-awesome-icon', FontAwesomeIcon)
  .use(router)
  .use(i18n)
  .use(
    createAuth0({
      domain: "aicare.eu.auth0.com",
      clientId: "oBlFORUxtmUNPL29rdz6rhEq24aQBAhd",
      authorizationParams: {
        scope: 'openid profile email',
        audience: "aicare-api",
        redirect_uri: window.location.origin,
      },
    }))
  .mount('#app');
