import { createApp } from 'vue'
import './style.css'
import './index.css'
import { library } from '@fortawesome/fontawesome-svg-core';
import App from './App.vue'
import { faTrashAlt, faComments } from '@fortawesome/free-solid-svg-icons';
import { FontAwesomeIcon } from '@fortawesome/vue-fontawesome';
import router from './router';
import i18n from './i18n';

library.add(faTrashAlt, faComments);

createApp(App)
  .component('font-awesome-icon', FontAwesomeIcon)
  .use(router) 
  .use(i18n)
  .mount('#app');
