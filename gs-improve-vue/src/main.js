import { createApp } from 'vue'
import './style.css'
import './index.css'
import { library } from '@fortawesome/fontawesome-svg-core';
import App from './App.vue'
import { faTrashAlt, faComments } from '@fortawesome/free-solid-svg-icons';
import { FontAwesomeIcon } from '@fortawesome/vue-fontawesome';
import router from './router'; // Import the router

library.add(faTrashAlt, faComments);

createApp(App)
  .component('font-awesome-icon', FontAwesomeIcon)
  .use(router) 
  .mount('#app');
