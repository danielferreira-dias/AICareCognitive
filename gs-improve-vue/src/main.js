import { createApp } from 'vue'
import './style.css'
import './index.css'
import { library } from '@fortawesome/fontawesome-svg-core';
import { faTrashAlt } from '@fortawesome/free-solid-svg-icons';
import { FontAwesomeIcon } from '@fortawesome/vue-fontawesome';
import App from './App.vue'

library.add(faTrashAlt);

createApp(App)
  .component('font-awesome-icon', FontAwesomeIcon)
  .mount('#app');
