// router/index.js
import { createRouter, createWebHistory } from 'vue-router';
import { authGuard } from "@auth0/auth0-vue";
import Patients from '../pages/Patients.vue';
import Surveys from '../pages/Surveys.vue';

const routes = [
    { path: '/', name: 'Patients', component: Patients, beforeEnter: authGuard },
    { path: '/surveys/:patientId', name: 'Surveys', component: Surveys, beforeEnter: authGuard }
];

const router = createRouter({
    history: createWebHistory(),
    routes,
});

export default router;
