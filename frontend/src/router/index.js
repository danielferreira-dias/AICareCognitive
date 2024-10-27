// router/index.js
import { createRouter, createWebHistory } from 'vue-router';
import Patients from '../pages/Patients.vue';
import Surveys from '../pages/Surveys.vue';

const routes = [
    { path: '/', name: 'Patients', component: Patients },
    { path: '/surveys/:patientId', name: 'Surveys', component: Surveys }
];

const router = createRouter({
    history: createWebHistory(),
    routes,
});

export default router;
