// router/index.js
import { createRouter, createWebHistory } from 'vue-router';
import PatientList from '../component/Idosos.vue';
import Question from '../component/Question.vue';

const routes = [
    { path: '/', name: 'PatientList', component: PatientList },
    { path: '/question/:patientId', name: 'Question', component: Question }
];

const router = createRouter({
    history: createWebHistory(),
    routes,
});

export default router;
