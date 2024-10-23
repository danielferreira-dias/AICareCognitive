// router/index.js
import { createRouter, createWebHistory } from 'vue-router';
import PatientList from '../component/Idosos.vue';
import Question from '../component/Question.vue';
import SurveyList from '../component/SurveyList.vue';
import ChatPage from '../component/ChatPage.vue';

const routes = [
    { path: '/', name: 'PatientList', component: PatientList },
    { path: '/question/:patientId', name: 'ChatPage', component: ChatPage }
];

const router = createRouter({
    history: createWebHistory(),
    routes,
});

export default router;
