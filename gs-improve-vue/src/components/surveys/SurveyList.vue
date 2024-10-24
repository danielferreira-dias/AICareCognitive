<template>
  <div class="p-6 bg-white shadow-md h-full border-r-2">
    <h2 class="text-2xl font-semibold mb-4">{{ $t('surveys.list.title') }}</h2>
    <button @click="showCreateSurveyPopup = true"
      class="bg-blue-600 text-white px-4 py-2 mb-4 rounded-lg hover:bg-blue-700 transition">
      {{ $t('surveys.list.createButton') }}
    </button>

    <ul class="space-y-4">
      <li
        v-for="survey in surveys"
        :key="survey.id"
        @click="selectSurvey(survey)"
        :class="[
          'border border-gray-300 p-2 rounded-md transition-transform transform cursor-pointer',
          { 'bg-gray-200': survey.id === selectedSurveyId, 'hover:scale-105': survey.id !== selectedSurveyId }
        ]"
      >
        <div class="flex flex-row justify-between items-center">
          <h3 class="text-lg font-bold text-gray-500">
            {{ $t('surveys.list.doneAt') }} {{ formatDate(survey.createDate) }}
          </h3>
          <div class="flex items-center">
            <div :class="['h-2 w-2 rounded-full', survey.active ? 'bg-green-500' : 'bg-red-500']"></div>
          </div>
        </div>
        <p class="text-gray-600 text-sm">{{ formatDate(survey.createDate) }}</p>
      </li>
    </ul>

    <!-- Create Survey Popup -->
    <div v-if="showCreateSurveyPopup" class="fixed inset-0 bg-black bg-opacity-50 flex justify-center items-center">
      <div class="bg-white p-6 rounded-lg w-80 shadow-lg">
        <h2 class="text-xl font-bold mb-4">{{ $t('surveys.list.createPopupTitle') }}</h2>
        <p class="text-gray-600 mb-6">{{ $t('surveys.list.createPopupMessage') }}</p>
        <div class="flex justify-end">
          <button @click="cancelCreateSurvey"
            class="bg-gray-400 text-white px-4 py-2 rounded-lg mr-2 hover:bg-gray-500 transition">
            {{ $t('surveys.list.createPopupCancel') }}
          </button>
          <button @click="addSurvey" class="bg-green-600 text-white px-4 py-2 rounded-lg hover:bg-green-700 transition">
            {{ $t('surveys.list.createPopupConfirm') }}
          </button>
        </div>
      </div>
    </div>
  </div>
</template>

<script>
import { getSurveysByPatientId } from '../../api/services/patientService';
import { createSurvey } from '../../api/services/surveyService';
import { formatDate } from '../../utils/formatDate';

export default {
  data() {
    return {
      surveys: [],
      patientId: this.$route.params.patientId,
      showCreateSurveyPopup: false,
      selectedSurveyId: null, // Track the selected survey's ID
    };
  },
  mounted() {
    this.fetchSurveys(this.patientId);
  },
  methods: {
    async fetchSurveys(patientId) {
      try {
        const surveys = await getSurveysByPatientId(patientId);
        this.surveys = surveys;
      } catch (error) {
        console.error('Error fetching surveys:', error);
      }
    },
    cancelCreateSurvey() {
      this.showCreateSurveyPopup = false;
    },
    async addSurvey() {
      try {
        const newSurvey = {
          patientId: this.patientId
        };
        const createdSurvey = await createSurvey(newSurvey);
        this.surveys.push(createdSurvey);
        this.showCreateSurveyPopup = false;
      } catch (error) {
        console.error('Error adding survey:', error);
      }
    },
    formatDate,
    selectSurvey(survey) {
      this.selectedSurveyId = survey.id; // Update selected survey ID
      this.$emit('selectSurvey', survey); // Emit event to parent with selected survey
    }
  }
};
</script>

<style scoped>
/* Your existing styles or leave empty for Tailwind CSS */
</style>
