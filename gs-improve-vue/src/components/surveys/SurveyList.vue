<template>
  <div class="p-6 bg-white shadow-md h-full border-r-2 flex flex-col">
    <h2 class="text-2xl font-semibold mb-4">{{ $t('surveys.list.title') }}</h2>
    <button @click="showCreateSurveyPopup = true"
      class="bg-blue-600 text-white px-4 py-2 mb-4 rounded-lg hover:bg-blue-700 transition">
      {{ $t('surveys.list.createButton') }}
    </button>

    <!-- Make the survey list container scrollable and take available space -->
    <div class="flex-1 overflow-y-auto">
      <ul class="space-y-4">
        <SurveyListItem v-for="survey in surveys" :key="survey.id" :survey="survey"
          :isSelected="survey.id === selectedSurveyId" @selectSurvey="selectSurvey" />
      </ul>
    </div>

    <!-- Use CreateSurveyModal component -->
    <CreateSurveyModal v-if="showCreateSurveyPopup" @close="cancelCreateSurvey" @create="addSurvey" />
  </div>
</template>

<script>
import SurveyListItem from './SurveyListItem.vue';
import CreateSurveyModal from './CreateSurveyModal.vue';
import { getSurveysByPatientId } from '../../api/services/patientService';
import { createSurvey } from '../../api/services/surveyService';

export default {
  components: {
    SurveyListItem,
    CreateSurveyModal
  },
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
        console.log(surveys);
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
        this.surveys.unshift(createdSurvey); // Add new survey to the top
        this.showCreateSurveyPopup = false;
      } catch (error) {
        console.error('Error adding survey:', error);
      }
    },
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
