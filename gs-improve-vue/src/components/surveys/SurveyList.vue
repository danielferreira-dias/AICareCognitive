<template>
  <div class="p-6 bg-white shadow-md h-full flex flex-col">
    <div class="flex justify-between items-center mb-4">
      <h2 class="text-2xl font-semibold">{{ $t('surveys.list.title') }}</h2>
      <button @click="showCreateSurveyPopup = true"
        class="bg-blue-600 text-white px-4 py-2 rounded-lg hover:bg-blue-700 transition">
        {{ $t('surveys.list.createButton') }}
      </button>
    </div>

    <div class="flex-1 overflow-y-auto pb-16">
      <ul class="space-y-4">
        <SurveyListItem v-for="survey in surveys" :key="survey.id" :survey="survey"
          :isSelected="survey.id === selectedSurveyId" @selectSurvey="selectSurvey"
          @confirmDelete="showDeleteModal = true; surveyToDelete = survey" />
      </ul>
    </div>

    <CreateSurveyModal v-if="showCreateSurveyPopup" @close="cancelCreateSurvey" @create="addSurvey" />

    <!-- Include the Delete Survey Modal -->
    <DeleteSurveyModal v-if="showDeleteModal" :survey="surveyToDelete" @cancel="cancelDelete" @confirm="deleteSurvey" />
  </div>
</template>

<script>
import SurveyListItem from './SurveyListItem.vue';
import CreateSurveyModal from './CreateSurveyModal.vue';
import DeleteSurveyModal from './DeleteSurveyModal.vue'; // Import Delete Survey Modal
import { getSurveysByPatientId } from '../../api/services/patientService';
import { createSurvey, deleteSurvey } from '../../api/services/surveyService';

export default {
  components: {
    SurveyListItem,
    CreateSurveyModal,
    DeleteSurveyModal
  },
  data() {
    return {
      surveys: [],
      patientId: this.$route.params.patientId,
      showCreateSurveyPopup: false,
      showDeleteModal: false, // Track if delete modal should be shown
      surveyToDelete: null, // Track the survey that is to be deleted
      selectedSurveyId: null // Track the selected survey's ID
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
        this.surveys.unshift(createdSurvey); // Add new survey to the top
        this.showCreateSurveyPopup = false;
      } catch (error) {
        console.error('Error adding survey:', error);
      }
    },
    selectSurvey(survey) {
      this.selectedSurveyId = survey.id;
      this.$emit('selectSurvey', survey);
    },
    cancelDelete() {
      this.showDeleteModal = false;
      this.surveyToDelete = null;
    },
    async deleteSurvey(survey) {
      try {
        await deleteSurvey(survey.id);
        this.surveys = this.surveys.filter(s => s.id !== survey.id); // Remove survey from the list
        this.cancelDelete();
      } catch (error) {
        console.error('Error deleting survey:', error);
      }
    }
  }
};
</script>
