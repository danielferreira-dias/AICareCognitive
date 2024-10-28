<template>
  <div class="flex flex-col md:flex-row h-screen w-full overflow-hidden">
    <div class="w-full md:w-[40%] md:h-full overflow-y-auto">
      <!-- Pass down a function to set selected survey -->
      <SurveyList :patientId="patientId" @selectSurvey="selectSurvey" />
    </div>

    <!-- Main content area for SurveyChat and Conclusions, with tabs -->
    <div class="flex-1 h-full flex flex-col overflow-hidden bg-white rounded-lg text-center border shadow-md">
      <!-- Render tab buttons if a survey is selected -->
      <div v-if="selectedSurvey" class="flex justify-center border-b">
        <button v-if="hasConclusion" @click="currentTab = 'conclusions'"
          :class="{ 'border-b-2 font-bold': currentTab === 'conclusions' }" class="px-4 py-2 text-2xl">
          {{ $t('surveys.tabs.conclusions') }}
        </button>
        <button @click="currentTab = 'survey'" :class="{ 'border-b-2 font-bold': currentTab === 'survey' }"
          class="px-4 py-2 text-2xl">
          {{ $t('surveys.tabs.survey') }}
        </button>
      </div>

      <!-- Conditional rendering of SurveyChat and Conclusions based on selected tab -->
      <div class="flex-1 overflow-hidden">
        <SurveyChat v-if="currentTab === 'survey' && selectedSurvey" :survey="selectedSurvey"
          @updateHasConclusion="updateHasConclusion" />
        <Conclusions v-if="currentTab === 'conclusions' && selectedSurvey && hasConclusion"
          :survey="selectedSurvey" />
      </div>
    </div>
  </div>
</template>

<script>
import SurveyChat from '../components/surveys/SurveyChat.vue';
import SurveyList from '../components/surveys/SurveyList.vue';
import Conclusions from '../components/surveys/Conclusions.vue';

export default {
  name: 'Surveys',
  components: {
    SurveyChat,
    SurveyList,
    Conclusions
  },
  data() {
    return {
      selectedSurvey: null,
      currentTab: 'survey', // Default tab set to 'survey'
      hasConclusion: false // Track if selected survey has conclusions
    };
  },
  props: {
    patientId: {
      type: String,
      required: true
    }
  },
  methods: {
    selectSurvey(survey) {
      this.selectedSurvey = survey;
      if (survey) {
        this.hasConclusion = survey.hasConclusion || false;
        this.currentTab = this.hasConclusion ? 'conclusions' : 'survey';
      } else {
        this.hasConclusion = false;
        this.currentTab = 'survey'; // Reset to default tab
      }
    },
    updateHasConclusion(hasConclusion) {
      this.hasConclusion = hasConclusion;
      if (hasConclusion) {
        this.currentTab = 'conclusions';
      }
    }
  }
};
</script>

<style scoped>
/* Additional styles here if needed */
</style>
