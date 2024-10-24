<template>
  <div class="flex flex-row h-screen w-full overflow-hidden">
    <div class="w-[40%] h-full overflow-y-auto">
      <!-- Pass down a function to set selected survey -->
      <SurveyList :patientId="patientId" @selectSurvey="selectSurvey" />
    </div>
    <div class="flex-1 h-full flex flex-col overflow-hidden">
      <!-- Only show SurveyChat if a survey is selected -->
      <SurveyChat v-if="selectedSurveyId" :surveyId="selectedSurveyId" />
    </div>
  </div>
</template>

<script>
import SurveyChat from '../components/surveys/SurveyChat.vue';
import SurveyList from '../components/surveys/SurveyList.vue';

export default {
  name: 'Surveys',
  components: {
    SurveyChat,
    SurveyList
  },
  data() {
    return {
      selectedSurveyId: null
    };
  },
  props: {
    patientId: {
      type: String,
      required: true
    }
  },
  methods: {
    selectSurvey(surveyId) {
      this.selectedSurveyId = surveyId;
    }
  }
};
</script>

<style scoped>
/* Add any additional styles here */
</style>
