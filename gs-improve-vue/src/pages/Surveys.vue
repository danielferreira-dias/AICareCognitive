<template>
  <div class="flex flex-row h-screen w-full overflow-hidden">
    <div class="w-[40%] h-full overflow-y-auto">
      <!-- Pass down a function to set selected survey -->
      <SurveyList :patientId="patientId" @selectSurvey="selectSurvey" />
    </div>
    <div class="flex-1 h-full flex flex-col overflow-hidden">
      <!-- Only show SurveyChat if a survey is selected -->
      <SurveyChat v-if="selectedSurvey" :survey="selectedSurvey" />
    </div>
  </div>
</template>

<script>
import SurveyChat from '../components/SurveyChat.vue';
import SurveyList from '../components/SurveyList.vue';

export default {
  name: 'Surveys',
  components: {
    SurveyChat,
    SurveyList
  },
  data() {
    return {
      selectedSurvey: null // This will store the selected survey
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
      this.selectedSurvey = survey; // Update selected survey
    }
  }
};
</script>

<style scoped>
/* Add any additional styles here */
</style>
