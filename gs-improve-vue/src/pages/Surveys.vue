<template>
  <div class="flex flex-row h-full w-full">
    <div class="w-[40%] h-full">
      <SurveyList :patientId="patientId" />
    </div>
    <div class="flex-1 h-full">
      <SurveyChat :patientId="patientId" />
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
  props: {
    patientId: {
      type: String,
      required: true
    }
  }
};
</script>

<style scoped>
/* Add any additional styles here */
</style>