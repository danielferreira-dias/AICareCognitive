<template>
    <div class="flex flex-row h-full w-full">
        <div class="w-[40%] h-full">
          <SurveyList :patientId="patientId" />
        </div>
      <div class="flex-1 h-full">
        <Question :patientId="patientId" />
      </div>
    </div>
  </template>
  
  <script>
    import Question from './Question.vue';
    import SurveyList from './SurveyList.vue';
  
  export default {
    name: 'ChatPage',
    components: {
      Question,
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
  