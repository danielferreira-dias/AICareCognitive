<template>
    <div class="p-6 bg-white shadow-md h-full border-r-2 ">
      <h2 class="text-2xl font-semibold mb-4">Survey List</h2>
      <ul class="space-y-4">
        <li
          v-for="survey in surveys"
          :key="survey.id"
          class="border border-gray-300 p-2 rounded-md transition-transform transform hover:scale-105"
        >
        <div class="flex flex-row justify-between items-center">
            <h3 class="text-lg font-bold text-gray-500">{{ survey.title }}</h3>
            <div class="flex items-center">
                <div :class="survey.active ? 'bg-green-500' : 'bg-red-500'" class="h-2 w-2 rounded-full"></div>
            </div>
        </div>
          <p class="text-gray-600 text-sm">{{ survey.date }}</p>
          <p class="text-gray-600 text-sm">Responses: {{ survey.responses }}</p>
        </li>
      </ul>
    </div>
  </template>
  
  <script>
  export default {
    data() {
      return {
        surveys: []
      };
    },
    mounted() {
      // Simulating an API call to fetch surveys
      this.fetchSurveys();
    },
    methods: {
      async fetchSurveys() {
        try {
            const response = await fetch("/surveys.json");
            this.surveys = await response.json();
        } catch (error) {
            console.error("Error fetching questions:", error);
        }        
  
      }
    },
    mounted() {
    this.fetchSurveys();
  },
  };
  </script>
  
  <style scoped>
  /* You can leave this empty as Tailwind CSS handles the styling */
  </style>
  