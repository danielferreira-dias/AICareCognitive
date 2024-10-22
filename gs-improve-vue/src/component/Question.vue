<template>
  <div class="w-[80%] md:w-2/3 h-fit bg-white rounded-lg flex flex-col text-center shadow-lg shadow-black">
      <!-- Title -->
      <div class="w-full h-fit text-2xl md:text-4xl font-bold my-10">AICare Survey</div>

      <!-- Question Display -->
      <div class="flex-1 flex flex-col items-center justify-center">
        <div v-if="currentQuestion" class="text-xl md:text-2xl mb-6">
          {{ currentQuestion.question }}
        </div>
        
        <!-- Render Options -->
        <div v-if="currentQuestion && currentQuestion.options">
          <!-- Checkbox Type -->
          <div v-if="currentQuestion.type === 'checkbox'" class="flex flex-col items-start">
            <div v-for="(option, index) in currentQuestion.options" :key="index" class="mb-2">
              <input 
                type="checkbox" 
                :id="`checkbox-${currentQuestion.id}-${index}`"
                :value="option.value"
                v-model="selectedAnswers" />
              <label :for="`checkbox-${currentQuestion.id}-${index}`" class="ml-2">
                {{ option.label }}
              </label>
            </div>
          </div>

          <!-- Radio Button Type -->
          <div v-if="currentQuestion.type === 'radiobutton'" class="flex flex-col items-start">
            <div v-for="(option, index) in currentQuestion.options" :key="index" class="mb-2">
              <input 
                type="radio" 
                :id="`radio-${currentQuestion.id}-${index}`"
                :value="option.value"
                v-model="selectedAnswer" 
                :name="`radio-group-${currentQuestion.id}`"/>
              <label :for="`radio-${currentQuestion.id}-${index}`" class="ml-2">
                {{ option.label }}
              </label>
            </div>
          </div>
        </div>

        <!-- Loading or No Questions Available -->
        <div v-else-if="loading" class="text-2xl">Loading...</div>
        <div v-else class="text-2xl">No questions available</div>
      </div>

      <!-- Buttons -->
      <div class="my-10" v-if="currentQuestion">
        <button
          @click="submitAnswer"
          class="text-white px-6 py-2 rounded-lg bg-black">
          Submit
        </button>
      </div>

      <!-- Progress Bar
      <div class="w-full h-4 bg-gray-200 overflow-hidden">
        <div 
          class="h-full bg-green-500" 
          :style="{ width: progressBarWidth + '%' }"
        ></div>
      </div> -->
    </div>
</template>

<script>
export default {
  data() {
    return {
      questions: [],           // Stores the fetched questions
      currentIndex: 0,         // Tracks the current question index
      loading: true,           // Flag for loading state
      selectedAnswer: null,    // For radio button selection
      selectedAnswers: [],     // For checkbox selections
    };
  },
  computed: {
    currentQuestion() {
      return this.questions[this.currentIndex] || null;
    },
    progressBarWidth() {
      return ((this.currentIndex) / this.questions.length) * 100;
    }
  },
  methods: {
    async fetchQuestions() {
      try {
        const response = await fetch("public/question.json");
        this.questions = await response.json();
        this.loading = false;
      } catch (error) {
        console.error("Error fetching questions:", error);
        this.loading = false;
      }
    },
    submitAnswer() {
      // Handling submission logic
      if (this.currentQuestion.type === 'radiobutton') {
        console.log('Selected radio answer:', this.selectedAnswer);
      } else if (this.currentQuestion.type === 'checkbox') {
        console.log('Selected checkbox answers:', this.selectedAnswers);
      }

      // Move to next question or finish survey
      if (this.currentIndex < this.questions.length - 1) {
        this.currentIndex++;
        this.resetSelections(); // Reset selections for the new question
      } else {
        alert("Survey complete!");
      }
    },
    resetSelections() {
      this.selectedAnswer = null;   // Reset radio button selection
      this.selectedAnswers = [];    // Reset checkbox selections
    }
  },
  mounted() {
    this.fetchQuestions();
  },
};
</script>
<style>

</style>