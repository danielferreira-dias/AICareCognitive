<template>
  <div class="h-full bg-white rounded-lg flex flex-col text-center border shadow-md">
    <div class="w-full h-fit text-2xl md:text-4xl font-bold my-10">AICare Survey</div>

    <div class="flex-1 flex flex-col items-center p-5 bg-gray-100 rounded-b-lg overflow-hidden">
      <!-- Chat history container with scroll and max height -->
      <div ref="chatContainer" class="flex-1 w-full overflow-y-auto mb-4">
        <div v-for="(message, index) in chatHistory" :key="index" class="flex w-full mb-4">
          <div v-if="message.type === 'question'" class="flex justify-start w-full">
            <div class="bg-blue-500 text-white p-4 rounded-lg max-w-xs">
              {{ $t(`surveys.chat.questions.${message.text}`) }}
            </div>
          </div>
          <div v-else-if="message.type === 'answer'" class="flex justify-end w-full">
            <div class="bg-gray-200 text-black p-4 rounded-lg max-w-xs">
              {{ $t(`surveys.chat.answers.${message.text}`) }}
            </div>
          </div>
        </div>
      </div>

      <!-- Display possible answers as horizontally stacked buttons on the user's side -->
      <div v-if="currentResult && currentResult.question.possibleAnswers" class="flex justify-end w-full mb-4">
        <div class="flex space-x-2">
          <div v-for="(answer, index) in currentResult.question.possibleAnswers" :key="index" class="mb-2">
            <button @click="selectAnswer(currentResult.question.id, answer)"
              class="bg-gray-200 text-black px-4 py-2 rounded-lg hover:bg-gray-300 transition">
              {{ $t(`surveys.chat.answers.${answer}`) }}
            </button>
          </div>
        </div>
      </div>

      <div v-else-if="loading" class="text-2xl">Loading...</div>
    </div>
  </div>
</template>

<script>
import { getSurveysAnsweredQuestions, getSurveysNextQuestion, answerSurvey } from '../../api/services/surveyService';

export default {
  props: {
    survey: {
      type: Object,
      required: true
    }
  },
  data() {
    return {
      results: [],             // Stores the fetched questions
      currentIndex: 0,         // Tracks the current question index
      loading: true,           // Flag for loading state
      selectedAnswer: null,    // For radio button selection
      selectedAnswers: [],     // For checkbox selections
      chatHistory: [],         // Array to store all messages (questions and answers)
      currentResult: null
    };
  },
  computed: {
    progressBarWidth() {
      return ((this.currentIndex) / this.results.length) * 100;
    }
  },
  watch: {
    // Watch for changes to the survey prop
    survey(newSurvey, oldSurvey) {
      if (newSurvey.id !== oldSurvey.id) {
        this.resetSurveyChat(); // Call method to reset and reload
      }
    }
  },
  methods: {
    async fetchAnsweredQuestions() {
      try {
        const answeredQuestions = await getSurveysAnsweredQuestions(this.survey.id);
        answeredQuestions.forEach(answeredQuestion => {
          this.addToChatHistory("question", answeredQuestion.question.text, answeredQuestion.question.id);
          this.addToChatHistory("answer", answeredQuestion.answer.response, answeredQuestion.question.id);
        });
        this.loading = false;
      } catch (error) {
        console.error("Error fetching answered questions:", error);
        this.loading = false;
      }
    },
    async fetchNextQuestion() {
      try {
        const nextQuestion = await getSurveysNextQuestion(this.survey.id);
        this.handleResult(nextQuestion);
      } catch (error) {
        console.error("Error fetching answered questions:", error);
        this.loading = false;
      }
    },
    async selectAnswer(questionId, answer) {
      try {
        const questionAnswer = {
          questionId: questionId,
          response: answer
        };
        const createdAnswer = await answerSurvey(this.survey.id, questionAnswer);
        this.addToChatHistory("answer", createdAnswer.response, questionId);
        this.fetchNextQuestion();
      } catch (error) {
        console.error('Error answering survey:', error);
      }
    },
    handleResult(result) {
      if (result) {
        if (result.type === "question") {
          this.addToChatHistory(result.type, result.question.text, result.question.id);
          this.currentResult = result;
        }
      }
    },
    addToChatHistory(type, text, questionId) {
      this.chatHistory.push({
        type: type,
        text: text,
        questionId: questionId
      });
      this.$nextTick(() => {
        this.scrollToBottom();
      });
    },
    scrollToBottom() {
      const container = this.$refs.chatContainer;
      if (container) {
        container.scrollTop = container.scrollHeight;
      }
    },
    resetSurveyChat() {
      // Reset state and reload the survey
      this.results = [];
      this.currentIndex = 0;
      this.loading = true;
      this.selectedAnswer = null;
      this.selectedAnswers = [];
      this.chatHistory = [];
      this.currentResult = null;
      
      // Fetch new survey data
      this.fetchAnsweredQuestions();
      this.fetchNextQuestion();
    }
  },
  mounted() {
    this.resetSurveyChat(); // Initial load
  },
};
</script>

<style scoped>
/* Add any additional styles here */
</style>
