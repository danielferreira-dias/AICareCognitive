<template>
  <div class="h-full bg-white rounded-lg flex flex-col text-center border shadow-md">

    <div class="flex-1 flex flex-col items-center p-5 bg-gray-100 rounded-b-lg overflow-hidden h-screen">
      <!-- Loading message centered -->
      <div v-if="loading" class="flex flex-1 justify-center items-center">
        <p class="text-2xl">Loading...</p>
      </div>

      <!-- Chat history container with scroll and max height -->
      <div v-else ref="chatContainer" class="flex-1 w-full overflow-y-auto mb-4 pb-16">
        <!-- Render chat history -->
        <ChatMessage v-for="(message, index) in chatHistory" :key="index" :type="message.type"
          :text="$t(`surveys.chat.${message.type}s.${message.text}`)" />

        <!-- Render possible answers aligned to the right -->
        <div v-if="currentResult && currentResult.question.possibleAnswers" class="flex justify-end w-full mb-4">
          <div class="flex space-x-2">
            <AnswerButton v-for="(answer, index) in currentResult.question.possibleAnswers" :key="index"
              :textKey="answer" :text="$t(`surveys.chat.answers.${answer}`)" :questionId="currentResult.question.id"
              :onAnswer="selectAnswer" />
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<script>
import { getSurveysAnsweredQuestions, getSurveysNextQuestion, answerSurvey } from '../../api/services/surveyService';
import ChatMessage from './ChatMessage.vue';
import AnswerButton from './AnswerButton.vue';

export default {
  props: {
    survey: {
      type: Object,
      required: true
    }
  },
  components: {
    ChatMessage,
    AnswerButton
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
  watch: {
    survey(newSurvey, oldSurvey) {
      if (newSurvey !== oldSurvey) {
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
        this.$nextTick(this.scrollToBottom); // Scroll to bottom after loading history
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
        this.currentResult = null;
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
      if (result && result.type === "question") {
        this.addToChatHistory(result.type, result.question.text, result.question.id);
        this.currentResult = result;
      } else {
        if (!this.survey.hasConclusion) {
          this.$emit('updateHasConclusion', true);
        }
      }
    },
    addToChatHistory(type, text, questionId) {
      this.chatHistory.push({ type, text, questionId });
      this.$nextTick(this.scrollToBottom); // Ensure to scroll down when a new message is added
    },
    scrollToBottom() {
      const container = this.$refs.chatContainer;
      if (container) container.scrollTop = container.scrollHeight;
    },
    resetSurveyChat() {
      // Reset state and reload the survey
      Object.assign(this.$data, this.$options.data());
      this.hasEmittedConclusion = false;
      this.fetchAnsweredQuestions();
      this.fetchNextQuestion();
    }
  },
  mounted() {
    this.resetSurveyChat();
  },
};
</script>

<style scoped>
.pb-16 {
  padding-bottom: 3rem;
}
</style>
