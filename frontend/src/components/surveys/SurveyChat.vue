<template>
  <div class="h-full bg-white rounded-lg flex flex-col text-center border shadow-md">

    <div class="flex-1 flex flex-col items-center p-5 bg-gray-100 rounded-b-lg overflow-hidden h-screen">
      <!-- Loading message centered -->
      <div v-if="loading" class="flex flex-1 justify-center items-center">
        <p class="text-2xl">Loading...</p>
      </div>

      <!-- Chat history container with scroll and max height -->
      <div v-else ref="chatContainer" class="flex-1 w-full overflow-y-auto mb-4 pb-16 pr-4">
        <!-- Render chat history -->
        <ChatMessage v-for="(message, index) in chatHistory" :key="index" :type="message.type"
          :text="`${message.type}s.${message.text}`" :animateTyping="message.animateTyping" />

        <!-- Render possible answers aligned to the right -->
        <div v-if="currentResult && currentResult.question.possibleAnswers" class="flex justify-end w-full mt-10 mb-4">
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
      loading: true,           // Flag for loading state
      chatHistory: [],         // Array to store all messages (questions and answers)
      currentResult: null
    };
  },
  watch: {
    survey(newSurvey, oldSurvey) {
      if (newSurvey !== oldSurvey) {
        this.resetSurveyChat(); // Reset and reload on survey change
      }
    }
  },
  methods: {
    async fetchAnsweredQuestions() {
      try {
        const answeredQuestions = await getSurveysAnsweredQuestions(this.survey.id);
        answeredQuestions.forEach(answeredQuestion => {
          this.addToChatHistory("question", answeredQuestion.question.text, answeredQuestion.question.id, false);
          this.addToChatHistory("answer", answeredQuestion.answer.response, answeredQuestion.question.id, false);
        });
      } catch (error) {
        console.error("Error fetching answered questions:", error);
      } finally {
        this.loading = false;
        this.$nextTick(this.scrollToBottom); // Scroll after loading all history
      }
    },
    async fetchNextQuestion() {
      try {
        const nextQuestion = await getSurveysNextQuestion(this.survey.id);
        this.handleResult(nextQuestion);
      } catch (error) {
        console.error("Error fetching next question:", error);
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
        this.addToChatHistory("answer", createdAnswer.response, questionId, false);
        this.fetchNextQuestion(); // Fetch next question after answer
      } catch (error) {
        console.error('Error answering survey:', error);
      }
    },
    handleResult(result) {
      if (result && result.type === "question") {
        this.addToChatHistory(result.type, result.question.text, result.question.id, true);
        this.currentResult = result;
      } else {
        if (!this.survey.hasConclusion) {
          this.survey.hasConclusion = true
          this.$emit('updateHasConclusion', true);
        }
      }
    },
    addToChatHistory(type, text, questionId, animateTyping) {
      this.chatHistory.push({ type, text, questionId, animateTyping });
      this.$nextTick(this.scrollToBottom); // Ensure scroll after adding
    },
    scrollToBottom() {
      const container = this.$refs.chatContainer;
      if (container) {
        container.scrollTop = container.scrollHeight; // Always scroll to the bottom
      }
    },
    resetSurveyChat() {
      this.chatHistory = []; // Clear chat history
      this.loading = true;   // Set loading state
      this.fetchAnsweredQuestions().then(() => {
        this.fetchNextQuestion(); // Only fetch next question after fetching history
    });
    }
  },
  mounted() {
    this.resetSurveyChat(); // Initial setup on mount
  }
};
</script>

<style scoped>
.pb-16 {
  padding-bottom: 3rem;
}
</style>
