<template>
  <div class="p-6 bg-white shadow-md rounded-lg h-full flex flex-col overflow-y-auto">
    <!-- Loading state -->
    <div v-if="loading" class="text-center text-lg">Loading conclusions...</div>

    <!-- Accepted Activities Section -->
    <div v-else-if="activities.length > 0">
      <p class="text-gray-700 mb-4">{{ $t('surveys.conclusions.recommendedActivities') }}</p>

      <!-- Accepted activities pills (green) -->
      <div class="flex flex-wrap gap-2">
        <div v-for="(activity, index) in activities" :key="index" @click="showJustification(activity, 'whynot')"
          class="px-4 py-2 bg-green-100 text-green-800 rounded-full text-sm font-semibold shadow cursor-pointer">
          {{ $t(`surveys.activities.${activity.description}`) }}
        </div>
      </div>
    </div>

    <!-- Rejected Activities Section -->
    <div v-if="rejectedActivities.length > 0" class="mt-6">
      <p class="text-gray-700 mb-4">{{ $t('surveys.conclusions.notRecommendedActivities') }}</p>

      <!-- Rejected activities pills (red) -->
      <div class="flex flex-wrap gap-2">
        <div v-for="(activity, index) in rejectedActivities" :key="index" @click="showJustification(activity, 'why')"
          class="px-4 py-2 bg-red-100 text-red-800 rounded-full text-sm font-semibold shadow cursor-pointer">
          {{ $t(`surveys.activities.${activity.description}`) }}
        </div>
      </div>
    </div>

    <!-- Justification Modal -->
    <JustificationModal :visible="showModal" :surveyId="survey.id" :type="justificationType" :activity="selectedActivity"
      :justification="justificationText" @close="showModal = false" />
  </div>
</template>

<script>
import { getSurveysNextQuestion, getRejectedActivities } from '../../api/services/surveyService';
import JustificationModal from './JustificationModal.vue';

export default {
  components: { JustificationModal },
  props: {
    survey: {
      type: Object,
      required: true,
    },
  },
  data() {
    return {
      activities: [], // Accepted activities
      rejectedActivities: [], // Rejected activities
      loading: true,
      showModal: false,
      selectedActivity: null,
      justificationType: '',
      justificationText: '', // Text to display in the modal
    };
  },
  async mounted() {
    await this.fetchActivities(this.survey.id);
  },
  watch: {
    survey(newSurvey) {
      if (newSurvey) {
        this.fetchActivities(newSurvey.id);
      }
    },
  },
  methods: {
    async fetchActivities(surveyId) {
      this.loading = true;
      try {
        const result = await getSurveysNextQuestion(surveyId);
        this.activities = result.conclusion.activities || [];

        const rejectedActivities = await getRejectedActivities(surveyId);
        this.rejectedActivities = rejectedActivities || [];
      } catch (error) {
        console.error('Error fetching activities:', error);
      } finally {
        this.loading = false;
      }
    },
    async showJustification(activity, justificationType) {
      this.selectedActivity = activity;
      this.justificationType = justificationType;
      this.showModal = true;
    },
  },
};
</script>
