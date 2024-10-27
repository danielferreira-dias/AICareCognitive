<template>
  <div class="p-6 bg-white shadow-md rounded-lg h-full">
    <!-- Loading state -->
    <div v-if="loading" class="text-center text-lg">Loading conclusions...</div>

    <!-- Accepted Activities Section -->
    <div v-else-if="activities.length > 0">
      <p class="text-gray-700 mb-4">{{ $t('surveys.conclusions.recommendedActivities') }}</p>

      <!-- Accepted activities pills (green) -->
      <div class="flex flex-wrap gap-2">
        <div v-for="(activity, index) in activities" :key="index"
          class="px-4 py-2 bg-green-100 text-green-800 rounded-full text-sm font-semibold shadow">
          {{ $t(`surveys.activities.${activity.description}`) }}
        </div>
      </div>
    </div>

    <!-- Rejected Activities Section -->
    <div v-if="rejectedActivities.length > 0" class="mt-6">
      <p class="text-gray-700 mb-4">{{ $t('surveys.conclusions.notRecommendedActivities') }}</p>

      <!-- Rejected activities pills (red) -->
      <div class="flex flex-wrap gap-2">
        <div v-for="(activity, index) in rejectedActivities" :key="index"
          class="px-4 py-2 bg-red-100 text-red-800 rounded-full text-sm font-semibold shadow">
          {{ $t(`surveys.activities.${activity.description}`) }}
        </div>
      </div>
    </div>
  </div>
</template>

<script>
import { getSurveysNextQuestion, getRejectedActivities } from '../../api/services/surveyService';

export default {
  props: {
    survey: {
      type: Object,
      required: true
    }
  },
  data() {
    return {
      activities: [], // Accepted activities
      rejectedActivities: [], // Rejected activities
      loading: true
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
    }
  },
  methods: {
    async fetchActivities(surveyId) {
      this.loading = true;
      try {
        const result = await getSurveysNextQuestion(surveyId);
        this.activities = result.conclusion.activities || [];

        // Fetch rejected activities
        const rejectedActivities = await getRejectedActivities(surveyId);
        this.rejectedActivities = rejectedActivities || [];
      } catch (error) {
        console.error('Error fetching activities:', error);
      } finally {
        this.loading = false;
      }
    }
  }
};
</script>

<style scoped>
/* Styling for pill containers */
.pill {
  padding: 0.5rem 1rem;
  border-radius: 9999px;
  font-weight: 600;
  font-size: 0.875rem;
  box-shadow: 0 1px 2px rgba(0, 0, 0, 0.1);
}

.bg-green-100 {
  background-color: #d4edda;
}

.text-green-800 {
  color: #155724;
}

.bg-red-100 {
  background-color: #f8d7da;
}

.text-red-800 {
  color: #721c24;
}
</style>
