<template>
  <div v-if="visible" class="fixed inset-0 flex items-center justify-center bg-black bg-opacity-50 z-50">
    <div class="bg-white p-6 rounded-lg shadow-lg max-w-md w-full">
      <h2 class="text-lg font-semibold mb-4">{{ $t(`surveys.activities.${activity.description}`) }}</h2>
      
      <!-- Loading message when data is still being fetched -->
      <p v-if="loading" class="text-gray-500">Loading justifications...</p>
      
      <!-- Justifications list when data is loaded -->
      <p v-else v-for="(justification, index) in justifications" :key="index" class="text-gray-700">
        {{ $t(`surveys.justifications.${justification.response}`) }}
      </p>
      
      <button @click="closeModal" class="mt-4 bg-blue-500 text-white px-4 py-2 rounded">
        Close
      </button>
    </div>
  </div>
</template>

<script>
import { getJustifications } from '../../api/services/surveyService';

export default {
  props: {
    visible: {
      type: Boolean,
      required: true
    },
    activity: {
      type: Object,
      required: true
    },
    type: {
      type: String,
      required: true
    },
    surveyId: {
      type: String,
      required: true
    }
  },
  data() {
    return {
      justifications: [],
      loading: false, // Add loading state
    };
  },
  methods: {
    closeModal() {
      this.$emit('close');
    },
    async fetchJustification(surveyId, activity, type) {
      this.loading = true; // Set loading to true when fetching begins
      try {
        const activityToCheck = {
          activityName: activity.description,
        };
        this.justifications = await getJustifications(surveyId, type, activityToCheck);
      } catch (error) {
        console.error('Error fetching justification:', error);
        this.justifications = ['Error loading justification.']; // Display error message in justifications
      } finally {
        this.loading = false; // Set loading to false when fetching completes
      }
    },
  },
  watch: {
    activity(newActivity) {
      if (this.visible) {
        this.fetchJustification(this.surveyId, newActivity, this.type);
      }
    },
  },
};
</script>

<style scoped>
/* Additional styling for the modal */
</style>
