<template>
  <div v-if="visible" class="fixed inset-0 flex items-center justify-center bg-black bg-opacity-50 z-50">
    <div class="bg-white p-6 rounded-lg shadow-lg max-w-md w-full">
      <h2 class="text-lg font-semibold mb-4">{{ $t(`surveys.activities.${activity.description}`) }}</h2>

      <!-- Loading message when data is still being fetched -->
      <p v-if="loading" class="text-gray-500">Loading justifications...</p>

      <!-- Justifications list when data is loaded -->
      <ul v-else class="space-y-4">
        <li v-for="(justification, index) in justifications" :key="index" class="bg-gray-100 p-3 rounded-lg shadow-sm">
          <span class="text-gray-700 flex items-start">
            <span class="mr-2 text-blue-500">&#8226;</span>
            {{ $t(`surveys.justifications.${type}.${justification.response}`) }}
          </span>
        </li>
      </ul>

      <button @click="closeModal" class="mt-4 bg-blue-500 text-white px-4 py-2 rounded">
        {{ $t("surveys.justifications.closeForm") }}
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
/* Style for the cursor blinking animation */
.cursor {
  display: inline-block;
  width: 8px;
  background-color: currentColor;
  animation: blink 0.7s steps(2, start) infinite;
}

/* Blinking animation */
@keyframes blink {

  0%,
  100% {
    opacity: 1;
  }

  50% {
    opacity: 0;
  }
}

/* Style for justification items */
.space-y-4 {
  margin-top: 1rem;
}

.bg-gray-100 {
  background-color: #f7fafc;
}

.text-gray-700 {
  color: #4a5568;
}

.text-blue-500 {
  color: #4299e1;
}

.p-3 {
  padding: 0.75rem;
}

.shadow-sm {
  box-shadow: 0 1px 2px rgba(0, 0, 0, 0.05);
}

.rounded-lg {
  border-radius: 0.5rem;
}
</style>
