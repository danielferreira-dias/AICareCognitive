<template>
  <div v-if="visible" class="fixed inset-0 flex items-center justify-center bg-black bg-opacity-50 z-50"
    @click.self="closeModal">
    <div class="bg-white p-6 rounded-lg shadow-lg max-w-md w-full relative max-h-screen overflow-y-auto">
      <h2 class="text-lg font-semibold mb-4">{{ $t(`surveys.activities.${activity.description}`) }}</h2>

      <!-- Loading message when data is still being fetched -->
      <p v-if="loading" class="text-gray-500">Loading justifications...</p>

      <!-- Justifications list when data is loaded -->
      <ul v-else class="space-y-4">
        <li v-for="(justification, index) in justifications" :key="index" class="bg-gray-100 p-3 rounded-lg shadow-sm">
          <!-- Justification Response -->
          <div class="flex items-start text-gray-700">
            <span class="mr-2 text-blue-500">&#8226;</span>
            {{ $t(`surveys.justifications.${type}.${justification.response}`) }}
          </div>

          <!-- Nested list for each rule triggered, shown only when showAllRules is true -->
          <ul v-if="showAllRules && justification.rulesTriggered && justification.rulesTriggered.length"
            class="pl-6 mt-2 space-y-2">
            <li v-for="(rule, ruleIndex) in justification.rulesTriggered" :key="ruleIndex"
              class="text-gray-600 flex items-start">
              <span class="mr-2 text-green-500">&#8226;</span>
              {{ rule }}
            </li>
          </ul>
        </li>
      </ul>

      <!-- Toggle Rules and Close buttons, only show Toggle if any justification has triggered rules -->
      <div class="flex justify-end mt-4 space-x-2">
        <button v-if="hasTriggeredRules" @click="toggleAllRules" class="bg-gray-200 text-blue-500 px-4 py-2 rounded">
          {{ showAllRules ? $t(`surveys.justifications.${type}.hideRules`) :
            $t(`surveys.justifications.${type}.showRules`) }}
        </button>
        <button @click="closeModal" class="bg-blue-500 text-white px-4 py-2 rounded">
          {{ $t("surveys.justifications.closeForm") }}
        </button>
      </div>
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
      loading: false,
      showAllRules: false, // Global toggle for showing all rules, initially hidden
    };
  },
  computed: {
    hasTriggeredRules() {
      // Checks if any justification has triggered rules
      return this.justifications.some(justification => justification.rulesTriggered && justification.rulesTriggered.length > 0);
    }
  },
  methods: {
    closeModal() {
      this.$emit('close');
      this.resetDialog();
    },
    toggleAllRules() {
      this.showAllRules = !this.showAllRules; // Toggle the display of all rules
    },
    async fetchJustification(surveyId, activity, type) {
      this.loading = true;
      try {
        const activityToCheck = {
          activityName: activity.description,
        };
        // Fetch justifications with showRules property for each
        this.justifications = (await getJustifications(surveyId, type, activityToCheck)).map(justification => ({
          ...justification,
          showRules: false,
        }));
      } catch (error) {
        console.error('Error fetching justification:', error);
        this.justifications = [{ response: 'Error loading justification.', showRules: false }];
      } finally {
        this.loading = false;
      }
    },
    resetDialog() {
      this.justifications = [];
      this.loading = false;
      this.showAllRules = false; // Reset showAllRules on close
    },
  },
  watch: {
    visible(newVisibility) {
      if (newVisibility) {
        this.fetchJustification(this.surveyId, this.activity, this.type);
      }
    },
  },
};
</script>
