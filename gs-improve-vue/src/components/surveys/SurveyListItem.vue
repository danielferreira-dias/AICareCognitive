<template>
  <li @click="$emit('selectSurvey', survey.id)" :class="[
    'border border-gray-300 p-2 rounded-md transition-transform transform cursor-pointer',
    { 'bg-blue-100': isSelected, 'hover:scale-105': !isSelected }
  ]">
    <div class="flex flex-row justify-between items-center">
      <h3 class="text-lg font-bold text-gray-500">
        {{ $t('surveys.list.doneAt') }} {{ formatDate(survey.createDate) }}
      </h3>
      <div class="flex items-center">
        <div :class="['h-2 w-2 rounded-full', survey.hasConclusion ? 'bg-green-500' : 'bg-red-500']"></div>
      </div>
    </div>
    <p class="text-gray-600 text-sm">{{ formatDate(survey.createDate) }}</p>
  </li>
</template>

<script>
import { formatDate } from '../../utils/formatDate';

export default {
  props: {
    survey: {
      type: Object,
      required: true
    },
    isSelected: {
      type: Boolean,
      default: false
    }
  },
  methods: {
    formatDate
  }
};
</script>

<style scoped>
/* Style for the selected survey item */
.bg-blue-100 {
  background-color: #ebf8ff;
  /* Light blue background for selected item */
}
</style>
