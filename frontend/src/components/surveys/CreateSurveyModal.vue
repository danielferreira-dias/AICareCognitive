<!-- CreateSurveyModal.vue -->
<template>
    <div class="fixed inset-0 bg-black bg-opacity-50 flex justify-center items-center">
        <div class="bg-white p-6 rounded-lg w-80 shadow-lg">
            <h2 class="text-xl font-bold mb-4">{{ $t('surveys.list.createPopupTitle') }}</h2>
            <p class="text-gray-600 mb-6">{{ $t('surveys.list.createPopupMessage') }}</p>
            <div class="flex justify-end">
                <button @click="$emit('close')"
                    class="bg-gray-400 text-white px-4 py-2 rounded-lg mr-2 hover:bg-gray-500 transition">
                    {{ $t('surveys.list.createPopupCancel') }}
                </button>
                <button @click="createSurvey"
                    class="bg-green-600 text-white px-4 py-2 rounded-lg hover:bg-green-700 transition">
                    {{ $t('surveys.list.createPopupConfirm') }}
                </button>
            </div>
        </div>
    </div>
</template>

<script>
export default {
    methods: {
        createSurvey() {
            // Emit the 'create' event to the parent component to handle the survey creation logic
            this.$emit('create', {});
        }
    }
};
</script>

<style scoped>
/* Add any additional styles here */
</style>