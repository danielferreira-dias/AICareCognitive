<template>
    <div class="fixed inset-0 bg-black bg-opacity-50 flex justify-center items-center">
        <div class="bg-white p-6 rounded-lg w-96">
            <h2 class="text-xl font-bold mb-4">{{ $t('surveys.deleteSurvey.title') }}</h2>
            <p>{{ $t('surveys.deleteSurvey.message', { name: survey.name || 'this survey' }) }}</p>
            <div class="flex justify-end mt-4">
                <button @click="$emit('cancel')"
                    class="bg-gray-400 text-white px-4 py-2 rounded-lg mr-2 hover:bg-gray-500 transition">
                    {{ $t('surveys.deleteSurvey.cancel') }}
                </button>
                <button @click="$emit('confirm', survey)"
                    class="bg-red-600 text-white px-4 py-2 rounded-lg hover:bg-red-700 transition">
                    {{ $t('surveys.deleteSurvey.confirm') }}
                </button>
            </div>
        </div>
    </div>
</template>

<script>
export default {
    props: {
        survey: {
            type: Object,
            required: true
        }
    }
};
</script>

<style scoped>
/* Add any necessary styles for the modal here */
</style>