<template>
    <button @click="handleClick" class="bg-gray-200 text-black px-4 py-2 rounded-lg hover:bg-gray-300 transition">
        {{ text }}
    </button>
</template>

<script>
export default {
    props: {
        text: {
            type: String,
            required: true
        },
        questionId: {
            type: String,
            required: true
        },
        onAnswer: {
            type: Function,
            required: true
        }
    },
    methods: {
        handleClick() {
            this.onAnswer(this.questionId, this.text);
        }
    }
};
</script>

<style scoped>
/* Scoped styling if needed */
</style>