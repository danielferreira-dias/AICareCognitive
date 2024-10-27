<template>
    <div :class="messageContainerClass">
        <div :class="messageClass">
            {{ text }}
        </div>
    </div>
</template>

<script>
export default {
    props: {
        type: {
            type: String,
            required: true
        },
        text: {
            type: String,
            required: true
        }
    },
    computed: {
        messageClass() {
            return this.type === 'question'
                ? 'bg-blue-500 text-white p-4 rounded-lg max-w-xs'
                : 'bg-gray-200 text-black p-4 rounded-lg max-w-xs';
        },
        messageContainerClass() {
            return this.type === 'question'
                ? 'flex justify-start w-full'
                : 'flex justify-end w-full';
        }
    }
};
</script>

<style scoped>
/* Add any additional styles here if needed */
</style>