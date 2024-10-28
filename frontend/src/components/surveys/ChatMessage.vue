<template>
    <div :class="messageContainerClass">
        <div :class="messageClass">
            <span>{{ displayedText }}</span><span v-if="isTyping" class="cursor">/</span>
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
    data() {
        return {
            displayedText: '', // stores the text currently being displayed
            currentCharIndex: 0, // keeps track of the typing progress
            isTyping: true // controls the visibility of the cursor
        };
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
    },
    mounted() {
        this.typeText();
    },
    methods: {
        typeText() {
            if (this.currentCharIndex < this.text.length) {
                this.displayedText += this.text[this.currentCharIndex];
                this.currentCharIndex++;
                setTimeout(this.typeText, 50); // Adjust the delay for typing speed
            } else {
                this.isTyping = false; // Stop showing the cursor when typing is complete
            }
        }
    }
};
</script>

<style scoped>
.cursor {
    display: inline-block;
    width: 8px;
    background-color: currentColor;
    animation: blink 0.7s steps(2, start) infinite;
}

/* Blinking animation */
@keyframes blink {
    0%, 100% {
        opacity: 1;
    }
    50% {
        opacity: 0;
    }
}
</style>
