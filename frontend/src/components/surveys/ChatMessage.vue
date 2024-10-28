<template>
    <div :class="messageContainerClass">
        <div :class="messageClass">
            <!-- Display typing effect only if typing is still in progress; otherwise, show the full translated text -->
            <span v-if="isTyping">{{ displayedText }}</span>
            <span v-else>{{ translatedText }}</span>
            <span v-if="isTyping" class="cursor">/</span>
        </div>
    </div>
</template>

<script>
import { useI18n } from 'vue-i18n';

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
            translatedText: '', // Holds the full translated text
            displayedText: '', // Holds the progressively displayed text for typing effect
            currentCharIndex: 0, // Keeps track of the typing progress
            isTyping: true // Controls the visibility of the cursor and typing effect
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
        this.startTypingEffect();
    },
    watch: {
        // Watch for changes in the current locale and reset typing effect if it changes
        '$i18n.locale'() {
            this.startTypingEffect();
        }
    },
    methods: {
        startTypingEffect() {
            this.translatedText = this.$t(`surveys.chat.${this.text}`);
            this.displayedText = ''; // Reset displayed text
            this.currentCharIndex = 0; // Reset typing index
            this.isTyping = true; // Reset typing status
            this.typeText(); // Start typing effect
        },
        typeText() {
            if (this.currentCharIndex < this.translatedText.length) {
                this.displayedText += this.translatedText[this.currentCharIndex];
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
    0%,
    100% {
        opacity: 1;
    }

    50% {
        opacity: 0;
    }
}
</style>
