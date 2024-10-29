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
export default {
    props: {
        type: {
            type: String,
            required: true
        },
        text: {
            type: String,
            required: true
        },
        animateTyping: {
            type: Boolean,
            default: false // Typing effect only occurs if explicitly requested
        }
    },
    data() {
        return {
            translatedText: '', // Holds the full translated text
            displayedText: '', // Holds the progressively displayed text for typing effect
            currentCharIndex: 0, // Keeps track of the typing progress
            isTyping: false // Controls the visibility of the cursor and typing effect
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
        this.renderText();
    },
    watch: {
        // Watch for changes in the current locale and update text without typing animation
        '$i18n.locale'() {
            this.updateTextWithoutTyping();
        }
    },
    methods: {
        renderText() {
            this.translatedText = this.$t(`surveys.chat.${this.text}`);
            if (this.animateTyping) {
                // Reset for typing animation if animateTyping is true
                this.displayedText = '';
                this.currentCharIndex = 0;
                this.isTyping = true;
                this.typeText();
            } else {
                // Display full text instantly if animateTyping is false
                this.displayedText = this.translatedText;
                this.isTyping = false;
            }
        },
        updateTextWithoutTyping() {
            // Update the full translated text without triggering typing animation
            this.translatedText = this.$t(`surveys.chat.${this.text}`);
            this.displayedText = this.translatedText;
            this.isTyping = false; // Ensure typing is disabled for this update
        },
        typeText() {
            if (this.currentCharIndex < this.translatedText.length) {
                this.displayedText += this.translatedText[this.currentCharIndex];
                this.currentCharIndex++;
                setTimeout(this.typeText, 17); // Increased speed by setting delay to 17 ms
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
