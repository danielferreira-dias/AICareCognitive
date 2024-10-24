<template>
    <header class="w-full bg-indigo-600 text-white p-4 flex justify-between items-center">
        <!-- Logo and Navigation Section -->
        <div class="flex items-center space-x-6">
            <!-- Logo -->
            <img class="w-auto h-8"
                src="https://landingfoliocom.imgix.net/store/collection/clarity-dashboard/images/logo.svg" alt="Logo" />

            <!-- Navigation Link -->
            <nav>
                <a href="/" title=""
                    class="text-sm font-medium transition-all duration-200 rounded-lg hover:bg-indigo-500 px-4 py-2">
                    {{ $t('navbar.patients') }}
                </a>
            </nav>
        </div>

        <!-- Language Switcher Section -->
        <div class="flex space-x-4">
            <span :class="['flag-icon', 'flag-icon-gb', 'cursor-pointer', 'transition', getFlagClass('en')]"
                @click="changeLanguage('en')"></span>
            <span :class="['flag-icon', 'flag-icon-pt', 'cursor-pointer', 'transition', getFlagClass('pt')]"
                @click="changeLanguage('pt')"></span>
            <span :class="['flag-icon', 'flag-icon-ve', 'cursor-pointer', 'transition', getFlagClass('es')]"
                @click="changeLanguage('es')"></span>
        </div>
    </header>
</template>

<script>
export default {
    mounted() {
        const savedLanguage = localStorage.getItem('preferredLanguage');
        if (savedLanguage) {
            this.$i18n.locale = savedLanguage;
        }
    },
    methods: {
        changeLanguage(lang) {
            this.$i18n.locale = lang;
            localStorage.setItem('preferredLanguage', lang); // Save the selected language to localStorage
        },
        getFlagClass(lang) {
            return this.$i18n.locale === lang ? 'flag-selected' : 'flag-deselected';
        }
    }
};
</script>

<style scoped>
.flag-deselected {
    filter: grayscale(100%);
    /* Gray out the flag */
    opacity: 0.5;
    /* Reduce opacity */
}

.flag-deselected:hover {
    filter: none;
    /* Restore color on hover */
    opacity: 1;
}

.flag-selected {
    filter: none;
    /* Keep full color */
    opacity: 1;
    /* Ensure full opacity */
}
</style>
