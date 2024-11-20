<template>
    <header class="w-full bg-indigo-600 text-white p-4 flex justify-between items-center">
        <!-- Logo and Navigation Section -->
        <div class="flex items-center space-x-6">
            <!-- Logo -->
            <img class="w-auto h-8 px-4" :src="logoAICARE" alt="aLogo AICARE" />

            <!-- Navigation Link -->
            <nav>
                <a v-if="isAuthenticated" href="/" title=""
                    class="text-lg font-medium transition-all duration-200 rounded-lg hover:bg-indigo-500 py-2 px-3">
                    {{ $t("navbar.patients") }}
                </a>
            </nav>
        </div>

        <!-- Dropdown and Language Switcher Section -->
        <div class="flex items-center space-x-5">
            <!-- Dropdown for Drools/Prolog -->
            <div class="relative">
                <select v-model="selectedEngine" @change="changeEngine"
                    class="bg-indigo-600 text-white pl-3 pr-8 transition-all duration-200 rounded-lg hover:bg-indigo-500 py-2 px-3">
                    <option value="prolog">Prolog</option>
                    <option value="drools">Drools</option>
                </select>
                <i class="fas fa-chevron-down absolute right-2 top-1/2 transform -translate-y-1/2 text-white"></i>
            </div>

            <!-- Display the email -->
            <span class="text-white">{{ user?.email }}</span>

            <Logout v-if="isAuthenticated" />

            <!-- Language Switcher -->
            <span :class="[
                'flag-icon',
                'flag-icon-gb',
                'cursor-pointer',
                'transition',
                getFlagClass('en'),
            ]" @click="changeLanguage('en')"></span>
            <span :class="[
                'flag-icon',
                'flag-icon-pt',
                'cursor-pointer',
                'transition',
                getFlagClass('pt'),
            ]" @click="changeLanguage('pt')"></span>
            <span :class="[
                'flag-icon',
                'flag-icon-ve',
                'cursor-pointer',
                'transition',
                getFlagClass('es'),
            ]" @click="changeLanguage('es')"></span>
        </div>
    </header>
</template>

<script>
import {
    getCurrentEngine,
    updateCurrentEngine,
} from "../../api/services/engineService";
import logoAICARE from "../../assets/images/logoAICARE.png";
import Logout from "../auth/Logout.vue";
import { useAuth0 } from '@auth0/auth0-vue';

export default {
    props: {
        email: {
            type: String,
            required: false, // Change to true if email is mandatory
        },
    },
    data() {
        return {
            logoAICARE,
            selectedEngine: "prolog", // Default to 'prolog'
        };
    },
    setup() {
        const auth0 = useAuth0();

        return {
            user: auth0.user,
            isAuthenticated: auth0.isAuthenticated,
        };
    },
    components: {
        Logout
    },
    async mounted() {
        const savedLanguage = localStorage.getItem("preferredLanguage");
        if (savedLanguage) {
            this.$i18n.locale = savedLanguage;
        }

        // Fetch the current engine from the engine service on load
        try {
            const result = await getCurrentEngine();
            this.selectedEngine = result.engine || "prolog"; // Default to 'prolog' if not set
        } catch (error) {
            console.error("Error fetching current engine:", error);
        }
    },
    methods: {
        changeLanguage(lang) {
            this.$i18n.locale = lang;
            localStorage.setItem("preferredLanguage", lang); // Save the selected language to localStorage
        },
        getFlagClass(lang) {
            return this.$i18n.locale === lang ? "flag-selected" : "flag-deselected";
        },
        async changeEngine() {
            try {
                const updateEngineDto = {
                    engine: this.selectedEngine,
                };
                await updateCurrentEngine(updateEngineDto); // Update the selected engine via the service
            } catch (error) {
                console.error("Error updating engine:", error);
            }
        },
    },
};
</script>

<style scoped>
.flag-deselected {
    filter: grayscale(100%);
    opacity: 0.5;
}

.flag-deselected:hover {
    filter: none;
    opacity: 1;
}

.flag-selected {
    filter: none;
    opacity: 1;
}

select {
    background-color: inherit;
    border: none;
    color: inherit;
    font-size: 1rem;
    cursor: pointer;
    outline: none;
    appearance: none;
}

select option {
    background-color: #4f46e5;
    color: #fff;
}
</style>
