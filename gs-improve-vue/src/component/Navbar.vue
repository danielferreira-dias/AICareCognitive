<template>
    <div class="flex w-full h-64 md:w-64 md:border-r-2 md:border-r-black md:h-full flex-row md:flex-col">
        <div class="flex-row md:flex-col flex-grow pt-5 overflow-y-auto bg-white">
            <div class="flex items-center flex-shrink-0 px-4">
                <img class="w-auto h-8"
                    src="https://landingfoliocom.imgix.net/store/collection/clarity-dashboard/images/logo.svg" alt="" />
            </div>

            <div class="px-4 mt-8 flex space-x-2">
                <span class="flag-icon flag-icon-gb cursor-pointer hover:opacity-75 transition"
                    @click="changeLanguage('en')"></span>
                <span class="flag-icon flag-icon-pt cursor-pointer hover:opacity-75 transition"
                    @click="changeLanguage('pt')"></span>
                <span class="flag-icon flag-icon-ve cursor-pointer hover:opacity-75 transition"
                    @click="changeLanguage('es')"></span>
            </div>

            <div class="px-4 mt-6">
                <hr class="border-gray-200" />
            </div>

            <div class="flex flex-row md:flex-col flex-1 px-3 mt-6">
                <div class="space-y-4 flex flex-row md:flex-col">
                    <nav class="flex-1 space-y-2 flex flex-row md:flex-col">
                        <a href="/" title=""
                            class="flex items-center px-4 py-2.5 text-sm font-medium text-white transition-all duration-200 bg-indigo-600 rounded-lg group">
                            {{ $t('navbar.patients') }}
                        </a>
                    </nav>
                </div>


            </div>
        </div>
    </div>
</template>

<script>
export default {
    methods: {
        changeLanguage(lang) {
            this.$i18n.locale = lang;
        },
    },
}
</script>

<style></style>