<template>
    <div class="flex w-full h-64 md:w-64 md:border-r-2 md:border-r-black md:h-full flex-row md:flex-col">
        <div class="flex-row md:flex-col flex-grow pt-5 overflow-y-auto bg-white">
            <div class="flex items-center flex-shrink-0 px-4">
                <img class="w-auto h-8" src="https://landingfoliocom.imgix.net/store/collection/clarity-dashboard/images/logo.svg" alt="" />
            </div>

            <div class="px-4 mt-8">
                <label for="" class="sr-only"> Search </label>
                <div class="relative">
                    <div class="absolute inset-y-0 left-0 flex items-center pl-3 pointer-events-none">
                        <svg class="w-5 h-5 text-gray-400" xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" stroke="currentColor" stroke-width="2">
                            <path stroke-linecap="round" stroke-linejoin="round" d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z"></path>
                        </svg>
                    </div>

                    <input type="search" name="" id="" class="block w-full py-2 pl-10 border border-gray-300 rounded-lg focus:ring-indigo-600 focus:border-indigo-600 sm:text-sm" placeholder="Search here" />
                </div>
            </div>

            <div class="px-4 mt-6">
                <hr class="border-gray-200" />
            </div>

            <div class="flex flex-row md:flex-col flex-1 px-3 mt-6">
                <div class="space-y-4 flex flex-row md:flex-col">
                    <nav class="flex-1 space-y-2 flex flex-row md:flex-col">
                        <a href="#" title="" class="flex items-center px-4 py-2.5 text-sm font-medium text-white transition-all duration-200 bg-indigo-600 rounded-lg group">
                            
                            Page 1
                        </a>

                        <a href="#" class="flex items-center px-4 py-2.5 text-sm font-medium transition-all duration-200 text-gray-900 hover:text-white rounded-lg hover:bg-indigo-600 group">
                            
                            Page 2
                        </a>

                        
                    </nav>

                    
                 
                </div>

                
            </div>
        </div>
    </div>
</template>

<script>
export default {

}
</script>

<style>

</style>