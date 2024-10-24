<template>
    <div
        class="flex flex-col md:flex-row items-center h-screen bg-gradient-to-t from-zinc-900 via-zinc-800 to-zinc-700">
        <Navbar />
        <router-view />
    </div>
</template>

<script>
import Navbar from '../components/common/Navbar.vue';
export default {
    components: {
        Navbar,
    }
};
</script>