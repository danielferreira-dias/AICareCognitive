<template>
    <div class="flex flex-col h-screen overflow-hidden">
        <Header />
        <div class="flex-1 flex overflow-hidden">
            <router-view class="flex-1 overflow-hidden" />
        </div>
    </div>
</template>

<script>
import Header from '../components/common/Header.vue';

export default {
    components: {
        Header
    }
};
</script>

<style scoped>
.flex-1 {
    flex: 1;
}
</style>