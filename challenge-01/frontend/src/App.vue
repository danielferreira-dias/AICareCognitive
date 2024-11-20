<template>
  <MainLayout />
</template>

<script>
import MainLayout from './layouts/MainLayout.vue';
import { initAuthToken } from './auth0.js';
export default {
  setup() {
    initAuthToken(); // Initialize token on app load
  },
  components: {
    MainLayout,
  }
};
</script>
