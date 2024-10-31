<template>
  <div class="flex flex-col h-screen overflow-hidden">
    <Header v-if="isAuthenticated" :email="user?.email" />
    <div class="flex-1 flex overflow-hidden">
      <!-- Only render router-view if the user is authenticated -->
      <router-view v-if="isAuthenticated" class="flex-1 overflow-y-auto" />
      <!-- Render the Login component when the user is not authenticated -->
      <Login v-else @login-success="handleLoginSuccess" />
    </div>
  </div>
</template>

<script>
import { computed, ref } from "vue";
import { initAuth, isAuthenticated } from "../auth"; // Import your auth functions
import Header from "../components/common/Header.vue";
import Login from "../components/Login.vue";

export default {
  components: {
    Header,
    Login,
  },
  setup() {
    const authenticated = ref(false);
    const user = ref(null);

    const initializeAuth = async () => {
      await initAuth();
      authenticated.value = await isAuthenticated();
    };

    const handleLoginSuccess = (userData) => {
      authenticated.value = true;
      user.value = userData; // Set user data from the Login component
    };

    const handleLogoutSuccess = () => {
      authenticated.value = false;
      user.value = null;
    };

    initializeAuth();

    return {
      authenticated,
      user,
      handleLoginSuccess,
      isAuthenticated: computed(() => authenticated.value),
    };
  },
};
</script>

<style scoped>
/* Add any additional custom styles here */
</style>
