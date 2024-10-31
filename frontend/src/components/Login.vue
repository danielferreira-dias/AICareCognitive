<template>
  <div class="min-h-screen flex items-center justify-center bg-gray-100 w-full">
    <div
      class="bg-white h-fit py-20 shadow-xl shadow-black rounded-lg p-8 max-w-lg w-full text-center flex flex-col justify-center items-center">
      <img class="w-64 h-auto px-4" :src="logoAICARE" alt="Logo AICARE" />
      <h1 class="text-3xl font-bold text-gray-700 my-10">
        Login to Your Account
      </h1>
      <form @submit.prevent="handleLogin" class="w-full">
        <div class="mb-4">
          <input
            type="email"
            v-model="email"
            placeholder="Email"
            required
            class="border border-gray-300 p-2 rounded w-full" />
        </div>
        <div class="mb-4">
          <input
            type="password"
            v-model="password"
            placeholder="Password"
            required
            class="border border-gray-300 p-2 rounded w-full" />
        </div>
        <button
          type="submit"
          class="w-full bg-blue-500 text-white px-4 py-2 rounded hover:bg-blue-600">
          Login
        </button>
      </form>
    </div>
  </div>
</template>

<script>
import logoAICARE from "../assets/images/logoAICARE.png";
import {
  login,
  logout,
  isAuthenticated,
  getUser,
  initAuth,
  handleRedirectCallback,
} from "../auth.js";

export default {
  data() {
    return {
      logoAICARE,
      isAuthenticated: false,
      user: null,
      email: "",
      password: "",
      // Hardcoded user credentials
      hardcodedUser: {
        email: "test@example.com",
        password: "password123",
        name: "Test User",
      },
    };
  },
  async created() {
    await initAuth();
    // Handle the redirect callback from Auth0
    if (
      window.location.search.includes("code=") &&
      window.location.search.includes("state=")
    ) {
      await handleRedirectCallback();
      window.history.replaceState({}, document.title, "/");
    }
    // Check if user is authenticated and get user info
    this.isAuthenticated = await isAuthenticated();
    if (this.isAuthenticated) {
      this.user = await getUser();
    }
  },
  methods: {
    async handleLogin() {
      if (
        this.email === this.hardcodedUser.email &&
        this.password === this.hardcodedUser.password
      ) {
        // If credentials match, set authenticated state and user data
        this.isAuthenticated = true;
        this.user = {
          name: this.hardcodedUser.name,
          email: this.hardcodedUser.email,
        };

        // Emit the login success event with user data
        this.$emit("login-success", this.user);
      } else {
        // If credentials do not match, show an error message
        alert("Login failed. Please check your credentials.");
      }
    },
    logout,
  },
};
</script>

<style scoped>
/* Add any additional custom styles here */
</style>
