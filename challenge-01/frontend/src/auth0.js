import { ref, watchEffect } from 'vue';
import { useAuth0 } from '@auth0/auth0-vue';

const accessToken = ref(null);

export const initAuthToken = () => {
  const auth0 = useAuth0();

  watchEffect(async () => {
    if (auth0.isAuthenticated.value) {
      try {
        accessToken.value = await auth0.getAccessTokenSilently();
      } catch (error) {
        console.error("Error retrieving access token:", error);
      }
    }
  });
};

export const getAccessToken = () => accessToken;
