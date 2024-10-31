import { createAuth0Client } from "@auth0/auth0-spa-js";

let auth0Client;

export const initAuth = async () => {
  auth0Client = await createAuth0Client({
    domain: "YOUR_AUTH0_DOMAIN",
    client_id: "YOUR_AUTH0_CLIENT_ID",
    redirect_uri: window.location.origin,
  });
};

export const login = async (email, password) => {
  try {
    await auth0Client.loginWithResourceOwner({
      username: email,
      password: password,
    });
  } catch (error) {
    throw error; // Rethrow the error to be caught in the handleLogin method
  }
};

export const logout = () => {
  auth0Client.logout({
    returnTo: window.location.origin,
  });
};

export const isAuthenticated = async () => {
  return await auth0Client.isAuthenticated();
};

export const getUser = async () => {
  return await auth0Client.getUser();
};

export const handleRedirectCallback = async () => {
  await auth0Client.handleRedirectCallback();
};
