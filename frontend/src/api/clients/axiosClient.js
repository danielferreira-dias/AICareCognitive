import axios from 'axios';
import { getAccessToken } from '../../auth0.js';

const axiosClient = axios.create({
  baseURL: process.env.NODE_ENV === 'production' ? '/api' : 'http://localhost:8080/api',
  timeout: 10000,
});

axiosClient.interceptors.request.use((config) => {
  const token = getAccessToken().value; // Retrieve token from ref
  if (token) {
    config.headers.Authorization = `Bearer ${token}`;
  }
  return config;
}, (error) => Promise.reject(error));

export default axiosClient;
