import axios from 'axios';

const axiosClient = axios.create({
    baseURL: process.env.NODE_ENV === 'production' ? '/api' : 'http://localhost:8080/api',
    timeout: 10000,
});

export default axiosClient;
