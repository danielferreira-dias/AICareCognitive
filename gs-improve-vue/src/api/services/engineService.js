import axiosClient from '../clients/axiosClient';

export const getCurrentEngine =  async () => {
    const response = await axiosClient.get('/engine');
    return response.data;
};

export const updateCurrentEngine = async (changeEngineDto) => {
    const response = await axiosClient.post('/engine', changeEngineDto);
    return response.data;
};
