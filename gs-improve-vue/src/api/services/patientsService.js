import axiosClient from '../clients/axiosClient';

export const getAllPatients =  async () => {
    const response = await axiosClient.get(`/patients/`);
    return response.data;
};

export const getPatientById = async (patientId) => {
    const response = await axiosClient.get(`/patients/${patientId}`);
    return response.data;
};

export const createPatient = async (createPatientRequestDto) => {
    const response = await axiosClient.post('/patients', createPatientRequestDto);
    return response.data;
};
