import axiosClient from '../clients/axiosClient';

export const getSurveysAnsweredQuestions =  async (surveyId) => {
    const response = await axiosClient.get(`/surveys/${surveyId}/answered-questions`);
    return response.data;
};

export const getSurveysNextQuestion =  async (surveyId) => {
    const response = await axiosClient.get(`/surveys/${surveyId}/next-question`);
    return response.data;
};

export const getSurveyById =  async (surveyId) => {
    const response = await axiosClient.get(`/surveys/${surveyId}`);
    return response.data;
};

export const createSurvey = async (createSurveyRequestDto) => {
    const response = await axiosClient.post('/surveys', createSurveyRequestDto);
    return response.data;
};

export const answerSurvey = async (surveyId, answerSurveyRequestDto) => {
    const response = await axiosClient.post(`/surveys/${surveyId}/answer`, answerSurveyRequestDto);
    return response.data;
};

export const deleteSurvey = async (surveyId) => {
    const response = await axiosClient.delete(`/surveys/${surveyId}`);
    return response.data;
};
