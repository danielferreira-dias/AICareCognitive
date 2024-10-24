package pt.isep.meia.AICare.application.services;

import lombok.var;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import pt.isep.meia.AICare.domain.entities.Answer;
import pt.isep.meia.AICare.domain.entities.Conclusion;
import pt.isep.meia.AICare.domain.entities.Survey;
import pt.isep.meia.AICare.domain.model.Justification;
import pt.isep.meia.AICare.domain.model.Result;
import pt.isep.meia.AICare.domain.model.Evidence;
import pt.isep.meia.AICare.domain.model.ResultTypeEnum;
import pt.isep.meia.AICare.infrastructure.repositories.AnswersRepository;
import pt.isep.meia.AICare.infrastructure.repositories.ConclusionsRepository;
import pt.isep.meia.AICare.infrastructure.repositories.QuestionsRepository;
import pt.isep.meia.AICare.infrastructure.repositories.SurveysRepository;

import java.io.IOException;
import java.util.List;
import java.util.UUID;

@Service
public class SurveyService {
    private final ConclusionService conclusionService;
    private final SurveysRepository surveysRepository;
    private final QuestionsRepository questionsRepository;
    private final AnswersRepository answersRepository;
    private final EngineService engineService;

    @Autowired
    public SurveyService(
            EngineService engineService,
            ConclusionService conclusionService,
            SurveysRepository surveysRepository,
            QuestionsRepository questionsRepository,
            AnswersRepository answersRepository) {
        this.engineService = engineService;
        this.conclusionService = conclusionService;
        this.surveysRepository = surveysRepository;
        this.questionsRepository = questionsRepository;
        this.answersRepository = answersRepository;
    }

    public Survey getSurveyById(UUID surveyId) {
        var survey = surveysRepository.findById(surveyId);
        return survey.orElse(null);
    }

    public Survey createSurvey(UUID patientId) {
        return surveysRepository.save(new Survey(patientId));
    }

    public Result getNextQuestion(UUID surveyId) throws IOException {
        var survey = surveysRepository.findById(surveyId);
        if (!survey.isPresent()) {
            return null;
        }

        var conclusion = conclusionService.getConclusionBySurveyId(surveyId);
        if(conclusion != null){
            return Result.fromConclusion(conclusion);
        }

        var question = questionsRepository.findUnansweredQuestionsBySurveyId(surveyId)
                .stream()
                .findFirst()
                .orElse(null);

        if(question != null){
            return Result.fromQuestion(question);
        }

        var evidences = answersRepository.findEvidencesBySurveyId(surveyId);

        var order = evidences.size() + 1;

        var result = engineService.getNextQuestion(surveyId, evidences, order);

        if(result == null){
            return null;
        }

        if(result.getType().equals(ResultTypeEnum.conclusion)){
            var createdConclusion = conclusionService.save(result.getConclusion());
            return Result.fromConclusion(createdConclusion);
        }

        var createdQuestion = questionsRepository.save(result.getQuestion());

        return Result.fromQuestion(createdQuestion);
    }

    public Answer answerQuestion(Answer answer) {
        var question = questionsRepository.findById(answer.getQuestionId());
        if (!question.isPresent()) {
            return null;
        }

        var result = engineService.postAnswer(question.get().getText(), answer.getResponse());

        if (!result) {
            return null;
        }

        return answersRepository.save(answer);
    }

    public List<Evidence> getAllAnsweredQuestionsById(UUID surveyId) {
        return answersRepository.findEvidencesBySurveyId(surveyId);
    }

    public List<Justification> getWhy(UUID surveyId) throws IOException {
        var evidences = answersRepository.findEvidencesBySurveyId(surveyId);
        return engineService.getWhy(surveyId, evidences);
    }

    public void deleteSurvey(UUID surveyId) {
        surveysRepository.deleteById(surveyId);
    }
}
