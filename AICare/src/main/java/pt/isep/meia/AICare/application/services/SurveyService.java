package pt.isep.meia.AICare.application.services;

import lombok.var;
import org.kie.api.runtime.KieSession;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import pt.isep.meia.AICare.application.configs.DroolsConfig;
import pt.isep.meia.AICare.domain.entities.Answer;
import pt.isep.meia.AICare.domain.entities.Question;
import pt.isep.meia.AICare.domain.entities.Survey;
import pt.isep.meia.AICare.domain.model.Conclusion;
import pt.isep.meia.AICare.domain.model.Result;
import pt.isep.meia.AICare.domain.model.Evidence;
import pt.isep.meia.AICare.infrastructure.repositories.AnswersRepository;
import pt.isep.meia.AICare.infrastructure.repositories.QuestionsRepository;
import pt.isep.meia.AICare.infrastructure.repositories.SurveysRepository;

import java.io.IOException;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Service
public class SurveyService {
    private final SurveysRepository surveysRepository;
    private final DroolsConfig droolsConfig;
    private final QuestionsRepository questionsRepository;
    private final AnswersRepository answersRepository;

    @Autowired
    public SurveyService(
            DroolsConfig droolsConfig,
            SurveysRepository surveysRepository,
            QuestionsRepository questionsRepository,
            AnswersRepository answersRepository) {
        this.surveysRepository = surveysRepository;
        this.droolsConfig = droolsConfig;
        this.questionsRepository = questionsRepository;
        this.answersRepository = answersRepository;
    }

    public Survey getSurveyById(UUID surveyId) {
        var survey = surveysRepository.findById(surveyId);
        return survey.orElse(null);
    }

    public Survey createSurvey(UUID patientId) {
        var createdSurvey = surveysRepository.save(new Survey(patientId, LocalDateTime.now()));
        return createdSurvey;
    }

    public Result getNextQuestion(UUID surveyId) throws IOException {
        var survey = surveysRepository.findById(surveyId);
        if (!survey.isPresent()) {
            return null;
        }

        var question = questionsRepository.findUnansweredQuestionsBySurveyId(surveyId)
                .stream()
                .findFirst()
                .orElse(null);

        if(question != null){
            return Result.fromQuestion(question);
        }

        KieSession session = droolsConfig.getKieSession();
        if (session == null) {
            return null;
        }

        var evidences = answersRepository.findEvidencesBySurveyId(surveyId);

        session.setGlobal("surveyId", surveyId);
        session.setGlobal("evidences", evidences);
        session.fireAllRules();

        for (var obj : session.getObjects()) {
            System.out.println(obj);
        }

        Conclusion conclusion = getConclusionFromSession(session);

        if(conclusion != null){
            return Result.fromConclusion(conclusion);
        }

        Question nextQuestion = getLastQuestionFromSession(session);

        var createdQuestion = questionsRepository.save(nextQuestion);

        return Result.fromQuestion(createdQuestion);
    }

    public Answer answerQuestion(Answer answer) {
        return answersRepository.save(answer);
    }

    public List<Evidence> getAllAnsweredQuestionsById(UUID surveyId) {
        return answersRepository.findEvidencesBySurveyId(surveyId);
    }

    private Question getLastQuestionFromSession(KieSession session) {
        return session.getObjects(new org.kie.api.runtime.ClassObjectFilter(Question.class))
                .stream()
                .filter(obj -> obj instanceof Question)
                .map(obj -> (Question) obj)
                .reduce((first, second) -> second)
                .orElse(null);
    }

    private Conclusion getConclusionFromSession(KieSession session) {
        return session.getObjects(new org.kie.api.runtime.ClassObjectFilter(Conclusion.class))
                .stream()
                .filter(obj -> obj instanceof Conclusion)
                .map(obj -> (Conclusion) obj)
                .findFirst()
                .orElse(null);
    }
}
