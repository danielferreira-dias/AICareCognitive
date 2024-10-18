package pt.isep.meia.AICare.application.services;

import lombok.var;
import org.kie.api.runtime.KieSession;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import pt.isep.meia.AICare.application.configs.KnowledgeBase;
import pt.isep.meia.AICare.models.Answer;
import pt.isep.meia.AICare.models.Question;
import pt.isep.meia.AICare.models.SurveySession;
import pt.isep.meia.AICare.infrastructure.SurveySessionsRepository;

import java.time.LocalDateTime;
import java.util.UUID;

@Service
public class SurveySessionService {
    private final SurveySessionsRepository surveySessionsRepository;
    private final KnowledgeBase knowledgeBase;

    @Autowired
    public SurveySessionService(SurveySessionsRepository surveySessionsRepository, KnowledgeBase knowledgeBase) {
        this.surveySessionsRepository = surveySessionsRepository;
        this.knowledgeBase = knowledgeBase;
    }

    public String startSurveySession(UUID patientId) {
        // Create the survey session entity
        var session = new SurveySession(patientId, LocalDateTime.now());
        var savedSession = surveySessionsRepository.save(session);

        // Create a new KieSession
        var kieSession = knowledgeBase.createNewSession();

        // Set the global "nextQuestion" and insert initial facts
        kieSession.setGlobal("nextQuestion", null);
        kieSession.insert(savedSession);
        kieSession.fireAllRules();

        // Retrieve the first question
        var firstQuestion = knowledgeBase.getNextQuestion(kieSession);
        kieSession.dispose();

        // Return the first question text, or a message if none was found
        if (firstQuestion == null) {
            return "No questions available. Survey completed.";
        }

        return firstQuestion.getText();
    }

    public String answerQuestion(UUID sessionId, Answer answer) {
        // Retrieve SurveySession from the database
        var session = surveySessionsRepository.findById(sessionId).orElse(null);
        if (session == null) {
            return "Session not found.";
        }

        // Create a new KieSession and process the answer
        var kieSession = knowledgeBase.createNewSession();
        kieSession.setGlobal("nextQuestion", null);
        kieSession.insert(session);
        kieSession.insert(answer);
        kieSession.fireAllRules();

        // Retrieve the next question
        var nextQuestion = knowledgeBase.getNextQuestion(kieSession);
        kieSession.dispose();

        if (nextQuestion == null) {
            return "Survey completed. Thank you for your participation!";
        }

        return nextQuestion.getText();
    }
}
