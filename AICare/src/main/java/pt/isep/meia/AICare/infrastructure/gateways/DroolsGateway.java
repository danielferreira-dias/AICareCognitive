package pt.isep.meia.AICare.infrastructure.gateways;

import lombok.var;
import org.kie.api.runtime.KieSession;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import pt.isep.meia.AICare.application.configs.DroolsConfig;
import pt.isep.meia.AICare.domain.entities.Conclusion;
import pt.isep.meia.AICare.domain.entities.Question;
import pt.isep.meia.AICare.domain.model.Evidence;
import pt.isep.meia.AICare.domain.model.Result;

import java.io.IOException;
import java.util.List;
import java.util.UUID;

@Service
public class DroolsGateway {

    private final DroolsConfig droolsConfig;

    @Autowired
    public DroolsGateway(
            DroolsConfig droolsConfig) {
        this.droolsConfig = droolsConfig;
    }

    public Result getNextQuestion(UUID surveyId, List<Evidence> evidences) throws IOException {
        var session = droolsConfig.getKieSession();
        if (session == null) {
            return null;
        }

        session.setGlobal("surveyId", surveyId);
        session.setGlobal("evidences", evidences);
        session.fireAllRules();

        var conclusion = getConclusionFromSession(session);

        if(conclusion != null){
            return Result.fromConclusion(conclusion);
        }

        var nextQuestion = getLastQuestionFromSession(session);

        return Result.fromQuestion(nextQuestion);
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
