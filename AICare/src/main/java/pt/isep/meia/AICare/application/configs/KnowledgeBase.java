package pt.isep.meia.AICare.application.configs;

import org.kie.api.runtime.KieContainer;
import org.kie.api.runtime.KieSession;
import org.springframework.stereotype.Service;
import pt.isep.meia.AICare.models.Question;

@Service
public class KnowledgeBase {
    private final KieContainer kieContainer;

    public KnowledgeBase(KieContainer kieContainer) {
        this.kieContainer = kieContainer;
    }

    public KieSession createNewSession() {
        return this.kieContainer.newKieSession("ksession-rules");
    }

    public Question getNextQuestion(KieSession kieSession) {
        return (Question) kieSession.getGlobal("nextQuestion");
    }
}