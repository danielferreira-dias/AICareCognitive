package pt.isep.meia.AICare.application.services;

import lombok.var;
import org.kie.api.runtime.KieContainer;
import org.kie.api.runtime.KieSession;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class DroolsService {
    private final KieContainer kieContainer;

    @Autowired
    public DroolsService(KieContainer kieContainer) {
        this.kieContainer = kieContainer;
    }

    public KieSession createNewSession() {
        try {
            var kieSession = kieContainer.newKieSession("ksession-rules");
            if (kieSession == null) {
                System.err.println("Failed to create KieSession: ksession-rules not found");
            }
            return kieSession;
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }

    public void insertFact(KieSession kieSession, Object fact) {
        kieSession.insert(fact);
    }

    public void fireRules(KieSession kieSession) {
        kieSession.fireAllRules();
    }

    public void disposeSession(KieSession kieSession) {
        if (kieSession != null) {
            kieSession.dispose();
        }
    }

    public Object getGlobal(KieSession kieSession, String globalName) {
        return kieSession.getGlobal(globalName);
    }

    public void setGlobal(KieSession kieSession, String globalName, Object globalObject) {
        kieSession.setGlobal(globalName, globalObject);
    }
}
