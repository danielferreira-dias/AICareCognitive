package pt.isep.meia.AICare.application.services;

import lombok.var;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import pt.isep.meia.AICare.application.configs.EngineProperties;
import pt.isep.meia.AICare.domain.model.Evidence;
import pt.isep.meia.AICare.domain.model.Justification;
import pt.isep.meia.AICare.domain.model.Result;
import pt.isep.meia.AICare.infrastructure.gateways.DroolsGateway;
import pt.isep.meia.AICare.infrastructure.gateways.PrologGateway;

import java.io.IOException;
import java.util.List;
import java.util.UUID;

@Service
public class EngineService {

    private final EngineProperties engineProperties;
    private final DroolsGateway droolsGateway;
    private final PrologGateway prologGateway;

    @Autowired
    public EngineService(
            EngineProperties engineProperties,
            DroolsGateway droolsGateway,
            PrologGateway prologGateway) {
        this.engineProperties = engineProperties;
        this.droolsGateway = droolsGateway;
        this.prologGateway = prologGateway;
    }

    public Result getNextQuestion(UUID surveyId, List<Evidence> evidences, int order) throws IOException {
        if(engineProperties.getType().equals("drools")){
            return droolsGateway.getNextQuestion(surveyId, evidences, order);
        }
        else{
            if(!evidences.isEmpty())
            {
                var result = prologGateway.postBulkAnswers(evidences);
                if (!result) {
                    return null;
                }
            }
            return prologGateway.getNextQuestion(surveyId, order);
        }
    }

    public boolean postAnswer(String question, String answer) {
        if(engineProperties.getType().equals("prolog")){
            return prologGateway.postAnswer(question, answer);
        }
        return true;
    }

    public List<Justification> getWhy(UUID surveyId, String activity, List<Evidence> evidences) throws IOException {
        if(engineProperties.getType().equals("drools")){
            return droolsGateway.getWhy(surveyId, evidences, activity);
        }

        var result = prologGateway.postBulkAnswers(evidences);
        if (!result) {
            return null;
        }

        return prologGateway.getWhy(activity);
    }

    public List<Justification> getWhyNot(UUID surveyId, String activity, List<Evidence> evidences) {
        var result = prologGateway.postBulkAnswers(evidences);
        if (!result) {
            return null;
        }

        return prologGateway.getWhyNot(activity);
    }
}
