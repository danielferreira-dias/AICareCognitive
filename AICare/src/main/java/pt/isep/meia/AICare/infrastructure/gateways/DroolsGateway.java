package pt.isep.meia.AICare.infrastructure.gateways;

import lombok.var;
import org.kie.api.runtime.KieSession;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import pt.isep.meia.AICare.application.configs.DroolsConfig;
import pt.isep.meia.AICare.domain.constants.ActivityConstants;
import pt.isep.meia.AICare.domain.entities.Question;
import pt.isep.meia.AICare.domain.model.*;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

@Service
public class DroolsGateway {

    private final DroolsConfig droolsConfig;

    @Autowired
    public DroolsGateway(DroolsConfig droolsConfig) {
        this.droolsConfig = droolsConfig;
    }

    public Result getNextQuestion(UUID surveyId, List<Evidence> evidences, int order) throws IOException {
        var session = droolsConfig.getKieSession();
        if (session == null) {
            return null;
        }

        try {
            session.setGlobal("surveyId", surveyId);
            session.setGlobal("evidences", evidences);

            System.out.println("Globals set:");
            for (var evidence : (List<Evidence>) session.getGlobal("evidences")){
                System.out.println("evidence: " + evidence.toString());
            }
            System.out.println("surveyId: " + session.getGlobal("surveyId"));

            session.getAgenda().getAgendaGroup("survey-rules").setFocus();
            session.fireAllRules();

            session.getAgenda().getAgendaGroup("question-group").setFocus();
            session.fireAllRules();

            boolean surveyCompleted = checkSurveyCompletion(session);

            if (!surveyCompleted) {
                var nextQuestion = getLastQuestionFromSession(session);
                if (nextQuestion != null) {
                    nextQuestion.setQuestionOrder(order);
                    return Result.fromQuestion(nextQuestion);
                }
            }

            var restrictions = getRestrictionsFromSession(session);
            var conclusions = getPreferredActivitiesFromSession(session);

            var restrictedActivityNames = restrictions.stream()
                    .map(Restrict::getActivity)
                    .collect(Collectors.toList());

            var permittedActivities = ActivityConstants.getAllActivities().stream()
                    .filter(activity -> !restrictedActivityNames.contains(activity))
                    .collect(Collectors.toList());

            var prioritizedActivities = conclusions.stream()
                    .map(PreferredActivity::getDescription)
                    .distinct()
                    .collect(Collectors.toList());

            var orderedActivities = new ArrayList<>(prioritizedActivities);
            permittedActivities.stream()
                    .filter(activity -> !prioritizedActivities.contains(activity))
                    .forEach(orderedActivities::add);

            return Result.fromActivities(surveyId, orderedActivities);
        }
        finally {
            clearSession(session);
        }
    }

    private boolean checkSurveyCompletion(KieSession session) {
        return session.getObjects(new org.kie.api.runtime.ClassObjectFilter(SurveyCompleted.class))
                .stream()
                .anyMatch(obj -> obj instanceof SurveyCompleted);
    }

    private Question getLastQuestionFromSession(KieSession session) {
        return session.getObjects(new org.kie.api.runtime.ClassObjectFilter(Question.class))
                .stream()
                .filter(obj -> obj instanceof Question)
                .map(obj -> (Question) obj)
                .reduce((first, second) -> second)
                .orElse(null);
    }

    private List<PreferredActivity> getPreferredActivitiesFromSession(KieSession session) {
        return session.getObjects(new org.kie.api.runtime.ClassObjectFilter(PreferredActivity.class))
                .stream()
                .filter(obj -> obj instanceof PreferredActivity)
                .map(obj -> (PreferredActivity) obj)
                .collect(Collectors.toList());
    }

    private List<Restrict> getRestrictionsFromSession(KieSession session) {
        return session.getObjects(new org.kie.api.runtime.ClassObjectFilter(Restrict.class))
                .stream()
                .filter(obj -> obj instanceof Restrict)
                .map(obj -> (Restrict) obj)
                .collect(Collectors.toList());
    }

    private void clearSession(KieSession session) {
        session.dispose();
    }
}
