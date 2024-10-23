package pt.isep.meia.AICare.infrastructure.gateways;

import lombok.var;
import org.kie.api.runtime.KieSession;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import pt.isep.meia.AICare.application.configs.DroolsConfig;
import pt.isep.meia.AICare.domain.constants.ActivityConstants;
import pt.isep.meia.AICare.domain.entities.Conclusion;
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
    public DroolsGateway(
            DroolsConfig droolsConfig) {
        this.droolsConfig = droolsConfig;
    }

    public Result getNextQuestion(UUID surveyId, List<Evidence> evidences) throws IOException {
        var session = droolsConfig.getKieSession();
        if (session == null) {
            return null;
        }

        // Initialize session globals
        session.setGlobal("surveyId", surveyId);
        session.setGlobal("evidences", evidences);
        session.fireAllRules();

        // Step 1: Check if the survey has been marked as completed
        boolean surveyCompleted = checkSurveyCompletion(session);

        if (!surveyCompleted) {
            // Step 2: If survey not completed, check for the next question
            var nextQuestion = getLastQuestionFromSession(session);
            if (nextQuestion != null) {
                return Result.fromQuestion(nextQuestion);
            }
        }

        // Step 3: If no more questions or survey completed, process filtered and ordered activities
        List<Restrict> restrictions = getRestrictionsFromSession(session);
        List<PreferredActivity> conclusions = getPreferredActivitiesFromSession(session);

        // Filter out restricted activities
        List<String> restrictedActivityNames = restrictions.stream()
                .map(Restrict::getActivity)
                .collect(Collectors.toList());

        List<String> permittedActivities = ActivityConstants.getAllActivities().stream()
                .filter(activity -> !restrictedActivityNames.contains(activity))
                .collect(Collectors.toList());

        // Order permitted activities, prioritizing those marked as conclusions
        List<String> prioritizedActivities = conclusions.stream()
                .map(PreferredActivity::getDescription)
                .distinct()
                .collect(Collectors.toList());

        // Place prioritized activities at the top
        List<String> orderedActivities = new ArrayList<>(prioritizedActivities);
        permittedActivities.stream()
                .filter(activity -> !prioritizedActivities.contains(activity))
                .forEach(orderedActivities::add);

        return Result.fromActivities(surveyId, orderedActivities);

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
}
