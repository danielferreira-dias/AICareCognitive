package pt.isep.meia.AICare.infrastructure.gateways;

import lombok.var;
import org.kie.api.command.Command;
import org.kie.internal.command.CommandFactory;
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
    public DroolsGateway(
            DroolsConfig droolsConfig) {
        this.droolsConfig = droolsConfig;
    }

    public Result getNextQuestion(UUID surveyId, List<Evidence> evidences, int order) throws IOException {
        var statelessSession = droolsConfig.getStatelessKieSession();
        if (statelessSession == null) {
            return null;
        }

        List<Object> objects = new ArrayList<>();

        // Create commands to insert objects and set globals
        List<Command<?>> commands = new ArrayList<>();
        commands.add(CommandFactory.newSetGlobal("surveyId", surveyId));
        commands.add(CommandFactory.newSetGlobal("evidences", evidences));
        commands.add(CommandFactory.newInsertElements(objects));

        // Fire rules in a stateless manner
        statelessSession.execute(CommandFactory.newBatchExecution(commands));

        // Retrieve results from session (adjust based on how you pass and return objects)
        boolean surveyCompleted = checkSurveyCompletion(objects);

        if (!surveyCompleted) {
            var nextQuestion = getLastQuestionFromEvidences(objects);
            if (nextQuestion != null) {
                nextQuestion.setQuestionOrder(order);
                return Result.fromQuestion(nextQuestion);
            }
        }

        var restrictions = getRestrictionsFromEvidences(objects);
        var conclusions = getPreferredActivitiesFromEvidences(objects);

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

    private boolean checkSurveyCompletion(List<Object> objects) {
        return objects.stream()
                .anyMatch(obj -> obj instanceof SurveyCompleted);
    }

    private Question getLastQuestionFromEvidences(List<Object> objects) {
        return objects.stream()
                .filter(obj -> obj instanceof Question)
                .map(obj -> (Question) obj)
                .reduce((first, second) -> second)
                .orElse(null);
    }

    private List<PreferredActivity> getPreferredActivitiesFromEvidences(List<Object> objects) {
        return objects.stream()
                .filter(obj -> obj instanceof PreferredActivity)
                .map(obj -> (PreferredActivity) obj)
                .collect(Collectors.toList());
    }

    private List<Restrict> getRestrictionsFromEvidences(List<Object> objects) {
        return objects.stream()
                .filter(obj -> obj instanceof Restrict)
                .map(obj -> (Restrict) obj)
                .collect(Collectors.toList());
    }
}
