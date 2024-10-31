package pt.isep.meia.AICare.infrastructure.gateways;

import lombok.var;
import org.kie.api.KieBase;
import org.kie.api.definition.KiePackage;
import org.kie.api.definition.rule.Rule;
import org.kie.api.event.rule.AfterMatchFiredEvent;
import org.kie.api.event.rule.DefaultAgendaEventListener;
import org.kie.api.runtime.KieSession;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import pt.isep.meia.AICare.application.configs.DroolsConfig;
import pt.isep.meia.AICare.domain.constants.ActivityConstants;
import pt.isep.meia.AICare.domain.entities.Question;
import pt.isep.meia.AICare.domain.model.*;

import java.io.IOException;
import java.util.*;
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

            session.getAgenda().getAgendaGroup("survey-rules").setFocus();
            session.fireAllRules();

            session.getAgenda().getAgendaGroup("question-group").setFocus();
            session.fireAllRules();

            var surveyCompleted = checkSurveyCompletion(session);

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
                    .distinct()
                    .collect(Collectors.toList());

            var prioritizedActivities = conclusions.stream()
                    .map(PreferredActivity::getDescription)
                    .distinct()
                    .collect(Collectors.toList());

            permittedActivities.sort(Comparator.comparingInt(activity -> {
                int index = prioritizedActivities.indexOf(activity);
                return index >= 0 ? index : Integer.MAX_VALUE;
            }));

            return Result.fromActivities(surveyId, permittedActivities);
        }
        finally {
            clearSession(session);
        }
    }

    public List<Justification> getWhyNot(UUID surveyId, List<Evidence> evidences, String activityToCheck) throws IOException {
        var session = droolsConfig.getKieSession();
        if (session == null) {
            return null;
        }

        try {
            session.setGlobal("surveyId", surveyId);
            session.setGlobal("evidences", evidences);
            session.getAgenda().getAgendaGroup("survey-rules").setFocus();
            session.fireAllRules();

            // Step 1: Get all restrictions from the session
            var allRestrictions = getRestrictionsFromSession(session);

            // Step 2: Filter restrictions by the specified activityToCheck
            var filteredRestrictions = allRestrictions.stream()
                    .filter(restriction -> restriction.getActivity().equals(activityToCheck))
                    .collect(Collectors.toList());

            // Step 3: Group filtered restrictions by activity and collect unique rules for each activity
            Map<String, Set<String>> groupedRestrictions = filteredRestrictions.stream()
                    .collect(Collectors.groupingBy(
                            Restrict::getReason,
                            Collectors.mapping(Restrict::getRestrictingRule, Collectors.toSet())
                    ));

            // Step 4: Convert each entry in the map to a Justification object
            var justifications = groupedRestrictions.entrySet().stream()
                    .map(entry -> {
                        String activity = entry.getKey();
                        List<String> uniqueRules = entry.getValue().stream().collect(Collectors.toList());
                        return new Justification(JustificationTypeEnum.why, activity, uniqueRules);
                    })
                    .collect(Collectors.toList());

            return justifications;
        } finally {
            clearSession(session);
        }
    }

    public List<Justification> getWhy(UUID surveyId, List<Evidence> evidences, String activityToCheck) throws IOException {
        var session = droolsConfig.getKieSession();
        if (session == null) {
            return null;
        }

        List<String> firedRules = new ArrayList<>();
        List<Justification> unmetConditions = new ArrayList<>();

        session.addEventListener(new DefaultAgendaEventListener() {
            @Override
            public void afterMatchFired(AfterMatchFiredEvent event) {
                firedRules.add(event.getMatch().getRule().getName());
            }
        });

        try {
            session.setGlobal("surveyId", surveyId);
            session.setGlobal("evidences", evidences);
            session.getAgenda().getAgendaGroup("survey-rules").setFocus();
            session.fireAllRules();

            // Step 1: Retrieve all rules from the KieBase to check which didn't fire
            KieBase kbase = session.getKieBase();
            Collection<KiePackage> packages = kbase.getKiePackages();

            // Step 2: Iterate over each rule to check if it applies to the given activity and restriction type
            Map<String, Map<String, List<String>>> groupedUnmetConditions = new HashMap<>();

            for (KiePackage kp : packages) {
                for (Rule rule : kp.getRules()) {
                    if (!firedRules.contains(rule.getName())) {
                        // Get restrictionType and activities metadata
                        String restrictionType = (String) rule.getMetaData().get("restrictionType");
                        String activitiesMeta = (String) rule.getMetaData().get("activities");

                        if (activitiesMeta != null && Arrays.asList(activitiesMeta.split(",")).contains(activityToCheck)) {
                            // Add this rule to the unmet conditions grouped by restrictionType
                            groupedUnmetConditions
                                    .computeIfAbsent(restrictionType, k -> new HashMap<>())
                                    .computeIfAbsent(activityToCheck, k -> new ArrayList<>())
                                    .add(rule.getName());
                        }
                    }
                }
            }

            // Step 3: Convert grouped unmet conditions into Justification objects
            for (Map.Entry<String, Map<String, List<String>>> restrictionTypeEntry : groupedUnmetConditions.entrySet()) {
                String restrictionType = restrictionTypeEntry.getKey();

                for (Map.Entry<String, List<String>> activityEntry : restrictionTypeEntry.getValue().entrySet()) {
                    List<String> rules = activityEntry.getValue();

                    unmetConditions.add(new Justification(
                            JustificationTypeEnum.whynot,
                            restrictionType,
                            rules
                    ));
                }
            }

            return unmetConditions.isEmpty() ? Collections.singletonList(new Justification(
                    JustificationTypeEnum.whynot, "generic_activity",
                    new ArrayList<>()
            )) : unmetConditions;

        } finally {
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
