package pt.isep.meia.AICare.domain.model;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import lombok.var;
import pt.isep.meia.AICare.domain.entities.Activity;
import pt.isep.meia.AICare.domain.entities.ConclusionPojo;

import java.util.Comparator;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

@Getter
@Setter
@AllArgsConstructor
public class Conclusion {
    private UUID id;
    private UUID surveyId;
    private List<Activity> activities;

    public Conclusion(UUID surveyId, List<Activity> activities) {
        this.surveyId = surveyId;
        this.activities = activities;
    }

    public static Conclusion fromPojo(ConclusionPojo conclusionPojo, List<Activity> activities) {
        activities.sort(Comparator.comparingInt(Activity::getSortingOrder));
        return new Conclusion(conclusionPojo.getId(), conclusionPojo.getSurveyId(), activities);
    }

    public static List<Conclusion> fromPojos(List<ConclusionPojo> conclusionPojos) {
        return conclusionPojos.stream()
                .map(conclusionPojo -> new Conclusion(conclusionPojo.getId(), conclusionPojo.getSurveyId(), null))
                .collect(Collectors.toList());
    }

    public static Conclusion fromPojo(ConclusionPojo conclusionPojo) {
        return new Conclusion(conclusionPojo.getId(), conclusionPojo.getSurveyId(), null);
    }

    public ConclusionPojo toPojo() {
        var conclusionPojo = new ConclusionPojo();
        conclusionPojo.setSurveyId(surveyId);
        return conclusionPojo;
    }
}
