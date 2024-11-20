package pt.isep.meia.AICare.application.services;

import lombok.var;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import pt.isep.meia.AICare.domain.entities.Survey;
import pt.isep.meia.AICare.domain.model.Conclusion;
import pt.isep.meia.AICare.infrastructure.repositories.ActivitiesRepository;
import pt.isep.meia.AICare.infrastructure.repositories.ConclusionsRepository;

import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

@Service
public class ConclusionService {
    private final ConclusionsRepository conclusionsRepository;
    private final ActivitiesRepository activitiesRepository;

    @Autowired
    public ConclusionService(
            ConclusionsRepository conclusionsRepository,
            ActivitiesRepository activitiesRepository) {
        this.conclusionsRepository = conclusionsRepository;
        this.activitiesRepository = activitiesRepository;
    }

    public Conclusion getConclusionBySurveyId(UUID surveyId) {
        var conclusion = conclusionsRepository.findConclusionBySurveyId(surveyId);
        if (conclusion == null) {
            return null;
        }

        var activities = activitiesRepository.findActivitiesByConclusionId(conclusion.getId());
        return Conclusion.fromPojo(conclusion, activities);
    }

    public List<Conclusion> getConclusionsBySurveys(List<Survey> surveys) {
        var surveyIds = surveys.stream().map(Survey::getId).collect(Collectors.toList());
        return Conclusion.fromPojos(conclusionsRepository.findConclusionsBySurveyIdIn(surveyIds));
    }

    public Conclusion save(Conclusion conclusion) {
        var createdPojo = conclusionsRepository.save(conclusion.toPojo());
        var activities = activitiesRepository.saveAll(conclusion.getActivities().stream()
                .peek(activity -> activity.setConclusionId(createdPojo.getId()))
                .collect(Collectors.toList()));

        return Conclusion.fromPojo(createdPojo, activities);
    }
}
