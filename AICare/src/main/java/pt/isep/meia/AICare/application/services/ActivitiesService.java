package pt.isep.meia.AICare.application.services;

import lombok.var;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import pt.isep.meia.AICare.domain.entities.Activity;
import pt.isep.meia.AICare.domain.entities.Conclusion;
import pt.isep.meia.AICare.infrastructure.repositories.ActivitiesRepository;

import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

@Service
public class ActivitiesService {
    private final ActivitiesRepository activitiesRepository;

    @Autowired
    public ActivitiesService(ActivitiesRepository activitiesRepository){
        this.activitiesRepository = activitiesRepository;
    }

    public void saveAllForConclusion(Conclusion createdConclusion, List<Activity> activities) {
        var activitiesToSave = activities.stream()
                .peek(activity -> activity.setConclusion(createdConclusion))
                .collect(Collectors.toList());
        activitiesRepository.saveAll(activitiesToSave);
    }

    public List<String> findActivitiesDescriptionsBySurveyId(UUID surveyId) {
        return activitiesRepository.findActivitiesDescriptionsBySurveyId(surveyId);
    }
}
