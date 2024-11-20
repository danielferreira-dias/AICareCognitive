package pt.isep.meia.AICare.application.services;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import pt.isep.meia.AICare.infrastructure.repositories.ActivitiesRepository;

import java.util.List;
import java.util.UUID;

@Service
public class ActivitiesService {
    private final ActivitiesRepository activitiesRepository;

    @Autowired
    public ActivitiesService(ActivitiesRepository activitiesRepository) {
        this.activitiesRepository = activitiesRepository;
    }

    public List<String> findActivitiesDescriptionsBySurveyId(UUID surveyId) {
        return activitiesRepository.findActivitiesDescriptionsBySurveyId(surveyId);
    }
}
