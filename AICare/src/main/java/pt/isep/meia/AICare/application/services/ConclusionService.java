package pt.isep.meia.AICare.application.services;

import lombok.var;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import pt.isep.meia.AICare.domain.entities.Conclusion;
import pt.isep.meia.AICare.domain.entities.Survey;
import pt.isep.meia.AICare.infrastructure.repositories.ConclusionsRepository;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

@Service
public class ConclusionService {
    private final ConclusionsRepository conclusionsRepository;

    @Autowired
    public ConclusionService(ConclusionsRepository conclusionsRepository){
        this.conclusionsRepository = conclusionsRepository;
    }

    public Conclusion getConclusionBySurveyId(UUID surveyId) {
        return conclusionsRepository.findConclusionBySurveyId(surveyId);
    }

    public List<Conclusion> getConclusionsBySurveys(List<Survey> surveys) {
        var surveyIds = surveys.stream().map(Survey::getId).collect(Collectors.toList());
        return conclusionsRepository.findConclusionsBySurveyIdIn(surveyIds);
    }

    public Conclusion saveDroppingActivities(Conclusion conclusion) {
        conclusion.setActivities(new ArrayList<>());
        return conclusionsRepository.save(conclusion);
    }
}
