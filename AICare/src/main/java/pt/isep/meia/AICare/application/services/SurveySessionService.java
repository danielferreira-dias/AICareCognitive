package pt.isep.meia.AICare.application.services;

import org.kie.api.runtime.KieSession;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import pt.isep.meia.AICare.models.SurveySession;
import pt.isep.meia.AICare.infrastructure.SurveySessionsRepository;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Service
public class SurveySessionService {
    private final SurveySessionsRepository surveySessionsRepository;
    private final DroolsService droolsService;

    @Autowired
    public SurveySessionService(
            SurveySessionsRepository surveySessionsRepository,
            DroolsService droolsService) {
        this.surveySessionsRepository = surveySessionsRepository;
        this.droolsService = droolsService;
    }

    public List<SurveySession> getAllSurveySessions() {
        return surveySessionsRepository.findAll();
    }

    public SurveySession createSurveySession(UUID patientId) {
        KieSession kieSession = droolsService.getKieSession();

        SurveySession session = new SurveySession(patientId, LocalDateTime.now());
        kieSession.insert(session);
        kieSession.fireAllRules();
        SurveySession savedSession = surveySessionsRepository.save(session);
        kieSession.dispose();

        return savedSession;
    }

    public SurveySession getSurveySessionById(UUID id) {
        return surveySessionsRepository.findById(id).orElse(null);
    }

    public void endSurveySession(UUID id) {
        SurveySession session = surveySessionsRepository.findById(id).orElse(null);
        if (session != null) {
            session.setEndTime(LocalDateTime.now());
            surveySessionsRepository.save(session);
        }
    }

    public void deleteSurveySession(UUID id) {
        surveySessionsRepository.deleteById(id);
    }
}
