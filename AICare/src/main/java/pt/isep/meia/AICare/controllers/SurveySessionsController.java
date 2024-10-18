package pt.isep.meia.AICare.controllers;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import pt.isep.meia.AICare.application.services.SurveySessionService;
import pt.isep.meia.AICare.infrastructure.PatientsRepository;
import pt.isep.meia.AICare.models.SurveySession;

import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/surveys")
public class SurveySessionsController {

    private final SurveySessionService surveySessionService;
    private final PatientsRepository patientsRepository;

    @Autowired
    public SurveySessionsController(SurveySessionService surveySessionService, PatientsRepository patientsRepository) {
        this.surveySessionService = surveySessionService;
        this.patientsRepository = patientsRepository;
    }

    @GetMapping
    public List<SurveySession> getAllSurveySessions() {
        return surveySessionService.getAllSurveySessions();
    }

    @PostMapping("/start/{patientId}")
    public ResponseEntity<SurveySession> startSurveySession(@PathVariable UUID patientId) {
        var patient = patientsRepository.findById(patientId).orElse(null);
        if (patient == null) {
            return ResponseEntity.notFound().build();
        }
        var session = surveySessionService.createSurveySession(patientId);
        return ResponseEntity.ok(session);
    }

    @GetMapping("/{id}")
    public ResponseEntity<SurveySession> getSurveySessionById(@PathVariable UUID id) {
        var session = surveySessionService.getSurveySessionById(id);
        return (session != null) ? ResponseEntity.ok(session) : ResponseEntity.notFound().build();
    }

    @PostMapping("/end/{id}")
    public ResponseEntity<Void> endSurveySession(@PathVariable UUID id) {
        surveySessionService.endSurveySession(id);
        return ResponseEntity.noContent().build();
    }

    @DeleteMapping("/{id}")
    public ResponseEntity<Void> deleteSurveySession(@PathVariable UUID id) {
        surveySessionService.deleteSurveySession(id);
        return ResponseEntity.noContent().build();
    }
}
