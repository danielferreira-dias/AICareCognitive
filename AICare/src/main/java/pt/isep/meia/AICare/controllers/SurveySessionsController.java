package pt.isep.meia.AICare.controllers;

import lombok.var;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import pt.isep.meia.AICare.application.services.SurveySessionService;
import pt.isep.meia.AICare.models.Answer;

import java.util.UUID;

@RestController
@RequestMapping("/api/surveys")
public class SurveySessionsController {

    private final SurveySessionService surveySessionService;

    @Autowired
    public SurveySessionsController(SurveySessionService surveySessionService) {
        this.surveySessionService = surveySessionService;
    }

    @PostMapping("/start/{patientId}")
    public ResponseEntity<String> startSurveySession(@PathVariable UUID patientId) {
        var firstQuestion = surveySessionService.startSurveySession(patientId);
        return ResponseEntity.ok(firstQuestion);
    }

    @PostMapping("/{sessionId}/answer")
    public ResponseEntity<String> answerQuestion(@PathVariable UUID sessionId, @RequestBody Answer answer) {
        var response = surveySessionService.answerQuestion(sessionId, answer);
        return ResponseEntity.ok(response);
    }
}
