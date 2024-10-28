package pt.isep.meia.AICare.controllers;

import lombok.var;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import pt.isep.meia.AICare.application.services.SurveyService;
import pt.isep.meia.AICare.domain.dtos.ActivityJustificationRequestDto;
import pt.isep.meia.AICare.domain.dtos.CreateSurveyRequestDto;
import pt.isep.meia.AICare.domain.dtos.RejectedActivityDto;
import pt.isep.meia.AICare.domain.entities.Answer;
import pt.isep.meia.AICare.domain.entities.Survey;
import pt.isep.meia.AICare.domain.model.Justification;
import pt.isep.meia.AICare.domain.model.JustificationTypeEnum;
import pt.isep.meia.AICare.domain.model.Result;
import pt.isep.meia.AICare.domain.model.Evidence;

import java.io.IOException;
import java.net.URI;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

@RestController
@RequestMapping("/api/surveys")
public class SurveysController {

    private final SurveyService surveyService;

    @Autowired
    public SurveysController(SurveyService surveyService) {
        this.surveyService = surveyService;
    }

    @GetMapping("{surveyId}")
    public ResponseEntity<Survey> getSurveyById(@PathVariable UUID surveyId) {
        var survey = surveyService.getSurveyById(surveyId);
        if(survey == null) {
            return ResponseEntity.notFound().build();
        }

        return ResponseEntity.ok(survey);
    }

    @PostMapping
    public ResponseEntity<Survey> createSurvey(@RequestBody CreateSurveyRequestDto requestDto) {
        var createdSurvey = surveyService.createSurvey(requestDto.getPatientId());
        return ResponseEntity
                .created(URI.create("/api/surveys/" + createdSurvey.getId()))
                .body(createdSurvey);
    }

    @GetMapping("{surveyId}/next-question")
    public ResponseEntity<Result> getSurveyNextQuestion(@PathVariable UUID surveyId) throws IOException {
        var nextQuestion = surveyService.getNextQuestion(surveyId);
        if(nextQuestion == null) {
            return ResponseEntity.notFound().build();
        }
        return ResponseEntity.ok(nextQuestion);
    }

    @PostMapping("{surveyId}/answer")
    public ResponseEntity<Answer> postSurveyQuestionAnswer(@PathVariable String surveyId, @RequestBody Answer answer) {
        var createdAnswer = surveyService.answerQuestion(answer);
        return ResponseEntity
                .created(URI.create("/api/surveys/" + surveyId + "/answered-questions"))
                .body(createdAnswer);
    }

    @GetMapping("{surveyId}/answered-questions")
    public ResponseEntity<List<Evidence>> getAllAnsweredQuestions(@PathVariable UUID surveyId) {
        var answeredQuestions = surveyService.getAllAnsweredQuestionsById(surveyId);
        return ResponseEntity.ok(answeredQuestions);
    }

    @GetMapping("{surveyId}/rejected-activities")
    public ResponseEntity<List<RejectedActivityDto>> getRejectedActivities(@PathVariable UUID surveyId) throws IOException {
        var rejectedActivities = surveyService.getRejectedActivities(surveyId);
        return ResponseEntity.ok(
                rejectedActivities.stream()
                        .map(RejectedActivityDto::new)
                        .collect(Collectors.toList())
        );
    }

    @PostMapping("{surveyId}/justifications/{type}")
    public ResponseEntity<List<Justification>> getJustifications(@PathVariable UUID surveyId, @PathVariable JustificationTypeEnum type, @RequestBody ActivityJustificationRequestDto requestDto) throws IOException {
        var justifications = surveyService.getActivityJustifications(surveyId, requestDto.activityName, type);
        if(justifications == null) {
            return ResponseEntity.notFound().build();
        }

        return ResponseEntity.ok(justifications);
    }

    @DeleteMapping("{surveyId}")
    public ResponseEntity<Void> deleteSurvey(@PathVariable UUID surveyId) {
        surveyService.deleteSurvey(surveyId);
        return ResponseEntity.noContent().build();
    }
}
