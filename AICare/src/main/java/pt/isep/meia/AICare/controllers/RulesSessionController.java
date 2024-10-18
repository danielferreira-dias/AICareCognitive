package pt.isep.meia.AICare.controllers;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/rules")
public class RulesSessionController {

    @Operation(summary = "Start a new survey session", description = "Starts a new Drools session for a patient")
    @ApiResponse(responseCode = "200", description = "Session ID created successfully")
    @PostMapping("/start-session/{patientId}")
    public ResponseEntity<String> startSurveySession(@PathVariable String patientId) {
        return ResponseEntity.ok("session-id");
    }
}
