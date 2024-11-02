package pt.isep.meia.AICare.controllers;

import lombok.var;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import pt.isep.meia.AICare.application.services.ConclusionService;
import pt.isep.meia.AICare.application.services.PatientService;
import pt.isep.meia.AICare.domain.dtos.SurveyListItemDto;
import pt.isep.meia.AICare.domain.entities.Patient;

import java.net.URI;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

@RestController
@RequestMapping("/api/patients")
public class PatientsController {

    private final PatientService patientService;
    private final ConclusionService conclusionService;

    @Autowired
    public PatientsController(
            PatientService patientService,
            ConclusionService conclusionService) {
        this.patientService = patientService;
        this.conclusionService = conclusionService;
    }

    @GetMapping
    public List<Patient> getAllPatients() {
        return patientService.getAllPatients();
    }

    @PostMapping
    public ResponseEntity<Patient> createPatient(@RequestBody Patient patient) {
        var createdPatient = patientService.createPatient(patient);
        return ResponseEntity
                .created(URI.create("/api/patients/" + createdPatient.getId()))
                .body(createdPatient);
    }

    @GetMapping("/{id}")
    public ResponseEntity<Patient> getPatientById(@PathVariable UUID id) {
        var patient = patientService.getPatientById(id);
        return (patient != null) ? ResponseEntity.ok(patient) : ResponseEntity.notFound().build();
    }

    @DeleteMapping("/{id}")
    public ResponseEntity<Void> deletePatient(@PathVariable UUID id) {
        patientService.deletePatient(id);
        return ResponseEntity.noContent().build();
    }

    @GetMapping("/{id}/surveys")
    public ResponseEntity<List<SurveyListItemDto>> getSurveysByPatientId(@PathVariable UUID id) {
        var surveys = patientService.getSurveysByPatientId(id);
        var conclusions = conclusionService.getConclusionsBySurveys(surveys);
        var result = surveys.stream()
                .map(survey -> survey.toListItemDto(
                        conclusions.stream()
                                .filter(conclusion -> conclusion.getSurveyId().equals(survey.getId()))
                                .findFirst()
                                .orElse(null)))
                .collect(Collectors.toList());
        return ResponseEntity.ok(result);
    }
}
