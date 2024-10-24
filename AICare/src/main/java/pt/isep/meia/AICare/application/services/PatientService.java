package pt.isep.meia.AICare.application.services;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import pt.isep.meia.AICare.domain.entities.Survey;
import pt.isep.meia.AICare.infrastructure.repositories.PatientsRepository;
import pt.isep.meia.AICare.domain.entities.Patient;

import java.util.List;
import java.util.UUID;

@Service
public class PatientService {
    private final PatientsRepository patientsRepository;

    @Autowired
    public PatientService(PatientsRepository patientsRepository) {
        this.patientsRepository = patientsRepository;
    }

    public List<Patient> getAllPatients() {
        return patientsRepository.findAllByOrderByCreateDateDesc();
    }

    public Patient createPatient(Patient patient) {
        return patientsRepository.save(patient);
    }

    public Patient getPatientById(UUID id) {
        return patientsRepository.findById(id).orElse(null);
    }

    public void deletePatient(UUID id) {
        patientsRepository.deleteById(id);
    }

    public List<Survey> getSurveysByPatientId(UUID id) {
        return patientsRepository.findSurveysOfPatient(id);
    }
}
