package pt.isep.meia.AICare.infrastructure.repositories;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import pt.isep.meia.AICare.domain.entities.Patient;
import pt.isep.meia.AICare.domain.entities.Survey;

import java.util.List;
import java.util.UUID;

public interface PatientsRepository extends JpaRepository<Patient, UUID> {
    @Query("SELECT s FROM Survey s WHERE s.patientId = :patientId ORDER BY s.createDate DESC")
    List<Survey> findSurveysOfPatient(@Param("patientId") UUID patientId);

    List<Patient> findAllByOrderByCreateDateDesc();
}
