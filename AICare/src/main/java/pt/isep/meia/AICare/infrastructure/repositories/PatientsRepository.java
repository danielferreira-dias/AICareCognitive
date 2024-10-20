package pt.isep.meia.AICare.infrastructure.repositories;

import org.springframework.data.jpa.repository.JpaRepository;
import pt.isep.meia.AICare.domain.entities.Patient;

import java.util.UUID;

public interface PatientsRepository extends JpaRepository<Patient, UUID> {
    // You can define custom query methods if needed
}
