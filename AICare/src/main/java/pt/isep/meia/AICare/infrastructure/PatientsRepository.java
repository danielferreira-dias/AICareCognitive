package pt.isep.meia.AICare.infrastructure;

import org.springframework.data.jpa.repository.JpaRepository;
import pt.isep.meia.AICare.models.Patient;

import java.util.UUID;

public interface PatientsRepository extends JpaRepository<Patient, UUID> {
    // You can define custom query methods if needed
}
