package pt.isep.meia.AICare.infrastructure.repositories;

import org.springframework.data.jpa.repository.JpaRepository;
import pt.isep.meia.AICare.domain.entities.Survey;

import java.util.UUID;

public interface SurveysRepository extends JpaRepository<Survey, UUID> {
}
