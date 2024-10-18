package pt.isep.meia.AICare.infrastructure;

import org.springframework.data.jpa.repository.JpaRepository;
import pt.isep.meia.AICare.models.SurveySession;

import java.util.UUID;

public interface SurveySessionsRepository extends JpaRepository<SurveySession, UUID> {
    // Custom query methods if needed
}
