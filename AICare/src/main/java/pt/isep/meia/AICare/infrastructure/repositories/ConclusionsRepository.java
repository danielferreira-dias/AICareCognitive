package pt.isep.meia.AICare.infrastructure.repositories;

import org.springframework.data.jpa.repository.JpaRepository;
import pt.isep.meia.AICare.domain.entities.Conclusion;

import java.util.List;
import java.util.UUID;

public interface ConclusionsRepository extends JpaRepository<Conclusion, UUID> {
    Conclusion findConclusionBySurveyId(UUID surveyId);
    List<Conclusion> findConclusionsBySurveyIdIn(List<UUID> surveyIds);
}
