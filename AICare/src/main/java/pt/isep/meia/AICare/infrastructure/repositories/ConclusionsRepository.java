package pt.isep.meia.AICare.infrastructure.repositories;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import pt.isep.meia.AICare.domain.entities.Conclusion;

import java.util.List;
import java.util.UUID;

public interface ConclusionsRepository extends JpaRepository<Conclusion, UUID> {
//    @Query("SELECT c FROM Conclusion c LEFT JOIN FETCH c.activities WHERE c.surveyId = :surveyId")
    Conclusion findConclusionBySurveyId(UUID surveyId);
    List<Conclusion> findConclusionsBySurveyIdIn(List<UUID> surveyIds);
}
