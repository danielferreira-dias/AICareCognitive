package pt.isep.meia.AICare.infrastructure.repositories;

import org.springframework.data.jpa.repository.JpaRepository;
import pt.isep.meia.AICare.domain.entities.ConclusionPojo;

import java.util.List;
import java.util.UUID;

public interface ConclusionsRepository extends JpaRepository<ConclusionPojo, UUID> {
    ConclusionPojo findConclusionBySurveyId(UUID surveyId);

    List<ConclusionPojo> findConclusionsBySurveyIdIn(List<UUID> surveyIds);
}
