package pt.isep.meia.AICare.infrastructure.repositories;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import pt.isep.meia.AICare.domain.entities.Answer;
import pt.isep.meia.AICare.domain.model.Evidence;

import java.util.List;
import java.util.UUID;

public interface AnswersRepository extends JpaRepository<Answer, UUID> {
    @Query("SELECT new pt.isep.meia.AICare.domain.model.Evidence(q, a) " +
            "FROM Answer a JOIN Question q ON a.questionId = q.id " +
            "WHERE q.surveyId = :surveyId")
    List<Evidence> findEvidencesBySurveyId(@Param("surveyId") UUID surveyId);
}
