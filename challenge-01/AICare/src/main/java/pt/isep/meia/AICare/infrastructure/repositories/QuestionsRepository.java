package pt.isep.meia.AICare.infrastructure.repositories;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import pt.isep.meia.AICare.domain.entities.Question;

import java.util.List;
import java.util.UUID;

public interface QuestionsRepository extends JpaRepository<Question, UUID> {
    @Query("SELECT q FROM Question q WHERE q.surveyId = :surveyId AND q.id NOT IN (SELECT a.questionId FROM Answer a WHERE a.questionId = q.id)")
    List<Question> findUnansweredQuestionsBySurveyId(@Param("surveyId") UUID surveyId);
}
