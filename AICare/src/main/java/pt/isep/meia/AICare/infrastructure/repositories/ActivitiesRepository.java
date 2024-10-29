package pt.isep.meia.AICare.infrastructure.repositories;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import pt.isep.meia.AICare.domain.entities.Activity;

import java.util.List;
import java.util.UUID;

public interface ActivitiesRepository extends JpaRepository<Activity, UUID> {
    @Query("SELECT a.description " +
            "FROM Activity a JOIN Conclusion c ON a.conclusionId = c.id " +
            "WHERE c.surveyId = :surveyId ")
    List<String> findActivitiesDescriptionsBySurveyId(@Param("surveyId") UUID surveyId);

    @Query("SELECT a " +
            "FROM Activity a JOIN Conclusion c ON a.conclusionId = c.id " +
            "WHERE c.surveyId = :surveyId AND a.id = :activityId")
    Activity findActivitiesByIdAndSurveyId(@Param("activityId") UUID activityId, @Param("surveyId") UUID surveyId);
}
