package pt.isep.meia.AICare.domain.model;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import lombok.var;
import pt.isep.meia.AICare.domain.entities.Activity;
import pt.isep.meia.AICare.domain.entities.Conclusion;
import pt.isep.meia.AICare.domain.entities.Question;
import pt.isep.meia.AICare.infrastructure.gateways.dtos.PrologResultDto;
import pt.isep.meia.AICare.infrastructure.gateways.dtos.PrologTypeEnum;

import java.util.Comparator;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

@Getter
@Setter
@AllArgsConstructor
public class Result {
    private ResultTypeEnum type;
    private Question question;
    private Conclusion conclusion;

    public static Result fromQuestion(Question question) {
        return new Result(ResultTypeEnum.question, question, null);
    }

    public static Result fromActivities(UUID surveyId, List<String> activities) {
        var activityEntities = activities.stream()
                .map(description -> {
                    Activity activity = new Activity();
                    activity.setDescription(description);
                    activity.setSortingOrder(activities.indexOf(description));
                    return activity;
                })
                .sorted(Comparator.comparing(Activity::getSortingOrder))
                .collect(Collectors.toList());

        var conclusion = new Conclusion(surveyId, activityEntities);

        return new Result(ResultTypeEnum.conclusion, null, conclusion);
    }

    public static Result fromConclusion(Conclusion conclusion) {
        return new Result(ResultTypeEnum.conclusion, null, conclusion);
    }

    public static Result fromPrologResult(UUID surveyId, PrologResultDto prologResultDto, int order) {
        if(prologResultDto.getType().equals(PrologTypeEnum.question)){
            var question = new Question(surveyId, prologResultDto.getQuestion(), order);
            question.setPossibleAnswers(prologResultDto.getPossibleAnswers());
            return new Result(ResultTypeEnum.question, question, null);
        } else {
            return Result.fromActivities(surveyId, prologResultDto.getConclusion());
        }
    }
}
