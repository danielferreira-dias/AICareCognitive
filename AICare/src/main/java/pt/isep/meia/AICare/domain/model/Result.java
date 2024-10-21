package pt.isep.meia.AICare.domain.model;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import pt.isep.meia.AICare.domain.entities.Conclusion;
import pt.isep.meia.AICare.domain.entities.Question;

@Getter
@Setter
@AllArgsConstructor
public class Result {
    private ResultTypeEnum type;
    private Question question;
    private Conclusion conclusion;

    public static Result fromQuestion(Question question) {
        return new Result(ResultTypeEnum.QUESTION, question, null);
    }

    public static Result fromConclusion(Conclusion conclusion) {
        return new Result(ResultTypeEnum.CONCLUSION, null, conclusion);
    }
}
