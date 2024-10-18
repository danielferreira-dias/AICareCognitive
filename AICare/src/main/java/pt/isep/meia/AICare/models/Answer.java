package pt.isep.meia.AICare.models;

import lombok.Getter;
import lombok.Setter;

@Setter
@Getter
public class Answer {
    private String questionId;
    private String response;

    public Answer(String questionId, String response) {
        this.questionId = questionId;
        this.response = response;
    }
}
