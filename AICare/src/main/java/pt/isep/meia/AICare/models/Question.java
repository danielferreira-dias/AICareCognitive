package pt.isep.meia.AICare.models;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class Question {
    private String text;
    private String id;

    public Question(String id, String text) {
        this.id = id;
        this.text = text;
    }
}
