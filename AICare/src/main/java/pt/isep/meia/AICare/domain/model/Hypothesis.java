package pt.isep.meia.AICare.domain.model;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class Hypothesis extends Fact {
    private String description;
    private String value;

    public Hypothesis(String description, String value) {
        this.description = description;
        this.value = value;
    }

    public String toString() {
        return (description + " = " + value);
    }
}