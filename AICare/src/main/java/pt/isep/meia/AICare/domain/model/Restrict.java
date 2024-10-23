package pt.isep.meia.AICare.domain.model;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class Restrict {
    private String activity;

    public Restrict(String activity) {
        this.activity = activity;
    }

    @Override
    public String toString() {
        return "Restrict{" +
                "activity='" + activity + '\'' +
                '}';
    }
}
