package pt.isep.meia.AICare.domain.model;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@AllArgsConstructor
public class Restrict {
    private String restrictingRule;

    private String activity;

    private String reason;

    @Override
    public String toString() {
        return "Restrict{" +
                "restrictingRule='" + restrictingRule + '\'' +
                ", activity='" + activity + '\'' +
                ", reason='" + reason + '\'' +
                '}';
    }
}
