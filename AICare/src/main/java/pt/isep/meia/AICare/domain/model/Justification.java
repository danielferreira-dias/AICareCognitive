package pt.isep.meia.AICare.domain.model;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

import java.util.List;

@Getter
@Setter
@AllArgsConstructor
public class Justification {
    private JustificationTypeEnum type;
    private String response;
    private List<String> rulesTriggered;
}
