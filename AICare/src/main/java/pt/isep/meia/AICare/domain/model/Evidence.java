package pt.isep.meia.AICare.domain.model;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import pt.isep.meia.AICare.domain.entities.Answer;
import pt.isep.meia.AICare.domain.entities.Question;

@Getter
@Setter
@AllArgsConstructor
public class Evidence {
    private Question question;
    private Answer answer;
}
