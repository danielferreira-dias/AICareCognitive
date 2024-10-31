package pt.isep.meia.AICare.domain.dtos;

import lombok.Getter;

@Getter
public class PrologJustificationDto {
    private String justification;
    private String ruleTriggered;
}
