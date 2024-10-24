package pt.isep.meia.AICare.infrastructure.gateways.dtos;

import lombok.Getter;

import java.util.List;

@Getter
public class PrologResultDto {
    private String question;
    private List<String> conclusion;
    private List<String> possibleAnswers;
    private PrologTypeEnum type;
}
