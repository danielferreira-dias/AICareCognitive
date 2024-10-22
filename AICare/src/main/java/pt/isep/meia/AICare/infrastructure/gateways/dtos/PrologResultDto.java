package pt.isep.meia.AICare.infrastructure.gateways.dtos;

import lombok.Getter;

@Getter
public class PrologResultDto {
    private String question;
    private String conclusion;
    private PrologTypeEnum type;
}
