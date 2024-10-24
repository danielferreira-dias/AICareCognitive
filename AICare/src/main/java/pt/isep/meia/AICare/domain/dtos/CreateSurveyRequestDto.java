package pt.isep.meia.AICare.domain.dtos;

import lombok.Getter;

import java.util.UUID;

@Getter
public class CreateSurveyRequestDto {
    private UUID patientId;
}
