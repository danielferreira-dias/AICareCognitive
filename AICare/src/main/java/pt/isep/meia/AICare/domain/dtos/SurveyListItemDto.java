package pt.isep.meia.AICare.domain.dtos;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

import java.util.Date;
import java.util.UUID;

@Getter
@Setter
@AllArgsConstructor
public class SurveyListItemDto {
    private UUID id;
    private Date createDate;
    private boolean hasConclusion;
}
