package pt.isep.meia.AICare.domain.dtos;

import lombok.Getter;

import java.util.List;

@Getter
public class PrologJustificationDto {
    public List<List<String>> justifications;
}
