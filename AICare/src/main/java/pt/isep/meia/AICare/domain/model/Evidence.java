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
    public static final String BLOOD_EAR = "blood_ear";
    public static final String EARACHE = "earache";
    public static final String DEAFNESS = "deafness";
    public static final String CEREBROSPINAL = "cerebrospinal";
    public static final String BLOOD_NOSE = "blood_nose";
    public static final String BLOOD_MOUTH = "blood_mouth";
    public static final String BLOOD_BROWN = "blood_brown";
    public static final String VOMITING = "vomiting";
    public static final String BLOOD_VAGINA = "blood_vagina";
    public static final String BLOOD_PENIS = "blood_penis";
    public static final String BLOOD_ANUS = "blood_anus";
    public static final String BLOOD_COFFEE = "blood_coffee";

    private Question question;
    private Answer answer;
}
