package pt.isep.meia.AICare.domain.entities;

import lombok.Getter;
import org.hibernate.annotations.GenericGenerator;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import java.util.UUID;

@Getter
@Entity
public class Conclusion {
    public static final String OTORRHAGIA = "Otorrhagia";
    public static final String SKULL_FRACTURE = "Skull fracture";
    public static final String EPISTAXE = "Epistaxe";
    public static final String HEMATHESE = "Hemathese";
    public static final String MOUTH_HAEMORRHAGE = "Mouth haemorrhage";
    public static final String METRORRHAGIA = "Metrorrhagia";
    public static final String HEMATURIA = "Hematuria";
    public static final String MELENA = "Melena";
    public static final String RECTAL_BLEEDING = "Rectal bleeding";
    public static final String UNKNOWN = "Consult the doctor!";

    @Id
    @GeneratedValue(generator = "UUID")
    @GenericGenerator(
            name = "UUID",
            strategy = "org.hibernate.id.UUIDGenerator"
    )
    @Column(columnDefinition = "BINARY(16)", updatable = false, nullable = false)
    private UUID id;

    private String description;

    public Conclusion() {
    }

    public Conclusion(String description) {
        this.description = description;
    }

    public String toString() {
        return ("Conclusion: " + description);
    }
}
