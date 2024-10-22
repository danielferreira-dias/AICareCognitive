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
    public static final String OTORRHAGIA = "otorrhagia";
    public static final String SKULL_FRACTURE = "skull_fracture";
    public static final String EPISTAXE = "epistaxe";
    public static final String HEMATHESE = "hemathese";
    public static final String MOUTH_HAEMORRHAGE = "mouth_haemorrhage";
    public static final String METRORRHAGIA = "metrorrhagia";
    public static final String HEMATURIA = "hematuria";
    public static final String MELENA = "melena";
    public static final String RECTAL_BLEEDING = "rectal_bleeding";
    public static final String UNKNOWN = "unknown";

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
