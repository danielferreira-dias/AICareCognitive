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
