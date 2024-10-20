package pt.isep.meia.AICare.domain.entities;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import lombok.Getter;
import lombok.Setter;
import org.hibernate.annotations.GenericGenerator;

import java.time.LocalDateTime;
import java.util.UUID;

@Setter
@Getter
@Entity
public class Survey {
    @Id
    @GeneratedValue(generator = "UUID")
    @GenericGenerator(
            name = "UUID",
            strategy = "org.hibernate.id.UUIDGenerator"
    )
    @Column(columnDefinition = "BINARY(16)", updatable = false, nullable = false)
    private UUID id;

    @Column(columnDefinition = "BINARY(16)", nullable = false)
    private UUID patientId;

    private LocalDateTime startTime;
    private LocalDateTime endTime;

    public Survey() {
    }

    public Survey(UUID patientId, LocalDateTime startTime) {
        this.patientId = patientId;
        this.startTime = startTime;
    }

}
