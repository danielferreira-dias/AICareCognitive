package pt.isep.meia.AICare.models;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import lombok.Getter;
import lombok.Setter;

import java.time.LocalDateTime;
import java.util.UUID;

@Setter
@Getter
@Entity
public class SurveySession {
    @Id
    @GeneratedValue
    @Column(columnDefinition = "VARCHAR(36)")
    private UUID id;

    private UUID patientId;

    private LocalDateTime startTime;
    private LocalDateTime endTime;

    public SurveySession() {
        this.id = java.util.UUID.randomUUID();
    }

    public SurveySession(UUID patientId, LocalDateTime startTime) {
        this();
        this.patientId = patientId;
        this.startTime = startTime;
    }

}
