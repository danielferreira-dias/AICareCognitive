package pt.isep.meia.AICare.domain.entities;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import lombok.Getter;
import lombok.Setter;
import org.hibernate.annotations.GenericGenerator;
import pt.isep.meia.AICare.domain.dtos.SurveyListItemDto;

import java.util.Date;
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

    private Date createDate;

    public Survey() {
        this.createDate = Date.from(java.time.Instant.now());
    }

    public Survey(UUID patientId) {
        this();
        this.patientId = patientId;
    }

    public SurveyListItemDto toListItemDto(Conclusion conclusion) {
        return new SurveyListItemDto(this.id, this.createDate, conclusion != null);
    }
}
