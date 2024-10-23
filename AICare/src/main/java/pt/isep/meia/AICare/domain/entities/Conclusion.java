package pt.isep.meia.AICare.domain.entities;

import lombok.Getter;
import org.hibernate.annotations.GenericGenerator;

import javax.persistence.*;
import java.util.List;
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

    @OneToMany(fetch = FetchType.LAZY, cascade = CascadeType.ALL)
    @JoinColumn(name = "conclusion_id")
    private List<Activity> activities;

    @Column(columnDefinition = "BINARY(16)")
    private UUID surveyId;

    public Conclusion() {
    }

    public Conclusion(UUID surveyId, List<Activity> activities) {
        this.surveyId = surveyId;
        this.activities = activities;
    }
}
