package pt.isep.meia.AICare.domain.entities;

import lombok.Getter;
import lombok.Setter;
import org.hibernate.annotations.GenericGenerator;
import org.hibernate.annotations.ManyToAny;

import javax.persistence.*;
import java.util.UUID;

@Entity
@Getter
@Setter
public class Activity {
    @Id
    @GeneratedValue(generator = "UUID")
    @GenericGenerator(
            name = "UUID",
            strategy = "org.hibernate.id.UUIDGenerator"
    )
    @Column(columnDefinition = "BINARY(16)", updatable = false, nullable = false)
    private UUID id;

    private String description;

    @ManyToOne
    @JoinColumn(name = "conclusion_id")
    private Conclusion conclusion;

    private int sortingOrder;
}
