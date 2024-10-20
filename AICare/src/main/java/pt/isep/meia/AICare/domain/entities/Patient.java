package pt.isep.meia.AICare.domain.entities;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import lombok.Getter;
import lombok.Setter;
import org.hibernate.annotations.GenericGenerator;
import pt.isep.meia.AICare.domain.model.GenderEnum;

import java.util.UUID;

@Setter
@Getter
@Entity
public class Patient {
    @Id
    @GeneratedValue(generator = "UUID")
    @GenericGenerator(
            name = "UUID",
            strategy = "org.hibernate.id.UUIDGenerator"
    )
    @Column(columnDefinition = "BINARY(16)", updatable = false, nullable = false)
    private UUID id;
    private String name;
    private int age;
    private GenderEnum gender;

    public Patient() {
    }

    public Patient(String name, int age, GenderEnum gender) {
        this.name = name;
        this.age = age;
        this.gender = gender;
    }
}
