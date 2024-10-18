package pt.isep.meia.AICare.models;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import lombok.Getter;
import lombok.Setter;

import java.util.UUID;

import static javax.persistence.GenerationType.*;

@Setter
@Getter
@Entity
public class Patient {
    @Id
    @Column(columnDefinition = "VARCHAR(36)")
    private UUID id;
    private String name;
    private int age;
    private GenderEnum gender;

    public Patient() {
        this.id = java.util.UUID.randomUUID();
    }

    public Patient(String name, int age, GenderEnum gender) {
        this();
        this.name = name;
        this.age = age;
        this.gender = gender;
    }
}
