package pt.isep.meia.AICare.domain.entities;

import lombok.Getter;
import lombok.Setter;
import org.hibernate.annotations.GenericGenerator;

import javax.persistence.*;
import java.util.List;
import java.util.UUID;

@Getter
@Setter
@Entity
public class Question {
    @Id
    @GeneratedValue(generator = "UUID")
    @GenericGenerator(
            name = "UUID",
            strategy = "org.hibernate.id.UUIDGenerator"
    )
    @Column(columnDefinition = "BINARY(16)", updatable = false, nullable = false)
    private UUID id;

    @Column(columnDefinition = "BINARY(16)", nullable = false)
    private UUID surveyId;

    private String text;

    @ElementCollection
    @CollectionTable(name = "question_possible_answers", joinColumns = @JoinColumn(name = "question_id"))
    @Column(name = "possible_answer")
    private List<String> possibleAnswers;

    public Question() {
    }

    public Question(UUID surveyId, String text) {
        this.surveyId = surveyId;
        this.text = text;
    }

    @Override
    public String toString() {
        return "Question{" +
                "id=" + id +
                ", surveyId=" + surveyId +
                ", text='" + text + '\'' +
                ", possibleAnswers=" + possibleAnswers +
                '}';
    }
}
