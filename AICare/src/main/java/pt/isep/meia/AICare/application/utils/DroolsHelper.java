package pt.isep.meia.AICare.application.utils;

import pt.isep.meia.AICare.domain.constants.AnswerConstants;
import pt.isep.meia.AICare.domain.model.Evidence;

import java.util.List;

public class DroolsHelper {
    public static boolean answer(List<Evidence> evidences, String evidence, String expectedAnswer) {
        for (Evidence qa : evidences) {
            if (qa.getQuestion().getText().equalsIgnoreCase(evidence) &&
                    qa.getAnswer().getResponse().equalsIgnoreCase(expectedAnswer)) {
                return true;
            }
        }
        return false;
    }

    public static boolean answer(List<Evidence> evidences, String evidence) {
        for (Evidence qa : evidences) {
            if (qa.getQuestion().getText().equalsIgnoreCase(evidence)) {
                return true;
            }
        }
        return false;
    }

    public static int getSymptomCount(List<Evidence> evidences, List<String> symptomKeys) {
        return (int) symptomKeys.stream()
                .filter(key -> evidences.stream()
                        .anyMatch(e -> e.getQuestion().getText().equals(key) && e.getAnswer().getResponse().equals(AnswerConstants.YES)))
                .count();
    }
}
