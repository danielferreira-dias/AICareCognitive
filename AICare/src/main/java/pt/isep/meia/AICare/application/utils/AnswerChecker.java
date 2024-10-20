package pt.isep.meia.AICare.application.utils;

import pt.isep.meia.AICare.domain.model.Evidence;

import java.util.List;

public class AnswerChecker {
    public static boolean answer(List<Evidence> evidences, String evidence, String expectedAnswer) {
        for (Evidence qa : evidences) {
            if (qa.getQuestion().getText().equalsIgnoreCase(evidence) &&
                    qa.getAnswer().getResponse().equalsIgnoreCase(expectedAnswer)) {
                return true;
            }
        }
        return false;
    }
}
