package pt.isep.meia.AICare.domain.constants;

import java.util.Arrays;
import java.util.List;

public class ActivityConstants {
    // Plastic Expression
    public static final String CONSTRUCTION_TECHNIQUE = "construction_technique";
    public static final String RECYCLED_MATERIALS = "recycled_materials";
    public static final String EXECUTION_FESTIVAL_WORK = "execution_festival_work";
    public static final String FELT_WORK = "felt_work";
    public static final String SMALL_ARRANGEMENTS = "small_arrangements";
    public static final String PAINTINGS = "paintings";
    public static final String CENTRE_DECORATION_WORK = "centre_decoration_work";

    // Daily Life
    public static final String DESSERT_MAKING = "dessert_making";

    // Cognitive and Sensory Stimulation
    public static final String TEMPORAL_ORIENTATION_EXERCISE = "temporal_orientation_exercise";
    public static final String KNOWLEDGE_EXERCISES = "knowledge_exercises";
    public static final String SPATIAL_RECOGNITION_EXERCISES = "spatial_recognition_exercises";
    public static final String VERBAL_TASK_EXERCISES = "verbal_task_exercises";
    public static final String IMAGE_RECOGNITION_GAMES = "image_recognition_games";
    public static final String MEMORY_EXERCISES = "memory_exercises";
    public static final String MANUAL_DEXTERITY_GAMES = "manual_dexterity_games";
    public static final String READING_WRITING_EXERCISES = "reading_writing_exercises";
    public static final String DIFFERENCES_GAMES = "differences_games";
    public static final String NUMERICAL_COMPREHENSION_EXERCISES = "numerical_comprehension_exercises";
    public static final String SENSES_RECOGNITION_EXERCISES = "senses_recognition_exercises";
    public static final String TASTE_REACTION = "taste_reaction";
    public static final String ODOR_DIFFERENTIATION = "odor_differentiation";
    public static final String ANIMAL_VOICES_RECOGNITION = "animal_voices_recognition";

    // Physical Exercise
    public static final String WALKING = "walking";
    public static final String WARMUP_EXERCISES = "warmup_exercises";
    public static final String PHYSIOTHERAPY = "physiotherapy";

    // Musical Entertainment
    public static final String KARAOKE = "karaoke";
    public static final String MUSICOGRAM = "musicogram";
    public static final String INSTRUMENTS_USE = "instruments_use";
    public static final String GUESS_THE_SONG = "guess_the_song";

    // Physical Animation Games
    public static final String PETANQUE_GAMES = "petanque_games";
    public static final String BOWLING_GAMES = "bowling_games";
    public static final String ADAPTED_HANDKERCHIEF_GAMES = "adapted_handkerchief_games";
    public static final String MIME_GAMES = "mime_games";

    // Social and Cultural: Entertainment Games
    public static final String BINGO_GAMES = "bingo_games";
    public static final String MISSAKO_GAMES = "missako_games";
    public static final String DOMINO_GAMES = "domino_games";
    public static final String TIC_TAC_TOE_GAMES = "tic_tac_toe_games";

    // Social and Cultural: Beauty Care
    public static final String MANICURE = "manicure";
    public static final String MASSAGES = "massages";
    public static final String MAKEUP = "makeup";

    // Social and Cultural: Theatre Workshop
    public static final String DRAMATIC_TEXT_READING = "dramatic_text_reading";
    public static final String CHARACTER_INTERPRETATION = "character_interpretation";

    public static List<String> getAllActivities() {
        return Arrays.asList(
                CONSTRUCTION_TECHNIQUE,
                RECYCLED_MATERIALS,
                EXECUTION_FESTIVAL_WORK,
                FELT_WORK,
                SMALL_ARRANGEMENTS,
                PAINTINGS,
                CENTRE_DECORATION_WORK,
                DESSERT_MAKING,
                TEMPORAL_ORIENTATION_EXERCISE,
                KNOWLEDGE_EXERCISES,
                SPATIAL_RECOGNITION_EXERCISES,
                VERBAL_TASK_EXERCISES,
                IMAGE_RECOGNITION_GAMES,
                MEMORY_EXERCISES,
                MANUAL_DEXTERITY_GAMES,
                READING_WRITING_EXERCISES,
                DIFFERENCES_GAMES,
                NUMERICAL_COMPREHENSION_EXERCISES,
                SENSES_RECOGNITION_EXERCISES,
                TASTE_REACTION,
                ODOR_DIFFERENTIATION,
                ANIMAL_VOICES_RECOGNITION,
                WALKING,
                WARMUP_EXERCISES,
                PHYSIOTHERAPY,
                KARAOKE,
                MUSICOGRAM,
                INSTRUMENTS_USE,
                GUESS_THE_SONG,
                PETANQUE_GAMES,
                BOWLING_GAMES,
                ADAPTED_HANDKERCHIEF_GAMES,
                MIME_GAMES,
                BINGO_GAMES,
                MISSAKO_GAMES,
                DOMINO_GAMES,
                TIC_TAC_TOE_GAMES,
                MANICURE,
                MASSAGES,
                MAKEUP,
                DRAMATIC_TEXT_READING,
                CHARACTER_INTERPRETATION);
    }
}
