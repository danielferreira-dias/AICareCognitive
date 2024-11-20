package pt.isep.meia.AICare.domain.constants;

public class EvidenceConstants {
    // Diagnosis-related
    public static final String DIAGNOSIS = "diagnosis";
    public static final String DIAGNOSIS_ALZHEIMER = "diagnosis_alzheimer";
    public static final String DIAGNOSIS_ALZHEIMER_STAGE = "diagnosis_alzheimer_stage";
    public static final String DIAGNOSIS_PARKINSON = "diagnosis_parkinson";
    public static final String DIAGNOSIS_PARKINSON_STAGE = "diagnosis_parkinson_stage";
    public static final String DIAGNOSIS_VASCULAR_DEMENTIA = "diagnosis_vascular_dementia";
    public static final String DIAGNOSIS_VASCULAR_DEMENTIA_STAGE = "diagnosis_vascular_dementia_stage";

    // Alzheimer's Observations
    // Initial
    public static final String OBSERVATION_ALZHEIMER_SPATIAL_DISORIENTATION = "observation_alzheimer_spacial_disorientation";
    public static final String OBSERVATION_ALZHEIMER_MEMORY_LOSS_FRUSTRATION = "observation_alzheimer_memory_loss_frustration";
    public static final String OBSERVATION_ALZHEIMER_SLIGHT_MEMORY_LOSS = "observation_alzheimer_slight_memory_loss";

    // Advanced
    public static final String OBSERVATION_ALZHEIMER_STARE = "observation_alzheimer_stare";
    public static final String OBSERVATION_ALZHEIMER_NEEDS_CONSTANT_SUPERVISION = "observation_alzheimer_needs_constant_supervision";
    public static final String OBSERVATION_ALZHEIMER_UNABLE_TO_FOLLOW_STIMULI = "observation_alzheimer_unable_to_follow_stimuli";
    public static final String OBSERVATION_ALZHEIMER_HISTORY_OF_FALLS = "observation_alzheimer_history_of_falls";

    // Parkinson's Observations
    // Initial
    public static final String OBSERVATION_PARKINSON_SHAKING = "observation_parkinson_shaking";
    public static final String OBSERVATION_PARKINSON_LOCOMOTION_DIFFICULTIES = "observation_parkinson_locomotion_difficulties";
    public static final String OBSERVATION_PARKINSON_BENT_SPINE = "observation_parkinson_bent_spine";
    public static final String OBSERVATION_PARKINSON_BALANCE_LOSS = "observation_parkinson_balance_loss";
    public static final String OBSERVATION_HEARING_LOSS_ONSET = "observation_hearing_loss_onset";

    // Advanced
    public static final String OBSERVATION_PARKINSON_FINE_MOTOR_CONTROL = "observation_parkinson_fine_motor_control";
    public static final String OBSERVATION_PARKINSON_INTENSE_TREMORS = "observation_parkinson_intense_tremors";
    public static final String OBSERVATION_PARKINSON_COORDINATION_DIFFICULTIES = "observation_parkinson_coordination_difficulties";

    // Vascular Dementia Observations
    // Initial
    public static final String OBSERVATION_VASCULAR_DEMENTIA_SLIGHT_MEMORY_LOSS = "observation_vascular_dementia_slight_memory_loss";
    public static final String OBSERVATION_VASCULAR_DEMENTIA_DEPRESSION_ANXIETY = "observation_vascular_dementia_depression_anxiety";
    public static final String OBSERVATION_VASCULAR_DEMENTIA_THINKING_PROBLEMS = "observation_vascular_dementia_thinking_problems";

    // Advanced
    public static final String OBSERVATION_VASCULAR_DEMENTIA_MEMORY_RECALL_DIFFICULTIES = "observation_vascular_dementia_memory_recall_difficulties";
    public static final String OBSERVATION_VASCULAR_DEMENTIA_PEOPLE_RECOGNITION = "observation_vascular_dementia_people_recognition";
    public static final String OBSERVATION_VASCULAR_DEMENTIA_AGGRESSIVENESS_INSOMNIA_AGITATION = "observation_vascular_dementia_aggressiveness_insomnia_agitation";
    public static final String OBSERVATION_VASCULAR_DEMENTIA_MOTOR_PROBLEMS = "observation_vascular_dementia_motor_problems";

    // Conditions-related
    public static final String SOCIAL_INTEGRATION = "conditions_social_integration";
    public static final String VISION = "conditions_vision";
    public static final String HEARING = "conditions_hearing";
    public static final String SPEECH = "conditions_speech";
    public static final String SMELL = "conditions_smell";
    public static final String UPPER_MOTOR_SKILLS = "conditions_upper_motor_skills";
    public static final String LOWER_MOTOR_SKILLS = "conditions_lower_motor_skills";
    public static final String OBJECT_HANDLING = "conditions_object_handling";
    public static final String READING_CONDITION = "conditions_reading";
    public static final String WRITING = "conditions_writing";
    public static final String MOBILITY = "conditions_mobility";

    // Preferences-related
    public static final String THEATRE = "preferences_theatre";
    public static final String MUSEUM = "preferences_museum";
    public static final String MUSIC = "preferences_music";
    public static final String READING_PREFERENCE = "preferences_reading"; // avoid conflict with the condition evidence
    public static final String RECREATIONAL_GROUP = "preferences_recreational_group";
    public static final String ART = "preferences_art";
    public static final String SPORTS = "preferences_sports";
    public static final String COOKING = "preferences_cooking";
    public static final String HANDICRAFTS = "preferences_handicrafts";
}
