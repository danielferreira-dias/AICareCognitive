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
    public static final String OBSERVATION_ALZHEIMER_SPATIAL_DISORIENTATION = "observation_alzheimer_spatial_disorientation";
    public static final String OBSERVATION_ALZHEIMER_MEMORY_LOSS_FRUSTRATION = "observation_alzheimer_memory_loss_frustration";
    public static final String OBSERVATION_ALZHEIMER_SLIGHT_MEMORY_LOSS = "observation_alzheimer_slight_memory_loss";
    public static final String OBSERVATION_ALZHEIMER_STARE = "observation_alzheimer_stare";
    public static final String OBSERVATION_ALZHEIMER_NEEDS_PERMANENT_WATCH = "observation_alzheimer_needs_permanent_watch";
    public static final String OBSERVATION_ALZHEIMER_CANT_EXECUTE_STIMULI = "observation_alzheimer_cant_execute_stimuli";
    public static final String OBSERVATION_ALZHEIMER_FALL_HISTORY = "observation_alzheimer_fall_history";

    // Parkinson's Observations
    public static final String OBSERVATION_PARKINSON_SHAKING = "observation_parkinson_shaking";
    public static final String OBSERVATION_PARKINSON_BENT_SPINE = "observation_parkinson_bent_spine";
    public static final String OBSERVATION_PARKINSON_BALANCE_LOSS = "observation_parkinson_balance_loss";
    public static final String OBSERVATION_PARKINSON_INITIAL_HEARING_LOSS = "observation_parkinson_initial_hearing_loss";
    public static final String OBSERVATION_PARKINSON_LOCOMOTION_DIFFICULTIES = "observation_parkinson_locomotion_difficulties";
    public static final String OBSERVATION_PARKINSON_INTENSE_SHAKING = "observation_parkinson_intense_shaking";
    public static final String OBSERVATION_PARKINSON_FINE_MOTOR_CONTROL = "observation_parkinson_fine_motor_control";

    // Vascular Dementia Observations
    public static final String OBSERVATION_VASCULAR_DEMENTIA_SLIGHT_MEMORY_LOSS = "observation_vascular_dementia_slight_memory_loss";
    public static final String OBSERVATION_VASCULAR_DEMENTIA_DEPRESSION_ANXIETY = "observation_vascular_dementia_depression_anxiety";
    public static final String OBSERVATION_VASCULAR_DEMENTIA_THINKING_PROBLEMS = "observation_vascular_dementia_thinking_problems";
    public static final String OBSERVATION_VASCULAR_DEMENTIA_HEAVY_MEMORY_LOSS = "observation_vascular_dementia_heavy_memory_loss";
    public static final String OBSERVATION_VASCULAR_DEMENTIA_PEOPLE_RECOGNITION = "observation_vascular_dementia_people_recognition";
    public static final String OBSERVATION_VASCULAR_DEMENTIA_HISTORY_AGGRESSIVENESS_INSOMNIA_AGITATION = "observation_vascular_dementia_history_aggressiveness_insomnia_agitation";
    public static final String OBSERVATION_VASCULAR_DEMENTIA_BODY_CONTROL = "observation_vascular_dementia_body_control";

    // Conditions-related
    public static final String SOCIAL_INTEGRATION = "social_integration";
    public static final String VISION = "vision";
    public static final String HEARING = "hearing";
    public static final String SPEECH = "speech";
    public static final String SMELL = "smell";
    public static final String UPPER_MOTOR_SKILLS = "upper_motor_skills";
    public static final String LOWER_MOTOR_SKILLS = "lower_motor_skills";
    public static final String OBJECT_HANDLING = "object_handling";
    public static final String READING_CONDITION = "reading";
    public static final String WRITING = "writing";
    public static final String MOBILITY = "mobility";

    // Preferences-related
    public static final String THEATRE = "theatre";
    public static final String MUSEUM = "museum";
    public static final String MUSIC = "music";
    public static final String READING_PREFERENCE = "reading"; // avoid conflict with the condition evidence
    public static final String RECREATIONAL_GROUP = "recreational_group";
    public static final String ART = "art";
    public static final String SPORTS = "sports";
    public static final String COOKING = "cooking";
    public static final String HANDICRAFTS = "handicrafts";
}
