:- dynamic evidence/2.

:- consult('inference_engine.pl'),
    write('Inference Engine loaded!'), nl.

% STEP 1: Diagnosis questions
next_question(diagnosis, [yes, no]) :-
    \+ evidence(diagnosis, _).

next_question(diagnosis_alzheimer, [yes, no]) :-
    evidence(diagnosis, yes),
    \+ evidence(diagnosis_alzheimer, _).

next_question(diagnosis_alzheimer_stage, [initial, advanced]) :-
    evidence(diagnosis_alzheimer, yes),
    \+ evidence(diagnosis_alzheimer_stage, _).

next_question(diagnosis_parkinson, [yes, no]) :-
    evidence(diagnosis, yes),
    \+ evidence(diagnosis_parkinson, _).

next_question(diagnosis_parkinson_stage, [initial, advanced]) :-
    evidence(diagnosis_parkinson, yes),
    \+ evidence(diagnosis_parkinson_stage, _).

next_question(diagnosis_vascular_dementia, [yes, no]) :-
    evidence(diagnosis, yes),
    \+ evidence(diagnosis_vascular_dementia, _).

next_question(diagnosis_vascular_dementia_stage, [initial, advanced]) :-
    evidence(diagnosis_vascular_dementia, yes),
    \+ evidence(diagnosis_vascular_dementia_stage, _).



% STEP 2: Alzheimers-related questions 
% INITIAL QUESTIONS 
next_question(observation_alzheimer_spacial_disorientation, [yes, no]) :-
    (evidence(diagnosis, no) ; evidence(diagnosis_alzheimer, no)),
    \+ evidence(observation_alzheimer_spacial_disorientation, _).

next_question(observation_alzheimer_memory_loss_frustration, [yes, no]) :-
    (evidence(diagnosis, no) ; evidence(diagnosis_alzheimer, no)),
    \+ evidence(observation_alzheimer_memory_loss_frustration, _).

next_question(observation_alzheimer_slight_memory_loss, [yes, no]) :-
    (evidence(diagnosis, no) ; evidence(diagnosis_alzheimer, no)),
    \+ evidence(observation_alzheimer_slight_memory_loss, _).

% ADVANCED QUESTIONS 
next_question(observation_alzheimer_stare, [yes, no]) :-
    ( evidence(diagnosis_alzheimer_stage, initial);
      %CHECK IF AT LEAST 2 INITIAL QUESTIONS = TRUE
      findall(yes, (
          evidence(observation_alzheimer_spacial_disorientation, yes); 
          evidence(observation_alzheimer_memory_loss_frustration, yes);
          evidence(observation_alzheimer_slight_memory_loss, yes)
      ), Results),
      length(Results, Count),
      Count >= 2),
      %CHECK IF AT MAX 1 ADVANCED QUESTIONS = TRUE
      findall(yes, (
        evidence(observation_alzheimer_stare, yes);
        evidence(observation_alzheimer_needs_constant_supervision, yes);
        evidence(observation_alzheimer_unable_to_follow_stimuli, yes);
        evidence(observation_alzheimer_history_of_falls, yes)
      ), AdvResults),
      length(AdvResults, AdvCount),
      AdvCount < 1,
    \+ evidence(observation_alzheimer_stare, _).

next_question(observation_alzheimer_needs_constant_supervision, [yes, no]) :-
    ( evidence(diagnosis_alzheimer_stage, initial);
       %CHECK IF AT LEAST 2 INITIAL QUESTIONS = TRUE
      findall(yes, (
        evidence(observation_alzheimer_spacial_disorientation, yes);
        evidence(observation_alzheimer_memory_loss_frustration, yes);
        evidence(observation_alzheimer_slight_memory_loss, yes)
      ), Results),
      length(Results, Count),
      Count >= 2),
      %CHECK IF AT MAX 1 ADVANCED QUESTIONS = TRUE
      findall(yes, (
        evidence(observation_alzheimer_stare, yes);
        evidence(observation_alzheimer_needs_constant_supervision, yes);
        evidence(observation_alzheimer_unable_to_follow_stimuli, yes);
        evidence(observation_alzheimer_history_of_falls, yes)
      ), AdvResults),
      length(AdvResults, AdvCount),
      AdvCount < 1,
    \+ evidence(observation_alzheimer_needs_constant_supervision, _).

next_question(observation_alzheimer_unable_to_follow_stimuli, [yes, no]) :-
    ( evidence(diagnosis_alzheimer_stage, initial);
      %CHECK IF AT LEAST 2 INITIAL QUESTIONS = TRUE
      findall(yes, (
        evidence(observation_alzheimer_spacial_disorientation, yes);
        evidence(observation_alzheimer_memory_loss_frustration, yes);
        evidence(observation_alzheimer_slight_memory_loss, yes)
      ), Results),
      length(Results, Count),
      Count >= 2),
      %CHECK IF AT MAX 1 ADVANCED QUESTIONS = TRUE
      findall(yes, (
        evidence(observation_alzheimer_stare, yes);
        evidence(observation_alzheimer_needs_constant_supervision, yes);
        evidence(observation_alzheimer_unable_to_follow_stimuli, yes);
        evidence(observation_alzheimer_history_of_falls, yes)
      ), AdvResults),
      length(AdvResults, AdvCount),
      AdvCount < 1,
    \+ evidence(observation_alzheimer_unable_to_follow_stimuli, _).

next_question(observation_alzheimer_history_of_falls, [yes, no]) :-
    ( evidence(diagnosis_alzheimer_stage, initial);
      %CHECK IF AT LEAST 2 INITIAL QUESTIONS = TRUE
      findall(yes, (
        evidence(observation_alzheimer_spacial_disorientation, yes);
        evidence(observation_alzheimer_memory_loss_frustration, yes);
        evidence(observation_alzheimer_slight_memory_loss, yes)
      ), Results),
      length(Results, Count),
      Count >= 2),
      %CHECK IF AT MAX 1 ADVANCED QUESTIONS = TRUE
      findall(yes, (
        evidence(observation_alzheimer_stare, yes);
        evidence(observation_alzheimer_needs_constant_supervision, yes);
        evidence(observation_alzheimer_unable_to_follow_stimuli, yes);
        evidence(observation_alzheimer_history_of_falls, yes)
      ), AdvResults),
      length(AdvResults, AdvCount),
      AdvCount < 1,
    \+ evidence(observation_alzheimer_history_of_falls, _).



% STEP 3: Parkinsons-related questions
% INITIAL QUESTIONS 
next_question(observation_parkinson_shaking, [yes, no]) :-
    (evidence(diagnosis, no) ; evidence(diagnosis_parkinson, no)),
    \+ evidence(observation_parkinson_shaking, _).

next_question(observation_parkinson_locomotion_difficulties, [yes, no]) :-
    (evidence(diagnosis, no) ; evidence(diagnosis_parkinson, no)),
    \+ evidence(observation_parkinson_locomotion_difficulties, _).

next_question(observation_parkinson_bent_spine, [yes, no]) :-
    (evidence(diagnosis, no) ; evidence(diagnosis_parkinson, no)),
    \+ evidence(observation_parkinson_bent_spine, _).

next_question(observation_parkinson_balance_loss, [yes, no]) :-
    (evidence(diagnosis, no) ; evidence(diagnosis_parkinson, no)),
    \+ evidence(observation_parkinson_balance_loss, _).

next_question(observation_hearing_loss_onset, [yes, no]) :-
    (evidence(diagnosis, no) ; evidence(diagnosis_parkinson, no)),
    \+ evidence(observation_hearing_loss_onset, _).

% ADVANCED QUESTIONS 
next_question(observation_parkinson_fine_motor_control, [yes, no]) :-
    ( evidence(diagnosis_parkinson_stage, initial);
      %CHECK IF AT LEAST 2 INITIAL QUESTIONS = TRUE
      findall(yes, (
        evidence(observation_parkinson_shaking, yes);
        evidence(observation_parkinson_locomotion_difficulties, yes);
        evidence(observation_parkinson_bent_spine, yes);
        evidence(observation_parkinson_balance_loss, yes);
        evidence(observation_hearing_loss_onset, yes)
      ), Results),
      length(Results, Count),
      Count >= 2),
      %CHECK IF AT MAX 1 ADVANCED QUESTIONS = TRUE
      findall(yes, (
        evidence(observation_parkinson_fine_motor_control, yes);
        evidence(observation_parkinson_intense_tremors, yes);
        evidence(observation_parkinson_coordination_difficulties, yes)
      ), AdvResults),
      length(AdvResults, AdvCount),
      AdvCount < 1,
    \+ evidence(observation_parkinson_fine_motor_control, _).

next_question(observation_parkinson_intense_tremors, [yes, no]) :-
    ( evidence(diagnosis_parkinson_stage, initial);
      %CHECK IF AT LEAST 2 INITIAL QUESTIONS = TRUE
      findall(yes, (
        evidence(observation_parkinson_shaking, yes);
        evidence(observation_parkinson_locomotion_difficulties, yes);
        evidence(observation_parkinson_bent_spine, yes);
        evidence(observation_parkinson_balance_loss, yes);
        evidence(observation_hearing_loss_onset, yes)
      ), Results),
      length(Results, Count),
      Count >= 2),
      %CHECK IF AT MAX 1 ADVANCED QUESTIONS = TRUE
      findall(yes, (
        evidence(observation_parkinson_fine_motor_control, yes);
        evidence(observation_parkinson_intense_tremors, yes);
        evidence(observation_parkinson_coordination_difficulties, yes)
      ), AdvResults),
      length(AdvResults, AdvCount),
      AdvCount < 1,
    \+ evidence(observation_parkinson_intense_tremors, _).

next_question(observation_parkinson_coordination_difficulties, [yes, no]) :-
    ( evidence(diagnosis_parkinson_stage, initial);
      %CHECK IF AT LEAST 2 INITIAL QUESTIONS = TRUE
      findall(yes, (
        evidence(observation_parkinson_shaking, yes);
        evidence(observation_parkinson_locomotion_difficulties, yes);
        evidence(observation_parkinson_bent_spine, yes);
        evidence(observation_parkinson_balance_loss, yes);
        evidence(observation_hearing_loss_onset, yes)
      ), Results),
      length(Results, Count),
      Count >= 2),
      %CHECK IF AT MAX 1 ADVANCED QUESTIONS = TRUE
      findall(yes, (
        evidence(observation_parkinson_fine_motor_control, yes);
        evidence(observation_parkinson_intense_tremors, yes);
        evidence(observation_parkinson_coordination_difficulties, yes)
      ), AdvResults),
      length(AdvResults, AdvCount),
      AdvCount < 1,
    \+ evidence(observation_parkinson_coordination_difficulties, _).



% STEP 4: Vascular dementia-related questions
% INITIAL QUESTIONS 
next_question(observation_vascular_dementia_slight_memory_loss, [yes, no]) :-
    (evidence(diagnosis, no) ; evidence(diagnosis_vascular_dementia, no)),
    \+ evidence(observation_vascular_dementia_slight_memory_loss, _).

next_question(observation_vascular_dementia_depression_anxiety, [yes, no]) :-
    (evidence(diagnosis, no) ; evidence(diagnosis_vascular_dementia, no)),
    \+ evidence(observation_vascular_dementia_depression_anxiety, _).

next_question(observation_vascular_dementia_thinking_problems, [yes, no]) :-
    (evidence(diagnosis, no) ; evidence(diagnosis_vascular_dementia, no)),
    \+ evidence(observation_vascular_dementia_thinking_problems, _).

% ADVANCED QUESTIONS 
next_question(observation_vascular_dementia_memory_recall_difficulties, [yes, no]) :-
    ( evidence(diagnosis_vascular_dementia_stage, initial);
      %CHECK IF AT LEAST 2 INITIAL QUESTIONS = TRUE
      findall(yes, (
        evidence(observation_vascular_dementia_slight_memory_loss, yes);
        evidence(observation_vascular_dementia_depression_anxiety, yes);
        evidence(observation_vascular_dementia_thinking_problems, yes)
      ), Results),
      length(Results, Count),
      Count >= 2),
      %CHECK IF AT MAX 1 ADVANCED QUESTIONS = TRUE
      findall(yes, (
        evidence(observation_vascular_dementia_memory_recall_difficulties, yes);
        evidence(observation_vascular_dementia_people_recognition, yes);
        evidence(observation_vascular_dementia_aggressiveness_insomnia_agitation, yes);
        evidence(observation_vascular_dementia_motor_problems, yes)
      ), AdvResults),
      length(AdvResults, AdvCount),
      AdvCount < 1,
    \+ evidence(observation_vascular_dementia_memory_recall_difficulties, _).

next_question(observation_vascular_dementia_people_recognition, [yes, no]) :-
    ( evidence(diagnosis_vascular_dementia_stage, initial);
      %CHECK IF AT LEAST 2 INITIAL QUESTIONS = TRUE
      findall(yes, (
        evidence(observation_vascular_dementia_slight_memory_loss, yes);
        evidence(observation_vascular_dementia_depression_anxiety, yes);
        evidence(observation_vascular_dementia_thinking_problems, yes)
      ), Results),
      length(Results, Count),
      Count >= 2),
      %CHECK IF AT MAX 1 ADVANCED QUESTIONS = TRUE
      findall(yes, (
        evidence(observation_vascular_dementia_memory_recall_difficulties, yes);
        evidence(observation_vascular_dementia_people_recognition, yes);
        evidence(observation_vascular_dementia_aggressiveness_insomnia_agitation, yes);
        evidence(observation_vascular_dementia_motor_problems, yes)
      ), AdvResults),
      length(AdvResults, AdvCount),
      AdvCount < 1,
    \+ evidence(observation_vascular_dementia_people_recognition, _).

next_question(observation_vascular_dementia_aggressiveness_insomnia_agitation, [yes, no]) :-
    ( evidence(diagnosis_vascular_dementia_stage, initial);
      %CHECK IF AT LEAST 2 INITIAL QUESTIONS = TRUE
      findall(yes, (
        evidence(observation_vascular_dementia_slight_memory_loss, yes);
        evidence(observation_vascular_dementia_depression_anxiety, yes);
        evidence(observation_vascular_dementia_thinking_problems, yes)
      ), Results),
      length(Results, Count),
      Count >= 2),
      %CHECK IF AT MAX 1 ADVANCED QUESTIONS = TRUE
      findall(yes, (
        evidence(observation_vascular_dementia_memory_recall_difficulties, yes);
        evidence(observation_vascular_dementia_people_recognition, yes);
        evidence(observation_vascular_dementia_aggressiveness_insomnia_agitation, yes);
        evidence(observation_vascular_dementia_motor_problems, yes)
      ), AdvResults),
      length(AdvResults, AdvCount),
      AdvCount < 1,
    \+ evidence(observation_vascular_dementia_aggressiveness_insomnia_agitation, _).

next_question(observation_vascular_dementia_motor_problems, [yes, no]) :-
    ( evidence(diagnosis_vascular_dementia_stage, initial);
      %CHECK IF AT LEAST 2 INITIAL QUESTIONS = TRUE
      findall(yes, (
        evidence(observation_vascular_dementia_slight_memory_loss, yes);
        evidence(observation_vascular_dementia_depression_anxiety, yes);
        evidence(observation_vascular_dementia_thinking_problems, yes)
      ), Results),
      length(Results, Count),
      Count >= 2),
      %CHECK IF AT MAX 1 ADVANCED QUESTIONS = TRUE
      findall(yes, (
        evidence(observation_vascular_dementia_memory_recall_difficulties, yes);
        evidence(observation_vascular_dementia_people_recognition, yes);
        evidence(observation_vascular_dementia_aggressiveness_insomnia_agitation, yes);
        evidence(observation_vascular_dementia_motor_problems, yes)
      ), AdvResults),
      length(AdvResults, AdvCount),
      AdvCount < 1,
    \+ evidence(observation_vascular_dementia_motor_problems, _).



% CONDITIONS
next_question(conditions_social_integration, [good_social_relations, severe_integration_issues, isolated_person]) :-
    \+ evidence(conditions_social_integration, _).

next_question(conditions_vision, [good_vision, vision_with_difficulties, blindness]) :-
    \+ evidence(conditions_vision, _).

next_question(conditions_hearing, [good_hearing, hearing_with_difficulties, deafness]) :-
    \+ evidence(conditions_hearing, _).

next_question(conditions_speech, [speaks_normally, speaks_with_difficulty, cannot_be_understood]) :-
    \+ evidence(conditions_speech, _).

next_question(conditions_smell, [smell_normally, smell_with_difficulty, no_sense_of_smell]) :-
    \+ evidence(conditions_smell, _).

next_question(conditions_upper_motor_skills, [ums_functions_normally, ums_has_difficulty, unable_to_use_upper_limbs]) :-
    \+ evidence(conditions_upper_motor_skills, _).

next_question(conditions_lower_motor_skills, [lms_functions_normally, lms_has_difficulty, unable_to_use_lower_limbs]) :-
    \+ evidence(conditions_lower_motor_skills, _).

next_question(conditions_object_handling, [full_control, partial_control, cannot_handle]) :-
    \+ evidence(conditions_object_handling, _).

next_question(conditions_reading, [reading_normally, writes_with_difficulty, cannot_read]) :-
    \+ evidence(conditions_reading, _).

next_question(conditions_writing, [writes_normally, writes_some_difficulty, cannot_write]) :-
    \+ evidence(conditions_writing, _).

next_question(conditions_mobility, [moves_easily, needs_assistance, total_dependence]) :-
    \+ evidence(conditions_mobility, _).



% Preferences
next_question(preferences_theatre, [yes, no]):-
    \+ evidence(preferences_theatre, _).

next_question(preferences_museum, [yes, no]):-
    \+ evidence(preferences_museum, _).

next_question(preferences_music, [yes, no]):-
    \+ evidence(preferences_music, _).

next_question(preferences_reading, [yes, no]):-
    \+ evidence(preferences_reading, _).

next_question(preferences_recreational_group, [yes, no]):-
    \+ evidence(preferences_recreational_group, _).

next_question(preferences_art, [yes, no]):-
    \+ evidence(preferences_art, _).

next_question(preferences_sports, [yes, no]):-
    \+ evidence(preferences_sports, _).

next_question(preferences_cooking, [yes, no]):-
    \+ evidence(preferences_cooking, _).

next_question(preferences_handicrafts, [yes, no]):-
    \+ evidence(preferences_handicrafts, _).
