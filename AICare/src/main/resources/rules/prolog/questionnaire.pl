:- dynamic evidence/2.

:- consult('inference_engine.pl').

% Step 1: Diagnosis questions
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

% Step 2: Alzheimer's-related questions
next_question(observation_alzheimer_spacial_disorientation, [yes, no]) :-
    evidence(diagnosis_alzheimer, no),
    \+ evidence(observation_alzheimer_spacial_disorientation, _).

next_question(observation_alzheimer_memory_loss_frustration, [yes, no]) :-
    evidence(diagnosis_alzheimer, no),
    \+ evidence(observation_alzheimer_memory_loss_frustration, _).

next_question(observation_alzheimer_slight_memory_loss, [yes, no]) :-
    evidence(diagnosis_alzheimer, no),
    \+ evidence(observation_alzheimer_slight_memory_loss, _).

next_question(observation_alzheimer_stare, [yes, no]) :-
    ( evidence(diagnosis_alzheimer_stage, initial);
      findall(yes, (
        evidence(observation_alzheimer_spacial_disorientation, yes);
        evidence(observation_alzheimer_memory_loss_frustration, yes);
        evidence(observation_alzheimer_slight_memory_loss, yes)
      ), Results),
      length(Results, Count),
      Count >= 2),
    \+ evidence(observation_alzheimer_stare, _).

% Step 3: Parkinson's-related questions
next_question(observation_parkinson_shaking, [yes, no]) :-
    evidence(diagnosis_parkinson, no),
    \+ evidence(observation_parkinson_shaking, _).

next_question(observation_parkinson_locomotion_difficulties, [yes, no]) :-
    evidence(diagnosis_parkinson, no),
    \+ evidence(observation_parkinson_locomotion_difficulties, _).

next_question(observation_parkinson_bent_spine, [yes, no]) :-
    evidence(diagnosis_parkinson, no),
    \+ evidence(observation_parkinson_bent_spine, _).

next_question(observation_parkinson_fine_motor_control, [yes, no]) :-
    ( evidence(diagnosis_parkinson_stage, initial);
      findall(yes, (
        evidence(observation_parkinson_shaking, yes);
        evidence(observation_parkinson_locomotion_difficulties, yes);
        evidence(observation_parkinson_bent_spine, yes);
        evidence(observation_parkinson_balance_loss, yes)
      ), Results),
      length(Results, Count),
      Count >= 2),
    \+ evidence(observation_parkinson_fine_motor_control, _).

% Step 4: Vascular dementia-related questions
next_question(observation_vascular_dementia_slight_memory_loss, [yes, no]) :-
    evidence(diagnosis_vascular_dementia, no),
    \+ evidence(observation_vascular_dementia_slight_memory_loss, _).

next_question(observation_vascular_dementia_depression_anxiety, [yes, no]) :-
    evidence(diagnosis_vascular_dementia, no),
    \+ evidence(observation_vascular_dementia_depression_anxiety, _).

next_question(observation_vascular_dementia_people_recognition, [yes, no]) :-
    ( evidence(diagnosis_vascular_dementia_stage, initial);
      findall(yes, (
        evidence(observation_vascular_dementia_slight_memory_loss, yes);
        evidence(observation_vascular_dementia_depression_anxiety, yes);
        evidence(observation_vascular_dementia_thinking_problems, yes)
      ), Results),
      length(Results, Count),
      Count >= 2),
    \+ evidence(observation_vascular_dementia_people_recognition, _).

% Conditions
next_question(social_integration, [good_social_relations, severe_integration_issues, isolated_person]) :-
    \+ evidence(social_integration, _).

next_question(vision, [good_vision, vision_difficulty, blindness]) :-
    \+ evidence(vision, _).

next_question(hearing, [good_hearing, hearing_difficulty, deafness]) :-
    \+ evidence(hearing, _).

next_question(speech, [normal_speech, speech_difficulty, non_verbal]) :-
    \+ evidence(speech, _).

next_question(smell, [normal_smell, smell_difficulty, no_sense_of_smell]) :-
    \+ evidence(smell, _).

next_question(upper_motor_skills, [normal, difficulty_with_precise_movements, unable_to_use_upper_limbs]) :-
    \+ evidence(upper_motor_skills, _).

next_question(lower_motor_skills, [normal, difficulty_with_precise_movements, unable_to_use_lower_limbs]) :-
    \+ evidence(lower_motor_skills, _).

next_question(object_handling, [full_control, partial_control, unable_to_handle_objects]) :-
    \+ evidence(object_handling, _).

next_question(reading, [no_difficulty, some_difficulty, unable_to_read]) :-
    \+ evidence(reading, _).

next_question(writing, [no_difficulty, some_difficulty, unable_to_write]) :-
    \+ evidence(writing, _).

next_question(mobility, [moves_easily, needs_assistance, total_dependence]) :-
    \+ evidence(mobility, _).

% Preferences
next_question(theatre, [yes, no]):-
    \+ evidence(theatre, _).

next_question(museum, [yes, no]):-
    \+ evidence(museum, _).

next_question(music, [yes, no]):-
    \+ evidence(music, _).

next_question(reading, [yes, no]):-
    \+ evidence(reading, _).

next_question(recreational_group, [yes, no]):-
    \+ evidence(recreational_group, _).

next_question(art, [yes, no]):-
    \+ evidence(art, _).

next_question(sports, [yes, no]):-
    \+ evidence(sports, _).

next_question(cooking, [yes, no]):-
    \+ evidence(cooking, _).

next_question(handicrafts, [yes, no]):-
    \+ evidence(handicrafts, _).
