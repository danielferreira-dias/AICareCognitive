:- dynamic evidence/2.

%------------------------------------------------------------------------
% FACTS
%------------------------------------------------------------------------

% DISEASE DEFINITIONS:
disease(alzheimer_initial) :-
    evidence(diagnosis, yes),
    evidence(diagnosis_alzheimer, yes),
    evidence(diagnosis_alzheimer_stage, initial).
disease(alzheimer_initial) :-
    findall(yes, (
        evidence(observation_alzheimer_spacial_disorientation, yes); 
        evidence(observation_alzheimer_memory_loss_frustration, yes);
        evidence(observation_alzheimer_slight_memory_loss, yes)
    ), Results),
    length(Results, Count),
    Count >= 2.
disease(alzheimer_advanced) :-
    evidence(diagnosis, yes),
    evidence(diagnosis_alzheimer, yes),
    evidence(diagnosis_alzheimer_stage, advanced).
disease(alzheimer_advanced) :-
    disease(alzheimer_initial),
    findall(yes, (
        evidence(observation_alzheimer_stare, yes);
        evidence(observation_alzheimer_needs_constant_supervision, yes);
        evidence(observation_alzheimer_unable_to_follow_stimuli, yes);
        evidence(observation_alzheimer_history_of_falls, yes)
    ), Results),
    length(Results, Count),
    Count >= 1.


disease(parkinson_initial) :-
    evidence(diagnosis, yes),
    evidence(diagnosis_parkinson, yes),
    evidence(diagnosis_parkinson_stage, initial).
disease(parkinson_initial) :-
    findall(yes, (
        evidence(observation_parkinson_shaking, yes);
        evidence(observation_parkinson_locomotion_difficulties, yes);
        evidence(observation_parkinson_bent_spine, yes);
        evidence(observation_parkinson_balance_loss, yes);
        evidence(observation_hearing_loss_onset, yes)
    ), Results),
    length(Results, Count),
    Count >= 2.
disease(parkinson_advanced):-
    evidence(diagnosis, yes),
    evidence(diagnosis_parkinson, yes),
    evidence(diagnosis_parkinson_stage, advanced).
disease(parkinson_advanced) :-
    disease(parkinson_initial),
    findall(yes, (
        evidence(observation_parkinson_fine_motor_control, yes);
        evidence(observation_parkinson_intense_tremors, yes);
        evidence(observation_parkinson_coordination_difficulties, yes)
    ), Results),
    length(Results, Count),
    Count >= 1.

    
disease(vascular_dementia_initial):-
    evidence(diagnosis, yes),
    evidence(diagnosis_vascular_dementia, yes),
    evidence(diagnosis_vascular_dementia_stage, initial).
disease(vascular_dementia_initial) :-
    findall(yes, (
        evidence(observation_vascular_dementia_slight_memory_loss, yes);
        evidence(observation_vascular_dementia_depression_anxiety, yes);
        evidence(observation_vascular_dementia_thinking_problems, yes)
    ), Results),
    length(Results, Count),
    Count >= 2.
disease(vascular_dementia_advanced):-
    evidence(diagnosis, yes),
    evidence(diagnosis_vascular_dementia, yes),
    evidence(diagnosis_vascular_dementia_stage, advanced).
disease(vascular_dementia_advanced) :-
    disease(vascular_dementia_initial),
    findall(yes, (
        evidence(observation_vascular_dementia_memory_recall_difficulties, yes);
        evidence(observation_vascular_dementia_people_recognition, yes);
        evidence(observation_vascular_dementia_aggressiveness_insomnia_agitation, yes);
        evidence(observation_vascular_dementia_motor_problems, yes)
    ), Results),
    length(Results, Count),
    Count >= 1.


get_diseases(Diseases) :-
    findall(X, disease(X), DiseasesList),
    sort(DiseasesList, Diseases).

%------------------------------------------------------------------------

% ACTIVITY DEFINITIONS:
%Plastic Expression
activity(construction_technique).
activity(recycled_materials).
activity(execution_festival_work).
activity(felt_work).
activity(small_arrangements).
activity(paintings).
activity(centre_decoration_work).

%Daily Life
activity(dessert_making).

%Cognitive and Sensory Stimulation
activity(temporal_orientation_exercise).
activity(knowledge_exercises).
activity(spatial_recognition_exercises).
activity(verbal_task_exercises).
activity(image_recognition_games).
activity(memory_exercises).
activity(manual_dexterity_games).
activity(reading_writing_exercises).
activity(differences_games).
activity(numerical_comprehension_exercises).
activity(senses_recognition_exercises).
activity(taste_reaction).
activity(odor_differentiation).
activity(animal_voices_recognition).

%Physical Exercise
activity(walking).
activity(warmup_exercises).
activity(physiotherapy).

%Musical Entertainment
activity(karaoke).
activity(musicogram).
activity(instruments_use).
activity(guess_the_song).

%Physical Animation Games
activity(petanque_games).
activity(bowling_games).
activity(adapted_handkerchief_games).
activity(mime_games).

%Social and Cultural: Entertainment Games
activity(bingo_games).
activity(missako_games).
activity(domino_games).
activity(tic_tac_toe_games).

%Social and Cultural: Beauty Care
activity(manicure).
activity(massages).
activity(makeup).

%Social and Cultural: Theatre Workshop
activity(dramatic_text_reading).
activity(character_interpretation).

%------------------------------------------------------------------------

% DISEASE/ACTIVITY RELATION (CANNOT PERFORM):
%Vascular Dementia - Initial
cannot(vascular_dementia_initial, memory_exercises).
cannot(vascular_dementia_initial, temporal_orientation_exercise).

%Vascular Dementia - Advanced
cannot(vascular_dementia_advanced, memory_exercises).
cannot(vascular_dementia_advanced, felt_work).
cannot(vascular_dementia_advanced, small_arrangements).
cannot(vascular_dementia_advanced, dessert_making).
cannot(vascular_dementia_advanced, execution_festival_work).
cannot(vascular_dementia_advanced, paintings).
cannot(vascular_dementia_advanced, temporal_orientation_exercise).
cannot(vascular_dementia_advanced, spatial_recognition_exercises).
cannot(vascular_dementia_advanced, manual_dexterity_games).
cannot(vascular_dementia_advanced, reading_writing_exercises).
cannot(vascular_dementia_advanced, differences_games).
cannot(vascular_dementia_advanced, petanque_games).
cannot(vascular_dementia_advanced, bowling_games).

%Alzheimer - Initial
cannot(alzheimer_initial, dramatic_text_reading).
cannot(alzheimer_initial, bingo_games).
cannot(alzheimer_initial, domino_games).
cannot(alzheimer_initial, differences_games).

%Alzheimer - Advanced
cannot(alzheimer_advanced, construction_technique).
cannot(alzheimer_advanced, recycled_materials).
cannot(alzheimer_advanced, execution_festival_work).
cannot(alzheimer_advanced, felt_work).
cannot(alzheimer_advanced, small_arrangements).
cannot(alzheimer_advanced, paintings).
cannot(alzheimer_advanced, verbal_task_exercises).
cannot(alzheimer_advanced, reading_writing_exercises).
cannot(alzheimer_advanced, differences_games).
cannot(alzheimer_advanced, odor_differentiation).
cannot(alzheimer_advanced, instruments_use).
cannot(alzheimer_advanced, guess_the_song).
cannot(alzheimer_advanced, bingo_games).
cannot(alzheimer_advanced, missako_games).
cannot(alzheimer_advanced, domino_games).
cannot(alzheimer_advanced, tic_tac_toe_games).
cannot(alzheimer_advanced, dramatic_text_reading).
cannot(parkinson_advanced, memory_exercises).

%Parkinson - Initial
cannot(parkinson_initial, small_arrangements).
cannot(parkinson_initial, manual_dexterity_games).
cannot(parkinson_initial, guess_the_song).
cannot(parkinson_initial, missako_games).

%Parkinson - Advanced
cannot(parkinson_advanced, construction_technique).
cannot(parkinson_advanced, execution_festival_work).
cannot(parkinson_advanced, felt_work).
cannot(parkinson_advanced, small_arrangements).
cannot(parkinson_advanced, paintings).
cannot(parkinson_advanced, memory_exercises).
cannot(parkinson_advanced, manual_dexterity_games).
cannot(parkinson_advanced, reading_writing_exercises).
cannot(parkinson_advanced, walking).
cannot(parkinson_advanced, instruments_use).
cannot(parkinson_advanced, guess_the_song).
cannot(parkinson_advanced, petanque_games).
cannot(parkinson_advanced, bowling_games).
cannot(parkinson_advanced, mime_games).
cannot(parkinson_advanced, missako_games).

%------------------------------------------------------------------------

% CONDITIONS (OTHER ASPECTS)
%1 Social Integration
condition(conditions_social_integration, good_social_relations) :-
    evidence(conditions_social_integration, good_social_relations).
condition(conditions_social_integration, severe_integration_issues) :-
    evidence(conditions_social_integration, severe_integration_issues).
condition(conditions_social_integration, isolated_person) :-
    evidence(conditions_social_integration, isolated_person).

%2 Vision
condition(conditions_vision, good_vision) :-
    evidence(conditions_vision, good_vision).
condition(conditions_vision, vision_with_difficulties) :-
    evidence(conditions_vision, vision_with_difficulties).
condition(conditions_vision, blindness) :-
    evidence(conditions_vision, blindness).

%3 Hearing
condition(conditions_hearing, good_hearing) :-
    evidence(conditions_hearing, good_hearing).
condition(conditions_hearing, hearing_with_difficulties) :-
    evidence(conditions_hearing, hearing_with_difficulties).
condition(conditions_hearing, deafness) :-
    evidence(conditions_hearing, deafness).
    
%4 Speech
condition(conditions_speech, speaks_normally) :-
    evidence(conditions_speech, speaks_normally).
condition(conditions_speech, speaks_with_difficulty) :-
    evidence(conditions_speech, speaks_with_difficulty).
condition(conditions_speech, cannot_be_understood) :-
    evidence(conditions_speech, cannot_be_understood).

%5 Smell
condition(conditions_smell, smell_normally) :-
    evidence(conditions_smell, smell_normally).
condition(conditions_smell, smell_with_difficulty) :-
    evidence(conditions_smell, smell_with_difficulty).
condition(conditions_smell, no_sense_of_smell) :-
    evidence(conditions_smell, no_sense_of_smell).

%6 Upper Motor Skills
condition(conditions_upper_motor_skills, ums_functions_normally) :-
    evidence(conditions_upper_motor_skills, ums_functions_normally).
condition(conditions_upper_motor_skills, ums_has_difficulty) :-
    evidence(conditions_upper_motor_skills, ums_has_difficulty).
condition(conditions_upper_motor_skills, unable_to_use_upper_limbs) :-
    evidence(conditions_upper_motor_skills, unable_to_use_upper_limbs).

%7 Lower Motor Skills
condition(conditions_lower_motor_skills, lms_functions_normally) :-
    evidence(conditions_lower_motor_skills, lms_functions_normally).
condition(conditions_lower_motor_skills, lms_has_difficulty) :-
    evidence(conditions_lower_motor_skills, lms_has_difficulty).
condition(conditions_lower_motor_skills, unable_to_use_lower_limbs) :-
    evidence(conditions_lower_motor_skills, cannot_move).

%8 Object Handling
condition(conditions_object_handling, full_control) :-
    evidence(conditions_object_handling, full_control).
condition(conditions_object_handling, partial_control) :-
    evidence(conditions_object_handling, partial_control).
condition(conditions_object_handling, cannot_handle) :-
    evidence(conditions_object_handling, cannot_handle).

%9 Reading
condition(conditions_reading, reading_normally) :-
    evidence(conditions_reading, reading_normally).
condition(conditions_reading, reading_with_difficulty) :-
    evidence(conditions_reading, reading_with_difficulty).
condition(conditions_reading, cannot_read) :-
    evidence(conditions_reading, cannot_read).

%10 Writing
condition(conditions_writing, writes_normally) :-
    evidence(conditions_writing, writes_normally).
condition(conditions_writing, writes_with_difficulty) :-
    evidence(conditions_writing, writes_with_difficulty).
condition(conditions_writing, cannot_write) :-
    evidence(conditions_writing, cannot_write).

%11 Mobility
condition(conditions_mobility, moves_easily) :-
    evidence(conditions_mobility, moves_easily).
condition(conditions_mobility, needs_assistance) :-
    evidence(conditions_mobility, needs_assistance).
condition(conditions_mobility, total_dependence) :-
    evidence(conditions_mobility, total_dependence).


get_conditions(Conditions):-
    findall(X, condition(X, _), ConditionsList),
    sort(ConditionsList, Conditions).

%------------------------------------------------------------------------

% GAMES BASED ON CONDITIONS:
%1 Social Integration
% Severe Integration Issues (Severity: Medium)
inadequate(severe_integration_issues, execution_festival_work).
inadequate(severe_integration_issues, centre_decoration_work).
inadequate(severe_integration_issues, image_recognition_games).
inadequate(severe_integration_issues, karaoke).
inadequate(severe_integration_issues, adapted_handkerchief_games).
inadequate(severe_integration_issues, mime_games).
inadequate(severe_integration_issues, bingo_games).
inadequate(severe_integration_issues, tic_tac_toe_games).

% Isolated Person (Severity: High)
inadequate(isolated_person, execution_festival_work).
inadequate(isolated_person, centre_decoration_work).
inadequate(isolated_person, dessert_making).
inadequate(isolated_person, image_recognition_games).
inadequate(isolated_person, karaoke).
inadequate(isolated_person, petanque_games).
inadequate(isolated_person, bowling_games).
inadequate(isolated_person, adapted_handkerchief_games).
inadequate(isolated_person, mime_games).
inadequate(isolated_person, bingo_games).
inadequate(isolated_person, missako_games).
inadequate(isolated_person, domino_games).
inadequate(isolated_person, tic_tac_toe_games).
inadequate(isolated_person, dramatic_text_reading).
inadequate(isolated_person, massages).
inadequate(isolated_person, makeup).
inadequate(isolated_person, dramatic_text_reading).
inadequate(isolated_person, character_interpretation).

%2 Vision
% Vision Difficulties (Severity: Medium)
inadequate(vision_with_difficulties, construction_technique).
inadequate(vision_with_difficulties, felt_work).
inadequate(vision_with_difficulties, small_arrangements).
inadequate(vision_with_difficulties, centre_decoration_work).
inadequate(vision_with_difficulties, dessert_making).
inadequate(vision_with_difficulties, image_recognition_games).
inadequate(vision_with_difficulties, reading_writing_exercises).
inadequate(vision_with_difficulties, differences_games).
inadequate(vision_with_difficulties, musicogram).
inadequate(vision_with_difficulties, petanque_games).
inadequate(vision_with_difficulties, bowling_games).
inadequate(vision_with_difficulties, bingo_games).
inadequate(vision_with_difficulties, dramatic_text_reading).

% Blindness (Severity: High)
inadequate(blindness, construction_technique).
inadequate(blindness, recycled_materials).
inadequate(blindness, execution_festival_work).
inadequate(blindness, felt_work).
inadequate(blindness, small_arrangements).
inadequate(blindness, paintings).
inadequate(blindness, centre_decoration_work).
inadequate(blindness, dessert_making).
inadequate(blindness, image_recognition_games).
inadequate(blindness, reading_writing_exercises).
inadequate(blindness, differences_games).
inadequate(blindness, walking).
inadequate(blindness, instruments_use).
inadequate(blindness, musicogram).
inadequate(blindness, petanque_games).
inadequate(blindness, bowling_games).
inadequate(blindness, adapted_handkerchief_games).
inadequate(blindness, mime_games).
inadequate(blindness, bingo_games).
inadequate(blindness, dramatic_text_reading).
inadequate(blindness, character_interpretation).

%3 Hearing
% Hearing Difficulties (Moderate Severity)
inadequate(hearing_with_difficulties, karaoke).
inadequate(hearing_with_difficulties, musicogram).
inadequate(hearing_with_difficulties, instruments_use).
inadequate(hearing_with_difficulties, guess_the_song).
inadequate(hearing_with_difficulties, animal_voices_recognition).
inadequate(hearing_with_difficulties, bingo_games).
inadequate(hearing_with_difficulties, dramatic_text_reading).

% Deafness (High Severity)
inadequate(deafness, dessert_making).
inadequate(deafness, karaoke).
inadequate(deafness, musicogram).
inadequate(deafness, instruments_use).
inadequate(deafness, guess_the_song).
inadequate(deafness, animal_voices_recognition).
inadequate(deafness, verbal_task_exercises).
inadequate(deafness, reading_writing_exercises).
inadequate(deafness, missako_games).
inadequate(deafness, bingo_games).
inadequate(deafness, mime_games).
inadequate(deafness, dramatic_text_reading).
inadequate(deafness, character_interpretation).

%4 Speech
% Speech Difficulty (Moderate Severity)
inadequate(speaks_with_difficulty, karaoke).
inadequate(speaks_with_difficulty, character_interpretation).
inadequate(speaks_with_difficulty, verbal_task_exercises).
inadequate(speaks_with_difficulty, dramatic_text_reading).
inadequate(speaks_with_difficulty, guess_the_song).
inadequate(speaks_with_difficulty, musicogram).

% Cannot Be Understood (High Severity)
inadequate(cannot_be_understood, verbal_task_exercises).
inadequate(cannot_be_understood, karaoke).
inadequate(cannot_be_understood, character_interpretation).
inadequate(cannot_be_understood, verbal_task_exercises).
inadequate(cannot_be_understood, dramatic_text_reading).
inadequate(cannot_be_understood, guess_the_song).
inadequate(cannot_be_understood, musicogram).

%5 Smell
% Smell Difficulty (Moderate Severity)
inadequate(smell_with_difficulty, odor_differentiation).
inadequate(smell_with_difficulty, taste_reaction).

% No sense of Smell (High Severity)
inadequate(no_sense_of_smell, odor_differentiation).
inadequate(no_sense_of_smell, taste_reaction).

%6 Upper Mobility
% Upper Mobility Difficulty (Moderate Severity)
inadequate(ums_has_difficulty, construction_technique).
inadequate(ums_has_difficulty, recycled_materials).
inadequate(ums_has_difficulty, felt_work).
inadequate(ums_has_difficulty, small_arrangements).
inadequate(ums_has_difficulty, paintings).
inadequate(ums_has_difficulty, centre_decoration_work).
inadequate(ums_has_difficulty, manual_dexterity_games).
inadequate(ums_has_difficulty, manicure).
inadequate(ums_has_difficulty, makeup).
inadequate(ums_has_difficulty, instruments_use).
inadequate(ums_has_difficulty, petanque_games).
inadequate(ums_has_difficulty, bowling_games).
inadequate(ums_has_difficulty, tic_tac_toe_games).
inadequate(ums_has_difficulty, domino_games).

% Unable to Use Upper Limbs (High Severity)
inadequate(unable_to_use_upper_limbs, construction_technique).
inadequate(unable_to_use_upper_limbs, recycled_materials).
inadequate(unable_to_use_upper_limbs, execution_festival_work).
inadequate(unable_to_use_upper_limbs, felt_work).
inadequate(unable_to_use_upper_limbs, small_arrangements).
inadequate(unable_to_use_upper_limbs, paintings).
inadequate(unable_to_use_upper_limbs, centre_decoration_work).
inadequate(unable_to_use_upper_limbs, dessert_making).
inadequate(unable_to_use_upper_limbs, differences_games).
inadequate(unable_to_use_upper_limbs, manual_dexterity_games).
inadequate(unable_to_use_upper_limbs, physiotherapy).
inadequate(unable_to_use_upper_limbs, manicure).
inadequate(unable_to_use_upper_limbs, makeup).
inadequate(unable_to_use_upper_limbs, musicogram).
inadequate(unable_to_use_upper_limbs, instruments_use).
inadequate(unable_to_use_upper_limbs, warmup_exercises).
inadequate(unable_to_use_upper_limbs, petanque_games).
inadequate(unable_to_use_upper_limbs, bowling_games).
inadequate(unable_to_use_upper_limbs, adapted_handkerchief_games).
inadequate(unable_to_use_upper_limbs, mime_games).
inadequate(unable_to_use_upper_limbs, bingo_games).
inadequate(unable_to_use_upper_limbs, missako_games).
inadequate(unable_to_use_upper_limbs, tic_tac_toe_games).
inadequate(unable_to_use_upper_limbs, domino_games).
inadequate(unable_to_use_upper_limbs, character_interpretation).

%7 Upper Mobility
% Lower Mobility Difficulty (Moderate Severity)
inadequate(lms_has_difficulty, petanque_games).
inadequate(lms_has_difficulty, bowling_games).
inadequate(lms_has_difficulty, physiotherapy).

% Unable to Use Lower Limbs (High Severity)
inadequate(unable_to_use_lower_limbs, walking).
inadequate(unable_to_use_lower_limbs, warmup_exercises).
inadequate(unable_to_use_lower_limbs, physiotherapy).
inadequate(unable_to_use_lower_limbs, petanque_games).
inadequate(unable_to_use_lower_limbs, bowling_games).
inadequate(unable_to_use_lower_limbs, adapted_handkerchief_games).
inadequate(unable_to_use_lower_limbs, mime_games).

%8 Object Handling
% Partial Control (Moderate Severity)
inadequate(partial_control, felt_work).
inadequate(partial_control, small_arrangements).
inadequate(partial_control, dessert_making).
inadequate(partial_control, manual_dexterity_games).
inadequate(partial_control, manicure).
inadequate(partial_control, makeup).
inadequate(partial_control, instruments_use).
inadequate(partial_control, musicogram).

% Cannot Handle Objects (High Severity)
inadequate(cannot_handle, construction_technique).
inadequate(cannot_handle, recycled_materials).
inadequate(cannot_handle, execution_festival_work).
inadequate(cannot_handle, felt_work).
inadequate(cannot_handle, small_arrangements).
inadequate(cannot_handle, paintings).
inadequate(cannot_handle, centre_decoration_work).
inadequate(cannot_handle, dessert_making).
inadequate(cannot_handle, knowledge_exercises).
inadequate(cannot_handle, manual_dexterity_games).
inadequate(cannot_handle, manicure).
inadequate(cannot_handle, makeup).
inadequate(cannot_handle, instruments_use).
inadequate(cannot_handle, musicogram).
inadequate(cannot_handle, domino_games).
inadequate(cannot_handle, missako_games).
inadequate(cannot_handle, tic_tac_toe_games).
inadequate(cannot_handle, petanque_games).
inadequate(cannot_handle, bowling_games).

%9 Reading
% Reading Difficulty (Moderate Severity)
inadequate(reading_with_difficulty, reading_writing_exercises).
inadequate(reading_with_difficulty, dramatic_text_reading).
inadequate(reading_with_difficulty, character_interpretation).
inadequate(reading_with_difficulty, musicogram).
inadequate(reading_with_difficulty, numerical_comprehension_exercises).

% Cannot Read (High Severity) 
inadequate(cannot_read, reading_writing_exercises).
inadequate(cannot_read, numerical_comprehension_exercises).
inadequate(cannot_read, musicogram).
inadequate(cannot_read, dramatic_text_reading).
inadequate(cannot_read, character_interpretation).
inadequate(cannot_read, knowledge_exercises).
inadequate(cannot_read, bingo_games).

%10 Writing Difficulty
% Writing (Moderate Severity)
inadequate(writes_with_difficulty, reading_writing_exercises).
inadequate(writes_with_difficulty, verbal_task_exercises).
inadequate(writes_with_difficulty, character_interpretation).
inadequate(writes_with_difficulty, musicogram).

% Cannot Write (High Severity)
inadequate(cannot_write, reading_writing_exercises).
inadequate(cannot_write, verbal_task_exercises).
inadequate(cannot_write, character_interpretation).
inadequate(cannot_write, musicogram).
inadequate(cannot_write, bingo_games).
inadequate(cannot_write, knowledge_exercises).
inadequate(cannot_write, numerical_comprehension_exercises).

%11 Mobilidade
% Mobility (Moderate Severity)
inadequate(needs_assistance, petanque_games).
inadequate(needs_assistance, bowling_games).
inadequate(needs_assistance, warmup_exercises).
inadequate(needs_assistance, physiotherapy).

% Total Dependency (High Severity)
inadequate(total_dependence, walking).
inadequate(total_dependence, warmup_exercises).
inadequate(total_dependence, physiotherapy).
inadequate(total_dependence, petanque_games).
inadequate(total_dependence, bowling_games).
inadequate(total_dependence, adapted_handkerchief_games).
inadequate(total_dependence, mime_games).

%------------------------------------------------------------------------

% PREFERENCES:
likes(preferences_theatre):-
    evidence(preferences_theatre, yes).
likes(preferences_museum):-
    evidence(preferences_museum, yes).
likes(preferences_music):-
    evidence(preferences_music, yes).
likes(preferences_reading):-
    evidence(preferences_reading, yes).
likes(preferences_recreational_group):-
    evidence(preferences_recreational_group, yes).
likes(preferences_art):-
    evidence(preferences_art, yes).
likes(preferences_sports):-
    evidence(preferences_sports, yes).
likes(preferences_cooking):-
    evidence(preferences_cooking, yes).
likes(preferences_cooking):-
    evidence(preferences_cooking, yes).
likes(preferences_handicrafts):-
    evidence(preferences_handicrafts, yes).


get_preferences(Preferences) :-
    findall(X, likes(X), PreferencesList),
    sort(PreferencesList, Preferences).

%------------------------------------------------------------------------

% LIKES/ACTIVITY (PREFERENCES)
%1-Theatre
preference(preferences_theatre, dramatic_text_reading).
preference(preferences_theatre, character_interpretation).
preference(preferences_theatre, mime_games).
%2-Museum
preference(preferences_museum, paintings).
preference(preferences_museum, small_arrangements).
preference(preferences_museum, walking).
%3-Music
preference(preferences_music, karaoke).
preference(preferences_music, musicogram).
preference(preferences_music, guess_the_song).
preference(preferences_music, instruments_use).
%4-Reading
preference(preferences_reading, dramatic_text_reading).
preference(preferences_reading, reading_writing_exercises).
preference(preferences_reading, verbal_task_exercises).
%5-Recreational Group
preference(preferences_recreational_group, petanque_games).
preference(preferences_recreational_group, bowling_games).
preference(preferences_recreational_group, mime_games).
preference(preferences_recreational_group, bingo_games).
preference(preferences_recreational_group, adapted_handkerchief_games).
preference(preferences_recreational_group, execution_festival_work).
preference(preferences_recreational_group, centre_decoration_work).
%6-Art
preference(preferences_art, paintings).
preference(preferences_art, centre_decoration_work).
preference(preferences_art, felt_work).
preference(preferences_art, small_arrangements).
%7-Sports
preference(preferences_sports, walking).
preference(preferences_sports, petanque_games).
preference(preferences_sports, bowling_games).
%8-Cooking
preference(preferences_cooking, dessert_making).
%9-Handicrafts
preference(preferences_handicrafts, construction_technique).
preference(preferences_handicrafts, recycled_materials).
preference(preferences_handicrafts, felt_work).
preference(preferences_handicrafts, small_arrangements).
preference(preferences_handicrafts, paintings).
preference(preferences_handicrafts, centre_decoration_work).
preference(preferences_handicrafts, manicure).
preference(preferences_handicrafts, makeup).
preference(preferences_handicrafts, massages).
