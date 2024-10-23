:- dynamic evidence/2.

% FACTS
%--------------------------------------------

% DISEASE DEFINITIONS:
disease(alzheimer_initial) :-
    evidence(diagnosis, yes),
    evidence(diagnosis_alzheimer, yes),
    evidence(diagnosis_alzheimer_stage, initial).
disease(alzheimer_initial) :-
    findall(yes, (
        evidence(observation_alzheimer_spatial_disorientation, yes);
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
        evidence(observation_alzheimer_needs_permanent_watch, yes);
        evidence(observation_alzheimer_cant_execute_stimuli, yes);
        evidence(observation_alzheimer_fall_history, yes)
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
        evidence(observation_parkinson_bent_spine, yes);
        evidence(observation_parkinson_balance_loss, yes);
        evidence(observation_parkinson_initial_hearing_loss, yes)
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
        evidence(observation_parkinson_locomotion_difficulties, yes);
        evidence(observation_parkinson_intense_shaking, yes);
        evidence(observation_parkinson_fine_motor_control, yes)
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
        evidence(observation_vascular_dementia_heavy_memory_loss, yes);
        evidence(observation_vascular_dementia_people_recognition, yes);
        evidence(observation_vascular_dementia_history_aggressiveness_insomnia_agitation, yes);
        evidence(observation_vascular_dementia_body_control, yes)
    ), Results),
    length(Results, Count),
    Count >= 1.


get_diseases(Diseases) :-
    findall(X, disease(X), DiseasesList),
    sort(DiseasesList, Diseases).
%--------------------------------------------


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


%--------------------------------------------


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


%--------------------------------------------


% CONDITIONS (OTHER ASPECTS)
%1 Social Integration
condition(social_integration, good_social_relations) :-
    evidence(social_integration, good_social_relations).
condition(social_integration, severe_integration_issues) :-
    evidence(social_integration, severe_integration_issues).
condition(social_integration, isolated_person) :-
    evidence(social_integration, isolated_person).
%2 Vision
condition(vision, good_vision) :-
    evidence(vision, good_vision).
condition(vision, vision_with_difficulties) :-
    evidence(vision, vision_with_difficulties).
condition(vision, blindness) :-
    evidence(vision, blindness).
%3 Hearing
condition(hearing, good_hearing) :-
    evidence(hearing, good_hearing).
condition(hearing, hearing_with_difficulties) :-
    evidence(hearing, hearing_with_difficulties).
condition(hearing, deafness) :-
    evidence(hearing, deafness).
%4 Speech
condition(speech, speaks_normally) :-
    evidence(speech, speaks_normally).
condition(speech, expresses_with_difficulty) :-
    evidence(speech, expresses_with_difficulty).
condition(speech, cannot_be_understood) :-
    evidence(speech, cannot_be_understood).
%5 Smell
condition(smell, functions_normally) :-
    evidence(smell, functions_normally).
condition(smell, difficulties_identifying_odors) :-
    evidence(smell, difficulties_identifying_odors).
condition(smell, complete_loss_of_smell) :-
    evidence(smell, complete_loss_of_smell).
%6 Upper Motor Skills
condition(upper_motor_skills, functions_normally) :-
    evidence(upper_motor_skills, functions_normally).
condition(upper_motor_skills, has_difficulty) :-
    evidence(upper_motor_skills, has_difficulty).
condition(upper_motor_skills, cannot_move) :-
    evidence(upper_motor_skills, cannot_move).
%7 Lower Motor Skills
condition(lower_motor_skills, functions_normally) :-
    evidence(lower_motor_skills, functions_normally).
condition(lower_motor_skills, has_difficulty) :-
    evidence(lower_motor_skills, has_difficulty).
condition(lower_motor_skills, cannot_move) :-
    evidence(lower_motor_skills, cannot_move).
%8 Object Handling
condition(object_handling, correct_manipulation) :-
    evidence(object_handling, correct_manipulation).
condition(object_handling, only_some_correctly) :-
    evidence(object_handling, only_some_correctly).
condition(object_handling, cannot_handle) :-
    evidence(object_handling, cannot_handle).
%9 Reading
condition(reading, reads_without_difficulty) :-
    evidence(reading, reads_without_difficulty).
condition(reading, has_some_difficulty) :-
    evidence(reading, has_some_difficulty).
condition(reading, cannot_read) :-
    evidence(reading, cannot_read).
%10 Writing
condition(writing, writes_without_difficulty) :-
    evidence(writing, writes_without_difficulty).
condition(writing, has_some_difficulty) :-
    evidence(writing, has_some_difficulty).
condition(writing, cannot_write) :-
    evidence(writing, cannot_write).
%11 Mobility
condition(mobility, moves_without_difficulty) :-
    evidence(mobility, moves_without_difficulty).
condition(mobility, has_some_difficulty) :-
    evidence(mobility, has_some_difficulty).
condition(mobility, total_dependence) :-
    evidence(mobility, total_dependence).

get_conditions(Conditions):-
    findall(X, condition(X, _), ConditionsList),
    sort(ConditionsList, Conditions).

%--------------------------------------------


% GAMES BASED ON CONDITIONS

%1 Social Integration
% Severe Integration Problems (Severity: Medium)
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


%--------------------------------------------


% PREFERENCES
likes(theatre):-
    evidence(theatre, yes).
likes(museum):-
    evidence(museum, yes).
likes(music):-
    evidence(music, yes).
likes(reading):-
    evidence(reading, yes).
likes(recreational_group):-
    evidence(recreational_group, yes).
likes(art):-
    evidence(art, yes).
likes(sports):-
    evidence(sports, yes).
likes(cooking):-
    evidence(cooking, yes).
likes(cooking):-
    evidence(cooking, yes).
likes(handicrafts):-
    evidence(handicrafts, yes).

get_preferences(Preferences) :-
    findall(X, likes(X), PreferencesList),
    sort(PreferencesList, Preferences).

%--------------------------------------------

% LIKES/ACTIVITY (PREFERENCES)
%1-Theatre
preference(theatre, dramatic_text_reading).
preference(theatre, character_interpretation).
preference(theatre, mime_games).
%2-Museum
preference(museum, paintings).
preference(museum, small_arrangements).
preference(museum, walking).
%3-Music
preference(music, karaoke).
preference(music, musicogram).
preference(music, guess_the_song).
preference(music, instruments_use).
%4-Reading
preference(reading, dramatic_text_reading).
preference(reading, reading_writing_exercises).
preference(reading, verbal_task_exercises).
%5-Recreational Group
preference(recreational_group, petanque_games).
preference(recreational_group, bowling_games).
preference(recreational_group, mime_games).
preference(recreational_group, bingo_games).
preference(recreational_group, adapted_handkerchief_games).
preference(recreational_group, execution_festival_work).
preference(recreational_group, centre_decoration_work).
%6-Art
preference(art, paintings).
preference(art, centre_decoration_work).
preference(art, felt_work).
preference(art, small_arrangements).
%7-Sports
preference(sports, walking).
preference(sports, petanque_games).
preference(sports, bowling_games).
%8-Cooking
preference(cooking, dessert_making).
%9-Handicrafts
preference(handicrafts, construction_technique).
preference(handicrafts, recycled_materials).
preference(handicrafts, felt_work).
preference(handicrafts, small_arrangements).
preference(handicrafts, paintings).
preference(handicrafts, centre_decoration_work).
preference(handicrafts, manicure).
preference(handicrafts, makeup).
preference(handicrafts, massages).
