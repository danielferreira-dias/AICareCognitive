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
cannot(vascular_dementia_initial, memory_exercises, avi-r1).
cannot(vascular_dementia_initial, temporal_orientation_exercise, avi-r2).

%Vascular Dementia - Advanced
cannot(vascular_dementia_advanced, memory_exercises, ava-r1).
cannot(vascular_dementia_advanced, felt_work, ava-r2).
cannot(vascular_dementia_advanced, small_arrangements, ava-r3).
cannot(vascular_dementia_advanced, dessert_making, ava-r4).
cannot(vascular_dementia_advanced, execution_festival_work, ava-r5).
cannot(vascular_dementia_advanced, paintings, ava-r6).
cannot(vascular_dementia_advanced, temporal_orientation_exercise, ava-r7).
cannot(vascular_dementia_advanced, spatial_recognition_exercises, ava-r8).
cannot(vascular_dementia_advanced, manual_dexterity_games, ava-r9).
cannot(vascular_dementia_advanced, reading_writing_exercises, ava-r10).
cannot(vascular_dementia_advanced, differences_games, ava-r11).
cannot(vascular_dementia_advanced, petanque_games, ava-r12).
cannot(vascular_dementia_advanced, bowling_games, ava-r13).

%Alzheimer - Initial
cannot(alzheimer_initial, dramatic_text_reading, ali-r1).
cannot(alzheimer_initial, bingo_games, ali-r2).
cannot(alzheimer_initial, domino_games, ali-r3).
cannot(alzheimer_initial, differences_games, ali-r4).

%Alzheimer - Advanced
cannot(alzheimer_advanced, construction_technique, ala-r1).
cannot(alzheimer_advanced, recycled_materials, ala-r2).
cannot(alzheimer_advanced, execution_festival_work, ala-r3).
cannot(alzheimer_advanced, felt_work, ala-r4).
cannot(alzheimer_advanced, small_arrangements, ala-r5).
cannot(alzheimer_advanced, paintings, ala-r6).
cannot(alzheimer_advanced, verbal_task_exercises, ala-r7).
cannot(alzheimer_advanced, reading_writing_exercises, ala-r8).
cannot(alzheimer_advanced, differences_games, ala-r9).
cannot(alzheimer_advanced, odor_differentiation, ala-r10).
cannot(alzheimer_advanced, instruments_use, ala-r11).
cannot(alzheimer_advanced, guess_the_song, ala-r12).
cannot(alzheimer_advanced, bingo_games, ala-r13).
cannot(alzheimer_advanced, missako_games, ala-r14).
cannot(alzheimer_advanced, domino_games, ala-r15).
cannot(alzheimer_advanced, tic_tac_toe_games, ala-r16).
cannot(alzheimer_advanced, dramatic_text_reading, ala-r17).
cannot(alzheimer_advanced, memory_exercises, ala-r18).

%Parkinson - Initial
cannot(parkinson_initial, small_arrangements, pki-r1).
cannot(parkinson_initial, manual_dexterity_games, pki-r2).
cannot(parkinson_initial, guess_the_song, pki-r3).
cannot(parkinson_initial, missako_games, pki-r4).

%Parkinson - Advanced
cannot(parkinson_advanced, construction_technique, pka-r1).
cannot(parkinson_advanced, execution_festival_work, pka-r2).
cannot(parkinson_advanced, felt_work, pka-r3).
cannot(parkinson_advanced, small_arrangements, pka-r4).
cannot(parkinson_advanced, paintings, pka-r5).
cannot(parkinson_advanced, memory_exercises, pka-r6).
cannot(parkinson_advanced, manual_dexterity_games, pka-r7).
cannot(parkinson_advanced, reading_writing_exercises, pka-r8).
cannot(parkinson_advanced, walking, pka-r9).
cannot(parkinson_advanced, instruments_use, pka-r10).
cannot(parkinson_advanced, guess_the_song, pka-r11).
cannot(parkinson_advanced, petanque_games, pka-r12).
cannot(parkinson_advanced, bowling_games, pka-r13).
cannot(parkinson_advanced, mime_games, pka-r14).
cannot(parkinson_advanced, missako_games, pka-r15).

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
    findall(X, condition(_, X), ConditionsList),
    sort(ConditionsList, Conditions).

%------------------------------------------------------------------------

% GAMES BASED ON CONDITIONS:
% 1 Social Integration
% Severe Integration Issues (Severity: Medium)
inadequate(severe_integration_issues, execution_festival_work, c-r1).
inadequate(severe_integration_issues, centre_decoration_work, c-r2).
inadequate(severe_integration_issues, image_recognition_games, c-r3).
inadequate(severe_integration_issues, karaoke, c-r4).
inadequate(severe_integration_issues, adapted_handkerchief_games, c-r5).
inadequate(severe_integration_issues, mime_games, c-r6).
inadequate(severe_integration_issues, bingo_games, c-r7).
inadequate(severe_integration_issues, tic_tac_toe_games, c-r8).

% Isolated Person (Severity: High)
inadequate(isolated_person, execution_festival_work, c-r8).
inadequate(isolated_person, centre_decoration_work, c-r9).
inadequate(isolated_person, dessert_making, c-r10).
inadequate(isolated_person, image_recognition_games, c-r11).
inadequate(isolated_person, karaoke, c-r12).
inadequate(isolated_person, petanque_games, c-r13).
inadequate(isolated_person, bowling_games, c-r14).
inadequate(isolated_person, adapted_handkerchief_games, c-r15).
inadequate(isolated_person, mime_games, c-r16).
inadequate(isolated_person, bingo_games, c-r17).
inadequate(isolated_person, missako_games, c-r18).
inadequate(isolated_person, domino_games, c-r19).
inadequate(isolated_person, tic_tac_toe_games, c-r20).
inadequate(isolated_person, dramatic_text_reading, c-r21).
inadequate(isolated_person, massages, c-r22).
inadequate(isolated_person, makeup, c-r23).
inadequate(isolated_person, character_interpretation, c-r24).

% 2 Vision
% Vision Difficulties (Severity: Medium)
inadequate(vision_with_difficulties, construction_technique, c-r26).
inadequate(vision_with_difficulties, felt_work, c-r27).
inadequate(vision_with_difficulties, small_arrangements, c-r28).
inadequate(vision_with_difficulties, centre_decoration_work, c-r29).
inadequate(vision_with_difficulties, dessert_making, c-r30).
inadequate(vision_with_difficulties, image_recognition_games, c-r31).
inadequate(vision_with_difficulties, reading_writing_exercises, c-r32).
inadequate(vision_with_difficulties, differences_games, c-r33).
inadequate(vision_with_difficulties, musicogram, c-r34).
inadequate(vision_with_difficulties, petanque_games, c-r35).
inadequate(vision_with_difficulties, bowling_games, c-r36).
inadequate(vision_with_difficulties, bingo_games, c-r37).
inadequate(vision_with_difficulties, dramatic_text_reading, c-r38).

% Blindness (Severity: High)
inadequate(blindness, construction_technique, c-r39).
inadequate(blindness, recycled_materials, c-r40).
inadequate(blindness, execution_festival_work, c-r41).
inadequate(blindness, felt_work, c-r42).
inadequate(blindness, small_arrangements, c-r43).
inadequate(blindness, paintings, c-r44).
inadequate(blindness, centre_decoration_work, c-r45).
inadequate(blindness, dessert_making, c-r46).
inadequate(blindness, image_recognition_games, c-r47).
inadequate(blindness, reading_writing_exercises, c-r48).
inadequate(blindness, differences_games, c-r49).
inadequate(blindness, walking, c-r50).
inadequate(blindness, instruments_use, c-r51).
inadequate(blindness, musicogram, c-r52).
inadequate(blindness, petanque_games, c-r53).
inadequate(blindness, bowling_games, c-r54).
inadequate(blindness, adapted_handkerchief_games, c-r55).
inadequate(blindness, mime_games, c-r56).
inadequate(blindness, bingo_games, c-r57).
inadequate(blindness, dramatic_text_reading, c-r58).
inadequate(blindness, character_interpretation, c-r59).

% 3 Hearing
% Hearing Difficulties (Moderate Severity)
inadequate(hearing_with_difficulties, karaoke, c-r60).
inadequate(hearing_with_difficulties, musicogram, c-r61).
inadequate(hearing_with_difficulties, instruments_use, c-r62).
inadequate(hearing_with_difficulties, guess_the_song, c-r63).
inadequate(hearing_with_difficulties, animal_voices_recognition, c-r64).
inadequate(hearing_with_difficulties, bingo_games, c-r65).
inadequate(hearing_with_difficulties, dramatic_text_reading, c-r66).

% Deafness (High Severity)
inadequate(deafness, dessert_making, c-r67).
inadequate(deafness, karaoke, c-r68).
inadequate(deafness, musicogram, c-r69).
inadequate(deafness, instruments_use, c-r70).
inadequate(deafness, guess_the_song, c-r71).
inadequate(deafness, animal_voices_recognition, c-r72).
inadequate(deafness, verbal_task_exercises, c-r73).
inadequate(deafness, reading_writing_exercises, c-r74).
inadequate(deafness, missako_games, c-r75).
inadequate(deafness, bingo_games, c-r76).
inadequate(deafness, mime_games, c-r77).
inadequate(deafness, dramatic_text_reading, c-r78).
inadequate(deafness, character_interpretation, c-r79).

% 4 Speech
% Speech Difficulty (Moderate Severity)
inadequate(speaks_with_difficulty, karaoke, c-r80).
inadequate(speaks_with_difficulty, character_interpretation, c-r81).
inadequate(speaks_with_difficulty, verbal_task_exercises, c-r82).
inadequate(speaks_with_difficulty, dramatic_text_reading, c-r83).
inadequate(speaks_with_difficulty, guess_the_song, c-r84).
inadequate(speaks_with_difficulty, musicogram, c-r85).

% Cannot Be Understood (High Severity)
inadequate(cannot_be_understood, verbal_task_exercises, c-r86).
inadequate(cannot_be_understood, karaoke, c-r87).
inadequate(cannot_be_understood, character_interpretation, c-r88).
inadequate(cannot_be_understood, dramatic_text_reading, c-r89).
inadequate(cannot_be_understood, guess_the_song, c-r90).
inadequate(cannot_be_understood, musicogram, c-r91).

% 5 Smell
% Smell Difficulty (Moderate Severity)
inadequate(smell_with_difficulty, odor_differentiation, c-r92).
inadequate(smell_with_difficulty, taste_reaction, c-r93).

% No sense of Smell (High Severity)
inadequate(no_sense_of_smell, odor_differentiation, c-r94).
inadequate(no_sense_of_smell, taste_reaction, c-r95).

% 6 Upper Mobility
% Upper Mobility Difficulty (Moderate Severity)
inadequate(ums_has_difficulty, construction_technique, c-r96).
inadequate(ums_has_difficulty, recycled_materials, c-r97).
inadequate(ums_has_difficulty, felt_work, c-r98).
inadequate(ums_has_difficulty, small_arrangements, c-r99).
inadequate(ums_has_difficulty, paintings, c-r100).
inadequate(ums_has_difficulty, centre_decoration_work, c-r101).
inadequate(ums_has_difficulty, manual_dexterity_games, c-r102).
inadequate(ums_has_difficulty, manicure, c-r103).
inadequate(ums_has_difficulty, makeup, c-r104).
inadequate(ums_has_difficulty, instruments_use, c-r105).
inadequate(ums_has_difficulty, petanque_games, c-r106).
inadequate(ums_has_difficulty, bowling_games, c-r107).
inadequate(ums_has_difficulty, tic_tac_toe_games, c-r108).
inadequate(ums_has_difficulty, domino_games, c-r109).

% Unable to Use Upper Limbs (High Severity)
inadequate(unable_to_use_upper_limbs, construction_technique, c-r110).
inadequate(unable_to_use_upper_limbs, recycled_materials, c-r111).
inadequate(unable_to_use_upper_limbs, execution_festival_work, c-r112).
inadequate(unable_to_use_upper_limbs, felt_work, c-r113).
inadequate(unable_to_use_upper_limbs, small_arrangements, c-r114).
inadequate(unable_to_use_upper_limbs, paintings, c-r115).
inadequate(unable_to_use_upper_limbs, centre_decoration_work, c-r116).
inadequate(unable_to_use_upper_limbs, dessert_making, c-r117).
inadequate(unable_to_use_upper_limbs, differences_games, c-r118).
inadequate(unable_to_use_upper_limbs, manual_dexterity_games, c-r119).
inadequate(unable_to_use_upper_limbs, physiotherapy, c-r120).
inadequate(unable_to_use_upper_limbs, manicure, c-r121).
inadequate(unable_to_use_upper_limbs, makeup, c-r122).
inadequate(unable_to_use_upper_limbs, musicogram, c-r123).
inadequate(unable_to_use_upper_limbs, instruments_use, c-r124).
inadequate(unable_to_use_upper_limbs, warmup_exercises, c-r125).
inadequate(unable_to_use_upper_limbs, petanque_games, c-r126).
inadequate(unable_to_use_upper_limbs, bowling_games, c-r127).
inadequate(unable_to_use_upper_limbs, adapted_handkerchief_games, c-r128).
inadequate(unable_to_use_upper_limbs, mime_games, c-r129).
inadequate(unable_to_use_upper_limbs, bingo_games, c-r130).
inadequate(unable_to_use_upper_limbs, missako_games, c-r131).
inadequate(unable_to_use_upper_limbs, tic_tac_toe_games, c-r132).
inadequate(unable_to_use_upper_limbs, domino_games, c-r133).
inadequate(unable_to_use_upper_limbs, character_interpretation, c-r134).

%7 Lower Mobility
% Lower Mobility Difficulty (Moderate Severity)
inadequate(lms_has_difficulty, petanque_games, c-r135).
inadequate(lms_has_difficulty, bowling_games, c-r136).
inadequate(lms_has_difficulty, physiotherapy, c-r137).

% Unable to Use Lower Limbs (High Severity)
inadequate(unable_to_use_lower_limbs, walking, c-r138).
inadequate(unable_to_use_lower_limbs, warmup_exercises, c-r139).
inadequate(unable_to_use_lower_limbs, physiotherapy, c-r140).
inadequate(unable_to_use_lower_limbs, petanque_games, c-r141).
inadequate(unable_to_use_lower_limbs, bowling_games, c-r142).
inadequate(unable_to_use_lower_limbs, adapted_handkerchief_games, c-r143).
inadequate(unable_to_use_lower_limbs, mime_games, c-r144).

%8 Object Handling
% Partial Control (Moderate Severity)
inadequate(partial_control, felt_work, c-r145).
inadequate(partial_control, small_arrangements, c-r146).
inadequate(partial_control, dessert_making, c-r147).
inadequate(partial_control, manual_dexterity_games, c-r148).
inadequate(partial_control, manicure, c-r149).
inadequate(partial_control, makeup, c-r150).
inadequate(partial_control, instruments_use, c-r151).
inadequate(partial_control, musicogram, c-r152).

% Cannot Handle Objects (High Severity)
inadequate(cannot_handle, construction_technique, c-r153).
inadequate(cannot_handle, recycled_materials, c-r154).
inadequate(cannot_handle, execution_festival_work, c-r155).
inadequate(cannot_handle, felt_work, c-r156).
inadequate(cannot_handle, small_arrangements, c-r157).
inadequate(cannot_handle, paintings, c-r158).
inadequate(cannot_handle, centre_decoration_work, c-r159).
inadequate(cannot_handle, dessert_making, c-r160).
inadequate(cannot_handle, knowledge_exercises, c-r161).
inadequate(cannot_handle, manual_dexterity_games, c-r162).
inadequate(cannot_handle, manicure, c-r163).
inadequate(cannot_handle, makeup, c-r164).
inadequate(cannot_handle, instruments_use, c-r165).
inadequate(cannot_handle, musicogram, c-r166).
inadequate(cannot_handle, domino_games, c-r167).
inadequate(cannot_handle, missako_games, c-r168).
inadequate(cannot_handle, tic_tac_toe_games, c-r169).
inadequate(cannot_handle, petanque_games, c-r170).
inadequate(cannot_handle, bowling_games, c-r171).

%9 Reading
% Reading Difficulty (Moderate Severity)
inadequate(reading_with_difficulty, reading_writing_exercises, c-r172).
inadequate(reading_with_difficulty, dramatic_text_reading, c-r173).
inadequate(reading_with_difficulty, character_interpretation, c-r174).
inadequate(reading_with_difficulty, musicogram, c-r175).
inadequate(reading_with_difficulty, numerical_comprehension_exercises, c-r176).

% Cannot Read (High Severity) 
inadequate(cannot_read, reading_writing_exercises, c-r177).
inadequate(cannot_read, numerical_comprehension_exercises, c-r178).
inadequate(cannot_read, musicogram, c-r179).
inadequate(cannot_read, dramatic_text_reading, c-r180).
inadequate(cannot_read, character_interpretation, c-r181).
inadequate(cannot_read, knowledge_exercises, c-r182).
inadequate(cannot_read, bingo_games, c-r183).

%10 Writing Difficulty
% Writing (Moderate Severity)
inadequate(writes_with_difficulty, reading_writing_exercises, c-r184).
inadequate(writes_with_difficulty, verbal_task_exercises, c-r185).
inadequate(writes_with_difficulty, character_interpretation, c-r186).
inadequate(writes_with_difficulty, musicogram, c-r187).

% Cannot Write (High Severity)
inadequate(cannot_write, reading_writing_exercises, c-r188).
inadequate(cannot_write, verbal_task_exercises, c-r189).
inadequate(cannot_write, character_interpretation, c-r190).
inadequate(cannot_write, musicogram, c-r191).
inadequate(cannot_write, bingo_games, c-r192).
inadequate(cannot_write, knowledge_exercises, c-r193).
inadequate(cannot_write, numerical_comprehension_exercises, c-r194).

%11 Mobilidade
% Mobility (Moderate Severity)
inadequate(needs_assistance, petanque_games, c-r195).
inadequate(needs_assistance, bowling_games, c-r196).
inadequate(needs_assistance, warmup_exercises, c-r197).
inadequate(needs_assistance, physiotherapy, c-r198).

% Total Dependency (High Severity)
inadequate(total_dependence, walking, c-r199).
inadequate(total_dependence, warmup_exercises, c-r200).
inadequate(total_dependence, physiotherapy, c-r201).
inadequate(total_dependence, petanque_games, c-r202).
inadequate(total_dependence, bowling_games, c-r203).
inadequate(total_dependence, adapted_handkerchief_games, c-r204).
inadequate(total_dependence, mime_games, c-r205).

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
preference(preferences_theatre, dramatic_text_reading, p-r1).
preference(preferences_theatre, character_interpretation, p-r2).
preference(preferences_theatre, mime_games, p-r3).
preference(preferences_theatre, makeup, p-r4).
%2-Museum
preference(preferences_museum, paintings, p-r5).
preference(preferences_museum, small_arrangements, p-r6).
preference(preferences_museum, walking, p-r7).
%3-Music
preference(preferences_music, karaoke, p-r8).
preference(preferences_music, musicogram, p-r9).
preference(preferences_music, guess_the_song, p-r10).
preference(preferences_music, instruments_use, p-r11).
%4-Reading
preference(preferences_reading, dramatic_text_reading, p-r12).
preference(preferences_reading, reading_writing_exercises, p-r13).
preference(preferences_reading, verbal_task_exercises, p-r14).
%5-Recreational Group
preference(preferences_recreational_group, petanque_games, p-r15).
preference(preferences_recreational_group, bowling_games, p-r16).
preference(preferences_recreational_group, mime_games, p-r17).
preference(preferences_recreational_group, bingo_games, p-r18).
preference(preferences_recreational_group, adapted_handkerchief_games, p-r19).
preference(preferences_recreational_group, execution_festival_work, p-r20).
preference(preferences_recreational_group, centre_decoration_work, p-r21).
%6-Art
preference(preferences_art, paintings, p-r22).
preference(preferences_art, centre_decoration_work, p-r23).
preference(preferences_art, felt_work, p-r24).
preference(preferences_art, small_arrangements, p-r25).
%7-Sports
preference(preferences_sports, walking, p-r26).
preference(preferences_sports, petanque_games, p-r27).
preference(preferences_sports, bowling_games, p-r28).
%8-Cooking
preference(preferences_cooking, dessert_making, p-r29).
%9-Handicrafts
preference(preferences_handicrafts, construction_technique, p-r30).
preference(preferences_handicrafts, recycled_materials, p-r31).
preference(preferences_handicrafts, felt_work, p-r32).
preference(preferences_handicrafts, small_arrangements, p-r33).
preference(preferences_handicrafts, paintings, p-r34).
preference(preferences_handicrafts, centre_decoration_work, p-r35).
preference(preferences_handicrafts, manicure, p-r36).
preference(preferences_handicrafts, makeup, p-r37).
preference(preferences_handicrafts, massages, p-r38).

