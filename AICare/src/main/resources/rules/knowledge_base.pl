:- dynamic evidence/2.

next_question(blood_ear) :-
    \+ evidence(blood_ear, _).

next_question(earache) :-
    hypothesis(haemorrhage, upper_type),
    \+ evidence(earache, _).

next_question(deafness) :-
    hypothesis(haemorrhage, upper_type),
    \+ evidence(deafness, _),
    \+ evidence(earache, yes).

next_question(cerebrospinal) :-
    hypothesis(haemorrhage, upper_type),
    \+ evidence(cerebrospinal, _),
    \+ evidence(earache, yes),
    \+ evidence(deafness, yes).

next_question(blood_nose) :-
    hypothesis(haemorrhage, lower_type),
    \+ evidence(blood_nose, _).

next_question(blood_mouth) :-
    hypothesis(haemorrhage, lower_type),
    \+ evidence(blood_mouth, _),
    \+ evidence(blood_nose, yes).

next_question(blood_brown) :-
    hypothesis(haemorrhage, lower_type),
    evidence(blood_mouth, yes),
    \+ evidence(blood_brown, _).

next_question(vomiting) :-
    hypothesis(haemorrhage, lower_type),
    evidence(blood_mouth, yes),
    \+ evidence(vomiting, _).

next_question(blood_vagina) :-
    hypothesis(haemorrhage, lower_type),
    \+ evidence(blood_vagina, _),
    \+ evidence(blood_mouth, yes),
    \+ evidence(blood_nose, yes).

next_question(blood_penis) :-
    hypothesis(haemorrhage, lower_type),
    \+ evidence(blood_penis, _),
    \+ evidence(blood_mouth, yes),
    \+ evidence(blood_nose, yes),
    \+ evidence(blood_vagina, yes).

next_question(blood_anus) :-
    hypothesis(haemorrhage, lower_type),
    \+ evidence(blood_anus, _),
    \+ evidence(blood_mouth, yes),
    \+ evidence(blood_nose, yes),
    \+ evidence(blood_vagina, yes),
    \+ evidence(blood_penis, yes).

next_question(blood_coffee) :-
    hypothesis(haemorrhage, lower_type),
    evidence(blood_anus, yes),
    \+ evidence(blood_coffee, _).

next_question(Conclusion) :-
    conclusion(Conclusion),
    !.

hypothesis(haemorrhage, upper_type) :-
    evidence(blood_ear, yes).

hypothesis(haemorrhage, lower_type) :-
    evidence(blood_ear, no).

conclusion(otorhagia) :-
    hypothesis(haemorrhage, upper_type),
    evidence(earache, yes).

conclusion(otorhagia) :-
    hypothesis(haemorrhage, upper_type),
    evidence(deafness, yes).

conclusion(skull_fracture) :-
    hypothesis(haemorrhage, upper_type),
    evidence(cerebrospinal, yes).

conclusion(epistaxe) :-
    hypothesis(haemorrhage, lower_type),
    evidence(blood_nose, yes).

conclusion(hematese) :-
    hypothesis(haemorrhage, lower_type),
    evidence(blood_mouth, yes),
    evidence(blood_brown, yes),
    evidence(vomiting, yes).

conclusion(mouth_haemorrhage) :-
    hypothesis(haemorrhage, lower_type),
    evidence(blood_mouth, yes),
    evidence(blood_brown, no),
    evidence(vomiting, no).

conclusion(metrorrhagia) :-
    hypothesis(haemorrhage, lower_type),
    evidence(blood_vagina, yes).

conclusion(hematuria) :-
    hypothesis(haemorrhage, lower_type),
    evidence(blood_penis, yes).

conclusion(melena) :-
    hypothesis(haemorrhage, lower_type),
    evidence(blood_anus, yes),
    evidence(blood_coffee, yes).

conclusion(rectal_bleeding) :-
    hypothesis(haemorrhage, lower_type),
    evidence(blood_anus, yes),
    evidence(blood_coffee, no).

conclusion(unknown) :-
    \+ conclusion(_),
    evidence(_, _).
