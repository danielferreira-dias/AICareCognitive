%--------------------------------------------------------------------------------
% Declare the fact as dynamic to allow adding and updating
:- dynamic allowed_activities_saved/1.
%--------------------------------------------------------------------------------

% Rule to load a knowledge base file
:- consult('knowledge_base.pl'),
   write('Knowledge base loaded!'), nl.

%--------------------------------------------------------------------------------

% Rule to save the allowed activities in a dynamic fact
initialize(Diseases) :-
    allowed_activities(Diseases, Activities),
    retractall(allowed_activities_saved(_)),  % Remove any previous fact
    assertz(allowed_activities_saved(Activities)).

% Rule to retrieve a list of disease/activity
allowed_activities(Diseases, AllowedActivities) :-
    findall(Activity,
            (activity(Activity),
             \+ (member(Disease, Diseases), cannot(Disease, Activity))),
            AllowedActivities).

%--------------------------------------------------------------------------------

% Rule to save the allowed activities in a dynamic fact
initialize2(Conditions) :-
    filter_suitable_activities(Conditions, Activities),
    retractall(allowed_activities_saved(_)),  % Remove any previous fact
    assertz(allowed_activities_saved(Activities)).

% Rule to filter allowed activities based on conditions
filter_suitable_activities(Conditions, SuitableActivities) :-
    allowed_activities_saved(Activities),
    findall(Activity,
            (member(Activity, Activities),
             \+ (member(Cond, Conditions), inadequate(Cond, Activity))),
            SuitableActivities).

%--------------------------------------------------------------------------------

% Rule for initialize3, which reorders the list of allowed activities based on given preferences
initialize3(Preferences) :-
    allowed_activities_saved(Activities),
    reorder_activities_by_preferences(Preferences, Activities, OrderedActivities),
    retractall(allowed_activities_saved(_)),  % Remove any previous fact
    assertz(allowed_activities_saved(OrderedActivities)).

% Rule to reorder activities, placing those that match preferences at the beginning
reorder_activities_by_preferences(Preferences, Activities, OrderedActivities) :-
    % Find activities related to preferences, removing duplicates
    findall(Activity,
            (member(Preference, Preferences), preference(Preference, Activity), member(Activity, Activities)),
            RepeatedPreferredActivities),
    sort(RepeatedPreferredActivities, PreferredActivities),  % Remove duplicates
    % Filter activities that are not preferred
    exclude(is_in_list(PreferredActivities), Activities, OtherActivities),
    % Combine preferred activities at the beginning of the list
    append(PreferredActivities, OtherActivities, OrderedActivities).

% Auxiliary predicate to check if an activity is in a list
is_in_list(List, Element) :-
    member(Element, List).

%--------------------------------------------------------------------------------

% Display the saved allowed activities
show_allowed_activities_saved :-
    allowed_activities_saved(Activities),
    maplist(writeln, Activities).

%--------------------------------------------------------------------------------   

conclusions(List) :-
    get_diseases(Diseases),
    get_conditions(Conditions),
    get_preferences(Preferences),
    initialize(Diseases),
    initialize2(Conditions),
    initialize3(Preferences),
    allowed_activities_saved(List).

%--------------------------------------------------------------------------------   
%MODULO DE EXPLICACIONES - WHY NOT - ACTIVITY?

% Verificar por qué una actividad no está permitida
why_not(Actividad, Justificacion) :-
    allowed_activities_saved(ActividadesPermitidas),
    \+ member(Actividad, ActividadesPermitidas),
    (
        reason_not_allowed(Actividad, Justificacion)
        ; Justificacion = ["La actividad ", Actividad, " no está permitida por razones desconocidas"]
    ).

% Definir razones específicas de exclusión
reason_not_allowed(Actividad, Justificacion) :-
    get_diseases(Diseases),
    member(Disease, Diseases),
    cannot(Disease, Actividad),
    Justificacion = ["La actividad ", Actividad, " no está permitida para la enfermedad ", Disease].

reason_not_allowed(Actividad, Justificacion) :-
    get_conditions(Conditions),
    member(Condition, Conditions),
    inadequate(Condition, Actividad),
    condition(Match, Condition),
    Justificacion = ["La actividad ", Actividad, " no es adecuada debido a la condición ", Match, ": ", Condition].

reason_not_allowed(Actividad, Justificacion) :-
    get_preferences(Preferences),
    \+ member(Actividad, Preferences),
    Justificacion = ["La actividad ", Actividad, " no coincide con las preferencias registradas"].


%--------------------------------------------------------------------------------   
%MODULO DE EXPLICACIONES - WHY - ACTIVITY?
% Explicación de por qué una actividad fue seleccionada
why(Actividad, Justificacion) :-
    allowed_activities_saved(ActividadesPermitidas),
    member(Actividad, ActividadesPermitidas),
    % Generar la explicación paso a paso
    findall(Razon, reason_allowed(Actividad, Razon), JustificacionesParciales),
    Justificacion = ["La actividad ", Actividad, " fue seleccionada por las siguientes razones: " | JustificacionesParciales].

% Razones específicas de selección basadas en preferencias
reason_allowed(Actividad, Razon) :-
    get_preferences(Preferences),
    member(Pref, Preferences),
    preference(Pref, Actividad),
    Razon = ["el usuario prefiere actividades de este tipo: ", Pref].

% Razones específicas de selección basadas en condiciones
reason_allowed(Actividad, Razon) :-
    get_conditions(Conditions),
    forall(member(Condition, Conditions), \+ inadequate(Condition, Actividad)),
    Razon = ["las condiciones del usuario no impiden desarrollar esta actividad"].

% Razones específicas de selección basadas en enfermedades (utilizando cannot/2)
reason_allowed(Actividad, Razon) :-
    get_diseases(Diseases),
    forall(member(Disease, Diseases), \+ cannot(Disease, Actividad)),
    Razon = ["las enfermedades del usuario no impiden desarrollar esta actividad"].
