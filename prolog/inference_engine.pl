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
             \+ (member(Disease, Diseases), cannot(Disease, Activity, _))),
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
             \+ (member(Cond, Conditions), inadequate(Cond, Activity, _))),
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
            (member(Preference, Preferences), preference(Preference, Activity, _), member(Activity, Activities)),
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
% EXPLANATION MODULE - WHY NOT - ACTIVITY?
% Check why an activity is not allowed
why_not(Activity, Justification) :-
    allowed_activities_saved(AllowedActivities),
    \+ member(Activity, AllowedActivities),
    findall([Reason, Rule], reason_not_allowed(Activity, [Reason, Rule]), PartialJustifications),
    Justification = PartialJustifications,
    !.

% Define specific reasons for exclusion
reason_not_allowed(Activity, [Justification, Rule]) :-
    get_diseases(Diseases),
    member(Disease, Diseases),
    cannot(Disease, Activity, Rule),
    Justification = ["diseases", Disease].

reason_not_allowed(Activity, [Justification, Rule]) :-
    get_conditions(Conditions),
    member(Condition, Conditions),
    inadequate(Condition, Activity, Rule),
    condition(Match, Condition),
    Justification = ["conditions", Match, Condition].

%--------------------------------------------------------------------------------   
% EXPLANATION MODULE - WHY - ACTIVITY?
% Explanation of why an activity was selected
why(Activity, Justification) :-
    allowed_activities_saved(AllowedActivities),
    member(Activity, AllowedActivities),
    % Generate the step-by-step explanation
    findall(Reason, reason_allowed(Activity, Reason), PartialJustifications),
    Justification = PartialJustifications,
    !.

% Specific selection reasons based on preferences
reason_allowed(Activity, Reason) :-
    get_preferences(Preferences),
    member(Pref, Preferences),
    preference(Pref, Activity, Rule),
    Reason = ["preferences", [[Pref, Rule]]].

% Specific selection reasons based on conditions that could have prevented the activity
reason_allowed(Activity, Reason) :-
    % Collect all conditions that could restrict the activity but did not
    findall([ConditionGroup, Condition, Rule], 
        (inadequate(Condition, Activity, Rule), 
         condition(ConditionGroup, Condition),
         \+ evidence(ConditionGroup, Condition)), 
        CouldHaveTriggeredConditions),
    % Ensure Reason is only added if there are conditions that could have been triggered
    CouldHaveTriggeredConditions \= [],
    Reason = ["conditions", CouldHaveTriggeredConditions].

% Specific selection reasons based on diseases that could have prevented the activity
reason_allowed(Activity, Reason) :-
    % Collect all diseases that could restrict the activity but did not
    findall([Disease, Rule], 
        (cannot(Disease, Activity, Rule), 
         \+ disease(Disease)), 
        CouldHaveTriggeredDiseases),
    % Ensure Reason is only added if there are diseases that could have been triggered
    CouldHaveTriggeredDiseases \= [],
    Reason = ["diseases", CouldHaveTriggeredDiseases].
