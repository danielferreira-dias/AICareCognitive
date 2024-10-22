%--------------------------------------------------------------------------------
% Declare the fact as dynamic to allow adding and updating
:- dynamic allowed_activities_saved/1.
%--------------------------------------------------------------------------------

% Rule to load a knowledge base file
:- consult('knowledge_base.pl'),
   write('Knowledge base loaded.'), nl.

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

conclusions(List) :-
    get_diseases(Diseases),
    get_preferences(Preferences),
    get_conditions(Conditions),
    initialize(Diseases),
    initialize2(Conditions),
    initialize3(Preferences),
    allowed_activities_saved(List).
