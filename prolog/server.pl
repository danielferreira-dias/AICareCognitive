:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/json)).
:- use_module(library(http/http_server_files)).

:- dynamic evidence/2.

:- consult('questionnaire.pl'),
    write('Questionnaire loaded!'), nl.

:- http_handler(root(next_question), get_next_question, []).
:- http_handler(root(get_why/Activity), get_why(Activity), [method(get)]).
:- http_handler(root(get_why_not/Activity), get_why_not(Activity), [method(get)]).
:- http_handler(root(answer), post_answer, []).
:- http_handler(root(bulk_answer), post_bulk_answer, []).

server(Port) :-
    http_server(http_dispatch, [port(Port)]).

get_next_question(_) :-
    (   next_question(Question, PossibleAnswers)
    ->  reply_json_dict(_{type: "question", question: Question, possibleAnswers: PossibleAnswers})
    ;   conclusions(Activities)
    ->  reply_json_dict(_{type: "conclusion", conclusion: Activities})
    ;   reply_json_dict(_{error: "No further questions or conclusions could be determined."})
    ),
    retractall(evidence(_, _)).

get_why(Activity, _) :-
    conclusions(_),
    (   why(Activity, Justifications)
    ->  reply_json_dict(_{justifications: Justifications})
    ;   reply_json_dict(_{error: "No reason why."})
    ),
    retractall(evidence(_, _)).

format_justification([JustificationList, Rule], _{justification: JustificationString, ruleTriggered: RuleString}) :-
    atomic_list_concat(JustificationList, '.', JustificationString),
    term_string(Rule, RuleString).

get_why_not(Activity, _) :-
    conclusions(_),
    (   why_not(Activity, Justifications),
        maplist(format_justification, Justifications, FormattedJustifications)
    ->  reply_json_dict(_{justifications: FormattedJustifications})
    ;   reply_json_dict(_{error: "No reason why not."})
    ).

post_answer(Request) :-
    catch(
        (   http_read_json_dict(Request, Dict),
            (   get_dict(evidence, Dict, Evidence),
                get_dict(answer, Dict, Answer)
            ->  atom_string(EvidenceAtom, Evidence),
                atom_string(AnswerAtom, Answer),
                assert_evidence(EvidenceAtom, AnswerAtom),
                reply_json_dict(_{status: "success", evidence: EvidenceAtom, answer: AnswerAtom})
            ;   reply_json_dict(_{error: "Invalid input, please provide 'evidence' and 'answer'."})
            )
        ),
        _,
        (
            reply_json_dict(_{error: "Internal server error"})
        )
    ).

post_bulk_answer(Request) :-
    catch(
        (   http_read_json_dict(Request, Dict),
            (   is_list(Dict)
            ->  bulk_assert_evidence(Dict),
                reply_json_dict(_{status: "success"})
            ;   reply_json_dict(_{error: "Invalid input, expected a list of evidence-answer objects."})
            )
        ),
        _,
        (
            reply_json_dict(_{error: "Internal server error"})
        )
    ).

bulk_assert_evidence([]).
bulk_assert_evidence([H|T]) :-
    get_dict(evidence, H, Evidence),
    get_dict(answer, H, Answer),
    atom_string(EvidenceAtom, Evidence),
    atom_string(AnswerAtom, Answer),
    assert_evidence(EvidenceAtom, AnswerAtom),
    bulk_assert_evidence(T).

assert_evidence(Evidence, Answer) :-
    retractall(evidence(Evidence, _)),
    assert(evidence(Evidence, Answer)).
