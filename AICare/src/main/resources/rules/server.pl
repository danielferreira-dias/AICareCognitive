% Load required libraries
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/json)).
:- use_module(library(http/http_server_files)).
:- dynamic evidence/2.

% Consult the knowledge base
:- consult('knowledge_base.pl').

% Define HTTP handlers
:- http_handler(root(next_question), get_next_question, []).
:- http_handler(root(answer), post_answer, []).

% Start the server on port 8081
server(Port) :-
    http_server(http_dispatch, [port(Port)]).

% Handler to get the next question or conclusion
get_next_question(_) :-
    (   next_question(Question)
    ->  (   is_conclusion(Question)
        ->  reply_json_dict(_{type: "conclusion", conclusion: Question})
        ;   reply_json_dict(_{type: "question", question: Question})
        ),
        retractall(evidence(_, _))
    ;   reply_json_dict(_{error: "No further questions or conclusions could be determined."})
    ).

% Helper predicate to determine if the result is a conclusion (e.g., it's not a question)
is_conclusion(Question) :-
    conclusion(Question),!.

% Handler to receive answers and assert them as facts
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

assert_evidence(Evidence, Answer) :-
    retractall(evidence(Evidence, _)),
    assert(evidence(Evidence, Answer)).

:- initialization(server(8081)).
