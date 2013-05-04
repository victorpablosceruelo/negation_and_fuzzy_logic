:- module(catcher_rt, [catcher_call/2], []).

:- use_module(library(messages)).

:- set_prolog_flag(write_strings, on).

:- meta_predicate catcher_call(addterm(goal), ?).

catcher_call(Goal, Term, Loc) :- catch(Goal, E, catcher_handler(Term, E, Loc)).

catcher_handler(Term, E, Loc) :-
	error_message(Loc, "At ~w, thrown exception ~w", [Term, E]),
	throw(E).
