:- module(_, _, [assertions, library(resdefs(resources_decl))]).
:- use_module(resources(resources_basic)).

delta_phrases(LitInfo, N) :-
	litinfo_get_lit(LitInfo, Head),
	phrase_const(Head),
	number_clauses(Head, N),
	!.
delta_phrases(_LitInfo, 0) :- !.

phrase_const('grammar:comb'(_, _, _)) :- !.
phrase_const('grammar:det'(_,  _, _)) :- !.
phrase_const('grammar:noun'(_, _, _)) :- !.
phrase_const('grammar:verb'(_, _, _)) :- !.

% This predicate should be defined through LitInfo
number_clauses('grammar:comb'(_, _, _), 2).
number_clauses('grammar:det'(_,  _, _), 2).
number_clauses('grammar:noun'(_, _, _), 4).
number_clauses('grammar:verb'(_, _, _), 4).
