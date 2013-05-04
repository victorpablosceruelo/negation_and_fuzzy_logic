% test: use current_predicate instead of $defines

:- module(_, _, []).

:- use_module(engine(internals)).
:- use_module(library(lists)).

get(M, NA, A) :-
	atom(M),
	'$current_predicate'(_, P),
	functor(P, N, A),
	atom_concat(M, R, N),
	atom_concat(':', NA, R).
	
test(M) :-
	'$defines'(F, A, M),
	  ( get(M, F, A) ->
	      true
	  ; display(notinget(M, F, A)), nl
	  ),
	  fail.
test(M) :-
	get(M, F, A),
	  ( '$defines'(F, A, M) ->
	      true
	  ; display(notindef(M, F, A)), nl
	  ),
	  fail.
test(_).

test0 :-
	static_module(M),
	test(M),
	fail.
test0.

:- use_module(compiler(dynload), [static_module/1]).
:- use_module(library(aggregates)).
allmods(Ms) :-
	findall(M, static_module(M), Ms).
