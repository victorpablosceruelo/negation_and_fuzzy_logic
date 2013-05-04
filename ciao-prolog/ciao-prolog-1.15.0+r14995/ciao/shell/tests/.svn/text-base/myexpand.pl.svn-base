:- module(myexpand, [body_exp/3], []).

:- use_module(engine(internals)).

:- multifile primitive_meta_predicate/2.

body_exp(B, Mod, NB) :- body_expansion(B, Mod, -, compile, NB).

uses_runtime_module_expansion. % Called from engine(mexpand)

module_warning(W) :- display(W), nl.

ciaopp_expansion :- fail.
mexpand_meta_args(M, P, Primitive) :-
	'$meta_args'(M, P),
	( '$primitive_meta_predicate'(P, M) ->
	    Primitive = true
	; Primitive = fail
	).
mexpand_imports(M, IM, F, N, EM) :-
	'$imports'(M, IM, F, N, EM).
mexpand_defines(M, F, N) :-
	'$defines'(M, F, N).
mexpand_multifile(M, F, N) :-
	'$multifile'(M, F, N).

redefining(_,_,_). % Avoid imported_needs_qual warnings

:- include(engine(mexpand)).
