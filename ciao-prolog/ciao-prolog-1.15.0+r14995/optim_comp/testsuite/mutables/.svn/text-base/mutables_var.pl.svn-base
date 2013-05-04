% (variation of mutables.pl) 
%
% Mutables variables can be implemented just with var/1, and
% new_atom/1.  This code is being used as a simple Prolog
% specification of semantics of ImProlog's mutable variables.
%
% Author: Jose F. Morales
% Mon Oct 27 12:36:20 CET 2008

:- module(_, [main/0], [compiler(complang)]).

% ---------------------------------------------------------------------------
% This example implements mutable variables in common Prolog only by
% using var/1. There is no need for contexts, or extra arguments.  The
% only side-effect is prolog_sys:new_atom/1, but it is required only
% to make comparison of mutable names work.
%
% However, this can be extremelly inefficient!

:- use_module(library(prolog_sys), [new_atom/1]).

:- '$context_def'(mutstore, none).
:- '$context_def'(mutstore_ro, none).
:- '$def_binder'(empty_mutstore, true).

:- op(50, fx, [(@)]).
:- fun_eval (@)/1.
@X := V :-
	nonvar(X), X = '$v'(_Id, _Type, Values),
	last_value(Values, V).

last_value(Xs, _) :- var(Xs), !, fail.
last_value([X|Xs], V) :- var(Xs), !, X = V.
last_value([_|Xs], V) :- last_value(Xs, V).

:- op(700, xfx, [(<-)]).
X <- V :-
	nonvar(X), X = '$v'(_Id, Type, Values),
	'$trust_metatype'(Type, pred(1)),
	check_type(Type, V),
	add_value(Values, V).

add_value(Xs, V) :- var(Xs), !, Xs = [V|_].
add_value([_|Xs], V) :- add_value(Xs, V).

:- meta_predicate newmut(pred(1), ?, ?).
newmut(Type, V) := '$v'(Id, Type, [V|_]) :-
	check_type(Type, V),
	Id = ~new_atom.

:- meta_predicate check_type(pred(1), ?).
check_type(Type, V) :-
	( Type(V) -> true ; throw(mutable_type_error(Type, V)) ).

% ---------------------------------------------------------------------------

:- include(.(test_code)).
