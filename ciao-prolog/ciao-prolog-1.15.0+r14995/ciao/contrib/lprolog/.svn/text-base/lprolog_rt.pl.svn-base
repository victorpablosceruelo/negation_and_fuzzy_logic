:- module(lprolog_rt, [
	new_env/2,
	new_ref/2,
	display_ref/4,
	ho_unify/4,
	hnorm/5,
	put_env/4,
	new_var_name/1,
	deref/3
	], []).

:- use_module(engine(attributes)).
:- use_module(pattern_unify).
:- use_module(hnorm, [hnorm/3]).
:- use_module(lambda_terms, [new_ref/2, deref/3, display_ref/2]).
:- use_module(library(write)).
:- use_module(library(prolog_sys)).

%for generating unique variable names
:- data last_var_n/1.
last_var_n(0).

% for now we maintain ther environment as a list
% "top level" for quantifier index is 1
new_env(1,[]).

% this simply wraps Amedeo's implementation of the pattern unification code. 
% exceptions thrown by the pattern_unify code ("const_clash" etc.) are treated as failure.
% it may be the case that all the thrown exceptions simply indicate a failure in unification.
% i need to find out and treat them differently if necessary.
ho_unify(X, Y, Env, Env) :- 
	catch(pattern_unify:pattern_unify(X,Y, Env), unification_error(E), handler(E)), !.

ho_unify(Y, X, Env, Env) :- 
	catch(pattern_unify:pattern_unify(X,Y, Env), unification_error(E), handler(E)).


handler(_) :- fail.
%handler(E) :- debug_msg(E), fail.
debug_msg(E) :- display('UNIFICATION ERROR: '), display(E), nl.


slv_hnorm(X, Y, _I, Env, Env) :-
	hnorm:hnorm(X, Y, Env).


hnorm(X, Y, _I, Env, Env) :-
	% this is cheating. sometimes, there's an error trying to
	% unify with the hnorm'ed second var.
	% my impression, is that this is really a "one-way" function
	% so there should probably be special treatment for it.
	detach_attribute(Y),
	hnorm:hnorm(X, Y, Env).
%     hnorm:hnorm(X, X1),
%     hnorm:hnorm(Y, Y1),
%     pattern_unify:pattern_unify(X1, Y1).


display_ref(X, _I, Env, Env) :-
	lambda_terms:display_ref(X, Env).


new_ref(X, Y) :- lambda_terms:new_ref(X, Y).

deref(X, Y, Env) :- lambda_terms:deref(X, Y, Env).

put_env(Name, HORef, Env, [ho_ref(Name, HORef) | Env]).


% TODO: could make this a flag or something to use prettier name
% or just new_atom
new_var_name('$VAR'(N1)) :- 
	% really should retract the old one, but this should work
	% since we're asserting at the front.
	last_var_n(N), !,
	%write('=====>'), write(N), nl,
	N1 is N + 1,
	asserta_fact(last_var_n(N1)).
%new_var_name(Name) :- 
	%new_atom(Name).
