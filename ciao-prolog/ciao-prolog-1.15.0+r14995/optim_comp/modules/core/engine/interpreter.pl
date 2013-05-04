:- module(interpreter, [], [pure]).

:- use_module(engine(basiccontrol)).
:- use_module(engine(exceptions)).
:- use_module(engine(term_basic)).
:- use_module(engine(term_typing)).
:- use_module(engine(io_aux)).

% ---------------------------------------------------------------------------
% Interpreter (called from within the emulator)

:- use_module(engine(internals), ['$unknown'/2]).
:- use_module(engine(hiord_rt), ['$meta_call'/1, call/1]).

:- export(interpret_body/1).
interpret_body(X) :-
	'$caller_choice'(Cut),
	metacall(X, Cut).

metacall(X, _) :- var(X), !, '$meta_call'(X). % it will throw an error
metacall('basiccontrol:true', _) :- !.
metacall('basiccontrol:otherwise', _) :- !.
metacall('basiccontrol:false', _) :- !, fail.
metacall('basiccontrol:!', ?) :- !,
	% TODO: this should be a debug message, or an exception
        message(warning, '! illegal in \\+ or if-parts of ->, if; ignored').
metacall('basiccontrol:!', Cut) :- !,
	'$trust_type'(Cut, smallint),
	'$cut'(Cut).
metacall('basiccontrol:,'(X, Y), Cut) :- !,
	metacall(X, Cut),
	metacall(Y, Cut).
metacall('basiccontrol:;'('basiccontrol:->'(X,Y),Z), Cut) :- !,
	( metacall(X, ?) ->
	    metacall(Y, Cut)
	; metacall(Z, Cut)
	).
metacall('basiccontrol:->'(X,Y), Cut) :- !,
	( metacall(X, ?) ->
	    metacall(Y, Cut)
	).
metacall('basiccontrol:;'(X,Y), Cut) :- !,
	( metacall(X, Cut)
	; metacall(Y, Cut)
	).
metacall('basiccontrol:\\+'(X), _) :- !,
	\+ metacall(X, ?).
metacall('basiccontrol:if'(P,Q,R), Cut) :- !,
	if(metacall(P, ?),
	   metacall(Q, Cut),
	   metacall(R, Cut)).
metacall(X, _) :-
	'$meta_call'(X).

:- export(undefined_goal/1).
undefined_goal(X) :- number(X), !,
	throw(error(type_error(callable,X), call/1-1)).
undefined_goal(X) :- var(X), !,
        throw(error(instantiation_error, call/1-1)).
undefined_goal(X) :-
	'$unknown'(F, F), do_undefined(F, X).

do_undefined(error, X) :-
        functor(X, F, A),
        throw(error(existence_error(procedure, F/A), F/A)).
do_undefined(warning, X) :-
        message(warning, ['The predicate ', X, ' is undefined']),
        fail.
% do_undefined(fail, X) :- fail.
