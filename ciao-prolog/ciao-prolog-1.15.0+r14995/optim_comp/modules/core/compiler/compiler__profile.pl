:- module(compiler__profile, [], [pure]).

% Small wrapper predicates to profile memory and time 

:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(engine(io_basic)).
:- use_module(engine(basiccontrol)).
:- use_module(engine(term_basic)).
:- use_module(engine(arithmetic)).
:- use_module(engine(hiord_rt)).

:- public profile_mem/2.
:- meta_predicate profile_mem(?, goal).
profile_mem(Action, G) :-
	statistics(memory, [L1|_]),
	G,
	statistics(memory, [L2|_]),
	Ld is L2 - L1,
	display(user_error, '{profile: '),
	display(user_error, Action),
	display(user_error, ' took '),
	display(user_error, Ld),
	display(user_error, ' mem}'),
	nl(user_error).

:- public profile_elapsed/2.
:- meta_predicate profile_elapsed(?, goal).
profile_elapsed(Action, G) :-
	statistics(walltime, [L1|_]),
	G,
	statistics(walltime, [L2|_]),
	Ld is L2 - L1,
	display(user_error, '{profile: '),
	display(user_error, Action),
	display(user_error, ' took '),
	display(user_error, Ld),
	display(user_error, ' ms}'),
	nl(user_error).
