% A collection of context definitions for profiling.
%
% Usage: execute your goal in a profile context and the execution time
% for that goal will be prompted to user_error
% 
% Author: Jose F. Morales
% Date: Fri Nov 14 21:22:20 CET 2008

{
:- '$all_static'.

:- '$def_binder'(profile0(_), true).

:- use_module(library(prolog_sys), [statistics/2]).

:- '$def_binder'(profile(A), goals_around(profile_begin(A, L1), profile_end(A, L1))).

profile_begin(A, L1) :-
	display(user_error, '{profile: begin '),
	display(user_error, A),
	display(user_error, '}'),
	nl(user_error),
	statistics(walltime, [L1|_]).

profile_end(A, L1) :-
	statistics(walltime, [L2|_]),
	Ld is L2 - L1,
	display(user_error, '{profile: ended '),
	display(user_error, A),
	display(user_error, ' took '),
	display(user_error, Ld),
	display(user_error, ' ms}'),
	nl(user_error).
%	( Ld > 200 -> pause(5) ; true ).

:- '$def_binder'(profile_costly(A), goals_around(profile_costly_begin(A, L1), profile_costly_end(A, L1))).

profile_costly_begin(_A, L1) :-
	statistics(walltime, [L1|_]).

profile_costly_end(A, L1) :-
	statistics(walltime, [L2|_]),
	Ld is L2 - L1,
	( Ld > 10.0 -> % show a message for goals that took more than 10.0 milliseconds
	    display(user_error, '{profile: goal '),
	    display(user_error, A),
	    display(user_error, ' took '),
	    display(user_error, Ld),
	    display(user_error, ' ms}'),
	    nl(user_error)
	; true
	).

%:- use_module(library(system), [pause/1]).
}.

