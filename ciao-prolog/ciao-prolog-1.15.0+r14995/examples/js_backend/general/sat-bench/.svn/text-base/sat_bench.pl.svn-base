:- module(sat_bench, [main/0], [condcomp]).

:- use_module(.(sat)).

% Do not repeat, just show the results
% :- compilation_fact(only_result).

% Define '$cputime'
:- use_module(library(prolog_sys)).
'$cputime'(X) :- statistics(runtime, [X|_]).

:- if(defined(only_result)).
count(1).
:- else.
count(X) :- repeat_count(X).
:- endif.

:- use_module(engine(io_basic)).
:- use_module(library(prolog_sys)).
write(X) :- display(X).

:- export(main/0).
main :-
	statistics(runtime, [X0|_]),
	solve_,
	statistics(runtime, [X|_]),
%	write(L),
%	nl,
	write('time : '),
	Y is X - X0,
	write(Y),
	nl.

solve_ :-
	( count(N),
	  repeat(N),
	  solve(_L),
	  display(_L), nl,
	  fail
	; true
	).

repeat(_N).
repeat(N) :- N > 1, N1 is N - 1, repeat(N1).
