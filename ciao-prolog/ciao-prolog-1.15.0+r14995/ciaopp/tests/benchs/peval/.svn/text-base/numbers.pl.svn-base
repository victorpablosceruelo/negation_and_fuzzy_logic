:- use_module(library(prolog_sys)).

iterations(10000000).

run(Time):-
	push_prolog_flag(gc,off),
	statistics(runtime,_),
	iterations(It),
	iterate(It),
	statistics(runtime,[_,Time]),
	pop_prolog_flag(gc).


iterate(0):-!.
iterate(N):-
%	main(8,9,_),!,
	fail,!,
	N1 is N - 1,
	iterate(N1).
iterate(N):-
	N1 is N - 1,
	iterate(N1).


