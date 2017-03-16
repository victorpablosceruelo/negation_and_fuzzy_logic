% Common definitions for Ciao benchmarks

:- use_module(library(prolog_sys)).
:- use_module(engine(io_basic)).

%:- '$pragma'(analyze_idet).

'$cputime'(X) :- statistics(runtime, [X|_]).

:- export(main/0).
main :- benchmark_start.

benchmark_start :-
	'$cputime'(T1),
	benchmark_data(Name, Count0, Data),
	Count is Count0 * 5, % scale count
	benchmark_loop(Count, Data),
	'$cputime'(T2),
	Time is T2-T1,
	display('name: '), display(Name), nl,
	display('count: '), display(Count), nl,
	display('time: '), display(Time), nl.

benchmark_loop(Count, Data) :-
	repeat(Count),
	benchmark(Data, _Result),
	fail.
benchmark_loop(_, _).

repeat(_N).
repeat(N) :-
	N > 1,
	N1 is N - 1,
	repeat(N1).



