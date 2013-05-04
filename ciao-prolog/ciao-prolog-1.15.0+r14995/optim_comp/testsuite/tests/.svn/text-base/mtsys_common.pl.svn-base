/* THIS FILE MUST BE PREPROCESSED BY CPP */
/* Common definitions for multi-system benchmarks */

/* Optimization mask */
#define OPT_TYPES 1
#define OPT_ARGMODES 2
#define OPT_ARGMEMS 4
#define OPT_ARGDEREFS 8
#define OPT_IMP 16
#define OPT_TRIM 32

#define OPTIMIZED (OPT_MASK & OPT_TYPES)

#if defined(MERCURY)

:- module MODULE.

:- interface.

:- import_module io.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

:- implementation.

:- import_module int, list.

main -->
	{ benchmark_data(Name, Count0, Data) },
	{ Count = Count0 * 5 }, % scale count
	{ dummy_result(Result) },
	{ benchmark_loop(Count, Data, Result, Out) },
	io__write_string(Name),
	io__write_int(Out).

:- pred benchmark_loop(int, benchmark_data, benchmark_result, int).
:- mode benchmark_loop(in, in, in, out) is det.
benchmark_loop(N, Data, _, Out) :-
        ( N > 0 ->
          % we need to fool the compiler with the iteration number
	  % and the artificially used 'Result'
          benchmark(N, Data, Result),
          benchmark_loop(N - 1, Data, Result, Out)
	; Out = 0
	).

#elif defined(CIAO)||defined(CIAO2)||defined(CIAO3)||defined(YAP)||defined(HPROLOG)||defined(SICSTUS)||defined(SWIPROLOG)||defined(GPROLOG)||defined(WAMCC)

#if defined(CIAO)||defined(CIAO2)||defined(CIAO3)
:- module(_, [main/0], []).
:- use_module(library(prolog_sys)).
#elif defined(SWIPROLOG)||defined(SICSTUS)||defined(HPROLOG)||defined(YAP)
:- module(temp, [main/0]).
#else
/* TODO: anything? */
/* :- module(temp, [main/0]). */
#endif

benchmark_system(SYSTEM). % replaced by cpp

#if defined(CIAO2)
:- '$pragma'(analyze_idet).
#elif defined(CIAO3)
:- '$pragma'(analyze_all).
:- '$default_preddef'(ptoc).
#endif

#if defined(WAMCC)
display(X) :- write(X).
nl :- write('\n').
#endif

'$cputime'(X) :- statistics(runtime, [X|_]).

main :- benchmark_start.

benchmark_start :-
	'$cputime'(T1),
	benchmark_data(Name, Count0, Data),
	Count is Count0 * 5, % scale count
	benchmark_loop(Count, Data),
	'$cputime'(T2),
	Time is T2-T1,
	benchmark_system(System),
	display('name: '), display(Name), nl,
	display('system: '), display(System), nl,
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

#else
#error "No recognized Prolog system specified"
#endif
