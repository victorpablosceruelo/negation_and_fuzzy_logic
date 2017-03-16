:- class(benchmark_runner, [], [oo_syntax, hiord, assertions, fsyntax, mutables, string_type]).

:- doc(title, "A Benchmark Runner").
% Benchmarks are executed in pristine workers.

:- use_module(engine(io_basic)).
:- use_module(library(arithpreds)).
:- use_class(library(stopwatch)).

:- attr repeat_count.
:- attr title.
:- attr goal.
:- attr vars.
:- attr timer.
:- attr verbose.

% TODO: add inteface or class for puzzle...
:- export(cons__/2).
cons__(Verbose, Puzzle) :-
        ~verbose = Verbose,
        ~title = ~Puzzle.name,
	~repeat_count = ~Puzzle.repeat_count,
	~vars = Vars, % the variables of the goal
	~goal = Puzzle.solve(Vars), % closure
	~timer = ~stopwatch.

% Do benchmark (execute all solutions, repeat several times)
:- export(benchmark/0).
benchmark :-
	Goal = ~goal,
        W = ~basiccontrol.worker,
        W.restart,
        W.execute(loop(Goal)).

loop(Goal) :-
	% use different loops to avoid overhead in checking 'verbose'
        ( ~verbose = yes ->
	    loop_((Goal, display(~vars), nl))
	; loop_(Goal)
	).

loop_(Goal) :-
	Count = ~repeat_count,
	timer.start,
	repeat_call(Count, Goal),
	% Stop
	stop_clock.

repeat_call(Count, Goal) :-
	( % (failure-driven loop)
          repeat(Count),
	    Goal,
	    fail
	; true
	).

repeat(_N).
repeat(N) :- N > 1, repeat(N - 1).

stop_clock :-
	display(~title), nl,
        Elapsed = ~timer.end,
	display("Finished in " + ~Elapsed.'$to_str' + " ms"), nl.

