:- use_package([oo_syntax, hiord, assertions, fsyntax, mutables, string_type]).

:- use_class(.(benchmark_runner)).

%:- use_module(engine(io_basic)).

:- include(.(testsuite_base)).

% Here starts the program
:- export(main/0).
main :-
	% TODO: fix use_module from different scopes,
	%       do a use_module, and enable this like
%	display("Testsuite for the Ciao JavaScript back-end"), nl,
        do_puzzles(~all_puzzles).

do_puzzles([]) :- !.
do_puzzles([X|Xs]) :- do_puzzle(X), do_puzzles(Xs).

do_puzzle(Puzzle) :-
	Verbose = no,
        R = ~benchmark_runner(Verbose, Puzzle),
	R.benchmark.

