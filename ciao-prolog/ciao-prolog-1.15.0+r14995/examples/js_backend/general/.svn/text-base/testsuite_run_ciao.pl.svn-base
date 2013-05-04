:- use_package([fsyntax, hiord]).
% (version for ciao/optim_comp)

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
	Puzzle:main,
%	Verbose = no,
%	Puzzle:name(Name),
%	display(name(Name)), nl,
%        Puzzle:solve(L),
%	display(l(L)), nl,
	!.

