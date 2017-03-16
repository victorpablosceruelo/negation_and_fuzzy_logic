:- module(disjwait, [main/0], [andprolog]).

:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(system)).
:- use_module(library(apll)).
:- use_module(library(format), [format/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main :-
	set_prolog_flag(gc, off),
	ensure_agents(3),

        statistics(walltime, [T1,_]),
	sequential,
        statistics(walltime, [T2,_]),
        Delta1 is T2 - T1,
	format("-- sequential, ~f ms.~n~n", [Delta1]),

        statistics(walltime, [T3,_]),
	restricted,
        statistics(walltime, [T4,_]),
        Delta2 is T4 - T3,
	format("-- restricted, ~f ms.~n~n", [Delta2]),

        statistics(walltime, [T5,_]),
	unrestricted,
        statistics(walltime, [T6,_]),
        Delta3 is T6 - T5,
	format("-- unrestricted, ~f ms.~n~n", [Delta3]),

        statistics(walltime, [T7,_]),
	disjwait,
        statistics(walltime, [T8,_]),
        Delta4 is T8 - T7,
	format("-- disjwait, ~f ms.~n~n", [Delta4]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sequential :-
	p(100000),
	p(1000000),
	display('sequential/0 done'), nl.

restricted :-
	p(100000) & p(1000000),
	display('restricted/0 done'), nl.

unrestricted :-
	p(100000) &> H1,
	p(1000000) &> H2,
	H1 <&,
	H2 <&,
	display('unrestricted/0 done'), nl.

disjwait :-
	p(100000) &> H1,
	p(1000000) &> H2,
	(H1 ; H2) <?,
	(
	    H1 &? ->
	    display('H1 finished'), nl,
	    H2 <&
	;
	    display('H2 finished'), nl,
	    H1 <&
	),
	display('disjwait/0 done'), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

p(0) :- !.
p(X) :-
	X > 0,
	X1 is X-1,
	p(X1).

