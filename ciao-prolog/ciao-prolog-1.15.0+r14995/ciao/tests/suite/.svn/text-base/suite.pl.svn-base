:- module(suite, [main/0], [assertions]).

:- use_module(knights).
:- use_module(memsize).
:- use_module(queens).
:- use_module(fibonacci).
:- use_module(bignums).
:- use_module(guardians).
:- use_module(jugs).

:- use_module(library(write)).
:- use_module(library(prolog_sys)).

:- test main.

main:-
        line,
        write('Executing suite of benchmarks'), nl,
        knights,
        memsize,
        queens,
        fib,
        bignums,
        guardians,
        jugs,
        statistics,
	!.


knights:-
        line,
        write('knights tour.  Should fail.'), nl,
        main(4).
knights.


memsize:-
        line,
        write('Memory operations.  Should give info about mem. and gc'), nl,
        mem.

queens:-
        line,
        write('All solutions to the 11 queens problem'), nl,
        q.

fib:-
        line,
        write('Working out the inverse fibonacci...'), nl,
        do_fib.
        
bignums:-
        line,
        write('Working with big integers...'), nl,
        do_bignums.

guardians:-
        line,
        write('Solving the guardians and locks problem...'), nl,
        guardians(1000,200,Unlocked),
        write('Unlocked cells: '),
        write(Unlocked),
        write('.'), nl.

jugs:-
        line,
        write('Solving the jugs problem...'), nl,
        solve_jugs(Solution),
        write('Solution: '),
        write(Solution),
        write('.'), nl,
        line.


line:- write(
'***************************************************************************'
), nl.
