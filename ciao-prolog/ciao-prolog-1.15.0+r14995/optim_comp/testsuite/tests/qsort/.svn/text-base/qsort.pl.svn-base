:- module(_, [main/0], [pure]).

:- use_module(engine(internals)).
:- use_module(engine(arithmetic)).
%:- use_module(engine(atomic_basic)).
%:- use_module(engine(attributes)).
%:- use_module(engine(basic_props)).
:- use_module(engine(basiccontrol)).
%:- use_module(engine(interpreter)).
%:- use_module(engine(data_facts)).
%:- use_module(engine(exceptions)).
%:- use_module(engine(io_aux)).
%:- use_module(engine(io_basic)).
%:- use_module(engine(prolog_flags)).
%:- use_module(engine(streams_basic)).
%:- use_module(engine(system_info)).
:- use_module(engine(term_basic)).

%:- use_module(engine(term_compare)).
%:- use_module(engine(term_typing)).
% qsort
%
% David H. D. Warren
%
% quicksort a list of 50 integers

:- include('../common').
benchmark_data(qsort, 10000, [27,74,17,33,94,18,46,83,65,2,32,53,28,85,99,47,28,82,6,11,55,29,39,81, 90,37,10,0,66,51,7,21,85,27,31,63,75,4,95,99,11,28,61,74,18, 92,40,53,59,8]).
benchmark(Data, Out) :-
	qsort(Data, Out, []).

qsort([], R, R).
qsort([X|L], R, R0) :-
        partition(L, X, L1, L2),
        qsort(L2, R1, R0),
        qsort(L1, R, [X|R1]).
partition([],_,[],[]).
partition([X|L],Y,[X|L1],L2) :-
        X =< Y, !,
        partition(L,Y,L1,L2).
partition([X|L],Y,L1,[X|L2]) :-
        partition(L,Y,L1,L2).
