:- module(prog_teresa,_,[rfuzzy, clpr]).
:- use_module(library(write),[write/1]).


:- set_prop parallelizing_prog_fuzzy_/3 => rel_ex_time/1,
                                           rel_ex_time/1,
                                           rel_ex_time/1.
:- default(parallelizing_prog_fuzzy_/3, 0).
parallelizing_prog_fuzzy_(Cond1,Cond2,Cond3) :~ max
        greater(Cond1),
        greater(Cond2),
        greater(Cond3).