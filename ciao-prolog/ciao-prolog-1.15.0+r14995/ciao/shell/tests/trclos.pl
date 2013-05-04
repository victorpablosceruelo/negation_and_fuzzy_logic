:- module(trclos, [rel/2, compute_trans_closure/0],[]).

:- data rel/2.

compute_trans_closure :-
        current_fact(rel(E0,E1)),
        current_fact(rel(E1,E2)),
        E0 \== E2,
        \+ current_fact(rel(E0,E2)), !,
        asserta_fact(rel(E0,E2)),
        compute_trans_closure.
compute_trans_closure.
