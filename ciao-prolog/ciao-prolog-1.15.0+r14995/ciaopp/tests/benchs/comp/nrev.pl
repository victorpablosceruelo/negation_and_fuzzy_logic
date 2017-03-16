:- module(_, [nrev/2], [assertions]).

:- entry nrev/2 : list(gnd) * var.

nrev([],[]).
nrev([H|L],RL) :-
        nrev(L,R),
        conc(R,[H],RL).

conc([],L,L).
conc([H|L],K,[H|R]) :-
        conc(L,K,R).
