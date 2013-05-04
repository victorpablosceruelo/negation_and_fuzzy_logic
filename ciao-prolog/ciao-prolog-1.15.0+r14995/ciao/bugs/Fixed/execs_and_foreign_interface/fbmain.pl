:- module(_, _, [foreign_interface]).

:- use_foreign_source(fbmod).

:- true pred foreignpred(in(Message)) :: atm + (foreign).

main :-
	foreignpred('hola'),
	nl.
