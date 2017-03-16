:- module(_, _, []).

:- use_module(library(aggregates)).

:- data o/2.

:- data r/3.

main :-
	asserta_fact(r(_,_,_)),
	asserta_fact(r(_,_,_)),
	current_fact(r(_,_,_)),
	R=0,
	asserta_fact(o(_,_)),
        findall(_,m(_),_),
	M=0,
        current_fact(o(_,M)),
        _ is R*(1+M/100),
        rs(0,_),
	fail.

:- data t/5.
t(_,_,_,_,_).
t(_,_,_,_,_).
t(_,_,_,_,_).
t(_,_,_,_,_).

m(_) :-
	X=100001,Y=0,
        current_fact(t(_,_,_,_,_)),
	atom_number(_,Y),
	atom_number(S1,X),
        v(X),
        _=S1.

v(_) :- findall(X,x(_,_,_,_,_,_,_,_,_),Xs).

x(_,_,_,_,_,_,_,_,_).

rs(Num,[]).
rs(Num,[a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a|Vs]) :- Num < 60000,
	N2 is Num + 1,
        rs(N2,Vs).
