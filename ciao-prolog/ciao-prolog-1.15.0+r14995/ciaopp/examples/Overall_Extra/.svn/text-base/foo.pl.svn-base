:- module(_,[p/1],[hiord,assertions,regtypes]).

p(X) :-
	q(X).

q(nil).
q(f(X,A)) :- 
	int(X),
	q(A).

:- regtype mytype/2.

mytype(nil,_).
mytype(f(A,B),T) :-
        T(A),
        mytype(B,T).
