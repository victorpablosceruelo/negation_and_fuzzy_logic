:- module(mul2, [r/1], [assertions, regtypes]). 


r(X) :-
        p(X),
	q(X,_Y).

p(a).
p(b).


% shown to be false with multi_success = on
:-check success q(A,B) => (ta(A), ta(B)).  

q(X,X).



:- regtype ta/1.

ta(a).

:- regtype tb/1.

tb(b).

