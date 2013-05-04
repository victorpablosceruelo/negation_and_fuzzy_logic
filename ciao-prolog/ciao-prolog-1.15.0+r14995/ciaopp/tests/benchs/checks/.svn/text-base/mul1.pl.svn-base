:- module(mul1, [r/0], [assertions, regtypes]). 



r :-
        p(X,Y),
	q(X,Y).

p(a,b).
p(b,a).

% assertion shown to be false in multivariant analysis (multi_success=on)

:-check success p(A,B) => (ta(A), ta(B)).
q(X,X).



:- regtype ta/1.

ta(a).

