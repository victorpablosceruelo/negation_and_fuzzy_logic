:- module(_,_, [fsyntax, clpq]).

:- fun_eval(arith(true)).

% A function
fact(0) := 1.
fact(N) := N * ~fact(--N) :- N > 0.

% A predicate
append([],X,X).
append([X|Y],Z,[X|W]) :-
	append(Y,Z,W).

% Using constraints (CLP(Q))
fib(X,Y) :- X .=. 0, Y .=. 0.
fib(X,Y) :- X .=. 1, Y .=. 1.
fib(N,F) :- N .>. 1,
            N1 .=. N - 1,
            N2 .=. N - 2,
            fib(N1, F1),
            fib(N2, F2),
            F .=. F1+F2.
