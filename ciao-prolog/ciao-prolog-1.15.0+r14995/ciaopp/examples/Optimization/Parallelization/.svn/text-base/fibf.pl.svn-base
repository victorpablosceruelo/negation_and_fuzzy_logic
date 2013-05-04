:- module(fibf, [fib/2], [fsyntax,assertions]). 

:- entry fib/2 : num * var.

:- fun_eval fib/1.

fib(X) := X = 0 ? 0
        | X = 1 ? 1.
fib(X) := fib(X1) + fib(X2) 
       :- X1 is X-1, X2 is X-2.


