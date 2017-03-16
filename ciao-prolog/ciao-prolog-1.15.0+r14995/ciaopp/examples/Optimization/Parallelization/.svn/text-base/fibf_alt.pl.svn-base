:- module(fibf_alt, [fib/2], [fsyntax,assertions]).

:- fun_eval(arith(true)).

:- entry fib/2 : num * var.

:- fun_eval fib/1.

% Works, but does not parallelize.
fib(X) := X = 0 ? 0
        | X = 1 ? 1
        | fib(X-1) + fib(X-2).



