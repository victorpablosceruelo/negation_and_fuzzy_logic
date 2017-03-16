:- module(_,[fact/2],[assertions, fsyntax, regtypes]).

:- fun_eval(arith(true)).

:- entry fact/2 : num * var.

fact(0) := 1.
fact(N) := N * ~fact(--N) :- N > 0.
