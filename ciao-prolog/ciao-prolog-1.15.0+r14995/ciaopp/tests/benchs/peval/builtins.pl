:- module(_,[p/1],[]).

:- use_package(assertions).

:- trust comp p(A) : nonvar(A) + (bind_ins, error_free).

p(_).

