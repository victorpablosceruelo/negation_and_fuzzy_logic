:- module(aux,[p/1,p_l/1],[assertions]).

p(_X).

:- trust comp p(X): list(X) + equiv(p_l(X)).

:- trust success p(X)=> true.

p_l(_X).
