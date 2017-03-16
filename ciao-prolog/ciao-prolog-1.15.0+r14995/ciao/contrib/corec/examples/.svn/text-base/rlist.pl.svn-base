:- module(rlist, [rlist/1], [corec, assertions, expander]).


:- true pred rlist(X) # "@var{X} is a regular list.".

:- corecursion rlist(=).

rlist([]).
rlist([_|T]) :- rlist(T).

