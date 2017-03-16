:- module(mytypes_func,[mylist/1,mylistr/2,mylist/2],
	               [assertions,regtypes,fsyntax,hiord]).

:- op(100,xfy,(&)).

:- regtype mylist/1.

mylist := [] | (_ & ~mylist).

:- prop mylistr/2.

mylistr(T) := [] | ~T & ~mylistr(T).


:- regtype mylist/2.

mylist([],_).
mylist(H & T,Type) :- Type(H), mylist(T,Type).
