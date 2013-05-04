
:- module(mytypes,[mylist/1,mylist/2],[ assertions, regtypes, hiord ]).

:- op(100,xfy,(&)).

:- regtype mylist/2.

mylist([],_).
mylist(H & T,Type) :- Type(H), mylist(T,Type).

:- regtype mylist/1.

mylist([]).
mylist(_ & T) :- mylist(T).
