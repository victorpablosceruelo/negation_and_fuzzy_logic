:- module(mytypes_func,[mylist/1,mylist/2],[assertions,regtypes,fsyntax,hiord ]).

:- op(100,xfy,(&)).

:- regtype mylist/2.

%% mylist(T) := [] ; ~T & ~mylist(T).

%% mylist(_) := [].
%% mylist(T) := ~T & ~mylist(T).

mylist([],_).
mylist(H & T,Type) :- Type(H), mylist(T,Type).

:- regtype mylist/1.

mylist := [] ; (_ & ~mylist).
