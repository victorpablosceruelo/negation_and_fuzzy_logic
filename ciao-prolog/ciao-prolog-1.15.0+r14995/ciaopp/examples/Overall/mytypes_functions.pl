
:- module(mytypes_functions,[mylist/1,mylist/2],[ assertions, regtypes, hiord, fsyntax ]).

:- op(100,xfy,(&)).

:- regtype mylist/2.

mylist([]) := _.
mylist(H & ~mylist(Type)) := Type(H).

:- regtype mylist/1.

mylist := [].
mylist := ( _ & ~mylist ).
