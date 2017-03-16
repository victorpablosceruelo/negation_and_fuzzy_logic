:- module(_, [nrev/2,mylist/1], [assertions,fsyntax,regtypes,nativeprops]).

:- entry nrev/2 : {list, ground} * var.

:- op(200,xfy,[&]).

nrev( nil )   := nil.
nrev( H & L ) := ~conc( ~nrev(L), H & nil ).

conc( nil,    L ) := L.
conc( H & L , K ) := H & ~conc(L,K) . 

mylist := nil | _ & ~mylist.

