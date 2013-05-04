:- module(revf, [nrev/2], [assertions,fsyntax]).

:- entry nrev/2 : {list, ground} * var.

nrev( [] )    := [].
nrev( [H|L] ) := ~conc( ~nrev(L),[H] ).

conc( [],    L ) := L.
conc( [H|L], K ) := [ H | ~conc(L,K) ]. 
