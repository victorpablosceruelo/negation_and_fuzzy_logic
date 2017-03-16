:- module(_, [nrev/2], [assertions,functional]).

:- entry nrev/2 : {list, ground} * var.

nrev( [] )    := [].
nrev( [H|T] ) := ~conc( nrev(T),[H] ).

conc( [],    L ) := L.
conc( [H|T], K ) := [ H | conc(T,K) ]. 
