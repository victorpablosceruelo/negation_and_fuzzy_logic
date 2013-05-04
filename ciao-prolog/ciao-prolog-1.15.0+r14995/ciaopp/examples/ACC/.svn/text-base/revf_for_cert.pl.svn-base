:- module(_, [nrev/2], [assertions,fsyntax,regtypes,nativeprops]).

:- entry nrev/2 : {list, ground} * var.

%nrev( [] )    := _.
nrev( [] )    := [].
nrev( [H|L] ) := ~conc( ~nrev(L),[H] ).

conc( [],    L ) := L.
conc( [H|L], K ) := [ H | ~conc(L,K) ]. 
