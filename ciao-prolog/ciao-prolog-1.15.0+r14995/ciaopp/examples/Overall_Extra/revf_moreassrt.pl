:- module(_, [nrev/2], [assertions,fsyntax,regtypes]).
:- use_module(library(assertions(native_props))).

:- entry nrev/2 : {list, ground} * var.

:- check comp nrev(A,B) + steps_ub(length(A)+1). 
:- check comp nrev(A,B) + steps(length(A)+1).    
:- check comp nrev(A,B) + steps_lb(length(A)).  

nrev( [] )    := [].
nrev( [H|L] ) := ~conc( ~nrev(L),[H] ).

conc( [],    L ) := L.
conc( [H|L], K ) := [ H | ~conc(L,K) ]. 
