:- module(_, [nrev/2], [assertions,fsyntax,regtypes,nativeprops]).

:- entry nrev/2 : {list, ground} * var.

%Assertion 1
 :- check comp nrev(A,_) + steps_ub(exp(length(A),2)).

%Assertion 2
:- check comp nrev(A,_) + steps_lb(length(A)).

%Assertion 3
:- check comp nrev(A,_) + steps_o(length(A)).

%Assertion 4
:- check comp nrev(A,_)  + (steps_lb(length(A)), steps_ub(exp(length(A),2))).

nrev( [] )    := [].
nrev( [H|L] ) := ~conc( ~nrev(L),[H] ).

conc( [],    L ) := L.
conc( [H|L], K ) := [ H | ~conc(L,K) ]. 
