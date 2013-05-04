:- module(_, [nrev/2], [assertions,functions,regtypes,nativeprops]).

:- function(arith(false)).

:- entry nrev/2 : {list, ground} * var.

:- check pred nrev(A,B)  : list(A) => num(B).  
:- check pred nrev(A,B)  : list(A) => list(B).  
:- check comp nrev(_,_)  + ( not_fails, is_det ).
:- check comp nrev(A,_)  + steps_ub( length(A)+1 ).  
:- check comp nrev(A,_)  + steps_ub(exp(length(A),2)+1.5*length(A)+1 ). 
:- check comp nrev(A,_)  + steps_o( length(A) ).  
:- check comp nrev(A,_)  + steps_o( exp(length(A),2) ).  


nrev( [] )    := [].
nrev( [H|L] ) := ~conc( ~nrev(L),[H] ).

:- check comp conc(_,_,_) + terminates.
:- check comp conc(A,_,_) + steps_o(length(A)).  
:- check comp conc(A,_,_) + steps_ub(length(A)+1).  

conc( [],    L ) := L.
conc( [H|L], K ) := [ H | ~conc(L,K) ]. 

