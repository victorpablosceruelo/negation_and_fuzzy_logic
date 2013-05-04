:- module(_, [nrev/2], [assertions,fsyntax,nativeprops]).

:- entry nrev/2 : {list, ground} * var.



 :- pred nrev(A,B)  : list(A) => list(B), size_lb(B,length(A)).  
 :- comp nrev(_,_)  + ( not_fails, is_det ).
 :- comp nrev(A,_)  + steps_o( exp(length(A),2) ).  

nrev( [] )    := [] .
nrev( [H|L] ) := ~conc( ~nrev(L),[H] ).



 :- comp conc(_,_,_) + ( terminates, is_det ).
 :- comp conc(A,_,_) + steps_o(length(A)).

conc( [],    L ) := L.
conc( [H|L], K ) := [ H | ~conc(L,K) ]. 
