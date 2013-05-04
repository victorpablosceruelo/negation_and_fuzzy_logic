:- module(_, [nrev/2], [assertions,functional,regtypes,nativeprops]).

:- entry nrev/2 : {list, ground} * var.


  :- pred nrev(A,B)  : list(A) => num(B).  
  :- success nrev(A,B) => size_o(B,length(A)).  
  :- comp nrev(_,_)  + ( not_fails, is_det, sideff(free)  ).
  :- comp nrev(A,_)  + steps_o( length(A) ).

nrev( [] )    := [].
nrev( [H|L] ) := ~conc( nrev(L),[H] ).


  :- comp conc(_,_,_) + ( terminates, non_det ).
  :- comp conc(A,_,_) + steps_o(length(A)).

conc( [],    L ) := L.
conc( [H|L], K ) := [ H | conc(L,K) ]. 
