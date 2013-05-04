:- module(_, [nrev/2], [assertions,fsyntax,nativeprops]).

:- entry nrev/2 : {list, ground} * var.

:- pred nrev(A,B) : list(A) 
                 => ( size_lb(B,2*length(A)) )
                  + ( not_fails, is_det, steps_o( length(A) ) ).

nrev( [] )    := [].
nrev( [H|L] ) := ~conc( ~nrev(L),[H] ).

:- pred conc(A,B,C) => size(C,length(A)+length(B)) 
                    + ( terminates, non_det, steps_o(length(A)) ).

conc( [],    L ) := L.
conc( [H|L], K ) := [ H | ~conc(L,K) ]. 
