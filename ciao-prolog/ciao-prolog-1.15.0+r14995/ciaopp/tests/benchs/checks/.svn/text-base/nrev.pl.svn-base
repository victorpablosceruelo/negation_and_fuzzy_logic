:- module(_, [nrev/2], [assertions,regtypes,nativeprops]).

:- entry nrev/2 : {list, ground} * var.

:- check comp nrev(_,_)  + ( not_fails, is_det ).
:- check comp nrev(_,_)  + not_fails.
:- check comp nrev(_,_)  + is_det.
:- check comp nrev(A,_)  + steps_o( length(A) ).  
:- check comp nrev(A,_)  + steps_o( exp(length(A),2) ).  
:- check comp nrev(A,_)  + steps_ub( length(A)+1 ).  
:- check comp nrev(A,_)  + steps_ub(exp(length(A),2)+1.5*length(A)+1 ). 

:- check pred nrev(A,B)  : list(A) => num(B).  
:- check pred nrev(A,B)  : list(A) => list(B).  

nrev([],[]).
nrev([H|L],RL) :-
        nrev(L,R),
        conc(R,[H],RL).

:- check comp conc(_,_,_) + terminates.
:- check comp conc(A,_,_) + steps_ub(length(A)+1).  
:- check comp conc(A,_,_) + steps_o(length(A)).  

conc([],L,L).
conc([H|L],K,[H|R]) :-
        conc(L,K,R).
