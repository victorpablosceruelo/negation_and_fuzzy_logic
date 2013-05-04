:- module(_1,[reverse/2],[assertions,regtypes,nativeprops,ciaopp(tests(resources)),predefres(res_steps),basicmodes]).

:- use_module(library(lists),[append/3]).

:- doc(author,"Edison Mera").

:- doc(module,"This program shows an example of an assertion
	about a predicate, @pred{append/3}, that is defined in other
	module. Note that there is not lower bound assertion for
	append/3, just to test the warning message").

:- resource steps_2.

:- head_cost(ub,steps_2,2).

:- head_cost(lb,steps_2,2).

:- literal_cost(ub,steps_2,0).

:- literal_cost(lb,steps_2,0).

:- trust_default+cost(ub,steps_2,0).

:- trust_default+cost(lb,steps_2,0).

:- entry reverse(X,Y)
         : ( var(Y), list(X,num) ).

:- true pred reverse(X,Y)
         : ( list(X,num), term(Y) )
        => ( list(X,num), list(Y) ).

:- true pred reverse(X,Y)
         : ( mshare([[Y]]), var(Y), ground([X]) )
        => ground([X,Y]).

:- true pred reverse(X,Y)
         : ( list(X,num), var(Y) )
        => ( list(X,num), list(Y) )
         + ( not_fails, covered ).

:- true pred reverse(X,Y)
         : ( list(X,num), var(Y) )
        => ( list(X,num), list(Y), size(lb,X,length(X)), size(lb,Y,length(X)) )
         + ( cost(lb,steps,0.5*exp(length(X),2)+1.5*length(X)+1), cost(lb,steps_2,2*length(X)+2) ).

:- true pred reverse(X,Y)
         : ( list(X,num), var(Y) )
        => ( list(X,num), list(Y), size(ub,X,length(X)), size(ub,Y,length(X)) )
         + ( cost(ub,steps,0.5*exp(length(X),2)+1.5*length(X)+1), cost(ub,steps_2,exp(length(X),2)+3.0*length(X)+2) ).

reverse([],[]).
reverse([X|T],L) :-
        reverse(T,L1),
        append(L1,[X],L).


