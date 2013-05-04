:- module(_1,[nrev/2],[assertions,regtypes,fsyntax,nativeprops,predefres(res_steps),basicmodes]).

:- entry nrev(_1,_2)
         : ( list(_1), ground(_1), var(_2) ).

:- true pred nrev(_2,_1)
         : ( list(_2), term(_1) )
        => ( list(_2), list(_1) ).

:- true pred nrev(_2,_1)
         : ( mshare([[_1]]), var(_1), ground([_2]) )
        => ground([_2,_1]).

:- true pred nrev(_2,_1)
         : ( list(_2), var(_1) )
        => ( list(_2), list(_1) )
         + ( not_fails, covered ).

:- true pred nrev(_2,_1)
         : ( list(_2), var(_1) )
        => ( list(_2), list(_1), size(lb,_2,length(_2)), size(lb,_1,length(_2)) )
         + cost(lb,steps,0.5*exp(length(_2),2)+1.5*length(_2)+1).

:- true pred nrev(_2,_1)
         : ( list(_2), var(_1) )
        => ( list(_2), list(_1), size(ub,_2,length(_2)), size(ub,_1,length(_2)) )
         + cost(ub,steps,0.5*exp(length(_2),2)+1.5*length(_2)+1).

nrev([],[]).
nrev([H|L],_1) :-
        nrev(L,_2),
        conc(_2,[H],_1).

:- true pred conc(_1,L,_2)
         : ( list(_1), rt3(L), term(_2) )
        => ( list(_1), non_empty_list(L), non_empty_list(_2) ).

:- true pred conc(_1,L,_2)
         : ( mshare([[_2]]), var(_2), ground([_1,L]) )
        => ground([_1,L,_2]).

:- true pred conc(_1,L,_2)
         : ( list(_1), rt3(L), var(_2) )
        => ( list(_1), non_empty_list(L), non_empty_list(_2) )
         + ( not_fails, covered ).

:- true pred conc(_1,L,_2)
         : ( list(_1), rt3(L), var(_2) )
        => ( list(_1), non_empty_list(L), non_empty_list(_2), size(lb,_1,length(_1)), size(lb,L,length(L)), size(lb,_2,length(L)+length(_1)) )
         + cost(lb,steps,length(_1)+1).

:- true pred conc(_1,L,_2)
         : ( list(_1), rt3(L), var(_2) )
        => ( list(_1), non_empty_list(L), non_empty_list(_2), size(ub,_1,length(_1)), size(ub,L,length(L)), size(ub,_2,length(L)+length(_1)) )
         + cost(ub,steps,length(_1)+1).

conc([],L,L).
conc([H|L],K,[H|_1]) :-
        conc(L,K,_1).


:- regtype rt3/1.

rt3([A]) :-
        term(A).


