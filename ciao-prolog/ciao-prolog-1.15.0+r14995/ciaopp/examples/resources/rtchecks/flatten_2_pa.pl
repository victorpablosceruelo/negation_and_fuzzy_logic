:- module(_1,[flatten/2],[assertions,regtypes,nativeprops,ciaopp(examples(resources(rtchecks))),basicmodes,rtchecks]).

:- resource res_steps.

:- head_cost(ub,res_steps,1).

:- literal_cost(ub,res_steps,0).

:- trust_default+cost(ub,res_steps,0).

:- true pred append(_1,L,_2)
         : ( list(_1,constant), list(L,constant), term(_2) )
        => ( list(_1,constant), list(L,constant), list(_2,constant) ).

:- true pred append(_1,L,_2)
         : ( mshare([[_2]]), var(_2), ground([_1,L]) )
        => ground([_1,L,_2]).

:- true pred append(_1,L,_2)
         : ( list(_1,constant), list(L,constant), var(_2) )
        => ( list(_1,constant), list(L,constant), list(_2,constant) )
         + ( not_fails, covered ).

:- true pred append(_1,L,_2)
         : ( list(_1,constant), list(L,constant), var(_2) )
        => ( list(_1,constant), list(L,constant), list(_2,constant), size(lb,_1,length(_1)), size(lb,L,length(L)), size(lb,_2,length(L)+length(_1)) )
         + cost(lb,ticks,406.2467806087939*length(_1)+302.2800160722923).

:- true pred append(_1,L,_2)
         : ( list(_1,constant), list(L,constant), var(_2) )
        => ( list(_1,constant), list(L,constant), list(_2,constant), size(ub,_1,length(_1)), size(ub,L,length(L)), size(ub,_2,length(L)+length(_1)) )
         + ( cost(ub,res_steps,length(_1)+1), cost(ub,ticks,441.8077927025248*length(_1)+330.92511167524) ).

append([],L,L).
append([H|L],L1,[H|R]) :-
        append(L,L1,R).

:- entry flatten(_1,_2)
         : ( gnd(_1), var(_2) ).

:- true pred flatten(X,Ys)
         : ( gnd(X), term(Ys) )
        => ( rt22(X), list(Ys,constant) ).

:- true pred flatten(X,Ys)
         : ( mshare([[Ys]]), var(Ys), ground([X]) )
        => ground([X,Ys]).

:- true pred flatten(X,Ys)
         : ( gnd(X), var(Ys) )
        => ( rt22(X), list(Ys,constant) )
         + ( not_fails, covered ).

:- true pred flatten(X,Ys)
         : ( gnd(X), var(Ys) )
        => ( rt22(X), list(Ys,constant), size(lb,X,size(X)), size(lb,Ys,size(X)) )
         + cost(lb,ticks,203.1233903043969*exp(size(X)-1,2)+604.5600321445846*size(X)).

:- true pred flatten(X,Ys)
         : ( gnd(X), var(Ys) )
        => ( rt22(X), list(Ys,constant), size(ub,X,size(X)), size(ub,Ys,size(X)+1) )
         + ( cost(ub,res_steps,0.5*exp(size(X)-1,2)+3*size(X)+1), cost(ub,ticks,236.2401058865625*exp(size(X)-1,2)+1103.658016053005*size(X)+472.480211773125) ).

flatten(X,[X]) :-
        atomic(X),
        X\==[],
        !.
flatten([],[]).
flatten([X|Xs],Ys) :-
        flatten(X,Ys1),
        flatten(Xs,Ys2),
        append(Ys1,Ys2,Ys).


:- regtype rt22/1.

rt22(A) :-
        atm(A).
rt22(A) :-
        num(A).
rt22([A|B]) :-
        rt22(A),
        rt22(B).


