:- module(_1,[flatten/2],[assertions,regtypes,ciaopp(tests(resources)),predefres(res_steps),nativeprops,basicmodes]).

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
         + cost(lb,steps,length(_1)+1).

:- true pred append(_1,L,_2)
         : ( list(_1,constant), list(L,constant), var(_2) )
        => ( list(_1,constant), list(L,constant), list(_2,constant), size(ub,_1,length(_1)), size(ub,L,length(L)), size(ub,_2,length(L)+length(_1)) )
         + ( cost(ub,res_steps,length(_1)+1), cost(ub,steps,length(_1)+1) ).

:- true pred append(_1,L,_2)
         : ( list(_1,constant), list(L,constant), var(_2) )
        => ( list(_1,constant), list(L,constant), list(_2,constant), size_lb(_1,length(_1)), size_lb(L,length(L)), size_lb(_2,length(L)+length(_1)), size_ub(_1,length(_1)), size_ub(L,length(L)), size_ub(_2,length(L)+length(_1)) )
         + ( steps_lb(length(_1)+1), steps_ub(length(_1)+1) ).

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
         + cost(lb,steps,0.5*exp(size(X)-1,2)+2*size(X)).

:- true pred flatten(X,Ys)
         : ( gnd(X), var(Ys) )
        => ( rt22(X), list(Ys,constant), size(ub,X,size(X)), size(ub,Ys,size(X)+1) )
         + ( cost(ub,res_steps,0.5*exp(size(X)-1,2)+3*size(X)+1), cost(ub,steps,0.5*exp(size(X)-1,2)+3*size(X)+1) ).

:- true pred flatten(X,Ys)
         : ( gnd(X), var(Ys) )
        => ( rt22(X), list(Ys,constant), size_lb(X,size(X)), size_lb(Ys,size(X)), size_ub(X,size(X)), size_ub(Ys,size(X)+1) )
         + ( steps_lb(0.5*exp(size(X)-1,2)+2*size(X)), steps_ub(0.5*exp(size(X)-1,2)+3*size(X)+1) ).

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


