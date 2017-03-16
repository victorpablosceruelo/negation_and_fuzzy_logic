:- module(_1,[p/1,testbuiltin/2],[assertions,ciaopp(tests(resources)),predefres(res_all),nativeprops,basicmodes,regtypes]).

:- entry p(_1)
         : var(_1).

:- true pred p(_1)
         : term(_1)
        => rt4(_1).

:- true pred p(_1)
         : ( mshare([[_1]]), var(_1) )
        => ground([_1]).

:- true pred p(_1)
         : var(_1)
        => rt4(_1)
         + ( not_fails, covered ).

:- true pred p(_1)
         : var(_1)
        => ( rt4(_1), size(lb,_1,1) )
         + ( cost(lb,giunif,0), cost(lb,gounif,1), cost(lb,nargs,1), cost(lb,steps,1), cost(lb,viunif,0), cost(lb,vounif,0) ).

:- true pred p(_1)
         : var(_1)
        => ( rt4(_1), size(ub,_1,1) )
         + ( cost(ub,giunif,0), cost(ub,gounif,3), cost(ub,nargs,3), cost(ub,steps,3), cost(ub,viunif,0), cost(ub,vounif,0) ).

:- true pred p(_1)
         : var(_1)
        => ( rt4(_1), size_lb(_1,1), size_ub(_1,1) )
         + ( steps_lb(1), steps_ub(3) ).

p(a).
p(b).
p(c).

:- entry testbuiltin(_1,_2)
         : ( list(_1,gnd), var(_2) ).

:- true pred testbuiltin(_1,_2)
         : ( list(_1,gnd), term(_2) )
        => ( list(_1,varnamepair), list(_2,varnamepair) ).

:- true pred testbuiltin(_1,_2)
         : ( mshare([[_2]]), var(_2), ground([_1]) )
        => ground([_1,_2]).

:- true pred testbuiltin(_1,_2)
         : ( list(_1,gnd), var(_2) )
        => ( list(_1,varnamepair), list(_2,varnamepair) )
         + ( possibly_fails, not_covered ).

:- true pred testbuiltin(_1,_2)
         : ( list(_1,gnd), var(_2) )
        => ( list(_1,varnamepair), list(_2,varnamepair), size(lb,_1,length(_1)), size(lb,_2,length(_1)) )
         + ( cost(lb,giunif,0), cost(lb,gounif,0), cost(lb,nargs,0), cost(lb,steps,0), cost(lb,viunif,0), cost(lb,vounif,0) ).

:- true pred testbuiltin(_1,_2)
         : ( list(_1,gnd), var(_2) )
        => ( list(_1,varnamepair), list(_2,varnamepair), size(ub,_1,length(_1)), size(ub,_2,length(_1)) )
         + ( cost(ub,giunif,25*length(_1)+1), cost(ub,gounif,2*length(_1)+1), cost(ub,nargs,2*length(_1)+2), cost(ub,steps,length(_1)+1), cost(ub,viunif,2*length(_1)), cost(ub,vounif,2*length(_1)) ).

:- true pred testbuiltin(_1,_2)
         : ( list(_1,gnd), var(_2) )
        => ( list(_1,varnamepair), list(_2,varnamepair), size_lb(_1,length(_1)), size_lb(_2,length(_1)), size_ub(_1,length(_1)), size_ub(_2,length(_1)) )
         + ( steps_lb(0), steps_ub(length(_1)+1) ).

testbuiltin([],[]).
testbuiltin([X|Xs],[Y|Ys]) :-
        X==a,
        X\==b,
        X is 2+1,
        X=:=1/2,
        X=\=1/2,
        X<1+2,
        X>1*2,
        X=<1/3,
        X>=3.0,
        atom(X),
        integer(X),
        number(X),
        atm(X),
        int(X),
        num(X),
        gnd(X),
        functor(a(b,c),Name,Arity),
        X=Name/Arity,
        Y=X,
        testbuiltin(Xs,Ys).


:- regtype rt4/1.

rt4(a).
rt4(b).
rt4(c).


