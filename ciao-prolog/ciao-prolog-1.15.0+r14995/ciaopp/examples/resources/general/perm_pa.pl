:- module(_1,[perm/2],[assertions,regtypes,nativeprops,ciaopp(tests(resources)),predefres(sta_exectime),predefres(res_wamcount),basicmodes]).

:- entry perm(_1,_2)
         : ( list(_1,gnd), var(_2) ).

:- true pred perm(_1,_2)
         : ( list(_1,gnd), term(_2) )
        => ( list(_1,gnd), list(_2,gnd) ).

:- true pred perm(_1,_2)
         : ( mshare([[_2]]), var(_2), ground([_1]) )
        => ground([_1,_2]).

:- true pred perm(_1,_2)
         : ( list(_1,gnd), var(_2) )
        => ( list(_1,gnd), list(_2,gnd) )
         + ( not_fails, covered ).

:- true pred perm(_1,_2)
         : ( list(_1,gnd), var(_2) )
        => ( list(_1,gnd), list(_2,gnd), size(lb,_1,length(_1)), size(lb,_2,length(_1)) )
         + ( cost(lb,exectime,11636.916*length(_1)+1592.736), cost(lb,wamcount,33*length(_1)+4) ).

:- true pred perm(_1,_2)
         : ( list(_1,gnd), var(_2) )
        => ( list(_1,gnd), list(_2,gnd), size(ub,_1,length(_1)), size(ub,_2,length(_1)) )
         + ( cost(ub,exectime,sum($(j),1,length(_1),17540.304*(exp(fact($(j)),-1)*fact(length(_1))* $(j)))+sum($(j),1,length(_1),11450.952*(exp(fact($(j)),-1)*fact(length(_1))))+1592.736*fact(length(_1))), cost(ub,wamcount,sum($(j),1,length(_1),46*(exp(fact($(j)),-1)*fact(length(_1))* $(j)))+sum($(j),1,length(_1),32*(exp(fact($(j)),-1)*fact(length(_1))))+4.0*fact(length(_1))) ).

perm([],[]).
perm([X|Xs],[R|Rs]) :-
        select(R,[X|Xs],Y),
        perm(Y,Rs).

:- true pred select(X,_1,Xs)
         : ( term(X), list(_1,gnd), term(Xs) )
        => ( gnd(X), rt2(_1), list(Xs,gnd) ).

:- true pred select(X,_1,Xs)
         : ( mshare([[X],[Xs]]), var(X), var(Xs), ground([_1]) )
        => ground([X,_1,Xs]).

:- true pred select(X,_1,Xs)
         : ( var(X), list(_1,gnd), var(Xs) )
        => ( gnd(X), rt2(_1), list(Xs,gnd) )
         + ( possibly_fails, not_covered ).

:- true pred select(X,_1,Xs)
         : ( var(X), list(_1,gnd), var(Xs) )
        => ( gnd(X), rt2(_1), list(Xs,gnd), size(lb,X,0), size(lb,_1,length(_1)), size(lb,Xs,length(_1)-1) )
         + ( cost(lb,exectime,0), cost(lb,wamcount,0) ).

:- true pred select(X,_1,Xs)
         : ( var(X), list(_1,gnd), var(Xs) )
        => ( gnd(X), rt2(_1), list(Xs,gnd), size(ub,X,bot), size(ub,_1,length(_1)), size(ub,Xs,length(_1)-1) )
         + ( cost(ub,exectime,17354.34*length(_1)), cost(ub,wamcount,45*length(_1)) ).

select(X,[X|Xs],Xs).
select(X,[Y|Ys],[Y|Zs]) :-
        select(X,Ys,Zs).


:- regtype rt2/1.

rt2([A|B]) :-
        gnd(A),
        list(B,gnd).


