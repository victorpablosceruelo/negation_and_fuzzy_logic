:- module(_1,[intersect/3],[assertions,nativeprops,ciaopp(tests(resources)),predefres(res_all),basicmodes,regtypes]).

:- doc(author,"Edison Mera").

:- resource res_steps.

:- head_cost(ub,res_steps,1).

:- head_cost(lb,res_steps,1).

:- literal_cost(ub,res_steps,0).

:- literal_cost(lb,res_steps,0).

:- trust_default+cost(ub,res_steps,0).

:- trust_default+cost(lb,res_steps,0).

:- entry intersect(_1,_2,_3)
         : ( list(_1,gnd), list(_2,gnd), var(_3) ).

:- true pred intersect(_1,_L,L3)
         : ( list(_1,gnd), list(_L,gnd), term(L3) )
        => ( list(_1,gnd), list(_L,gnd), list(L3,gnd) ).

:- true pred intersect(_1,_L,L3)
         : ( mshare([[L3]]), var(L3), ground([_1,_L]) )
        => ground([_1,_L,L3]).

:- true pred intersect(_1,_L,L3)
         : ( list(_1,gnd), list(_L,gnd), var(L3) )
        => ( list(_1,gnd), list(_L,gnd), list(L3,gnd) )
         + ( not_fails, covered ).

:- true pred intersect(_1,_L,L3)
         : ( list(_1,gnd), list(_L,gnd), var(L3) )
        => ( list(_1,gnd), list(_L,gnd), list(L3,gnd), size(lb,_1,length(_1)), size(lb,_L,length(_L)), size(lb,L3,0) )
         + ( cost(lb,giunif,length(_1)+1), cost(lb,gounif,1), cost(lb,nargs,3*length(_1)+3), cost(lb,res_steps,length(_1)+1), cost(lb,steps,length(_1)+1), cost(lb,viunif,3*length(_1)+1), cost(lb,vounif,length(_1)) ).

:- true pred intersect(_1,_L,L3)
         : ( list(_1,gnd), list(_L,gnd), var(L3) )
        => ( list(_1,gnd), list(_L,gnd), list(L3,gnd), size(ub,_1,length(_1)), size(ub,_L,length(_L)), size(ub,L3,length(_1)) )
         + ( cost(ub,giunif,length(_L)*length(_1)+2*length(_1)+1), cost(ub,gounif,length(_1)+1), cost(ub,nargs,2*(length(_L)*length(_1))+5*length(_1)+3), cost(ub,res_steps,length(_L)*length(_1)+2*length(_1)+1), cost(ub,steps,length(_L)*length(_1)+2*length(_1)+1), cost(ub,viunif,3*(length(_L)*length(_1))+6*length(_1)+1), cost(ub,vounif,2*length(_1)) ).

:- true pred intersect(_1,_L,L3)
         : ( list(_1,gnd), list(_L,gnd), var(L3) )
        => ( list(_1,gnd), list(_L,gnd), list(L3,gnd), size_lb(_1,length(_1)), size_lb(_L,length(_L)), size_lb(L3,0), size_ub(_1,length(_1)), size_ub(_L,length(_L)), size_ub(L3,length(_1)) )
         + ( steps_lb(length(_1)+1), steps_ub(length(_L)*length(_1)+2*length(_1)+1) ).

intersect([],_L,[]).
intersect([H|L1],L2,[H|L3]) :-
        memberchk(H,L2),
        !,
        intersect(L1,L2,L3).
intersect([_H|L1],L2,L3) :-
        intersect(L1,L2,L3).

:- true pred memberchk(X,_1)
         : ( gnd(X), list(_1,gnd) )
        => ( gnd(X), rt2(_1) ).

:- true pred memberchk(X,_1)
         : ground([X,_1])
        => ground([X,_1]).

:- true pred memberchk(X,_1)
         : ( gnd(X), list(_1,gnd) )
        => ( gnd(X), rt2(_1) )
         + ( possibly_fails, not_covered ).

:- true pred memberchk(X,_1)
         : ( gnd(X), list(_1,gnd) )
        => ( gnd(X), rt2(_1), size(lb,X,size(X)), size(lb,_1,length(_1)) )
         + ( cost(lb,giunif,0), cost(lb,gounif,0), cost(lb,nargs,0), cost(lb,res_steps,0), cost(lb,steps,0), cost(lb,viunif,0), cost(lb,vounif,0) ).

:- true pred memberchk(X,_1)
         : ( gnd(X), list(_1,gnd) )
        => ( gnd(X), rt2(_1), size(ub,X,size(X)), size(ub,_1,length(_1)) )
         + ( cost(ub,giunif,length(_1)+1), cost(ub,gounif,0), cost(ub,nargs,2*length(_1)+2), cost(ub,res_steps,length(_1)+1), cost(ub,steps,length(_1)+1), cost(ub,viunif,3*length(_1)+3), cost(ub,vounif,0) ).

:- true pred memberchk(X,_1)
         : ( gnd(X), list(_1,gnd) )
        => ( gnd(X), rt2(_1), size_lb(X,size(X)), size_lb(_1,length(_1)), size_ub(X,size(X)), size_ub(_1,length(_1)) )
         + ( steps_lb(0), steps_ub(length(_1)+1) ).

memberchk(X,[X|_1]) :- !.
memberchk(X,[_1|L]) :-
        memberchk(X,L).


:- regtype rt2/1.

rt2([A|B]) :-
        gnd(A),
        list(B,gnd).


