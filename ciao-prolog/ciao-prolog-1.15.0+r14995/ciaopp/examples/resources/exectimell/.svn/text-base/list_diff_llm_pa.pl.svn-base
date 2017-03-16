:- module(_1,[list_diff/3],[assertions,regtypes,ciaopp(examples(resources(exectimell))),nativeprops,basicmodes]).

:- entry list_diff(_1,_2,_3)
         : ( list(_1,num), list(_2,num), var(_3) ).

:- true pred list_diff(_1,_L,L3)
         : ( list(_1,num), list(_L,num), term(L3) )
        => ( list(_1,num), list(_L,num), list(L3,num) ).

:- true pred list_diff(_1,_L,L3)
         : ( mshare([[L3]]), var(L3), ground([_1,_L]) )
        => ground([_1,_L,L3]).

:- true pred list_diff(_1,_L,L3)
         : ( list(_1,num), list(_L,num), var(L3) )
        => ( list(_1,num), list(_L,num), list(L3,num) )
         + ( not_fails, covered ).

:- true pred list_diff(_1,_L,L3)
         : ( list(_1,num), list(_L,num), var(L3) )
        => ( list(_1,num), list(_L,num), list(L3,num), size(lb,_1,length(_1)), size(lb,_L,length(_L)), size(lb,L3,0) )
         + ( cost(lb,exectime,8969.004*length(_1)+2702.688), cost(lb,exectime_me,8969.004*length(_1)+2702.688), cost(lb,wamcount,25*length(_1)+7) ).

:- true pred list_diff(_1,_L,L3)
         : ( list(_1,num), list(_L,num), var(L3) )
        => ( list(_1,num), list(_L,num), list(L3,num), size(ub,_1,length(_1)), size(ub,_L,length(_L)), size(ub,L3,length(_1)) )
         + ( cost(ub,exectime,9028.967999999997*(length(_L)*length(_1))+23599.5*length(_1)+5405.376), cost(ub,exectime_me,9028.967999999997*(length(_L)*length(_1))+23599.5*length(_1)+5405.376), cost(ub,wamcount,24*(length(_L)*length(_1))+64*length(_1)+14) ).

list_diff([],_L,[]).
list_diff([H|L1],L2,L3) :-
        memberchk(H,L2),
        !,
        list_diff(L1,L2,L3).
list_diff([H|L1],L2,[H|L3]) :-
        list_diff(L1,L2,L3).

:- true pred memberchk(X,_1)
         : ( num(X), list(_1,num) )
        => ( num(X), rt2(_1) ).

:- true pred memberchk(X,_1)
         : ground([X,_1])
        => ground([X,_1]).

:- true pred memberchk(X,_1)
         : ( num(X), list(_1,num) )
        => ( num(X), rt2(_1) )
         + ( possibly_fails, not_covered ).

:- true pred memberchk(X,_1)
         : ( num(X), list(_1,num) )
        => ( num(X), rt2(_1), size(lb,X,int(X)), size(lb,_1,length(_1)) )
         + ( cost(lb,exectime,0), cost(lb,exectime_me,0), cost(lb,wamcount,0) ).

:- true pred memberchk(X,_1)
         : ( num(X), list(_1,num) )
        => ( num(X), rt2(_1), size(ub,X,int(X)), size(ub,_1,length(_1)) )
         + ( cost(ub,exectime,9028.967999999997*length(_1)+1424.040000000005), cost(ub,exectime_me,9028.967999999997*length(_1)+1424.040000000005), cost(ub,wamcount,24*length(_1)+4) ).

memberchk(X,[X|_1]) :- !.
memberchk(X,[_1|L]) :-
        memberchk(X,L).


:- regtype rt2/1.

rt2([A|B]) :-
        num(A),
        list(B,num).


