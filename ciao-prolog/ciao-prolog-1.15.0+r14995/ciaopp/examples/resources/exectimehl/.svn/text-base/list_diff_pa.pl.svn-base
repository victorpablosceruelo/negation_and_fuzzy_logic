:- module(_1,[diff/3],[assertions,ciaopp(examples(resources(exectimehl))),nativeprops,basicmodes,regtypes]).

:- entry diff(_1,_2,_3)
         : ( list(_1,num), list(_2,num), var(_3) ).

:- true pred diff(_1,_L,L3)
         : ( list(_1,num), list(_L,num), term(L3) )
        => ( list(_1,num), list(_L,num), list(L3,num) ).

:- true pred diff(_1,_L,L3)
         : ( mshare([[L3]]), var(L3), ground([_1,_L]) )
        => ground([_1,_L,L3]).

:- true pred diff(_1,_L,L3)
         : ( list(_1,num), list(_L,num), var(L3) )
        => ( list(_1,num), list(_L,num), list(L3,num) )
         + ( not_fails, covered ).

:- true pred diff(_1,_L,L3)
         : ( list(_1,num), list(_L,num), var(L3) )
        => ( list(_1,num), list(_L,num), list(L3,num), size(lb,_1,length(_1)), size(lb,_L,length(_L)), size(lb,L3,0) )
         + cost(lb,exectime_model4,447.5208755545427*length(_1)+408.978205425055).

:- true pred diff(_1,_L,L3)
         : ( list(_1,num), list(_L,num), var(L3) )
        => ( list(_1,num), list(_L,num), list(L3,num), size(ub,_1,length(_1)), size(ub,_L,length(_L)), size(ub,L3,length(_1)) )
         + cost(ub,exectime_model4,421.8789561504807*(length(_L)*length(_1))+902.1048660347636*length(_1)+439.427029583319).

diff([],_L,[]).
diff([H|L1],L2,L3) :-
        memberchk(H,L2),
        !,
        diff(L1,L2,L3).
diff([H|L1],L2,[H|L3]) :-
        diff(L1,L2,L3).

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
         + cost(lb,exectime_model4,0).

:- true pred memberchk(X,_1)
         : ( num(X), list(_1,num) )
        => ( num(X), rt2(_1), size(ub,X,int(X)), size(ub,_1,length(_1)) )
         + cost(ub,exectime_model4,421.8789561504807*length(_1)+421.8789561504807).

memberchk(X,[X|_1]) :- !.
memberchk(X,[_1|L]) :-
        memberchk(X,L).


:- regtype rt2/1.

rt2([A|B]) :-
        num(A),
        list(B,num).


