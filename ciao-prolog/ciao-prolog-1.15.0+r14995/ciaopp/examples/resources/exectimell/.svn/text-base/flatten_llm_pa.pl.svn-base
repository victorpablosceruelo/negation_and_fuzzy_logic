:- module(_1,[flatten/2],[assertions,regtypes,ciaopp(examples(resources(exectimell))),nativeprops,basicmodes]).

:- doc(author,"Edison Mera").

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
         + ( cost(lb,exectime,9930.623999999996*length(_1)+2819.484), cost(lb,exectime_me,9930.623999999996*length(_1)+2819.484), cost(lb,wamcount,26*length(_1)+7) ).

:- true pred append(_1,L,_2)
         : ( list(_1,constant), list(L,constant), var(_2) )
        => ( list(_1,constant), list(L,constant), list(_2,constant), size(ub,_1,length(_1)), size(ub,L,length(L)), size(ub,_2,length(L)+length(_1)) )
         + ( cost(ub,exectime,9930.623999999996*length(_1)+2819.484), cost(ub,exectime_me,9930.623999999996*length(_1)+2819.484), cost(ub,wamcount,26*length(_1)+7) ).

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
         + ( cost(lb,exectime,4965.311999999998*exp(size(X)-1,2)+14704.512*size(X)), cost(lb,exectime_me,4965.311999999998*exp(size(X)-1,2)+14704.512*size(X)), cost(lb,wamcount,13.0*exp(size(X)-1,2)+41*size(X)) ).

:- true pred flatten(X,Ys)
         : ( gnd(X), var(Ys) )
        => ( rt22(X), list(Ys,constant), size(ub,X,size(X)), size(ub,Ys,size(X)+1) )
         + ( cost(ub,exectime,4965.311999999998*exp(size(X)-1,2)+24635.13599999999*size(X)+5505.191999999999), cost(ub,exectime_me,4965.311999999998*exp(size(X)-1,2)+24635.13599999999*size(X)+5505.191999999999), cost(ub,wamcount,13.0*exp(size(X)-1,2)+67*size(X)+16) ).

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


