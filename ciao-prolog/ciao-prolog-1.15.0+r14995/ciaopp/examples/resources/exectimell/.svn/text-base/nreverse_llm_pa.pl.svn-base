:- module(_1,[nreverse/2],[assertions,regtypes,ciaopp(examples(resources(exectimell))),nativeprops,basicmodes]).

:- true pred append(_1,X,_2)
         : ( list(_1,num), rt2(X), term(_2) )
        => ( list(_1,num), rt19(X), rt19(_2) ).

:- true pred append(_1,X,_2)
         : ( mshare([[_2]]), var(_2), ground([_1,X]) )
        => ground([_1,X,_2]).

:- true pred append(_1,X,_2)
         : ( list(_1,num), rt2(X), var(_2) )
        => ( list(_1,num), rt19(X), rt19(_2) )
         + ( not_fails, covered ).

:- true pred append(_1,X,_2)
         : ( list(_1,num), rt2(X), var(_2) )
        => ( list(_1,num), rt19(X), rt19(_2), size(lb,_1,length(_1)), size(lb,X,length(X)), size(lb,_2,length(X)+length(_1)) )
         + ( cost(lb,exectime,9930.623999999996*length(_1)+2819.484), cost(lb,exectime_me,9930.623999999996*length(_1)+2819.484), cost(lb,wamcount,26*length(_1)+7) ).

:- true pred append(_1,X,_2)
         : ( list(_1,num), rt2(X), var(_2) )
        => ( list(_1,num), rt19(X), rt19(_2), size(ub,_1,length(_1)), size(ub,X,length(X)), size(ub,_2,length(X)+length(_1)) )
         + ( cost(ub,exectime,9930.623999999996*length(_1)+2819.484), cost(ub,exectime_me,9930.623999999996*length(_1)+2819.484), cost(ub,wamcount,26*length(_1)+7) ).

append([],X,X).
append([X|Xs],Y,[X|Zs]) :-
        append(Xs,Y,Zs).

:- entry nreverse(Xs,Ys)
         : ( var(Ys), list(Xs,num) ).

:- true pred nreverse(Xs,Ys)
         : ( list(Xs,num), term(Ys) )
        => ( list(Xs,num), list(Ys,num) ).

:- true pred nreverse(Xs,Ys)
         : ( mshare([[Ys]]), var(Ys), ground([Xs]) )
        => ground([Xs,Ys]).

:- true pred nreverse(Xs,Ys)
         : ( list(Xs,num), var(Ys) )
        => ( list(Xs,num), list(Ys,num) )
         + ( not_fails, covered ).

:- true pred nreverse(Xs,Ys)
         : ( list(Xs,num), var(Ys) )
        => ( list(Xs,num), list(Ys,num), size(lb,Xs,length(Xs)), size(lb,Ys,length(Xs)) )
         + ( cost(lb,exectime,4965.311999999998*exp(length(Xs),2)+6474.840000000003*length(Xs)+1592.736), cost(lb,exectime_me,4965.311999999998*exp(length(Xs),2)+6474.840000000003*length(Xs)+1592.736), cost(lb,wamcount,13.0*exp(length(Xs),2)+19.0*length(Xs)+4) ).

:- true pred nreverse(Xs,Ys)
         : ( list(Xs,num), var(Ys) )
        => ( list(Xs,num), list(Ys,num), size(ub,Xs,length(Xs)), size(ub,Ys,length(Xs)) )
         + ( cost(ub,exectime,4965.311999999998*exp(length(Xs),2)+6474.840000000003*length(Xs)+1592.736), cost(ub,exectime_me,4965.311999999998*exp(length(Xs),2)+6474.840000000003*length(Xs)+1592.736), cost(ub,wamcount,13.0*exp(length(Xs),2)+19.0*length(Xs)+4) ).

nreverse([],[]).
nreverse([X|Xs],Y) :-
        nreverse(Xs,Y0),
        append(Y0,[X],Y).


:- regtype rt2/1.

rt2([A]) :-
        num(A).


:- regtype rt19/1.

rt19([A|B]) :-
        num(A),
        list(B,num).


