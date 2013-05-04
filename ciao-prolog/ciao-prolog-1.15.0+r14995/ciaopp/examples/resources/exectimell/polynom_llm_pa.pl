:- module(_1,[evalpol/3],[assertions,regtypes,ciaopp(examples(resources(exectimell))),nativeprops,basicmodes]).

:- entry evalpol(C,X,R)
         : ( list(C,int), int(X), var(R) ).

:- true pred evalpol(C,X,R)
         : ( list(C,int), int(X), term(R) )
        => ( list(C,int), int(X), num(R) ).

:- true pred evalpol(C,X,R)
         : ( mshare([[R]]), var(R), ground([C,X]) )
        => ground([C,X,R]).

:- true pred evalpol(C,X,R)
         : ( list(C,int), int(X), var(R) )
        => ( list(C,int), int(X), num(R) )
         + ( not_fails, covered ).

:- true pred evalpol(C,X,R)
         : ( list(C,int), int(X), var(R) )
        => ( list(C,int), int(X), num(R), size(lb,C,length(C)), size(lb,X,int(X)), size(lb,R,0) )
         + ( cost(lb,exectime,9598.188*length(C)+2646.372), cost(lb,exectime_me,9598.188*length(C)+2646.372), cost(lb,wamcount,25*length(C)+7) ).

:- true pred evalpol(C,X,R)
         : ( list(C,int), int(X), var(R) )
        => ( list(C,int), int(X), num(R), size(ub,C,length(C)), size(ub,X,int(X)), size(ub,R,bot) )
         + ( cost(ub,exectime,9598.188*length(C)+2646.372), cost(ub,exectime_me,9598.188*length(C)+2646.372), cost(ub,wamcount,25*length(C)+7) ).

evalpol([],_X,0).
evalpol([C|L],X,R) :-
        evalpol(L,X,R0),
        R1 is R0*X,
        R is R1+C.


