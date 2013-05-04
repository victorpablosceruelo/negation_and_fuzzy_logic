:- module(_1,[evalpol/3],[assertions,regtypes,nativeprops,ciaopp(examples(resources(exectimehl))),basicmodes]).

:- export(int_list/1).

:- prop int_list/1+regtype.

:- prop int_list(_1)
         + regtype.

:- true pred int_list(_1)
         : term(_1)
        => list(_1,character_code).

:- true pred int_list(_1)
         : mshare([[_1]])
        => ground([_1]).

:- true pred int_list(_1)
         : term(_1)
        => list(_1,character_code)
         + ( possibly_fails, not_covered ).

int_list([]).
int_list([X|L]) :-
        int(X),
        int_list(L).

:- entry evalpol(C,X,R)
         : ( int_list(C), int(X), var(R) ).

:- true pred evalpol(C,X,R)
         : ( int_list(C), int(X), term(R) )
        => ( list(C,character_code), int(X), num(R) ).

:- true pred evalpol(C,X,R)
         : ( mshare([[R]]), var(R), ground([C,X]) )
        => ground([C,X,R]).

:- true pred evalpol(C,X,R)
         : ( int_list(C), int(X), var(R) )
        => ( list(C,character_code), int(X), num(R) )
         + ( not_fails, covered ).

:- true pred evalpol(C,X,R)
         : ( int_list(C), int(X), var(R) )
        => ( list(C,character_code), int(X), num(R), size(lb,C,length(C)), size(lb,X,int(X)), size(lb,R,0) )
         + cost(lb,exectime_model4,850.3278722114378*length(C)+408.978205425055).

:- true pred evalpol(C,X,R)
         : ( int_list(C), int(X), var(R) )
        => ( list(C,character_code), int(X), num(R), size(ub,C,length(C)), size(ub,X,int(X)), size(ub,R,bot) )
         + cost(ub,exectime_model4,887.7081924967438*length(C)+439.427029583319).

evalpol([],_X,0).
evalpol([C|L],X,R) :-
        evalpol(L,X,R0),
        R is R0*X+C.


