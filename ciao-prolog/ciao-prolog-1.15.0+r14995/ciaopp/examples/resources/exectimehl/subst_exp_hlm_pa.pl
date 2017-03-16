:- module(_1,[substitute/3],[assertions,regtypes,ciaopp(examples(resources(exectimehl))),nativeprops,basicmodes]).

:- export(replacement/1).

:- prop replacement/1+regtype.

:- prop replacement(_1)
         + regtype.

:- true pred replacement(_1)
         : term(_1)
        => rt0(_1).

:- true pred replacement(_1)
         : mshare([[_1]])
        => mshare([[_1]]).

:- true pred replacement(_1)
         : term(_1)
        => rt0(_1)
         + ( possibly_fails, not_covered ).

replacement(A=B) :-
        arithexpression(A),
        arithexpression(B).

:- entry substitute(X,Y,Z)
         : ( arithexpression(X), list(Y,replacement), var(Z) ).

:- true pred substitute(X,Y,Z)
         : ( arithexpression(X), list(Y,replacement), term(Z) )
        => ( arithexpression(X), list(Y,replacement), arithexpression(Z) ).

:- true pred substitute(X,Y,Z)
         : ( mshare([[Z]]), var(Z), ground([X,Y]) )
        => ground([X,Y,Z]).

:- true pred substitute(X,Y,Z)
         : ( arithexpression(X), list(Y,replacement), var(Z) )
        => ( arithexpression(X), list(Y,replacement), arithexpression(Z) )
         + ( not_fails, covered ).

:- true pred substitute(X,Y,Z)
         : ( arithexpression(X), list(Y,replacement), var(Z) )
        => ( arithexpression(X), list(Y,replacement), arithexpression(Z), size(lb,X,size(X)), size(lb,Y,length(Y)), size(lb,Z,0) )
         + cost(lb,exectime_model4,541.558024186782*size(X)).

:- true pred substitute(X,Y,Z)
         : ( arithexpression(X), list(Y,replacement), var(Z) )
        => ( arithexpression(X), list(Y,replacement), arithexpression(Z), size(ub,X,size(X)), size(ub,Y,length(Y)), size(ub,Z,bot) )
         + cost(ub,exectime_model4,1066.554527798208*(length(Y)*size(X))+1066.554527798208*length(Y)+951.5997745260136*size(X)+374.123201854641).

substitute(A+B,Subs,NewA+NewB) :-
        !,
        substitute(A,Subs,NewA),
        substitute(B,Subs,NewB).
substitute(A*B,Subs,NewA*NewB) :-
        !,
        substitute(A,Subs,NewA),
        substitute(B,Subs,NewB).
substitute(A-B,Subs,NewA-NewB) :-
        !,
        substitute(A,Subs,NewA),
        substitute(B,Subs,NewB).
substitute(A=B,Subs,NewA=NewB) :-
        !,
        substitute(A,Subs,NewA),
        substitute(B,Subs,NewB).
substitute(exp(A,B),Subs,exp(NewA,B)) :-
        integer(B),
        !,
        substitute(A,Subs,NewA).
substitute(A,Subs,B) :-
        find_replacement(A,Subs,B),
        !.
substitute(A,_1,A).

:- true pred find_replacement(A,_1,B)
         : ( arithexpression(A), list(_1,replacement), term(B) )
        => ( arithexpression(A), rt1(_1), arithexpression(B) ).

:- true pred find_replacement(A,_1,B)
         : ( mshare([[B]]), var(B), ground([A,_1]) )
        => ground([A,_1,B]).

:- true pred find_replacement(A,_1,B)
         : ( arithexpression(A), list(_1,replacement), var(B) )
        => ( arithexpression(A), rt1(_1), arithexpression(B) )
         + ( possibly_fails, not_covered ).

:- true pred find_replacement(A,_1,B)
         : ( arithexpression(A), list(_1,replacement), var(B) )
        => ( arithexpression(A), rt1(_1), arithexpression(B), size(lb,A,size(A)), size(lb,_1,length(_1)), size(lb,B,0) )
         + cost(lb,exectime_model4,0).

:- true pred find_replacement(A,_1,B)
         : ( arithexpression(A), list(_1,replacement), var(B) )
        => ( arithexpression(A), rt1(_1), arithexpression(B), size(ub,A,size(A)), size(ub,_1,length(_1)), size(ub,B,bot) )
         + cost(ub,exectime_model4,1066.554527798208*length(_1)).

find_replacement(A,[A=B|_1],B).
find_replacement(A,[_1|Ys],B) :-
        find_replacement(A,Ys,B).


:- regtype rt1/1.

rt1([A=B|C]) :-
        arithexpression(A),
        arithexpression(B),
        list(C,replacement).


:- regtype rt0/1.

rt0(A=B) :-
        term(A),
        term(B).


