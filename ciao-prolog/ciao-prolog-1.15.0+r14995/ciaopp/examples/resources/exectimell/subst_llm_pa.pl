:- module(_1,[substitute/3],[assertions,regtypes,ciaopp(examples(resources(exectimell))),nativeprops,basicmodes]).

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
         + ( cost(lb,exectime,18491.22*size(X)), cost(lb,exectime_me,18491.22*size(X)), cost(lb,wamcount,48*size(X)) ).

:- true pred substitute(X,Y,Z)
         : ( arithexpression(X), list(Y,replacement), var(Z) )
        => ( arithexpression(X), list(Y,replacement), arithexpression(Z), size(ub,X,size(X)), size(ub,Y,length(Y)), size(ub,Z,bot) )
         + ( cost(ub,exectime,16765.176*(length(Y)*size(X))+16765.176*length(Y)+57966.3*size(X)+38769.336), cost(ub,exectime_me,16765.176*(length(Y)*size(X))+16765.176*length(Y)+57966.3*size(X)+38769.336), cost(ub,wamcount,44*(length(Y)*size(X))+44*length(Y)+146*size(X)+97) ).

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
substitute(A**B,Subs,NewA**B) :-
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
         + ( cost(lb,exectime,0), cost(lb,exectime_me,0), cost(lb,wamcount,0) ).

:- true pred find_replacement(A,_1,B)
         : ( arithexpression(A), list(_1,replacement), var(B) )
        => ( arithexpression(A), rt1(_1), arithexpression(B), size(ub,A,size(A)), size(ub,_1,length(_1)), size(ub,B,bot) )
         + ( cost(ub,exectime,16234.236*length(_1)), cost(ub,exectime_me,16234.236*length(_1)), cost(ub,wamcount,41*length(_1)) ).

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


