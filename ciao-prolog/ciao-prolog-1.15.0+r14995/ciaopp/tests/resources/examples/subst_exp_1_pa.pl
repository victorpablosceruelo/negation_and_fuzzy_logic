:- module(_1,[substitute/3],[assertions,regtypes,nativeprops,ciaopp(tests(resources)),predefres(res_all),basicmodes]).

:- doc(author,"Edison Mera").

:- resource res_steps.

:- head_cost(ub,res_steps,1).

:- literal_cost(ub,res_steps,0).

:- head_cost(lb,res_steps,1).

:- literal_cost(lb,res_steps,0).

:- trust_default+cost(ub,res_steps,0).

:- trust_default+cost(lb,res_steps,0).

:- prop replacement/1+regtype.

:- prop replacement(_1)
         + regtype.

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
         + ( cost(lb,giunif,0), cost(lb,gounif,0), cost(lb,nargs,3*size(X)), cost(lb,res_steps,size(X)), cost(lb,steps,size(X)), cost(lb,viunif,3*size(X)), cost(lb,vounif,2*size(X)) ).

:- true pred substitute(X,Y,Z)
         : ( arithexpression(X), list(Y,replacement), var(Z) )
        => ( arithexpression(X), list(Y,replacement), arithexpression(Z), size(ub,X,size(X)), size(ub,Y,length(Y)), size(ub,Z,bot) )
         + ( cost(ub,giunif,3*(length(Y)*size(X))+3*length(Y)+size(X)), cost(ub,gounif,size(X)), cost(ub,nargs,6*(length(Y)*size(X))+6*length(Y)+3*size(X)+3), cost(ub,res_steps,2*(length(Y)*size(X))+2*length(Y)+size(X)+1), cost(ub,steps,2*(length(Y)*size(X))+2*length(Y)+size(X)+1), cost(ub,viunif,7*(length(Y)*size(X))+7*length(Y)+5*size(X)+2), cost(ub,vounif,2*(length(Y)*size(X))+2*length(Y)+3*size(X)+1) ).

:- true pred substitute(X,Y,Z)
         : ( arithexpression(X), list(Y,replacement), var(Z) )
        => ( arithexpression(X), list(Y,replacement), arithexpression(Z), size_lb(X,size(X)), size_lb(Y,length(Y)), size_lb(Z,0), size_ub(X,size(X)), size_ub(Y,length(Y)), size_ub(Z,inf) )
         + ( steps_lb(size(X)), steps_ub(2*(length(Y)*size(X))+2*length(Y)+size(X)+1) ).

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
        integer(B),
        !,
        substitute(A,Subs,NewA).
substitute(A,Subs,B) :-
        find_replacement(A,Subs,B),
        !.
substitute(A,_1,A).

:- true pred find_replacement(A,_1,B)
         : ( arithexpression(A), list(_1,replacement), term(B) )
        => ( arithexpression(A), rt0(_1), arithexpression(B) ).

:- true pred find_replacement(A,_1,B)
         : ( mshare([[B]]), var(B), ground([A,_1]) )
        => ground([A,_1,B]).

:- true pred find_replacement(A,_1,B)
         : ( arithexpression(A), list(_1,replacement), var(B) )
        => ( arithexpression(A), rt0(_1), arithexpression(B) )
         + ( possibly_fails, not_covered ).

:- true pred find_replacement(A,_1,B)
         : ( arithexpression(A), list(_1,replacement), var(B) )
        => ( arithexpression(A), rt0(_1), arithexpression(B), size(lb,A,size(A)), size(lb,_1,length(_1)), size(lb,B,0) )
         + ( cost(lb,giunif,0), cost(lb,gounif,0), cost(lb,nargs,0), cost(lb,res_steps,0), cost(lb,steps,0), cost(lb,viunif,0), cost(lb,vounif,0) ).

:- true pred find_replacement(A,_1,B)
         : ( arithexpression(A), list(_1,replacement), var(B) )
        => ( arithexpression(A), rt0(_1), arithexpression(B), size(ub,A,size(A)), size(ub,_1,length(_1)), size(ub,B,bot) )
         + ( cost(ub,giunif,3*length(_1)), cost(ub,gounif,0), cost(ub,nargs,6*length(_1)), cost(ub,res_steps,2*length(_1)), cost(ub,steps,2*length(_1)), cost(ub,viunif,7*length(_1)), cost(ub,vounif,2*length(_1)) ).

:- true pred find_replacement(A,_1,B)
         : ( arithexpression(A), list(_1,replacement), var(B) )
        => ( arithexpression(A), rt0(_1), arithexpression(B), size_lb(A,size(A)), size_lb(_1,length(_1)), size_lb(B,0), size_ub(A,size(A)), size_ub(_1,length(_1)), size_ub(B,inf) )
         + ( steps_lb(0), steps_ub(2*length(_1)) ).

find_replacement(A,[A=B|_1],B).
find_replacement(A,[_1|Ys],B) :-
        find_replacement(A,Ys,B).


:- regtype rt0/1.

rt0([A=B|C]) :-
        arithexpression(A),
        arithexpression(B),
        list(C,replacement).


