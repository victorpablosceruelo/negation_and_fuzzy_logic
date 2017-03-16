:- module(_1,[iqueen/5],[assertions,nativeprops,ciaopp(tests(resources)),predefres(res_all),basicmodes,regtypes]).

:- doc(author,"Nai-Wei Lin").

:- doc(author,"Edison Mera").

:- doc(module,"This program plays the inverse n-queens game.").

:- resource res_steps.

:- head_cost(ub,res_steps,1).

:- head_cost(lb,res_steps,1).

:- literal_cost(ub,res_steps,0).

:- literal_cost(lb,res_steps,0).

:- trust_default+cost(ub,res_steps,0).

:- trust_default+cost(lb,res_steps,0).

:- entry iqueen(_1,_2,_3,_4,_5)
         : ( int(_1), int(_2), int(_3), int(_4), int(_5) ).

:- true pred iqueen(X1,X2,X3,X4,X5)
         : ( int(X1), int(X2), int(X3), int(X4), int(X5) )
        => ( int(X1), int(X2), int(X3), int(X4), character_code(X5) ).

:- true pred iqueen(X1,X2,X3,X4,X5)
         : ground([X1,X2,X3,X4,X5])
        => ground([X1,X2,X3,X4,X5]).

:- true pred iqueen(X1,X2,X3,X4,X5)
         : ( int(X1), int(X2), int(X3), int(X4), int(X5) )
        => ( int(X1), int(X2), int(X3), int(X4), character_code(X5) )
         + ( possibly_fails, covered ).

:- true pred iqueen(X1,X2,X3,X4,X5)
         : ( int(X1), int(X2), int(X3), int(X4), int(X5) )
        => ( int(X1), int(X2), int(X3), int(X4), character_code(X5), size(lb,X1,int(X1)), size(lb,X2,int(X2)), size(lb,X3,int(X3)), size(lb,X4,int(X4)), size(lb,X5,int(X5)) )
         + ( cost(lb,giunif,0), cost(lb,gounif,0), cost(lb,nargs,5), cost(lb,res_steps,1), cost(lb,steps,1), cost(lb,viunif,5), cost(lb,vounif,0) ).

:- true pred iqueen(X1,X2,X3,X4,X5)
         : ( int(X1), int(X2), int(X3), int(X4), int(X5) )
        => ( int(X1), int(X2), int(X3), int(X4), character_code(X5), size(ub,X1,int(X1)), size(ub,X2,int(X2)), size(ub,X3,int(X3)), size(ub,X4,int(X4)), size(ub,X5,int(X5)) )
         + ( cost(ub,giunif,72.0), cost(ub,gounif,0), cost(ub,nargs,86.0), cost(ub,res_steps,32.0), cost(ub,steps,32.0), cost(ub,viunif,95.0), cost(ub,vounif,0) ).

:- true pred iqueen(X1,X2,X3,X4,X5)
         : ( int(X1), int(X2), int(X3), int(X4), int(X5) )
        => ( int(X1), int(X2), int(X3), int(X4), character_code(X5), size_lb(X1,int(X1)), size_lb(X2,int(X2)), size_lb(X3,int(X3)), size_lb(X4,int(X4)), size_lb(X5,int(X5)), size_ub(X1,int(X1)), size_ub(X2,int(X2)), size_ub(X3,int(X3)), size_ub(X4,int(X4)), size_ub(X5,int(X5)) )
         + ( steps_lb(1), steps_ub(32.0) ).

iqueen(X1,X2,X3,X4,X5) :-
        safe([X1,X2,X3,X4,X5]).

:- true pred safe(_1)
         : list(_1,character_code)
        => list(_1,character_code).

:- true pred safe(_1)
         : ground([_1])
        => ground([_1]).

:- true pred safe(_1)
         : list(_1,character_code)
        => list(_1,character_code)
         + ( possibly_fails, covered ).

:- true pred safe(_1)
         : list(_1,character_code)
        => ( list(_1,character_code), size(lb,_1,length(_1)) )
         + ( cost(lb,giunif,1), cost(lb,gounif,0), cost(lb,nargs,1), cost(lb,res_steps,1), cost(lb,steps,1), cost(lb,viunif,0), cost(lb,vounif,0) ).

:- true pred safe(_1)
         : list(_1,character_code)
        => ( list(_1,character_code), size(ub,_1,length(_1)) )
         + ( cost(ub,giunif,2.5*exp(length(_1),2)+0.5*length(_1)+1), cost(ub,gounif,0), cost(ub,nargs,3.0*exp(length(_1),2)+length(_1)+1), cost(ub,res_steps,exp(length(_1),2)+length(_1)+1), cost(ub,steps,exp(length(_1),2)+length(_1)+1), cost(ub,viunif,3.5*exp(length(_1),2)+0.5*length(_1)), cost(ub,vounif,0) ).

:- true pred safe(_1)
         : list(_1,character_code)
        => ( list(_1,character_code), size_lb(_1,length(_1)), size_ub(_1,length(_1)) )
         + ( steps_lb(1), steps_ub(exp(length(_1),2)+length(_1)+1) ).

safe([]).
safe([X|L]) :-
        attacks(L,X,1),
        safe(L).

:- trust comp attacks(A,B,C)
         + ( size_metric(A,length), size_metric(B,int), size_metric(C,void) ).

:- true pred attacks(A,B,C)
         : ( list(A,character_code), int(B), num(C) )
        => ( list(A,character_code), int(B), num(C) ).

:- true pred attacks(A,B,C)
         : ground([A,B,C])
        => ground([A,B,C]).

:- true pred attacks(A,B,C)
         : ( list(A,character_code), character_code(B), num(C) )
        => ( list(A,character_code), character_code(B), num(C) )
         + ( possibly_fails, covered ).

:- true pred attacks(A,B,C)
         : ( list(A,character_code), character_code(B), num(C) )
        => ( list(A,character_code), character_code(B), num(C), size(lb,A,length(A)), size(lb,B,int(B)), size(lb,C,0) )
         + ( cost(lb,giunif,1), cost(lb,gounif,0), cost(lb,nargs,3), cost(lb,res_steps,1), cost(lb,steps,1), cost(lb,viunif,2), cost(lb,vounif,0) ).

:- true pred attacks(A,B,C)
         : ( list(A,character_code), character_code(B), num(C) )
        => ( list(A,character_code), character_code(B), num(C), size(ub,A,length(A)), size(ub,B,int(B)), size(ub,C,0) )
         + ( cost(ub,giunif,5*length(A)+1), cost(ub,gounif,0), cost(ub,nargs,6*length(A)+3), cost(ub,res_steps,2*length(A)+1), cost(ub,steps,2*length(A)+1), cost(ub,viunif,7*length(A)+2), cost(ub,vounif,0) ).

:- true pred attacks(A,B,C)
         : ( list(A,character_code), character_code(B), num(C) )
        => ( list(A,character_code), character_code(B), num(C), size_lb(A,length(A)), size_lb(B,int(B)), size_lb(C,0), size_ub(A,length(A)), size_ub(B,int(B)), size_ub(C,0) )
         + ( steps_lb(1), steps_ub(2*length(A)+1) ).

attacks([],_1,_2).
attacks([Y|L],X,D) :-
        attack(X,Y,D),
        D1 is D+1,
        attacks(L,X,D1).

:- trust comp attack(A,B,C)
         + ( size_metric(A,int), size_metric(B,int), size_metric(C,void) ).

:- true pred attack(A,B,C)
         : ( int(A), int(B), num(C) )
        => ( int(A), int(B), num(C) ).

:- true pred attack(A,B,C)
         : ground([A,B,C])
        => ground([A,B,C]).

:- true pred attack(A,B,C)
         : ( int(A), int(B), num(C) )
        => ( int(A), int(B), num(C) )
         + ( possibly_fails, not_covered ).

:- true pred attack(A,B,C)
         : ( int(A), int(B), num(C) )
        => ( int(A), int(B), num(C), size(lb,A,int(A)), size(lb,B,int(B)), size(lb,C,0) )
         + ( cost(lb,giunif,0), cost(lb,gounif,0), cost(lb,nargs,0), cost(lb,res_steps,0), cost(lb,steps,0), cost(lb,viunif,0), cost(lb,vounif,0) ).

:- true pred attack(A,B,C)
         : ( int(A), int(B), num(C) )
        => ( int(A), int(B), num(C), size(ub,A,int(A)), size(ub,B,int(B)), size(ub,C,0) )
         + ( cost(ub,giunif,2), cost(ub,gounif,0), cost(ub,nargs,3), cost(ub,res_steps,1), cost(ub,steps,1), cost(ub,viunif,3), cost(ub,vounif,0) ).

:- true pred attack(A,B,C)
         : ( int(A), int(B), num(C) )
        => ( int(A), int(B), num(C), size_lb(A,int(A)), size_lb(B,int(B)), size_lb(C,0), size_ub(A,int(A)), size_ub(B,int(B)), size_ub(C,0) )
         + ( steps_lb(0), steps_ub(1) ).

attack(X,Y,_1) :-
        X=:=Y,
        !.
attack(X,Y,D) :-
        Y-X=:=D,
        !.
attack(X,Y,D) :-
        Y-X=:= -D.


