:- module(_1,[color_map/5],[assertions,regtypes,nativeprops,ciaopp(tests(resources)),predefres(res_all),basicmodes]).

:- resource res_steps.

:- head_cost(ub,res_steps,1).

:- literal_cost(ub,res_steps,0).

:- head_cost(lb,res_steps,1).

:- literal_cost(lb,res_steps,0).

:- trust_default+cost(ub,res_steps,0).

:- trust_default+cost(lb,res_steps,0).

:- prop color/1+regtype.

:- entry color_map(_1,_2,_3,_4,_5)
         : ( var(_1), var(_2), var(_3), var(_4), var(_5) ).

:- true pred color_map(A,B,C,D,E)
         : ( term(A), term(B), term(C), term(D), term(E) )
        => ( color(A), color(B), color(C), color(D), color(E) ).

:- true pred color_map(A,B,C,D,E)
         : ( mshare([[A],[A,B],[A,B,C],[A,B,C,D],[A,B,C,D,E],[A,B,C,E],[A,B,D],[A,B,D,E],[A,B,E],[A,C],[A,C,D],[A,C,D,E],[A,C,E],[A,D],[A,D,E],[A,E],[B],[B,C],[B,C,D],[B,C,D,E],[B,C,E],[B,D],[B,D,E],[B,E],[C],[C,D],[C,D,E],[C,E],[D],[D,E],[E]]), var(A), var(B), var(C), var(D), var(E) )
        => ground([A,B,C,D,E]).

:- true pred color_map(A,B,C,D,E)
         : ( var(A), var(B), var(C), var(D), var(E) )
        => ( color(A), color(B), color(C), color(D), color(E) )
         + ( possibly_fails, covered ).

:- true pred color_map(A,B,C,D,E)
         : ( var(A), var(B), var(C), var(D), var(E) )
        => ( color(A), color(B), color(C), color(D), color(E), size(lb,A,1), size(lb,B,1), size(lb,C,1), size(lb,D,1), size(lb,E,1) )
         + ( cost(lb,giunif,0), cost(lb,gounif,5), cost(lb,nargs,10), cost(lb,res_steps,6), cost(lb,steps,6), cost(lb,viunif,0), cost(lb,vounif,5) ).

:- true pred color_map(A,B,C,D,E)
         : ( var(A), var(B), var(C), var(D), var(E) )
        => ( color(A), color(B), color(C), color(D), color(E), size(ub,A,1), size(ub,B,1), size(ub,C,1), size(ub,D,1), size(ub,E,1) )
         + ( cost(ub,giunif,0), cost(ub,gounif,3905), cost(ub,nargs,28910), cost(ub,res_steps,10156), cost(ub,steps,10156), cost(ub,viunif,25000), cost(ub,vounif,5) ).

:- true pred color_map(A,B,C,D,E)
         : ( var(A), var(B), var(C), var(D), var(E) )
        => ( color(A), color(B), color(C), color(D), color(E), size_lb(A,1), size_lb(B,1), size_lb(C,1), size_lb(D,1), size_lb(E,1), size_ub(A,1), size_ub(B,1), size_ub(C,1), size_ub(D,1), size_ub(E,1) )
         + ( steps_lb(6), steps_ub(10156) ).

color_map(A,B,C,D,E) :-
        color(A),
        color(B),
        color(C),
        color(D),
        color(E),
        legal_coloring(A,B,C,D,E).

:- true pred legal_coloring(A,B,C,D,E)
         : ( color(A), color(B), color(C), color(D), color(E) )
        => ( color(A), color(B), color(C), color(D), color(E) ).

:- true pred legal_coloring(A,B,C,D,E)
         : ground([A,B,C,D,E])
        => ground([A,B,C,D,E]).

:- true pred legal_coloring(A,B,C,D,E)
         : ( color(A), color(B), color(C), color(D), color(E) )
        => ( color(A), color(B), color(C), color(D), color(E) )
         + ( possibly_fails, not_covered ).

:- true pred legal_coloring(A,B,C,D,E)
         : ( color(A), color(B), color(C), color(D), color(E) )
        => ( color(A), color(B), color(C), color(D), color(E), size(lb,A,size(A)), size(lb,B,size(B)), size(lb,C,size(C)), size(lb,D,size(D)), size(lb,E,size(E)) )
         + ( cost(lb,giunif,0), cost(lb,gounif,0), cost(lb,nargs,0), cost(lb,res_steps,0), cost(lb,steps,0), cost(lb,viunif,0), cost(lb,vounif,0) ).

:- true pred legal_coloring(A,B,C,D,E)
         : ( color(A), color(B), color(C), color(D), color(E) )
        => ( color(A), color(B), color(C), color(D), color(E), size(ub,A,size(A)), size(ub,B,size(B)), size(ub,C,size(C)), size(ub,D,size(D)), size(ub,E,size(E)) )
         + ( cost(ub,giunif,0), cost(ub,gounif,0), cost(ub,nargs,8), cost(ub,res_steps,2), cost(ub,steps,2), cost(ub,viunif,8), cost(ub,vounif,0) ).

:- true pred legal_coloring(A,B,C,D,E)
         : ( color(A), color(B), color(C), color(D), color(E) )
        => ( color(A), color(B), color(C), color(D), color(E), size_lb(A,size(A)), size_lb(B,size(B)), size_lb(C,size(C)), size_lb(D,size(D)), size_lb(E,size(E)), size_ub(A,size(A)), size_ub(B,size(B)), size_ub(C,size(C)), size_ub(D,size(D)), size_ub(E,size(E)) )
         + ( steps_lb(0), steps_ub(2) ).

legal_coloring(A,B,C,D,E) :-
        A\==B,
        A\==C,
        A\==D,
        A\==E,
        c(B,C,D),
        C\==E.

:- true pred c(X,Y,Z)
         : ( color(X), color(Y), color(Z) )
        => ( color(X), color(Y), color(Z) ).

:- true pred c(X,Y,Z)
         : ground([X,Y,Z])
        => ground([X,Y,Z]).

:- true pred c(X,Y,Z)
         : ( color(X), color(Y), color(Z) )
        => ( color(X), color(Y), color(Z) )
         + ( possibly_fails, not_covered ).

:- true pred c(X,Y,Z)
         : ( color(X), color(Y), color(Z) )
        => ( color(X), color(Y), color(Z), size(lb,X,size(X)), size(lb,Y,size(Y)), size(lb,Z,size(Z)) )
         + ( cost(lb,giunif,0), cost(lb,gounif,0), cost(lb,nargs,0), cost(lb,res_steps,0), cost(lb,steps,0), cost(lb,viunif,0), cost(lb,vounif,0) ).

:- true pred c(X,Y,Z)
         : ( color(X), color(Y), color(Z) )
        => ( color(X), color(Y), color(Z), size(ub,X,size(X)), size(ub,Y,size(Y)), size(ub,Z,size(Z)) )
         + ( cost(ub,giunif,0), cost(ub,gounif,0), cost(ub,nargs,3), cost(ub,res_steps,1), cost(ub,steps,1), cost(ub,viunif,3), cost(ub,vounif,0) ).

:- true pred c(X,Y,Z)
         : ( color(X), color(Y), color(Z) )
        => ( color(X), color(Y), color(Z), size_lb(X,size(X)), size_lb(Y,size(Y)), size_lb(Z,size(Z)), size_ub(X,size(X)), size_ub(Y,size(Y)), size_ub(Z,size(Z)) )
         + ( steps_lb(0), steps_ub(1) ).

c(X,Y,Z) :-
        X\==Y,
        X\==Z.

:- prop color(_1)
         + regtype.

:- true pred color(_1)
         : term(_1)
        => color(_1).

:- true pred color(_1)
         : ( mshare([[_1]]), var(_1) )
        => ground([_1]).

:- true pred color(_1)
         : var(_1)
        => color(_1)
         + ( not_fails, covered ).

:- true pred color(_1)
         : var(_1)
        => ( color(_1), size(lb,_1,1) )
         + ( cost(lb,giunif,0), cost(lb,gounif,1), cost(lb,nargs,1), cost(lb,res_steps,1), cost(lb,steps,1), cost(lb,viunif,0), cost(lb,vounif,0) ).

:- true pred color(_1)
         : var(_1)
        => ( color(_1), size(ub,_1,1) )
         + ( cost(ub,giunif,0), cost(ub,gounif,5), cost(ub,nargs,5), cost(ub,res_steps,5), cost(ub,steps,5), cost(ub,viunif,0), cost(ub,vounif,0) ).

:- true pred color(_1)
         : var(_1)
        => ( color(_1), size_lb(_1,1), size_ub(_1,1) )
         + ( steps_lb(1), steps_ub(5) ).

color(blue).
color(green).
color(orange).
color(red).
color(yellow).


