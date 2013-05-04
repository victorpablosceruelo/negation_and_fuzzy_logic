:- module(_1,[color_map/5],[assertions,regtypes,ciaopp(tests(resources)),predefres(res_all),nativeprops,basicmodes]).

:- prop color/1+regtype.

:- load_resource_module(color_map_res).

:- resource max_number_of_unifs.

:- head_cost(ub,max_number_of_unifs,'color_map:delta_max_number_of_unifs').

:- literal_cost(ub,max_number_of_unifs,0).

:- trust_default+cost(ub,max_number_of_unifs,0).

:- resource calls_to_builtins.

:- head_cost(ub,calls_to_builtins,'color_map:delta_calls_to_builtins').

:- literal_cost(ub,calls_to_builtins,0).

:- trust_default+cost(ub,calls_to_builtins,0).

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
         + ( cost(lb,giunif,0), cost(lb,gounif,5), cost(lb,nargs,10), cost(lb,steps,6), cost(lb,viunif,0), cost(lb,vounif,5) ).

:- true pred color_map(A,B,C,D,E)
         : ( var(A), var(B), var(C), var(D), var(E) )
        => ( color(A), color(B), color(C), color(D), color(E), size(ub,A,1), size(ub,B,1), size(ub,C,1), size(ub,D,1), size(ub,E,1) )
         + ( cost(ub,calls_to_builtins,0), cost(ub,giunif,0), cost(ub,gounif,3905), cost(ub,max_number_of_unifs,104691), cost(ub,nargs,72660), cost(ub,steps,32031), cost(ub,viunif,68750), cost(ub,vounif,5) ).

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
         + ( cost(lb,giunif,0), cost(lb,gounif,0), cost(lb,nargs,0), cost(lb,steps,0), cost(lb,viunif,0), cost(lb,vounif,0) ).

:- true pred legal_coloring(A,B,C,D,E)
         : ( color(A), color(B), color(C), color(D), color(E) )
        => ( color(A), color(B), color(C), color(D), color(E), size(ub,A,size(A)), size(ub,B,size(B)), size(ub,C,size(C)), size(ub,D,size(D)), size(ub,E,size(E)) )
         + ( cost(ub,calls_to_builtins,0), cost(ub,giunif,0), cost(ub,gounif,0), cost(ub,max_number_of_unifs,31), cost(ub,nargs,22), cost(ub,steps,9), cost(ub,viunif,22), cost(ub,vounif,0) ).

legal_coloring(A,B,C,D,E) :-
        not_equal(A,B),
        not_equal(A,C),
        not_equal(A,D),
        not_equal(A,E),
        c(B,C,D),
        not_equal(C,E).

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
         + ( cost(lb,giunif,0), cost(lb,gounif,0), cost(lb,nargs,0), cost(lb,steps,0), cost(lb,viunif,0), cost(lb,vounif,0) ).

:- true pred c(X,Y,Z)
         : ( color(X), color(Y), color(Z) )
        => ( color(X), color(Y), color(Z), size(ub,X,size(X)), size(ub,Y,size(Y)), size(ub,Z,size(Z)) )
         + ( cost(ub,calls_to_builtins,0), cost(ub,giunif,0), cost(ub,gounif,0), cost(ub,max_number_of_unifs,10), cost(ub,nargs,7), cost(ub,steps,3), cost(ub,viunif,7), cost(ub,vounif,0) ).

c(X,Y,Z) :-
        not_equal(X,Y),
        not_equal(X,Z).

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
         + ( cost(lb,giunif,0), cost(lb,gounif,1), cost(lb,nargs,1), cost(lb,steps,1), cost(lb,viunif,0), cost(lb,vounif,0) ).

:- true pred color(_1)
         : var(_1)
        => ( color(_1), size(ub,_1,1) )
         + ( cost(ub,calls_to_builtins,0), cost(ub,giunif,0), cost(ub,gounif,5), cost(ub,max_number_of_unifs,10), cost(ub,nargs,5), cost(ub,steps,5), cost(ub,viunif,0), cost(ub,vounif,0) ).

color(blue).
color(green).
color(orange).
color(red).
color(yellow).

%% %% :- trust pred not_equal(_1,_2)
%% %%    : ( color(_1), color(_2) )
%% %%    + cost(ub,max_number_of_unifs,3).

:- check calls not_equal(_1,_2)
         : ( color(_1), color(_2) ).

:- trust comp not_equal(_1,_2)
         : ( color(_1), color(_2) )
         + cost(ub,max_number_of_unifs,3).

:- true pred not_equal(X,Y)
         : ( color(X), color(Y) )
        => ( color(X), color(Y) ).

:- true pred not_equal(X,Y)
         : ground([X,Y])
        => ground([X,Y]).

:- true pred not_equal(X,Y)
         : ( color(X), color(Y) )
        => ( color(X), color(Y) )
         + ( possibly_fails, not_covered ).

:- true pred not_equal(X,Y)
         : ( color(X), color(Y) )
        => ( color(X), color(Y), size(lb,X,size(X)), size(lb,Y,size(Y)) )
         + ( cost(lb,giunif,0), cost(lb,gounif,0), cost(lb,nargs,0), cost(lb,steps,0), cost(lb,viunif,0), cost(lb,vounif,0) ).

:- true pred not_equal(X,Y)
         : ( color(X), color(Y) )
        => ( color(X), color(Y), size(ub,X,size(X)), size(ub,Y,size(Y)) )
         + ( cost(ub,calls_to_builtins,0), cost(ub,giunif,0), cost(ub,gounif,0), cost(ub,max_number_of_unifs,3), cost(ub,nargs,2), cost(ub,steps,1), cost(ub,viunif,2), cost(ub,vounif,0) ).

not_equal(X,Y) :-
        X\==Y.


