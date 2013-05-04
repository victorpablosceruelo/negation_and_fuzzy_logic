:- module(_1,[schedule/1,legal_schedule/7],[assertions,ciaopp(tests(resources)),predefres(res_all),nativeprops,basicmodes,regtypes]).

:- doc(author,"Nai-Wei Lin").

:- doc(author,"Edison Mera").

:- use_module(library(aggregates)).

:- use_package(ciaopp(tests(resources(examples(schedule_res))))).

:- entry schedule(_1)
         : var(_1).

:- true pred schedule(MinTime)
         : term(MinTime)
        => arithexpression(MinTime).

:- true pred schedule(MinTime)
         : ( mshare([[MinTime]]), var(MinTime) )
        => ground([MinTime]).

:- true pred schedule(MinTime)
         : var(MinTime)
        => arithexpression(MinTime)
         + ( possibly_fails, covered ).

schedule(MinTime) :-
        aggregates:findall(G,legal_schedule(_A,_B,_C,_D,_E,_F,G),Time),
        earliest_completion(Time,MinTime).

:- true pred legal_schedule(A,B,C,D,E,F,G)
         : ( term(A), term(B), term(C), term(D), term(E), term(F), term(G) )
        => ( num(A), num(B), num(C), num(D), num(E), num(F), num(G) ).

:- true pred legal_schedule(A,B,C,D,E,F,G)
         : mshare([[A],[A,B],[A,B,C],[A,B,C,D],[A,B,C,D,E],[A,B,C,D,E,F],[A,B,C,D,E,F,G],[A,B,C,D,E,G],[A,B,C,D,F],[A,B,C,D,F,G],[A,B,C,D,G],[A,B,C,E],[A,B,C,E,F],[A,B,C,E,F,G],[A,B,C,E,G],[A,B,C,F],[A,B,C,F,G],[A,B,C,G],[A,B,D],[A,B,D,E],[A,B,D,E,F],[A,B,D,E,F,G],[A,B,D,E,G],[A,B,D,F],[A,B,D,F,G],[A,B,D,G],[A,B,E],[A,B,E,F],[A,B,E,F,G],[A,B,E,G],[A,B,F],[A,B,F,G],[A,B,G],[A,C],[A,C,D],[A,C,D,E],[A,C,D,E,F],[A,C,D,E,F,G],[A,C,D,E,G],[A,C,D,F],[A,C,D,F,G],[A,C,D,G],[A,C,E],[A,C,E,F],[A,C,E,F,G],[A,C,E,G],[A,C,F],[A,C,F,G],[A,C,G],[A,D],[A,D,E],[A,D,E,F],[A,D,E,F,G],[A,D,E,G],[A,D,F],[A,D,F,G],[A,D,G],[A,E],[A,E,F],[A,E,F,G],[A,E,G],[A,F],[A,F,G],[A,G],[B],[B,C],[B,C,D],[B,C,D,E],[B,C,D,E,F],[B,C,D,E,F,G],[B,C,D,E,G],[B,C,D,F],[B,C,D,F,G],[B,C,D,G],[B,C,E],[B,C,E,F],[B,C,E,F,G],[B,C,E,G],[B,C,F],[B,C,F,G],[B,C,G],[B,D],[B,D,E],[B,D,E,F],[B,D,E,F,G],[B,D,E,G],[B,D,F],[B,D,F,G],[B,D,G],[B,E],[B,E,F],[B,E,F,G],[B,E,G],[B,F],[B,F,G],[B,G],[C],[C,D],[C,D,E],[C,D,E,F],[C,D,E,F,G],[C,D,E,G],[C,D,F],[C,D,F,G],[C,D,G],[C,E],[C,E,F],[C,E,F,G],[C,E,G],[C,F],[C,F,G],[C,G],[D],[D,E],[D,E,F],[D,E,F,G],[D,E,G],[D,F],[D,F,G],[D,G],[E],[E,F],[E,F,G],[E,G],[F],[F,G],[G]])
        => ground([A,B,C,D,E,F,G]).

:- true pred legal_schedule(A,B,C,D,E,F,G)
         : ( term(A), term(B), term(C), term(D), term(E), term(F), term(G) )
        => ( num(A), num(B), num(C), num(D), num(E), num(F), num(G) )
         + ( possibly_fails, covered ).

legal_schedule(A,B,C,D,E,F,G) :-
        generator(7,10,[A,B,C,D,E,F,G]),
        schedule_constraints(A,B,C,D,E,F,G).

:- true pred schedule_constraints(A,B,C,D,E,F,G)
         : ( num(A), num(B), num(C), num(D), num(E), num(F), num(G) )
        => ( num(A), num(B), num(C), num(D), num(E), num(F), num(G) ).

:- true pred schedule_constraints(A,B,C,D,E,F,G)
         : ground([A,B,C,D,E,F,G])
        => ground([A,B,C,D,E,F,G]).

:- true pred schedule_constraints(A,B,C,D,E,F,G)
         : ( num(A), num(B), num(C), num(D), num(E), num(F), num(G) )
        => ( num(A), num(B), num(C), num(D), num(E), num(F), num(G) )
         + ( possibly_fails, not_covered ).

schedule_constraints(A,B,C,D,E,F,G) :-
        precedence_constraints(A,B,C,D,E,F,G),
        distance_constraints(B,C,D).

:- true pred precedence_constraints(A,B,C,D,E,F,G)
         : ( num(A), num(B), num(C), num(D), num(E), num(F), num(G) )
        => ( num(A), num(B), num(C), num(D), num(E), num(F), num(G) ).

:- true pred precedence_constraints(A,B,C,D,E,F,G)
         : ground([A,B,C,D,E,F,G])
        => ground([A,B,C,D,E,F,G]).

:- true pred precedence_constraints(A,B,C,D,E,F,G)
         : ( num(A), num(B), num(C), num(D), num(E), num(F), num(G) )
        => ( num(A), num(B), num(C), num(D), num(E), num(F), num(G) )
         + ( possibly_fails, not_covered ).

precedence_constraints(A,B,C,D,E,F,G) :-
        B>=A+1,
        C>=A+1,
        D>=A+1,
        E>=B+5,
        E>=C+3,
        F>=D+5,
        F>=E+2,
        G>=F+1.

:- true pred distance_constraints(B,C,_D)
         : ( num(B), num(C), num(_D) )
        => ( num(B), num(C), num(_D) ).

:- true pred distance_constraints(B,C,_D)
         : ground([B,C,_D])
        => ground([B,C,_D]).

:- true pred distance_constraints(B,C,_D)
         : ( num(B), num(C), num(_D) )
        => ( num(B), num(C), num(_D) )
         + ( possibly_fails, not_covered ).

distance_constraints(B,C,_D) :-
        C>=B+1.
distance_constraints(_B,C,D) :-
        D>=C+1.

:- true pred generator(M,_1,_2)
         : ( num(M), rt1(_1), list(_2) )
        => ( num(M), rt1(_1), numlist(_2) ).

:- true pred generator(M,_1,_2)
         : ( mshare([[_2]]), ground([M,_1]) )
        => ground([M,_1,_2]).

:- true pred generator(M,_1,_2)
         : ( num(M), rt1(_1), list(_2) )
        => ( num(M), rt1(_1), numlist(_2) )
         + ( possibly_fails, not_covered ).

generator(0,_1,[]).
generator(M,N,[Q|L]) :-
        M>0,
        choose(N,Q),
        M1 is M-1,
        generator(M1,N,L).

:- true pred choose(N,M)
         : ( num(N), term(M) )
        => ( num(N), num(M) ).

:- true pred choose(N,M)
         : ( mshare([[M]]), ground([N]) )
        => ground([N,M]).

:- true pred choose(N,M)
         : ( num(N), term(M) )
        => ( num(N), num(M) )
         + ( possibly_fails, not_covered ).

choose(N,N) :-
        N>0.
choose(N,M) :-
        N>0,
        N1 is N-1,
        choose(N1,M).

:- true pred earliest_completion(_1,MinTime)
         : ( list(_1), term(MinTime) )
        => ( list(_1,arithexpression), arithexpression(MinTime) ).

:- true pred earliest_completion(_1,MinTime)
         : ( mshare([[MinTime]]), var(MinTime), ground([_1]) )
        => ground([_1,MinTime]).

:- true pred earliest_completion(_1,MinTime)
         : ( list(_1), var(MinTime) )
        => ( list(_1,arithexpression), arithexpression(MinTime) )
         + ( possibly_fails, covered ).

earliest_completion([],10000).
earliest_completion([T|Time],MinTime) :-
        earliest_completion(Time,MTime),
        min(T,MTime,MinTime).

:- true pred min(X,Y,_1)
         : ( term(X), arithexpression(Y), term(_1) )
        => ( arithexpression(X), arithexpression(Y), arithexpression(_1) ).

:- true pred min(X,Y,_1)
         : ( mshare([[_1]]), var(_1), ground([X,Y]) )
        => ground([X,Y,_1]).

:- true pred min(X,Y,_1)
         : ( gnd(X), arithexpression(Y), var(_1) )
        => ( arithexpression(X), arithexpression(Y), arithexpression(_1) )
         + ( possibly_fails, not_covered ).

min(X,Y,X) :-
        X=<Y.
min(X,Y,Y) :-
        X>Y.


:- regtype rt1/1.

rt1(10).


