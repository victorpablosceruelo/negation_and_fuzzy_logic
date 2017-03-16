:- module(_1,[eight_queens/1],[assertions,nativeprops,ciaopp(tests(resources)),library(resdefs(resources_decl)),basicmodes,regtypes]).

:- doc(author,"Nai-Wei Lin").

:- doc(author,"Edison Mera").

:- doc(module,"This program plays the 8-queens game.").

:- load_resource_module(eight_queen_res).

:- resource movements.

:- head_cost(ub,movements,'eight_queen:delta_qmovements').

:- literal_cost(ub,movements,0).

:- trust_default+cost(ub,movements,0).

:- entry eight_queens(_1)
         : var(_1).

:- true pred eight_queens(_1)
         : term(_1)
        => rt321(_1).

:- true pred eight_queens(_1)
         : ( mshare([[_1]]), var(_1) )
        => ground([_1]).

:- true pred eight_queens(_1)
         : var(_1)
        => rt321(_1)
         + ( possibly_fails, covered ).

:- true pred eight_queens(_1)
         : var(_1)
        => ( rt321(_1), size(ub,_1,8) )
         + cost(ub,movements,19173961.0).

eight_queens([X1,X2,X3,X4,X5,X6,X7,X8]) :-
        generator(8,8,[X1,X2,X3,X4,X5,X6,X7,X8]),
        queens(X1,X2,X3,X4,X5,X6,X7,X8).

:- true pred queens(X1,X2,X3,X4,X5,X6,X7,X8)
         : ( num(X1), num(X2), num(X3), num(X4), num(X5), num(X6), num(X7), num(X8) )
        => ( num(X1), num(X2), num(X3), num(X4), num(X5), num(X6), num(X7), num(X8) ).

:- true pred queens(X1,X2,X3,X4,X5,X6,X7,X8)
         : ground([X1,X2,X3,X4,X5,X6,X7,X8])
        => ground([X1,X2,X3,X4,X5,X6,X7,X8]).

:- true pred queens(X1,X2,X3,X4,X5,X6,X7,X8)
         : ( num(X1), num(X2), num(X3), num(X4), num(X5), num(X6), num(X7), num(X8) )
        => ( num(X1), num(X2), num(X3), num(X4), num(X5), num(X6), num(X7), num(X8) )
         + ( possibly_fails, covered ).

:- true pred queens(X1,X2,X3,X4,X5,X6,X7,X8)
         : ( num(X1), num(X2), num(X3), num(X4), num(X5), num(X6), num(X7), num(X8) )
        => ( num(X1), num(X2), num(X3), num(X4), num(X5), num(X6), num(X7), num(X8), size(ub,X1,int(X1)), size(ub,X2,int(X2)), size(ub,X3,int(X3)), size(ub,X4,int(X4)), size(ub,X5,int(X5)), size(ub,X6,int(X6)), size(ub,X7,int(X7)), size(ub,X8,int(X8)) )
         + cost(ub,movements,0).

queens(X1,X2,X3,X4,X5,X6,X7,X8) :-
        safe([X1,X2,X3,X4,X5,X6,X7,X8]).

:- true pred safe(_1)
         : numlist(_1)
        => numlist(_1).

:- true pred safe(_1)
         : ground([_1])
        => ground([_1]).

:- true pred safe(_1)
         : numlist(_1)
        => numlist(_1)
         + ( possibly_fails, covered ).

:- true pred safe(_1)
         : numlist(_1)
        => ( numlist(_1), size(ub,_1,length(_1)) )
         + cost(ub,movements,0).

safe([]).
safe([X|L]) :-
        noattacks(L,X,1),
        safe(L).

:- trust comp noattacks(X,Y,Z)
         + ( size_metric(X,length), size_metric(Y,int), size_metric(Z,void) ).

:- true pred noattacks(X,Y,Z)
         : ( numlist(X), num(Y), num(Z) )
        => ( numlist(X), num(Y), num(Z) ).

:- true pred noattacks(X,Y,Z)
         : ground([X,Y,Z])
        => ground([X,Y,Z]).

:- true pred noattacks(X,Y,Z)
         : ( numlist(X), num(Y), num(Z) )
        => ( numlist(X), num(Y), num(Z) )
         + ( possibly_fails, not_covered ).

:- true pred noattacks(X,Y,Z)
         : ( numlist(X), num(Y), num(Z) )
        => ( numlist(X), num(Y), num(Z), size(ub,X,length(X)), size(ub,Y,int(Y)), size(ub,Z,0) )
         + cost(ub,movements,0).

noattacks([],_1,_2).
noattacks([Y|L],X,D) :-
        noattack(X,Y,D),
        D1 is D+1,
        noattacks(L,X,D1).

:- true pred noattack(X,Y,D)
         : ( num(X), num(Y), num(D) )
        => ( num(X), num(Y), num(D) ).

:- true pred noattack(X,Y,D)
         : ground([X,Y,D])
        => ground([X,Y,D]).

:- true pred noattack(X,Y,D)
         : ( num(X), num(Y), num(D) )
        => ( num(X), num(Y), num(D) )
         + ( possibly_fails, not_covered ).

:- true pred noattack(X,Y,D)
         : ( num(X), num(Y), num(D) )
        => ( num(X), num(Y), num(D), size(ub,X,int(X)), size(ub,Y,int(Y)), size(ub,D,int(D)) )
         + cost(ub,movements,0).

noattack(X,Y,D) :-
        X=\=Y,
        Y-X=\=D,
        Y-X=\= -D.

:- true pred generator(M,_1,_2)
         : ( num(M), rt0(_1), list(_2) )
        => ( num(M), rt0(_1), numlist(_2) ).

:- true pred generator(M,_1,_2)
         : ( mshare([[_2]]), ground([M,_1]) )
        => ground([M,_1,_2]).

:- true pred generator(M,_1,_2)
         : ( num(M), rt0(_1), list(_2) )
        => ( num(M), rt0(_1), numlist(_2) )
         + ( possibly_fails, not_covered ).

:- true pred generator(M,_1,_2)
         : ( num(M), rt0(_1), list(_2) )
        => ( num(M), rt0(_1), numlist(_2), size(ub,M,int(M)), size(ub,_1,int(_1)), size(ub,_2,int(M)) )
         + cost(ub,movements,- (exp(exp(int(_1),-1)-1,-1)*exp(int(_1),int(M)-1))+exp(exp(int(_1),-1)-1,-1)*exp(int(_1),-1)+exp(int(_1),int(M))).

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

:- true pred choose(N,M)
         : ( num(N), term(M) )
        => ( num(N), num(M), size(ub,N,int(N)), size(ub,M,int(N)+1) )
         + cost(ub,movements,0).

choose(N,N) :-
        N>=1.
choose(N,M) :-
        N>1,
        N1 is N-1,
        choose(N1,M).


:- regtype rt321/1.

rt321([A,B,C,D,E,F,G,H]) :-
        num(A),
        num(B),
        num(C),
        num(D),
        num(E),
        num(F),
        num(G),
        num(H).


:- regtype rt0/1.

rt0(8).


