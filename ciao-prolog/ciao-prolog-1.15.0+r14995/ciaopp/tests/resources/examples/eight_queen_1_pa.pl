:- module(_1,[eight_queens/1],[assertions,regtypes,nativeprops,ciaopp(tests(resources)),predefres(res_all),basicmodes]).

:- doc(author,"Nai-Wei Lin").

:- doc(author,"Edison Mera").

:- doc(module,"This program plays the 8-queens game.").

:- resource res_steps.

:- literal_cost(ub,res_steps,0).

:- literal_cost(lb,res_steps,0).

:- head_cost(ub,res_steps,1).

:- head_cost(lb,res_steps,1).

:- trust_default+cost(ub,res_steps,0).

:- trust_default+cost(lb,res_steps,0).

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
        => ( rt321(_1), size(lb,_1,8) )
         + ( cost(lb,giunif,0), cost(lb,gounif,9), cost(lb,nargs,1), cost(lb,res_steps,1), cost(lb,steps,1), cost(lb,viunif,0), cost(lb,vounif,8) ).

:- true pred eight_queens(_1)
         : var(_1)
        => ( rt321(_1), size(ub,_1,8) )
         + ( cost(ub,giunif,3523215355.0), cost(ub,gounif,19173979.0), cost(ub,nargs,3640655868.0), cost(ub,res_steps,1299035866.0), cost(ub,steps,1299035866.0), cost(ub,viunif,4019341602.0), cost(ub,vounif,43141418.0) ).

:- true pred eight_queens(_1)
         : var(_1)
        => ( rt321(_1), size_lb(_1,8), size_ub(_1,8) )
         + ( steps_lb(1), steps_ub(1299035866.0) ).

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
        => ( num(X1), num(X2), num(X3), num(X4), num(X5), num(X6), num(X7), num(X8), size(lb,X1,int(X1)), size(lb,X2,int(X2)), size(lb,X3,int(X3)), size(lb,X4,int(X4)), size(lb,X5,int(X5)), size(lb,X6,int(X6)), size(lb,X7,int(X7)), size(lb,X8,int(X8)) )
         + ( cost(lb,giunif,0), cost(lb,gounif,0), cost(lb,nargs,8), cost(lb,res_steps,1), cost(lb,steps,1), cost(lb,viunif,8), cost(lb,vounif,0) ).

:- true pred queens(X1,X2,X3,X4,X5,X6,X7,X8)
         : ( num(X1), num(X2), num(X3), num(X4), num(X5), num(X6), num(X7), num(X8) )
        => ( num(X1), num(X2), num(X3), num(X4), num(X5), num(X6), num(X7), num(X8), size(ub,X1,int(X1)), size(ub,X2,int(X2)), size(ub,X3,int(X3)), size(ub,X4,int(X4)), size(ub,X5,int(X5)), size(ub,X6,int(X6)), size(ub,X7,int(X7)), size(ub,X8,int(X8)) )
         + ( cost(ub,giunif,202.0), cost(ub,gounif,0), cost(ub,nargs,209.0), cost(ub,res_steps,74.0), cost(ub,steps,74.0), cost(ub,viunif,236.0), cost(ub,vounif,0) ).

:- true pred queens(X1,X2,X3,X4,X5,X6,X7,X8)
         : ( num(X1), num(X2), num(X3), num(X4), num(X5), num(X6), num(X7), num(X8) )
        => ( num(X1), num(X2), num(X3), num(X4), num(X5), num(X6), num(X7), num(X8), size_lb(X1,int(X1)), size_lb(X2,int(X2)), size_lb(X3,int(X3)), size_lb(X4,int(X4)), size_lb(X5,int(X5)), size_lb(X6,int(X6)), size_lb(X7,int(X7)), size_lb(X8,int(X8)), size_ub(X1,int(X1)), size_ub(X2,int(X2)), size_ub(X3,int(X3)), size_ub(X4,int(X4)), size_ub(X5,int(X5)), size_ub(X6,int(X6)), size_ub(X7,int(X7)), size_ub(X8,int(X8)) )
         + ( steps_lb(1), steps_ub(74.0) ).

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
        => ( numlist(_1), size(lb,_1,length(_1)) )
         + ( cost(lb,giunif,1), cost(lb,gounif,0), cost(lb,nargs,1), cost(lb,res_steps,1), cost(lb,steps,1), cost(lb,viunif,0), cost(lb,vounif,0) ).

:- true pred safe(_1)
         : numlist(_1)
        => ( numlist(_1), size(ub,_1,length(_1)) )
         + ( cost(ub,giunif,3.0*exp(length(_1),2)+1), cost(ub,gounif,0), cost(ub,nargs,3.0*exp(length(_1),2)+length(_1)+1), cost(ub,res_steps,exp(length(_1),2)+length(_1)+1), cost(ub,steps,exp(length(_1),2)+length(_1)+1), cost(ub,viunif,3.5*exp(length(_1),2)+0.5*length(_1)), cost(ub,vounif,0) ).

:- true pred safe(_1)
         : numlist(_1)
        => ( numlist(_1), size_lb(_1,length(_1)), size_ub(_1,length(_1)) )
         + ( steps_lb(1), steps_ub(exp(length(_1),2)+length(_1)+1) ).

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
        => ( numlist(X), num(Y), num(Z), size(lb,X,length(X)), size(lb,Y,int(Y)), size(lb,Z,0) )
         + ( cost(lb,giunif,0), cost(lb,gounif,0), cost(lb,nargs,0), cost(lb,res_steps,0), cost(lb,steps,0), cost(lb,viunif,0), cost(lb,vounif,0) ).

:- true pred noattacks(X,Y,Z)
         : ( numlist(X), num(Y), num(Z) )
        => ( numlist(X), num(Y), num(Z), size(ub,X,length(X)), size(ub,Y,int(Y)), size(ub,Z,0) )
         + ( cost(ub,giunif,6*length(X)+1), cost(ub,gounif,0), cost(ub,nargs,6*length(X)+3), cost(ub,res_steps,2*length(X)+1), cost(ub,steps,2*length(X)+1), cost(ub,viunif,7*length(X)+2), cost(ub,vounif,0) ).

:- true pred noattacks(X,Y,Z)
         : ( numlist(X), num(Y), num(Z) )
        => ( numlist(X), num(Y), num(Z), size_lb(X,length(X)), size_lb(Y,int(Y)), size_lb(Z,0), size_ub(X,length(X)), size_ub(Y,int(Y)), size_ub(Z,0) )
         + ( steps_lb(0), steps_ub(2*length(X)+1) ).

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
        => ( num(X), num(Y), num(D), size(lb,X,int(X)), size(lb,Y,int(Y)), size(lb,D,int(D)) )
         + ( cost(lb,giunif,0), cost(lb,gounif,0), cost(lb,nargs,0), cost(lb,res_steps,0), cost(lb,steps,0), cost(lb,viunif,0), cost(lb,vounif,0) ).

:- true pred noattack(X,Y,D)
         : ( num(X), num(Y), num(D) )
        => ( num(X), num(Y), num(D), size(ub,X,int(X)), size(ub,Y,int(Y)), size(ub,D,int(D)) )
         + ( cost(ub,giunif,3), cost(ub,gounif,0), cost(ub,nargs,3), cost(ub,res_steps,1), cost(ub,steps,1), cost(ub,viunif,3), cost(ub,vounif,0) ).

:- true pred noattack(X,Y,D)
         : ( num(X), num(Y), num(D) )
        => ( num(X), num(Y), num(D), size_lb(X,int(X)), size_lb(Y,int(Y)), size_lb(D,int(D)), size_ub(X,int(X)), size_ub(Y,int(Y)), size_ub(D,int(D)) )
         + ( steps_lb(0), steps_ub(1) ).

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
        => ( num(M), rt0(_1), numlist(_2), size(lb,M,int(M)), size(lb,_1,int(_1)), size(lb,_2,int(M)) )
         + ( cost(lb,giunif,0), cost(lb,gounif,0), cost(lb,nargs,0), cost(lb,res_steps,0), cost(lb,steps,0), cost(lb,viunif,0), cost(lb,vounif,0) ).

:- true pred generator(M,_1,_2)
         : ( num(M), rt0(_1), list(_2) )
        => ( num(M), rt0(_1), numlist(_2), size(ub,M,int(M)), size(ub,_1,int(_1)), size(ub,_2,int(M)) )
         + ( cost(ub,giunif,- (exp(exp(int(_1),-1)-1,-1)*exp(int(_1),int(M)-1))-6*(exp(exp(int(_1),-1)-1,-1)*exp(int(_1),int(M)))+exp(exp(int(_1),-1)-1,-1)*exp(int(_1),-1)+6*exp(exp(int(_1),-1)-1,-1)+exp(int(_1),int(M))), cost(ub,gounif,- (exp(exp(int(_1),-1)-1,-1)*exp(int(_1),int(M)-1))+exp(exp(int(_1),-1)-1,-1)*exp(int(_1),-1)+exp(int(_1),int(M))), cost(ub,nargs,- (3*(exp(exp(int(_1),-1)-1,-1)*exp(int(_1),int(M)-1)))-4*(exp(exp(int(_1),-1)-1,-1)*exp(int(_1),int(M)))+3*(exp(exp(int(_1),-1)-1,-1)*exp(int(_1),-1))+4*exp(exp(int(_1),-1)-1,-1)+3*exp(int(_1),int(M))), cost(ub,res_steps,- (exp(exp(int(_1),-1)-1,-1)*exp(int(_1),int(M)-1))-2*(exp(exp(int(_1),-1)-1,-1)*exp(int(_1),int(M)))+exp(exp(int(_1),-1)-1,-1)*exp(int(_1),-1)+2*exp(exp(int(_1),-1)-1,-1)+exp(int(_1),int(M))), cost(ub,steps,- (exp(exp(int(_1),-1)-1,-1)*exp(int(_1),int(M)-1))-2*(exp(exp(int(_1),-1)-1,-1)*exp(int(_1),int(M)))+exp(exp(int(_1),-1)-1,-1)*exp(int(_1),-1)+2*exp(exp(int(_1),-1)-1,-1)+exp(int(_1),int(M))), cost(ub,viunif,- (2*(exp(exp(int(_1),-1)-1,-1)*exp(int(_1),int(M)-1)))-2*(exp(exp(int(_1),-1)-1,-1)*exp(int(_1),int(M)))+2*(exp(exp(int(_1),-1)-1,-1)*exp(int(_1),-1))+2*exp(exp(int(_1),-1)-1,-1)+exp(int(_1),int(M))), cost(ub,vounif,- (2*(exp(exp(int(_1),-1)-1,-1)*exp(int(_1),int(M)-1)))-2*(exp(exp(int(_1),-1)-1,-1)*exp(int(_1),int(M)))+2*(exp(exp(int(_1),-1)-1,-1)*exp(int(_1),-1))+2*exp(exp(int(_1),-1)-1,-1)) ).

:- true pred generator(M,_1,_2)
         : ( num(M), rt0(_1), list(_2) )
        => ( num(M), rt0(_1), numlist(_2), size_lb(M,int(M)), size_lb(_1,int(_1)), size_lb(_2,int(M)), size_ub(M,int(M)), size_ub(_1,int(_1)), size_ub(_2,int(M)) )
         + ( steps_lb(0), steps_ub(- (exp(exp(int(_1),-1)-1,-1)*exp(int(_1),int(M)-1))-2*(exp(exp(int(_1),-1)-1,-1)*exp(int(_1),int(M)))+exp(exp(int(_1),-1)-1,-1)*exp(int(_1),-1)+2*exp(exp(int(_1),-1)-1,-1)+exp(int(_1),int(M))) ).

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
        => ( num(N), num(M), size(lb,N,int(N)), size(lb,M,1) )
         + ( cost(lb,giunif,0), cost(lb,gounif,0), cost(lb,nargs,0), cost(lb,res_steps,0), cost(lb,steps,0), cost(lb,viunif,0), cost(lb,vounif,0) ).

:- true pred choose(N,M)
         : ( num(N), term(M) )
        => ( num(N), num(M), size(ub,N,int(N)), size(ub,M,int(N)+1) )
         + ( cost(ub,giunif,4*int(N)), cost(ub,gounif,0), cost(ub,nargs,4*int(N)), cost(ub,res_steps,2*int(N)), cost(ub,steps,2*int(N)), cost(ub,viunif,2*int(N)), cost(ub,vounif,2*int(N)) ).

:- true pred choose(N,M)
         : ( num(N), term(M) )
        => ( num(N), num(M), size_lb(N,int(N)), size_lb(M,1), size_ub(N,int(N)), size_ub(M,int(N)+1) )
         + ( steps_lb(0), steps_ub(2*int(N)) ).

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


