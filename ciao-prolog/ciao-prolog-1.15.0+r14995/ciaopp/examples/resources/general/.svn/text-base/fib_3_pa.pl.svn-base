:- module(_1,[fib/2],[assertions,nativeprops,regtypes,ciaopp(tests(resources)),res_arith(res_arith),basicmodes]).

:- entry fib(X,Y)
         : ( num(X), var(Y) ).

:- true pred fib(X,Y)
         : ( num(X), term(Y) )
        => ( num(X), num(Y) ).

:- true pred fib(X,Y)
         : ( mshare([[Y]]), var(Y), ground([X]) )
        => ground([X,Y]).

:- true pred fib(X,Y)
         : ( num(X), var(Y) )
        => ( num(X), num(Y) )
         + ( not_fails, covered ).

:- true pred fib(X,Y)
         : ( num(X), var(Y) )
        => ( num(X), num(Y), size(lb,X,int(X)), size(lb,Y,0.4472135954999579*exp(1.618033988749895,int(X))-0.4472135954999579*exp(-0.6180339887498949,int(X))) )
         + cost(lb,arith,278.7574664383562*exp(1.618033988749895,int(X))+106.4758775616439*exp(-0.6180339887498949,int(X))-385.2333440000001).

:- true pred fib(X,Y)
         : ( num(X), var(Y) )
        => ( num(X), num(Y), size(ub,X,int(X)), size(ub,Y,0.4472135954999579*exp(1.618033988749895,int(X))-0.4472135954999579*exp(-0.6180339887498949,int(X))) )
         + cost(ub,arith,278.7574664383562*exp(1.618033988749895,int(X))+106.4758775616439*exp(-0.6180339887498949,int(X))-385.2333440000001).

fib(0,0) :- !.
fib(1,1) :- !.
fib(M,N) :-
        M1 is M-1,
        M2 is M-2,
        fib(M1,N1),
        fib(M2,N2),
        N is N1+N2.


