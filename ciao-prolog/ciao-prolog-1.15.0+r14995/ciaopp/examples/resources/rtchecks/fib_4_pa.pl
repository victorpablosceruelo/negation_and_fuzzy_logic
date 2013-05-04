:- module(_1,[fib/2],[assertions,regtypes,nativeprops,ciaopp(examples(resources(rtchecks))),basicmodes,rtchecks]).

:- doc(author,"Edison Mera").

:- doc(module,"This program calculates the N-th fibonacci number.").

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
         + ( possibly_fails, not_covered ).

:- true pred fib(X,Y)
         : ( num(X), var(Y) )
        => ( num(X), num(Y), size(lb,X,int(X)), size(lb,Y,0.4472135954999579*exp(1.618033988749895,int(X))-0.4472135954999579*exp(-0.6180339887498949,int(X))) )
         + cost(lb,ticks,0).

:- true pred fib(X,Y)
         : ( num(X), var(Y) )
        => ( num(X), num(Y), size(ub,X,int(X)), size(ub,Y,0.4472135954999579*exp(1.618033988749895,int(X))-0.4472135954999579*exp(-0.6180339887498949,int(X))) )
         + cost(ub,ticks,1047.679575866031*exp(1.618033988749895,int(X))+400.1779886617496*exp(-0.6180339887498949,int(X))-1086.26003378194).

fib(0,0) :- !.
fib(1,1) :- !.
fib(M,N) :-
        M>1,
        M1 is M-1,
        M2 is M-2,
        fib(M1,N1),
        fib(M2,N2),
        N is N1+N2.


