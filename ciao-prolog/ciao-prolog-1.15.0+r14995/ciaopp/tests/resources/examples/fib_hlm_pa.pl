:- module(_1,[fib/2],[assertions,regtypes,ciaopp(tests(resources)),nativeprops,basicmodes]).

:- doc(author,"Edison Mera").

:- doc(module,"This program calculate the N-th fibonacci number.").

:- doc(summary,"This consider arithmetics operator by operator,
	which is slower.").

:- resource exectime_model4.

:- compound_resource(exectime_model4,[arith,exectime_hlm_63]).

:- trust_default+cost(ub,exectime_model4,0).

:- trust_default+cost(lb,exectime_model4,0).

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
         + cost(lb,exectime_model4,0).

:- true pred fib(X,Y)
         : ( num(X), var(Y) )
        => ( num(X), num(Y), size(ub,X,int(X)), size(ub,Y,0.4472135954999579*exp(1.618033988749895,int(X))-0.4472135954999579*exp(-0.6180339887498949,int(X))) )
         + cost(ub,exectime_model4,1079.549525717058*exp(1.618033988749895,int(X))+412.3512262850874*exp(-0.6180339887498949,int(X))-1114.766846055425).

:- true pred fib(X,Y)
         : ( num(X), var(Y) )
        => ( num(X), num(Y), size_lb(X,int(X)), size_lb(Y,0.4472135954999579*exp(1.618033988749895,int(X))-0.4472135954999579*exp(-0.6180339887498949,int(X))), size_ub(X,int(X)), size_ub(Y,0.4472135954999579*exp(1.618033988749895,int(X))-0.4472135954999579*exp(-0.6180339887498949,int(X))) )
         + ( steps_lb(0), steps_ub(1.447213595499958*exp(1.618033988749895,int(X))+0.5527864045000421*exp(-0.6180339887498949,int(X))-1.0) ).

fib(0,0) :- !.
fib(1,1) :- !.
fib(M,N) :-
        M>1,
        M1 is M-1,
        M2 is M-2,
        fib(M1,N1),
        fib(M2,N2),
        N is N1+N2.


