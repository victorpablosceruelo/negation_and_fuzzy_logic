:- module(_1,[fib/2],[assertions,regtypes,ciaopp(tests(resources)),predefres(res_arith),res_arith(res_arith_each),nativeprops,basicmodes]).

:- doc(author,"Edison Mera").

:- doc(module,"This program calculate the N-th fibonacci number.").

:- doc(summary,"This consider arithmetics operator by operator,
	which is slower.").

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
         + ( cost(lb,arith,0), cost(lb,'arith_operator_#/2',0), cost(lb,'arith_operator_**/2',0), cost(lb,'arith_operator_*/2',0), cost(lb,'arith_operator_++/1',0), cost(lb,'arith_operator_+/1',0), cost(lb,'arith_operator_+/2',0), cost(lb,'arith_operator_--/1',0), cost(lb,'arith_operator_-/1',0), cost(lb,'arith_operator_-/2',0), cost(lb,'arith_operator_///2',0), cost(lb,'arith_operator_//2',0), cost(lb,'arith_operator_/\\/2',0), cost(lb,'arith_operator_<</2',0), cost(lb,'arith_operator_>>/2',0), cost(lb,'arith_operator_\\//2',0), cost(lb,'arith_operator_\\/1',0), cost(lb,'arith_operator_abs/1',0), cost(lb,'arith_operator_atan/1',0), cost(lb,'arith_operator_ceiling/1',0), cost(lb,'arith_operator_cos/1',0), cost(lb,'arith_operator_exp/1',0), cost(lb,'arith_operator_float/1',0), cost(lb,'arith_operator_float_fractional_part/1',0), cost(lb,'arith_operator_float_integer_part/1',0), cost(lb,'arith_operator_floor/1',0), cost(lb,'arith_operator_gcd/2',0), cost(lb,'arith_operator_integer/1',0), cost(lb,'arith_operator_log/1',0), cost(lb,'arith_operator_mod/2',0), cost(lb,'arith_operator_rem/2',0), cost(lb,'arith_operator_round/1',0), cost(lb,'arith_operator_sign/1',0), cost(lb,'arith_operator_sqrt/1',0), cost(lb,'arith_operator_truncate/1',0) ).

:- true pred fib(X,Y)
         : ( num(X), var(Y) )
        => ( num(X), num(Y), size(ub,X,int(X)), size(ub,Y,0.4472135954999579*exp(1.618033988749895,int(X))-0.4472135954999579*exp(-0.6180339887498949,int(X))) )
         + ( cost(ub,arith,278.7574664383562*exp(1.618033988749895,int(X))+106.4758775616439*exp(-0.6180339887498949,int(X))-385.2333440000001), cost(ub,'arith_operator_#/2',0), cost(ub,'arith_operator_**/2',0), cost(ub,'arith_operator_*/2',0), cost(ub,'arith_operator_++/1',0), cost(ub,'arith_operator_+/1',0), cost(ub,'arith_operator_+/2',0.7236067977499789*exp(1.618033988749895,int(X))+0.2763932022500211*exp(-0.6180339887498949,int(X))-1.0), cost(ub,'arith_operator_--/1',0.7236067977499789*exp(1.618033988749895,int(X))+0.2763932022500211*exp(-0.6180339887498949,int(X))-1.0), cost(ub,'arith_operator_-/1',0), cost(ub,'arith_operator_-/2',0.7236067977499789*exp(1.618033988749895,int(X))+0.2763932022500211*exp(-0.6180339887498949,int(X))-1.0), cost(ub,'arith_operator_///2',0), cost(ub,'arith_operator_//2',0), cost(ub,'arith_operator_/\\/2',0), cost(ub,'arith_operator_<</2',0), cost(ub,'arith_operator_>>/2',0), cost(ub,'arith_operator_\\//2',0), cost(ub,'arith_operator_\\/1',0), cost(ub,'arith_operator_abs/1',0), cost(ub,'arith_operator_atan/1',0), cost(ub,'arith_operator_ceiling/1',0), cost(ub,'arith_operator_cos/1',0), cost(ub,'arith_operator_exp/1',0), cost(ub,'arith_operator_float/1',0), cost(ub,'arith_operator_float_fractional_part/1',0), cost(ub,'arith_operator_float_integer_part/1',0), cost(ub,'arith_operator_floor/1',0), cost(ub,'arith_operator_gcd/2',0), cost(ub,'arith_operator_integer/1',0), cost(ub,'arith_operator_log/1',0), cost(ub,'arith_operator_mod/2',0), cost(ub,'arith_operator_rem/2',0), cost(ub,'arith_operator_round/1',0), cost(ub,'arith_operator_sign/1',0), cost(ub,'arith_operator_sqrt/1',0), cost(ub,'arith_operator_truncate/1',0) ).

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


