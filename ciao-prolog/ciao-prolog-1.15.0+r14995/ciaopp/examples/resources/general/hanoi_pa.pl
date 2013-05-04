:- module(_1,[hanoi/4],[assertions,nativeprops,regtypes,ciaopp(tests(resources)),library(resdefs(resources_decl)),basicmodes]).

:- load_resource_module(hanoi_res).

:- resource movements.

:- head_cost(ub,movements,'hanoi:head_movements').

:- literal_cost(ub,movements,'hanoi:lit_movements').

:- trust_default+cost(ub,movements,0).

:- impl_defined(move_disks/2).

:- export(elem/1).

:- prop elem/1+regtype.

:- trust comp hanoi(N,A,B,C)
         + ( size_metric(A,void), size_metric(B,void), size_metric(C,void) ).

:- entry hanoi(A,B,C,D)
         : ( num(A), elem(B), elem(C), elem(D) ).

:- true pred hanoi(N,A,B,C)
         : ( num(N), elem(A), elem(B), elem(C) )
        => ( num(N), elem(A), elem(B), elem(C) ).

:- true pred hanoi(N,A,B,C)
         : ground([N,A,B,C])
        => ground([N,A,B,C]).

:- true pred hanoi(N,A,B,C)
         : ( num(N), elem(A), elem(B), elem(C) )
        => ( num(N), elem(A), elem(B), elem(C) )
         + ( not_fails, covered ).

:- true pred hanoi(N,A,B,C)
         : ( num(N), elem(A), elem(B), elem(C) )
        => ( num(N), elem(A), elem(B), elem(C), size(ub,N,int(N)), size(ub,A,0), size(ub,B,0), size(ub,C,0) )
         + cost(ub,movements,inf).

hanoi(1,A,_1,C) :-
        move_disks(A,C),
        !.
hanoi(N,A,B,C) :-
        N1 is N-1,
        hanoi(N1,A,C,B),
        hanoi(N1,B,A,C),
        move_disks(A,C).

:- prop elem(_1)
         + regtype.

:- true pred elem(_1)
         : term(_1)
        => elem(_1).

:- true pred elem(_1)
         : mshare([[_1]])
        => ground([_1]).

:- true pred elem(_1)
         : term(_1)
        => elem(_1)
         + ( possibly_fails, not_covered ).

elem(a).
elem(b).
elem(c).


