:- module(_miprueba,[par/1,no_par/1],ciaopp).

:- use_package(.(finite)).
:- use_package(.(cnegf)).

%:-use_module(.(opt_cnegf)).

:- true pred par(A)
         : term(A)
        => ( nnegint(A), size_ub(A,int(A)) )
         + ( not_fails, not_covered, steps_ub(1) ).

:- true pred par(A)
         : mshare([[A]])
        => ground(A).

par(2).

par(4).

:- true pred no_par(A)
         : term(A)
        => ( nnegint(A), size_ub(A,int(A)) )
         + ( possibly_fails, not_covered ).

:- true pred no_par(A)
         : mshare([[A]])
        => ground(A).

no_par(4) :-
        par(2).

multi(0,22).

multi(_1,Y) :-
        multi(0,Y).

[].

%%:- regtype t34/1.

%%t34(4).

definitiva(2):- neg(par(2)).

no_definitiva(2):- neg(no_par(2)).

