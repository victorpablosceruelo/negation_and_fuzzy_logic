:- module(mmat,
	[
	    seq/2,
	    par/2,
	    par_nondet/2,
	    data/1
	],
	[fsyntax, andprolog_nd]).


:- use_module(library(lists), [append/3, length/2]).
:- use_module(library(random)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

size(50).

data(X) :- size(N), gen_list(N,N,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

seq(L,X) :-  mmatrix_seq(L,L,X).
par(L, X) :-  mmatrix_par(L,L,X).
par_nondet(L,X) :- mmatrix_par_nondet(L,L,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mmatrix_seq([],_,[]).
mmatrix_seq([R|RR],X,[Res|RRes]):-
        multiply_seq(X,R,Res),
        mmatrix_seq(RR,X,RRes).

multiply_seq([],_,[]).
multiply_seq([V0|Rest], V1, [Result|Others]):-
        vmul(V0,V1,Result),
        multiply_seq(Rest, V1, Others).

vmul([],[],0).
vmul([H1|T1], [H2|T2], Result):-
        vmul(T1,T2, Newresult),
        Product is H1*H2,
        Result is Product+Newresult.

mmatrix_par([],_,[]).
mmatrix_par([R|RR],X,[Res|RRes]):-
        multiply_par(X,R,Res) '&!'
        mmatrix_par(RR,X,RRes).

multiply_par([],_,[]).
multiply_par([V0|Rest], V1, [Result|Others]):-
        vmul(V0,V1,Result) '&!'
        multiply_par(Rest, V1, Others).

mmatrix_par_nondet([],_,[]).
mmatrix_par_nondet([R|RR],X,[Res|RRes]):-
        multiply_par_nondet(X,R,Res) &
        mmatrix_par_nondet(RR,X,RRes).

multiply_par_nondet([],_,[]).
multiply_par_nondet([V0|Rest], V1, [Result|Others]):-
        vmul(V0,V1,Result) &
        multiply_par_nondet(Rest, V1, Others).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- fun_eval arith(true).

:- fun_eval gen_list/2.
gen_list(0,_) := [].
gen_list(X,Y) := [~gen_list_(Y)|gen_list(X-1,Y)] :- X > 0.

:- fun_eval gen_list_/1.
gen_list_(0) := [].
gen_list_(X) := [X|~gen_list_(X-1)] :- X > 0.
