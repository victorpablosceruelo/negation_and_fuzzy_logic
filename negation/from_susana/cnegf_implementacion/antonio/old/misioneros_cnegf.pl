:- module(misioneros_cnegf, _,[.(cnegf)]).




%%%%%%%%%%%%MISIONEROS%%%%%%%%%%%%%%%%%
accion(cruzar(C,M)):-
	 (C=0;C=1;C=2),
	 (M=0;M=1;M=2),
	 C+M=<2, C+M>=1.

posible(cruzar(C,M),estado(Ci,Mi,_,_,izq)):-
	C=<Ci,M=<Mi.
posible(cruzar(C,M),estado(_,_,Cd,Md,der)):-
	C=<Cd,M=<Md.

resulta(E1,cruzar(C,M),E2):-
	posible(cruzar(C,M),E1),
	E1 = estado(Ci,Mi,Cd,Md,izq),
	Cip is Ci-C,
	Mip is Mi-M,
	Cdp is Cd+C,
	Mdp is Md+M,
	E2 = estado(Cip,Mip,Cdp,Mdp,der).

resulta(E1,cruzar(C,M),E2):-
	posible(cruzar(C,M),E1),
	E1 = estado(Ci,Mi,Cd,Md,der),
	Cip is Ci+C,
	Mip is Mi+M,
	Cdp is Cd-C,
	Mdp is Md-M,
	E2 = estado(Cip,Mip,Cdp,Mdp,izq).

generar_plan(L):-
	inicial(I),
	final(F),
	generar_plan_posible(I,L,F).

generar_plan_posible(I,[],I).
generar_plan_posible(I,[X|Xs],F):-
	generar_plan_posible(I,Xs,Intermedio),
	accion(X), resulta(Intermedio,X,F), seguro(F).

seguro(estado(Ci,Mi,Cd,Md,_)):-
	(Ci=<Mi; Mi=0),
	(Cd=<Md; Md=0).

inicial(estado(3,3,0,0,izq)).
final(estado(0,0,3,3,der)).

no_generar_plan(L):- cnegf(generar_plan(L)).

%%%%%%%%%MISIONEROS FINITO%%%%%%%%%%%%%%%%%%%%%
accion1(cruzar1(C,M)):-
	 (C=0;C=1;C=2),
	 (M=0;M=1;M=2),
	 C+M=<2, C+M>=1.

posible1(cruzar1(C,M),estado1(Ci,Mi,_,_,izq)):-
	C=<Ci,M=<Mi.
posible1(cruzar1(C,M),estado1(_,_,Cd,Md,der)):-
	C=<Cd,M=<Md.

resulta1(E1,cruzar1(C,M),E2):-
	posible1(cruzar1(C,M),E1),
	E1 = estado1(Ci,Mi,Cd,Md,izq),
	Cip is Ci-C,
	Mip is Mi-M,
	Cdp is Cd+C,
	Mdp is Md+M,
	E2 = estado1(Cip,Mip,Cdp,Mdp,der).

resulta1(E1,cruzar1(C,M),E2):-
	posible1(cruzar1(C,M),E1),
	E1 = estado1(Ci,Mi,Cd,Md,der),
	Cip is Ci+C,
	Mip is Mi+M,
	Cdp is Cd-C,
	Mdp is Md-M,
	E2 = estado1(Cip,Mip,Cdp,Mdp,izq).

generar_plan1(L):-
	inicial1(I),
	final1(F),
	generar_plan_posible1(I,L,F),
	longitud(L,0,N),
	(N=5;(dist(N,5),!,fail)).% solo saco los de longitud 5
%generar_plan1(_L):- fail,!.

longitud([],N,Res):- Res=N.
longitud([_X|Xs],N,Res):-
	N1 is N+1,
	longitud(Xs,N1,Res).
generar_plan_posible1(I,[],I).
generar_plan_posible1(I,[X|Xs],F):-
	generar_plan_posible1(I,Xs,Intermedio),
	accion1(X), resulta1(Intermedio,X,F), seguro1(F).

seguro1(estado1(Ci,Mi,Cd,Md,_)):-
	(Ci=<Mi; Mi=0),
	(Cd=<Md; Md=0).

inicial1(estado1(2,2,0,0,izq)).
final1(estado1(0,0,2,2,der)).

no_generar_plan1(L):- cnegf(generar_plan1(L)).
