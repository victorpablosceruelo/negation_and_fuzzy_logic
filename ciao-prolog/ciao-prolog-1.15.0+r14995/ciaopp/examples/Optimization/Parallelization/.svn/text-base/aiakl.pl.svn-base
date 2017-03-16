/*-----------------------------------------------------------------
Program: initialization for abstract unification in AKL analyzer
Author:  Dan Sahlin and Thomas Sjoland
Date:	 March, 1993
-----------------------------------------------------------------*/

:- entry(init_vars(E1,E2,E1in,E2in),
         [var([E1in,E2in]),mshare([[E1in],[E2in]])]).
%:- entry(init_vars(E1,E2,E1in,E2in),
%          [var([E1in,E2in]),mshare([[E1],[E2],[E1in],[E2in]])]).

init_vars(E1,E2,E1init,E2init) :-
	find_all_vars(E1,Vars1),
	find_all_vars(E2,Vars2),
	intersect(Vars1,Vars2,_X,Notin1,Notin2),
	init_vars2(Notin1,E1,E1init),
	init_vars2(Notin2,E2,E2init).

find_all_vars(E,Vars) :-
	find_all_vars2(E,Vars0),
	qsort(Vars0,Vars).

find_all_vars2([],[]).
find_all_vars2([Vars=_Values|Es],AllVars) :-
	append(Vars,AllVars1,AllVars),
	find_all_vars2(Es,AllVars1).

init_vars2(Notin,E,Einit) :-
	init_vars3(Notin,E,Einit0),
	qsort(Einit0,Einit).

init_vars3([],E,E).
init_vars3([Var|Vars],E,[[Var]=[unbound]|Es]) :-
	init_vars3(Vars,E,Es).

append([],A,A).   % tid 5. 2.9%
append([A|B],C,[A|D]) :-
	append(B,C,D).

intersect(As,[],[],[],As) :- !.
intersect([],Bs,[],Bs,[]) :- !.
intersect([A|As],[B|Bs],Cs,Ds,Es) :-
	(A=B ->
		Cs = [A|Cs2],
		intersect(As,Bs,Cs2,Ds,Es)
	;A@<B ->
		Es = [A|Es2],
		intersect(As,[B|Bs],Cs,Ds,Es2)
	;       Ds = [B|Ds2],
		intersect([A|As],Bs,Cs,Ds2,Es)
	).

partition(F,[],[],[]).
partition(F,[X|Y],[X|Y1],Y2) :- 
	X @=< F, 
	partition(F,Y,Y1,Y2).
partition(F,[X|Y],Y1,[X|Y2]) :- 
	X @> F,
	partition(F,Y,Y1,Y2). 

qsort(X,Y):- qsort(X,Y,[]).

qsort([],X,X).
qsort([First|L1],L2,T) :-
	partition(First,L1,Ls,Lg), 
	qsort1(First,Lg,Lg2,T,T1),
	qsort(Ls,L2,T1).

qsort1(First,Lg,Lg2,T,T1):-
	T1=[First|Lg2],
	qsort(Lg,Lg2,T).

end(X,X).

