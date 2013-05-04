
:- module(aiakl,[init_vars/4],[ assertions ]).
:- use_module(library(assertions(native_props)), [indep/2]).
:- use_module(library(sort)).

:- entry init_vars(E1,E2,E1in,E2in)
       : ( ground([E1,E2])
	 , indep(E1in,E2in)
	 , var(E1in)
	 , var(E2in)
	 ).

% test
goal(E1,E2) :- init_vars([[a]=[any]],[[b]=[any]],E1,E2).

init_vars(E1,E2,E1init,E2init) :-
	find_all_vars(E1,Vars1),
	find_all_vars(E2,Vars2),
	intersect(Vars1,Vars2,_X,Notin1,Notin2),
	init_vars2(Notin1,E1,E1init),
	init_vars2(Notin2,E2,E2init).

find_all_vars(E,Vars) :-
	find_all_vars2(E,Vars0),
	sort(Vars0,Vars).

find_all_vars2([],[]).
find_all_vars2([Vars=_Values|Es],AllVars) :-
	append(Vars,AllVars1,AllVars),
	find_all_vars2(Es,AllVars1).

init_vars2(Notin,E,Einit) :-
	init_vars3(Notin,E,Einit0),
	sort(Einit0,Einit).

init_vars3([],E,E).
init_vars3([Var|Vars],E,[[Var]=[unbound]|Es]) :-
	init_vars3(Vars,E,Es).

append([],A,A).   % tid 5. 2.9%
append([A|B],C,[A|D]) :-
	append(B,C,D).

% intersect(As,Bs,Cs,Ds,Es)
% As and Bs are two sorted lists
% the common elements are returned in Cs
% the elements in Bs but not in As are returned in Ds
% the elements in As but not in Bs are returned in Es


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



%% Version comment prompting control for this file.
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "off"
%% End:

