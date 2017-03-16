:- module(caca,_).

% check_all_true(Lc) checks that any list of Lc is simplified to true.
% Lc is a conjunction of disjunctions of disequalities.
check_all_true([]).
check_all_true([Disj|Lc]):-
	simple_disj_true(Disj),
	check_all_true(Lc).

% simple_disj_true(Disj) checks the disjunction of disequalities Disj
% is equivalent to true.
simple_disj_true(Disj):-
	rm_fA_all(Disj,DisjExist),
	\+ unify_neg_disj(DisjExist).

% unify_neg_disj(Disj) negates the disjunction of disequalities Disj.
% Therefore it unifies the disequalies of Disj and operates the conjunction
% of the results of it.
unify_neg_disj([]).
unify_neg_disj([(T1/T2) | DisjExist]):-
	T1=T2,
	unify_neg_disj(DisjExist).

% rm_fA(G1,G2) returns G2 that is G1 eliminating the universal 
% quantification of free variables
rm_fA(Term,Term):-
	ground(Term),!.
rm_fA(Term,Term):-
	var(Term),!.
rm_fA(fA(Var),Var):-
	var(Var),!. % It is not necessary to check it 
rm_fA(Term,Term1):-
	Term=..[Functor|Args],
	rm_fA_all(Args,Args1),
	Term1=..[Functor|Args1].

% rm_fA_all(Args,Args1) its a map using rm_fA/2 over the arguments of Args
rm_fA_all([],[]).
rm_fA_all([Arg|Args],[Arg1|Args1]):-
	rm_fA(Arg,Arg1),
	rm_fA_all(Args,Args1).



