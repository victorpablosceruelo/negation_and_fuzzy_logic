%:- use_module(library(clpfd)).

solve_bck(Goal,K) :- solve_(Goal,K,[]).

solve_([],_K,_ST).
solve_([A|R],K,AncStack) :-
	clause_(A,B,_), % it is a user predicate
	functor(A,F,Ar),
	check_(AncStack,F,Ar,K),
	append(B,['$pop$'|R],NewGoal),
	solve_(NewGoal,K,[F/Ar|AncStack]).
solve_(['$pop$'|R],K,[_|AncStack]) :-
	!,solve_(R,K,AncStack).
solve_([\+A|R],K,AncStack) :- !,
	neg2pos(A,A_p),
	solve_([A_p|R],K,AncStack).
solve_([A|R],K,AncStack) :-
	constraint(A),
	arith_to_clpfd(A,A_p),
	catch(call(A_p),_,fail),
	solve_(R,K,AncStack).

arith_to_clpfd(call(A),call(A_p)) :-
	A =..[F|Args],
	arith_to_clpfd_(F,Args,A_p),!.
arith_to_clpfd(A,A).


arith_to_clpfd_(=,[A,B],'#='(A,B)) :- number(B).
arith_to_clpfd_(\=,[A,B],'#\\='(A,B)).
arith_to_clpfd_(=\=,[A,B],'#\\='(A,B)).
arith_to_clpfd_(\==,[A,B],'#\\='(A,B)).
arith_to_clpfd_(<,[A,B],'#<'(A,B)).
arith_to_clpfd_(>,[A,B],'#>'(A,B)).
arith_to_clpfd_(=<,[A,B],'#=<'(A,B)).
arith_to_clpfd_(>=,[A,B],'#>='(A,B)).


neg2pos(call(A),call(A_neg)) :-
	A =..[F|Args],
	arith_to_clpfd_(F,Args,A_p),!,
	A_p =..[F_p|Args_p],
	negate_clpfd(F_p,Args_p,A_neg).
neg2pos(call(A),call(A_p)) :-
	A =..['=',X,Y],!,
	negate_unif(X,Y,A_p).

negate_clpfd('#=',[A,B],'#\\='(A,B)).
negate_clpfd('#\\=',[A,B],'#='(A,B)).
negate_clpfd('#>=',[A,B],'#<'(A,B)).
negate_clpfd('#=<',[A,B],'#>'(A,B)).
negate_clpfd('#>',[A,B],'#=<'(A,B)).
negate_clpfd('#<',[A,B],'#>='(A,B)).

negate_unif(X,[],'='(X,[_|_])).
negate_unif(X,[_],'='(X,[])).
negate_unif(X,[_],'='(X,[_,_|_])).

check_([],_,_,K) :- K > 0.
check_([F/Ar|As],F,Ar,K) :- !,
	K > 1,
	K1 is K - 1,
	check_(As,F,Ar,K1).
check_([_|As],F,Ar,K) :- 
	check_(As,F,Ar,K).

constraint(A) :- \+ clause_(A,_,_).
