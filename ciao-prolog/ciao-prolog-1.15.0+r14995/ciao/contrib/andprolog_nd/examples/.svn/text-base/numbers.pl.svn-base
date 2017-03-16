:- module(numbers,
	[
	    seq/2,
	    par_nondet/2,
	    data/1
	],
	[andprolog_nd]).

:- use_module(library(lists), [length/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

n(10000).
l([1,7,8,3,8]).
gc(3).

data([N,L]) :- n(N), l(L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

seq([N,L],true) :- numbers_seq(N,_,L).
par_nondet([N,L],true) :- gc(GC), length(L,Size), numbers_par(N,Size,GC,_,L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

numbers_seq(N,E,L) :-
	search_seq(N, E, L), !.

numbers_par(N,S,G,E,L) :-
	search_par(N, S, G, E, L), !.

operate(A, B, C):- C is A + B.
operate(A, B, C):- C is A - B.
operate(A, B, C):- C is B - A.
operate(A, B, C):- C is A * B.
operate(A, B, C):- B =\= 0, C is A / B.
operate(A, B, C):- A =\= 0, C is B / A.


 % search(?X, ?Y, +Z): the number X can be expressed by the expression Y
 % using the numbers in the list Z. As a heuristic rule, addition and
 % multiplication are tried before.

search_seq(X, X, [X]):- !.                  % Green.
search_seq(X, E, L):-
	asim_subset(L, _, La, _, Lb, _),
	search_seq(Xa, A, La),
	search_seq(Xb, B, Lb),
	oper_comm(Xa, Xb, A, B, X, E).
search_seq(X, E, L):-
	asim_subset(L, _, La, _, Lb, _),
	search_seq(Xa, A, La),
	search_seq(Xb, B, Lb),
	oper_nocomm(Xa, Xb, A, B, X, E).

search_par(X, S, GC, E, L) :- 
	S =< GC, !,
	search_seq(X,E,L).
search_par(X, _, _, X, [X]):- !.                  % Green.
search_par(X, S, G, E, L) :-
	    asim_subset(L, S, La, Sa, Lb, Sb),
	    search_par(Xa, Sa, G, A, La) & search_par(Xb, Sb, G, B, Lb),
	    oper_comm(Xa, Xb, A, B, X, E).
search_par(X, S, G, E, L):-
	asim_subset(L, S, La, Sa, Lb, Sb),
	search_par(Xa, Sa, G, A, La) & search_par(Xb, Sb, G, B, Lb),
	oper_nocomm(Xa, Xb, A, B, X, E).

oper_comm(Xa, Xb, A, B, X, A + B):- perform(Xa + Xb, X).
oper_comm(Xa, Xb, A, B, X, A * B):- perform(Xa * Xb, X).

oper_nocomm(Xa, Xb, A, B, X, A - B):- perform(Xa - Xb, X).
oper_nocomm(Xa, Xb, A, B, X, B - A):- perform(Xb - Xa, X).
oper_nocomm(Xa, Xb, A, B, X, A / B):- perform(Xa / Xb, X).
oper_nocomm(Xa, Xb, A, B, X, B / A):- perform(Xb / Xa, X).


 % perform(+Op, ?Res): Op is a rational or an operation between two
 % rationals, and Res is the result (rational) of that operation.

perform(Op, Res):-
	rewrite(Op, Op1),
	reduce(Op1, Res).


 % rewrite(?X, ?Y): X is a rational or an expression involving TWO
 % rationals and Y is the same expression rewrote so that the main
 % functor (if any) is '/'/2.

rewrite(A / B + C / D, (A * D + B * C) / (B * D)).
rewrite(A / B + C, (A + C * B) / B ):- atomic(C).
rewrite(C + A / B, (A + C * B) / B):- atomic(C).
rewrite(A + B, A + B):- atomic(A), atomic(B).
rewrite(A / B - C / D,  (A * D - B * C) / (B * D)).
rewrite(A / B - C, (A - B * C) / B):- atomic(C).
rewrite(C - A / B, (C * B - A) / B):- atomic(C).
rewrite(A - B, A - B):- atomic(A), atomic(B).
rewrite((A / B) * (C / D), (A * C) / (B * D)).
rewrite(A * (B / C), (A * B) / C):- atomic(A).
rewrite((A / B) * C, (A * C) / B):- atomic(C).
rewrite(A * B, A * B):- atomic(A), atomic(B).
rewrite((A / B) / (C / D), (A * D) / (B * C)).
rewrite(A / (B / C), (A * C) / B):- atomic(A).
rewrite((A / B) / C, A / (B * C)):- atomic(C).
rewrite(A / B, A / B):- atomic(A), atomic(B).
rewrite(A, A):- atomic(A).


 %

reduce(_ / Y, _):- 
        0 =:= Y, !, fail.
reduce(X / Y, Z):- !,
	(0 =:= X mod Y ->
	     Z is X // Y;
            Z = X / Y
	).
reduce(X, Y):- Y is X.


 % fac_add(X, Y, Z): X = Y + Z, X, Y and Z natural numbers.

fac_add(X, Y, Z):-
	X2 is X // 2,
	X1 is X - X2,
	fac_add2(X2, X1, Y, Z).
fac_add2(X, Y, X1, Y1):- swap(X, Y, X1, Y1).
fac_add2(X, Y, X1, Y1):-
	X > 0,
	Xa is X - 1,
	Ya is Y + 1,
	fac_add2(Xa, Ya, X1, Y1).


 % fac_mul(X, Y, Z): X = Y * Z, X, Y and Z natural numbers.

fac_mul(X, Y, Z):-
	sqrt(X, X2),
	fac_mul2(X, X2, Y1, Z1),
	swap(Y1, Z1, Y, Z).
fac_mul2(X, Xd, Xd, D):-
	Xd =\= 0,
	0 =:= X mod Xd,
	D is X // Xd.
fac_mul2(X, Xd, Y, Z):-
	Xd > 0,
	Xd1 is Xd - 1,
	fac_mul2(X, Xd1, Y, Z).


 % Horrible integer square root algorithm.

sqrt(1, 1):- !.
sqrt(X, Y):-
	sqrt2(X, X, 0, Y).
sqrt2(X, Max, Min, Y):-
	Med is (Max + Min) // 2,
	sqrt3(X, Max, Med, Min, Y).
sqrt3(_, __, Med,  Min, Min):-
	Med - Min < 1, !.
sqrt3(X, Max, Med, Min, Y):-
	(
	        Med * Med =< X ->
		sqrt2(X, Max, Med, Y)
	;
	        sqrt2(X, Med, Min, Y)
	).

 % Only different numbers are swapped.

swap(X, Y, X, Y).
swap(X, Y, Y, X):- X =\= Y.


 % subset(X, Y, Z): Y and Z are subsets of X. Relative order is preserved.

subset([], [], []).
subset([X|Xs], Ys, [X|Zs]):- subset(Xs, Ys, Zs).
subset([X|Xs], [X|Ys], Zs):- subset(Xs, Ys, Zs).


 % nep_subset(X, Y, Z): Y and Z are non-empty subsets of X

nep_subset(X, Y, Z):-
	Y = [_|_],
	Z = [_|_],
	subset(X, Y, Z).


 % asim_subset(X, Y, Z): Y and Z are non-empty subsets of X and a
 % solution given for Y on backtracking will never be returned for Z

asim_subset([X|Xs], Sx, [X|Ys], Sy, Zs, Sz):-
	Zs = [_|_],
	subset(Xs, Ys, Zs),
	length([X|Xs],Sx),
	length([X|Ys],Sy),
	length(Zs,Sz).


 % select(X, Y, Z): the element X is caught from the list Y to give
 % the list Z.

select(X, [X|Y], Y).
select(X, [_|Y], Z):-
	select(X, Y, Z).

 
 %

 %% member(X, [X|_]).
 %% member(X, [_|L]):- member(X, L).


 % Definition of a natural number.

natural(0).
natural(X):-
	natural(Y),
	X is Y + 1.


 % Only one solution.

% one_express(X, Y, Z):- express(X, Y, Z), !.


 %

% time(T) :- statistics(runtime,[_,T]).


 % Database

 % found(+X, +E, +L): asserts that the expression E, using numbers in L,
 % has value X. The database has an entry for each different list, and
 % the expressions are organised as a binary tree indexed by the
 % number to be obtained.


% found(X, E, List):-
% 	(
% 	        current_fact(found(List, Tree), Ref) ->
% 		    erase(Ref)
% 	;
% 	        Tree = nil
% 	),
% 	insert(Tree, X, E, NewTree),
% 	asserta_fact(found(List, NewTree)).


 % already_found(?X, ?E, ?L): the tuple (X, E, L) has already been
 % found.

% already_found(X, E, List):-
% 	List = [_, _, _|_],
% 	current_fact(found(List,Tree)),
% 	visit(Tree, X, E).


 % impossible(+X, +L): states that the number X can't de obtained from
 % numbers in L. A database of triples (A, B, L) is maintained; each
 % of them mean that the numbers form A to B can't be obtained from
 % the numbers in L.

% impossible(X, L):-
% 	current_fact(impossible(L, A, B), Ref),
% 	impossible2(X, A, B, L, Ref), !.
% impossible(X, L):-
%         asserta_fact(impossible(L, X, X)).

impossible2(X, A, B, _, _):-
	X >= A, X =< B, !.
impossible2(X, A, _B, _L, Ref):-
	X =:= A - 1, !,
	erase(Ref).
% 	asserta_fact(impossible(L, X, B)).
impossible2(X, _A, B, _L, Ref):-
	X =:= B + 1, !,
	erase(Ref).
% 	asserta_fact(impossible(L, A, X)).


 % already_impossible(+X, +L): the number X can't be derived from the
 % ones in  L. This is done by maintaining tuples (A, B, L), which mean "the
 % numbers from A to B can't be obtained from the ones in L".

% already_impossible(X, L):-
% 	current_fact(impossible(L, A, B)),
% 	A =< X, B >= X, !.


 % Clean the whole database.

% clean:-
%         retractall_fact(found(_,_)),
%         retractall_fact(impossible(_,_,_)).


%  % Print the database.

% all:- all_pos, fail.
% all:- all_impos, fail.
% all.


%  % Print all the posibilities found so far

% all_pos:-
% 	nl, write('Possible: '), nl,
% 	already_found(X, E, L),
% 	tab(4), write([X, E, L]), nl,
% 	fail.
% all_pos.


%  % Id. with the not possible ones.

% all_impos:-
% 	nl, write('Impossible: '), nl,
% 	current_fact(impossible(L, A, B)),
% 	tab(4), write([A, B, L]), nl,
% 	fail.
% all_impos.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 % An implementation of balanced trees
 % structure: avl(Key, Value, LeftSon, RightSon, Depth, Balance).
 % Balance is LeftSon depth minus RightSon depth. Since Depth is
 % max(depth(LeftSon),depth(RightSon)) + 1, this information is enough
 % to know the depth of each son.

insert(nil, K, V, avl(K, V, nil, nil, 1, 0)).
insert(avl(K, V, L, R, D, B), K, V, avl(K, V, L, R, D, B)).
insert(avl(K1, V1, Lson, Rson, _, _), K, V, NewTree):-
	K < K1,
	insert(Lson, K, V, NewLeft),
	compose_balance(NewLeft, Rson, Nb),
	balance(Nb, K1, V1, NewLeft, Rson, NewTree).
insert(avl(K1, V1, Lson, Rson, _, _), K, V, NewTree):-
	K > K1,
	insert(Rson, K, V, NewRight),
	compose_balance(Lson, NewRight, Nb),
	balance(Nb, K1, V1, Lson, NewRight, NewTree).


balance(B, K1, V1, L, R, avl(K1, V1, L, R, D, Nb)):-
	B < 2, B > -2, !,
	compose_balance(L, R, Nb),
	compose_depth(L, R, D).

 % Balance left branch.

balance(2, Kb, Vb, avl(Ka, Va, S1, S2, _, Bl), S3, T):-
	(
	        Bl > 0 ->
		    T = avl(Ka, Va, S1, Rt, Dt, 0),
		        Rt = avl(Kb, Vb, S2, S3, Db, Bb),
			    compose_balance(S2, S3, Bb)
	;
	        T = avl(Kc, Vc, Lt, Rt, Dt, 0),
		    Lt = avl(Ka, Va, S1, S4, Dlt, Blt),
		        Rt = avl(Kb, Vb, S5, S3, Db, Brt),
			    S2 = avl(Kc, Vc, S4, S5, _, _),
			        compose_balance(S5, S3, Brt),
				    compose(S1, S4, Dlt, Blt)
	),
	find_depth(S3, D3),
	Dt is D3 + 2,
	Db is D3 + 1.

 % Balance right branch

balance(-2, Kb, Vb, S3, avl(Ka, Va, S2, S1, _, Bl), T):-
	(
	        Bl < 0 ->
		    T = avl(Ka, Va, Lt, S1, Dt, 0),
		        Lt = avl(Kb, Vb, S3, S2, Db, Bb),
			    compose_balance(S3, S2, Bb)
	;
	        T = avl(Kc, Vc, Lt, Rt, Dt, 0),
		    Rt = avl(Ka, Va, S4, S1, Dlt, Blt),
		        Lt = avl(Kb, Vb, S3, S5, Db, Brt),
			    S2 = avl(Kc, Vc, S5, S4, _, _),
			        compose_balance(S5, S3, Brt),
				    compose(S1, S4, Dlt, Blt)
	),
	find_depth(S3, D3),
	Dt is D3 + 2,
	Db is D3 + 1.



compose_balance(A, B, Nb):-
	find_depth(A, Da),
	find_depth(B, Db),
	Nb is Da - Db.

compose_depth(A, B, Nd):-
	find_depth(A, Da),
	find_depth(B, Db),
	(Da > Db -> Nd is Da; Nd is Db).

compose(L, R, D, B):-
	find_depth(L, Dl),
	find_depth(R, Dr),
	B is Dl - Dr,
	(Dl > Dr -> D is Dl; D is Dr).

find_depth(avl(_,_,_,_,Dl,_), Dl).
find_depth(nil, 0).

visit(avl(K, V, _, _, _, _), K, V).
visit(avl(_, _, L, _, _, _), K, V):- visit(L, K, V).
visit(avl(_, _, _, R, _, _), K, V):- visit(R, K, V).


