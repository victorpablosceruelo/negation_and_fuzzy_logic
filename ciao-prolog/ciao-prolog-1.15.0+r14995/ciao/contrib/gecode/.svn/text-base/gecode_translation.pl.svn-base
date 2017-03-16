:- use_module(library(gecode(gecode_rt))).

:- use_module(library(lists),[append/3]).
:- use_module(library(write),[print/1]).
:- use_module(engine(attributes)).

% For meta-calls
A .=. B :- '$parseGECODE'(A .=. B).
A .>. B :- '$parseGECODE'(A .>. B).
A .<. B :- '$parseGECODE'(A .<. B).
A .>=. B :- '$parseGECODE'(A .>=. B).
A .=<. B :- '$parseGECODE'(A .=<. B).
A .<>. B :- '$parseGECODE'(A .<>. B).

parseGECODE(E):- 
	'$parseGECODE'(E).

%I can specialized this for X == Y, better than X - Y == 0
'$parseGECODE'(E) :-
	functor(E, F, 2),
	split(E, A1, A2),
 %% 	parseGECODE_arg(A1, V1),
 %%   	parseGECODE_arg(A2, V2),
 %% 	process_e(F, V1, V2).
	expr_simpl(A1-A2,A),
	simpl_merge_expr(A,AS),
	process_a(AS, V),
	process_e(F, V, 0).

split(E, A1, A2) :- arg(1, E, A1), arg(2, E, A2).
 
expr_simpl(Expr, SimplExpr) :-
	(
	    functor(Expr, F, 2) ->
	    split(Expr, Expr1, Expr2),
	    expr_simpl(Expr1, ExprSimpl1),
	    expr_simpl(Expr2, ExprSimpl2),
	    merge_expr(F,ExprSimpl1,ExprSimpl2,SimplExpr1),
	    simpl_merge_expr(SimplExpr1,SimplExpr)
	;
	    (
		var(Expr) ->
		SimplExpr = [item(1,[Expr])]
	    ;
		SimplExpr = [item(Expr,[])]
	    )
	).

simpl_merge_expr([],[]).
simpl_merge_expr([item(0,_)|R],RS) :-
	!,
	simpl_merge_expr(R,RS).
simpl_merge_expr([H|R],[H|RS]) :-
	simpl_merge_expr(R,RS).

merge_expr(+,[],E,E).
merge_expr(+,[item(N,Vars)|ResE1],E2,[item(Sum,Vars)|RSimplExpr]) :-
	get_member(item(M,Vars),E2,ResE2),
	Sum is N + M,
	merge_expr(+,ResE1,ResE2,RSimplExpr).

merge_expr(-,E1,E2,E) :-
	make_opposite(E2,Op_E2),
	merge_expr(+,E1,Op_E2,E).

merge_expr(*,E1,E2,E_Merge) :-
	make_mult(E1,E2,E),
	simpl_mult(E,E_Merge).

get_member(item(0,_),[],[]) :- !.
get_member(item(N,Vars1),[item(N,Vars2)|R],R) :- 
	Vars1 == Vars2, !.

get_member(item(N,Vars1),[item(M,Var2)|R],[item(M,Var2)|ResR]) :- 
	get_member(item(N,Vars1),R,ResR).

make_opposite([],[]).
make_opposite([item(N,Vars)|R1],[item(M,Vars)|R2]) :-
	M is N * (-1),
	make_opposite(R1,R2).

simpl_mult([],[]).
simpl_mult([item(N,Vars)|ResM],[item(Sum,Vars)|ResSM]) :-
	get_members(ResM,item(N,Vars),ResMRes,M),
	Sum is N + M,
	simpl_mult(ResMRes,ResSM).

get_members([],_,[],0) :- !.
get_members([item(N1,Vars2)|R],item(_,Vars1),ResSM,M) :- 
	Vars1 == Vars2, !,
	get_members(R,item(_,Vars1),ResSM,N2),
	M is N1 + N2.

get_members([item(M,Var2)|R],item(_,Vars1),[item(M,Var2)|ResR],N) :- 
	get_members(R,item(_,Vars1),ResR,N).

make_mult([],_,[]).
make_mult([item(N,Vars)|R],E,Mult) :-
	make_mult_factor(E,item(N,Vars),F1),
	make_mult(R,E,RMult),
	append(F1,RMult,Mult).

make_mult_factor([],_,[]) :- !.
make_mult_factor([item(N1,Vars1)|R],item(N2,Vars2),[item(M,Vars)|RMF]) :-
	append(Vars1,Vars2,VarsTmp),
	simpl_list(VarsTmp,Vars,Repet),
	M is Repet * N1 * N2,
	make_mult_factor(R,item(N2,Vars2),RMF).

simpl_list([],[],1).
simpl_list([X|R],[Min|L3],N) :-
	get_minimun(R,X,Min),
	get_member_list([X|R],Min,L2,N1),
	simpl_list(L2,L3,N2),
	N is N2 * N1.

get_minimun([],Min,Min).
get_minimun([H|R],Min1,Min) :-
	get_attribute(H,'$gecode_id'(HId,_)),
	get_attribute(Min1,'$gecode_id'(Min1Id,_)),
	get_min(HId,Min1Id,H,Min1,Min2),
	get_minimun(R,Min2,Min).

get_min(V1,V2,X,_,X) :-
	V1 < V2, !.
get_min(_,_,_,Y,Y).

get_member_list([],_,[],0).
get_member_list([H|R],X,RR,M) :-
	get_attribute(H,'$gecode_id'(HId,_)),
	get_attribute(X,'$gecode_id'(XId,_)),
	HId = XId, !,
	get_member_list(R,X,RR,N),
	M is N + 1.
get_member_list([H|R],X,[H|RR],N) :-
	get_member_list(R,X,RR,N).

process_a([], 0) :- !.
process_a([item(N,Vars)|R], V) :- 
	process_a(R,V2),
	process_m(Vars,N,V1),
	'$add'(V1, V2, V).

process_m([], N, N).
process_m([V1|R], N, V) :- 
	process_m(R, N, V2),
	'$mul'(V1, V2, V).

process_e(.=.,V1, V2) :- '$.=.'(V1,V2).
process_e(.>., V1, V2) :- '$.>.'(V1, V2).
process_e(.<., V1, V2) :- '$.<.'(V1, V2).
process_e(.=<., V1, V2) :- '$.=<.'(V1, V2).
process_e(.>=., V1, V2) :- '$.>=.'(V1, V2).
process_e(.<>., V1, V2) :- '$.<>.'(V1, V2).
 
do_in([],_).
do_in(ListOfVars, Domain) :-
	ListOfVars = [X|Xs],
	X in Domain,
	Xs in Domain.

ListOfVars in Domain :- 
	nonvar(ListOfVars), 
	list(ListOfVars), !,
	do_in(ListOfVars, Domain).

X in M .. N :- 
	'$in'(X, [[M|N]]).

all_different([X|Xs]) :-
	all_different_aux(Xs, X),
	all_different(Xs).
all_different([]) .

all_different_aux([X|Xs], V) :-
	'$.<>.'(V, X), 
	all_different_aux(Xs, V).
all_different_aux([],_) .

