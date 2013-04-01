
:- module(intneg_aux, [ term_vars/2, terms_are_equal/2,
			is_a_list/1, list_length/2,
			list_to_conj/2, conj_to_list/2,
			memberchk/2,
			remove_repeated_terms/2,
			var_is_in_term/2,
			term_is_forall/1, term_is_not_forall/1,
			find_equal_head_name_cls/4,
			transpose_matrix/3,
			convert_clauses_format/3,
			revert_clauses_format/2,
			obtain_clause_head/2,
			obtain_clause_cj/2,
			obtain_clause_arity/2,
			obtain_clause_body/2,
			joint_bodies_with_op_and/3,
			joint_bodies_with_op_or/3,
			head_and_tail/3,
			debug_cls/2,
			debug_formatted_cl/2,
			debug_formatted_cls/2,
			debug_separation/0,
			mgu/3, clause_head/4,
			intneg_or/3, intneg_and/3,
			intneg_chk/2, intneg_special/3,
			intneg_deMorgan/3,
			wfs_memberchk/3, wfs_memberchk_for_pos/1,
			remove_vars_in_2nd_list/3,
			adequate_forall_results/3,
			new_counter_index/1]).

% var_esta_en_formula/2, eq/2,
% :- import append/3 from basics.
:- use_module(library(lists),[append/3]).

% A separate counter to distinguish versions.
:- dynamic counter/1.

% To save forall answers.
:- dynamic forall_answers/2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

adequate_forall_results(Cte, Result_In, Results) :-
	copy_term(Result_In, Result),
	assertz(forall_answers(Cte, Result)),
	findall(Tmp_Result,(retract(forall_answers(Cte, Tmp_Result))), Results),
	assert_results_again(Cte, Results),
	debug_formatted_cl('Forall index is ', Cte),
	debug_formatted_cls('Forall accumuled results', Results).

assert_results_again(_Cte, []) :- !.
assert_results_again(Cte, [Result|Results]) :-
	assertz(forall_answers(Cte, Result)),
	assert_results_again(Cte, Results).


new_counter_index(Index) :-
	retract(counter(X)), !,
	Index is X + 1,
	assertz(counter(Index)).
new_counter_index(1) :-
	assertz(counter(1)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% is_a_list(Term) es cierto si Term es una lista
is_a_list(Term) :-
	var(Term), !, fail.
is_a_list(Term) :- 
	functor(Term, '.', 2), 
	Term=..['.'|_Args], !.

% Old way (It does not work)
% is_a_list([]).
% is_a_list([_X|_Y]).

list_length([], 0) :- !.
list_length([_E|L], N) :-
	list_length(L, N_Aux),
	N is N_Aux + 1.

%list_length(L, N) :-
%	is_a_list(L),
%	list_length_aux(L, 0, N).
%list_length_aux([], N, N) :- !.
%list_length_aux([_Elto], N1, N2) :- !,
%	N2 is N1 + 1.
%list_length_aux([_Elto|Others], N1, N2) :- !,
%	N3 is N1 + 1,
%	list_length_aux(Others, N3, N2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Transforma una lista de elementos, en una conjuncion de los
% elementos de la lista.
% Transform a element's list, in a conjuntion of elements from the
% list.
list_to_conj([A|ListB],(A,B)):-
	list_to_conj(ListB,B),!.
list_to_conj([A],A).

% conj_to_list(Conj,List) provides in List the elements of
% the conjunction Conj
conj_to_list((A,B),[A|ListB]) :- !,
	conj_to_list(B,ListB).
conj_to_list(A,[A]) :- !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clause_head(Head, Name, Arity, Args) :-
	functor(Head, Name, Arity),
	Arity > 0, !,
	Head=..[Name|Args].

clause_head(Head, Name, Arity, []) :-
	functor(Head, Name, Arity),
	Arity == 0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%head([], _) :- fail.
head_and_tail([Elto|Others], Elto, Others).

%tail([], []).
%tail([_Elto|Others], Others).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% varset(X, Y) genera una lista con todas las variables de X.

term_vars(Term, List) :- term_vars_aux(Term, [], List).

term_vars_aux(Term, L,  [Term|L]) :- var(Term), !. % Term
term_vars_aux(Term, L1, L2) :- 
	clause_head(Term, _Name, _Arity, Args),
	term_is_not_forall(Term), % fA vars are not of use.
	term_vars_aux_list(Args, L1, L2), !.
term_vars_aux(Term, L, L) :- 
	term_is_forall(Term). % fA vars are not of use.

%term_vars_aux(X, L1, L1) :-
%	nl,
%	write('I do not know if it this term has variables: '),
%	write(X), nl.

term_vars_aux_list([], L1, L1) :- !. % Empty list.
term_vars_aux_list([X], L1, L2) :- !,
	term_vars_aux(X, L1, L2).
term_vars_aux_list([X|Others], L1, L3) :- !,
	term_vars_aux(X, L1, L2),
	term_vars_aux_list(Others, L2, L3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% memberchk en XSB prolog unifica, cosa q en Ciao no hace.
% es por eso q este predicado esta redefinido aqui.

memberchk(T1, [T2|_L]) :- 
	terms_are_equal(T1,T2), !.
%	write('% DBG % '),
%	write(terms_are_equal(T1,T2)), nl.
memberchk(T1, [_T2|L]) :- !,
	memberchk(T1, L).

remove_repeated_terms([], []) :- !.
remove_repeated_terms([Term], [Term]) :- !.
remove_repeated_terms([Term|L1], L2) :-
	memberchk(Term, L1), !,
	remove_repeated_terms(L1, L2).
remove_repeated_terms([Term|L1], [Term|L2]) :- !,
	remove_repeated_terms(L1, L2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

terms_are_equal(T1, T2) :- % Same var
        var(T1),
        var(T2),
        T1==T2,!.
terms_are_equal(T1, T2) :- % Different vars.
        (   var(T1) ;
	    var(T2)),
        !, fail. % Fail

terms_are_equal(T1, T2) :- % For functors.
	clause_head(T1, Name, Arity, Args1),
	clause_head(T2, Name, Arity, Args2),
	!,
	terms_are_equal_list(Args1, Args2).

terms_are_equal(_T1, _T2) :- % Other things
%	write(terms_are_equal(T1, T2)), write(' ERROR '), nl,
	!, fail. % Fail

terms_are_equal_list([],[]) :- !. % Empty lists.
terms_are_equal_list([T1|L1],[T2|L2]) :- !, % Lists.
	terms_are_equal(T1, T2),
	terms_are_equal_list(L1,L2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

var_is_in_term(Var, Term) :-
	var(Var),
	var_is_in_term_aux(Var, Term). % Only LHS.
var_is_in_term_aux(Var, Term) :-
	var(Term), !,
	Term == Var, !.
var_is_in_term_aux(Var, Term) :-
	term_is_not_forall(Term),
	clause_head(Term, _Name, _Arity, Args),
	var_is_in_term_list(Var, Args).

var_is_in_term_list(Var, [Arg|Others]) :-
	var_is_in_term_aux(Var, Arg) ;
	var_is_in_term_list(Var, Others).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% code mgu start

mgu(T1, T2, T3):-
	copy_term(T1, T1_Copy), copy_term(T2, T2_Copy),
	unificate_terms(T1_Copy, T2_Copy, T3),
	!. % Backtracking is not allowed.
	
unificate_terms(T1, T2, T2) :- % Both vars.
	var(T1), var(T2), !, T1=T2.

unificate_terms(T1, T2, T2) :- % Var and functor.
	var(T1), functor(T2, _Name, _Arity),
	term_is_not_forall(T2), !, T1=T2, !.

unificate_terms(T1, T2, T1) :- % Var and functor.
	var(T2), functor(T1, _Name, _Arity),
	term_is_not_forall(T1),	!, T2=T1, !.

% Unification with fA returns fA.
unificate_terms(_T1, T2,  T2) :- term_is_forall(T2), !.
unificate_terms( T1, _T2, T1) :- term_is_forall(T1), !.

unificate_terms(T1, T2, _T3) :- % Don't play more with vars.
	(   var(T1) ; var(T2) ), !, fail.

unificate_terms(T1, T2, T3) :-
	clause_head(T1, Name, Arity, Args1),
	clause_head(T2, Name, Arity, Args2),
	unificate_terms_lists(Args1, Args2, Args3),
	clause_head(T3, Name, Arity, Args3).

unificate_terms_lists([], [], []) :- !.
unificate_terms_lists([T1], [T2], [T3]) :- 
	unificate_terms(T1, T2, T3), !.
unificate_terms_lists([T1|L1], [T2|L2], [T3|L3]) :- !,
	unificate_terms(T1, T2, T3),
	unificate_terms_lists(L1, L2, L3).

term_is_not_forall(Term) :- var(Term), !.
term_is_not_forall(Term) :- \+(term_is_forall(Term)), !.

term_is_forall(Term) :- var(Term), !, fail.
term_is_forall(Term) :- clause_head(Term, 'fA', 1, _Args1).

% code mgu end 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% find_and_remove_equal_head_name_cls(Cl, Cls, EqName, NotEqName).
find_equal_head_name_cls(Cl, Cls, EqName, NotEqName) :- !,
	find_equal_head_name_cls_aux_1(Cl, Cls, EqName, NotEqName, 0, _N).

find_equal_head_name_cls_aux_1(_Cl, [], [], [], _N1, _N2) :- !.
find_equal_head_name_cls_aux_1(Cl1, [Cl2], EqName, NotEqName, N1, N2) :-
	!, % Backtracking not allowed.
	find_equal_head_name_cls_aux_2(Cl1, Cl2, EqName, NotEqName, N1, N2).	
find_equal_head_name_cls_aux_1(Cl1, [Cl2 | Cls], EqName, NotEqName, N1, N3) :-
	!, % Backtracking not allowed.
	find_equal_head_name_cls_aux_2(Cl1, Cl2, EqName1, NotEqName1, N1, N2),
	find_equal_head_name_cls_aux_1(Cl1, Cls, EqName2, NotEqName2, N2, N3),
	append(EqName1, EqName2, EqName),
	append(NotEqName1, NotEqName2, NotEqName).

find_equal_head_name_cls_aux_2(Cl1, Cl2, [Cl3], [], N1, N2) :-
	cls_of_same_predicate(Cl1, Cl2, Cl3, N1, N2), !.
find_equal_head_name_cls_aux_2(_Cl1, Cl2, [], [Cl2], N1, N1) :- !.

cls_of_same_predicate(cl(Name, Arity, _Cj1, _NoCj1, _B1, _OldIndex1),
		      cl(Name, Arity,  Cj2,  NoCj2,  B2, _OldIndex2),
		      cl(Name, Arity,  Cj2,  NoCj2,  B2,  NewIndex),
		      OldIndex, NewIndex) :- NewIndex is OldIndex + 1, !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

transpose_matrix([], Res_Classified, Res_Classified) :- !.
transpose_matrix([Res | Other_Res], Res_Classified_In, Res_Classified_Out) :-
	transpose_matrix_aux_1(Res, Res_Classified_In, Res_Classified_Aux),
	transpose_matrix(Other_Res, Res_Classified_Aux, Res_Classified_Out).

transpose_matrix_aux_1(Vars, [], Res_Classified) :- % Empty
	transpose_matrix_aux_2(Vars, Res_Classified). 
transpose_matrix_aux_1(Vars, Res_Classified_In, Res_Classified_Out) :-
	Res_Classified_In \== [], % Non-Empty
	transpose_matrix_aux_3(Vars, Res_Classified_In, Res_Classified_Out).

transpose_matrix_aux_2([], []) :- !. % Build the initial list.
transpose_matrix_aux_2([Var|Other_Vars], [[Var]|Res_Classified]) :- 
	transpose_matrix_aux_2(Other_Vars, Res_Classified).

transpose_matrix_aux_3([], [], []) :- !. % Add the other elements to the list.
transpose_matrix_aux_3([Var|Vars], [L1|Res_L1], [[Var|L1]|Res_L2]) :-
	transpose_matrix_aux_3(Vars, Res_L1, Res_L2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

convert_clauses_format([], [], []) :- !.
convert_clauses_format([Cl], [F_Cl], []) :-
	convert_1_clause_format(Cl, F_Cl), !.
convert_clauses_format([Cl], [], [Cl]) :- !.
convert_clauses_format([Cl | Cls], [F_Cl | F_Cls], F_Invalid_Cls) :-
	convert_1_clause_format(Cl, F_Cl), !,
	convert_clauses_format(Cls, F_Cls, F_Invalid_Cls).
convert_clauses_format([Cl | Cls], F_Cls, [Cl|F_Invalid_Cls]) :- !,
	convert_clauses_format(Cls, F_Cls, F_Invalid_Cls).

convert_1_clause_format((:- _Declaration), fail) :- !, fail.

convert_1_clause_format((H :- B), cl(Name, Arity, Args, [], body((B)), (-1))) :-
	!, % Pred with body.
%	write('% DBG % '),
%	write((H :- B)), nl,
	clause_head(H, Name, Arity, Args).

convert_1_clause_format((H), cl(Name, Arity, Args, [], body(true), (-1))) :-
	!, % Pred without body.
%	write('% DBG % '),
%	write(H), nl,
	clause_head(H, Name, Arity, Args).

% revert_clauses_format(F_OutCls, OutCls) :-
revert_clauses_format(F_OutCls, OutCls) :-
	revert_cls_format_list(F_OutCls, OutCls, 0),
	!. % Backtracking not allowed.
revert_clauses_format(F_OutCls, []) :- !,
	debug_separation,
	write('% DBG % ERROR % revert_clauses_format fails. '), nl,
	debug_formatted_cls('END: ', F_OutCls), nl, nl.

revert_cls_format_list([], [], _N) :- !.
revert_cls_format_list([F_Cl], [Cl], N1) :- !,
	N2 is N1 + 1,
	revert_cl_format(F_Cl, Cl, N2).
revert_cls_format_list([F_Cl|F_Cls], [Cl|Cls], N1) :- !,
	N2 is N1 + 1,
	revert_cl_format(F_Cl, Cl, N2),
	revert_cls_format_list(F_Cls, Cls, N2).

%revert_cls_format(F_Cl, Cl, N),
revert_cl_format(cl(Name, Arity, Cjs, [], Body1, N1), Cl, N2) :-
	include_debug_info_in_body(Body1, Body2, N2),
	revert_cl_format_aux(cl(Name, Arity, Cjs, [], Body2, N1), Cl),
	!. % Backtracking not allowed.

revert_cl_format(Cl_In, 'error', _N) :-
	write('% DBG % ERROR % revert_cl_format fails. Cl_In: '), nl,
	write(Cl_In), nl.

%include_debug_info_in_body(Body1, Body3, N2) :-
%	joint_bodies_with_op_and(Body1, body((write('ends_'), write(N2), nl)), Body2),
%	joint_bodies_with_op_and(body((write('try_'), write(N2), nl)), Body2, Body3).
include_debug_info_in_body(Body, Body, _N). % To cancel debugging.

revert_cl_format_aux(cl(Name, Arity, Cjs, [], body([]), _N), (Head)) :- !, % Empty body
	clause_head(Head, Name, Arity, Cjs).

revert_cl_format_aux(cl(Name, Arity, Cjs, [], body(Body), _N), (Head :- Body)) :-
        Body\==[], !, % Non-empty body.
	clause_head(Head, Name, Arity, Cjs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

obtain_clause_head(cl(Name, Arity, Cj, _NoCjs, _Body, _N), Head) :-
	clause_head(Head, Name, Arity, Cj).

obtain_clause_cj(cl(_Name, _Arity, Cj, _NoCjs, _Body, _N), Cj).
%	write(obtain_clause_cj((_Name1, _Arity1, Cj, _NoCjs, _Body), Cj)), nl.

obtain_clause_arity(cl(_Name, Arity, _Cj, _NoCjs, _Body, _N), Arity).

obtain_clause_body(cl(_Name, _Arity, _Cj, _NoCjs, Body, _N), Body).
%	write(obtain_clause_body(cl(_Name, _Arity, _Cj, _NoCjs, Body, _N), Body)), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

joint_bodies_with_op_and(body([]),    Body2,       Body2) :- !.
joint_bodies_with_op_and(Body1,       body([]),    Body1) :- !.
joint_bodies_with_op_and(body(true),  Body2,       Body2) :- !. % Optimization.
joint_bodies_with_op_and(Body1,       body(true),  Body1) :- !. % Optimization.
joint_bodies_with_op_and(body(fail),  _Body2,      body(fail)) :- !. % Optimization.
joint_bodies_with_op_and(_Body1,      body(fail),  body(fail)) :- !. % Optimization.
joint_bodies_with_op_and(body(Body1), body(Body2), body(((Body1) , (Body2)))) :-
	Body1\==[], Body2\==[], !.

joint_bodies_with_op_or(body([]),    Body2,       Body2) :- !.
joint_bodies_with_op_or(Body1,       body([]),    Body1) :- !.
joint_bodies_with_op_or(body(true),  _Body2,      body(true)) :- !. % Optimization.
joint_bodies_with_op_or(_Body1,      body(true),  body(true)) :- !. % Optimization.
joint_bodies_with_op_or(body(fail),  Body2,       Body2) :- !. % Optimization.
joint_bodies_with_op_or(Body1,       body(fail),  Body1) :- !. % Optimization.
joint_bodies_with_op_or(body(Body1), body(Body2), body(((Body1) ; (Body2)))) :-
	Body1\==[], Body2\==[], !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

debug_formatted_cls(Msg, Cls) :-
%	nl, % Space between last msg and this one.
	write('% DBG % '),
	write(Msg), nl,
	debug_formatted_cls_aux(Cls),
	nl, % Space between this msg and next one.
	!. % Backtracking NOT allowed here.

debug_formatted_cls_aux([]).
debug_formatted_cls_aux([Cl|Cls]) :-
	debug_formatted_cl(' ', Cl),
	debug_formatted_cls_aux(Cls).
debug_formatted_cls_aux(Cl) :-
	debug_formatted_cl('ONE CLAUSE % ', Cl).
debug_formatted_cl(Pre, Cl) :-
	write('% DBG % '),
	write(Pre), 
	write(Cl), nl.
	

debug_separation :-
	write('% DBG % ---------------------------------------------------------'),
	nl, !. % Backtracking NOT allowed here.

debug_cls(_Pre, []).
debug_cls(Pre, [Cl|Cls]) :-
	debug_formatted_cl(Pre, Cl), 
	debug_cls(Pre, Cls).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% remove_vars_in_2nd_list(L1, L2, L3).
remove_vars_in_2nd_list(L1, [], L1) :- !.
remove_vars_in_2nd_list(L1, [Var_2|L2], L3) :-
	remove_vars_in_2nd_list_aux(L1, Var_2, L_Aux), !,
	remove_vars_in_2nd_list(L_Aux, L2, L3).

remove_vars_in_2nd_list_aux([], _Var_2, []) :- !.
remove_vars_in_2nd_list_aux([Var_1|L1], Var_2, L3) :-
	Var_1 == Var_2, !,
	remove_vars_in_2nd_list_aux(L1, Var_2, L3).
remove_vars_in_2nd_list_aux([Var_1|L1], Var_2, [Var_1|L3]) :-
	Var_1 \== Var_2, !,
	remove_vars_in_2nd_list_aux(L1, Var_2, L3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

intneg_or(X, Arg1, Arg2) :-
	functor(X, ';', 2),
	X=..[';', Arg1, Arg2].
intneg_and(X, Arg1, Arg2) :-
	functor(X, ',', 2),
	X=..[',', Arg1, Arg2].

intneg_chk(intneg(X), X).

intneg_special(X, X, _L) :-
	clause_head(X, 'intneg_dist', 2, _Args).
intneg_special(X, Y, L) :-
	clause_head(X, 'intneg_forall', N, Args),
	append(Args, [L], NewArgs),
	NewN is N + 1,
	clause_head(Y, 'intneg_forall', NewN, NewArgs).

%wfs_memberchk(T1, L1, _L2) :-
%	write('Looking for: '), write(T1),
%	write(' in '), write(L1), nl, fail. 
wfs_memberchk(T1, [T2|_L], []) :-
	terms_are_equal(T1,T2),
	write('Found Call Loop.'), nl,
	!.
wfs_memberchk(T1, [T2|L1], [T2|L2]) :-
	!, wfs_memberchk(T1, L1, L2).

%wfs_memberchk_for_pos(L) :-
%	write('Looking for pos(X) in: '), write(L), nl, fail. 
wfs_memberchk_for_pos([pos(_X)|_L]).
wfs_memberchk_for_pos([neg(_X)|L]) :- wfs_memberchk_for_pos(L).

intneg_deMorgan(X_In, X_Out, pos) :-
	intneg_or(X_In, _A, _B), !, 
	intneg_deMorgan_aux(X_In, X_Out).
intneg_deMorgan(X_In, X_Out, pos) :-
	intneg_and(X_In, _A, _B), !, 
	intneg_deMorgan_aux(X_In, X_Out).
intneg_deMorgan(X_In, X_Out, pos) :-
	intneg_chk(X_In, X_Out), !.
intneg_deMorgan(X_In, X_In, neg).

intneg_deMorgan_aux(X_In, X_Out) :- % Or -> And
	intneg_or(X_In, A_In, B_In), !,
	intneg_deMorgan_aux(A_In, A_Out),
	intneg_deMorgan_aux(B_In, B_Out),
	intneg_and(X_Out, A_Out, B_Out).

intneg_deMorgan_aux(X_In, X_Out) :- % And -> Or
	intneg_and(X_In, A_In, B_In), !,
	intneg_deMorgan_aux(A_In, A_Out),
	intneg_deMorgan_aux(B_In, B_Out),
	intneg_or(X_Out, A_Out, B_Out).

intneg_deMorgan_aux(X_In, X_Out) :- % Remove double negation. 
	intneg_chk(X_In, X_Out), !.
intneg_deMorgan_aux(X_In, intneg(X_In)). % Put negation.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

