:- module(size__res,
	    [
		size_clauses/9,
		explicit_output_size/10,
		implicit_output_size/10,
		remove_recursive_comps/4,
% 		remove_time_recursive_comps/4, % EMM
		size_analysis/9,
		up_low_approximation_expr/4,
		min_approximation/3,
		up_low_approximation_minmax/4,
		minimum_list/2
	    ], [assertions, resources(inferres_decl)]).

% December, 1991

:- doc(author, "Nai-Wei Lin").
:- doc(author, "Edison Mera").

:- doc(module, "This file contains the procedures for performing
	the argument size analysis for the predicates in the program
	in topologically sorted order.").

:- use_module(library(messages), [debug_message/2]).
:- use_module(resources(algebraic_res(general_form_res)),
	    [
		time_general_form/2,
		general_form/2
	    ]).
:- use_module(resources(algebraic_res(maxmin_res)), [max_expr/3]).
:- use_module(resources(init_res(symtable_res)),
	    [
		insert_symbol_field/4,
		find_symbol_field/4
	    ]).
:- use_module(resources(top_res(utility_res)),
	    [
		minimum/3,
		maximum/3,
		ith_list_element/3
	    ]).
:- use_module(resources(algebraic_res(normal_form_res)),
	    [
		normal_form/2,
		time_normal_form/2
	    ]).
:- use_module(resources(algebraic_res(normal_form_basic_res)), [userfunc/1]).
:- use_module(resources(algebraic_res(simpl_form_res)),
	    [list_simplification/2]).
:- use_module(resources(algebraic_res(arithm_opers_res)), [min/3, max/3]).
:- use_module(resources(algebraic_res(algebraic__res)),
	    [
		time_add_expr/3,
		time_multiply_expr/3,
		time_add_expr/3,
		time_multiply_expr/3,
		exp_expr/3,
		log_expr/3,
		factorial_expr/2
	    ]).
:- use_module(resources(size_res(size_diff_equ_res)), [solve_size_equs/6]).
:- use_module(resources(solution_res(relation_res)),  [fail_clause/1]).
:- use_module(resources(size_res(ground_size_res)),   [ground_term_size/3]).
:- use_module(resources(size_res(term_size_res)),     [general_term_size/10]).
:- push_prolog_flag(unused_pred_warnings, no).
:- use_module(resources(dependency_res(position_res)),
	    [new_pos/3, gen_clause_pos/2, pos_t/1]).
:- pop_prolog_flag(unused_pred_warnings).
:- use_module(resources(dependency_res(gvars_res)), [find_gvars_field/4]).
:- use_module(resources(size_res(clause_res)),      [clause_term_measure/8]).
:- use_module(resources(dependency_res(adg_res)),   [find_adg_field/4]).
:- use_module(resources(size_res(normalize__res)),
	    [
		init_normalize_queue/3,
		normalize/15,
		substitute_literal_formal/4,
		substitute/4
	    ]).
:- use_module(resources(algebraic_res(sumprod_res)),
	    [sum_expr/5, prod_expr/5]).
:- use_module(resources(size_res(implicit_size_res)), [implicit_var_size/6]).
% ground_term_depth/3 undefined in source
:- use_module(resources(init_res(builtin_res)),
	    [find_entry_trusted_field/6]).
:- use_module(resources(resources_basic)).

% See topdriver.pl for this: (NOW IMPORTED, PBC)
%% approximation(Approx):- ciao:approximation(Approx).

% Added by PLG (22-Mar-97)

:- push_prolog_flag(multi_arity_warnings, off).

% up_low_approximation_expr(lower, S1, S2, Sol):- min_expr(S1, S2, Sol).

up_low_approximation_expr(lower, S1, S2, Sol) :-
	!,
	min_approximation(S1, S2, Sol).
up_low_approximation_expr(upper, S1, S2, Sol) :-
	max_expr(S1, S2, Sol).

min_approximation(S1, S2, Sol) :-
	max_expr(S1, S2, Sol1),
	(
	    (Sol1 == inf ; Sol1 == bot) ->
	    normal_form(0, Sol)
	;
	    (
		Sol1 == S1 ->
		Sol = S2
	    ;
		Sol = S1
	    )
	).

%% min_approximation(S1, S2, Sol):-  
%%    max_expr(S1, S2, Sol1),
%%    (Sol1 == inf -> 
%%           Sol = 0
%%           ;
%%           (Sol1 == S1 -> Sol = S2 ; Sol = S1)
%%    ).   


% Added by PLG (22-Mar-97)

% Warning: check this!
up_low_approximation(lower, S1, S2, Sol) :- !, minimum(S1, S2, Sol).
%% up_low_approximation(lower, S1, S2, Sol):- !,
%%     minimum(S1, S2, Sol1),
%%     ((Sol1 == inf; Sol1 == bot) -> Sol = 0 ; Sol = Sol1).
% up_low_approximation(lower, S1, S2, Sol):- !, minimum_aproximation(S1, S2, Sol).
up_low_approximation(upper, S1, S2, Sol) :- maximum(S1, S2, Sol).

%% minimum_aproximation(S1, S2, Sol):-
%%    maximum(S1, S2, Sol1),
%%    (Sol1 == inf -> 
%%           Sol = 0
%%           ;
%%           (Sol1 == S1 -> Sol = S2 ; Sol = S1)
%%    ).   

% Added by PLG (22-Mar-97)
% Warning, check this! %chk2
% S1 and S2 are numbers.

% up_low_approximation_minmax(S1, S2, Sol):-
% 	approximation(Approx),
% 	up_low_approximation_minmax(Approx, S1, S2, Sol).

up_low_approximation_minmax(lower, S1, S2, Sol) :- !, min(S1, S2, Sol).
up_low_approximation_minmax(upper, S1, S2, Sol) :- max(S1, S2, Sol).

% Added by PLG (22-Mar-97)
%
%  Compute the minimum of a list of normal form expressions.
%
minimum_list([Size],   Size) :- !.
minimum_list([S|Size], Sol) :-
	minimum_list(Size, Sols),
%general_form(Sols, GSol),
%write(GSol), nl,
	min_approximation(Sols, S, Sol).

% End added

:- pred size_analysis/9 :: list * approx * bound * list(bottom_entry) *
	list(symbol_entry) * list * list * list * list + (not_fails, is_det) #
	"Perform the argument size analysis for a strongly connected component.".
size_analysis(Comp, Approx, Bound, BT, ST, Comp, Adg, Gvars, Size) :-
	size_analysis_(Comp, Approx, Bound, BT, ST, Comp, Adg, Gvars, [],
	    Size).

check_member(_, Y) :- var(Y), !, fail.
check_member(X, [Y|_]) :- X == Y, !.
check_member(X, [_|L]) :- check_member(X, L).

:- pred size_analysis_/10 :: list * approx * bound * list(bottom_entry) *
	list(symbol_entry) * list * list * list * list(comp_t) * list
	+ (not_fails, is_det).
size_analysis_([], _, _, _, _, _, _, _, _, []).
size_analysis_([Pred|CompList], Approx, Bound, BT, ST, Comp, [Adg|AList],
	    [Gvars|GList], RSize, [Size|SList]) :-
	find_symbol_field(ST, Pred, clause, ClauseKeys),
	(
	    find_entry_trusted_field(size, BT, ST, Pred, Approx, Sol1),
	    ground(Sol1),
	    find_symbol_field(ST, Pred, mode, Mode),
	    check_member('-', Mode) -> % If there are trusted sizes for all outputs -- EMM
	    Size = [Sol1|_]
	;
	    size_clauses(ClauseKeys, Approx, BT, ST, Comp, Adg, Gvars, RSize,
		Size),
	    debug_message("SIZE equations for ~q/~q = ~q ~n", [F, A, Size]),
	    solve_size_equs(Pred, Bound, ST, Comp, Size, Sol1),
	    debug_message("SIZE closed form for ~q/~q = ~q ~n", [F, A, Sol1])
	),
	size_analysis_(CompList, Approx, Bound, BT, ST, Comp, AList, GList,
	    [comp(Pred, Sol1)|RSize], SList),
	remove_recursive_comps(Sol1, ST, size, Sol),
	insert_symbol_field(ST, Pred, size, Sol).

:- pop_prolog_flag(multi_arity_warnings).

:- pred size_clauses/9 :: list(clause_key_t) * approx * list(bottom_entry) *
	list(symbol_entry) * list * list * list * list(comp_t) * list
	+ (not_fails, is_det) #
"Perform the argument size analysis for the set of clauses in a predicate.".
size_clauses(ClauseKeys, _, _, _, _, _, _, _, _) :-
	var(ClauseKeys),
	!.
size_clauses([ClauseKey|CList], Approx, BT, ST, Comp, [Adg|AList],
	    [Gvars|GList], RSize, [Size|SList]) :-
	clause_key(ClauseKey, ClausePPKey, Key),
	size_function(Approx, ClausePPKey, Key, BT, ST, Comp, Adg, Gvars,
	    RSize, TSize),
	list_simplification(TSize, Size),
	size_clauses(CList, Approx, BT, ST, Comp, AList, GList, RSize,
	    SList).

:- pred size_function/10 :: approx * clause_ppkey_t * atm * list(bottom_entry)
	* list(symbol_entry) * list * list * term * list(comp_t) * list
	+ (not_fails, is_det) #
	"Compute the size functions of a clause as difference equations.".
size_function(Approx, ClausePPKey, Key, BT, ST, Comp, Adg, Gvars, RSize, Size)
:- clause_head(ClausePPKey, Head),
	size_func(Head, Approx, ClausePPKey, Key, BT, ST, Comp, Adg, Gvars,
	    RSize, Size).

:- pred size_func/11 + (not_fails, is_det).
size_func(Head, Approx, ClausePPKey, Key, BT, ST, Comp, Adg, Gvars, RSize,
	    Size) :-
	functor(Head, F, N),
	find_symbol_field(ST, F/N, mode,    Mode),
	find_symbol_field(ST, F/N, measure, Measure),
	input_argument_size(1, Head, Mode, Measure, ClausePPKey, BT, ST, Adg,
	    Gvars, Size),
	(
	    fail_clause(ClausePPKey) ->
	    fail_output_size(Mode, Size)
	;
	    output_argument_size(1, Head, Mode, Measure, Approx,
		ClausePPKey, Key, BT, ST, Comp, Adg, Gvars, Size, RSize, Size)
	).

:- pred input_argument_size/10 + (not_fails, is_det) # "Compute the
	size function for an input position in a clause as a
	difference equation.".
input_argument_size(_, _, [], _, _, _, _, _, _, []).
input_argument_size(N, Head, [(+)|ModeList], [Measure|MeasureList],
	    ClausePPKey, BT, ST, Adg, Gvars, [Size2|Size]) :-
	arg(N, Head, Term),
	new_pos(0, N, Pos),
	( var(Term) ->
	    implicit_input_size(Measure, Term, Pos, Adg, ClausePPKey, Size1) ;
	    explicit_input_size(Measure, Term, Size1) ),
	( Size1 == bot ->
	    Size2 = Pos;
	    Size2 = Size1 ),
%write(Size2), nl,
	N1 is N +1,
	input_argument_size(N1, Head, ModeList, MeasureList, ClausePPKey,
	    BT, ST, Adg, Gvars, Size).
input_argument_size(N, Head, [(-)|ModeList], [_|MeasureList], ClausePPKey,
	    BT, ST, Adg, Gvars, [_|Size]) :-
	N1 is N +1,
	input_argument_size(N1, Head, ModeList, MeasureList, ClausePPKey,
	    BT, ST, Adg, Gvars, Size).

:- pred output_argument_size/15 :: nnegint * term * list * list * approx *
	clause_ppkey_t * atm * list * list * term * term * term * term * term *
	list + (not_fails, is_det) # "Compute the size function for an output
	position in a clause as a difference equation.".
output_argument_size(_, _, [], _, _, _, _, _, _, _, _, _, _, _, []) :-
	!.
output_argument_size(N, Head, [(+)|ModeList], [_Measure|MeasureList], Approx,
	    ClausePPKey, Key, BT, ST, Comp, Adg, Gvars, ISize, RSize,
	    [_|Size]) :- !,
	N1 is N + 1,
	output_argument_size(N1, Head, ModeList, MeasureList, Approx,
	    ClausePPKey, Key, BT, ST, Comp, Adg, Gvars, ISize, RSize, Size).
output_argument_size(N, Head, [(-)|ModeList], [Measure|MeasureList], Approx,
	    ClausePPKey, Key, BT, ST, Comp, Adg, Gvars, ISize, RSize,
	    [Size3|Size]) :-
	arg(N, Head, Term),
	new_pos(0, N, Pos),
	(
	    var(Term) ->
	    implicit_output_size(Measure, Term, Adg, Approx, ClausePPKey, Key,
		BT, ST, Gvars, Size1)
	;
	    Size1 = bot
	),
%write(Size1), nl,
	(
	    Size1 == bot ->
	    (
		explicit_output_size(Measure, Approx, ClausePPKey, Key, BT, ST,
		    Adg, Gvars, Term, Size2),
%write(Size2), nl,
		normalize_size_function(Size2, Pos, Approx, BT, ST, Comp,
		    ClausePPKey, Key, Adg, Gvars, ISize, RSize, Size3)
	    )
	;
	    Size3 = Size1
	),
%write(Size3), nl,
	N1 is N +1,
	output_argument_size(N1, Head, ModeList, MeasureList, Approx,
	    ClausePPKey, Key, BT, ST, Comp, Adg, Gvars, ISize, RSize, Size).

:- pred implicit_input_size/6 + (not_fails, is_det) #
	"Compute the implicit size of a head input.".
implicit_input_size(Measure, Term, Pos, Adg, ClausePPKey, Size) :-
	implicit_var_size(Measure, Term, Pos, Adg, ClausePPKey, Size).

:- pred explicit_input_size/3 + (not_fails, is_det) #
	"Compute the explicit size of a head input.".
explicit_input_size(Measure, Term, Size) :-
	ground_term_size(Measure, Term, Size).

:- pred implicit_output_size/10 :: term * term * list * approx * clause_ppkey_t
	* atm *list * list * term * term + (not_fails, is_det) #
	"Compute the implicit size of a head output.".
implicit_output_size(Measure, Term, Adg, Approx, ClausePPKey, Key, BT, ST,
	    Gvars, Size) :-
	find_gvars_field(Gvars, Term, def, PosList),
	approx_to_bound(Approx, Bound),
	implicit_output_sizes(PosList, Approx, Bound, BT, ST, Measure, Term,
	    Adg, ClausePPKey, Key, Size).

:- pred implicit_output_sizes/11 :: list(pos_t) * approx * bound * list * list
	* term * term * list * clause_ppkey_t * atm * term + (not_fails, is_det
	).
implicit_output_sizes(PosList, _, _, _, _, _, _, _, _, _, bot) :-
	var(PosList),
	!.
implicit_output_sizes([], _, _, _, _, _, _, _, _, _, bot) :- !.
implicit_output_sizes([Pos|PosList], Approx, Bound, BT, ST, Measure, Term, Adg,
	    ClausePPKey, Key, Size) :-
	clause_term_measure(Approx, BT, ST, ClausePPKey, Key, Pos, HTerm, _),
	(
	    HTerm == Term ->
	    implicit_var_size(Measure, Term, Pos, Adg, ClausePPKey, Size1)
	;
	    Size1 = bot
	),
	implicit_output_sizes(PosList, Approx, Bound, BT, ST, Measure, Term,
	    Adg, ClausePPKey, Key, Size2),
% Added by PLG (22-Mar-97)
	up_low_approximation(Bound, Size1, Size2, Size).
% End added
% Commented by PLG (22-Mar-97)
% maximum(Size1, Size2, Size).

:- pred explicit_output_size/10 + (not_fails, is_det) #
	"Compute the explicit size of a head output.".
explicit_output_size(Measure, Approx, ClausePPKey, Key, BT, ST, Adg, Gvars,
	    Term, Size) :-
	gen_clause_pos(Adg, PosSet),
	general_term_size(Measure, Approx, ClausePPKey, Key, BT, ST, Gvars,
	    PosSet, Term, Size).

%
%  Normalize the size function corresponding to an output position.
%
normalize_size_function(Size, Pos, Approx, BT, ST, Comp, ClausePPKey, Key, Adg,
	    Gvars, ISize, RSize, NSize) :-
	gen_clause_pos(Adg, PosSet),
	find_adg_field(Adg, Pos, pred, PredPos),
	init_normalize_queue(PredPos, QHead, QTail),
	normalize(Size, QHead, QTail, Approx, BT, ST, Comp, ClausePPKey, Key,
	    Adg, Gvars, PosSet, ISize, RSize, NSize).
%

:- pred fail_output_size/2 :: list * list + (not_fails, is_det).

fail_output_size([],       []).
fail_output_size([+|Mode], [_|Size]) :-
	!,
	fail_output_size(Mode, Size).
fail_output_size([-|Mode], [bot|Size]) :-
	fail_output_size(Mode, Size).

%
%

% Added and Modified by EMM

:- pred remove_recursive_comps(S, ST, Type, R) :: list(ST)
	: symbol_field(Type).

:- comp remove_recursive_comps/4 + not_fails.
% rtcheck --EMM
remove_recursive_comps(S, ST, Type, R) :-
	remove_recursive_comps_(S, ST, Type, R),
	!.
% remove_recursive_comps(S, _ST, _Type, S) :-
% 	show_message(warning, "Can not remove recursive comps").

remove_recursive_comps_([],        _,  _,    []).
remove_recursive_comps_([S1|Sol1], ST, Type, [S|Sol]) :-
	remove_recursive_general_comp(S1, ST, Type, S),
	remove_recursive_comps_(Sol1, ST, Type, Sol).

remove_recursive_general_comp(S1, ST, Type, S) :-
	time_normal_form(S1, N1),
	remove_time_recursive_comp(N1, ST, Type, N),
	time_general_form(N, S).
% End Added and Modified by EMM

remove_time_recursive_comp(Expr, ST, resources(Resources), Sol) :-
	!,
	remove_resource_recursive_comp(Expr, Resources, ST, Sol).
remove_time_recursive_comp(Expr, ST, Type, Sol) :-
	remove_recursive_comp(Expr, ST, Type, Sol).

remove_resource_recursive_comp([],           [],     _,  []).
remove_resource_recursive_comp([Expr|Exprs], [R|Rs], ST, [Sol|Sols]) :-
	remove_recursive_comp(Expr, ST, resource(R), Sol),
	remove_resource_recursive_comp(Exprs, Rs, ST, Sols).

remove_recursive_comp(top,        _,  _,    top).
remove_recursive_comp(bot,        _,  _,    bot).
remove_recursive_comp(inf,        _,  _,    inf).
remove_recursive_comp(expr(T, F), ST, Type, Sol) :-
	remove_recursive_comp1(T, ST, Type, Sols),
	Sol1 = expr([], F),
	time_add_expr(Sols, Sol1, Sol).

%
remove_recursive_comp1([], _ST, _Type, Zero) :-
	normal_form(0, Zero).
remove_recursive_comp1([term(T, F)|Ts], ST, Type, Sol) :-
	remove_recursive_comp2(T, ST, Type, Sol1),
	C = expr([], F),
	time_multiply_expr(Sol1, C, Sol2),
	remove_recursive_comp1(Ts, ST, Type, Sols),
	time_add_expr(Sol2, Sols, Sol).

%
remove_recursive_comp2([], _, _, One) :-
	normal_form(1, One).
remove_recursive_comp2([P|Ps], ST, Type, Sol) :-
	remove_recursive_comp3(P, ST, Type, Sol1),
	remove_recursive_comp2(Ps, ST, Type, Sols),
	time_multiply_expr(Sol1, Sols, Sol).

%
remove_recursive_comp3(P, ST, Type, Sol) :-
	userfunc(P),
	functor(P, F, _),
	arg(1, P, Arity),
	general_form(Arity, A),
	find_symbol_field(ST, F/A, Type, Size),
	arg(2, P, OPos),
	general_form(OPos, O),
	ith_list_element(O, Size, Osize),
	substitute_literal_formal(A, Osize, 1, NOsize),
	find_symbol_field(ST, F/A, (mode), Mode),
	substitute_actual(Mode, 1, 3, P, ST, NOsize, Type, Sol1),
	time_normal_form(Sol1, Sol).
remove_recursive_comp3(exp(E1, E2), ST, Type, Sol) :-
	remove_recursive_comp(E1, ST, Type, Sol1),
	remove_recursive_comp(E2, ST, Type, Sol2),
	exp_expr(Sol1, Sol2, Sol).
remove_recursive_comp3(log(E1, E2), ST, Type, Sol) :-
	remove_recursive_comp(E1, ST, Type, Sol1),
	remove_recursive_comp(E2, ST, Type, Sol2),
	log_expr(Sol1, Sol2, Sol).
remove_recursive_comp3(fact(E1), ST, Type, Sol) :-
	remove_recursive_comp(E1, ST, Type, Sol1),
	factorial_expr(Sol1, Sol).
remove_recursive_comp3(sum(E1, E2, E3, E4), ST, Type, Sol) :-
	remove_recursive_comp(E4, ST, Type, Expr),
	general_form(E1, Var),
	sum_expr(Var, E2, E3, Expr, Sol).
remove_recursive_comp3(prod(E1, E2, E3, E4), ST, Type, Sol) :-
	remove_recursive_comp(E4, ST, Type, Expr),
	general_form(E1, Var),
	prod_expr(Var, E2, E3, Expr, Sol).
remove_recursive_comp3(arg(E1, E2), ST, Type, Sol) :-
	remove_recursive_comp(E1, ST, Type, Sol1),
	remove_recursive_comp(E2, ST, Type, Sol2),
	Sol = expr([], [factor([arg(Sol1, Sol2)], 1)]).
remove_recursive_comp3(arity(E1), ST, Type, Sol) :-
	remove_recursive_comp(E1, ST, Type, Sol1),
	Sol = expr([], [factor([arity(Sol1)], 1)]).


%
%
substitute_actual([],       _, _, _, _,  Sol,   _,    Sol).
substitute_actual([+|Mode], M, N, P, ST, Osize, Type, Sol) :-
%!,
	arg(N, P, Arg),
	remove_recursive_comp(Arg, ST, Type, RArg),
	general_form(RArg, Rarg),
	new_pos(1, M, Pos),
	substitute(Osize, Pos, Rarg, NOsize),
	M1 is M+1,
	N1 is N+1,
	substitute_actual(Mode, M1, N1, P, ST, NOsize, Type, Sol).
substitute_actual([-|Mode], M, N, P, ST, Osize, Type, Sol) :-
	M1 is M+1,
% 	N1 is N+1,
% 	M1 = M,
	N1 = N,
	substitute_actual(Mode, M1, N1, P, ST, Osize, Type, Sol).
