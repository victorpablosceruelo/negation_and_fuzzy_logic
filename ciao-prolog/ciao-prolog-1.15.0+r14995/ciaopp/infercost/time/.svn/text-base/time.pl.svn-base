:- module(time, [time_analysis/9], [assertions, infercost(infercost_decl)]).

:- doc(author, "Nai-Wei Lin").
:- doc(author, "Pedro L@'{o}pez").
:- doc(module, "This file contains the procedures for performing
   the time analysis for the predicates in the program in
   topologically sorted order.  Perform the time analysis for a
   strongly connected component. Original version from December,
   1991. Modified by PLG and EMM.").

:- use_module(library(messages), [debug_message/2]).
:- use_module(infer(infer),      [get_info/5]).
:- use_module(infer(infer_dom),  [flag_set/3, flag_is/3]).

:- use_module(infercost(algebraic(simpl_form)), [simplification/2]).
:- use_module(infercost(dependency(position)), [gen_clause_pos/2,
		gen_literal_iopos/5]).
:- use_module(infercost(init(symtable)),
	    [
		find_symbol_field/4,
		insert_symbol_field/4
	    ]).
:- use_module(infercost(init(builtin)),
	    [
		second_order_predicate/1,
		second_order_predicate_pred_arg/2,
		second_order_predicate_pred_num/3
	    ]).
:- use_module(infercost(init(initsystem_basic)), [clause_type/2]).
:- use_module(infercost(size(normalize_)),
	    [
		init_normalize_queue/3,
		literal_output_comp/11,
		normalize/13
	    ]).
:- use_module(infercost(size(clause)), [ith_clause_literal/3]).
:- use_module(infercost(size(size_)),  [remove_recursive_comps/4]).
:- use_module(infercost(solution(comp_diff_equ)),
	    [solve_complexity_equ/6]).
:- use_module(infercost(solution(binding)),
	    [normalize_solution_function/12]).
:- use_module(infercost(solution(relation)), [recursive_clause/2]).
:- use_module(infercost(top(utility)),
	    [
		addition/3,
		multiply/3,
		nonsequence/1,
		member/2
	    ]).
:- use_module(infercost(cost_approx)).

% Added by PLG (Sep 97)
%% was:
%% time_analysis(Comp, BT, ST, Comp, Size, Adg, Gvars, Ldg, Time) :-
%% 	time_analysis(Comp, BT, ST, Comp, Size, Adg, Gvars, Ldg, [], Time).

time_analysis(Approx, BT, ST, Comp, Size, Adg, Gvars, Ldg, Time) :-
	time_analysis_(Comp, Approx, BT, ST, Comp,
	    Size, Adg, Gvars, Ldg, [], Time).

:- comp time_analysis_/11 + (not_fails, is_det).
% rtcheck -- EMM
time_analysis_([], _Approx, _BT, _ST, _Comp, _SList,
	    _AList, _GList, _LList, _RTime, []).
time_analysis_([Pred|CompList], Approx, BT, ST, Comp,
	    [Size|SList], [Adg|AList], [Gvars|GList], [Ldg|LList],
	    RTime, [Time|TList]) :-
 	find_symbol_field(ST, Pred, time, Time1),
	(
	    var(Time1) ->
	    (
		find_symbol_field(ST, Pred, clause, Clauses),
		bound_time_predicate(Approx, Pred, Clauses, BT, ST, Comp, Size,
		    Adg, Gvars, Ldg, RTime, Time2),
%write(Time2),nl,
		debug_message("COST (recurrence equations):~q ~n", [Time2]),
		solve_complexity_equ(Pred, ST, Comp, Time2, Size, TTime),
		debug_message("COST (closed form):~q ~n", [TTime]),
		Time3 = [TTime]
	    )
	;
	    debug_message("Cost (closed form, equation was computed) ~n ~q ~n",
		[Time1]),
	    Time3 = Time1
	),
%write(Time3),nl,
	time_analysis_(CompList, Approx, BT, ST, Comp, SList, AList,
	    GList, LList, [comp(Pred, Time3)|RTime], TList),
	(remove_recursive_comps(Time3, ST, time, Time4) 
           -> Time = Time4
            ; set_top_time(Time)
        ),
	insert_symbol_field(ST, Pred, time, Time).

set_top_time(X):- 
	approximation(Approx),
	set_top_time_(Approx, X).

set_top_time_(upper, inf).
set_top_time_(lower, 0).



bound_time_predicate(upper, _Pred, Clauses, BT, ST, Comp, Size,
	    Adg, Gvars, Ldg, RTime, Time2) :-
	time_clauses(Clauses, upper, BT, ST, Comp, Size, Adg, Gvars, Ldg,
	    RTime, Time2).
bound_time_predicate(lower, Pred, Clauses, BT, ST, Comp, Size, Adg, Gvars, Ldg,
	    RTime, Time2) :-
	lower_bound_time_predicate(Pred, Clauses, BT, ST, Comp, Size, Adg,
	    Gvars, Ldg, RTime, Time2).

:- pred non_fail_cover_pred_info(PredName, FailInfo, CoverInfo) ::
	(PredName = F/A) # "Check
   whether a predicate is non-failing (definitely will not fail)".

non_fail_cover_pred_info(F/A, FailInfo, CoverInfo) :-
	functor(Head, F, A),
	get_info(nonfail, pred, F/A, Head, [FailInfo, CoverInfo]),
	!.
non_fail_cover_pred_info(_, FailFlag, CoverFlag) :-
	flag_set(nf, possibly_fails, FailFlag),
	flag_set(nf, not_covered,    CoverFlag).

%% successful_pred(Head, FailInfo) :-
%% 	functor(Head, F, A),
%% 	functor(Head0, F, A),
%% 	trust_nonfail(Head0, _InTypes, _OuTypes, FailInfo, _CoverInfo),!.
%% 	% is_renaming(Head, Head0), !.
%% successful_pred(_Head, possible_fail).

%% is_a_non_failing_pred(F/A) :-
%%    functor(Head, F, A),
%%    successful_pred(Head, FailInfo),
%%    FailInfo == not_fail.

%% Perform lower bound time analysis -PLG 
%% 
%% CASES:
%% ------
%% 
%% 1) If a PREDICATE is NON-FAILING: then at least one of its clauses
%%  will not fail, and thus, a lower bound on its cost is the minimum of
%%  of the costs of (all of) its clauses (including the ones which have
%%  some literal which has not been proven to be non-failing). And the
%%  cost of a clause is the sum of the costs of all of its literals.
%% 
%% 2) If a PREDICATE is FAILING (non-failure is not ensured), then we have
%% two cases:
%% 
%%   A) THE PREDICATE IS COVERED: then there is at least one clause whose
%%      test will not fail. Then, a lower bound on its cost is the
%%      minimum of the costs of (all of) its clauses. In this case,
%%      the cost of a clause is the sum of the cost of the sequence of
%%      non-failing literals just before the first literal which is not
%%      ensured not to fail.
%% 
%%   B) THE PREDICATE IS NOT COVERED: Then, a lower bound on its cost is
%%      zero (since all head unifications can fail in the worst case).
%% 
%% TREATEMENT OF THE CUT
%% ---------------------
%% 
%% 1) If a PREDICATE is NON-FAILING: ignore. 
%% 
%% 2) If a PREDICATE is FAILING:
%% 
%%   A) THE PREDICATE IS COVERED:  ignore. Note that the implicit test of a
%%    cut is taken into account in the following clauses.
%% 
%%   B) THE PREDICATE IS NOT COVERED: ignore.
%% 
%% INPROVEMENT:
%% ------------
%% 
%%    NOTE however that it can be possible to get a more accurate
%%  lower-bound by discarding some of the clauses. For example, by taking
%%  the CUT into account.
%% 
%% Cases:
%% ------
%% 
%%   i) Detecting a NON-FAILING CUT:
%%   Suppose C1, C2, C3, ..., Cn are clauses such that there is a
%% non-failing cut on Cn, then the clauses Cn+1, Cn+2, ..., Cm can be
%% ignored (in fact, they are dead code).
%% 
%%   A cut is non-failing if all preceding clauses and a restriction of
%% Cn to the literals before the cut are a non-failing set, that is, at
%% least one of the clauses will succeed.
%% 
%%  NOTE that NON-FAILING, covered + FAILING, not covered predicates can
%% have non-failing cuts.
%% 
%% ii) SEARCHING FOR ONLY ONE SOLUTION:
%%   If we are searching for only one solution, and C1, C2, C3,...,Cn 
%%   constitute a non-failing set (that is, at least one of the clauses
%%   will succeed) then the clauses Cn+1, Cn+2, ..., Cm can be ignored
%%   (in fact, they are dead code).
%% 
%% If we are searching for only one solution and find both, a non-failing
%% set of clauses and a failing cut, then use the option that discard the
%% maximun number of clauses.

%% Literals which are builtins that act either as tests or assignments
%% are ignored in the cost analysis since: tests are used to check
%% covering, and assignments always succeed.

lower_bound_time_predicate(Pred, Clauses, BT, ST, Comp, Size, Adg, Gvars, Ldg,
	    RTime, Time2) :-
	debug_message("CALL: ~q", [non_fail_cover_pred_info(Pred, FailInfo,
		    CoverInfo)]),
	non_fail_cover_pred_info(Pred, FailInfo, CoverInfo),
	debug_message("EXIT: ~q", [non_fail_cover_pred_info(Pred, FailInfo,
		    CoverInfo)]),
	(
	    flag_is(nf, not_fails, FailInfo) ->
	    time_clauses(Clauses, nf_lower, BT, ST, Comp, Size, Adg, Gvars,
		Ldg, RTime, Time2)
	;
	    (
		(
		    flag_is(nf, possibly_fails, FailInfo),
		    flag_is(nf, covered,        CoverInfo)
		) ->
		time_clauses(Clauses, lower, BT, ST, Comp, Size, Adg, Gvars,
		    Ldg, RTime, Time2)
	    ;
		set_zero_cost(Clauses, Time2)
	    )
	).

:- pred set_zero_cost(Clauses, ZeroCost) # " Create a list of length
   equal to the number of clauses of the predicate with
   zeros. I.e. set the cost of each clause to zero.".

% Modified by EMM
set_zero_cost(Clauses, ZeroCost) :-
	zero_time(Zero),
	set_zero_cost_(Clauses, Zero, ZeroCost).

set_zero_cost_(Clauses, _Zero, []) :- var(Clauses), !.
set_zero_cost_(Clauses, Zero,  [Zero|Time2]) :-
	nonvar(Clauses),
	Clauses = [_|Clauses1],
	set_zero_cost(Clauses1, Time2).

zero_time(0).
% End Modified by EMM

/*
%
%  Perform the time analysis for a predicate.
%
time_predicate(Pred, BT, ST, Comp, Size, Adg, Gvars, Ldg, Time) :-
	find_symbol_field(ST, Pred, time, Time1),
	(
	    var(Time1) ->
	    (
		find_symbol_field(ST, Pred, clause, Clauses),
		time_clauses(Clauses, upper, BT, ST, Comp, Size, Adg, Gvars,
		    Ldg, Time2),
		find_symbol_field(ST, Pred, mutex, Mutex),
		solve_comp_equs(Pred, ST, Comp, Time2, Size, Mutex, Time),
		insert_symbol_field(ST, Pred, time, [Time])
	    )
	;
	    Time = Time1
	).
*/

:- pred time_clauses(Clauses, Type, BT, ST, Comp, SList, AList, GList, LList,
	    RTime, Time)
# " Perform the time analysis for the set of clauses in a predicate.".

time_clauses(Clauses, _, _, _, _, _, _, _, _, _, []) :-
	var(Clauses),
	!.
time_clauses([Clause:_Key|CList], Type, BT, ST, Comp, [Size|SList],
	    [Adg|AList], [Gvars|GList], [Ldg|LList], RTime, [Time|TList]) :-
	time_clause(Type, Clause, BT, ST, Comp, Size, Adg, Gvars, Ldg, RTime,
	    Time1),
	simplification(Time1, Time),
	time_clauses(CList, Type, BT, ST, Comp, SList,
	    AList, GList, LList, RTime, TList).

:- pred time_clause(Approx, Clause, BT, ST, Comp, Size, Adg, Gvars, Ldg, RTime,
	    Time) # "Perform the time analysis for a clause.".

time_clause(Approx, Clause, BT, ST, Comp, Size, Adg, Gvars, Ldg, RTime,
	    Time) :-
	clause_type(Clause, Type),
	time_clause_(Type, Approx, Clause, BT, ST, Comp, Size, Adg, Gvars, Ldg,
	    RTime, Time).

time_clause_(2, Approx, Clause, BT, ST, Comp, Size, Adg, Gvars, Ldg,
	    RTime, Time) :-
	Clause = (Head :- Body),
	head_cost(Head, HeadCost),
	time_body(Approx, Body, 1, Clause, BT, ST, Comp, Size, Adg, Gvars, Ldg,
	    1, RTime, Time1),
	addition(Time1, HeadCost, Time).
time_clause_(3, _, Clause, _, _, _, _, _, _, _, _, Time) :-
	head_cost(Clause, Time).

% Added by EMM
head_cost(_Head, 1).
body_cost(_Body, 0).
% End Added by EMM

time_body(upper, Body, LitNum, Clause, BT, ST, Comp, Size, Adg, Gvars, Ldg,
	    Times, RTime, Time) :-
	upper_time_body(Body, LitNum, Clause, BT, ST, Comp, Size, Adg, Gvars,
	    Ldg, Times, RTime, Time).
time_body(lower, Body, LitNum, Clause, BT, ST, Comp, Size, Adg, Gvars, Ldg,
	    Times, RTime, Time) :-
	lower_time_body(Body, LitNum, Clause, BT, ST, Comp, Size, Adg, Gvars,
	    Ldg, Times, RTime, Time).
time_body(nf_lower, Body, LitNum, Clause, BT, ST, Comp, Size, Adg, Gvars, Ldg,
	    Times, RTime, Time) :-
	nf_lower_time_body(Body, LitNum, Clause, BT, ST, Comp, Size, Adg,
	    Gvars, Ldg, Times, RTime, Time).

%
%  Perform the time analysis for the body of a clause.
%
upper_time_body((Lit, Body), LitNum, Clause, BT, ST, Comp,
	    Size, Adg, Gvars, Ldg, Times, RTime, Time) :-
	!, % Added by PLG cut (1-Sep-97)
	time_literal(Lit, LitNum, Clause, BT, ST, Comp, Size,
	    Adg, Gvars, Ldg, RTime, Time1),
	(
	    Lit == (!) ->
	    Time2 = Time1
	;
	    multiply(Time1, Times, Time2)
	),
	(
	    Lit == (!) ->
	    Times1 = 1
	;
	    (
		frequency_literal(Lit, LitNum, Clause, BT, ST, Comp, Size,
		    Adg, Gvars, _, _, Sol2),
		multiply(Times, Sol2, Times1)
	    )
	),
	LitNum1 is LitNum + 1,
	upper_time_body(Body, LitNum1, Clause, BT, ST, Comp,
	    Size, Adg, Gvars, Ldg, Times1, RTime, Time3),
	addition(Time2, Time3, Time).
upper_time_body(Lit, LitNum, Clause, BT, ST,
	    Comp, Size, Adg, Gvars, Ldg, Times, RTime, Time) :-
	nonsequence(Lit),
	time_literal(Lit, LitNum, Clause, BT, ST, Comp, Size, Adg, Gvars, Ldg,
	    RTime, Time1),
	multiply(Time1, Times, Time).

% Added by PLG (23-Mar-97)

%% approximation_time_body(Body,LitNum,Clause,BT,ST,Comp,Size,Adg,Gvars,
%%                                 Ldg,Times,RTime,Time) :-
%%    approximation(Approx),
%%    (Approx == upper -> 
%%        time_body(Body,LitNum,Clause,BT,ST,Comp,Size,Adg,Gvars,Ldg,
%%                  Times,RTime,Time)
%%        ;
%%        (Approx == lower,
%%        lower_time_body(Body,LitNum,Clause,BT,ST,Comp,Size,Adg,Gvars,Ldg,
%%                        Times,RTime,Time))
%%    ). 
%% 

%
%  Perform the lower bound time analysis for the body of a clause.
%

%% Literals which are builtins that act either as tests or assignments
%% are ignored in the cost analysis since: tests are used to check
%% covering, and assignments always succeed.

not_ending_literal(X) :-
	test_or_assignment(X),
	!.
not_ending_literal(X) :-
	functor(X, F, A),
	debug_message("CALL: ~q", [non_fail_cover_pred_info(F/A, FailInfo,
		    _CoverInfo)]),
	non_fail_cover_pred_info(F/A, FailInfo, _CoverInfo),
	debug_message("EXIT: ~q", [non_fail_cover_pred_info(F/A, FailInfo,
		    _CoverInfo)]),
	flag_is(nf, not_fails, FailInfo).

test_or_assignment(X) :-
	var(X),
	!,
	fail.
test_or_assignment(\+(X)) :-
	!,
	test_or_assignment(X).
test_or_assignment(X) :-
	functor(X, F, A),
	utility:member(
	    [(!) /1,
		(=) /2,
		(==) /2,
		(\==) /2,
		(is) /2,
		(=:=) /2,
		(=\=) /2,
		(<) /2,
		(>) /2,
		(=<) /2,
		(>=) /2,
		(number) /1,
		(integer) /1,
		(atom) /1,
		(atomic) /1
	    ], F/A).

lower_time_body((Lit, Body), LitNum, Clause, BT, ST, Comp, Size, Adg,
	    Gvars, Ldg, Times, RTime, Time) :-
	!,
	(
	    not_ending_literal(Lit) ->
	    time_literal(Lit, LitNum, Clause, BT, ST, Comp, Size, Adg,
		Gvars, Ldg, RTime, Time1),
	    LitNum1 is LitNum + 1,
	    lower_time_body(Body, LitNum1, Clause, BT, ST, Comp, Size,
		Adg, Gvars, Ldg, Times, RTime, Time3),
	    addition(Time1, Time3, Time)
	;
	    zero_time(Time)
	).
lower_time_body(Lit, LitNum, Clause, BT, ST, Comp, Size, Adg, Gvars, Ldg,
	    Times, RTime, Time) :-
	nonsequence(Lit),
	(
	    not_ending_literal(Lit) ->
	    time_literal(Lit, LitNum, Clause, BT, ST, Comp, Size, Adg, Gvars,
		Ldg, RTime, Time1),
	    multiply(Time1, Times, Time)
	;
	    zero_time(Time)
	).

nf_lower_time_body((Lit, Body), LitNum, Clause, BT, ST, Comp, Size, Adg, Gvars,
	    Ldg, Times, RTime, Time) :-
	!,
	time_literal(Lit, LitNum, Clause, BT, ST, Comp, Size, Adg, Gvars, Ldg,
	    RTime, Time1),
	LitNum1 is LitNum + 1,
	nf_lower_time_body(Body, LitNum1, Clause, BT, ST, Comp, Size, Adg,
	    Gvars, Ldg, Times, RTime, Time3),
	addition(Time1, Time3, Time).
nf_lower_time_body(Lit, LitNum, Clause, BT, ST, Comp, Size, Adg, Gvars, Ldg,
	    Times, RTime, Time) :-
	nonsequence(Lit),
	time_literal(Lit, LitNum, Clause, BT, ST, Comp, Size, Adg, Gvars,
	    Ldg, RTime, Time1),
	multiply(Time1, Times, Time).

% End added

:- pred time_literal(Lit, LitNum, Clause, BT, ST, Comp, Size, Adg, Gvars, _Ldg,
	    RTime, Time) # " Perform the time analysis for a literal.".

time_literal(Lit, LitNum, Clause, BT, ST, Comp, Size, Adg, Gvars, _Ldg, RTime,
	    Time) :-
	functor(Lit, F, A),
	(
	    second_order_predicate(F/A) ->
	    (
		second_order_predicate_pred_arg(Lit, Lit1),
		functor(Lit1, F1, A1),
		arg(2, Clause, Body),
		second_order_predicate_pred_num(Body, LitNum, Num1)
	    )
	;
	    (
		F1 = F,
		A1 = A,
		Num1 = LitNum,
		Lit1 = Lit
	    )
	),
	literal_output_comp(F1/A1, Lit1, Num1, 1, BT, ST, Comp, Adg, time,
	    RTime, LitTime),
	normalize_time_function(LitTime, F1/A1, Num1, BT, ST, Comp, Clause,
	    Adg, Gvars, Size, RTime, Time0),
	body_cost(Lit1, Costs),
	addition(Time0, Costs, Time).

frequency_literal(Lit, LitNum, Clause, BT, ST, Comp, Size, Adg, Gvars, _, _,
	    Sol) :-
	functor(Lit, F, A),
	literal_output_comp(F/A, Lit, LitNum, 1, BT, ST, [], Adg, det, [],
	    Sol1),
	normalize_solution_function(Sol1, F/A, LitNum, BT, ST, Comp, Clause,
	    Adg, Gvars, Size, [], Sol).
/*
	(
	    second_order_predicate(F/A) ->
	    (
		second_order_predicate_pred_arg(Lit, Lit1),
		functor(Lit1, F1, A1),
		arg(2, Clause, Body),
		second_order_predicate_pred_num(Body, LitNum, Num1)
	    )
	;
	    (
		F1 = F,
		A1 = A,
		Num1 = LitNum
	    )
	),
	literal_output_comp(F1/A1, Lit, Num1, 1, BT, ST, [], Adg, det, [],
	    Sol1),
	normalize_solution_function(Sol1, F1/A1, Num1, BT, ST, Comp, Clause,
	    Adg, Gvars, Size, [], Sol2),
	(
	    second_order_predicate(F/A) ->
	    (
		gen_literal_iopos(Adg, F1/A1, Num1, (-), Pos),
		pos_var(Pos, Lit1, Vars),
		arg(1, Lit, Arg1),
		term_var(Arg1, Var1),
		(
		    opened_set_equivalent(Var1, Vars) ->
		    Sol = 1
		;
		    Sol = Sol2
		)
	    )
	;
	    Sol = Sol2
	).
*/


:- pred normalize_time_function(LitTime, LitName, LitNum, BT, ST,
	    Comp, Clause, Adg, Gvars, Size, RTime, Time) # "Normalize the time
   function of a literal.".

normalize_time_function(LitTime, LitName, LitNum, BT, ST, Comp, Clause, Adg,
	    Gvars, Size, RTime, Time) :-
	gen_clause_pos(Adg, PosSet),
	(
	    recursive_clause(Clause, Comp) ->
	    (
		ith_clause_literal(0, Clause, Lit),
		functor(Lit, F, N),
		find_symbol_field(ST, F/N, size, ISz)
	    )
	;
	    ISz = Size
	),
	gen_literal_iopos(Adg, LitName, LitNum, (+), Pos),
	init_normalize_queue(Pos, QHd, QTl),
	normalize(LitTime, QHd, QTl, BT, ST, [], Clause, Adg, Gvars,
	    PosSet, ISz, RTime, Time).
