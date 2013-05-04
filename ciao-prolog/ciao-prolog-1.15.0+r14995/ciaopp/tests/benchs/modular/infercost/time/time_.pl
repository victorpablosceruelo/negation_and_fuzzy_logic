%
%  time.pl			Nai-Wei Lin			December, 1991
%
%  This file contains the procedures for performing the time
%  analysis for the predicates in the program in topologically sorted order.
%

%
%  Perform the time analysis for a strongly connected component.
%


% Added by PLG (Sep 97)
 %% was:
 %% time_analysis(Comp,BT,ST,Comp,Size,Adg,Gvars,Ldg,Time) :-
 %% 	time_analysis(Comp,BT,ST,Comp,Size,Adg,Gvars,Ldg,[],Time).


time_analysis(Comp,BT,ST,Comp,Size,Adg,Gvars,Ldg,Time) :-
    approximation(Approx),
    (Approx == upper ->
	time_analysis(Comp,BT,ST,Comp,Size,Adg,Gvars,Ldg,[],Time)
        ; 
        lower_time_analysis(Comp,BT,ST,Comp,Size,Adg,Gvars,Ldg,[],Time)).

% Perform upper bound time analysis -PLG 

time_analysis([],_,_,_,_,_,_,_,_,[]).
time_analysis([Pred|CompList],BT,ST,Comp,[Size|SList],[Adg|AList],
	      [Gvars|GList],[Ldg|LList],RTime,[Time|TList]) :-
	find_symbol_field(ST,Pred,time,Time1),
	(var(Time1) ->
		(find_symbol_field(ST,Pred,clause,Clauses),
		 time_clauses(Clauses,BT,ST,Comp,Size,Adg,Gvars,Ldg,RTime,
			Time2),
		 %write(Time2),nl,
		 solve_complexity_equ(Pred,ST,Comp,Time2,Size,TTime),
		 Time3 = [TTime]);
		Time3 = Time1),
	%write(Time3),nl,
	time_analysis(CompList,BT,ST,Comp,SList,AList,GList,LList,
		[comp(Pred,Time3)|RTime],TList),
	remove_recursive_comps(Time3,ST,time,Time),
	insert_symbol_field(ST,Pred,time,Time).

% Check whether a predicate is non-failing (definitely will not fail)

non_fail_cover_pred_info(F/A, FailInfo, CoverInfo):-
 functor(Head0,F,A),
% debug_message("CALL: ~q", [db_get(trust_nonfail(Head0, _InTypes, _OuTypes, FailInfo, CoverInfo))]),
 db_get(trust_nonfail(Head0, _InTypes, _OuTypes, FailInfo, CoverInfo)), !,
% debug_message("EXIT: ~q", [db_get(trust_nonfail(Head0, _InTypes, _OuTypes, FailInfo, CoverInfo))]).
 true.
non_fail_cover_pred_info(_, FailFlag, CoverFlag):-
     flag_set_possibly_fails(FailFlag), 
     flag_set_not_covered(CoverFlag).

 %% successful_pred(Head, FailInfo):-
 %% 	functor(Head,F,A),
 %% 	functor(Head0,F,A),
 %% 	trust_nonfail(Head0, _InTypes, _OuTypes, FailInfo, _CoverInfo),!.
 %% 	% is_renaming(Head,Head0), !.
 %% successful_pred(_Head, possible_fail).

 %% is_a_non_failing_pred(F/A):-
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
 
lower_time_analysis([],_,_,_,_,_,_,_,_,[]).
lower_time_analysis([Pred|CompList],BT,ST,Comp,[Size|SList],[Adg|AList],
	      [Gvars|GList],[Ldg|LList],RTime,[Time|TList]) :-
	find_symbol_field(ST,Pred,time,Time1),
	(var(Time1) ->
		(find_symbol_field(ST,Pred,clause,Clauses),
                 lower_bound_time_predicate(Pred,Clauses,BT,ST,Comp,Size,
                                            Adg,Gvars,Ldg,RTime,Time2),
		 %write(Time2),nl,
		 solve_complexity_equ(Pred,ST,Comp,Time2,Size,TTime),
		 Time3 = [TTime]);
		Time3 = Time1),
	%write(Time3),nl,
	lower_time_analysis(CompList,BT,ST,Comp,SList,AList,GList,LList,
		[comp(Pred,Time3)|RTime],TList),
	remove_recursive_comps(Time3,ST,time,Time),
	insert_symbol_field(ST,Pred,time,Time).


lower_bound_time_predicate(Pred,Clauses,BT,ST,Comp,Size,Adg,Gvars,Ldg,RTime,Time2):-
%   debug_message("CALL: ~q", [non_fail_cover_pred_info(Pred, FailInfo, CoverInfo)]), 
   non_fail_cover_pred_info(Pred, FailInfo, CoverInfo),
%   debug_message("EXIT: ~q", [non_fail_cover_pred_info(Pred, FailInfo, CoverInfo)]), 
   (flag_is_not_fails(FailInfo) -> 
        nf_lower_time_clauses(Clauses,BT,ST,Comp,Size,Adg,Gvars,Ldg,RTime,
			           Time2) 
        ;
        ((flag_is_possibly_fails(FailInfo), flag_is_covered(CoverInfo)) -> 
             lower_time_clauses(Clauses,BT,ST,Comp,Size,Adg,Gvars,Ldg,RTime,Time2)
             ; set_zero_cost(Clauses, Time2))).

% Create a list of length equal to the number of clauses of the
% predicate with zeros. I.e. set the cost of each clause to zero.
 
set_zero_cost(Clauses, []):- var(Clauses), !.
set_zero_cost(Clauses, [0|Time2]):-
       nonvar(Clauses),
       Clauses = [_|Clauses1],
       set_zero_cost(Clauses1, Time2).
/*
%
%  Perform the time analysis for a predicate.
%
time_predicate(Pred,BT,ST,Comp,Size,Adg,Gvars,Ldg,Time) :-
	find_symbol_field(ST,Pred,time,Time1),
	(var(Time1) ->
		(find_symbol_field(ST,Pred,clause,Clauses),
		 time_clauses(Clauses,BT,ST,Comp,Size,Adg,Gvars,Ldg,Time2),
		 find_symbol_field(ST,Pred,mutex,Mutex),
		 solve_comp_equs(Pred,ST,Comp,Time2,Size,Mutex,Time),
		 insert_symbol_field(ST,Pred,time,[Time]));
		Time = Time1).
*/

%
%  Perform the time analysis for the set of clauses in a predicate.
%
% Upper bounds:

time_clauses(Clauses,_,_,_,_,_,_,_,_,[]) :-
	var(Clauses).
time_clauses(Clauses,BT,ST,Comp,[Size|SList],[Adg|AList],[Gvars|GList],
	     [Ldg|LList],RTime,[Time|TList]) :-
	nonvar(Clauses),
	Clauses = [Clause|CList],
	time_clause(Clause,BT,ST,Comp,Size,Adg,Gvars,Ldg,RTime,Time1),
	simplification(Time1,Time),
	time_clauses(CList,BT,ST,Comp,SList,AList,GList,LList,RTime,TList).

lower_time_clauses(Clauses,_,_,_,_,_,_,_,_,[]) :-
	var(Clauses).
lower_time_clauses(Clauses,BT,ST,Comp,[Size|SList],[Adg|AList],[Gvars|GList],
	     [Ldg|LList],RTime,[Time|TList]) :-
	nonvar(Clauses),
	Clauses = [Clause|CList],
	lower_time_clause(Clause,BT,ST,Comp,Size,Adg,Gvars,Ldg,RTime,Time1),
	simplification(Time1,Time),
	lower_time_clauses(CList,BT,ST,Comp,SList,AList,GList,LList,RTime,TList).

nf_lower_time_clauses(Clauses,_,_,_,_,_,_,_,_,[]) :-
	var(Clauses).
nf_lower_time_clauses(Clauses,BT,ST,Comp,[Size|SList],[Adg|AList],[Gvars|GList],
	     [Ldg|LList],RTime,[Time|TList]) :-
	nonvar(Clauses),
	Clauses = [Clause|CList],
	nf_lower_time_clause(Clause,BT,ST,Comp,Size,Adg,Gvars,Ldg,RTime,Time1),
	simplification(Time1,Time),
	nf_lower_time_clauses(CList,BT,ST,Comp,SList,AList,GList,LList,RTime,TList).

%
%  Perform the time analysis for a clause.
%

% Upper bounds:

time_clause(Clause,BT,ST,Comp,Size,Adg,Gvars,Ldg,RTime,Time) :-
	clause_type(Clause,Type),
	time_clause(Type,Clause,BT,ST,Comp,Size,Adg,Gvars,Ldg,RTime,Time).

time_clause(2,Clause,BT,ST,Comp,Size,Adg,Gvars,Ldg,RTime,Time) :-
	Clause = (_:-Body),
	time_body(Body,1,Clause,BT,ST,Comp,Size,Adg,Gvars,Ldg,1,RTime,Time1),
	addition(Time1,1,Time).
time_clause(3,_,_,_,_,_,_,_,_,_,1).

% Lower bounds (non-failing predicate):

nf_lower_time_clause(Clause,BT,ST,Comp,Size,Adg,Gvars,Ldg,RTime,Time) :-
	clause_type(Clause,Type),
	nf_lower_time_clause(Type,Clause,BT,ST,Comp,Size,Adg,Gvars,Ldg,RTime,Time).

nf_lower_time_clause(2,Clause,BT,ST,Comp,Size,Adg,Gvars,Ldg,RTime,Time) :-
	Clause = (_:-Body),
	nf_lower_time_body(Body,1,Clause,BT,ST,Comp,Size,Adg,Gvars,
                                Ldg,1,RTime,Time1),
	addition(Time1,1,Time).
nf_lower_time_clause(3,_,_,_,_,_,_,_,_,_,1).

% Lower bounds (possible-failing predicate):

lower_time_clause(Clause,BT,ST,Comp,Size,Adg,Gvars,Ldg,RTime,Time) :-
	clause_type(Clause,Type),
	lower_time_clause(Type,Clause,BT,ST,Comp,Size,Adg,Gvars,Ldg,RTime,Time).

lower_time_clause(2,Clause,BT,ST,Comp,Size,Adg,Gvars,Ldg,RTime,Time) :-
	Clause = (_:-Body),
	lower_time_body(Body,1,Clause,BT,ST,Comp,Size,Adg,Gvars,
                                Ldg,1,RTime,Time1),
	addition(Time1,1,Time).
lower_time_clause(3,_,_,_,_,_,_,_,_,_,1).


%
%  Perform the time analysis for the body of a clause.
%
time_body((Lit,Body),LitNum,Clause,BT,ST,Comp,Size,Adg,Gvars,Ldg,Times,RTime,
		Time) :- !, % Added by PLG cut (1-Sep-97) 
	time_literal(Lit,LitNum,Clause,BT,ST,Comp,Size,Adg,Gvars,Ldg,RTime,
		Time1),
	(Lit == (!) ->
		Time2 = Time1;
		multiply(Times,Time1,Time2)),
	(Lit == (!) ->
		Times1 = 1; 
		(frequency_literal(Lit,LitNum,Clause,BT,ST,Comp,Size,Adg,
			Gvars,_,_,Sol2),
		 multiply(Times,Sol2,Times1))),
	LitNum1 is LitNum+1,
	time_body(Body,LitNum1,Clause,BT,ST,Comp,Size,Adg,Gvars,Ldg,Times1,
		  RTime,Time3),
	addition(Time2,Time3,Time).
time_body(Lit,LitNum,Clause,BT,ST,Comp,Size,Adg,Gvars,Ldg,Times,RTime,Time) :-
	nonsequence(Lit),
	time_literal(Lit,LitNum,Clause,BT,ST,Comp,Size,Adg,Gvars,Ldg,RTime,
		Time1),
	multiply(Times,Time1,Time).

% Added by PLG (23-Mar-97)

 %% approximation_time_body(Body,LitNum,Clause,BT,ST,Comp,Size,Adg,Gvars,
 %%                                 Ldg,Times,RTime,Time):-
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

not_ending_literal(X):- test_or_assignment(X), !.
not_ending_literal(X):- 
   functor(X, F, A),
%   debug_message("CALL: ~q", [non_fail_cover_pred_info(F/A, FailInfo, _CoverInfo)]), 
   non_fail_cover_pred_info(F/A, FailInfo, _CoverInfo), 
%   debug_message("EXIT: ~q", [non_fail_cover_pred_info(F/A, FailInfo, _CoverInfo)]), 
   flag_is_not_fails(FailInfo).

test_or_assignment(X):- var(X), !, fail.
test_or_assignment(\+(X)):- !, test_or_assignment(X).
test_or_assignment(X):-
   functor(X, F, A), 
   member(
      [ (!)/1,      
        (=)/2,      
        (==)/2,     
        (\==)/2,
        (is)/2, 
        (=:=)/2,     
        (=\=)/2,     
        (<)/2,      
        (>)/2,      
        (=<)/2,     
        (>=)/2,     
        (number)/1,
        (integer)/1,
        (atom)/1, 
        (atomic)/1
        ], F/A). 

lower_time_body((Lit,Body),LitNum,Clause,BT,ST,Comp,Size,Adg,Gvars,Ldg,
                Times, RTime, Time):- !, 
   (not_ending_literal(Lit) ->
	time_literal(Lit,LitNum,Clause,BT,ST,Comp,Size,Adg,Gvars,Ldg,RTime,
		Time1),
	LitNum1 is LitNum+1,
	lower_time_body(Body,LitNum1,Clause,BT,ST,Comp,Size,Adg,Gvars,Ldg,
                       Times, RTime,Time3),
	addition(Time1,Time3,Time)
       ; Time is 0
      ).
lower_time_body(Lit,LitNum,Clause,BT,ST,Comp,Size,Adg,Gvars,Ldg,Times,RTime,
               Time):-
   nonsequence(Lit),
   (not_ending_literal(Lit) ->
	time_literal(Lit,LitNum,Clause,BT,ST,Comp,Size,Adg,Gvars,Ldg,RTime,
		Time1),
	multiply(Times,Time1,Time)
        ; Time is 0).


nf_lower_time_body((Lit,Body),LitNum,Clause,BT,ST,Comp,Size,Adg,Gvars,Ldg,
                Times, RTime, Time):- !, 
	time_literal(Lit,LitNum,Clause,BT,ST,Comp,Size,Adg,Gvars,Ldg,RTime,
		Time1),
	LitNum1 is LitNum+1,
	nf_lower_time_body(Body,LitNum1,Clause,BT,ST,Comp,Size,Adg,Gvars,Ldg,
                       Times, RTime,Time3),
	addition(Time1,Time3,Time).
nf_lower_time_body(Lit,LitNum,Clause,BT,ST,Comp,Size,Adg,Gvars,Ldg,Times,RTime,
               Time):-
   nonsequence(Lit),
   time_literal(Lit,LitNum,Clause,BT,ST,Comp,Size,Adg,Gvars,Ldg,RTime,
		Time1),
   multiply(Times,Time1,Time).

% End added

%
%  Perform the time analysis for a literal.
%
time_literal(Lit,LitNum,Clause,BT,ST,Comp,Size,Adg,Gvars,_,RTime,Time) :-
	functor(Lit,F,A),
	(second_order_predicate(F/A) ->
		(second_order_predicate_pred_arg(Lit,Lit1),
		 functor(Lit1,F1,A1),
		 arg(2,Clause,Body),
		 second_order_predicate_pred_num(Body,LitNum,Num1));
		(F1 = F, A1 = A, Num1 = LitNum)),
	literal_output_comp(F1/A1,Num1,1,BT,ST,Comp,Adg,time,RTime,LitTime),
	normalize_time_function(LitTime,F1/A1,Num1,BT,ST,Comp,Clause,
		Adg,Gvars,Size,RTime,Time).

frequency_literal(Lit,LitNum,Clause,BT,ST,Comp,Size,Adg,Gvars,_,_,Sol) :-
	functor(Lit,F,A),
	literal_output_comp(F/A,LitNum,1,BT,ST,[],Adg,det,[],Sol1),
	normalize_solution_function(Sol1,F/A,LitNum,BT,ST,Comp,Clause,
			Adg,Gvars,Size,[],Sol).
/*
	(second_order_predicate(F/A) ->
		(second_order_predicate_pred_arg(Lit,Lit1),
		 functor(Lit1,F1,A1),
		 arg(2,Clause,Body),
		 second_order_predicate_pred_num(Body,LitNum,Num1));
		(F1 = F, A1 = A, Num1 = LitNum)),
	literal_output_comp(F1/A1,Num1,1,BT,ST,[],Adg,det,[],Sol1),
	normalize_solution_function(Sol1,F1/A1,Num1,BT,ST,Comp,Clause,
			Adg,Gvars,Size,[],Sol2),
	(second_order_predicate(F/A) ->
		(gen_literal_iopos(Adg,F1/A1,Num1,(-),Pos),
		 pos_var(Pos,Lit1,Vars),
		 arg(1,Lit,Arg1),
		 term_var(Arg1,Var1),
		 (opened_set_equivalent(Var1,Vars) ->
			Sol = 1;
			Sol = Sol2));
		Sol = Sol2).
*/


%
%  Normalize the time function of a literal.
%
normalize_time_function(LitTime,LitName,LitNum,BT,ST,Comp,Clause,Adg,Gvars,
			Size,RTime,Time) :-
	gen_clause_pos(Adg,PosSet),
	(recursive_clause(Clause,Comp) ->
		(ith_clause_literal(0,Clause,Lit),
		 functor(Lit,F,N),
		 find_symbol_field(ST,F/N,size,ISz));
		ISz = Size),
	gen_literal_iopos(Adg,LitName,LitNum,(+),Pos),
	init_normalize_queue(Pos,QHd,QTl),
	normalize(LitTime,QHd,QTl,BT,ST,[],Clause,Adg,Gvars,PosSet,ISz,RTime,Time).





%% Control version comment prompting for the file.
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "off"
%% End:

