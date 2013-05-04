:- module(assrt_ctchecks_pred,
	[
	simplify_assertions/2
        ],
	[ assertions
	]).

%% Own library:
:- use_module(ctchecks(assrt_ctchecks_common), 
	[ 
%% 	  num_var_in_goal/4,
	  check_type_calls/2,
	  get_assertions/3,
	  recorda_assertion/2,
	  synt_compose_disj/3,
	  synt_compose_conj/3
%% 	  statically_comp/4
%% 	  type/1
	]).
%% :- use_module(ctchecks(assrt_ctchecks_pp), [pp_abs_execute_type/4]).
:- use_module(ciaopp(preprocess_flags), [current_pp_flag/2]).

:- use_module(ctchecks(comp_ctchecks), [abs_execute_comp/5]).
:- use_module(ctchecks(ctchecks_messages), 
	[ message_calls/7,
	  message_success/8,
	  message_comp/7,
	  message_builtin/5
	]).

%% CiaoPP library:
%% :- use_module(domain(typeslib),
%% 	[ dz_type_included/2,
%% 	  set_top_type/1
%%  	]).
:- use_module(plai(domains), 
	[ abs_sort/3
%% 	  compute_lub/3,
%% 	  identical_abstract/3,
%% 	  full_info_to_asub/4
	]).
:- use_module(infer(infer), [get_completes_lub/6]).
:- use_module(infer(infer_dom), [abs_execute_with_info/4]).
%% :- use_module(spec(abs_exec),[ cond/4 ]).
:- use_module(spec(s_simpspec), [make_atom_list/2]).
:- use_module(program(assrt_db), [assertion_read/9, assertion_body/7]).
:- use_module(program(assrt_norm), [denorm_goal_prop/3]). %, norm_goal_prop/3
:- use_module(program(p_unit), [predicate_names/1]).

%% Ciao library:
%% :- use_module(library(idlists),	[ memberchk/2 ]).
%% :- use_module(library(lists), [ append/3, length/2 ]).
:- use_module(library(messages), [note_message/3]).
:- use_module(library(terms_check), [ask/2]).
%% :- use_module(library(terms_vars), [ varset/2]).

%% :- multifile term_domain/1.
%% 
%% once_input_user_interface(AbsInt,TmpCall,Vars,NewCall):-
%% 	full_info_to_asub(AbsInt,TmpCall,Vars,NewCall),!.
%% 
%% set_type_of_term(Term,Type,Typing):-
%% 	norm_goal_prop(Type,Typing,Term).
%% 
%% lit_type_to_pure_type_lit(_Goal,ASub0,ASub):-
%% 	rewritetype(ASub0,ASub).

%-------------------------------------------------------------------

:- doc(bug,"1. Multivariant analysis is not fully exploited. Multiple
	versions are collapsed into one before simplifying
	assertions").
:- doc(bug,"2. Should be reprogrammed so as to not depend on any AbsInt,
	but use everything that has been analyzed (i.e., use infer.pl).").
:- doc(bug,"3. Checks for comp assertions not yet integrated.").
:- doc(bug,"4. Assertions turn true are not shown.").

%-------------------------------------------------------------------
% entry point of assertion simplification at compile time.

:- pred simplify_assertions(+Types,+Modes)
	# "Entry point for simplification of assertions.".

simplify_assertions(Types,Modes):-
	predicate_names(Preds),
	make_atom_list(Preds,Keys),
	get_assertions(Keys,Preds,Basic),
	abs_execute_assertions(Basic,Types,Modes),
%	check_calls_if_builtin(Keys,Preds,Types,Modes), is useless!
	!.

%-------------------------------------------------------------------

abs_execute_assertions([],_,_).
abs_execute_assertions([(Key,Goal,Ass)|Basic],Types,Modes):-
	decide_get_info(Types,Key,Goal,TypesInfo),
	decide_get_info(Modes,Key,Goal,ModesInfo),
	abs_execute_ass_predicate(Ass,Key,Goal,TypesInfo,ModesInfo),
	abs_execute_assertions(Basic,Types,Modes).	

%% rewritetype([],[]).
%% rewritetype([V:T|TCallT],[F|TCall0]):-
%% 	rewritetypeasub(T,V,F),
%% 	rewritetype(TCallT,TCall0).
%% 
%% rewritetypeasub((_N,T),V,F):- !,
%% 	set_type_of_term(V,T,F).
%% rewritetypeasub(T,V,F):-
%% 	set_type_of_term(V,T,F).

decide_get_info(none,_Key,_Goal,[]):-!.
decide_get_info(Modes,Key,MGoal,[MGoal,MCall,MSuccess,Modes]):-
	get_completes_lub(Key,MGoal,Modes,yes,MCall,MSuccess), !.
decide_get_info(_Modes,_Key,_Goal,[]).


abs_execute_ass_predicate([],_Key,_Goal,_TypesInfo,_ModesInfo).
abs_execute_ass_predicate([A|Ass],Key,Goal,TypesInfo,ModesInfo):-
	abs_exec_one_assertion(TypesInfo,A,Key,NA,Flag),
	( new_status(Flag,A,Goal) -> true
	; abs_exec_one_assertion(ModesInfo,NA,Key,NNA,NFlag),
	  ( new_status(NFlag,NA,Goal) -> true
	  ; abs_exec_comp_assertion(NNA,Key,NNNA,NNFlag),
	    ( new_status(NNFlag,NNA,Goal) -> true
	    ; recorda_assertion(Goal,NNNA),
	      current_pp_flag( ass_not_stat_eval , STAT ),
	      (
		  STAT == nothing
	      ->
	          true
	      ;
		  message( STAT , ['could not reduce: ' , A , ' with goal: ' , Goal] )
	      )
	) ) ),
	abs_execute_ass_predicate(Ass,Key,Goal,TypesInfo,ModesInfo).



new_status(Flag,A,Goal):-
	Flag \== check,
	modify_status(Flag,A,A0),
	recorda_assertion(Goal,A0).




modify_status(Status,A,NA):-
	A = assert(Id,check,C,D,E),
	current_pp_flag(verbose_ctchecks,VCT),
	(VCT == on -> 
	    note_message(Id,"~w assertion has new status: ~w",[C,Status])
	;
	    true
	),
	NA = assert(Id,Status,C,D,E).

abs_exec_one_assertion([],A,_Key,A,check).
abs_exec_one_assertion([AGoal,ACall,ASucc,AbsInt],A,Key,NA,Status):-
	A = assert(Id,check,success,ABody,Dict),!,
	assertion_body(Goal,_Compat,Call,Success,_Comp,_Comm,ABody),
	abs_execute_exp(Goal,Call,AbsInt,AGoal,ACall,NewCall),
	abs_execute_exp(Goal,Success,AbsInt,AGoal,ASucc,NewSuccess),
	reduce_success(NewCall,NewSuccess,Status),
	NA = assert(Id,check,success,NBody,Dict),
	assertion_body(Goal,_Compat,NewCall,NewSuccess,_Comp,_Comm,NBody),
	decide_error_success(Status,AbsInt,Goal,Call,Success,Dict,Id,Key,ASucc).
abs_exec_one_assertion([AGoal,ACall,_ASucc,AbsInt],A,Key,NA,Status):-
	A = assert(Id,check,calls,ABody,Dict),!,
	assertion_body(Goal,_Compat,Calls,_Succ,_Comp,_Comm,ABody),
	abs_execute_exp(Goal,Calls,AbsInt,AGoal,ACall,NewCalls),
	reduce_calls(NewCalls,Status),
	NA = assert(Id,check,calls,NBody,Dict),
	assertion_body(Goal,_Compat,NewCalls,_Succ,_Comp,_Comm,NBody),
	decide_error_calls(Status,AbsInt,Goal,Calls,Dict,Id,Key,ACall).
abs_exec_one_assertion([AGoal,ACall,_ASucc,AbsInt],A,_Key,NA,Status):-
	A = assert(Id,check,comp,ABody,Dict),
	assertion_body(Goal,_Compat,Call,_Succ,Comp,_Comm,ABody),
	abs_execute_exp(Goal,Call,AbsInt,AGoal,ACall,NewCall),
	Status = check,
	NA = assert(Id,Status,comp,NBody,Dict),
	assertion_body(Goal,_Compat,NewCall,_Succ,Comp,_Comm,NBody).

abs_exec_comp_assertion(A,Key,NA,Status):-
	A = assert(Id,check,comp,ABody,Dict), !,
	assertion_body(Goal,_Compat,Call,_Succ,Comp,_Comm,ABody),
	abs_execute_comp(Goal,Comp,AComp,NewComp,Status0),
	reduce_comp(Call,Status0,Status),
	NA = assert(Id,check,comp,NBody,Dict),
	assertion_body(Goal,_Compat,Call,_Succ,NewComp,_Comm,NBody),
	decide_error_comp(Status,Goal,Call,Comp,Dict,Id,Key,AComp).
abs_exec_comp_assertion(A,_Key,A,check).

%% abs_execute_exp(Goal,Exp,AbsInt,Goal,Info,NewExp):-
%% 	term_domain(AbsInt),!,
%% 	pred_abs_execute_with_info(Exp,AbsInt,Goal,Info,NewExp).
abs_execute_exp(Goal,Exp,AbsInt,AGoal,Info_u,NewExp):-
	ask(Goal,AGoal),
	ask(AGoal,Goal),
	Goal = AGoal, %temporary solution
	abs_sort(AbsInt,Info_u,Info),
	pred_abs_execute_with_info(Exp,AbsInt,_,Info,NewExp).

pred_abs_execute_with_info([Exp1],AbsInt,Goal,Info,NewExp):-!,
	pred_abs_execute_with_info(Exp1,AbsInt,Goal,Info,NewExp).
pred_abs_execute_with_info([Exp1|Exp2],AbsInt,Goal,Info,NewExp):-!,
	pred_abs_execute_with_info(Exp1,AbsInt,Goal,Info,NewExp1),
	(NewExp1 == fail ->
	    NewExp = fail
	;
	    pred_abs_execute_with_info(Exp2,AbsInt,Goal,Info,NewExp2),
	    synt_compose_conj(NewExp1,NewExp2,NewExp)).
pred_abs_execute_with_info((Exp1,Exp2),AbsInt,Goal,Info,NewExp):-!,
	pred_abs_execute_with_info(Exp1,AbsInt,Goal,Info,NewExp1),
	(NewExp1 == fail ->
	    NewExp = fail
	;
	    pred_abs_execute_with_info(Exp2,AbsInt,Goal,Info,NewExp2),
	    synt_compose_conj(NewExp1,NewExp2,NewExp)).
pred_abs_execute_with_info((Exp1;Exp2),AbsInt,Goal,Info,NewExp):-!,
	pred_abs_execute_with_info(Exp1,AbsInt,Goal,Info,NewExp1),
	(NewExp1 == true ->
	    NewExp = true
	;
	    pred_abs_execute_with_info(Exp2,AbsInt,Goal,Info,NewExp2),
	    synt_compose_disj(NewExp1,NewExp2,NewExp)).
%% pred_abs_execute_with_info(Prop,AbsInt,Goal,Info,Sense):-
%% 	term_domain(AbsInt), !,
%% 	(type(Prop) ->
%% 	    pp_abs_execute_type(Prop,Goal,Info,Sense)
%% 	;
%% 	    Sense = Prop).
pred_abs_execute_with_info(Prop,AbsInt,_,Info,Sense):-
	abs_execute_with_info(AbsInt,Info,Prop,Sense),!.
pred_abs_execute_with_info(Prop,_AbsInt,_Goal,_Info,Prop).


reduce_calls(true,checked):-!.  % -
reduce_calls(fail,false):-!.    % +
reduce_calls(_Calls,check).

reduce_success(fail,_Success,checked):-!.  % -
reduce_success(_Call,true,checked):-!.     % +
reduce_success(_,fail,false):-!.           % -
reduce_success(_,_,check).

reduce_comp(fail,_Comp,checked):-!.
reduce_comp(_Call,true,checked):-!.
reduce_comp(_Call,Comp,Comp).


%% decide_error_calls(false,AbsInt,Goal,Calls,Dict,Id,Key,ACalls):-
%% 	term_domain(AbsInt),!,
%% 	message_calls_with_types(Goal,Calls,Dict,Id,Key,ACalls).
decide_error_calls(false,AbsInt,Goal,Calls,Dict,Id,Key,ACalls):-!,
	message_calls(AbsInt,Goal,Calls,Dict,Id,Key,ACalls).
decide_error_calls(_,_AbsInt,_Goal,_Calls,_Dict,_Id,_Key,_).

%% decide_error_success(false,AbsInt,Goal,Call,Succ,Dict,Id,Key,ASuccess):-
%% 	term_domain(AbsInt),!,
%% 	message_success_with_types(Goal,Call,Succ,Dict,Id,Key,ASuccess).
decide_error_success(false,AbsInt,Goal,Call,Succ,Dict,Id,Key,ASuccess):-!,
	message_success(AbsInt,Goal,Call,Succ,Dict,Id,Key,ASuccess).
decide_error_success(_,_AbsInt,_Goal,_Call,_Succ,_Dict,_Id,_Key,_).

decide_error_comp(false,Goal,Call,Comp,Dict,Id,Key,AComp):-!,
        denorm_all_comp_props(Comp,NewComp),
	message_comp(Goal,Call,NewComp,Dict,Id,Key,AComp).
decide_error_comp(_,_Goal,_Call,_Comp,_Dict,_Id,_Key,_).

denorm_all_comp_props((NC,NComp),(C,Comp)):- !,
	denorm_goal_prop(NC,C,_),
        denorm_all_comp_props(NComp,Comp).
denorm_all_comp_props(NC,C):-
	denorm_goal_prop(NC,C,_).

/* Dead code? (PBC)
% if the preconditions are valid (superset of the analysis calls) and 
% the posconditions are also valid (superset of the analysis success)
% then the assertion is valid
success_assertion_holds(AbsInt,Key,Goal:Call=>Succ):-
	superset_analysis_of_assertion(Call,Key,Goal,AbsInt),
	postcond_holds_exp(Succ,Key,Goal,AbsInt).

postcond_holds_exp((Prop1,Prop2),Key,Goal,AbsInt):-!,
	postcond_holds_exp(Prop1,Key,Goal,AbsInt),
	postcond_holds_exp(Prop2,Key,Goal,AbsInt).
postcond_holds_exp((Prop1;Prop2),Key,Goal,AbsInt):-!,
	(postcond_holds_exp(Prop1,Key,Goal,AbsInt)
	;
	 postcond_holds_exp(Prop2,Key,Goal,AbsInt)).
postcond_holds_exp(Succ,Key,Goal,AbsInt):-
	postcond_holds_prop(AbsInt,Succ,Key,Goal).

postcond_holds_prop(AbsInt,Prop,Key,Goal):-
	term_domain(AbsInt),!,
	type(Prop),
	arg(1,Prop,Var),
	functor(Goal,F,A),
	num_var_in_goal(A,Goal,Var,Num),
	functor(Goal0,F,A),
	make_atom([F,A],Key), %% AADEBUG
	get_completes_lub(Key,Goal0,AbsInt,yes,_Call,Body),!,
	arg(Num,Goal0,Var0),
	get_arg_type(Body,Var0,ArgN),
	functor(ArgN,TypeA,1),
	functor(Prop,TypeP,1),
	dz_type_included(TypeA, TypeP).
postcond_holds_prop(AbsInt,Prop,Key,Goal):-!,
	get_completes_lub(Key,AGoal,AbsInt,yes,_ACall,ASucc),
	ask(Goal,AGoal),
	ask(AGoal,Goal),
	Goal = AGoal, %temporary solution
	abs_sort(AbsInt,ASucc,Info),
	functor(Prop,F,A),
	statically_comp(AbsInt,F/A,true,Condition),
	cond(Condition,AbsInt,Prop,Info).

% Note that only conjunctions are treated in preconditions !!
collect_type_properties((Left,Right),Types,Others):-!,
	type(Left),!,
	Types=[Left|MoreTypes],
	collect_type_properties(Right,MoreTypes,Others).
collect_type_properties((Left,Right),Types,[Left|Others]):-!,
	collect_type_properties(Right,Types,Others).
collect_type_properties(true,[],[]):-!.
collect_type_properties(Prop,Types,Others):-!,
	(type(Prop) ->
	    Types = [Prop], Others = []
	;
	    Types = [], Others = [Prop]).

fill_type_arguments(Goal,Types,AllTypes):-
	functor(Goal,_F,N),
	fill_n_arg_type(N,Goal,Types,AllTypes).

fill_n_arg_type(0,_Goal,Types,Types):-!.
fill_n_arg_type(N,Goal,Types,AllTypes):-
	arg(N,Goal,Var),
	N1 is N -1,
	(type_for_var(Var,Types) ->
	    fill_n_arg_type(N1,Goal,Types,AllTypes)
	;
	    set_top_type(T),
	    set_type_of_term(Var,T,Any),
	    AllTypes = [Any|MoreTypes],
	    fill_n_arg_type(N1,Goal,Types,MoreTypes)).
	
type_for_var(Var,[X|_]):-
	X=..[_|Args],
	memberchk(Var,Args),!.
type_for_var(Var,[_|Xs]):-
	type_for_var(Var,Xs).

superset_analysis_of_assertion(Call,Key,Goal,AbsInt):-
	decide_superset_types(AbsInt,Call,Goal,Key,Others),
	list_to_conj(Others,OthersConj),
	superset_analysis_of_assertion_exp(OthersConj,Key,Goal,AbsInt).

decide_superset_types(AbsInt,Call,Goal,Key,Others):-
	term_domain(AbsInt),!,
	collect_type_properties(Call,Types,Others),
	(Types \== [true] ->
	    fill_type_arguments(Goal,Types,AllTypes),
	    all_types_hold(AllTypes,AbsInt,Goal,Key)
	;
	    true).
decide_superset_types(Others,_Call,_Goal,_Key,Others).

all_types_hold([],_,_,_).
all_types_hold([Prop|Types],AbsInt,Goal,Key):-
	arg(1,Prop,Var),
	functor(Goal,F,A),
	num_var_in_goal(A,Goal,Var,Num),
	functor(Goal0,F,A),
	make_atom([F,A],Key), %% AADEBUG
	get_completes_lub(Key,Goal0,AbsInt,yes,_Call,Body),!,
	arg(Num,Goal0,Var0),
	get_arg_type(Body,Var0,ArgN),
	functor(ArgN,TypeA,1),
	functor(Prop,TypeP,1),
	hack_if_any(TypeP,TypeA),
	all_types_hold(Types,AbsInt,Goal,Key).

hack_if_any(Type,Type):-!.
hack_if_any(Pdef,Adef):-
	dz_type_included(Pdef, Adef).

superset_analysis_of_assertion_exp((Prop1,Prop2),Key,Goal,AbsInt):-!,
	(superset_analysis_of_assertion_exp(Prop1,Key,Goal,AbsInt)
	;
	 superset_analysis_of_assertion_exp(Prop2,Key,Goal,AbsInt)).
superset_analysis_of_assertion_exp((Prop1;Prop2),Key,Goal,AbsInt):-!,
	superset_analysis_of_assertion_exp(Prop1,Key,Goal,AbsInt),
	superset_analysis_of_assertion_exp(Prop2,Key,Goal,AbsInt).
superset_analysis_of_assertion_exp(true,_Key,_Goal,_AbsInt):-!.
superset_analysis_of_assertion_exp(Call,Key,Goal,AbsInt):-
	superset_analysis_of_assertion_prop(AbsInt,Call,Key,Goal).

superset_analysis_of_assertion_prop(typeshfr,Call,Key,Goal):-!,
	 superset_analysis_of_assertion_prop(shfr,Call,Key,Goal).
superset_analysis_of_assertion_prop(AbsInt,Call,Key,Goal):-
	get_completes_lub(Key,AGoal,AbsInt,yes,ACall,_ASucc),
	ask(Goal,AGoal),
	ask(AGoal,Goal),
	Goal = AGoal, %temporary solution
	abs_sort(AbsInt,ACall,Call1),
        varset(Goal,Vars),
	conj_to_list(Call,TmpCall),
        once_input_user_interface(AbsInt,TmpCall,Vars,NewCall),
	compute_lub(AbsInt,[NewCall,Call1],Lub),
	identical_abstract(AbsInt,Lub,Call1).


% I do not know how to implement this yet
incompatible(_Goal,_Call,_AGoal,_Call1):-
	fail.


get_arg_type([T|_Ts],Var0,ArgN):-
	arg(1,T,Arg),
	Arg == Var0,!,
	ArgN =T.
get_arg_type([_|Ts],Var0,ArgN):-
	get_arg_type(Ts,Var0,ArgN).


list_of_types_or_any(Prop,Goal,TypeList):-
	functor(Goal,_F,A),
	length(TypeList,A),
	type_or_any(TypeList,Goal,Prop,1).


type_or_any([],_Goal,_Prop,_A).
type_or_any([Type|Types],Goal,Prop,A):-
	arg(A,Goal,Arg),
	arg(1,Prop,X),
	(X == Arg ->
	    Type = Prop
	;
	    set_top_type(Any),
	    functor(Type,Any,1),
	    arg(1,Type,Arg)),
	A1 is A+1,
	type_or_any(Types,Goal,Prop,A1).

list_of_any([]).
list_of_any([Any|Anies]):-
	set_top_type(Any),
	list_of_any(Anies).
*/


%-------------------------------------------------------------------

% this is useless, since builtins are NOT in the list of Preds!
check_calls_if_builtin([],[],_,_).
check_calls_if_builtin([Key|Keys],[F/A|Preds],Types,Modes):-
	functor(Goal,F,A),
	check_type_calls(Goal,Calls),!,
	get_completes_lub(Key,AGoal,Types,yes,Calls1,_Succ1),
	abs_execute_exp(Goal,Calls,Types,AGoal,Calls1,NewCalls),
	reduce_calls(NewCalls,Status),
	( Status=false ->
	  message_builtin(Goal,Key,Types,Calls1,Calls)
	; get_completes_lub(Key,AGoal,Modes,yes,Calls2,_Succ2),
	  abs_execute_exp(Goal,NewCalls,Modes,AGoal,Calls2,RestCalls),
	  reduce_calls(RestCalls,false) ->
	  message_builtin(Goal,Key,Modes,Calls2,Calls)
	; true
	),
	check_calls_if_builtin(Keys,Preds,Types,Modes).
check_calls_if_builtin([_|Keys],[_|Preds],Types,Modes):-
	check_calls_if_builtin(Keys,Preds,Types,Modes).
