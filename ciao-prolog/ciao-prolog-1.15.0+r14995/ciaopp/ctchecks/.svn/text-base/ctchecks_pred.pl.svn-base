% --- Creo que si la expresion de abs_execute_one_asertion se 
% se reduce, pero no llega a valer fail, entonces el list_to_conj fallaria.

:- module( ctchecks_pred, [ simplify_assertions_all/1, ctchecks_log/5 ],
	[ assertions , api( ciaopp_api ) ,isomodes] ).

:- use_module(ciaopp(preprocess_flags)).

:- use_module(ctchecks(comp_ctchecks), [abs_execute_comp/5, abs_execute_sizes/5]).
:- use_module(ctchecks(ctchecks_pred_messages), [inform_as_changes_to_user/1]).

%% CiaoPP library:

:- use_module(plai(domains), 
   [
     glb/4, 
     info_to_asub/5, 
     unknown_call/4,
     call_to_entry/9,
     identical_abstract/3]).

:- use_module(program(p_unit), [entry_assertion/3]).

:- use_module(infer(infer), [get_completes_lub/6, get_completes/4, get_info/5]).
:- use_module(ctchecks(ctchecks_common)).

%% Ciao library:
:- use_module(library(terms_vars), [varset/2]).
:- use_module(library(lists), [append/3]).
:- use_module(library(formulae)).

%[LD] for interval
%:- use_module(ciaopp(preprocess_flags),[current_pp_flag/2]).
:- use_module(infercost(algebraic(polynom_comp)),
	[
	    polynom_current_assertion/1
	]).
%[/LD] for interval

%-------------------------------------------------------------------
% all fixed (PP):
%:- doc(bug,"1. Multivariant analysis is not fully exploited. Multiple
%	versions are collapsed into one before simplifying
%	assertions").
%:- doc(bug,"2. Should be reprogrammed so as to not depend on any AbsInt,
%	but use everything that has been analyzed (i.e., use infer.pl).").
%:- doc(bug,"3. Checks for comp assertions not yet integrated.").
%:- doc(bug,"4. Assertions turn true are not shown.").

:- doc(bug,"5. Compatibility properties are not considered at all.").

%-------------------------------------------------------------------


 
% DTM: close your eyes here!! PP: how cute...
get_pp_info( P , PP_info ) :- 
	functor( P , N , A ),
	number_codes( A , AC ),
	atom_codes( NA , AC ),
	atom_concat( N  , '/' , P1 ),
	atom_concat( P1 , NA , PP_info ).


decide_get_info(none,_Key,_Goal,[]):-!.
% set of completes
decide_get_info(AbsInt,Key,MGoal,Completes):-
	current_pp_flag(multivariant_ctchecks, on),!,
	get_completes(Key,MGoal,AbsInt,Completes).
% completes lub
decide_get_info(AbsInt,Key,MGoal,[complete(MGoal,MCall,[MSuccess],Key,lub)]):-
%	current_pp_flag(collapse_ai_vers, on),
	current_pp_flag(multivariant_ctchecks, off),
 	get_completes_lub(Key,MGoal,AbsInt,yes,MCall,MSuccess),!.
decide_get_info(AbsInt,Key,MGoal,Info) :-
	get_info(AbsInt,pred,Key,MGoal,Info),!.
decide_get_info(_AbsInt,_Key,_Goal, []).

:- data ctchecks_log/5.
:- pred ctchecks_log(?Abs,?Status,?Type,?Key,?Id) + no_rtcheck # 
	"Returns upon backtracking completes (@var{Key}+@var{Id}+@var{Abs}) 
         which participated during CT checking in giving assertion of 
         type @var{Type} (calls or success) status @var{check}.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred simplify_assertions_all(+Domains)
	# "Entry point for simplification of assertions (all domains
	   dealing with calls and successes).".

simplify_assertions_all(Domains):-
	retractall_fact(ctchecks_log(_,_,_,_,_)),
	get_key_predicates( [internal] , Preds ),
	process_preds_all( Preds , Domains ).

process_preds_all([], _).
process_preds_all([P|Ps], D) :-
	get_assertions(P, as${ status => check , type => Type }, 
			 (Type == calls ; Type == success; Type == comp),
			LA ),
	check_pred_all(P, LA, D),
	process_preds_all(Ps, D).


check_pred_all(P, LA, Domains) :-
	get_pp_info(P, PP_info),
	decide_get_info_all(Domains, PP_info, P, Info),
	abs_execute_ass_predicate_all(LA, PP_info, P, Domains, Info, NLA),
	inform_as_changes_to_user(NLA).


decide_get_info_all([],_,_,[]).
decide_get_info_all([D|Ds],PP_info,P,[I|Is]):-
	decide_get_info(D,PP_info,P,I),
	decide_get_info_all(Ds,PP_info,P,Is).

%[LD] some modification on following code

%Commenting old code
%abs_execute_ass_predicate_all([], _Key, _Goal, _Domains, _Info, []).
%abs_execute_ass_predicate_all([A|Ass], Key, Goal, Domains, Info, Out) :-
%	abs_exec_one_assertion_all(Domains, Info, A ,Key, DomsOut, InfoOut, NA, _Status),
%	Out = [u(A,NA,DomsOut,InfoOut)|AsR],
%	abs_execute_ass_predicate_all(Ass, Key, Goal, Domains, Info, AsR).

%Modified code
abs_execute_ass_predicate_all([], _Key, _Goal, _Domains, _Info, []).
abs_execute_ass_predicate_all([A|Ass], Key, Goal, Domains, Info, Out) :-
	current_pp_flag( ctchecks_intervals , on ),
	%assert fact for interval information
	asserta_fact(polynom_current_assertion(A)), %%LD
	abs_exec_one_assertion_all(Domains, Info, A ,Key, DomsOut, InfoOut, NA, _Status),
	Out = [u(A,NA,DomsOut,InfoOut)|AsR],
	%retract fact for interval information
	retractall_fact(polynom_current_assertion(A)), %%LD
	abs_execute_ass_predicate_all(Ass, Key, Goal, Domains, Info, AsR).


abs_execute_ass_predicate_all([A|Ass], Key, Goal, Domains, Info, Out) :-
	current_pp_flag( ctchecks_intervals , off ),
	abs_exec_one_assertion_all(Domains, Info, A ,Key, DomsOut, InfoOut, NA, _Status),
	Out = [u(A,NA,DomsOut,InfoOut)|AsR],
	abs_execute_ass_predicate_all(Ass, Key, Goal, Domains, Info, AsR).
%[/LD] some modification


abs_exec_one_assertion_all([], [], A, Key, [generic_comp], [AInfoOut], NAOut, StatusOut):-
	abs_exec_comp_assertion(A, Key, AInfo, NA, Status),
	( Status \== check ->
	  AInfoOut = AInfo,
	  NAOut = NA,
	  StatusOut = Status
	; abs_exec_size_assertion(NA, Key, AInfo1, NAOut, StatusOut),
	  append(AInfo, AInfo1, AInfoOut)
	).

abs_exec_one_assertion_all([D|Ds], [I|Is], A, Key, DomOut, InfoOut, NewA, Status) :-
	abs_exec_one_assertion(D, I, A, Key, NA ,Flag),
	( Flag = false, DomOut = [D], InfoOut = [I],!
	; Flag = checked, DomOut = [], InfoOut = [],!
	; true
	),
	( new_status(Flag,A,_Goal) -> 
	  Status = Flag,
	  NewA = NA
	; 
	  abs_exec_one_assertion_all(Ds, Is, NA, Key, DomOut1, InfoOut1, NewA, Status),
	  (  Status = check ->
	     DomOut = [D|DomOut1], InfoOut = [I|InfoOut1]
	  ;
	     DomOut = DomOut1, InfoOut = InfoOut1 
	  )
	).
				  

%------------------------------------------------------------------------------

abs_exec_complete_cs(_Goal,_Call,_Succ,_AbsInt,[],nosucc,nosucc,nosucc).
abs_exec_complete_cs(Goal,Call,Succ,AbsInt,
	             [complete(AGoal,ACall,ASuccs,Key,Id)|Cmpls],
		      NCalls,NSuccs,Status):-
	abs_execute_exp(Goal,Call,AbsInt,AGoal,ACall,NCall),
	abs_exec_each_succ(Goal,Call,Succ,AbsInt,AGoal,ASuccs,NCall,
	                   LocalNSucc,LocalStatus),
	reduce_compl_fin(LocalStatus,LogStatus),
	assertz_fact(ctchecks_log(AbsInt,LogStatus,success,Key,Id)),		   
	( LocalStatus == true, is_perfect_match(AbsInt,Goal,ACall) ->
	  Status = perfect
	; abs_exec_complete_cs(Goal,Call,Succ,AbsInt,Cmpls,NCalls0,NSuccs0,Status0),
	  reduce_compl(LocalStatus, Status0, Status),
	  compose_cs_goals(Status,Call,NCalls0,NCall,NCalls),        % left to prove goals of calls
	  compose_cs_goals(Status,Succ,LocalNSucc,NSuccs0,NSuccs)   % left to prove goals of successes
	).

% checks wether there is an entry exactly matching the precondition,
% in which case the assertion may get status true instaed of checked.
is_perfect_match(Abs,Call,ACall) :- 
	entry_assertion(Call,Entry,_),
	varset(Call,Vs),
	info_to_asub(Abs,_,Entry,Vs,EntryASub),
	identical_abstract(Abs,ACall,EntryASub).

compose_cs_goals(dont_know,OrigL,G1,G2,Orig) :-
%	G1 \== G2, G2 \== fail, G1 \== fail,!,
	G1 \== G2, G2 \== nosucc, G1 \== nosucc,!,
	list_to_conj(OrigL,Orig).
compose_cs_goals(_Status,Orig,G1,G2,GOut) :-
	synt_compose_disj(Orig,G1,G2,GOut).

abs_exec_each_succ(_Goal, _Call, _Succ, _AbsInt, _AGoal, [], _NCall, nosucc, nosucc).
abs_exec_each_succ(Goal, Call, Succ, AbsInt, AGoal, [ASucc|ASuccs], NCall, NSucc, Status):-
	current_pp_flag(pred_ctchecks, Flag), 
	Flag == on_succ,
	!,
 	varset(Goal, Gv),
 	varset(ASucc, ASv),
 	info_to_asub(AbsInt, _, Call, Gv, Cond), 
 	unknown_call(AbsInt, Gv, Cond, Cond0),
	call_to_entry(AbsInt, Gv, Goal, ASv, AGoal, [], Cond0, Cond1, _ExtraInfo),
 	glb(AbsInt, Cond1, ASucc, CondASucc),
  	(   CondASucc = '$bottom' ->  % no success possible with current Pre, thus this
                                      % complete should be "neutral" for the whole assertion
	    LocalStatus = nosucc,
	    NSuccess = fail
  	;
	    abs_execute_exp(Goal, Succ, AbsInt, AGoal, CondASucc, NSuccess),
	    reduce_success(NCall, NSuccess, LocalStatus0),
	    reduce_compl_fin(LocalStatus, LocalStatus0)
  	),
	abs_exec_each_succ(Goal, Call, Succ, AbsInt, AGoal, ASuccs, NCall, NSuccess1, NStatus),  
 	reduce_compl_succ(LocalStatus, NStatus ,Status),
	compose_compl_goals(LocalStatus, NStatus, Succ, NSuccess, NSuccess1, NSucc).

abs_exec_each_succ(Goal, Call, Succ, AbsInt, AGoal,  [ASucc|ASuccs], NCall, NSucc, Status):-
	abs_execute_exp(Goal, Succ, AbsInt, AGoal, ASucc, NSuccess),
	reduce_success(NCall, NSuccess, LocalStatus0),
	reduce_compl_fin(LocalStatus, LocalStatus0),
	abs_exec_each_succ(Goal, Call, Succ, AbsInt, AGoal, ASuccs, NCall, NSuccess1, NStatus),  
 	reduce_compl_succ(LocalStatus, NStatus ,Status),
	compose_compl_goals(LocalStatus, NStatus, Succ, NSuccess, NSuccess1, NSucc).



compose_compl_goals(S1,S2,Orig,_,_,OrigC):-
	S1 \== S2,
	( S1 \== dont_know; S2 \== nosucc ),
	( S2 \== dont_know; S1 \== nosucc ),!,
	list_to_conj(Orig,OrigC).
compose_compl_goals(_,_,Orig,A,B,Out) :-
	synt_compose_disj(Orig,A,B,Out).

%--------------------------------------------------------------------------------



abs_exec_complete_call(_Goal, _Call, _AbsInt, [], nosucc, nosucc).
abs_exec_complete_call(Goal, Call, AbsInt, 
	            [complete(AGoal, ACall, _ASuccs,Key,Id)|Cmpls], NCalls, Status):-
	abs_execute_exp(Goal, Call,  AbsInt, AGoal, ACall, NCall),
	reduce_calls(NCall,SingleStatus0),
	assertz_fact(ctchecks_log(AbsInt,SingleStatus0,calls,Key,Id)),
	reduce_compl_fin(SingleStatus,SingleStatus0),
	abs_exec_complete_call(Goal, Call, AbsInt, Cmpls, NCalls0, Status0),
	synt_compose_disj(Call, NCall,NCalls0,NCalls), 
	reduce_compl(SingleStatus, Status0, Status).



%--------------------------------------------------------------------------------


abs_exec_one_assertion( _AbsInt, [ ] , A , _Key , A , check ).

abs_exec_one_assertion( AbsInt, Cmpls, A , _Key , NA , Status ) :-
	A = as${
		 status    => check ,     type      => success ,
	         head      => Goal  ,     compat    => Co      ,
		 call      => Call  ,     succ      => Success ,
		 orig_call => OrigCall,   orig_succ => OrigSuccess ,
		 comp      => Comp  ,     dic       => Dict    , 
		 locator   => Loc   ,     comment   => Comm    ,
		 fromwhere => From 
	       },
	!,
	abs_exec_complete_cs(Goal, Call, Success, AbsInt, Cmpls, NCall, NSuccess, Status0),
	reduce_compl_fin(Status0,Status),
	(
	  Status == check
	->
	  list_to_conj( OCall    , NCall    ),
	  list_to_conj( OSuccess , NSuccess )
	;
	  OCall    = Call ,
	  OSuccess = Success
	),
	NA =as${
		 status    => Status,     type      => success  ,
	         head      => Goal  ,     compat    => Co       ,
		 call      => OCall ,     succ      => OSuccess ,
		 orig_call => OrigCall  , orig_succ => OrigSuccess ,
		 comp      => Comp  ,     dic       => Dict     , 
		 locator   => Loc   ,     comment   => Comm     ,
		 fromwhere => From 
	       }.

abs_exec_one_assertion( AbsInt, Cmpls, A , _Key , NA , Status ) :-
	A = as${
		 status    => check ,     type      => calls   ,
	         head      => Goal  ,     compat    => Co      ,
		 call      => Call  ,     succ      => Success ,
		 orig_call => OrigCall  , orig_succ => OrigSuccess ,
		 comp      => Comp  ,     dic       => Dict    , 
		 locator   => Loc   ,     comment   => Comm    ,
		 fromwhere => From 
	       },
	!,
	abs_exec_complete_call(Goal, Call, AbsInt, Cmpls, NCall, Status0),
	reduce_compl_fin(Status0, Status),
	(
	  Status == check
	->
	  list_to_conj( NNCall    , NCall    )
	;
	  NNCall = Call
	),
	NA =as${
		 status    => Status,     type      => calls   ,
	         head      => Goal  ,     compat    => Co      ,
		 call      => NNCall,     succ      => Success ,
		 orig_call => OrigCall  , orig_succ => OrigSuccess ,
	         comp      => Comp  ,     dic       => Dict    , 
		 locator   => Loc   ,     comment   => Comm    ,
		 fromwhere => From 
	       }.


abs_exec_one_assertion( AbsInt, Cmpls, A , _Key , NA , Status ) :-
	A = as${
		 status    => check ,     type      => comp    ,
	         head      => Goal  ,     compat    => Co      ,
		 call      => Call  ,     succ      => Success ,
		 orig_call => OrigCall  , orig_succ => OrigSuccess ,
		 comp      => Comp  ,     dic       => Dict    , 
		 locator   => Loc   ,     comment   => Comm    ,
		 fromwhere => From 
	       },
	!,
	abs_exec_complete_call(Goal, Call, AbsInt, Cmpls, NCall, _Status0),
	Status = check,
	list_to_conj( NNCall    , NCall    ),
	NA =as${
		 status    => Status, type          => comp    ,
	         head      => Goal  , compat        => Co      ,
		 call      => NNCall , succ         => Success ,
		 orig_call => OrigCall  , orig_succ => OrigSuccess ,
		 comp      => Comp  , dic           => Dict    , 
		 locator   => Loc   , comment       => Comm    ,
		 fromwhere => From 
	       }.



abs_exec_comp_assertion( A , _Key , AComp, NA , Status ) :-
	A = as${
		 status    => check , type          => comp    ,
	         head      => Goal  , compat        => Co      ,
		 call      => Call  , succ          => Success ,
		 orig_call => OrigCall  , orig_succ => OrigSuccess ,
		 comp      => Comp  , dic           => Dict    , 
		 locator   => Loc   , comment       => Comm    ,
		 fromwhere => From 
	       },
%	format("Assertion:~n~p",[A]),nl,nl,
%	displayq([Call,OrigCall,Success,OrigSuccess]),nl,
	abs_execute_comp(Goal, Comp,  AComp, NComp, Status0),
	list_to_conj(Call,CCall),
	reduce_comp(CCall,Status0,Status),
%	list_to_conj(NNComp,NComp),
	NA =as${
		 status    => Status,   type      => comp    ,
	         head      => Goal  ,   compat    => Co      ,
		 call      => Call  ,   succ      => Success ,
		 orig_call => OrigCall, orig_succ => OrigSuccess ,
		 comp      => NComp ,   dic       => Dict    , 
		 locator   => Loc   ,   comment   => Comm    ,
		 fromwhere => From 
	       }.

abs_exec_comp_assertion(A,_Key,[],A,check).



abs_exec_size_assertion( A , _Key , ASize, NA , Status ) :-
	A = as${
		 status    => check , type          => success   ,
	         head      => Goal  , compat        => Co      ,
		 call      => Call  , succ          => Success ,
		 orig_call => OrigCall  , orig_succ => OrigSuccess ,
		 comp      => Comp  , dic           => Dict    , 
		 locator   => Loc   , comment       => Comm    ,
		 fromwhere => From 
	       },
	abs_execute_sizes(Goal, Success,  ASize, NSuccess, Status0),
	list_to_conj(Call,CCall),
	reduce_comp(CCall,Status0,Status),
%	list_to_conj(NNComp,NComp),
	NA =as${
		 status    => Status,   type      => success    ,
	         head      => Goal  ,   compat    => Co      ,
		 call      => Call  ,   succ      => NSuccess ,
		 orig_call => OrigCall, orig_succ => OrigSuccess ,
		 comp      => Comp  ,   dic       => Dict    , 
		 locator   => Loc   ,   comment   => Comm    ,
		 fromwhere => From 
	       }.

abs_exec_size_assertion(A,_Key,[],A,check).


abs_execute_exp( Goal , Exp , AbsInt , AGoal , Info_u , NewExp ):-
	varset( Goal , Vars ),
%	display( abs_execute( Goal,Exp,AbsInt,AGoal , Info_u , Vars ) ),
	abs_execute( AbsInt , Goal , Exp , AGoal , Vars , Info_u , NewExp ).


synt_compose_disj(_, Exp1,Exp2,NewExp):-
	Exp1 == Exp2,!,
	NewExp = Exp1.
synt_compose_disj(_,nosucc,A,A) :-!.
synt_compose_disj(_,A,nosucc,A) :-!.
synt_compose_disj(Orig, _NewExp1, _NewExp2, Orig).

new_status(Flag,_,_):-
	Flag \== check,
	!.


reduce_calls(  true  , checked ) :- !.  % -
reduce_calls(  fail  , false   ) :- !.  % +
reduce_calls( _Calls , check   ).

reduce_success( fail , _Success , check   ) :- !. % -
reduce_success( _Call, true     , checked ) :- !. % +
reduce_success( _    , fail     , false   ) :- !. % -
reduce_success( _    , _        , check   ).

% PP: Commented out for a while...
% reduce_comp( fail  , _Comp , check   ) :- !.  % ??? was 'checked' in the old version
reduce_comp( _Call , true  , checked ) :- !.
reduce_comp( _     , fail  , false   ) :- !.
reduce_comp( _Call , Comp  , Comp    ).


reduce_compl(true, true, true) :- !.
reduce_compl(fail , fail, fail) :- !.  
reduce_compl(nosucc, S, S) :- !. 
reduce_compl(S, nosucc, S) :- !. 
reduce_compl(_, _, dont_know).

reduce_compl_succ(true, true, true) :- !. 
reduce_compl_succ(fail, fail, fail) :- !. 
reduce_compl_succ(nosucc, S, S) :- !. 
reduce_compl_succ(S, nosucc, S) :- !. 
reduce_compl_succ(_, _, dont_know).


reduce_compl_fin(perfect, true) :-!.
reduce_compl_fin(true, checked) :-!.
reduce_compl_fin(fail, false) :-!.
reduce_compl_fin(nosucc, checked) :-!.
reduce_compl_fin(dont_know, check).
