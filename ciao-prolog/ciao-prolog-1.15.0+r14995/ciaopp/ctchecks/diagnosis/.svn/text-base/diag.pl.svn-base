:-module(diag,[how/6],[assertions,regtypes]).

:- doc(title,"Diagnoser (very alpha)").

:- doc(module,"This is diagnoser module.").

:- doc(bug,"Integration with assertion checking must be done.").
:- doc(bug,"Integreation with emacs mode would be desired too").
:- doc(bug,"Does not work with eterms -> needs further investigation.").
:- doc(bug,"Perhaps should be domain independent.").


% local stuff
:- use_module('./diag_supp').

% ciaopp stuff
:- use_module(plai(plai_db), [complete/7, memo_table/6]).
:- use_module(program(clause_db), [source_clause/3, clause_locator/2]).
:- use_module(plai(domains), [project/5, 	
	                      call_to_entry/9, 
			      exit_to_prime/8,
			      unknown_entry/3,
			      identical_abstract/3,
			      glb/4,
			      info_to_asub/5,
			      extend/5,
			      less_or_equal/3]).
:- use_module(program(p_unit_basic), [type_of_goal/2]).
:- use_module(program(clidlist), [atom2data/5]).

% ciao libs
:- use_module(library(idlists), [member_0/2]).
:- use_module(library(terms_vars), [varset/2]).
:- use_module(library(messages)).
:- use_module(library(sets), [ord_subtract/3]).
:- use_module(library(iso_misc), [once/1]).

/*
how0(Abs,SgKey,Lit,AssProps,AllWhere) :-
	statistics(runtime,_),
	inform_user(['{Diagnosing']),
	findall(Where,how(Abs,SgKey,calls,Lit,AssProps,Where),AllWhere),
	statistics(runtime,[_,CTime]),
	inform_user(['{completed in ',CTime,' msec.}']),
	inform_user(['}']),
	displayq(AllWhere).
*/

:- regtype bug_location/1.
bug_location(entry(_,_)).
bug_location(exit(_,_)).

:- regtype mode_t/1.
mode_t(call).
mode_t(succ).

:- pred how(Abs,Key,Mode,Lit,Props,Bug) : 
     (atom(Abs),atom(Key),mode_t(MOde),callable(Lit),list(Props,callable)) => 
      bug_location(Bug) # 
     "Locates a bug @var{Bug} related to domain @var{Abs}, a program point 
      @var{Key}, with expected properties @var{Props}  (call or success - determined by @var{Mode}). 
      @var{Lit} is a literal pointed by @var{Key} and used to link variables from @var{Props} and those 
      from the clause. Must be preceded by analysis.".

%% Sample usage:
%% ?- how(ptypes,'qsort5:qsort/2/1/4',qsort(A,B),
%%        ['basic_props:list'(B,'basic_props:num')],W).
%% Note: does not work with eterms.

how(Abs,SgKey,Mode,Lit,AssProps,Where) :-
	varset(AssProps,Vars),
	get_clause_id(SgKey,KeyCl),
	source_clause(KeyCl,clause(Head,Body),Dic),
	( Mode = succ ->
	  get_next_lit(SgKey,SgKeyM)
	; SgKeyM = SgKey
	),
	memo_table(SgKeyM,Abs,ID,_Child,AllVars0,[Info]),
	Dic = dic(AllVars0,_Names),
	get_init_lit(Body,Head,Lit0,SgKey),
	get_init_vars(Lit0,Lit,Vars,Vars0),
	rename_props(AssProps,Vars0,AssProps1),
	project(Abs,Vars0,_,Info,InfoV),
	info_to_asub(Abs,_,AssProps1,Vars0,Props),
	unknown_entry(Abs,Vars0,EmptyV),
	( identical_abstract(Abs,InfoV,EmptyV) ->
	  warning_message("Variables ~w have value top. Diagnosis aborted.",[Vars0]),
	  fail
 	; how_body(Abs,SgKeyM,ID,AllVars0,Vars0,trace(InfoV,Props,Vars0,[]),
	                                           [],[],[],Where),
	  diag_message(Where)
	).


rename_props([],[],[]).
rename_props([P|Ps],[V|Vs],[RP|RPs]) :-
	P =.. [F,_|Args],
	RP =.. [F,V|Args],
	rename_props(Ps,Vs,RPs).

get_init_vars(Lit0,Lit,Vars,Vars0) :-
	varset(Lit0,LVs0),
	varset(Lit,LVs),
	filter_vars(LVs,LVs0,Vars,Vars0).

filter_vars([],[],_,[]).
filter_vars([LV|LVs],[LV0|LVs0],Vs, Vars0) :-
	( member_0(LV,Vs) ->
	  Vars0 = [LV0|Vs0]
	; Vars0 = Vs0
	),
	filter_vars(LVs,LVs0,Vs, Vs0).
 
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% how_body/10
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
how_body(Abs,SgKey,ID,Vars,SVar,Trace,VDown,VUp,S,Where) :-
	get_prev_lit(SgKey,PrevKey),
	get_clause_id(PrevKey,KeyCl),
	source_clause(KeyCl,clause(_Head,Body),_),
	find_lit(Body,Goal:PrevKey),
	type_of_goal(imported,Goal),
	!,
%	display('builtin/imported? '), displayq(PrevKey),nl,
	how_body(Abs,PrevKey,ID,Vars,SVar,Trace,VDown,VUp,S,Where).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% MOVE LEFT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
how_body(Abs,SgKey,ID,Vars,SVar,Trace,VDown,VUp,S,Where) :-
	memo_table(SgKey,Abs,ID,_Child,Vars,[Info]),
	get_prev_lit(SgKey,PrevKey),
	memo_table(PrevKey,Abs,ID,Child,Vars,[PrevInfo]),
	!,
	project(Abs,SVar,_,Info,_InfoV),
	project(Abs,SVar,_,PrevInfo,_PrevInfoV),
	get_clause_id(PrevKey,KeyCl),
	source_clause(KeyCl,clause(_Head,Body),dic(Vars,_)),
	find_lit(Body,Goal:PrevKey),
	 how_lit(Abs,PrevKey,Goal,Vars,PrevInfo,Child,Trace,VDown,VUp,[ID|S],Where).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% HEAD OF CLAUSE 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
how_body(Abs,SgKey,ID,Vars,_SVs,trace(InitInfo,InitProp,InitVar,Ss),
          VDown,VUp,S,Where0) :-
	get_clause_id(SgKey,ClKey),
	memo_table(ClKey,Abs,ID,_,_,[_ClInfo]),
	source_clause(ClKey,clause(Head,_Body),dic(Vars,_)),
	pop_ID(S,PId,NewS),
	complete(_PreKey,Abs,EGoal,_Entry0,_,ID,Parents),
	member((PKey,PId),Parents),
	\+ member((PKey,PId),VUp),  % the abstract graph might have cycles
%	!,
	( PId == 0 ->  % Entry point
	  Trace = trace(InitInfo,InitProp,InitVar,[entry(EGoal,Head,Vars)|Ss]),
	  fail,
	  Where0 = entry(PKey,ID)
	; get_clause_id(PKey,KeyClause),
	  source_clause(KeyClause,clause(_,Body),dic(PVars,_VNames)),
	  find_lit(Body,Goal:PKey),
	  memo_table(PKey,Abs,PId,_Son,PVars,[GoalClInfo]),
	  varset(Goal,Gv),
	  project(Abs,Gv,_,GoalClInfo,GoalInfo),
	  Trace = trace(InitInfo,InitProp,InitVar,[entry(Goal,Head,Vars)|Ss]),
	  \+ check_trace_deadend(Abs,GoalInfo,Trace),  
	  ( check_trace(Abs,GoalClInfo,Trace,_PSVars)  
              -> Where0 = entry(PKey,ClKey)
	  ;   %display(leaving_clause(ClKey,ID,PKey)),nl,
               how_body(Abs,PKey,PId,PVars, PVars,%PSVars,
               Trace,VDown,[(PKey,PId)|VUp],NewS,Where0)
	  )
	).

%how_body(_Abs,SgKey,ID,_,_,_,_VDown,_VUp,_S,_Where0) :-
%	display('Failed: '),
%	displayq(how_body(SgKey,ID)),
%        nl,
%	fail.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% how_lit/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%how_lit(_,SgKey,_,_,_,no,_,_,_,_,builtin(SgKey)) :- !.

how_lit(Abs,SgKey,Goal,UpVars,Call,Node,trace(IInfo,IProp,IVar,Ss),VDown,VUp,S,Where) :-
	\+ member((SgKey,Node),VDown),
	memo_table(ClKey,Abs,Node,_Child,ClVars,[ClInfo]),
	is_clause_id(ClKey),
	source_clause(ClKey,clause(Head,_Body),dic(ClVars,_VNames)),
	varset(Goal,Gv),
	varset(Head,Hv),
	project(Abs,Hv,_,ClInfo,HInfo),

	unknown_entry(Abs,Gv,EmptyProj),
	project(Abs,Gv,_,Call,CallG),
	call_to_entry(Abs,Gv,Goal,Hv,Head,[],CallG,Entry,_),
	once(exit_to_prime(Abs,Goal,Hv,Head,Gv,Entry,(no,EmptyProj),_Prime)),
	NTrace = trace(IInfo,IProp,IVar,[exit(Head,Goal,ClVars,UpVars)|Ss]),
	\+ check_trace_deadend(Abs,HInfo,NTrace),
	( check_trace(Abs,HInfo,NTrace,_NewSVars) -> 
	  Where = exit(SgKey,ClKey)
	;   
	    how_body(Abs,ClKey,Node,ClVars,ClVars,%NewSVars1,
                     NTrace,
	             [(SgKey,Node)|VDown],VUp,S,Where)

        ).
%how_lit(_,SgKey,Goal,_,_,Node,_,_VDown,_VUp,_,_) :-
%	displayq('Failed: '),
%	displayq(how_lit(SgKey,Goal,Node)),nl,fail.

pop_ID([],_,[]).
pop_ID([ID|IDs],ID,IDs).

/*
get_call_info(Abs,ClKey,Node,Hv,ClVars,HCall) :-
	get_prev_lit(ClKey,PrKey),
	memo_table(PrKey,Abs,Node,_Child,ClVars,[ClInfo]),!,
	project(Abs,Hv,_,ClInfo,HCall).
get_call_info(Abs,_,_,Hv,_,HCall):-
	unknown_entry(Abs,Hv,HCall).
*/

% Fails if the initial property cannot be proven

% for entry point
% check_trace_e(Abs,Info,trace(VInfo,Prop,Var,Ss),bingo) :-
% 	follow_trace(Abs,Info,Ss,Var,VInfo,Prop,[],normal).

check_trace(Abs,Info,trace(VInfo,Prop,Var,[S|Ss]),bingo) :-
	reduce0(Abs,S,Info,Info1,InStack),
	follow_trace(Abs,Info1,Ss,Var,VInfo,Prop,InStack,normal),!.
%	displayq([S|Ss]),nl.

check_trace_deadend(Abs,Info,trace(VInfo,Prop,Var,[S|Ss])) :-
	reduce0_dead(Abs,S,Info,Info1,InStack),
	follow_trace(Abs,Info1,Ss,Var,VInfo,Prop,InStack,deadend).



reduce0_dead(Abs,entry(Goal,Head,Vars),InfoG,InfoOut,[InfoCl]) :-
	varset(Goal,Gv),
	varset(Head,Hv),
	ord_subtract(Vars,Hv,Fv),	
	call_to_entry(Abs,Gv,Goal,Hv,Head,Fv,InfoG,InfoOut,_),
	unknown_entry(Abs,Vars,EmptyCl),
	extend(Abs,InfoOut,Hv,EmptyCl,InfoCl).
reduce0_dead(Abs,exit(Head,Goal,_HCVs,GCVs),InfoH,InfoOut,[InfoCl]) :-
	varset(Head,Hv),
	varset(Goal,Gv),
	unknown_entry(Abs,Gv,EmptyInfoG),
	unknown_entry(Abs,GCVs,EmptyCl),
 	once(exit_to_prime(Abs,Goal,Hv,Head,Gv,InfoH,(no,EmptyInfoG),InfoOut)),
	extend(Abs,InfoOut,Gv,EmptyCl,InfoCl).


reduce0(Abs,entry(Goal,Head,Vars),_InfoG,InfoOut,[InfoCl]) :-
	varset(Goal,Gv),
	unknown_entry(Abs,Gv,EmptyInfo),
	varset(Head,Hv),
	ord_subtract(Vars,Hv,Fv),	
	call_to_entry(Abs,Gv,Goal,Hv,Head,Fv,EmptyInfo,InfoOut,_),
	unknown_entry(Abs,Vars,EmptyCl),
	extend(Abs,InfoOut,Hv,EmptyCl,InfoCl).
reduce0(Abs,exit(Head,Goal,_HCVs,GCVs),_InfoH,InfoOut,[InfoCl]) :-
	varset(Head,Hv),
	varset(Goal,Gv),
	unknown_entry(Abs,Hv,EmptyInfo),
	unknown_entry(Abs,Gv,EmptyInfoG),
	unknown_entry(Abs,GCVs,EmptyCl),
 	once(exit_to_prime(Abs,Goal,Hv,Head,Gv,EmptyInfo,(no,EmptyInfoG),InfoOut)),
	extend(Abs,InfoOut,Gv,EmptyCl,InfoCl).


follow_trace(Abs,InfoIn,[],Var,_VInfo,Prop,_S,Mode):-
	InfoIn \== '$bottom',
	project(Abs,Var,_,InfoIn,InfoPr),
	( Mode = normal ->
	  glb(Abs,InfoPr,Prop,GLB),
	  GLB == '$bottom'
%	  display('FOUND BUG!!!'), nl,nl
	; less_or_equal(Abs,InfoPr,Prop) % succeeds if ass property is checked
	).

follow_trace(Abs,InfoIn,[Step|Steps],Var,VInfo,Prop,St0,Mode) :-
	reduce(Abs,Step,InfoIn,InfoOut,St0,St1),
	follow_trace(Abs,InfoOut,Steps,Var,VInfo,Prop,St1,Mode).



reduce(Abs,entry(Goal,Head,Vars),InfoG,InfoOut,S,[InfoCl|S]) :-	
	varset(Goal,Gv),
	varset(Head,Hv),
	range(InfoG,IGv),
	unknown_entry(Abs,Gv,EmptyGoal),
	extend(Abs,InfoG,IGv,EmptyGoal,NewInfoG_x),
	project(Abs,Gv,_,NewInfoG_x,NewInfoG),
	ord_subtract(Vars,Hv,Fv),		
        call_to_entry(Abs,Gv,Goal,Hv,Head,Fv,NewInfoG,InfoOut,_),
	unknown_entry(Abs,Vars,EmptyCl),
	extend(Abs,InfoOut,Hv,EmptyCl,InfoCl),!.

reduce(Abs,exit(Head,Goal,HCVs,GCVs),InfoH,InfoOut,[InfoCl|S],S1) :-	
	varset(Goal,Gv),
	varset(Head,Hv),
	range(InfoH,IHv),
	range(InfoCl,Range),
	HCVs = Range,
	extend(Abs,InfoH,IHv,InfoCl,NewInfoCl),
	project(Abs,Hv,_,NewInfoCl,NewInfoH),
	unknown_entry(Abs,Gv,EmptyInfo),
	once(exit_to_prime(Abs,Goal,Hv,Head,Gv,NewInfoH,(no,EmptyInfo),InfoOut)),
	add_or_replace(Abs,GCVs,InfoOut,S,S1),!.

add_or_replace(Abs,Vs,I0,[],[I]):-
	unknown_entry(Abs,Vs,Empty),
	range(I0,Vs0),
	extend(Abs,I0,Vs0,Empty,I).
add_or_replace(Abs,Vs,I0,[I1|Is],[I|Is]):-
	range(I1,Vs),
	range(I0,Vs0),
	extend(Abs,I0,Vs0,I1,I).

% get_first_lit(entry(L,_,_),L).
% get_first_lit(exit(L,_,_,_),L).


%find_var([V|_],[N|_],N,[V]) :-!.
%find_var([_|Vs],[_|Ns],N,V) :-
%	find_var(Vs,Ns,N,V).


diag_message(exit(Goal,Head)) :-
	clause_locator(Head,ClLoc),
	error_message(ClLoc,"An error found in the clause head, ~nwhen exiting after call ~w.",[Goal]).
diag_message(entry(Goal,Head)) :-
	atom2data(Goal,_,_,_,P),
	get_clause_id(Goal,ClKey),
	clause_locator(ClKey,ClLoc),
	error_message(ClLoc,"An error found at literal ~w ~nwhen entering clause ~w.",[P,Head]).

