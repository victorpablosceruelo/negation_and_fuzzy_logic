:- module(entry_policy,[ get_entry_info/3 ],[assertions, isomodes]).

:- doc(module,"This module provides the entry policy algorithms
	for modular analysis. The entry policy is determined by the
	@code{entry_policy} preprocessing flag.").

:- use_module(program(p_abs), 
	[ read_registry_file/3,
	  registry/3,
	  registry_headers/2,
	  add_to_imdg_list/5,
	  add_changed_module/4,
	  get_module_from_sg/2,
	  may_be_improved_mark/2,
	  not_valid_mark/2
	]).
:- use_module(program(itf_db), [curr_file/2, current_itf/3]).
:- use_module(plai(domains), [identical_proj/5, abs_sort/3]).
:- use_module(library(filenames), [basename/2]).
:- use_module(ciaopp(preprocess_flags), [current_pp_flag/2]).
:- use_module(program(p_unit), 	[entry_assertion/3, type_of_goal/2, type_of_directive/2]).
:- use_module(plai(domains), [unknown_entry/3, unknown_call/4, info_to_asub/5, empty_entry/3]).
:- use_module(library(terms_vars), [varset/2]).
:- use_module(library(counters), [setcounter/2, inccounter/2]).
:- use_module(library(vndict), [vars_names_dict/3]).
:- use_module(plai(transform), [transform_clauses/5]).
:- use_module(plai(intermod), [top_level_module/2]).
:- use_module(spec(s_simpspec), [make_atom/2]).

%%------------------------------------------------------------------

:- pred get_entry_info(+AbsInt,-Sg,-Proj) 

# "Provides on backtracking entry abstract substitutions for the
  current module in the global level of intermodular analysis. In the
  case of manual scheduling, this predicate should be called for every
  module in the program unit. In the case of automatic scheduling,
  this predicate should only be called with top-level(U).".

get_entry_info(AbsInt,Sg,Proj):-
	current_pp_flag(entry_policy,Policy),
	call_pattern(Policy,AbsInt,Sg,Proj).

%% ********************************************************************
%% 
%% ********************************************************************

:- pred call_pattern(+Policy,+AbsInt,-Sg,-Call)

# "Provides on backtracking the call patterns that must be analyzed
  for the current module in an intermodular analysis context.
  Therefore, it does not provide user-defined entry points, which must
  be obtained using non-intermodular mechanisms.".

:- doc(bug,"when using 'force' policy there should be a smarter
	way to know if user entries should be included or not (now
	they are not included unless there is no .reg file or the
	module is the top-level one.)").

%% Reads ALL entries in registry (even if they are unmarked), but only generates user entries if
%% there is no registry file.
call_pattern(force,AbsInt,Sg,Call_s):-
	curr_file(File,CurrModule),
	basename(File,Base),
	p_abs:read_registry_file(CurrModule,Base,quiet), % It is not reread if it is already in memory.
%  	( 
%  	    current_fact(p_abs:registry(_,CurrModule,_)) ->  %% Checks if there exist registry entries.
%  	    %% does not add user entries unless it is the top-level module
 	    add_entries_if_needed(top_level,CurrModule,AbsInt),
%  	;
%  	    %% adds user entries, because we do not know anything about how this module is called.
%  	    add_entries_if_needed(force,CurrModule,AbsInt) 
%  	),
	current_fact(p_abs:registry(_,CurrModule,registry(AbsInt,Sg,Call,_Succ,_Spec,_Imdg,_Mark))),
	abs_sort(AbsInt,Call,Call_s).
call_pattern(force_assrt,AbsInt,Sg,Call_s):-
	curr_file(File,CurrModule),
	basename(File,Base),
	p_abs:read_registry_file(CurrModule,Base,quiet), % It is not reread if it is already in memory.
	add_entries_if_needed(top_level,CurrModule,AbsInt),
	current_fact(p_abs:registry(_,CurrModule,registry(AbsInt,Sg,Call,_Succ,_Spec,Imdg,_Mark))),
%% Filtering entries from assertions, exported predicate list, and initialization/on_abort.
	(member('$query',Imdg) -> true),  %% Executed only once.
	abs_sort(AbsInt,Call,Call_s).
call_pattern(Policy,AbsInt,Sg,Call_s):-
	(Policy = all ; Policy = top_level),
	current_pp_flag(success_policy,SP),
	read_registry_curr_modules(AbsInt),
	add_entries_if_needed(Policy,_AllModules,AbsInt),
	!,
	curr_file(_,CurrModule),
	current_fact(p_abs:registry(_,CurrModule,registry(AbsInt,Sg,Call,_Succ,_Spec,_Imdg,Mark))),
	(may_be_improved_mark(SP,Mark) ; not_valid_mark(SP,Mark)),
	abs_sort(AbsInt,Call,Call_s).

%% --------------------------------------------------------------------

read_registry_curr_modules(_AbsInt):-
	curr_file(File,CurrModule),
	basename(File,Base),
	p_abs:read_registry_file(CurrModule,Base,quiet), % It is not reread if it is already in memory.
	fail.
read_registry_curr_modules(_AbsInt).

%% --------------------------------------------------------------------

:- pred add_entries_if_needed(+Policy,+Module,+AbsInt)

# "Adds the entries written in the source code of @var{CurrModule} for
  the domains being used, if they have not been added before.".

add_entries_if_needed(Policy,Module,AbsInt):-
	entry_assertions_to_registry(Policy,Module,AbsInt),
	update_registry_headers(Policy,Module,AbsInt).

%% --------------------------------------------------------------------

update_registry_headers(Policy,CurrModule,AbsInt):-
	curr_file(_,CurrModule), %% CurrModule may be a free variable (then all current modules must be updated).
	( top_level_module(CurrModule,_), Policy = top_level
	; Policy = all 
	; Policy = force),
	current_fact(registry_headers(CurrModule,entries_already_analyzed(Domains)),Ref),
	\+ member(AbsInt,Domains),
	erase(Ref),
	assertz_fact(registry_headers(CurrModule,entries_already_analyzed([AbsInt|Domains]))),
	fail.
update_registry_headers(_Policy,_CurrModule,_AbsInt).

%% --------------------------------------------------------------------

:- pred entry_assertions_to_registry(+Policy,+Module,+AbsInt)

# "Adds the entries written in the source code of the modules loaded
  for the domains being used, if they have not been added before.".

entry_assertions_to_registry(Policy,Module,AbsInt):-
	current_pp_flag(success_policy,SP),
	entry_point(Policy,AbsInt,Goal,_Qv,Call,Prime,Module),
	functor(Goal,F,A),
	functor(CGoal,F,A),  % direct access to predicate.
	make_atom([F,A],SgKey),
	( current_fact(registry(SgKey,Module,registry(AbsInt,CGoal,CCall,CPrime,CSpec,ImdgList,CMark)),Ref),
	  abs_sort(AbsInt,Call,Call_s),
	  abs_sort(AbsInt,CCall,CCall_s),
	  identical_proj(AbsInt,Goal,Call_s,CGoal,CCall_s) ->
	  add_to_imdg_list(AbsInt,'$query',ImdgList,NewImdg,Changed),
	  (
	      Changed = y ->
	      erase(Ref),
	      assertz_fact(registry(SgKey,Module,registry(AbsInt,CGoal,CCall,CPrime,CSpec,NewImdg,CMark)))
	  ;
	      true
	  )
	; may_be_improved_mark(SP,Mark),
	  assertz_fact(registry(SgKey,Module,registry(AbsInt,Goal,Call,Prime,_Spec1,['$query'],Mark)))
	),
	curr_file(File,Module),
	basename(File,Base),
	add_changed_module(Module,Base,Module,current),
	fail.
entry_assertions_to_registry(_Policy,_Module,_AbsInt).

%% --------------------------------------------------------------------

:- pred entry_point(+Policy,+AbsInt,-Goal,Qv,-Call,-Prime,Module) 

# "Modified version of the same predicate in plai.pl. @var{Policy}
  determines which entry points are added for analysis.".

entry_point(Policy,AbsInt,Goal,Qv,Call,Prime,Module):-
	( 
	    type_of_goal(exported,Goal), 
	    top_level_module(TopLevelModule,_),
	    get_module_from_sg(Goal,Module),
	    ( TopLevelModule = Module, Policy = top_level
	    ; Policy = all 
	    ; Policy = force )  %% If all of this succeed, all exported predicates must be analyzed.
	; 
	    current_itf(multifile,Goal,Module),
	    type_of_goal(multifile,Goal)        %% multifiles must be analyzed in any case.
	),  
	current_fact(registry_headers(Module,entries_already_analyzed(Domains))),
	\+ member(AbsInt,Domains),
%%
	functor(Goal,F,A),
	functor(G,F,A),
	\+ entry_assertion(G,_Call,_Name),
	varset(Goal,Qv),
	unknown_entry(AbsInt,Qv,Call),
	unknown_call(AbsInt,Qv,Call,Prime).
entry_point(_Policy,AbsInt,Name,[],Call,Prime,_Module):- %% init and on_abort must be analyzed always.
	setcounter(0,0),
	( type_of_directive(initialization,Body)
	; type_of_directive(on_abort,Body) ),
	inccounter(0,Name), % Name of directive a number
	varset(Body,Bv),
	vars_names_dict(Ds,Bv,_Ns),
	transform_clauses([(clause(Name,Body),Name)],Ds,[nr],[],AbsInt),
	empty_entry(AbsInt,[],Call),
	unknown_call(AbsInt,[],Call,Prime).
entry_point(_Policy,AbsInt,Goal,Qv,Call,Prime,Module):- %% entries must be analyzed always (if dynamic preds!)
	entry_assertion(Goal,CInfo,_Name),
	get_module_from_sg(Goal,Module),
	varset(Goal,Qv),
	info_to_asub(AbsInt,_approx,CInfo,Qv,Call),
	unknown_call(AbsInt,Qv,Call,Prime).
