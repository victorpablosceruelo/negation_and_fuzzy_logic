:- module(intermod,
	[
	    cleanup_intermod/0,
	    top_level_module/2,
	    set_top_level/1,
	    valid_mod_analysis/1,
            set_modules_analyzed/1,					
	    modular_analyze/2,
	    modular_analyze/3,
	    get_modules_analyzed/1,
%	    manual_analyze/2,
%	    manual_analyze/3,
%	    auto_analyze/3,
	    auto_check/2,
%	    monolithic_analyze/3,
	    auto_transform/3,
	    auto_simp_libs/2,
	    auto_simp_libs/3,
           % scenario 2
	    auto_ctcheck/1,
	    auto_ctcheck/2,
	    auto_ctcheck/4,
	   % scenario 1 
            monolithic_ctcheck/1,
            monolithic_ctcheck/2,
	    monolithic_ctcheck/3,
	   % scenario 5 
	    inductive_ctcheck/2,
	    inductive_ctcheck/4,
	    inductive_ctcheck_summary/3,
           % scenario 3
	    auto_ctcheck_opt/2,
	    auto_ctcheck_opt/3,
%
	    auto_ctcheck_list/3,
	    auto_ctcheck_summary/2,
	    auto_ctcheck_summary/3,
	    cleanreg/0,
	    cleanreg/1
	],[assertions, basicmodes]).
:- use_package(spec(nomem_usage)).


:- doc(module,"This module provides intermodular analysis to
	Ciaopp.").

%%------------------------------------------------------------------

:- use_module(program(itf_db), [curr_file/2, cleanup_itf_db/0]).
:- use_module(ciaopp(preprocess_flags), [push_pp_flag/2, pop_pp_flag/1, current_pp_flag/2, set_pp_flag/2]).
:- use_module(plai(fixpo_plai),             [cleanup_fixpoint/1]).
:- use_module(plai(fixpo_dd),               [cleanup_fixpoint/1]).
:- use_module(plai(fixpo_di),               [cleanup_fixpoint/1]).
:- use_module(plai(fixpo_check_di),         [cleanup_fixpoint/1]).
:- use_module(plai(fixpo_check_di2),        [cleanup_fixpoint/1]).
:- use_module(plai(fixpo_check_di3),        [cleanup_fixpoint/1]).
:- use_module(plai(fixpo_check_di4),        [cleanup_fixpoint/1]).
:- use_module(plai(fixpo_check_reduced_di), [cleanup_fixpoint/1]).

:- use_module(infer(infer_db), [domain/1]).

:- use_module(ciaopp(driver), [module/1, module/2, analyze/1, 
	                      acheck_summary/1]).
:- use_module(program(p_unit), [program/2, replace_program/2]).
%:- use_module(program(p_asr),[cleanup_p_asr/0]).
:- use_module(plai(re_analysis), [update_ai_info_case/4]).
:- use_module(plai(plai), [mod_plai/5]).
:- use_module(program(p_abs), 
	[
	    cleanup_p_abs/0,
	    cleanup_p_abs_all/0,
	    gen_registry_info/3,
	    gen_registry_info/4,
	    save_registry_info/2,
	    save_registry_info/3,
	    change_open_mode/2,
	    update_spec_info/2,
	    get_spec_info_imported/0,
	    get_modules_to_analyze/3,
	    get_all_modules/2,
	    get_all_modules/3,
	    get_all_module_cycles/2,
	    module_is_processable/1,
	    registry_is_empty/3
	]).

:- use_module(library(sets), [insert/3]).
:- use_module(library(lists), [select/3, reverse/2, append/3]).
:- use_module(library(filenames), [no_path_file_name/2]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(spec(spec), [simplify_specialize/6]).
:- use_module(spec(codegen), [codegen/4, codegen_af/4]).
:- use_module(ciaopp(printer), [output/1, output/0]).
:- use_module(plai(plai_db), [complete/7, cleanup_plai_db/1]).
:- use_module(plai(fixpo_ops), [
	complete_prev/7, 
%	copy_completes/1,
	store_previous_analysis_completes/1,
	store_previous_analysis_aux_info/1,
	reset_previous_analysis/1, 
	restore_previous_analysis/1,
	compare_completes_with_prev/3,
	remove_useless_info/1
			      ]).
:- use_module(program(p_abs), [get_module_from_sg/2]).
:- use_module(program(aux_filenames), [get_module_filename/3, just_module_name/2]).
:- use_module(library(counters)).
:- use_module(library(sort), [sort/2]).
:- use_module(spec(unfold_times), [global_time_ellapsed/3]).
:- use_module(library(system), 
	[
	    system/1,
	    delete_file/1,
	    delete_directory/1,
	    file_property/2,
	    make_dirpath/1,
	    directory_files/2,
	    file_exists/1
	]).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(dynamic), [asserta/1, retract/1]).
:- use_module(library(compiler(c_itf)), [cleanup_itf_cache/0]).
% :- use_module(library(read), [read/1]).
%%------------------------------------------------------------------

:- multifile dump_flags_list/2.

dump_flags_list( intermod , [entry_policy,global_scheduling,process_libraries] ).

%%------------------------------------------------------------------

:- pred top_level(Base) 

# "@var{Base} is the top-level module of the program unit to be
  analyzed.".

:- data top_level/1.

%%------------------------------------------------------------------

set_top_level(TopLevel):-
	absolute_file_name(TopLevel, '_opt', '.pl', '.', _, AbsBase, _),
	set_fact(top_level(AbsBase)).


%%------------------------------------------------------------------

:- pred top_level_module(-TopLevelModule,-TopLevelBase)

# "@var{TopLevelModule} is the top-level module of the current program
  unit, and @var{TopLevelBase} is its basename.".

top_level_module(TopLevelModule,TopLevelBase):-
	current_fact(top_level(TopLevelBase)),
	just_module_name(TopLevelBase,TopLevelModule).

%%------------------------------------------------------------------

:- pred cleanup_intermod 

# "Cleans up the internal database of the intermodular analysis global
  level.".

cleanup_intermod:-
	plai_db:cleanup_plai_db(AbsInt),
	fixpo_ops:reset_previous_analysis(AbsInt),
	fixpo_plai:cleanup_fixpoint(AbsInt),
	fixpo_dd:cleanup_fixpoint(AbsInt),
	fixpo_di:cleanup_fixpoint(AbsInt),
	fixpo_check_di:cleanup_fixpoint(AbsInt),
	fixpo_check_di2:cleanup_fixpoint(AbsInt),
	fixpo_check_di3:cleanup_fixpoint(AbsInt),
	fixpo_check_di4:cleanup_fixpoint(AbsInt),
	fixpo_check_reduced_di:cleanup_fixpoint(AbsInt),
	%%
	set_fact(queue([])),
	retractall_fact(module_processed(_,_)),
	retractall_fact(top_level(_)),
	cleanup_p_abs_all,
%%jcf-25.04.2005
	cleanup_itf_db,
%	cleanup_p_asr.
	true.
/*
:- use_module(plai(fixpo_plai),             [init_fixpoint/0]).
:- use_module(plai(fixpo_dd),               [init_fixpoint/0]).
:- use_module(plai(fixpo_di),               [init_fixpoint/0]).
:- use_module(plai(fixpo_check_di),         [init_fixpoint/0]).
:- use_module(plai(fixpo_check_di2),        [init_fixpoint/0]).
:- use_module(plai(fixpo_check_di3),        [init_fixpoint/0]).
:- use_module(plai(fixpo_check_di4),        [init_fixpoint/0]).
:- use_module(plai(fixpo_check_reduced_di), [init_fixpoint/0]).

init_fixpoint(plai):- fixpo_plai:init_fixpoint.
init_fixpoint(dd):- fixpo_dd:init_fixpoint.
init_fixpoint(di):- fixpo_di:init_fixpoint.
init_fixpoint(check_di):- fixpo_check_di:init_fixpoint.
init_fixpoint(check_di2):- fixpo_check_di2:init_fixpoint.
init_fixpoint(check_di3):- fixpo_check_di3:init_fixpoint.
init_fixpoint(check_di4):- fixpo_check_di4:init_fixpoint.
init_fixpoint(check_reduc_di) :- fixpo_check_reduced_di:init_fixpoint.
*/
%% ********************************************************************
%% Intermodular analysis with manual scheduling.
%% ********************************************************************

/*
:- pred manual_analyze(+Analysis,+Module) 

# "Performs the analysis of @var{Module} in the @var{AbsInt} abstract
   domain using the @em{manual} global scheduling.".

:- set_prolog_flag(multi_arity_warnings,off).

manual_analyze(Analysis,Base):- 
	manual_analyze(Analysis,Base,_).
*/

:- pred manual_analyze(+Analysis,+FileName,+OpenMode) 

# "Performs the analysis of module @var{FileName} in the @var{AbsInt}
   abstract domain using the @em{manual} global scheduling, and sets
   the mode of the registry file of module @var{FileName} to
   @var{OpenMode}. @var{OpenMode} can take the values
   @code{read_write} (which allows updating the registry information
   of module @var{FileName} when other related modules are being
   analyzed) or @code{read_only} (the registry information of
   @var{Module} will not be changed unless it is reanalyzed using
   @code{manual_analyze/2-3}).".

manual_analyze(Analysis,FileName,OpenMode):-
	atom(Analysis),!,
	manual_analyze([Analysis],FileName,OpenMode).
manual_analyze(Analyses,FileName,OpenMode):-
	absolute_file_name(FileName, '_opt', '.pl', '.', _, Base, _),
	valid_mod_analysis(Analyses), !,
	push_pp_flag(intermod,on),
	module(Base,_LoadInfo),
	curr_file(File,_),
	inform_user(['{Analyzing with manual_analyze: ',File]),
	set_top_level(Base),
%	push_pp_flag(entry_policy,all),
	program(Cls,Ds),
	cleanup_p_abs,
	( var(OpenMode) ->
	  true
	; change_open_mode(Base,OpenMode)
	), !,
	set_fact(total_info([])),
	analyze_several_domains(Analyses,Cls,Ds),
	gen_registry_info(quiet,_,_,_),
	save_registry_info(quiet,_SaveInfo),  %% all registry files must be saved.
%	pop_pp_flag(entry_policy),
	pop_pp_flag(intermod),
	set_modules_analyzed([Base]),
	inform_user(['}']).
	
:- set_prolog_flag(multi_arity_warnings,on).
	
%%------------------------------------------------------------------

:- prop valid_mod_analysis(Domain) # "Succeeds if @var{Domain} is a
valid analisys domain for modular analysis".

:- prop valid_mod_analysis(DomainList) # "Succeeds if the domains in
@var{DomainList} are valid analisys domains for modular analysis".

valid_mod_analysis([]).
valid_mod_analysis([A|As]):-
	!,
	valid_mod_analysis(A),
	valid_mod_analysis(As).
valid_mod_analysis(Analysis):-
	aidomain(Analysis), !.
valid_mod_analysis(Analysis):-
	inform_user(['{Not a valid modular analysis: ',Analysis,'}']),
	fail.

:- multifile aidomain/1.  % This predicate is defined in domains.pl.

%%------------------------------------------------------------------

:- pred analyze_(Cost,Cls,Ds,Info).
%% Taken from driver.pl. Look at that module to get info related to
%% analyses not implemented yet (and implement them!)

analyze_(Cost,_Cls,_Ds,_Info):-
	set_pp_flag(cost_approximation,Cost), !,
	inform_user(['{Not implemented in modular framework yet: ',Cost,'}']),
	fail.
analyze_(nfg,_Cls,_Ds,nfinfo(_TimeNf,_Num_Pred,_Num_NF_Pred,_NCov)):- !,
	inform_user(['{Not implemented in modular analysis yet: ',nfg,'}']),
	fail.
analyze_(seff,_Cls,_Ds,_Info):- !,
	inform_user(['{Not implemented in modular framework yet: ',seff,'}']),
	fail.
analyze_(AbsInt,Cls,Ds,Info):-
	current_pp_flag(fixpoint,Fixp),
	% some domains may change widen and lub:
	current_pp_flag(widen,W),
	current_pp_flag(multi_success,L),
	mod_plai(Cls,Ds,Fixp,AbsInt,Info),
	set_pp_flag(multi_success,L),
	set_pp_flag(widen,W).

%% ********************************************************************
%% Intermodular analysis.
%% ********************************************************************
modular_analyze(Analysis,TopLevel):-
	modular_analyze(Analysis,TopLevel,_Info).
modular_analyze(Analysis,TopLevel,Info):-
	set_modules_analyzed([]),
	current_pp_flag(mnu_modules_to_analyze, Mods),
	current_pp_flag(ext_policy, ExtPolicy),
	current_pp_flag(module_loading, LoadPolicy),
	modular_analyze_(Mods,ExtPolicy,LoadPolicy,Analysis,TopLevel,Info).

modular_analyze_(current,assertions,_,_Analysis,_TopLevel,_Info):-
	% non-modular analysis!!
	true.
modular_analyze_(current,registry,_,Analysis,TopLevel,Info):-
	manual_analyze(Analysis,TopLevel,Info).
modular_analyze_(all,registry,one,Analysis,TopLevel,Info):-
	auto_analyze(Analysis,TopLevel,Info).
modular_analyze_(all,registry,all,Analysis,TopLevel,Info):-
	monolithic_analyze(Analysis,TopLevel,Info).
modular_analyze_(all,registry,threshold,_Analysis,_TopLevel,_Info):-
	inform_user(['threshold loading policy not implemented yet.']).
modular_analyze_(all,registry,threshold_scc,_Analysis,_TopLevel,_Info):-
	inform_user(['threshold_scc loading policy not implemented yet.']).
modular_analyze_(all,assertions,_LoadPolicy,_Analysis,_TopLevel,_Info):-
	inform_user(['analyzing all modules without using registry seems to make no sense.']).

%% --------------------------------------------------------------------

:- pred get_modules_analyzed(ModList) => list(ModList) 

# "Returns the list of modules analyzed the last time a modular
  analysis was executed.".

get_modules_analyzed(ModList):-
	current_fact(modules_analyzed(ModList)).

:- pred set_modules_analyzed(ModList) : list(ModList) 

# "Sets the list of modules which have been analyzed.".

set_modules_analyzed(ModList0):-
	get_module_names_only(ModList0,ModList),!,
	set_fact(modules_analyzed(ModList)).
set_modules_analyzed(ModList):-
	set_fact(modules_analyzed(ModList)).

get_module_names_only([],[]).
get_module_names_only([(M,_,_)|ModList0],[M|ModList]):-
	get_module_names_only(ModList0,ModList).


:- data modules_analyzed/1.

modules_analyzed([]).


%% ********************************************************************
%% Intermodular analysis with automatic scheduling.
%% ********************************************************************

:- pred auto_analyze(+AbsInt, +TopLevel, -Info) 

# "Performs the analysis of the program unit for which @var{Module} is
   the top-level module in the @var{AbsInt} abstract domain using an
   @em{automatic} global scheduling. The global scheduling to be
   used is determined by the 'global_scheduling' preprocessing flag.".

auto_analyze(Analysis,TopLevel,Info):-
	atom(Analysis),!,
	auto_analyze([Analysis],TopLevel,Info).
auto_analyze(Analyses,TopLevel,Info):-
	statistics(runtime,[T1,_]),  %% total ellapsed time.
	valid_mod_analysis(Analyses), !,
	inform_user(['{Analyzing with auto_analyze: ',TopLevel]),
	reset_mem_usage,
	push_prolog_flag(gc,on),
	set_top_level(TopLevel),
	push_pp_flag(intermod,auto),
%%jcf-20.10.2005%	push_pp_flag(entry_policy,top_level),  %%Must be done before calling to auto_analyze
	%% 
%%% retractall_fact(module_times(_)),
	statistics(runtime,[_,_]),   %% setup time.
	get_modules_to_analyze(Analyses,TopLevel,ModList),
	current_pp_flag(global_scheduling,Scheduling),
	setup_scheduling(Scheduling,Analyses,TopLevel,ModList),
	statistics(runtime,[_,SetupTime]),  %% setup time.
	auto_analyze_(Scheduling,Analyses,AnInfo),
%%% total_module_times,
	save_registry_info(quiet,[time(SaveTime,_)]),
	statistics(runtime,[T2,_]),  %% total ellapsed time.
	set_fact(total_info(AnInfo)),
	add_to_total_info([time(SaveTime,[(savereg,SaveTime)]),time(SetupTime,[(setup,SetupTime)])]),
	global_time_ellapsed(T2,T1,Ellapsed),
	add_to_total_info([time(Ellapsed,[(total_ellapsed,Ellapsed)])]),
	pop_prolog_flag(gc),
	ask_mem_usage(TotalMem,DetMem),
	(
	    nonvar(TotalMem) ->
	    add_to_total_info([memory(TotalMem,DetMem)])
	;
	    true
	),
	get_total_info(Info0),
	add_iterations_info(Info0,Info),
%%jcf-20.10.2005%	pop_pp_flag(entry_policy),
	set_modules_analyzed(ModList),
	pop_pp_flag(intermod),
	inform_user(['}']),
	!.

%%%:- data module_times/1.
%%%:- use_module(library(numlists)).
:- use_module(library(prolog_sys), [statistics/2]).
%%%total_module_times:-
%%% findall(X,module_times(X),L),
%%% sum_list(L,N),
%%% display(module_times(N)),nl.

%%------------------------------------------------------------------

:- pred setup_scheduling(SchedPolicy,Domains,TopLevel,ModList) 

# "This predicate sets up some stuff in specific global scheduling
 policies. Implemented policies are as follows:

@begin{itemize} 

@item @tt{abs_depth_first} Starting from the modules which require
	analysis (those in @var{ModList}, it uses the depth in the
	intermodule dependency graph as priority, and analyzes the
	modules with higher priority first.

@item @tt{naive_top_down} Traverses the list of modules in the
	intermodule graph in top-down order, and analyzes the modules
	which have pending patterns.  Initially, the modules with
	pending patterns are the ones in @var{ModList}.  It stops when
	in a module traversal there is no module with pending
	patterns.

@item @tt{naive_bottom_up} The same as @tt{naive_top_down}, but the
	list of modules is in reverse top-down order.

@item @tt{top_down_preanalysis} The same as @tt{naive_top_down}, but
	initially all modules are scheduled for analysis. This
	scheduling policy is intended for using an entry policy
	different of @tt{top_level} (for example, @tt{all}).

@item @tt{bottom_up_preanalysis} The same as
	@tt{top_down_preanalysis}, but a bottom-up traversal of the
	intermodule graph is made.
".

setup_scheduling(abs_depth_first,_Analyses,TopLevel,ModList):-
	retractall_fact(force_analysis(_)),
	retractall_fact(iterations(_)),
	push_modules_priorities(ModList),
	retractall_fact(module_depth(_,_)),
	get_all_module_cycles(TopLevel,CycleList),
	gen_module_depths(CycleList,_).
setup_scheduling(naive_top_down,_Analyses,TopLevel,ModList):-
	retractall_fact(naive_module_order(_)),
	retractall_fact(naive_pending_modules(_)),
	retractall_fact(force_analysis(_)),
	retractall_fact(iterations(_)),
	get_all_module_cycles(TopLevel,CycleList),
	reverse(CycleList,RevCycleList),
	assert_in_order(RevCycleList),
	add_naive_pending_modules_(ModList).
setup_scheduling(naive_bottom_up,_Analyses,TopLevel,ModList):-
	retractall_fact(naive_module_order(_)),
	retractall_fact(naive_pending_modules(_)),
	retractall_fact(force_analysis(_)),
	retractall_fact(iterations(_)),
	get_all_module_cycles(TopLevel,CycleList),
	assert_in_order(CycleList),
	add_naive_pending_modules_(ModList).
setup_scheduling(top_down_preanalysis,_Analyses,TopLevel,_ModList):-
	retractall_fact(naive_module_order(_)),
	retractall_fact(naive_pending_modules(_)),
	retractall_fact(force_analysis(_)),
	retractall_fact(iterations(_)),
	get_all_module_cycles(TopLevel,CycleList),
	reverse(CycleList,RevCycleList),
	assert_in_order(RevCycleList),
	add_pending_modules_preanalysis(RevCycleList).
setup_scheduling(bottom_up_preanalysis,_Analyses,TopLevel,_ModList):-
	retractall_fact(naive_module_order(_)),
	retractall_fact(naive_pending_modules(_)),
	retractall_fact(force_analysis(_)),
	retractall_fact(iterations(_)),
	get_all_module_cycles(TopLevel,CycleList),
	assert_in_order(CycleList),
	add_pending_modules_preanalysis(CycleList).
setup_scheduling(_Sched,_Analyses,_TopLevel,ModList):-
	retractall_fact(force_analysis(_)),
	retractall_fact(iterations(_)),
	push_modules_priorities(ModList).

gen_module_depths([MList],1):-
	gen_module_depths_(MList,1).
gen_module_depths([MList|MLists],N):-
	gen_module_depths(MLists,N1),
	N is N1+1,
	gen_module_depths_(MList,N).

gen_module_depths_([],_).
gen_module_depths_([M|Ms],N):-
	asserta_fact(module_depth(M,N)),
	gen_module_depths_(Ms,N).

:- data module_depth/2.
:- data naive_module_order/1.

assert_in_order([]):- !.
assert_in_order([Elem|List]):-
	assert_in_order(Elem),
	assert_in_order(List).
assert_in_order(Module):-
	atom(Module),
	assertz_fact(naive_module_order(Module)).


%%------------------------------------------------------------------

assert_domain(AbsInt):-
	current_fact(domain(AbsInt)), !.
assert_domain(AbsInt):-
	assertz_fact(domain(AbsInt)).

%%------------------------------------------------------------------

%% Cumulative information for modular analyses. In this predicate
%% are grouped all the results returned by the successive calls to 
%% plai:analyze_/4.
:- data total_info/1.

%%------------------------------------------------------------------

% number of (groups of) modules analyzed during the intermodular analysis. 
% Number of calls to the analyzer.
:- data iterations/1.     

%%------------------------------------------------------------------

increment_iterations:-
	( 
	    retract_fact(iterations(It0)) ->
	    It is It0 + 1,
	    set_fact(iterations(It))
	;
	    set_fact(iterations(1))
	).

%%------------------------------------------------------------------

%% Adds information to total_info/1. 
%% - Time information is added;
%% - The maximum of the memory information is stored;
%% - assertion information is appended.
add_to_total_info(Info):-
	current_fact(total_info(TotalInfo)),
	add_to_total_info_1(Info,TotalInfo,TotalInfo1),
	set_fact(total_info(TotalInfo1)), 
	!.

add_to_total_info_1([],TotalInfo,TotalInfo).
add_to_total_info_1([Elem|Info],TotalInfo,TotalInfo2):-
	add_to_total_info_2(Elem,TotalInfo,TotalInfo1),
	add_to_total_info_1(Info,TotalInfo1,TotalInfo2).

add_to_total_info_2(Elem,[],[NewElem]):-
	total_info_new_element(Elem,NewElem).
add_to_total_info_2(Elem,[TElem|TotalInfo],[TElem1|TotalInfo]):-
	same_element(Elem,TElem),
	!,
	add_to_total_info_3(Elem,TElem,TElem1).
add_to_total_info_2(Elem,[TElem|TotalInfo],[TElem|TotalInfo1]):-
	add_to_total_info_2(Elem,TotalInfo,TotalInfo1).

same_element(time(_,_),time(_,_)).
same_element(memory(_,_),memory(_,_)).
same_element(assert_count(Module,_),assert_count(Module,_)).

total_info_new_element(time(A,B),time(A,B)).
total_info_new_element(memory(A,B),memory(A,B)).
total_info_new_element(assert_count(M,L),assert_count(M,NL)):-
	app_count_diff_2(L,NL).

add_to_total_info_3(time(T,TL),time(TT,TTL),time(TT1,TTL1)):-
	TT1 is TT+T,
	add_times(TL,TTL,TTL1).
add_to_total_info_3(memory(T,TL),memory(TT,TTL),memory(TT1,TTL1)):-
	TT1 is TT+T,
	max_memory(TL,TTL,TTL1).
add_to_total_info_3(assert_count(_,TL),assert_count(_,TTL),assert_count(_,TTL1)):-
	app_count_diff(TL,TTL,TTL1).

add_times([],L,L).
add_times([(N,T)|TLs],TTLs,TTL2s):-
	add_times_2((N,T),TTLs,TTL1s),
	add_times(TLs,TTL1s,TTL2s).

add_times_2((N,T),[],[(N,T)]).
add_times_2((N,T),[(N,TT)|TTLs],[(N,TT1)|TTLs]):-
	TT1 is TT+T.
add_times_2((N,T),[(N0,TT)|TTLs],[(N0,TT)|TTL1s]):-
	add_times_2((N,T),TTLs,TTL1s).

	
max_memory([],L,L).
max_memory([(N,T,S)|TLs],[(N,TT,SS)|TTLs],[(N,TT1,SS1)|TTL1s]):-
	max(TT,T,TT1),
	max(SS,S,SS1),
	max_memory(TLs,TTLs,TTL1s).

max(A,B,A):-
	A >= B, !.
max(_A,B,B).

app_count_diff([],L,L2):-
	app_count_diff_2(L,L2). %% The stored list is empty.
app_count_diff([(N,T)|TLs],[(N,V)|TTLs],[(N,TV)|TTL2s]):-
	app_count_diff_3(T,V,TV),
	app_count_diff(TLs,TTLs,TTL2s).

% if there is no previous list, just converts the numbers in lists with 1 element.
app_count_diff_2([],[]).
app_count_diff_2([(N,V)|Ts],[(N,[V])|TTs]):-
	app_count_diff_2(Ts,TTs).

% appends the number to the list, substracting the previous numbers in the list.
% (because assertions checked in previous steps are checked again).
app_count_diff_3([],V,[V]).
app_count_diff_3([N0|Ts],V,[N0|TVs]):-
	V1 is V-N0,
	app_count_diff_3(Ts,V1,TVs).

get_total_info(Info):-
	current_fact(total_info(Info0)),
	sort_total_info(Info0,Info),
	!.

%% All lists of results in the same order for every item.
sort_total_info([],[]).
sort_total_info([I0|I0s],[I1|I1s]):-
	functor(I0,F,A),
	arg(1,I0,Arg1),
	arg(2,I0,I0list),
	sort(I0list,I1list),
	functor(I1,F,A),
	arg(1,I1,Arg1),
	arg(2,I1,I1list),
	sort_total_info(I0s,I1s).
	
	


%%------------------------------------------------------------------

add_iterations_info(Info0,[iterations(N,[])|Info0]):-
	current_fact(iterations(N)), !.
add_iterations_info(Info0,[iterations(0,[])|Info0]).
	

%%------------------------------------------------------------------

auto_analyze_(NaiveSched,Analyses,Info):-
	is_naive_scheduling(NaiveSched),
	set_fact(total_info([])),
	retractall_fact(there_are_previous_errors),
	do_naive_intermod(Analyses),
	get_total_info(Info).
auto_analyze_(Scheduling,Analyses,Info):-
	set_fact(total_info([])),
	retractall_fact(there_are_previous_errors),
	do_intermod(Scheduling,Analyses),
	get_total_info(Info).

is_naive_scheduling(naive_top_down).
is_naive_scheduling(naive_bottom_up).
is_naive_scheduling(top_down_preanalysis).
is_naive_scheduling(bottom_up_preanalysis).

push_modules_priorities([]).
push_modules_priorities([(M,D,F)|ModList]):-
	push(M,D),
	( F = force ->
	  asserta_fact(force_analysis(M))
	;
	  true
	),
	push_modules_priorities(ModList).


% Modules for which the analysis must be forced, even if they 
% have their registry entries up-to-date.
:- data force_analysis/1.

%% Adds the modules in the list to naive_pending_modules/1, and
%% sets force analysis of the ones which need it.
add_naive_pending_modules_([]).
add_naive_pending_modules_([(M,_,F)|ModList]):-
	add_naive_pending_modules([M]),
	( F = force ->
	  asserta_fact(force_analysis(M))
	;
	  true
	),
	add_naive_pending_modules_(ModList).


add_pending_modules_preanalysis([]):- !.
add_pending_modules_preanalysis([X|Xs]):-
	add_pending_modules_preanalysis(X),
	add_pending_modules_preanalysis(Xs).
add_pending_modules_preanalysis(M):-
	atom(M),
	add_naive_pending_modules([M]).
	

	
%%------------------------------------------------------------------

:- data naive_pending_modules/1.

do_naive_intermod(Analyses):-
	current_fact(naive_pending_modules(_)),
	!,
	findall(CurrMod, current_fact(naive_module_order(CurrMod)), Modules),
	naive_analyze_modules(Analyses,Modules),
	(
	    there_are_previous_errors ->
	    true
	;
	    do_naive_intermod(Analyses)
	).
do_naive_intermod(_Analyses).

%%------------------------------------------------------------------

%% Analizes all modules in naive_module_order/1. Stores in
%% naive_pending_modules/1 those related modules which need 
%% reanalysis.
%%
%% AbsInt can be either a domain name or a list of domains.

naive_analyze_modules(_AbsInt, []):-
	!.
naive_analyze_modules(_,_):-
	there_are_previous_errors,
	!.
naive_analyze_modules(AbsInt, [CurrMod|Mods]):-
	retract_fact(naive_pending_modules(CurrMod)),
	!,
	inform_user(['{intermod: analyzing ',CurrMod]),
	cleanup_p_abs,
	module(CurrMod,[time(LoadTime,_)]),
	program(Cls,Ds),
%jcf-04.05.2005
%jcf-04.05.2005%	top_level_module(_,TopLevel),
%jcf-04.05.2005%	module_list(ModList),
%jcf-04.05.2005%	propagate_invalid_info(AbsInt,TopLevel,ModList),
%jcf-04.05.2005
	( 
	    ( current_fact(force_analysis(CurrMod))
	    ; current_pp_flag(interleave_an_check,on)
	    ) ->
	    push_pp_flag(entry_policy,force)
	; 
	    true
	),
	increment_iterations,
	analyze_several_domains(AbsInt,Cls,Ds), % It adds Info to total_info.
	gen_registry_info(quiet,Callers,Imported,[time(GenRegTime,_)]),
%JCF(18.04.05) Comment out following line!!!
	save_registry_info(quiet,_),
	add_to_total_info([time(LoadTime,[(load,LoadTime)]),time(GenRegTime,[(genreg,GenRegTime)])]),
	( 
	    ( retract_fact(force_analysis(CurrMod))
	    ; current_pp_flag(interleave_an_check,on)
	    ) ->
	    ( pop_pp_flag(entry_policy) -> true )  % only once.
	; 
	    true
	),
	add_naive_pending_modules(Callers),
	add_naive_pending_modules(Imported),
	ctcheck_module_naive(CurrMod),
%	output,
	inform_user(['}']),
        naive_analyze_modules(AbsInt,Mods).
naive_analyze_modules(AbsInt, [_CurrMod|Mods]):-
	naive_analyze_modules(AbsInt,Mods).

%%------------------------------------------------------------------

:- data there_are_previous_errors/0.

ctcheck_module_naive(Module):-
	current_pp_flag(interleave_an_check,on), 
	!,
%	acheck_summary(Result),
	acheck_info(assert_count(CTInfo),Summary),
	add_to_total_info([assert_count(Module,CTInfo)]),
	(
	    Summary == error ->
	    inform_user(['{Compile-time check errors found in: ',Module,'}']),
	    retractall_fact(naive_pending_modules(_)),           %% Remove all pending modules.
	    set_fact(there_are_previous_errors),
	    output
	;
	    true
	).
ctcheck_module_naive(_).

%%------------------------------------------------------------------

% Analyzes the clauses in Cls for the domains in the list.
analyze_several_domains([],_Cls,_Ds).
analyze_several_domains([AbsInt|As],Cls,Ds):-
	analyze_(AbsInt,Cls,Ds,Info),
	add_to_total_info(Info),
	assert_domain(AbsInt),
	analyze_several_domains(As,Cls,Ds), !.
	


%%------------------------------------------------------------------

add_naive_pending_modules([]).
add_naive_pending_modules([M|Ms]):-
	( 
	    current_fact(naive_pending_modules(M)) ->
	    true
	;
	    asserta_fact(naive_pending_modules(M))
	),
	add_naive_pending_modules(Ms).


%%------------------------------------------------------------------

:- pred do_intermod(Scheduling, AbsInt) : atm * atm

# "Computes the intermodular fixpoint of the analysis of the current
  program unit (given by top-level) in the @var{AbsInt} abstract
  domain and using @var{Scheduling} scheduling policy.

  @var{AbsInt} can be either a domain name or a list of domains.".

do_intermod(Scheduling,AbsInt):-
	do_intermod_one_module(Scheduling,AbsInt),
	!,
	do_intermod_remaining(Scheduling,AbsInt).
do_intermod(_Scheduling,_AbsInt).

do_intermod_remaining(Scheduling,AbsInt):-
	do_intermod_one_module(Scheduling,AbsInt),
	do_intermod_remaining(Scheduling,AbsInt).
do_intermod_remaining(_Scheduling,_AbsInt).

do_intermod_one_module(Scheduling,AbsInt):-
	pop(CurrMod,CurrPty),
	inform_user(['{intermod: analyzing ',CurrMod,' with priority ',CurrPty]),
	cleanup_p_abs,
%%% statistics(walltime,[T1,_]),
	module(CurrMod,[time(LoadTime,_)]),
%%% statistics(walltime,[T2,_]),
%%% T is T2 - T1,
%%% assertz_fact(module_times(T)),
	program(Cls,Ds),
	( 
	    current_fact(force_analysis(CurrMod)) ->
	    push_pp_flag(entry_policy,force)
	; 
	    true
	),
	increment_iterations,
	analyze_several_domains(AbsInt,Cls,Ds), % It adds Info to total_info.
	gen_registry_info(quiet,Callers,Imported,[time(GenRegTime,_)]),
	add_to_total_info([time(LoadTime,[(load,LoadTime)]),time(GenRegTime,[(genreg,GenRegTime)])]),
%% jcf: following line only useful for testing output.
%% atom_concat(CurrMod,'_out.pl',CurrModOut), output(CurrModOut),
	( 
	    retract_fact(force_analysis(CurrMod)) ->
	    pop_pp_flag(entry_policy)
	; 
	    true
	),
	calc_priority_callers(Scheduling,CurrPty,Callers,CallersPty),
	calc_priority_imported(Scheduling,CurrPty,Imported,ImportedPty),
	inform_user(['{intermod: adding',Callers,' to the priority queue.}']),
	inform_user(['{intermod: adding',Imported,' to the priority queue.}']),
	push(Callers,CallersPty),
	push(Imported,ImportedPty),
	ctcheck_module_intermod(CurrMod),
	inform_user(['}']).

%% --------------------------------------------------------------------

ctcheck_module_intermod(Module):-
	current_pp_flag(interleave_an_check,on), 
	!,
	acheck_summary(Result),
	(
	    Result == error ->
	    inform_user(['{Compile-time check errors found in: ',Module,'}']),
	    reset_queue,  %%Stops the intermodular algorithm.
	    output
	;
	    true
	).
ctcheck_module_intermod(_).

%% --------------------------------------------------------------------

:- pred calc_priority_callers(Policy,CurrPty,Callers,CallersPty) 
	: (atm(Policy), int(CurrPty), list(Callers)) => list(CallersPty) 

# "Calculates the priority of the callers modules in the priority
  queue, @var{CallersPty}, given the priority @var{CurrPty} of the
  current module that has been just analyzed, and the scheduling
  policy @var{Policy}.".


calc_priority_callers(depth_first,CurrPty,_Callers,CallersPty):-
	CallersPty is CurrPty-1.
calc_priority_callers(upper_first,CurrPty,_Callers,CallersPty):-
	CallersPty is CurrPty+1.
calc_priority_callers(once,_CurrPty,_Callers,none).
calc_priority_callers(abs_depth_first,_CurrPty,[],[]).
calc_priority_callers(abs_depth_first,_CurrPty,[Caller|Callers],[P|Ps]):-
	current_fact(module_depth(Caller,P)),
	calc_priority_callers(abs_depth_first,_CurrPty,Callers,Ps).
	
	
%% --------------------------------------------------------------------

:- pred calc_priority_imported(Policy,CurrPty,Imported,ImportedPty) 
	: (atm(Policy), int(CurrPty), list(Imported)) => list(ImportedPty)

# "Calculates the priority of the imported modules in the priority
  queue, @var{ImportedPty}, given the priority @var{CurrPty} of the
  current module that has been just analyzed, using the scheduling
  policy @var{Policy}.".


calc_priority_imported(depth_first,CurrPty,_,ImportedPty):-
	ImportedPty is CurrPty+1.
calc_priority_imported(upper_first,CurrPty,_,ImportedPty):-
	ImportedPty is CurrPty-1.
calc_priority_imported(once,_CurrPty,_,none).
calc_priority_imported(abs_depth_first,_CurrPty,[],[]).
calc_priority_imported(abs_depth_first,_CurrPty,[IM|IMs],[P|Ps]):-
	current_fact(module_depth(IM,P)),
	calc_priority_callers(abs_depth_first,_CurrPty,IMs,Ps).

%% ********************************************************************
%% Monolithic intermodular analysis.
%% ********************************************************************

:- pred monolithic_analyze(+AbsInt,+TopLevel,-Info) 

# "Performs the analysis of the program unit for which @var{Module} is
   the top-level module in the @var{AbsInt} abstract domain using a
   @em{monolithic} scheduling: all the modules in the program unit
   are loaded and analyzed simultaneously.".

monolithic_analyze(Analysis,TopLevel,Info):-
	atom(Analysis),!,
	monolithic_analyze([Analysis],TopLevel,Info).
monolithic_analyze(Analyses,TopLevel,Info):-
	statistics(runtime,[T1,_]),  %% total ellapsed time.
	valid_mod_analysis(Analyses), !,
	cleanup_intermod,
	inform_user(['{Analyzing with monolithic_analyze: ',TopLevel]),
	reset_mem_usage,
	push_prolog_flag(gc,on),
	set_top_level(TopLevel),
	push_pp_flag(intermod,auto),
%%jcf-20.10.2005%	push_pp_flag(entry_policy,top_level), %% This must be done before calling to auto_analyze
	%% nn
	get_all_modules(TopLevel,ModList),
%%%%
	module(ModList,[time(LoadTime,_)]),
	program(Cls,Ds),
	set_fact(total_info([])),
	analyze_several_domains(Analyses,Cls,Ds),
%%%%
	gen_registry_info(quiet,_Callers,_Imported,[time(GenRegTime,_)]),
	save_registry_info(quiet,[time(SaveTime,_)]),
	statistics(runtime,[T2,_]),  %% total ellapsed time.
	add_to_total_info([time(LoadTime,[(load,LoadTime)]),time(GenRegTime,[(genreg,GenRegTime)]),time(SaveTime,[(savereg,SaveTime)])]),
	global_time_ellapsed(T2,T1,Ellapsed),
	add_to_total_info([time(Ellapsed,[(total_ellapsed,Ellapsed)])]),
	pop_prolog_flag(gc),
	ask_mem_usage(TotalMem,DetMem),
	(
	    nonvar(TotalMem) ->
	    add_to_total_info([memory(TotalMem,DetMem)])
	;
	    true
	),
	get_total_info(Info),
	%% 
%%jcf-20.10.2005%	pop_pp_flag(entry_policy),
	pop_pp_flag(intermod),
	set_modules_analyzed(ModList),
	inform_user(['}']).


%% ******************************************************************
%% Priority Queue handling predicates.
%% ******************************************************************

:- pred queue(QueueList) : list 

# "Data predicate to store (in a single fact) the priority
  queue. @list{QueueList} must be the list of @tt{priority-module}
  pairs in reverse order.".

:- data queue/1.

queue([]).

%% --------------------------------------------------------------------

:- pred reset_queue # "Empties the queue.".

reset_queue:-
	set_fact(queue([])).

%% --------------------------------------------------------------------

:- pred pop(-Element,-Priority) => (atm(Element), int(Priority))

# "Pops the element @var{Element} with highest priority from the
  priority queue.".

pop(Element,Priority):-
	retract_fact(queue([Pty-Element|Rest])),
	Priority is Pty * (-1),
	set_fact(queue(Rest)).

%% --------------------------------------------------------------------

:- pred push(+Element,+Priority) : (atm(Element), int(Priority))

# "Pushes a new element @var{Element} with priority @var{Priority}
  into the priority queue. If @var{Element} is already in the queue,
  it is not duplicated, but its priority is changed to the maximum
  prioriy of the already existing element and the priority of the new
  element.".

:- pred push(+ElementList,+Priority) : list(atm) * integer

# "Pushes a set of new elements @var{ElementList} with priority
  @var{Priority} into the priority queue. If any element of
  @var{ElementList} is already in the queue, it is not duplicated, but
  its priority is changed to the maximum prioriy of the already
  existing element and the priority of the new element.".

push([],_):- !.
push([Element|Rest],[Pty|Ptys]):-
	push(Element,Pty),
	push(Rest,Ptys),!.
push([Element|Rest],Priority):-
	integer(Priority),
	push(Element,Priority),
	push(Rest,Priority),
	!.
% no integer priority means nothing.
push(_,Priority):-
	\+ integer(Priority).

push(Element,Priority):-
	integer(Priority),!,
	Pty is Priority * (-1),
	current_fact(queue(Queue)),
	( select(Pty0-Element,Queue,Queue0) ->
	  ( Pty0 =< Pty ->
	    true
	  ; insert(Queue0,Pty-Element,NewQueue),
	    set_fact(queue(NewQueue))
	  )
	; insert(Queue,Pty-Element,NewQueue),
	  set_fact(queue(NewQueue))
	).
% no integer priority means nothing.
push(_,_Priority).

%% ******************************************************************
%% Modular analysis checking
%%    registry must have reached a fixpoint!!!!!!!
%% ******************************************************************

:- pred auto_check(+Analysis,+TopLevel) 

# "After using @pred{auto_analyze/2}, this predicate allows checking
  the results of the analysis. Generates internal (@code{complete/7})
  information for all the modules in the program unit @var{TopLevel},
  and stores it in memory in order to compare it with the results of
  @pred{monolithic_analyze/2}.".

auto_check(Analysis,TopLevel):-
	atom(Analysis),  % Only one analysis domain is considered.
	valid_mod_analysis(Analysis), !,
	cleanup_intermod,
	inform_user(['{Generating check info for program unit: ',TopLevel]),
	set_top_level(TopLevel),
	push_pp_flag(intermod,auto),
	push_pp_flag(entry_policy,force),
	push_pp_flag(dump_pp,off),
	get_all_modules(TopLevel,ModList),
	retractall_fact(complete_prev(_,_,_,_,_,_,_)),
	push_pp_flag(reuse_fixp_id,on),
	fixpo_ops:reset_previous_analysis(Analysis),
	auto_check_modules(Analysis,ModList), %% reanalyzes all modules' entries.
	%%
	%% Checking that completes are equal to those computed with monolithic_analyze.
	module(ModList),
	program(Cls,Ds),
	fixpo_ops:restore_previous_analysis(Analysis),   %% restores needed types. module/1 removes them.
	analyze_(Analysis,Cls,Ds,_Info),
	assert_domain(Analysis),
	remove_useless_info(Analysis),
%	compare_completes_with_prev(Analysis,Flag,'='),
	compare_completes_with_prev(Analysis,Flag,'>='),
	(
	    var(Flag) ->
	    inform_user(['Comparison with monolithic analysis completed successfully.'])
	;
	    inform_user(['Comparison with monolithic analysis has not succeeded. See previous messages.'])
	),
	%%
	pop_pp_flag(reuse_fixp_id),
	pop_pp_flag(dump_pp),
	pop_pp_flag(entry_policy),
	pop_pp_flag(intermod),
	inform_user(['}']).

% checking_fixpoint(check_di).


auto_check_modules(_Analysis,[]).
auto_check_modules(Analysis,[M|Ms]):-
	auto_check_one_module(Analysis,M),
	auto_check_modules(Analysis,Ms).

auto_check_one_module(Analysis,File):-
	absolute_file_name(File, '_opt', '.pl', '.', _, BaseAbs, _),
	just_module_name(BaseAbs,Module),
 	inform_user(['{generating check info for module: ',BaseAbs]),
%jcf (to save memory; the process will be slower).
%jcf	cleanup_p_abs_all,
%jcf
	module(BaseAbs),
 	program(Cls,Ds),
%
	fixpo_ops:restore_previous_analysis(Analysis),   %% restores needed types (module/1 removes them)
%
 	analyze_(Analysis,Cls,Ds,_Info),
 	assert_domain(Analysis),
	remove_useless_info(Analysis),
	gen_registry_info(quiet,_,_),
	filter_completes(Analysis,Module),
	fixpo_ops:store_previous_analysis_aux_info(Analysis), %% Stores types of previous analyses.
	fixpo_ops:store_previous_analysis_completes(Analysis), %%Stores info of latest analysis.
	!.

% true(_).

filter_completes(AbsInt,Module):-
	current_fact(complete(_A,AbsInt,Sg,_C,_D,_E,_F),Ref),
	get_module_from_sg(Sg,Module0), 
	Module \= Module0,
	Module0 \= multifile,
	module_is_processable(Module0),
	erase(Ref),
	fail.
filter_completes(_AbsInt,_Module).

/*
move_prev_to_completes(AbsInt):-
	retractall_fact(complete(_,_,_,_,_,_,_)),
	retract_fact(complete_prev(A,AbsInt,Sg,C,D,E,F)),
	asserta_fact(complete(A,AbsInt,Sg,C,D,E,F)),
	fail.
move_prev_to_completes(_AbsInt).
*/

%% ******************************************************************
%% Modular program transformations (for specialization)
%% ******************************************************************

:- pred module_processed(Module,AlreadyProcessed) 

# "Lists the modules in the program unit and whether they are already
   processed or not.".

:- data module_processed/2.

%%------------------------------------------------------------------

valid_transformation(Trans):-
	transformation(Trans), !.
valid_transformation(Trans):-
	inform_user(['{Not a valid transformation: ',Trans,'}']),
	fail.

%%------------------------------------------------------------------

:- prop transformation(Transformation)
	# "@var{Transformation} is a valid transformation identifier.".
:- multifile transformation/1.

%% ---------------------------------------------------------------------------

:- pred auto_transform(+Analysis,+Trans,+TopLevel) 

# "Performs transformation @var{Trans} of the program unit which has
  @var{TopLevel} as top-level module, using @var{Analysis} to get
  information about the program.".
:- set_prolog_flag(multi_arity_warnings,off).

auto_transform(Analysis,Trans,TopLevel):-
	auto_transform(Analysis,Trans,TopLevel,_Info).

auto_transform(Analysis,Trans,TopLevel,Info):-
	valid_transformation(Trans), !,
	cleanup_intermod,
	inform_user(['{Transforming with auto_transform: ',TopLevel]),
	set_top_level(TopLevel),
	push_pp_flag(intermod,auto),
	push_pp_flag(entry_policy,force),
	%% 
	get_all_module_cycles(TopLevel,CycleList),
	statistics(runtime,[T1,_]),  
        auto_transform_(Analysis,Trans,CycleList),
	statistics(runtime,[T2,_]),  
	global_time_ellapsed(T2,T1,Ellapsed),
	Info = [time(Ellapsed,[(transform,Ellapsed)])],
	save_registry_info(quiet,_SaveInfo),
	%% 
	pop_pp_flag(entry_policy),
	pop_pp_flag(intermod),
	inform_user(['}']).

:- set_prolog_flag(multi_arity_warnings,on).

%% ---------------------------------------------------------------------------

auto_transform_(_Analysis,_Trans,[]).
auto_transform_(Analysis,Trans,[Cycle|CycleList]):-
	transform_one_cycle(Analysis,Trans,Cycle),
	auto_transform_(Analysis,Trans,CycleList).

%% ---------------------------------------------------------------------------

transform_one_cycle(Analysis,Trans,[Base]):- !,
	transform_one_module(Analysis,Trans,Base,_Changed).
transform_one_cycle(Analysis,Trans,Cycle):-
	transform_fixpoint(Analysis,Trans,Cycle).

%% ---------------------------------------------------------------------------

transform_fixpoint(Analysis,Trans,Cycle):-
	transform_module_list(Analysis,Trans,Cycle,Changed),
	(
	    Changed = yes ->
	    transform_fixpoint(Analysis,Trans,Cycle)
	;
	    true
	).

%% ---------------------------------------------------------------------------

transform_module_list(_Analysis,_Trans,[],no).
transform_module_list(Analysis,Trans,[Base|Bases],Changed):-
	transform_one_module(Analysis,Trans,Base,Changed0),
	( 
	    Changed0 == yes ->
	    Changed = yes
	;
	    Changed = Changed1
	),
	transform_module_list(Analysis,Trans,Bases,Changed1).

%% ---------------------------------------------------------------------------

transform_one_module(Analysis,Trans,File,Changed):-
	absolute_file_name(File, '_opt', '.pl', '.', _, BaseAbs, _),
 	inform_user(['{intermod: transforming ',BaseAbs,'}']),
%jcf%-very special cases: basiccontrol.pl, etc.
	just_module_name(BaseAbs,Mod),
	( 
	    module_not_transformable(Mod) ->
	    inform_user(['{intermod: Module not transformable: ',BaseAbs,'}'])
	;
%jcf%
	    (
		registry_is_empty(Analysis,Mod,BaseAbs) ->
		inform_user(['{intermod: Module does not need transformation: ',BaseAbs,'}'])
	    ;
		module(BaseAbs),
		program(Cls,Ds),
		analyze_(Analysis,Cls,Ds,_Info),
		assert_domain(Analysis),
		gen_registry_info(quiet,_Callers,_Imported),
%    Program must be re-read.
		program(Cls2,Ds2),
		get_spec_info_imported,
		transform_(Trans,Analysis,Cls2,Ds2,BaseAbs,Changed),
% Missing: replacement of specialized versions in .reg files!!
		save_registry_info(quiet,BaseAbs,_SaveInfo),
%
		atom_concat(BaseAbs,'_opt.pl',OutFile),
		output(OutFile)
            )
         ),
	 !.

%KLUDGE!!
module_not_transformable(basiccontrol).
%module_not_transformable(internals).

transform_(Trans,AbsInt,Cls,Ds,BaseAbs,Changed):-
	simpspec_(Trans,AbsInt,Cls,Ds,TmpCls,TmpDs),
	update_spec_info(BaseAbs,Changed),
	update_ai_info_case(TmpCls,TmpDs,NewCls,NewDs),
 	replace_program(NewCls,NewDs).

%%This pred has been taken and adapted from driver.pl.
simpspec_(vers,_AbsInt,_Cls,_Ds,_NewCls,_NewDs):- !,
	inform_user(['{vers not implemented yet in auto_transform/4}']),
	fail.
simpspec_(codegen,AbsInt,Cls,Ds,NewCls,NewDs):- !,
	(
	    current_pp_flag(local_control,off)
	-> 
            NewCls = Cls,
	    NewDs = Ds 
	;
	    codegen(AbsInt,NewCls,NewDs,_Info)
	).
simpspec_(codegen_af,AbsInt,Cls,Ds,NewCls,NewDs):- !,
	(
	    current_pp_flag(local_control,off)
	-> 
            NewCls = Cls,
	    NewDs = Ds 
	;
	    codegen_af(AbsInt,NewCls,NewDs,_Info)
	).
simpspec_(Spec,AbsInt,Cls,Ds,NewCls,NewDs):-
	simplify_specialize(AbsInt,Spec,Cls,Ds,NewCls,NewDs).

%% ******************************************************************
%% Modular compile-time checking
%% ******************************************************************

:- pred auto_ctcheck_list(+Analysis,+TopLevel, +Modules) 

# "Performs CT assertion checking of modules on the @car{Modules} list
  of the program unit which has
  @var{TopLevel} as a top-level module, using @var{Analysis} to get
  information about the program.".


auto_ctcheck_list(Analysis, TopLevel,Modules) :-
	auto_ctcheck(Analysis, TopLevel,_Info,Modules).



:- pred auto_ctcheck(+Analysis,+TopLevel) 

# "Performs CT assertion checking of the program unit which has
  @var{TopLevel} as a top-level module, using @var{Analysis} to get
  information about the program.".

auto_ctcheck_summary(Analysis,Summary) :-
	current_prolog_flag(main_module,TopLevel),
	auto_ctcheck_internal(Analysis, TopLevel,_Info,_Modules,Summary).

auto_ctcheck_summary(Analysis, TopLevel,Summary) :-
	auto_ctcheck_internal(Analysis, TopLevel,_Info,_Modules,Summary).


auto_ctcheck(Analysis) :-
	current_prolog_flag(main_module,TopLevel),
	auto_ctcheck_internal(Analysis, TopLevel,_Info,_Modules,_).

auto_ctcheck(Analysis, TopLevel) :-
	auto_ctcheck_internal(Analysis, TopLevel,_Info,_Modules,_).


auto_ctcheck(Analysis, TopLevel,Info,ModuleList) :-
	auto_ctcheck_internal(Analysis, TopLevel, Info,ModuleList,_).

auto_ctcheck_internal(Analysis, TopLevel, [(time,Time),Info],ModuleList,Summary) :-
%	valid_mod_analysis_all(Analysis),!,
	statistics(runtime,[T1,_]),  
        cleanup_intermod,
	inform_user(['{Modular-based assertion checking with auto_ctchecks: ',TopLevel]),
	set_top_level(TopLevel),
	push_pp_flag(intermod,auto),
	( current_pp_flag(ct_ext_policy, assertions) ->
	  push_pp_flag(entry_policy, force_assrt),
	  push_pp_flag(success_policy,top)
	; push_pp_flag(entry_policy, force),
	  push_pp_flag(success_policy,under_all)
	),
	( var(ModuleList) ->
	  get_all_modules(TopLevel, ModuleList)
	; true
	),
%	display(modules(ModuleList)),
        auto_ctcheck_(Analysis, TopLevel, ModuleList, Info,Summary),
	pop_pp_flag(entry_policy),
	pop_pp_flag(success_policy),
	pop_pp_flag(intermod),
	statistics(runtime,[T2,_]),
 	Time is T2 - T1,
	inform_user(['}']).


auto_ctcheck_(_Analysis, _TopModule, [], assert_count([]),ok).
auto_ctcheck_(Analysis, TopModule, [Module|Modules], assert_count(Info),SOut) :-
	module(Module,_LoadInfo),
	curr_file(File,_),
	inform_user(['{Analyzing for auto_ctcheck: ',File]),
	set_top_level(TopModule),
	program(Cls,Ds),
	cleanup_p_abs,
	analyze_tmp(Analysis,Cls,Ds),
	assert_domain(Analysis),
	inform_user(['}']),!,
	acheck_info(assert_count(Info1),Summ),
	output,
	auto_ctcheck_(Analysis, TopModule, Modules, assert_count(Info2),S1),
	combine_info(Info1, Info2, Info),
	combine_summ(Summ,S1,SOut).

combine_summ(ok,ok,OK) :-!, OK = ok.
combine_summ(error,_,E) :- !, E=error.
combine_summ(_,error,E) :- !, E=error.
combine_summ(_,_,warning).

analyze_tmp(As,Cls,Ds) :-
	As = [_|_],!,
	analyze_iter(As,Cls,Ds).
analyze_tmp(A,Cls,Ds) :-
	analyze_(A,Cls,Ds,_Info),
	assert_domain(A).

analyze_iter([],_,_).
analyze_iter([A|As],Cls,Ds) :-
	analyze_tmp(A,Cls,Ds),
	analyze_iter(As,Cls,Ds).

/*
valid_mod_analysis_all([]):-!.
valid_mod_analysis_all([A|As]):-!,
	valid_mod_analysis(A),
	valid_mod_analysis_all(As).
valid_mod_analysis_all(A):-
	valid_mod_analysis(A).
*/

combine_info(I, [], I) :-!.
combine_info([], I, I) :-!.
combine_info([(C,V)|Is],[(C,V1)|Is1],[(C,V2)|Is2]) :-
	V2 is V + V1,
	combine_info(Is, Is1, Is2).


% -----------------------------------------------------------------------------
:- pred auto_ctcheck_opt(+Analysis,+TopLevel) 

# "Performs CT assertion checking of the program unit which has
  @var{TopLevel} as a top-level module, using @var{Analysis} to get
  information about the program (exploits order of the modules).".

:- doc(bug,"auto_ctcheck_opt/2-3 does modify the source code of
	program modules.  This issue can only be solved when _opt.pl
	files are handled properly.").

auto_ctcheck_opt(Analysis, TopLevel) :-
	auto_ctcheck_opt(Analysis, TopLevel,_Info).

auto_ctcheck_opt(Analysis, TopLevel, [(time,Time),Info]) :-
	valid_mod_analysis(Analysis),!,

        cleanup_intermod,
	inform_user(['{Modular-based assertion checking with auto_ctcheck_opt: ',TopLevel]),
	
%jcf%	copy_sources,
	push_pp_flag(intermod,auto),
	push_pp_flag(entry_policy,force), 
%jcf%	atom_concat('test_opt/',TopLevel,CopyTopLevel),
	TopLevel = CopyTopLevel,
%jcf%
%jcf%	auto_analyze(Analysis, CopyTopLevel),

	statistics(runtime,[T1,_]),  
	set_top_level(CopyTopLevel),
	get_all_module_cycles(CopyTopLevel, ModuleLList),
	list_of_lists_to_list(ModuleLList, ModuleList),
%	display(modules(ModuleList)),

        auto_ctcheck_opt_(Analysis, TopLevel, ModuleList, Info),
	pop_pp_flag(entry_policy),
	pop_pp_flag(intermod),
	statistics(runtime,[T2,_]),
 	Time is T2 - T1,
	inform_user(['}']).


auto_ctcheck_opt_(_Analysis, _TopModule, [], assert_count([])).
auto_ctcheck_opt_(Analysis, TopModule, [Module|Modules], assert_count(Info)) :-

%	absolute_file_name(FileName, '_opt', '.pl', '.', _, Base, _),

	module(Module,_LoadInfo),
	curr_file(File,_),
	inform_user(['{Analyzing for auto_ctcheck: ',File]),
	set_top_level(TopModule),
%	push_pp_flag(entry_policy,all),
	program(Cls,Ds),
	cleanup_p_abs,
	analyze_(Analysis,Cls,Ds,_Info),
	assert_domain(Analysis),
	inform_user(['}']),!,
	acheck_info(assert_count(Info1),_),
	atom_concat(Module,'.pl',Module_pl),
	output(Module_pl),
	atom_concat(Module,'.ast',Module_ast),
	( file_exists(Module_ast) ->
	  delete_file(Module_ast)
	; true
	),
	auto_ctcheck_opt_(Analysis, TopModule, Modules, assert_count(Info2)),
	combine_info(Info1, Info2, Info).

/*
copy_sources :-
	( file_exists(test_opt)
	; make_directory(test_opt)
	),
	!,
%	no_path_file_name(FullOrigName,M),
%	atom_concat(Dir,M,FullOrigName),
%	atom_concat(Dir,'*.pl',Pattern),
	ls('*.pl',Files),

%	atom_concat(Dir,'test_opt/', CopyDir),
	copy_files(Files, 'test_opt/',[overwrite]).
%	atom_concat(CopyDir,M, Copy).
*/

% I know, it should not be here....
list_of_lists_to_list([],[]).
list_of_lists_to_list([X|Xs],L) :-
	list_of_lists_to_list(Xs,L1),
	append(X,L1,L).


% -----------------------------------------------------------------------------
monolithic_ctcheck(Analysis) :-
	current_prolog_flag(main_module,TopLevel),
	monolithic_ctcheck(Analysis, TopLevel,_Info).

monolithic_ctcheck(Analysis,TopLevel):-
	monolithic_ctcheck(Analysis,TopLevel,_Info).

monolithic_ctcheck(Analysis,TopLevel,[(time,Time),Info]):-
	valid_mod_analysis(Analysis), !,
	cleanup_intermod,
	inform_user(['{Generating check info for program unit: ',TopLevel]),
	set_top_level(TopLevel),
	push_pp_flag(intermod,auto),
	push_pp_flag(entry_policy,force),
	push_pp_flag(dump_pp,off),
	get_all_modules(TopLevel,ModList),
	statistics(runtime,[T1,_]), 
	module(ModList,[time(_LoadTime,_)]),
	program(Cls,Ds),
	analyze_(Analysis,Cls,Ds,_Info0),
	assert_domain(Analysis),
	pop_pp_flag(dump_pp),
	pop_pp_flag(entry_policy),
	pop_pp_flag(intermod),
	inform_user(['}']),
	acheck_info(Info,_Summary),
	statistics(runtime,[T2,_]),
	Time is T2 - T1.


% -----------------------------------------------------------------------------

:- push_prolog_flag(multi_arity_warnings,off).

inductive_ctcheck_summary(Analysis,TopLevel,ERR):-
	inductive_ctcheck(Analysis,TopLevel,_Info,ERR).

inductive_ctcheck(Analysis,TopLevel):-
	inductive_ctcheck(Analysis,TopLevel,_Info,_ERR).

inductive_ctcheck(Analysis,TopLevel,[(time,Time),Info],ERR):-
%	valid_mod_analysis_all(Analysis), !,
	inform_user(['{Inductive assertions checking in : ',TopLevel]),
	get_all_modules(TopLevel,ModList),
	push_pp_flag(intermod, off),
	statistics(runtime,[T1,_]), 
	ind_ctcheck_(Analysis, ModList, Info,ERR), 
	pop_pp_flag(intermod),
	inform_user(['}']),
	statistics(runtime,[T2,_]),
	Time is T2 - T1,
	set_modules_analyzed(ModList).

ind_ctcheck_(_Analysis, [], assert_count([]),ok).
ind_ctcheck_(Analysis, [Module|Modules], assert_count(Info),ERR) :-
	module(Module),
	analyze_list(Analysis), !,
	acheck_info(assert_count(Info1),ERR1),
	output,
	ind_ctcheck_(Analysis, Modules, assert_count(Info2),ERR2),
	combine_info(Info1, Info2, Info),
	combine_summ(ERR1,ERR2,ERR).


analyze_list([]).
analyze_list([A|As]):-
	analyze(A),
	analyze_list(As).


:- pop_prolog_flag(multi_arity_warnings).
% -----------------------------------------------------------------------------

acheck_info(Info,Summary) :-
	setcounter(pp_checked_c,0),
	setcounter(pp_check_c,0),
	setcounter(pp_false_c,0),
	setcounter(simp_checked_c,0),
	setcounter(simp_check_c,0),	
	setcounter(simp_false_c,0),
	setcounter(pp_checked_s,0),
	setcounter(pp_check_s,0),
	setcounter(pp_false_s,0),
	setcounter(simp_checked_s,0),
	setcounter(simp_check_s,0),	
	setcounter(simp_false_s,0),
	setcounter(simp_true_s,0),
	acheck_summary(Summary),
	getcounter(pp_checked_c,PPCheckedC),
	getcounter(pp_check_c,PPCheckC),
	getcounter(pp_false_c,PPFalseC),
	getcounter(simp_checked_c,SimpCheckedC),
	getcounter(simp_check_c,SimpCheckC),	
	getcounter(simp_false_c,SimpFalseC),
	getcounter(pp_checked_s,PPCheckedS),
	getcounter(pp_check_s,PPCheckS),
	getcounter(pp_false_s,PPFalseS),
	getcounter(simp_checked_s,SimpCheckedS),
	getcounter(simp_check_s,SimpCheckS),	
	getcounter(simp_false_s,SimpFalseS),
	getcounter(simp_true_s,SimpTrueS),
	Info = assert_count([(pp_checked_c,PPCheckedC),
		(pp_check_c,PPCheckC),
		(pp_false_c,PPFalseC),
		(simp_checked_c,SimpCheckedC),
		(simp_check_c,SimpCheckC),	
		(simp_false_c,SimpFalseC),
		(pp_checked_s,PPCheckedS),
		(pp_check_s,PPCheckS),
		(pp_false_s,PPFalseS),
		(simp_checked_s,SimpCheckedS),
		(simp_check_s,SimpCheckS),	
         	(simp_false_s,SimpFalseS),
		(simp_true_s,SimpTrueS)]).


% -----------------------------------------------------------------------------

:- push_prolog_flag(multi_arity_warnings,off).

cleanreg:-
	current_pp_flag(tmp_dir,Dir),
	(
	    Dir = source ->
	    inform_user(['reg files not erased because tmp_dir is set to source. Use cleanreg/1 instead.'])
	;
	    delete_files_type(Dir,'.reg')
	).

cleanreg(File):-
	current_prolog_flag(main_module,TopLevel),
	(
	    TopLevel == '' ->
	    cleanreg2(File)
	;
	    cleanreg2(TopLevel)
	).

:- pop_prolog_flag(multi_arity_warnings).

cleanreg2(Top):-
	get_all_modules(Top,ModList),
	get_modules_regnames(ModList,RegList),
	delete_files(RegList).

get_modules_regnames([],[]).
get_modules_regnames([Mod|ModList],[Reg|RegList]):-
	get_module_filename(reg,Mod,Reg),
	get_modules_regnames(ModList,RegList).

%%-------------------------------------------------------------------

delete_files(FileList):-
	list(FileList),
	!,
	delete_files_('.',FileList).
delete_files(Dir):-
	directory_files(Dir,Files),
	delete_files_(Dir,Files).

delete_files_(_Dir,[]).
delete_files_(Dir,['.'|Files]):-
	!,
	delete_files_(Dir,Files).
delete_files_(Dir,['..'|Files]):-
	!,
	delete_files_(Dir,Files).
delete_files_(Dir,[File|Files]):-
	(
	    atom_concat('/',_,File) ->
	    File = AbsFile
	;
	    atom_concat([Dir,'/',File],AbsFile)
	),
	(
	    file_exists(AbsFile) ->
	    (
		file_property(AbsFile,type(directory)) ->
		delete_files(AbsFile),
		delete_directory(AbsFile)
	    ;
		inform_user(['{Erasing ',AbsFile,'}']),
		delete_file(AbsFile)
	    )
	;
	    true
	),
	delete_files_(Dir,Files).

%---------------------------------------------------------------------

copy_file(FileSpec,TargetDir):-
	atom_concat(['/bin/cp ',FileSpec,' ',TargetDir],Cmd),
	system(Cmd).

%---------------------------------------------------------------------

delete_files_type(Dir,Ext):-
	directory_files(Dir,Files),
	delete_files_type_(Dir,Ext,Files).

delete_files_type_(_Dir,_Ext,[]).
delete_files_type_(Dir,Ext,[File|Files]):-
	absolute_file_name(File,'',Ext,Dir,AbsFile,_,_),
	(
	    file_exists(AbsFile) ->
	    delete_file(AbsFile)
	;
	    true
	),
	delete_files_type_(Dir,Ext,Files).

%% ******************************************************************
%% dead-code elimination for libraries.
%% ******************************************************************

:- doc(bug,"The code for auto_simp_libs/2-3 is still under rough
	development.").

:- pred auto_simp_libs(+TopLevel,+Dir) 

# "Generates a copy of the program represented by @var{TopLevel} and
  the libraries used (except those in lib/engine) in @var{Dir}, and
  removes dead-code from both user modules and libraries.".

auto_simp_libs(TopLevel,Dir0):-
	auto_simp_libs(TopLevel,Dir0,_Info).

auto_simp_libs(TopLevel,Dir,Info):-
	cleanup_intermod,
	inform_user(['{Processing with auto_simp_libs: ',TopLevel]),
	set_top_level(TopLevel),
	(
	    file_exists(Dir) ->
	    inform_user(['{Removing all files in ',Dir,'}']),
	    push_pp_flag(intermod,auto),
	    push_pp_flag(entry_policy,force),
%	    push_pp_flag(process_libraries,no_engine),
	    push_pp_flag(process_libraries,on),
%
	    delete_files(Dir),
%	    cleanup_itf_cache,
	    get_all_modules(TopLevel,ModList,InclList),
	    inform_user(['Copying library files to ',Dir]),
	    copy_modules(ModList,Dir,TargetList),
	    copy_modules(InclList,Dir,_InclTargetList),
	    get_modules_regnames(TargetList,RegList),
	    delete_files(RegList),
	    asserta(library_directory(Dir)),
	    atom_concat(Dir,'/engine',DirEngine),
	    asserta(file_search_path(engine,DirEngine)),
	    get_new_base(Dir,TopLevel,NewTopLevel),
	    cleanup_itf_cache,
	    auto_analyze(pdb,NewTopLevel,Info0),
%	    monolithic_analyze(pdb,NewTopLevel),
	    auto_transform(pdb,simp,NewTopLevel,Info1),
	    append(Info0,Info1,Info),
	    retract(library_directory(Dir)),
	    retract(file_search_path(engine,DirEngine)),
	    pop_pp_flag(process_libraries),
	    pop_pp_flag(entry_policy),
	    pop_pp_flag(intermod)
	;
	    inform_user(['Directory does not exist: ',Dir])
	),
	inform_user(['}']).

:- multifile library_directory/1.
:- dynamic library_directory/1.

copy_modules([],_,[]).
copy_modules([Mod|ModList],Dir,[TargetMod|TargetModList]):-
	get_module_filename(pl,Mod,FileName),
	inform_user(['{Copying: ',FileName]),
	copy_lib_subdir(Dir,FileName,SubDir),
	get_new_base(SubDir,Mod,TargetMod),
	copy_modules(ModList,Dir,TargetModList).

% If FileName (absolute file name) is in a subdirectory of a library directory, then the
% subdir must be reproduced in Dir. Returns SubDir, the absolute path of
% the subdirectory in Dir.
% If FileName is not in a library directory, it is copied directly to Dir, and Dir is 
% returned as third argument.
copy_lib_subdir(Dir,FileName,SubDir):-
	get_lib_subdir0(FileName,SubDir0),
	!,
	atom_concat([Dir,'/',SubDir0],SubDir),
	make_dirpath(SubDir),
	copy_file(FileName,SubDir).
copy_lib_subdir(Dir,FileName,Dir):-
	copy_file(FileName,Dir).

:- dynamic library_directory/1.
:- multifile library_directory/1.
:- multifile file_search_path/2.
:- dynamic file_search_path/2.
get_lib_subdir0(FileName,SubDir):-
	library_directory(LibDir0),
	atom_concat(LibDir0,'/',LibDir),
	atom_concat(LibDir,SubDirFile,FileName),
	no_path_file_name(FileName,NoPathFile),
	atom_concat(SubDir,NoPathFile,SubDirFile),
	!.


get_new_base(Dir,Mod,TargetMod):-
	just_module_name(Mod,ModName),
	absolute_file_name(ModName,'','.pl',Dir,_,TargetMod,_).
%	atom_concat([Dir,'/',ModName],TargetMod).

