:- module(p_abs,[
	cleanup_p_abs/0,
	cleanup_p_abs_all/0,
	gen_registry_info/3,
	gen_registry_info/4,
	save_registry_info/2,
	save_registry_info/3,
	update_spec_info/2,
	get_spec_info_imported/0,
	%%%%%%%%%%%%%%%%%%%%%%% LOW-LEVEL FILE ACCESS PRIMITIVES
	read_registry_file/3,
	reread_registry_file/3,
	write_registry_file/3,
	registry/3,
	%%%%%%%%%%%%%%%%%%%%%%% 
        get_imported_modules/0,
	imported_modules/2,
	registry_headers/2,
	add_to_imdg_list/5,
	add_changed_module/4,
	open_mode/3,
	change_open_mode/2,
	may_be_improved_mark/2,
	not_valid_mark/2,
%%%intermodule-graph
	get_modules_to_analyze/3,
	get_all_modules/2,
	get_all_modules/3,
	get_all_module_cycles/2,
	get_all_modules_depth/2,
	get_module_from_sg/2,
	recover_from_invalid_state/2,
	propagate_invalid_info/3,
%%%intermodule-graph
	module_is_processable/1,
	registry_is_empty/3,
%%%Resource intermodule-analysis (JNL)
	get_imported_calls/1
	],[assertions,regtypes,basicmodes,isomodes]).
:- use_package(spec(nomem_usage)).

:- use_module(spec(spec_multiple), [publish_pred_name/2, get_version_name/4]).

:- use_module(program(itf_db), [current_itf/3, curr_file/2]).

:- use_module(infer(infer_db), [domain/1]).
:- use_module(ciaopp(preprocess_flags), [current_pp_flag/2]).
:- use_module(plai(plai_db),   [complete/7]).
:- use_module(plai(fixpo_ops), [collect_exported_completes/2]).
:- use_module(plai(domains),   [identical_proj/5, less_or_equal_proj/5, abs_sort/3]).
:- use_module(library(read),   [read/2]).
:- use_module(library(write),  [writeq/2]).
:- use_module(library(system), [modif_time/2, modif_time0/2, file_exists/1,
	working_directory/2]).
:- use_module(library(filenames),  [basename/2]).
:- use_module(library(aggregates), [findall/3, setof/3, '^'/2]).
:- use_module(spec(modular_spec), [dyn_abs_spec/5]).
:- use_module(library(compiler(c_itf)), 
	[false/1, process_files_from/7, uses/2, includes/2]).
:- use_module(library(assertions(c_itf_props))).
:- use_module(library(lists), [append/3]).
:- use_module(typeslib(dumper), 	
	[ 
	    acc_auxiliary_info/2,
	    dump_auxiliary_info/1,
	    imp_auxiliary_info/4,
	    restore_auxiliary_info/2
	]).

:- use_module(library(ctrlcclean), [ctrlc_clean/1]).
:- use_module(library(errhandle), [error_protect/1]).  
:- use_module(program(aux_filenames), 
	[
	    get_module_filename/3,
	    just_module_name/2,
	    is_library/1,
	    get_loaded_module_name/3
	]).
:- use_module(program(clidlist), [atom2data/5]).
:- use_module(spec(s_simpspec), [make_atom/2]).
:- use_module(library(prolog_sys), [statistics/2]).

% :- doc(bug,"Fast read/write must replace term-read/write.").

:- doc(bug,"auxiliary files version must be handled correctly.").

:- doc(bug,"Success information cannot be multivariant.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% JUST FOR TESTING %%%%%%%%%%%%%%%%%%%%%%
:- use_module(library(fastrw), [fast_read/1, fast_write/1, fast_read/2, fast_write/2]).
%% :- set_prolog_flag(multi_arity_warnings, off).
%% fast_read(X):-
%% 	read(X),
%% 	X \== end_of_file.
%% fast_write(X):-
%% 	writeq(X),display('.'),nl.
%% 
%% fast_read(Stream,X):-
%% 	read(Stream,X),
%% 	X \== end_of_file.
%% fast_write(Stream,X):-
%% 	writeq(Stream,X),display(Stream,'.'),nl(Stream).
%% :- set_prolog_flag(multi_arity_warnings, on).

%% --------------------------------------------------------------------

:- pred registry(Key,Module,Registry) : atm * atm * reg_type

# "Data predicate to locally store information about the registry of
  one or several modules. @var{Module} is the name of the module for
  which @var{Registry} is an entry in the registry file. It
  corresponds to the @em{global answer table} as it is described in
  @cite{mod-an-spec_tr_2003}, or other auxiliary information (e.g.,
  types).".

:- data registry/3.

%% --------------------------------------------------------------------

:- pred typedef(Module,TypeDef) : atm * term

# "Data predicate to locally store information about the types used in
  the registry of one or several modules. @var{Module} is the name of
  the module for which the type definition @var{TypeDef} is referenced
  in the registry file. The original definition of @var{TypeDef} may
  not reside in @var{Module}, but in a related module.".

:- data typedef/2.

/*
:- pred typedef_already_loaded(Module) : atm

# "Succeeds if the type definitions for module @var{Module} have been
  already uploaded to ciaopp (by means of @code{dumper} predicates).".

:- data typedef_already_loaded/1.
*/
%% --------------------------------------------------------------------

:- regtype reg_type/1.

reg_type(registry(_AbsInt,_Sg,_Call,_Succ,_Spec,_Imdg,_Mark)).

%% --------------------------------------------------------------------

:- pred registry_headers(Module,HeaderTerm) : atm * term 

# "@var{HeaderTerm} is a term read from the registry header of module
  @var{Module}. Data predicate to store the header terms of every
  registry file read. The list of registry header terms depends on the
  registry file version, and is stored in
  @tt{registry_header_format/2}".

:- data registry_headers/2.

%% --------------------------------------------------------------------

:- pred imported_modules(Module,Base) : atm * atm

# "List of imported modules".

:- data imported_modules/2.

%% --------------------------------------------------------------------

:- pred caller_modules(Module,BaseName) : atm * atm

# "List of caller modules to be processed.".

:- data caller_modules/2.

%% --------------------------------------------------------------------

:- pred changed_modules(Module,Base,SourceModule,Mode,WhenModuleChanged,RequireReanalysis) 

# "List of modules changed when updating the analysis results in the
  registry. Registry information of @var{Module} with basename
  @var{Base} has been changed as a result of analyzing
  @var{SourceModule}. @var{Mode} represents the relationship between
  @var{Module} and @var{SourceModule}, and it can be @code{imported},
  @code{caller} or @code{current}.

  @var{WhenModuleChanged} indicates in which run of the analysis of
  @var{SourceModule} the registry information of @var{Module}
  changed. This argument can be instantiated to values @code{current}
  (modules whose information changed as result of the last analysis
  run) or @code{previous} (modules whose information changed as result
  of a previous analysis run).

  @var{RequireReanalysis} indicates that the module should be
  reanalyzed. This is only useful in the context of
  @pred{gen_registry_info/3-4}.".

:- data changed_modules/6.

%% --------------------------------------------------------------------

:- pred external_reachable(SgImpKey,AbsInt,SgExp,CallExp,SgImp,CallImp)

# "List of pairs (exported call pattern, imported call pattern) for a
  given domain for computing the intermodular dependency graph.".

:- data external_reachable/6.

%% --------------------------------------------------------------------

:- pred reg_version(Version) : atm
# "Contains a version number which identifies
   the registry files associated with this version of the assertion
   library. Should be changed every time changes are made which render
   registry files incompatible, since this forces recomputation
   of all such files.".

reg_version('2.3').

%reg_extension('.reg').  %% moved to in p_asr

%% --------------------------------------------------------------------

:- pred registry_header_format(Version,TermList) : atm * list(term) 

# "@var{TermList} is the list of terms which must appear in the header
   of version @var{Version} registry files, excluding the version
   number term itself.".

registry_header_format('1.0',[]).
registry_header_format('2.0',
	[ pl_date(_),         %% Date of the .pl file to which refers the .reg file.
	  already_analyzed(_) %% List of domains for which module entries have been analyzed.
	]).
registry_header_format('2.1',
	[ pl_date(_),         %% Date of the .pl file to which refers the .reg file.
	  already_analyzed(_),%% List of domains for which module entries have been analyzed
	  open_mode(_)        %% Mode in which the .reg file is opened (read_write, read_only).
	]).
registry_header_format('2.2',
	[ pl_date(_),         %% Date of the .pl file to which refers the .reg file.
	  entries_already_analyzed(_),%% List of domains for which module entries have been analyzed
	  open_mode(_)        %% Mode in which the .reg file is opened (read_write, read_only).
	]).
registry_header_format('2.3',
	[ pl_date(_),         %% Date of the .pl file to which refers the .reg file.
	  entries_already_analyzed(_),%% List of domains for which module entries have been analyzed
	  open_mode(_)        %% Mode in which the .reg file is opened (read_write, read_only).
	]).

%% ********************************************************************
%% ********************************************************************

:- pred cleanup_p_abs 

# "Cleans up internal data predicates.".

cleanup_p_abs:-
	retractall_fact(imported_modules(_,_)),
	retractall_fact(caller_modules(_,_)),
	retractall_fact(external_reachable(_,_,_,_,_,_)),
	retractall_fact(module_is_processable_cache(_,_,_)),
%	retractall_fact(typedef_already_loaded(_)),
	move_last_changes_to_previous.

%% --------------------------------------------------------------------

move_last_changes_to_previous:-
	current_fact(changed_modules(Mod,Base,CurrModule,Mode,current,ReqReanalysis),Ref),
	erase(Ref),
	( current_fact(changed_modules(Mod,Base,CurrModule,Mode,previous,ReqReanalysis)) ->
	  true
	; asserta_fact(changed_modules(Mod,Base,CurrModule,Mode,previous,ReqReanalysis))
	),
	fail.
move_last_changes_to_previous.

%% --------------------------------------------------------------------

:- pred cleanup_p_abs_all

# "Cleans up all the data predicates.".

cleanup_p_abs_all:-
	cleanup_registry(_),
	retractall_fact(changed_modules(_,_,_,_,_,_)),
	cleanup_p_abs.

%% --------------------------------------------------------------------

cleanup_registry(Module):-
	retractall_fact(registry(_,Module,_)),
	retractall_fact(registry_headers(Module,_)),
	retractall_fact(typedef(Module,_)),
%	retractall_fact(typedef_already_loaded(Module)).
	true.

%% ********************************************************************
%% ********************************************************************
%% For compatibility. 
%:- reexport(plai(entry_policy),[call_pattern/3]).
%:- reexport(plai(success),[succ_pattern/4]).

%% --------------------------------------------------------------------

:- pred get_imported_modules 

# "Gets the list of imported modules from the current module. This list is
  obtained from the itf information of the current module, and is stored in
  @tt{imported_modules/1}.".

get_imported_modules:-
	current_fact(imported_modules(_,_)), !.
get_imported_modules:-
	current_itf(imports,_Sg,Module0), 
	just_module_name(Module0,Module), %%% *just* the module name, no prefixes allowed.
	get_loaded_module_name(Module,_AbsFile,AbsBase),
	add_imported_module(Module,AbsBase),
	fail.
get_imported_modules.

%% --------------------------------------------------------------------

:- pred get_caller_modules

# "Gets the list of caller modules to the current modules. This list is
  obtained from the registry information for the current modules, and
  is stored in @tt{caller_modules/2}.".

get_caller_modules:-
	current_fact(caller_modules(_,_)), !.
get_caller_modules:-
	curr_file(_F,CurrModule),
	current_fact(registry(_,CurrModule,registry(_AbsInt,_Sg,_Call,_Succ,_Spec,ImdgList,_Mark))),
	get_module_names(ImdgList,Modules,Bases),
	add_caller_modules(Modules,Bases),
	fail.
get_caller_modules.

get_module_names([],[],[]).
get_module_names(['$query'|Imdgs],Ms,Bases):-
	get_module_names(Imdgs,Ms,Bases).
get_module_names([(_SgCaller,_Caller,Base)|Imdgs],[M|Ms],[Base|Bases]):-
	just_module_name(Base,M),
	get_module_names(Imdgs,Ms,Bases).

%% ********************************************************************
%% ********************************************************************

:- pred gen_registry_info(+Verb,-Callers,-Imported)

# "Obtains from analysis internal structures the information on
  exported predicates regarding the current module and related
  modules. Returns in @var{Callers} and @var{Imported} the list of
  basenames of related modules whose registry information has been
  updated.".

gen_registry_info(Verb,Callers,Imported):-
	get_imported_modules,
	read_registry_current_files(Verb),
	read_registry_imported_files(Verb), %% slow!
	get_caller_modules,  %% AFTER reading current files registries!!
	read_registry_caller_files(Verb),   %% slow?
	get_imported_calls(ImportedCalls),
	compute_external_reachability(ImportedCalls),
	update_imported(ImportedCalls),
	update_current_files(Verb),
%% Only modules changed in last run must be returned.
	get_imported_changed_modules(Imported),
	get_caller_changed_modules(Callers),
	unmark_typedefs_already_loaded,
	!.

%% --------------------------------------------------------------------

:- pred gen_registry_info(+Verb,-Callers,-Imported,-Info)

# "As @pred{gen_registry_info/3}, but also returns @var{Info}.".

gen_registry_info(Verb,Callers,Imported,[time(T2,[])]):-
	statistics(runtime,[T0,_]),
	gen_registry_info(Verb,Callers,Imported),
	statistics(runtime,[T1,_]),
	T2 is T1-T0,
	verb_message(Verb,['{Generated registry in ',T2,' msec.}']).

%% --------------------------------------------------------------------

unmark_typedefs_already_loaded:-
%	retractall_fact(typedef_already_loaded(_)).
	true.

%% --------------------------------------------------------------------

get_imported_changed_modules(Imported):-
	findall(Base,(
			 curr_file(_,CurrModule),
			 changed_modules(M,Base,CurrModule,imported,current,y),
			 \+ curr_file(_,M),
	                 module_is_processable(Base)
		     ),Imported).

%% --------------------------------------------------------------------

get_caller_changed_modules(Callers):-
	findall(Base,(
			 curr_file(_,CurrModule),
			 changed_modules(M,Base,CurrModule,caller,current,y),
			 \+ curr_file(_,M)
		     ),Callers).

%% --------------------------------------------------------------------

read_registry_current_files(Verb):-
	curr_file_base(CurrBase,CurrModule),
	read_registry_file(CurrModule,CurrBase,Verb),
	fail.
read_registry_current_files(_Verb).

%% ====================================================================

:- doc(bug,"Currently read_registry_imported_files/1 and
	read_registry_caller_files/1 read reg files for all imported
	and caller modules. an interesting improvement could be to
	read only those files of modules for which we have new call
	patterns").

read_registry_imported_files(Verb):-
	current_fact(imported_modules(IM,Base)),
	read_registry_file(IM,Base,Verb),
	fail.
read_registry_imported_files(_Verb).

read_registry_caller_files(Verb):-
	caller_modules(CM,Base),
%%	current_itf(defines_module,CM,Base),
	read_registry_file(CM,Base,Verb),
	fail.
read_registry_caller_files(_Verb).

%% ====================================================================

:- pred update_current_files(+Verb)

# "This predicate updates the registry of the current modules. If the
  current modules source files have been modified after generating the
  registry, all its results are discarded, generating a new
  registry. If not, the registries are updated modifying only those
  entries for which we have better results.".

update_current_files(Verb):-
	curr_file_base(Base,CurrModule),
	update_current_registry(Base,CurrModule,Verb),
	update_registry_header_pl_date(CurrModule,Base),
	fail.
update_current_files(_Verb).

%% --------------------------------------------------------------------

:- pred update_current_registry(+CurrBase,+CurrModule,+Verb)

# "This predicate updates the registry of the current module. If the
  current module has been modified after generating the registry, all
  its results are discarded, generating a new registry. If not, the
  file is updated modifying only those entries for which we have
  better results. Type definitions are replaced accordingly.".

update_current_registry(Base,CurrModule,_Verb):-
	current_fact(registry(SgKey,CurrModule,registry(AbsInt,Sg,Call,OldSucc,SpecName,ImdgList,OldMark)),Ref),
	functor(Sg,F,A),
	functor(SgComplete,F,A), % direct access to predicate.
	current_fact(complete(SgKey,AbsInt,SgComplete,CallComplete,[Succ],_Id,_)), %access by SgKey
	abs_sort(AbsInt,CallComplete,CallComplete_s),
	abs_sort(AbsInt,Call,Call_s),
	identical_proj(AbsInt,SgComplete,CallComplete_s,Sg,Call_s),
%% If the entry has been reanalyzed, it is unmarked in registry.
	erase(Ref),
	assertz_fact(registry(SgKey,CurrModule,
	             registry(AbsInt,SgComplete,CallComplete,Succ,SpecName,ImdgList,unmarked))),
	add_changed_module(CurrModule,Base,CurrModule,current),
	%% If success info has changed, callers must be marked for reanalysis or invalidated.
	abs_sort(AbsInt,Succ,Succ_s),
	abs_sort_nonfree(AbsInt,OldSucc,OldSucc_s),
	update_mem_usage,
	current_pp_flag(success_policy,SP),
	( 
	    nonvar(OldSucc_s), 
	    identical_proj(AbsInt,SgComplete,Succ_s,Sg,OldSucc_s) ->
	    ( 
		not_valid_mark(SP,OldMark) ->
%jcf-26.11.2004 (study this!!!)	    ,current_pp_flag(intermod, Imod),
%jcf-26.11.2004	    Imod \== auto ->   %% oops, only with manual_analyze!!!!
		may_be_improved_mark(SP,CallersMark),
		mark_callers_registry(AbsInt,CurrModule,ImdgList,CallersMark,_)
            ;
	        true
            )
	; 
	    compare_and_get_mark(SP,AbsInt,SgComplete,Succ_s,Sg,OldSucc_s,CallersMark),
	    mark_callers_registry(AbsInt,CurrModule,ImdgList,CallersMark,_)
	),
	fail.
update_current_registry(_Base,CurrModule,_Verb):-
	update_current_typedefs(CurrModule),
	!.

%% --------------------------------------------------------------------

%% NOTE: This predicate decides when to set invalid marks, depending on the success policy.
compare_and_get_mark(SP,_AbsInt,_SgComplete,_Succ_s,_Sg,OldSucc_s,Mark):-
%% This case should not occur.
	var(OldSucc_s), !,
	may_be_improved_mark(SP,Mark).
compare_and_get_mark(_SP,AbsInt,SgComplete,Succ_s,Sg,OldSucc_s,'+'):-
	less_or_equal_proj(AbsInt,SgComplete,Succ_s,Sg,OldSucc_s),
	!.
compare_and_get_mark(_SP,_AbsInt,_SgComplete,_Succ_s,_Sg,_OldSucc_s,'-').

%% ********************************************************************
%% ********************************************************************

:- pred not_valid_mark(?SP,?Mark) 

# "Succeeds if a registry entry marked with @var{Mark} cannot be used
  when the success policy @var{SP} is applied.".

not_valid_mark(SP,Mark):-
	may_be_improved_mark(SP,OppositeMark),
	opposite_mark(OppositeMark, Mark).

opposite_mark('-','+').
opposite_mark('+','-').

:- pred may_be_improved_mark(?SP,?Mark) 

# "Succeeds if a registry entry marked with @var{Mark} can be used
when the success policy @var{SP} is applied, and the analysis results
can be improved by reanalysing the module.".

may_be_improved_mark(over_first,'+').
may_be_improved_mark(over_best,'+').
may_be_improved_mark(over_all,'+').
may_be_improved_mark(top,'+').
may_be_improved_mark(under_first,'-').
may_be_improved_mark(under_best,'-').
may_be_improved_mark(under_all,'-').
may_be_improved_mark(bottom,'-').
may_be_improved_mark(bottom_up,'+').  % Is this right?

%% --------------------------------------------------------------------

:- data tmp_current_module/1.

update_current_typedefs(CurrModule):-
	retractall_fact(typedef(CurrModule,_TypeDef)),
%	retractall_fact(typedef_already_loaded(CurrModule)),
	set_fact(tmp_current_module(CurrModule)),
 	current_fact(registry(_,CurrModule,registry(AbsInt,_Sg,Call,Succ,_SpecName,ImdgList,_Mark))),
	get_imdg_asubs(ImdgList,ImdgASubList),
	dumper:acc_auxiliary_info(AbsInt,[Call,Succ|ImdgASubList]),
	fail.
update_current_typedefs(_CurrModule):-
	dumper:dump_auxiliary_info(store_typedef).

add_imported_typedefs(AbsInt,Module,ASubs):-
%	retractall_fact(typedef_already_loaded(Module)),
	set_fact(tmp_current_module(Module)),
	dumper:acc_auxiliary_info(AbsInt,ASubs),
	dumper:dump_auxiliary_info(store_typedef), !.

%%%
store_typedef(TypeDef):-
	current_fact(tmp_current_module(CurrModule)),
	(
	    %%%NOTE: TypeDef comparison should be smarter!!!!!!!
	    current_fact(typedef(CurrModule,TypeDef)) ->
	    true
	;
	    asserta_fact(typedef(CurrModule,TypeDef))
	).
	
%% --------------------------------------------------------------------

%% If substitution is a free var, there is nothing to sort.
abs_sort_nonfree(_AbsInt,Sub1,Sub1):-
	var(Sub1), !.
abs_sort_nonfree(AbsInt,Sub1,Sub2):-
	abs_sort(AbsInt,Sub1,Sub2).

%% --------------------------------------------------------------------

:- pred mark_callers_registry(+AbsInt,+CurrModule,+ImdgList,+NewMark,-BasenamesMarked)

# "Entries of callers entries in @var{ImdgList} are marked with
  @var{NewMark} (if it is greater than their current mark).".

mark_callers_registry(_AbsInt,_CurrModule,[],_NewMark,[]).
mark_callers_registry(AbsInt,CurrModule,['$query'|Imdgs],NewMark,BasenamesMarked):-
	mark_callers_registry(AbsInt,CurrModule,Imdgs,NewMark,BasenamesMarked), !.
mark_callers_registry(AbsInt,CurrModule,[(SgCaller,Caller,Base)|Imdgs],NewMark,BasenamesMarked):-
	current_pp_flag(success_policy,SP),
	just_module_name(Base,M),
	read_registry_file(M,Base,quiet),  %% just in case. If it is already loaded, it does nothing.
	functor(SgCaller,F,A),
	functor(Sg,F,A),  % direct access to predicate.
	make_atom([F,A],SgKey),
	( current_fact(registry(SgKey,M,registry(AbsInt,Sg,Call,Succ,Spec,ImdgList,OldMark)),Ref),
	  abs_sort(AbsInt,Caller,Caller_s),
	  abs_sort(AbsInt,Call,Call_s),
	  identical_proj(AbsInt,SgCaller,Caller_s,Sg,Call_s) ->
	  ( less_or_equal_mark(SP,NewMark,OldMark) ->
	    BasenamesMarked = BasenamesMarked0
%% ,writeq(no_mark_caller(registry(SgKey,M,registry(AbsInt,Sg,Call,Succ,Spec,ImdgList,NewMark)))),nl
	  ; erase(Ref),
	    BasenamesMarked = [Base|BasenamesMarked0],
	    assertz_fact(registry(SgKey,M,registry(AbsInt,Sg,Call,Succ,Spec,ImdgList,NewMark)))
%% ,writeq(caller(registry(SgKey,M,registry(AbsInt,Sg,Call,Succ,Spec,ImdgList,NewMark)))),nl
	  ),
	  add_changed_module(M,Base,CurrModule,caller,y)
	; true
%% ,writeq(no_mark_caller(registry(SgKey,M,registry(AbsInt,Sg,Call,Succ,Spec,ImdgList,NewMark)))),nl
	),
	update_mem_usage,
	mark_callers_registry(AbsInt,CurrModule,Imdgs,NewMark,BasenamesMarked0), !.

%% ********************************************************************
%% ********************************************************************

:- pred update_spec_info(+File,-Changed)

# "Updates the information about version names of specialized predicates in @var{File}.".

update_spec_info(File,Changed):-
	absolute_file_name(File,'_opt','.pl','.',_,AbsBase,_),
	just_module_name(AbsBase,Module),
	read_registry_file(Module,AbsBase,quiet),
	update_current_registry_spec_info(AbsBase,Module,Changed).

:- data changed/1.

update_current_registry_spec_info(Base,CurrModule,_Changed):-
	set_fact(changed(no)),
	publish_pred_name(PredName,PredArity),
	functor(Sg,PredName,PredArity),
	make_atom([PredName,PredArity],SgKey),
	current_fact(registry(SgKey,CurrModule,registry(AbsInt,Sg,Call,Succ,SpecName,ImdgList,Mark)),Ref),
	get_version_name(AbsInt,Sg,Call,NewSpecName),
	SpecName \== NewSpecName,
	erase(Ref),
	set_fact(changed(yes)),
	assertz_fact(registry(SgKey,CurrModule,
	             registry(AbsInt,Sg,Call,Succ,NewSpecName,ImdgList,Mark))),
	add_changed_module(CurrModule,Base,CurrModule,current),
	fail.
update_current_registry_spec_info(_Base,_Module,Changed):-
	current_fact(changed(Changed)).

%% ********************************************************************
%% ********************************************************************

:- pred get_spec_info_imported

# "Gets the information about version names of specialized predicates
  belonging to the list of imported modules from the current module,
  and puts them into the specializer's abstract executability table.".

get_spec_info_imported:-
	get_imported_modules,         %% just in case.
	findall(M,current_fact(imported_modules(M,_)),Modules),
	get_spec_info(Modules).

%% --------------------------------------------------------------------

get_spec_info([]).
get_spec_info([Module|Modules]):-
	current_itf(defines_module,Module,Base),
	just_module_name(Base,Module),
	read_registry_file(Module,Base,quiet),
	retractall_fact(dyn_abs_spec(Module,_,_,_,_)),
	get_spec_info_one_module(Module,SpecList),
	sort_spec_info_one_module(SpecList,SortedSpecList),
	assert_spec_info_one_module(Module,SortedSpecList),
	get_spec_info(Modules).

%% --------------------------------------------------------------------

get_spec_info_one_module(Module,NameList):-
	findall( (AbsInt,Sg,Proj,SpecName), 
	    ( current_fact(registry(_,Module,registry(AbsInt,Sg,Proj,_Succ,SpecName,_ImdgList,unmarked))),
	      nonvar(SpecName)
	    ), NameList).

%% --------------------------------------------------------------------

sort_spec_info_one_module(L1,L2):-
	qsort(L1,L2,[]).

qsort([X|L],R,R2) :-
	partition(L,X,L1,L2),
        qsort(L2,R1,R2),
	qsort(L1,R,[X|R1]).
qsort([],R,R).

partition([],_,[],[]).
partition([E|R],C,[E|Left1],Right):- 
	spec_less(E,C), !,
	partition(R,C,Left1,Right).
partition([E|R],C,Left,[E|Right1]):-
	partition(R,C,Left,Right1).

%% --------------------------------------------------------------------

spec_less((AbsInt1,_Sg1,_Proj1,_SpecName1),(AbsInt2,_Sg2,_Proj2,_SpecName2)):-
	AbsInt1 @< AbsInt2, !.
spec_less((AbsInt,Sg1,_Proj1,_SpecName1),(AbsInt,Sg2,_Proj2,_SpecName2)):-
	functor(Sg1,F1,A1),
	functor(Sg2,F2,A2),
	(F1 @< F2 ; A1 < A2), !.
spec_less((AbsInt,Sg1,Proj1,_SpecName1),(AbsInt,Sg2,Proj2,_SpecName2)):-
	abs_sort(AbsInt,Proj1,Proj1_s),
	abs_sort(AbsInt,Proj2,Proj2_s),
	less_or_equal_proj(AbsInt,Sg1,Proj1_s,Sg2,Proj2_s),
	\+ identical_proj(AbsInt,Sg1,Proj1_s,Sg2,Proj2_s), !.

%% --------------------------------------------------------------------

assert_spec_info_one_module(_,[]).
assert_spec_info_one_module(Module,[ (AbsInt,Sg,Proj,SpecName) |SpecList]):-
	assertz_fact(dyn_abs_spec(Module,Sg,AbsInt,Proj,SpecName)),
	assert_spec_info_one_module(Module,SpecList).


%% ********************************************************************
%% ********************************************************************

:- set_prolog_flag(multi_arity_warnings,off).

:- pred save_registry_info(+Verb,-Info) : atm * term

# "Writes on disk the registry information of the modules loaded into
  Ciaopp or related to modules loaded into Ciaopp. This information is
  updated by @pred{gen_registry_info/3}. This predicate must be called
  after performing intermodular preprocessing (analysis,
  specialization...), even if @pred{save_registry_info/3} has been
  used.".

save_registry_info(Verb,[time(T,[])]):-
	%ALL changed modules MUST be saved!
	statistics(runtime,_),
	( setof(Base,M^M2^Mode^All^Req^(
				     current_fact(changed_modules(M,Base,M2,Mode,All,Req)),
				     module_is_processable(Base)
			       ),ML) ->
	  write_registry_files(ML,Verb),
	  retract_saved_files(ML)
	; true
	),
	statistics(runtime,[_,T]),
	verb_message(Verb,['{Saved registry in ',T,' msec.}']),
	update_mem_usage,
	!.

%% ------------------------------------------------------------------

:- pred save_registry_info(+Verb,+CurrBase,-Info) : atm * atm * term

# "Writes on disk the registry information of the modules related to
  the current module (@var{CurrBase}) which have been modified. This
  information is updated by @pred{gen_registry_info/3}. Even if this
  predicate is used, @pred{save_registry_info/2} must be used after
  performing intermodular preprocessing.".

save_registry_info(Verb,CurrBase,[time(T,[])]):-
	statistics(runtime,_),
	just_module_name(CurrBase,CurrModule),
 	( setof(Base,Mode^M^All^Req^(
 			    current_fact(changed_modules(M,Base,CurrModule,Mode,All,Req)),
			    module_is_processable(Base)
 			),ML) ->
	  write_registry_files(ML,Verb),
	  retract_saved_files(ML)
	; true
	),
	statistics(runtime,[_,T]),
	verb_message(Verb,['{Saved registry in ',T,' msec.}']),
	update_mem_usage,
	!.

:- set_prolog_flag(multi_arity_warnings,on).

%% ------------------------------------------------------------------

write_registry_files([],_Verb).
write_registry_files([Base|BList],Verb):-
	just_module_name(Base,M),
	write_registry_file(Base,M,Verb),
	write_registry_files(BList,Verb).

%% ------------------------------------------------------------------

retract_saved_files([]).
retract_saved_files([Base|Bases]):-
	retractall_fact(changed_modules(_,Base,_,_,_,_)),
	retract_saved_files(Bases).
	

%% ====================================================================

:- pred update_imported/1

# "Determines if the registry of imported modules must be
  regenerated, and updates it in memory accordingly.".

:- data deleted_imdg_entries/3.

update_imported(ICalls):-
	domain(AbsInt),
%jcf Following two lines delete registry entries not used any longer. 
%jcf To enable that again, just uncomment them (the first line 
%jcf introduces an important slowdown in this predicate; in case this 
%jcf must be reintroduced, a more efficient implementation should be 
%jcf studied).
%	current_fact(imported_modules(IM,Base)),
%	delete_imported_imdg_not_used(AbsInt,IM),
%jcf
	%% Add/update changed dependencies with imported modules.
	update_imported_imdg(AbsInt,ICalls),
	fail.
update_imported(_ICalls).

:- pred get_imported_calls(-ICalls) 

# "Returns a list of tuples (@var{IM},@var{IMBase},@var{Sg}), where
  @var{Sg} is an imported call of module @var{IM} (basename
  @var{IMBase}).  Only the imported calls from processable modules are
  considered.".

get_imported_calls(ICalls):-
	(
	    setof( (File,Sg), (
				  current_itf(imports,Sg,File)
			     ), ICalls0) ->
	    remove_duplicates(ICalls0,ICalls1),
	    get_module_names_bases(ICalls1,ICalls)
	;
	    ICalls = []
	).

:- set_prolog_flag(multi_arity_warnings,off).
:- use_module(library(sort)).
%% Tries to remove the calls that unify, but that are not removed by setof/3.
remove_duplicates(L1,L2):-
	sort(L1,L11),
	remove_duplicates(L11,[],L2).

remove_duplicates([],L,L).
remove_duplicates([X|Xs],[X|Ys],L):-
	remove_duplicates(Xs,[X|Ys],L), !.
remove_duplicates([X|Xs],Ys,L):-
	remove_duplicates(Xs,[X|Ys],L), !.
:- set_prolog_flag(multi_arity_warnings,on).

get_module_names_bases([],[]).
get_module_names_bases([(user,_Sg)|Xs],Ys):-
	!,
	get_module_names_bases(Xs,Ys).
get_module_names_bases([(File,Sg)|Xs],[(IM,IMBase,Sg)|Ys]):-
	just_module_name(File,IM),
	current_fact(imported_modules(IM,IMBase)),
	module_is_processable(IMBase),
	!,
	get_module_names_bases(Xs,Ys).
get_module_names_bases([(_File,_Sg)|Xs],Ys):-
	get_module_names_bases(Xs,Ys).

/*
delete_imported_imdg_not_used(AbsInt,IM):-
	%% select those reg entries in IM with imdg pointing to current module.
	current_fact(registry(SgKey,IM,registry(AbsInt,SgIm,CallIm,SuccIm,SpecIm,OldImdg,MarkIm)),Ref),
	delete_imported_imdg_not_used_(OldImdg,NewImdg,AbsInt,IM,SgIm,CallIm,Changed),
	( nonvar(Changed) ->
	  erase(Ref),
	  ( NewImdg = [] -> 
	    true
	  ; assertz_fact(registry(SgKey,IM,registry(AbsInt,SgIm,CallIm,SuccIm,SpecIm,NewImdg,MarkIm)))
	  )
	),
	fail.
delete_imported_imdg_not_used(_AbsInt,_IM).

%% --------------------------------------------------------------------

delete_imported_imdg_not_used_([],[],_AbsInt,_IM,_SgIm,_CallIm,_).
delete_imported_imdg_not_used_([(SgPrev,Prev,CurrBase)|OldImdg],NewImdg,AbsInt,IM,SgIm,CallIm,y):-
        can_be_deleted_imdg((SgPrev,Prev,CurrBase),AbsInt,SgIm,CallIm),
	!,
	just_module_name(CurrBase,CurrMod),
	current_itf(defines_module,IM,IMBase),
	add_changed_module(IM,IMBase,CurrMod,imported),
	delete_imported_imdg_not_used_(OldImdg,NewImdg,AbsInt,IM,SgIm,CallIm,_).
delete_imported_imdg_not_used_([ImdgEntry|OldImdg],[ImdgEntry|NewImdg],
	                       AbsInt,IM,SgIm,CallIm,Changed):-
        %% '$query' entries from entries are not deleted.
	delete_imported_imdg_not_used_(OldImdg,NewImdg,AbsInt,IM,SgIm,CallIm,Changed), !.

%% --------------------------------------------------------------------

%% There are three conditions which must hold to delete an imdg entry:
%% 1. The imdg entry refers to a current module, AND
%% 2. The imported call pattern is reachable from an exported 
%%    caller pattern (there is a path in the abstract and-or graph), AND
%% 3. The caller pattern has been analyzed actually.
can_be_deleted_imdg((SgIMCaller,IMCaller,CurrBase),AbsInt,SgIm,CallIm):-
        curr_file_base(CurrBase,_CurrMod),
	\+ imported_is_reachable(AbsInt,SgIm,CallIm,SgIMCaller,IMCaller),
	caller_has_been_analyzed(AbsInt,SgIMCaller,IMCaller).

imported_is_reachable(AbsInt,SgIm,CallIm,SgIMCaller,IMCaller):-
	abs_sort(AbsInt,CallIm,CallIm_s),
	get_exported_caller(AbsInt,SgIm,CallIm_s,SgCaller,Caller),
	abs_sort(AbsInt,Caller,Caller_s),
	abs_sort(AbsInt,IMCaller,IMCaller_s),
	identical_proj(AbsInt,SgIMCaller,IMCaller_s,SgCaller,Caller_s).

caller_has_been_analyzed(AbsInt,SgCaller,Caller):-
	functor(SgCaller,F,A),
	functor(SgCaller_n,F,A),  % direct access to predicate.
	abs_sort(AbsInt,Caller,Caller_s),
	current_fact(complete(_Key,AbsInt,SgCaller_n,Caller_n,_,_,_)),
	abs_sort(AbsInt,Caller_n,Caller_ns),
	identical_proj(AbsInt,SgCaller_n,Caller_ns,SgCaller,Caller_s).
*/

%% --------------------------------------------------------------------

update_imported_imdg(AbsInt,ICalls):-
%jcf%	get_imported_calls(IM,IMBase,Sg),
	member((IM,IMBase,Sg),ICalls),
	functor(Sg,F0,A0),
	functor(SgComp,F0,A0),
	make_atom([F0,A0],CompKey),
	current_fact(complete(CompKey,AbsInt,SgComp,Proj,[Prime],_Id,_)),
	abs_sort(AbsInt,Proj,Proj_s),
% writeq(before(get_exported_caller(AbsInt,SgComp,Proj_s,SgCaller,Caller))),nl,
        get_exported_caller(AbsInt,SgComp,Proj_s,SgCaller,Caller),
% writeq(after(get_exported_caller(AbsInt,SgComp,Proj_s,SgCaller,Caller))),nl,
%% the exported caller must be one of those stored in curr module's .reg, but not the imported module.
	get_module_from_sg(SgCaller,CurrModule0),
	CurrModule0 \== IM,
	curr_file_base(CurrBase,CurrModule),
	(
	    CurrModule0 == CurrModule
	;
	    CurrModule0 == multifile
	),
%%
	functor(SgCaller,F,A),
	functor(SgCaller_n,F,A),  % direct access to predicate.
	abs_sort(AbsInt,Caller,Caller_s),
	make_atom([F,A],SgKey),
	current_fact(registry(SgKey,CurrModule,registry(AbsInt,SgCaller_n,Caller_n,_,_,_,_))),
	abs_sort(AbsInt,Caller_n,Caller_ns),
	identical_proj(AbsInt,SgCaller_n,Caller_ns,SgCaller,Caller_s),
	%% imported registry entries are marked
	functor(SgComp,F2,A2),
	functor(SgOld,F2,A2),  % direct access to predicate.
	make_atom([F2,A2],SgKey2),
	( current_fact(registry(SgKey2,IM,registry(AbsInt,SgOld,ProjOld,_PrimeOld,SpecName,ImdgList,MarkOld)),Ref),
	  abs_sort(AbsInt,ProjOld,ProjOld_s),
	  identical_proj(AbsInt,SgOld,ProjOld_s,SgComp,Proj_s) ->
	  add_to_imdg_list(AbsInt,(SgCaller,Caller_s,CurrBase),ImdgList,NewImdgList,Added),
	  Added = y, %% only if there are changes in ImdgList. If not, this fails and backtracks.
%jcf-26.11.2004	  ( less_or_equal_mark(MarkOld,marked) -> NewMark = marked ; NewMark = MarkOld ),
	  NewMark = MarkOld,
%jcf-26.11.2004
	  erase(Ref),
	  assertz_fact(registry(SgKey2,IM,registry(AbsInt,SgComp,Proj_s,Prime,SpecName,NewImdgList,NewMark))),
% writeq(imported(registry(SgKey2,IM,registry(AbsInt,SgComp,Proj_s,Prime,SpecName,NewImdgList,NewMark)))),nl,
	  add_changed_module(IM,IMBase,CurrModule,imported,n),
	  add_imported_typedefs(AbsInt,IM,[Caller_s,Proj_s,Prime])
	; NewImdgList = [(SgCaller,Caller_s,CurrBase)],
	  current_pp_flag(success_policy,SP),
	  may_be_improved_mark(SP,NewMark),
	  assertz_fact(registry(SgKey2,IM,registry(AbsInt,SgComp,Proj_s,Prime,SpecName,NewImdgList,NewMark))),
% writeq(imported(registry(SgKey2,IM,registry(AbsInt,SgComp,Proj_s,Prime,SpecName,NewImdgList,marked)))),nl,
	  add_changed_module(IM,IMBase,CurrModule,imported,y),
	  add_imported_typedefs(AbsInt,IM,[Caller_s,Proj_s,Prime])
	),
	update_mem_usage,
	fail.
update_imported_imdg(_AbsInt,_ICalls).


%% --------------------------------------------------------------------

:- pred add_to_imdg_list(+AbsInt,+Caller,+OldList,-NewList,-Added) 

# "Adds an element @var{Caller} (formed by either a tuple
  (@var{SgCaller},@var{Caller_s},@var{Base}) or an atom
  @code{'$query'}) to @var{OldList}, a list of intermodular
  dependencies. @var{Added} will be instantiated to 'y' if
  @var{Caller} is actually added to @var{NewList}, and 'n' if it was
  already in @var{OldList}.".

add_to_imdg_list(_AbsInt,'$query',[],['$query'],y).
add_to_imdg_list(_AbsInt,'$query',['$query'|Is],['$query'|Is],n):- !.
add_to_imdg_list(AbsInt,'$query',[(Sg,Caller,Base)|Is],[(Sg,Caller,Base)|NIs],Added):-
	add_to_imdg_list(AbsInt,'$query',Is,NIs,Added),!.
add_to_imdg_list(_AbsInt,(Sg,Caller,Base),[],[(Sg,Caller,Base)],y).
add_to_imdg_list(AbsInt,(SgCaller,Caller_s,Base),Is,Is,n):-
	Is = [(SgCallerOld,CallerOld,Base)|_],
%added-4.11.2004
	functor(SgCaller,F,A),
	functor(SgCallerOld,F,A),  %% if they are different functors, this speeds up this process.
%
	abs_sort(AbsInt,CallerOld,CallerOld_s),
	identical_proj(AbsInt,SgCaller,Caller_s,SgCallerOld,CallerOld_s),!.
add_to_imdg_list(AbsInt,(SgCaller,Caller_s,Base),[I|Is],[I|NIs],Added):-
	add_to_imdg_list(AbsInt,(SgCaller,Caller_s,Base),Is,NIs,Added),!.
	

%% --------------------------------------------------------------------

:- pred get_exported_caller(+AbsInt,+Sg,+Proj_s,-SgCaller,-Caller).
%% external_reachable/6 is generated when generate_abs_files/1 execution is requested. 

get_exported_caller(AbsInt,Sg,Proj_s,SgCaller,Caller):-
	functor(Sg,F,A),
	functor(SgExt,F,A),  % direct access to predicate.
	make_atom([F,A],SgExtKey),
	current_fact(external_reachable(SgExtKey,AbsInt,SgCaller,Caller,SgExt,ProjExt)),
%%%%%%%	abs_sort(AbsInt,Proj,Proj_s),
	abs_sort(AbsInt,ProjExt,ProjExt_s),
	identical_proj(AbsInt,Sg,Proj_s,SgExt,ProjExt_s).

%% ********************************************************************
%% ********************************************************************

:- pred read_registry_file(+Module,+Base,+Verb) : atm * atm * atm

# " Reads the registry file of @var{Module} and loads it into
  @tt{registry/2}, erasing any previous registry information for that
  module. @var{Base} must be the absolute file name, but excluding
  file extension.".

read_registry_file(Module,_Base,_Verb):-
	check_registry_already_read(Module), !,
	upload_typedefs_all_domains(Module).
read_registry_file(Module,Base,Verb):-
	cleanup_registry(Module),
	get_module_filename(reg,Base,RegName),
	get_module_filename(pl,Base,PlName),
	( file_exists(RegName) ->
	  open(RegName, read, Stream),
	  ( read_registry_header(Verb,Module,Stream) ->
	    ( registry_up_to_date(Module,PlName) ->
	      ForceMark = unmarked  %% no mark at all.
	    ; 
%jcf-26.11.2004		ForceMark = invalid,
%jcf-26.11.2004		verb_message(Verb,['{Non-up-to-date file: ',RegName,'. All entries will be marked as invalid.}'])
		current_pp_flag(success_policy,SP),
		may_be_improved_mark(SP,ForceMark)
%jcf-26.11.2004
	    ),
	    current_input(CI),
	    set_input(Stream),
	    verb_message(Verb,['{Reading ',RegName]),
	    read_types_data_loop(Module,NextTuple),   % NextTuple is the tuple after the last type definition.
	    read_reg_data_loop(Module,NextTuple,ForceMark),
	    set_input(CI),
	    upload_typedefs_all_domains(Module),
	    verb_message(Verb,'}')
	  ; verb_message(Verb,['{Wrong version of file: ',RegName,'. It will be overwritten.}']),
	    create_registry_header(Module,PlName),
	    add_changed_module(Module,Base,Module,registry_created)
	  ),
	  close(Stream)
	; verb_message(Verb,['{Non-existing file: ',RegName,'}']),
	  create_registry_header(Module,PlName),
	  add_changed_module(Module,Base,Module,registry_created)
	), !.

%% --------------------------------------------------------------------

% Reads types from std. input. The last tuple read (immediately after the last type read) is 
% returned in NextTuple.
read_types_data_loop(Module,NextTuple):-
	retractall_fact(typedef(Module,_)),
%	retractall_fact(typedef_already_loaded(Module)),
	repeat,
	(
	    fast_read(NextTuple) ->
	    ( 
%		NextTuple = typedef(TypeName,TypeDef) ->
		is_type_related(NextTuple) ->		
		assertz_fact(typedef(Module,NextTuple)),
		fail
	    ; 
		true
	    )
	;
	    NextTuple = end_of_file
	).

is_type_related(typedef(_,_)).
is_type_related(param_type_symbol_renaming(_,_)).

%% --------------------------------------------------------------------

read_reg_data_loop(_,end_of_file,_).
read_reg_data_loop(Module,Registry,ForceMark) :-
	( 
	    Registry = registry(AbsInt,Sg,Call,Succ,SpecName,ImdgList,Mark) ->
	    current_pp_flag(success_policy,SP),
	    functor(Sg,F,A),
	    make_atom([F,A],SgKey),
	    ( 
		less_or_equal_mark(SP,Mark,ForceMark) ->
		assertz_fact(registry(SgKey,Module,registry(AbsInt,Sg,Call,Succ,SpecName,ImdgList,ForceMark)))
	    ; 
		assertz_fact(registry(SgKey,Module,registry(AbsInt,Sg,Call,Succ,SpecName,ImdgList,Mark)))
	    )
	; 
	    assertz_fact(registry(none,Module,Registry))  %% It has no key!!
	),
	(
	    fast_read(NextRegistry) ->
	    read_reg_data_loop(Module,NextRegistry,ForceMark)
	;
	    true
	).


%% --------------------------------------------------------------------

check_registry_already_read(Module):-
	current_fact(registry_headers(Module,_)).

%% ********************************************************************
%% ********************************************************************

:- pred upload_typedefs(+AbsInt,+Module) : atm * atm

# "Uploads into CiaoPP the types used by the registry entries of
  @var{Module} in domain @var{AbsInt}. @var{Module} registry info must
  be already loaded into memory. If the type information has been
  already uploaded, it is not loaded again.".

upload_typedefs(AbsInt,Module):-
	%%% uploading typedefs, and updating registry information with typedef renamings.
	set_fact(tmp_current_module(Module)),
	dumper:restore_auxiliary_info(restore_types,Dict),
	current_fact(registry(SgKey,Module,registry(AbsInt,Sg,Call0,Succ0,SpecName,ImdgList0,Mark)),Ref),
	get_imdg_asubs(ImdgList0,ImdgSubList0),
	dumper:imp_auxiliary_info(AbsInt,Dict,[Call0,Succ0|ImdgSubList0],[Call,Succ|ImdgSubList]),
	replace_imdg_subs(ImdgList0,ImdgSubList,ImdgList),
	erase(Ref),
	assertz_fact(registry(SgKey,Module,registry(AbsInt,Sg,Call,Succ,SpecName,ImdgList,Mark))),
	fail.
upload_typedefs(_AbsInt,Module):-
	%%% downloading typedef renamings again, and replacing typedef/2 definitions.
	update_current_typedefs(Module),
%	assertz_fact(typedef_already_loaded(Module)),
	!.
 	
upload_typedefs_all_domains(Module):-
	upload_typedefs(_AbsInt,Module).

% Returns the type definitions on backtracking from the temporary pred tmp_restore_types/1.
restore_types(TypeDef):-
	current_fact(tmp_current_module(Module)),
	retract_fact(typedef(Module,TypeDef)).

%% --------------------------------------------------------------------

% Given a list of imdg tuples, obtains the list of asubs for those imdg tuples.
get_imdg_asubs([],[]).
get_imdg_asubs(['$query'|Imdgs],Asubs):-
	get_imdg_asubs(Imdgs,Asubs).
get_imdg_asubs([(_Sg,Proj,_Base)|Imdgs],[Proj|Asubs]):-
	get_imdg_asubs(Imdgs,Asubs).

%% --------------------------------------------------------------------

% Replaces the asubs of a list of imdg tuples by a list of new asubs.
replace_imdg_subs([],[],[]).
replace_imdg_subs(['$query'|Imdgs0],Asubs,['$query'|Imdgs]):-
	replace_imdg_subs(Imdgs0,Asubs,Imdgs).
replace_imdg_subs([(Sg,_,Base)|Imdgs0],[Proj|Asubs],[(Sg,Proj,Base)|Imdgs]):-
	replace_imdg_subs(Imdgs0,Asubs,Imdgs).


%% --------------------------------------------------------------------

:- pred read_registry_header(Verb,Module,Stream) : atm * atm * stream 

# "Reads the header of @var{Module}'s registry file from @var{Stream},
  and stores it in the data predicate @tt{registry_header/2}. If the
  registry header is wrong, or it corresponds to a non-valid
  version, this predicate fails.".

read_registry_header(_Verb,Module,Stream):-
	read(Stream,v(V)),
	registry_header_format(V,HeaderTerms),
	read_registry_header_terms(Stream,Module,HeaderTerms).
read_registry_header(Verb,Module,_Stream):-
	current_itf(defines_module,Module,Base),
	verb_message(Verb,['{Wrong version or corrupted file header: ',Base,'}']),
	fail.

read_registry_header_terms(_Stream,_Module,[]).
read_registry_header_terms(Stream,Module,[H|Hs]):-
	fast_read(Stream,H),
	assertz_fact(registry_headers(Module,H)),
	read_registry_header_terms(Stream,Module,Hs).

%% --------------------------------------------------------------------

:- pred create_registry_header(+Module,+PlFileName) : atm * atm

# "Creates in memory a new registry header for @var{Module}. This
  predicate must be aware of the contents of the header and the order
  of the header terms (stored as terms in
  @pred{registry_header_format/2})".

create_registry_header(Module,PlFile):-
	retractall_fact(registry_headers(Module,_)),
	modif_time0(PlFile,Date),
	assertz_fact(registry_headers(Module,pl_date(Date))),
	assertz_fact(registry_headers(Module,entries_already_analyzed([]))),
	( (is_library(PlFile), \+current_pp_flag(process_libraries,on)) ->
	  assertz_fact(registry_headers(Module,open_mode(read_only)))
	; assertz_fact(registry_headers(Module,open_mode(read_write)))
	).

%% --------------------------------------------------------------------

update_registry_header_pl_date(Module,Base):-
	get_module_filename(pl,Base,FileName),
	modif_time(FileName, FileTime),
	retract_fact(registry_headers(Module,pl_date(_))),
	assertz_fact(registry_headers(Module,pl_date(FileTime))).

%% --------------------------------------------------------------------

:- pred write_registry_header(Module,Stream) : atm * stream 

# "Writes the header of @var{Module}'s registry file to @var{Stream}
  from the data predicate @tt{registry_headers/2}.".

write_registry_header(Module,Stream):-
	reg_version(V),
	writeq(Stream,v(V)),display(Stream,'.'),nl(Stream),
	registry_header_format(V,HeaderTerms),
	write_header_terms(Stream,Module,HeaderTerms).

write_header_terms(_Stream,_Module,[]).
write_header_terms(Stream,Module,[H|Hs]):-
	registry_headers(Module,H),
	fast_write(Stream,H),
	write_header_terms(Stream,Module,Hs).

%% ********************************************************************
%% ********************************************************************

:- pred reread_registry_file(+Module,+Base,+Verb)

# "Overwrites the registry file of @var{Module} in memory reading it
  from disk.".

reread_registry_file(Module,Base,Verb):-
	cleanup_registry(Module),
	read_registry_file(Module,Base,Verb).

%% ********************************************************************
%% ********************************************************************

:- pred get_all_modules(+TopLevelFile,-ModList) 

# "Obtains @var{ModList}, the list of modules in the program unit
  whose top-level module is @var{TopLevelFile}. This list is formed by
  the modules which appear in the top-down modular graph traversal
  with registries set to read_write mode (and including library
  modules if process_libraries flag is set to 'on'.".

get_all_modules(TopLevelFile,ModList):-
	absolute_file_name(TopLevelFile,'_opt','.pl','.',AbsFile,_,_),
	get_intermodule_graph(AbsFile,true),
	( 
	    setof( M , intermodule_list(M) , ModList)
 	; 
	    ModList = []
	),
	retractall_fact(intermodule_graph(_,_)),
	retractall_fact(intermodule_list(_)),
	retractall_fact(include_list(_)),
	retractall_fact(initial_vertex(_,_)),
	!.

:- pred get_all_modules(+TopLevelFile, -ModList, -IncludeList) 
	: filename(TopLevelFile) 
 => (list(ModList,filename), list(IncludeList,filename))

# "The same as @pred{get_all_modules/2}, but a list of included files
  is also returned.  This list includes not only files explicitly
  included with @code{:- include} declarations, but also packages
  used.".

get_all_modules(TopLevelFile,ModList,IncludeList):-
	absolute_file_name(TopLevelFile,'_opt','.pl','.',AbsFile,_,_),
	get_intermodule_graph(AbsFile,true),
	( 
	    setof( M , intermodule_list(M) , ModList)
 	; 
	    ModList = []
	),
	( 
	    setof( I , include_list(I) , IncludeList)
 	; 
	    IncludeList = []
	),
	retractall_fact(intermodule_graph(_,_)),
	retractall_fact(intermodule_list(_)),
	retractall_fact(include_list(_)),
	retractall_fact(initial_vertex(_,_)),
	!.

%% ********************************************************************
%% ********************************************************************

:- pred get_all_modules_depth(+TopLevelFile,-ModList) 

# "Given the top-level module of a program, @var{TopLevelFile},
  obtains in @var{ModList} the list of pairs (module,depth) with all
  modules in the modular graph with their maximal depth (without
  cycles).  All the modules in a cycle have the same depth.".

:- doc(bug,"not tested yet.").
get_all_modules_depth(TopLevelFile,ModList):-
	absolute_file_name(TopLevelFile,'_opt','.pl','.',AbsFile,AbsBase,_),
	get_intermodule_graph(AbsFile,true),
	compute_intermodule_graph_depth(AbsBase),
	( setof( (M,D) , ( 
			     current_fact(intermodule_list(M)),
			     current_fact(intermodule_graph_depth(M,D))
			 ) , ModList)
 	; ModList = []),
	retractall_fact(intermodule_graph_depth(_,_)),
	retractall_fact(intermodule_graph(_,_)),
	retractall_fact(intermodule_list(_)),
	retractall_fact(include_list(_)),
	retractall_fact(initial_vertex(_,_)).

%% ********************************************************************
%% ********************************************************************

:- pred get_all_module_cycles(+TopLevelFile,-CycleList) 

# "Obtains @var{CycleList}, the list of cycles in the program unit
  whose top-level module is @var{TopLevelFile}.  A cycle is a ciclic
  dependency in the module dependency graph.  Every element of
  @var{CycleList} is a list of the modules which belong to each cycle.
  Modules not belonging to any cycle are represented as one-element
  lists. @var{CycleList} is sorted as a post-order traversal of the
  inter-cycle dependency graph.

  The modules included in @var{CycleList} are those which appear in
  the top-down modular graph traversal with registries set to
  read_write mode (and including library modules if process_libraries
  flag is set to 'on'.)".

get_all_module_cycles(TopLevelFile,SortedCycleList):-
	absolute_file_name(TopLevelFile,'_opt','.pl','.',AbsFile,AbsBase,_),
	get_intermodule_graph(AbsFile,true),
	findall( vertex(M1,Ms,0,0,undef), initial_vertex(M1,Ms), Vertex),
	intermod_sccs(Vertex,CycleList),
	get_postorder_traversal(AbsBase,CycleList,SortedCycleList),
	retractall_fact(intermodule_graph(_,_)),
	retractall_fact(intermodule_list(_)),
	retractall_fact(include_list(_)),
	retractall_fact(initial_vertex(_,_)).

get_postorder_traversal(TopLevel,Cs,SortedCycles):-
	retractall_fact(module_scc(_,_)),
	retractall_fact(interscc_edge(_,_)),
	retractall_fact(scc_depth(_,_)),
	( 
	    Cs = [] ->
	    store_sccs([[TopLevel]],0)
	;
	    store_sccs(Cs,0)
	),
	gen_interscc_edges,
	current_fact(module_scc(TopLevel,STopLevel)),
	interscc_postorder_traversal(STopLevel,SortedCs),
	get_cycles(SortedCs,SortedCycles),
	retractall_fact(module_scc(_,_)),
	retractall_fact(interscc_edge(_,_)),
	retractall_fact(scc_depth(_,_)).

%% In order to get the postorder traversal, 
%% difference lists are used, represented by List-Tail structures.
interscc_postorder_traversal(STopLevel,PostOrder):-
	interscc_children_traversal(STopLevel,A-A,PostOrder-Tail),
	Tail = [STopLevel].
	
interscc_children_traversal(Node, PO0-T0, PO1-T1):-
	findall(Child, interscc_edge(Node,Child), Children),
	interscc_children_traversal_2(Children,PO0-T0, PO1-T1).

interscc_children_traversal_2([],PO-T,PO-T).
interscc_children_traversal_2([Node|Nodes], PO0-T0, POn-Tn):-
	( 
	    member_vartail(Node,PO0) ->
	    PO1-T2 = PO0-T0
	;
	    interscc_children_traversal(Node, PO0-T0, PO1-T1),
	    T1 = [Node|T2]
	),
	interscc_children_traversal_2(Nodes, PO1-T2, POn-Tn).


member_vartail(_Node,V):-
	var(V), !, fail.
member_vartail(Node,[Node|_]):- !.
member_vartail(Node,[_|Rest]):-
	member_vartail(Node,Rest).

get_cycles([],[]).
get_cycles([CssId|CssIds],[Cycle|SortedCycles]):-
	findall(M, module_scc(M,CssId), Cycle),
	get_cycles(CssIds,SortedCycles).


%% ********************************************************************
%% ********************************************************************

:- pred propagate_invalid_info(+AbsInt,+TopLevel,+BaseList) 

# "Marks as 'invalid' all registry entries in @var{BaseList} which
  transitively depend on invalid entries. ".
:- doc(bug,"Not tested yet. Perhaps it is useless").

propagate_invalid_info(AbsInt,TopLevel,BaseList):-
	get_invalid_modules_from_list(AbsInt,BaseList,InvalidBaseListNoForce),
	propagate_invalid_info_(TopLevel,InvalidBaseListNoForce), !.

%% --------------------------------------------------------------------

get_invalid_modules_from_list(_,[],[]).
get_invalid_modules_from_list(AbsInt,[Base|BaseList],[(Base,no_force)|InvalidBaseList]):-
	just_module_name(Base,Module),
	read_registry_file(Module,Base,quiet),
	\+ all_entries_valid(Base,AbsInt),
	!,
	get_invalid_modules_from_list(AbsInt,BaseList,InvalidBaseList).
get_invalid_modules_from_list(AbsInt,[_Base|BaseList],InvalidBaseList):-
	get_invalid_modules_from_list(AbsInt,BaseList,InvalidBaseList).


%% ********************************************************************
%% ********************************************************************

:- pred recover_from_invalid_state(+AbsInt,+TopLevel)

# "Checks if there is any invalid state in the program unit starting
  from @var{TopLevel}, and marks transitively as invalid all affected
  entries in any module of the program unit. This predicate is useful
  only in manual analysis of modular programs.".

recover_from_invalid_state(AbsInt,TopLevel):-
	get_invalid_modules(AbsInt,TopLevel,ModList),
	just_module_name(TopLevel,TopLevelModule),
	propagate_invalid_info_(TopLevelModule,ModList),!.

%% ********************************************************************
%% ********************************************************************

:- pred get_invalid_modules(+AbsInt,+TopLevelFile,-ModList)

# "Obtains in @var{ModList} the list of modules' basenames in the
  program unit which are in an invalid state, either if they have
  'invalid' entries in their .reg files, or their source code has
  changed since the last time they were analyzed.  If there is no .reg
  file for a module, its parents are added to @var{ModList}. Every
  element in @var{ModList} is a pair of the form
  (@var{Module},@var{Force}). If a source file has changed wrt since
  it was analyzed, or is the parent of a module with no .reg file,
  then @var{Force} is 'force', which means that all entries in the
  .reg file are invalid. The rest of the elements of @var{ModList}
  have 'no_force'.".

get_invalid_modules(AbsInt,TopLevelFile,ModList):-
	retractall_fact(module_to_analyze(_,_)),
	retractall_fact(module_to_analyze_parents(_)),
	absolute_file_name(TopLevelFile,'_opt','.pl','.',AbsFile,AbsBase,_),
	%
	get_intermodule_graph(AbsFile,check_registry_valid(AbsInt)),
	include_parents(AbsBase),
	( setof( (B,F) , ( 
			     current_fact(module_to_analyze(B,F))
			 ) , ModList)
 	; ModList = []),
	retractall_fact(module_to_analyze(_,_)),
	retractall_fact(module_to_analyze_parents(_)),
	retractall_fact(intermodule_graph(_,_)),
	retractall_fact(intermodule_list(_)),
	retractall_fact(include_list(_)),
	retractall_fact(initial_vertex(_,_)).

% Succeeds if the registry of Base is valid with respect to
% AbsInt: no registry entry is invalid, and the registry file exists.
check_registry_valid(Base,AbsInt):-
	just_module_name(Base,Module),
	cleanup_registry(Module),
	get_module_filename(pl,Base,PlName),
	get_module_filename(reg,Base,RegName),
	( 
	    file_exists(RegName) ->
	    ( 
		read_registry_file(Module,Base,quiet),
		registry_up_to_date(Module,PlName) ->
		(
		    all_entries_valid(Base,AbsInt) ->
		    true
		; 
		    add_module_to_analyze(Base,no_force)
		)
	    ;
		add_module_to_analyze(Base,force)
	    )
	; 
	    %% If there is no registry file, parent module is added
	    %% for analysis (as call patterns are needed for the imported module).
	    asserta_fact(module_to_analyze_parents(Base)),
	    %% Registry header must be created, anyway.
	    create_registry_header(Module,PlName),
	    add_changed_module(Module,Base,Module,registry_created)
	).

%% Succeeds if all entries of domain AbsInt are valid (marked or unmarked).
all_entries_valid(Base,AbsInt):-
	just_module_name(Base,Module),
	current_pp_flag(success_policy,SP),
	not_valid_mark(SP,Invalid),
	\+ current_fact(registry(_,Module,registry(AbsInt,_,_,_,_,_,Invalid))).

%% --------------------------------------------------------------------

:- pred propagate_invalid_info_(+TopLevelModule,+ModList)

# "Traverses the intermodule dependency graph, and transitively
  propagates invalid states to caller modules. @var{ModList} is a pair
  of the form (@var{Module},@var{Force}). If @var{Force} is
  @code{force}, all registry entries must be marked as invalid, and
  their caller modules invalidated accordingly. If @var{Force} is
  @code{no_force}, then only the invalid registry entries are
  propagated to the caller modules.  This is an internal predicate
  (the corresponding exported predicate is
  propagate_invalid_info/2).".

propagate_invalid_info_(_TopLevel,[]).
propagate_invalid_info_(TopLevel,[(Base,Force)|ModList]):-
	just_module_name(Base,Module),
	read_registry_file(Module,Base,quiet),
	(
	    Force = force ->  % All entries in the registry must be invalid.
	    current_pp_flag(success_policy,SP),
	    not_valid_mark(SP,Invalid),
	    mark_all_entries(Module,Invalid),
	    add_changed_module(Module,Base,TopLevel,transitive),
	    invalidate_callers(Module,ModList1)
	;
	    invalidate_callers(Module,ModList1)
	),
	append(ModList,ModList1,NewModList),
	propagate_invalid_info_(TopLevel,NewModList).

%%Marks all entries, even if they are of different domains.
mark_all_entries(Module,Mark):-
	retract_fact(registry(SgKey,Module,registry(AbsInt,Sg,Call,Succ,Spec,Imdg,_))),
	assertz_fact(registry(SgKey,Module,registry(AbsInt,Sg,Call,Succ,Spec,Imdg,Mark))),
	fail.
mark_all_entries(_Module,_Mark).

%%Marks as invalid all the entries corresponding to the callers of invalid 
%%entries in Module.
invalidate_callers(Module,ModList):-
	current_pp_flag(success_policy,SP),
	not_valid_mark(SP,Invalid),
	findall((AbsInt,Imdg),(
			 current_fact(registry(_,Module,registry(AbsInt,_Sg,_Call,_Succ,_Spec,Imdg,Invalid)))
		     ), ListOfImdgs),
	invalidate_callers_1(Module,ListOfImdgs,ModList).

invalidate_callers_1(_,[],[]).
invalidate_callers_1(Module,[(AbsInt,ImdgList)|ListOfImdgs],ModList):-
	current_pp_flag(success_policy,SP),
	not_valid_mark(SP,Invalid),
	mark_callers_registry(AbsInt,Module,ImdgList,Invalid,BasenamesMarked),
	include_no_force(BasenamesMarked,ModsMarked),
	invalidate_callers_1(Module,ListOfImdgs,ModList1),
	append(ModsMarked,ModList1,ModList).

include_no_force([],[]).
include_no_force([B|Bs],[(B,no_force)|Ms]):-
	include_no_force(Bs,Ms).

%% ********************************************************************
%% ********************************************************************

:- pred get_modules_to_analyze(+AbsInt,+TopLevel,-ModList) 

# "Obtains the list of modules to analyze. This list is formed by the modules
  which  have their .reg file outdated, or if the module is not
  completely analyzed (some of the entries in the .reg file are marked
  or invalid). For those modules which have no .reg file, the
  @em{parents} of the module are included in the list (as there are
  no call patterns for those modules without .reg file). 

  The structure of the elements in the list is a term
  (@var{Mod},@var{Depth},@var{Force}), where @var{Mod} stands for the
  module name, @var{Depth} is the maximum depth without cycles in the
  intermodular graph, and @var{Force} marks those modules which must
  be completely reanalyzed (only useful for the parents of the modules
  with no reg file).

  @var{AbsInt} can be either an abstract domain name or a list of
  abstract domains.".

get_modules_to_analyze(AbsInt,TopLevelFile,ModList):-
	retractall_fact(module_to_analyze(_,_)),
	retractall_fact(module_to_analyze_parents(_)),
	absolute_file_name(TopLevelFile,'_opt','.pl','.',AbsFile,AbsBase,_),
	%
	get_intermodule_graph(AbsFile,check_registry_up_to_date(AbsInt,AbsBase)),
	compute_intermodule_graph_depth(AbsBase),
	include_parents(AbsBase),
	( setof( (M,D,F) , ( 
			     current_fact(module_to_analyze(M,F)),
			     current_fact(intermodule_graph_depth(M,D))
			 ) , ModList)
 	; ModList = []),
	retractall_fact(module_to_analyze(_,_)),
	retractall_fact(module_to_analyze_parents(_)),
	retractall_fact(intermodule_graph_depth(_,_)),
	retractall_fact(intermodule_graph(_,_)),
	retractall_fact(intermodule_list(_)),
	retractall_fact(include_list(_)),
	retractall_fact(initial_vertex(_,_)).

% module_to_analyze(Module,ForceAnalysis).
:- data module_to_analyze/2.  
:- data module_to_analyze_parents/1.

% Succeeds if the registry of Base is up-to-date with respect to
% AbsInt. TopLevelBase is handled in a special way, as it is
% checked that the module entries have been added to .reg file.
%
% AbsInt can be either a domain name or a list of domains.
check_registry_up_to_date(Base,AbsInt,TopLevelBase):-
	just_module_name(Base,Module),
	cleanup_registry(Module),
	get_module_filename(pl,Base,PlName),
	get_module_filename(reg,Base,RegName),
	( file_exists(RegName) ->
	  ( read_registry_file(Module,Base,quiet),
	    registry_up_to_date(Module,PlName),
	    all_entries_unmarked(Base,AbsInt,TopLevelBase) ->
	    true
	  ; add_module_to_analyze(Base,no_force)
	  )
	; %% If there is no registry file, parents of this module are added
 	  %% for analysis (as call patterns are needed for the imported module).
	  asserta_fact(module_to_analyze_parents(Base))
	).

%% Converts module_to_analyze_parents(X) into
%% module_to_analyze(Y,force), where Y is parent of X.
include_parents(TopBase):-
	current_fact(module_to_analyze_parents(Base)),
	(
	    Base == TopBase -> %% TopLevel module must be added even if it has parents.
	    add_module_to_analyze(Base,force)
	;
	    true
	),
	( 
	    current_fact(intermodule_graph(_,Base)) ->
	    current_fact(intermodule_graph(Parent,Base)),
	    ( 
		module_to_analyze_parents(Parent) ->
		true
	    ;
		(
		    module_is_processable(Parent) ->
		    add_module_to_analyze(Parent,force)
		;
		    true
		)
	    )
	; %% if there are no parents, Base must be added.
	    (
		module_is_processable(Base) ->
		add_module_to_analyze(Base,force)
	    ;
		true
	    )
	),
	fail.
include_parents(_TopBase):-
	retractall_fact(module_to_analyze_parents(_)).

add_module_to_analyze(Base,Force):-
	( current_fact(module_to_analyze(Base,Force0),Ref) ->
	  ( Force = force, Force0 = no_force ->
	    erase(Ref),
	    asserta_fact(module_to_analyze(Base,force))
	  ; true
	  )
	; asserta_fact(module_to_analyze(Base,Force))
	).

% get_related_module(Base,IFile):-
% 	imports_pred(Base,IFile,_F,_A,_DefType,_Meta,_EndFile).

%% Succeeds if all entries are unmarked for AbsInt.
%% AbsInt can be either a domain name or a list of domains.
all_entries_unmarked(Base,AbsInt,TopLevelBase):-
	just_module_name(Base,Module),
	(
	    Base = TopLevelBase ->
	    current_fact(registry_headers(Module,entries_already_analyzed(Domains))),
	    list_member(AbsInt,Domains)
	;
	    true
	),
	\+ current_fact(registry(_,Module,registry(AbsInt,_,_,_,_,_,'+'))),
	\+ current_fact(registry(_,Module,registry(AbsInt,_,_,_,_,_,'-'))).

true(_).

% Succeeds when either first arg is an atom and it is in the list of the second
% arg, or if the intersection of both lists is not empty (it uses unification).
list_member([A|_As],Bs):-
	member(A,Bs).
list_member([_|As],Bs):-
	list_member(As,Bs).
list_member(A,Bs):-
	member(A,Bs).

%% ********************************************************************
%% INTERMODULAR GRAPH TRAVERSAL
%% ********************************************************************

:- pred intermodule_graph(Caller,Called) 

# "Module graph. It succeeds iff module with basename @var{Caller}
  imports module with basename @var{Called}.".

:- data intermodule_graph/2. 

:- data intermodule_list/1. 

:- data include_list/1. 

:- data initial_vertex/2.  %% For tarjan algorithm.

:- pred get_intermodule_graph(+AbsFile,+GoalBeforeLoading) 

# "Obtains in @pred{intermodule_graph/2} the dependencies among
  modules of the program unit given by the module in file
  @var{AbsFile} (depending on process_libraries flag, library modules
  are included or not in the program unit).  For every module included
  in the intermodular graph, @var{ProcessGoal} is called.".

%get_intermodule_graph(AbsFile,_GoalBeforeLoading):-
%	read_grf_file(AbsFile),
%	!.
get_intermodule_graph(AbsFile,GoalBeforeLoading):-
	retractall_fact(intermodule_graph(_,_)),
	retractall_fact(intermodule_list(_)),
	retractall_fact(include_list(_)),
	retractall_fact(initial_vertex(_,_)),
	error_protect(ctrlc_clean(
				     process_files_from(AbsFile, zzz, any, 
				     process_one_module, 
				     check_stop_one_module(GoalBeforeLoading), false, true)
				 )).

process_one_module(Base):-
	asserta_fact(intermodule_list(Base)),
	( 
%	    setof(IFile,get_related_module(Base,IFile),IFileList0) ->
	    findall(IFile,uses(Base,IFile),IFileList0) ->
	    file_path(Base,BasePath),
	    working_directory(Old,BasePath),
	    processable_basenames(IFileList0,IBaseList),
	    findall(I, includes(Base,I), IncludeList0),
	    basenames(IncludeList0,IncludeList),
	    assert_include_list(IncludeList),
	    working_directory(BasePath,Old),
	    assertz_fact(initial_vertex(Base,IBaseList)),
	    assert_intermodule_graph(Base,IBaseList)
	;
	    assertz_fact(initial_vertex(Base,[]))
	).

file_path(Base,Path):-
	absolute_file_name(Base,'_opt','.pl','.',_,_,Path).

%% Asserts the list of imported  modules received as argument. 
assert_intermodule_graph(_Base,[]).
assert_intermodule_graph(Base,[IBase|IBaseList]):-
	asserta_fact(intermodule_graph(Base,IBase)),
	assert_intermodule_graph(Base,IBaseList).

assert_include_list([]).
assert_include_list([I|Is]):-
	(
	    current_fact(include_list(I)) ->
	    true
	;
	    asserta_fact(include_list(I))
	),
	assert_include_list(Is).

%% Calls Goal only once, adding Base as its first argument.
%% This predicate does not fail.
call_once_with_extra_arg(Base,Goal0):-
	Goal0 =.. [PredName | Args],
	Goal =.. [PredName, Base | Args],
	call(Goal),!.
call_once_with_extra_arg(_Base,_Goal0).

%% Given a list of file names, removes from them the modules
%% which are not processable, and returns the list of the remaining basenames.
processable_basenames([],[]).
processable_basenames([File|Files],[Base|Bases]):-
%%      Current dir is the one of the base being processed.
	absolute_file_name(File,'_opt','.pl','.',_,Base,_),
	module_is_processable(Base), !,
	processable_basenames(Files,Bases).
processable_basenames([_File|Files],Bases):-
	processable_basenames(Files,Bases).

basenames([],[]).
basenames([File|Files],[Base|Bases]):-
%%      Current dir is the one of the base being processed.
	absolute_file_name(File,'_opt','.pl','.',_,Base,_),
	basenames(Files,Bases).


%% Checks if module traversal must stop at current Base (not reading below it).
check_stop_one_module(Base,_GoalBeforeLoading):-
	\+ module_is_processable(Base), !.
check_stop_one_module(Base,GoalBeforeLoading):-
	\+ call_once_with_extra_arg(Base,GoalBeforeLoading),
	asserta_fact(intermodule_list(Base)),  %% There is no need to load Base, but 
	assertz_fact(initial_vertex(Base,[])). %% these facts must be asserted.

%% ********************************************************************
%% GRAPH FILE HANDLING
%% ********************************************************************

%%% grf_version('1.0').
%%% 
%%% :- pred read_grf_file(+AbsPlName)
%%% 
%%% # "Reads the graph file corresponding to @var{AbsPlName} toplevel
%%%   module if it exists, and checks if it is up-to-date wrt to all
%%%   modules and registry files.  If it is not, the graph is rebuilt and
%%%   the graph file overwritten.".
%%% 
%%% read_grf_file(AbsPlName):-
%%% 	retractall_fact(intermodule_graph(_,_)),
%%% 	retractall_fact(intermodule_list(_)),
%%% 	retractall_fact(initial_vertex(_,_)),
%%% 	get_module_filename(grf,AbsPlName,GrfName),
%%% 	file_up_to_date(GrfName,AbsPlName),
%%% 	open(GrfName,read,Fd),
%%% 	read_grf_file_(quiet,Fd,GrfName),
%%% 	!.
%%% 
%%% read_grf_file_(Verb,Fd,GrfName):-
%%% 	read_and_check_grf_version(Verb,Fd,GrfName), % Fails if not right version.
%%% 	verb_message(Verb, ['{Reading ',GrfName]),
%%% 	read_asg_data_loop(Verb,Fd,GrfName),   % Fails if not up to date wrt sources.
%%% 	verb_message(Verb, '}'),
%%% 	close(Fd),
%%% 	!.
%%% read_grf_file_(Verb,Fd,_GrfName):-
%%% 	close(Fd),
%%% 	verb_message(Verb, '}'),
%%% 	fail.
%%% 
%%% read_and_check_grf_version(_Verb,Fd,_):-
%%% 	grf_version(V), 
%%% 	read(Fd,v(V)),
%%% 	!.
%%% read_and_check_grf_version(Verb,_,GrfName):-
%%% 	verb_message(Verb,['{Old version in ',GrfName,'}']),
%%% 	fail.
%%% 
%%% read_grf_data_loop(Verb,Fd,GrfName):-
%%% 	fast_read(Fd,X),
%%% 	!,
%%% 	process_grf_data(GrfName,X),
%%% 	read_asg_data_loop(Verb,Fd,GrfName).
%%% read_asg_data_loop(_Verb,_Fd,_AsgName).
%%% 
%%% 
%%% process_grf_data(GrfName,module(Base,_PlDate,Status,Vertex)):-
%%%  	get_module_filename(pl, Base, PlName),
%%%  	get_module_filename(reg, Base, RegName),
%%% 	file_up_to_date(GrfName,PlName),
%%% 	file_up_to_date(GrfName,RegName),
%%% 	
%%% 	retractall_fact(intermodule_graph(_,_)),
%%% 	retractall_fact(intermodule_list(_)),
%%% 	retractall_fact(initial_vertex(_,_)).
%%% %@@@@@	    
%%% 
%%% 
%%% 
%%% store_asg_data([]).
%%% store_asg_data([defines(M,Base)|Xs]):-
%%% 	assert_itf(defines_module,M,_,_,Base),
%%% 	store_asg_data(Xs).
%%% store_asg_data([related_file(M)|Xs]):-
%%% 	add_related_file(M),
%%% 	store_asg_data(Xs).
%%% store_asg_data([X|Xs]) :-
%%% 	assertz_fact(X),
%%% 	store_asg_data(Xs).
%%% 
%%% 

 
%% ********************************************************************
%% INTERMODULAR GRAPH DEPTH COMPUTATION
%% ********************************************************************

:- pred intermodule_graph_depth(Module,Depth)

# "Module @var{Module} has depth @var{Depth} in the intermodular graph
  contained in @pred{intermodule_graph/2}. All modules included in a
  strongly connected component are labelled with the same depth.".

:- data intermodule_graph_depth/2. 

:- pred compute_intermodule_graph_depth(TopLevel) 

# "Computes the intermodule graph (contained in
  @pred{intermodule_graph/2}) with depths and stores it in
  @pred{intermodule_graph_depth/2}. The depth of every node in the
  graph is computed considering that all nodes in a strongly connected
  component are labelled with the same depth. The depth of a given
  node is the largest depth from the top-level module.

  This predicate must be called after calling get_intermodule_graph/2
  as it needs initial_vertex/2 already populated.".

:- use_module(plai(tarjan), [step2/2]).

compute_intermodule_graph_depth(TopLevel):-
	retractall_fact(intermodule_graph_depth(_,_)),
	findall( vertex(M1,Ms,0,0,undef), initial_vertex(M1,Ms), Vertex),
	intermod_sccs(Vertex,Cs),
	compute_noncyclic_depth(TopLevel,Cs).

:- pred intermod_sccs(in(Vertices),out(Cs)) 

# "@var{Cs} is the list of strongly connected components in the
  digraph represented by the list @var{Vertex} of vertex/5
  structures.".

intermod_sccs([],[]) :-	!.
intermod_sccs(Vertex,Cs) :-
	(Vertex == [] ->
	    Cs = []
	;
	    Vertex = [V|Vs],
	    S0 = state([V|Vs],V,[],0),
	    step2(S0,Cs)
	).

:- data module_scc/2.       % Module M belongs to scc S.
:- data interscc_edge/2.   % Scc S1 is connected to Scc S2.
:- data scc_depth/2.        % Scc S has depth D.

:- pred compute_noncyclic_depth(in(TopLevel),in(Cs)) 

# "computes the depth of every module, labelling the modules in a
  strongly connected component (in @var{Cs}) with the same depth. The
  result is stored in intermodule_graph_depth/2.".

compute_noncyclic_depth(TopLevel,Cs):-
	retractall_fact(module_scc(_,_)),
	retractall_fact(interscc_edge(_,_)),
	retractall_fact(scc_depth(_,_)),
	( 
	    Cs = [] ->
	    store_sccs([[TopLevel]],0)
	;
	    store_sccs(Cs,0)
	),
	gen_interscc_edges,
	current_fact(module_scc(TopLevel,STopLevel)),
	compute_scc_depth(STopLevel,0),
	gen_intermodule_graph_depth,
	retractall_fact(module_scc(_,_)),
	retractall_fact(interscc_edge(_,_)),
	retractall_fact(scc_depth(_,_)).

store_sccs([],_).
store_sccs([C|Cs],Id):-
	store_scc(C,Id),
	Id1 is Id+1,
	store_sccs(Cs,Id1).

store_scc([],_).
store_scc([M|Ms],Id):-
	assertz_fact(module_scc(M,Id)),
	store_scc(Ms,Id).

gen_interscc_edges:-
	current_fact(intermodule_graph(M,N)),
	current_fact(module_scc(M,SM)),
	current_fact(module_scc(N,SN)),
	SM \== SN,
	\+ current_fact(interscc_edge(SM,SN)),
	assertz_fact(interscc_edge(SM,SN)),
	fail.
gen_interscc_edges.

compute_scc_depth(Scc,Depth):-
	(
	    current_fact(scc_depth(Scc,Depth0),Ref) ->
	    (
		Depth0 < Depth ->
		erase(Ref),
		assertz_fact(scc_depth(Scc,Depth))
	    ;
		true
	    )
	;
	    assertz_fact(scc_depth(Scc,Depth))
	),
	recurse_scc_children(Scc,Depth).

recurse_scc_children(Scc,Depth):-
	Depth1 is Depth + 1,
	current_fact(interscc_edge(Scc,SccChild)),
	compute_scc_depth(SccChild,Depth1),
	fail.
recurse_scc_children(_Scc,_Depth).

gen_intermodule_graph_depth:-
	current_fact(scc_depth(Scc,Depth)),
	current_fact(module_scc(Mod,Scc)),
	assertz_fact(intermodule_graph_depth(Mod,Depth)),
	fail.
gen_intermodule_graph_depth.

%% ********************************************************************
%% ********************************************************************

:- pred write_registry_file(Base,Module,Verb) : atm * atm * atm

# "Writes to disk the registry information stored in memory for module
  @var{Module} which has as base file name @var{Base}.".

write_registry_file(Base,Module,Verb):-
	get_module_filename(reg,Base,RegName),
	open(RegName,write,Stream), % overwrites the previous file.
	write_registry_header(Module,Stream),
	current_output(CO),
	set_output(Stream),
	verb_message(Verb,['{Writing ',RegName]),
	write_registry_file_types(Module),
	write_registry_file_loop(Module),
	set_output(CO),
	close(Stream),
	verb_message(Verb,'}').

write_registry_file_types(Module):-
	current_fact(typedef(Module,TypeDef)),
	fast_write(TypeDef),
	fail.
write_registry_file_types(_Module).

write_registry_file_loop(Module):-
 	current_fact(registry(_,Module,Registry)),
 	fast_write(Registry),
	fail.
write_registry_file_loop(_Module).


%% ********************************************************************
%% ********************************************************************

:- pred change_open_mode(+Base,+OpenMode) 

# "@var{OpenMode} is the new open mode of the module with basename
  @var{Base}. @var{OpenMode} can take the values @code{read_write}
  and @code{read_only}.".

change_open_mode(Base,OpenMode):-
	just_module_name(Base,Module),
	read_registry_file(Module,Base,verbose),  %% if it is not loaded yet, loads it.
	retract_fact(registry_headers(Module,open_mode(_OldOpenMode))),
	asserta_fact(registry_headers(Module,open_mode(OpenMode))),
	add_changed_module(Module,Base,Module,current).
	
%% --------------------------------------------------------------------

:- pred open_mode(Base,Type,OpenMode) 

# "Module with basename @var{Base} is of type @var{Type} and it is
   opened with mode @var{OpenMode}. @var{Type} can be @code{user} or
   @code{library}. @var{OpenMode} is used to indicate if an imported
   module's registry can be updated. It can take the values
   @code{read_write} or @code{read_only}.".

open_mode(Base,Type,OpenMode) :-
%% It only works if module's registry has been already loaded. If it has not, it fails.
	just_module_name(Base,Module),
	current_fact(registry_headers(Module,open_mode(OpenMode))),
	( is_library(Base) ->
	  Type = library
	; Type = user
	).

%% ********************************************************************
%% ********************************************************************

:- pred registry_is_empty(+Domain,+Mod,+Base): atm * atm * atm

# "Succeeds if the registry of module @var{Mod} with base name
  @var{Base} is empty for the abstract domain @var{Domain}.".

registry_is_empty(Analysis,Mod,Base):-
	read_registry_file(Mod,Base,quiet),
	\+ current_fact(registry(_,Mod,registry(Analysis,_,_,_,_,_,_))).

%% ********************************************************************

:- pred module_is_processable(+Base) 

# "Succeeds if module in @var{Base} can be processed by intermodular
  preprocessing tools. This predicate may have to load the registry
  file of that module, in order to check that the module has
  read-write mode.".

module_is_processable(B):-
	current_pp_flag(process_libraries,ProcessLibs),
	module_is_processable_(B,ProcessLibs).

module_is_processable_(Base,_ProcessLibs):-
	user_module(Base), !, fail.
module_is_processable_(Base,ProcessLibs):-
	current_fact(module_is_processable_cache(Base,Processable,ProcessLibs)),
	!,
	Processable == yes.
module_is_processable_(Base,ProcessLibs):-
	curr_file_base(Base,_Module),  %% all current modules are processable.
	!,
	assert_if_not_asserted_yet(module_is_processable_cache(Base,yes,ProcessLibs)).
module_is_processable_(Base,ProcessLibs):-
	is_library(Base), 
	\+ (ProcessLibs == on), !,
	assert_if_not_asserted_yet(module_is_processable_cache(Base,no,off)),
	fail.
module_is_processable_(Base,ProcessLibs):-
	just_module_name(Base,Module),
	read_registry_file(Module,Base,quiet),  %% just in case.
	open_mode(Base,_,read_write), !,
	assert_if_not_asserted_yet(module_is_processable_cache(Base,yes,ProcessLibs)).
module_is_processable_(Base,ProcessLibs):-
	assert_if_not_asserted_yet(module_is_processable_cache(Base,no,ProcessLibs)),
	fail.

:- meta_predicate assert_if_not_asserted_yet(fact).

assert_if_not_asserted_yet(Goal):-
	\+ current_fact(Goal) ->
	asserta_fact(Goal)
    ;
	true.

:- pred module_is_processable_cache(Base,Processable,ProcessLibsFlag)

# "Cache predicate for speeding up when checking whether a module is
   processable or not. It unifies @var{Processable} with 'yes' if
   module in @var{Base}.pl is processable, or 'no' if it is not.
   @var{ProcessLibs} contains the value of 'process_libraries' flag
   for which the module is or is not processable.".

:- data module_is_processable_cache/3.

%% ********************************************************************
%% TOOLBOX
%% ********************************************************************

curr_file_base(Base,Module):-
	curr_file(File,Module),
	basename(File,Base).

%% --------------------------------------------------------------------

:- pred registry_up_to_date(Module,PLFile) : atom * atom

# "Succeeds if registry header of @var{Module} refers to current
  version of @var{PLFile} (comparing modification dates).".

registry_up_to_date(Module,PlName):-
	( current_fact(registry_headers(Module,pl_date(RegTime))) -> 
	  modif_time(PlName, PlTime),
	  RegTime = PlTime
	; true).

%% --------------------------------------------------------------------

:- pred less_or_equal_mark(+SP,?Mark0,?Mark1) : atm * atm * atm

# "Given a success policy @var{SP}, succeeds if @var{Mark0} is less or
  equal than @var{Mark1}. Invalid marks are the biggest marks.".

less_or_equal_mark(_SP,Mark,Mark):- !.      % if one of the marks is a free var, it is instantiated here.
less_or_equal_mark(_SP,unmarked,_Mark).
less_or_equal_mark(SP,Mark0,Mark1):-
	may_be_improved_mark(SP,Mark0),
	not_valid_mark(SP,Mark1).

%% --------------------------------------------------------------------

:- pred add_changed_module(+Module,+Base,+SourceModule,+Mode) 

# "Adds a new entry to @pred{changed_modules/6}. @var{Module} registry
  info has been changed as a result of analyzing @var{SourceModule},
  and the relatioship between @var{SourceModule} and @var{Module} is
  @var{Mode} (@code{imported}, @code{caller} or @code{current}).".

:- set_prolog_flag(multi_arity_warnings,off).

add_changed_module(Module,Base,SourceModule,Mode):-
	add_changed_module(Module,Base,SourceModule,Mode,n).

add_changed_module(Module,Base,SourceModule,Mode,ReqReanalysis):-
	changed_modules(Module,Base,SourceModule,Mode,current,ReqReanalysis), !.
add_changed_module(Module,Base,SourceModule,Mode,ReqReanalysis):-
	assertz_fact(changed_modules(Module,Base,SourceModule,Mode,current,ReqReanalysis)).

:- set_prolog_flag(multi_arity_warnings,on).
%% --------------------------------------------------------------------

add_imported_module(M,Base):-
	imported_modules(M,Base), !.
add_imported_module(M,Base):-
	assertz_fact(imported_modules(M,Base)), !.

%% --------------------------------------------------------------------

add_caller_module(M,_):-
	caller_modules(M,_), !.
add_caller_module(M,Base):-
	assertz_fact(caller_modules(M,Base)).

%% --------------------------------------------------------------------

add_caller_modules([],[]).
% add_caller_modules([_|Ms],[B|Bs]):-
% 	curr_file(B,_), !, %% Current modules are not considered caller modules.
% 	add_caller_modules(Ms,Bs).
add_caller_modules([M|Ms],[B|Bs]):-
	add_caller_module(M,B),
	add_caller_modules(Ms,Bs), !.

%% --------------------------------------------------------------------

%% 'user' module cannot be treated as a normal module.
user_module(user).  

%% ====================================================================

:- pred compute_external_reachability(+ImportedCalls)

# "For every exported predicate defined in the current module,
  calculates all the imported predicates which are reachable in a
  given abstract domain. Results are generated in
  external_reachable/6".

compute_external_reachability(ImportedCalls):-
	retractall_fact(external_reachable(_,_,_,_,_,_)),
	domain(AbsInt),
	compute_external_reachability_1(AbsInt,ImportedCalls),
	fail.
compute_external_reachability(_).

:- pred graph_reachable(KeyFrom,AbsInt,IdFrom,ModuleFrom,ImportedId,ImportedSg,ImportedASub) 

# "There is a path from node @var{IdFrom}, key @var{KeyFrom} in module
  @var{ModuleFrom} to the imported call pattern
  @var{ImportedSg}/@var{ImportedASub} in the abstract and-or
  graph. @var{ImportedId} is the Id of the complete containing the
  imported call pattern (used to speedup the process, avoiding the
  comparison of abstract substitutions with
  @var{ImportedSg}/@var{ImportedASub}.)".

:- data graph_reachable/7.
:- data graph_visited/7.    %% visited paths in the abstract graph.
                            %% (just to avoid cycles). It has the same structure than graph_reachable.

compute_external_reachability_1(AbsInt,ImportedCalls):-
	retractall_fact(graph_reachable(_,_,_,_,_,_,_)),
	retractall_fact(graph_visited(_,_,_,_,_,_,_)),
	initial_graph(AbsInt,ImportedCalls),
	fixpo_ops:collect_exported_completes(AbsInt,ExpIdList),
	graph_closure(AbsInt,ExpIdList),
	gen_external_reachable(AbsInt),
	retractall_fact(graph_reachable(_,_,_,_,_,_,_)),
	retractall_fact(graph_visited(_,_,_,_,_,_,_)),
	!.

%% --------------------------------------------------------------------

:- pred initial_graph(+AbsInt,+ImportedCalls)

# "Calculates the initial @pred{graph_reachable/7} path set. It is
  just a set of dummy call graph edges for all the exported
  predicates.".

initial_graph(AbsInt,ImportedCalls):-
%jcf-17.05.2005% to speed up registry generation. Only processable modules are considered, no duplicates.
%	current_itf(imports,Sg,_),
	member((_,_,Sg),ImportedCalls),
%jcf%
	functor(Sg,N,A),
	make_atom([N,A],SgKey),
	current_fact(complete(SgKey,AbsInt,Sg,Proj,_,Id,RefList)),
	assert_initial_arcs(AbsInt,Id,Sg,Proj,RefList),
	fail.
initial_graph(_,_).

assert_initial_arcs(_AbsInt,_Id,_Sg,_Proj,[]):- !.
assert_initial_arcs(AbsInt,Id,Sg,Proj,[(KeyPrev,IdPrev)|RefList]):-
	atom2data(KeyPrev,F,A,_,_),
	make_atom([F,A],NKeyPrev0),
	key_or_id_complete(NKeyPrev0,AbsInt,SgPrev,_,_,IdPrev,_,NKeyPrev),
	( 
	    get_module_from_sg(SgPrev,ModIdPrev) ->
	    true
	;
	    ModIdPrev = ''  %% Oops, '\+/1' has no module in Sg.
	),
	add_graph_reachable(NKeyPrev,AbsInt,IdPrev,ModIdPrev,Id,Sg,Proj),
	assert_initial_arcs(AbsInt,Id,Sg,Proj,RefList), !.

add_graph_reachable(NKeyPrev,AbsInt,IdPrev,ModIdPrev,Id,_Sg,_Proj):-
	current_fact(graph_reachable(NKeyPrev,AbsInt,IdPrev,ModIdPrev,Id,_Sg0,_Proj0)),
	!.
add_graph_reachable(NKeyPrev,AbsInt,IdPrev,ModIdPrev,Id,Sg,Proj):-
	asserta_fact(graph_reachable(NKeyPrev,AbsInt,IdPrev,ModIdPrev,Id,Sg,Proj)).

%% --------------------------------------------------------------------

:- pred graph_closure(AbsInt, ExpIdList)

# "Calculates the reachability graph closure, given the initial
  external_reachable/6 graph. This predicate goes backwards from
  imported predicates patterns towards exported predicates patterns,
  though it does not traverse the boundaries between modules (in case
  there were several current modules.)".

:- data continue/0.
graph_closure(AbsInt,ExpIdList):-
	set_fact(continue),
	repeat,
	(
	    continue ->
	    retractall_fact(continue),
	    current_fact(graph_reachable(Key0,AbsInt,Id0,Module0,Id,Sg,Proj), Ref),
	    assertz_fact(graph_visited(Key0,AbsInt,Id0,Module0,Id,Sg,Proj)),
%% Even if the starting node of this path corresponds to an exported 
%% predicate, the graph must be extended: there can be calls to an 
%% exported pattern from an internal predicate.
	    current_fact(complete(Key0,AbsInt,_,_,_,Id0,RefList)),
	    extend_graph_reachable(AbsInt,Module0,Id,Sg,Proj,RefList),
	    ( 
		member(Id0,ExpIdList) ->
		true
	    ;
		erase(Ref)
	    ),
	    fail
	;
	    true
	).

%% --------------------------------------------------------------------

:- pred extend_graph_reachable(+AbsInt,+Module,+Id,+Sg,+Proj,+RefList).

extend_graph_reachable(_AbsInt,_Module,_Id,_Sg,_Proj,[]).
extend_graph_reachable(AbsInt,Module,Id,Sg,Proj,[(KeyPrev,IdPrev)|RefList]):-
	atom2data(KeyPrev,F,A,_,_),
	make_atom([F,A],NKeyPrev0),
	key_or_id_complete(NKeyPrev0,AbsInt,_SgPrev,_,_,IdPrev,_,NKeyPrev),
	( 
	    \+ already_visited(NKeyPrev,AbsInt,IdPrev,Module,Id,Sg,Proj) ->
	    add_graph_reachable(NKeyPrev,AbsInt,IdPrev,Module,Id,Sg,Proj),
	    set_fact(continue)
	; 
	    true
	),
	extend_graph_reachable(AbsInt,Module,Id,Sg,Proj,RefList),
	!.   %% no backtrackable!
extend_graph_reachable(AbsInt,Module,Id,Sg,Proj,[_|RefList]):-
%       If there is no complete info, continue with the rest of the list.
	extend_graph_reachable(AbsInt,Module,Id,Sg,Proj,RefList), 
	!.

% already_visited(+Key0,+AbsInt,+Id0,+Module,+Sg,+Proj).
already_visited(Key0,AbsInt,Id0,Module,Id,_Sg,_Proj):-
	current_fact(graph_visited(Key0,AbsInt,Id0,Module,Id,_Sg0,_Proj0)).

%% key_or_id_complete(+NKeyPrev,+AbsInt,-Sg,-Proj,-Succ,+Id,-Ref)).
key_or_id_complete(SgKey,AbsInt,Sg,Proj,Succ,Id,Ref,SgKey):-
	current_fact(complete(SgKey,AbsInt,Sg,Proj,Succ,Id,Ref)),
	!.
key_or_id_complete(_SgKey0,AbsInt,Sg,Proj,Succ,Id,Ref,SgKey):-
	current_fact(complete(SgKey,AbsInt,Sg,Proj,Succ,Id,Ref)).


%% --------------------------------------------------------------------

:- pred gen_external_reachable(AbsInt)

# "Populates @pred{external_reachable/6} data predicate, looking for
  those paths in @pred{graph_reachable/7} which correspond to call
  patterns from an exported predicate to an imported one.".

gen_external_reachable(AbsInt):-
	current_fact(graph_reachable(KeyFrom,AbsInt,IdFrom,ModFrom,_IdTo,SgTo,ProjTo)),
	current_fact(complete(KeyFrom,AbsInt,SgFrom,ProjFrom,_,IdFrom,_)),
	get_module_from_sg(SgTo,ModTo),
	ModFrom \== ModTo,
	(
	    current_itf(exports,SgFrom,_)
	;
	    ModFrom = multifile
	),
	functor(SgTo,F,A),
	make_atom([F,A],SgToKey),
	assertz_fact(external_reachable(SgToKey,AbsInt,SgFrom,ProjFrom,SgTo,ProjTo)),
	fail.
gen_external_reachable(_).

%% ---------------------------------------------------------------------------

verb_message(verbose,Message) :-
	io_aux:message(Message).
verb_message(debug,Message) :-
	io_aux:message(Message).
verb_message(quiet,_Message).

%-----------------------------------------------------------------------------

:- pred get_module_from_sg(+Sg, ?Module) :: term * atm

# "@var{Module} is the name of the module for the predicate to which
  call pattern @var{Sg} corresponds.".

get_module_from_sg(Sg,Module):-
	current_itf(imports,Sg,Module0),
	!,
	(
	    just_module_name(Module0,Module) ->
	    true
	;
	    Module = Module0
	).
get_module_from_sg(Sg,Module):-
	current_itf(defines_pred,Sg,Module0),
	!,
	(
	    just_module_name(Module0,Module) ->
	    true
	;
	    Module = Module0
	).
get_module_from_sg(Sg,Module):-
	functor(Sg,Name,_),
	atom_codes(Name,NameS),
	get_module(NameS,ModuleS),
	atom_codes(Module0,ModuleS), 
	Module0 = Module,
	!. 

get_module([0':|_],[]):- !.
get_module([C|Cs],[C|Ms]):-
	get_module(Cs,Ms).
