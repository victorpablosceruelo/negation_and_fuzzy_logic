:- module(driver,
	[ analyze/1,
	  analyze/2,
	  acheck/0,
	  acheck/1,
	  acheck_summary/1,
%	  acheck_all/0,
	  module/1,
	  module/2,
	  transform/1,
	  transform/2,
	  clean_analysis_info/0,
% regtype
	  ctcheck_sum/1
	],
	[
	    assertions,
	    basicmodes,
	    regtypes,
	    nativeprops
%	    api( ciaopp_api ) 
	]).

%------------------------------------------------------------------------

:- doc(module,"This module provides the main entry points for a user.
      Its predicates will be available at the Ciao shell prompt as
      commands for guiding the preprocessing of programs.

      This is also the module that you have to modify to incorporate a
      new feature into CiaoPP. Add a clause for @tt{analyze/2} (and for
      @tt{analysis/1}) for a new analysis. Add a clause for @tt{transform/2}
      (and for @tt{transformation/1}) for a new program transformation.

      As an alternative, you can add clauses for @tt{analysis/4} and
      @tt{analysis/1} or @tt{transformation/4} and
      @tt{transformation/1}.  Since these predicates are multifile,
      you can do this in your own sources, in which case you don't
      need to modify this module. The files
      @tt{examples/Extending/myanalyzer.pl} and
      @tt{examples/Extending/myspecialyzer.pl} in the source directory
      are examples of this.").

:- doc(bug,"1. Remember to do the cleaning_up lazily").
:- doc(bug,"3. Program point compile time checking with the det and nf 
	domain needs some work. It is now turned off since it loops").

%------------------------------------------------------------------------
% Reexports.
%------------------------------------------------------------------------

:- reexport(plai(trace_fixp),[trace_fixp/1]). % for documentation
:- doc(doinclude,trace_fixp/1).

%------------------------------------------------------------------------
% Basic modules.
%------------------------------------------------------------------------

:- use_module(library(aggregates), [findall/3]).

:- use_module(ciaopp(api(api_base))).
:- use_module(ciaopp(api(api_module))).
:- use_module(ciaopp(api(api_predcl))).
:- use_module(ciaopp(api(api_printer)), 
	[ internal_remember_disj_conj_cls_names/1 ] ).

:- use_module(ciaopp(preprocess_flags), 
	[ current_pp_flag/2,
	  set_pp_flag/2,
	  push_pp_flag/2,
	  pop_pp_flag/1]).
:- use_module(program(p_unit), 
	[ preprocessing_unit/3, 
	  program/2,
	  replace_program/2]). 
:- use_module(program(itf_db), [curr_file/2]).

%------------------------------------------------------------------------
% Preprocessing modules.
%------------------------------------------------------------------------
:- use_module(plai(plai), [plai/5, cleanup_plai/1]).
:- use_module(plai(re_analysis), [update_ai_info_case/4]).

:- use_module(typeslib(typeslib), [undoall_types/0]).
%:- use_module(typeslib(typeslib),[ undoall_types/0,assert_initial_types/0]). 

:- use_module(spec(spec), [simplify_specialize/6]).
:- use_module(spec(spec_multiple), [all_versions/5]).
:- use_module(spec(codegen), [codegen/4, codegen_af/4, codegen_min/4]).
:- use_module(poly_spec(codegen_pcpe), [print_all_solutions/2]).
:- use_module(poly_spec(heuristic_pcpe), [get_all_solutions/1]).
:- use_module(spec(slicing), [slicing/3]).
:- use_module(spec(arg_filtering), [arg_filtering/5]).

:- use_module(infer(vartypes), [gather_vartypes/2]).
:- use_module(infer(infer_db), [cleanup_infer_db/1, domain/1]).
:- use_module(infer(infer_dom), [knows_of/2]).
:- use_module(infer(inferseff), [analyze_side_effects/1, cleanup_seff/0]).

:- use_module(program(unexpand), 
	[ 
% --- DTM: This can be done just after loading module...
% --- DTM: This will go to c_itf soon I guess
	  generate_unexpanded_data/1, % TODO: kludge?
	  clean_unexpanded_data/0     % TODO: kludge?
	]).

% complexity_analysis(_NewCls,_NewDs).
:- use_module(ciaopp(infernf)).

%:- use_module(granul(tr_granul),[annotate_granul/6]).

:- use_module(ctchecks(ctchecks_pred), [simplify_assertions_all/1]).
:- use_module(ctchecks(ctchecks), [ctchecks_pp/1]).

:- use_module(ctchecks(ctchecks_pred_messages), [init_ctcheck_sum/0, 
	is_any_false/1, is_any_check/1]).

% :- use_module(ctchecks(assrt_ctchecks_pred) , [ simplify_assertions/2 ]).
:- use_module(ctchecks(assrt_ctchecks_pp), [pp_compile_time_prog_types/3]).
:- use_module(ctchecks(preproc_errors), [cleanup_errors/0]).

:- use_module(library(prolog_sys), [statistics/2]).
%% *** Needs to be revised (is_checker/1) MH
:- use_module(plai(plai), [mod_plai/5, is_checker/1]).
:- use_module(library(filenames), [basename/2]).
:- use_module(program(p_abs), 
	[
	    cleanup_p_abs/0,
	    cleanup_p_abs_all/0,
	    gen_registry_info/3,
	    save_registry_info/2,
	    recover_from_invalid_state/2
	]).
:- use_module(plai(intermod), [set_top_level/1]).

:- use_module(syntax(tr_syntax_new)).

%------------------------------------------------------------------------
:- pred module(+FileName) : sourcename
	# "Reads the code of @var{FileName} and its preprocessing unit,
          and sets it as the current module.".

:- pred module(+FileNameList) : list(atm)
	# "Reads the code of the list of file names @var{FileNameList} (and
	   their preprocessing units), and sets them as the current
	   modules.".

module([M|Ms]):-
	!,
	module_with_flag([M|Ms],time,_).
module(Module):-
	module_with_flag([Module],time,_).

:- pred module(+File,-Info) # "Same as @pred{module/1} but it also
      prints the time required to read the module and returns extra
      information (including the time) on its second argument.".

module([M|Ms],Info):-
	!,
	module_with_flag([M|Ms],time,Info).
module(Module,Info):-
	module_with_flag([Module],time,Info).

module_with_flag(ModList,Flag,Info):-
	push_prolog_flag(runtime_checks, no),
	statistics(runtime,_),
% DTM: it is done in define_new_module
%       cleanup_plai(_),
% 	cleanup_infer_db(_),
% 	cleanup_seff,
% 	cleanup_p_abs,
%	cleanup_errors,
	undoall_types,
	retractall_fact(domain(_)),
	% load 
	absolute_file_names(ModList,AbsoluteNameList),
	(   AbsoluteNameList = [AbsoluteName] -> true
	;   AbsoluteNameList = AbsoluteName
	),
	inform_user( ['{Loading current module from ' , AbsoluteName] ),
	get_module( AbsoluteName , M ),
	define_new_module( M , AbsoluteName ),
	preprocessing_unit(AbsoluteNameList,_Ms,E),
	( E == yes -> Info=[error|Info0] ; Info=Info0 ),
	% assert_initial_types, 
	statistics(runtime,[_,T1]),
	(Flag == notime ->
	    true
	;
	    display_list(['{loaded in ', T1, ' msec.}\n'])
	),
	Info0=[time(T1,[])],
	inform_user(['}']),
	
	% DTM: Added this in fact to write in the directory 
        %      of the read file
% 	absolute_file_name(M,'_opt','.pl','.',_,Base,_),
% 	add_action( Base ),
% 	( 
% 	    MoreModules = [] ->
% 	    true
% 	;
% 	    add_action( 'modules' )
% 	),
	curr_file( _, Mod ),
	clean_unexpanded_data,
	generate_unexpanded_data( Mod ),
	pop_prolog_flag(runtime_checks).

%------------------------------------------------------------------------

:- pred clean_analysis_info 
# "Cleans all analysis info but keep the program as wether it would be
just read.".

clean_analysis_info :-
	% cleanup database 
        cleanup_plai(_),
	cleanup_infer_db(_),
	cleanup_seff,
	retractall_fact(domain(_)),
	cleanup_errors.

%------------------------------------------------------------------------


absolute_file_names([],[]).
absolute_file_names([M|Ms],[A|As]):-
	absolute_file_name(M,'_opt','.pl','.',A,_,_),
	absolute_file_names(Ms,As).





% DTM: moved to p_unit
% assert_curr_files([],[]).
% assert_curr_files([A|As],[M|Ms]):-
% 	asserta_fact(curr_file(A,M)),
% 	assert_curr_files(As,Ms).

%------------------------------------------------------------------------

:- pred analyze(-Analysis) => analysis
	# "Returns on backtracking all available analyses.".
:- pred analyze(+Analysis) : analysis + (not_fails, no_choicepoints)
	# "Analyzes the current module with @var{Analysis}.".

analyze(Analysis):- var(Analysis), !, analysis(Analysis).
analyze(Analysis):- analyze(Analysis,_).

:- pred analyze(+Analysis,-Info) 

# "Same as analyze(@var{Analysis}) but returns information that can be
	used to check the results of the analysis.".

analyze(Analysis,Info):-
	analysis(Analysis), !,
	curr_file(File,_),
	current_pp_flag(fixpoint,Fixp),
        %% *** Needs to be revised MH
	(  is_checker(Fixp)
	-> Header = '{Checking certificate for '
	;  Header = '{Analyzing '),
	inform_user([Header,File]),
	current_pp_flag(intermod,Intermod),
	( 
	    Intermod = on ->
	    basename(File,Base),
	    intermod:set_top_level(Base),
	    push_pp_flag(entry_policy,force),  %% needed for generating proper output!
%	    push_pp_flag(success_policy,over_all),
	    cleanup_p_abs_all,
	    recover_from_invalid_state(Analysis,Base)
	;
	    true
	),
	program(Cls,Ds),
	add_action( Analysis ),
	analyze_(Analysis,Cls,Ds,Info),
	assert_domain(Analysis),
	(
	    Intermod = on ->
	    gen_registry_info(quiet,_,_),
	    save_registry_info(quiet,_SaveInfo),  %% all registry files must be saved.
%	    pop_pp_flag(success_policy),
	    pop_pp_flag(entry_policy)
	;
	    true
	),
	inform_user(['}']).
analyze(Analysis,_Info):-
	inform_user(['{Not a valid analysis: ',Analysis,'}']),
	fail.

:- use_package(ciaopp(analysis_register)).

analysis_needs_load(Analysis) :-
	lazy_analysis(Analysis),
	\+ loaded_analysis(Analysis).

:- use_module(library(compiler),   [use_module/1]).
load_analysis(Analysis) :-
	analysis_module(Analysis, Module),
	use_module(Module).

% take care of incompatibilities here!
analyze_(nfg,Cls,_Ds,nfinfo(TimeNf,Num_Pred,Num_NF_Pred,NCov)):- !,
	cleanup_infer_db(nfg),
	cleanup_infer_db(vartypes),
	gather_vartypes(Cls,Trusts),
        non_failure_analysis(Cls,Trusts,TimeNf,Num_Pred,Num_NF_Pred,NCov).
analyze_(seff,Cls,_Ds,_Info):- !,
	analyze_side_effects(Cls).
analyze_(Analysis,Cls,Ds,Info):-
	(analysis_needs_load(Analysis) -> load_analysis(Analysis) ; true),
	analysis(Analysis,Cls,Ds,Info), !.
analyze_(AbsInt,Cls,Ds,Info):-
	current_pp_flag(fixpoint,Fixp),
	% some domains may change widen and lub:
	current_pp_flag(widen,W),
	current_pp_flag(multi_success,L),
	add_packages_if_needed(AbsInt),
	(
	    \+ current_pp_flag(intermod,off) ->
	    mod_plai(Cls,Ds,Fixp,AbsInt,Info)
	;
	    plai(Cls,Ds,Fixp,AbsInt,Info)
	),
 	set_pp_flag(multi_success,L),
	set_pp_flag(widen,W).

assert_domain(AbsInt):-
	current_fact(domain(AbsInt)), !.
assert_domain(AbsInt):-
	assertz_fact(domain(AbsInt)).

:- push_prolog_flag(multi_arity_warnings,off).

:- pred analysis(Analysis,Clauses,Dictionaries,Info) : analysis(Analysis)
	# "Performs @var{Analysis} on program @var{Clauses}.".
:- multifile analysis/4.

:- prop analysis(Analysis)
	# "@var{Analysis} is a valid analysis identifier.".
:- multifile analysis/1.

analysis(nfg).
analysis(seff).
analysis(AbsInt):- aidomain(AbsInt), !.
analysis(Analysis) :- lazy_analysis(Analysis), !.


:- pop_prolog_flag(multi_arity_warnings).

% for documenting the multifile aidomain/1
% No way! :- use_module(plai(domains)).
:- doc(aidomain/1,"See the chapter on @tt{domains}.").
:- multifile aidomain/1.



:- pred add_packages_if_needed(Analysis) : analysis(Analysis)
	# "For the given @var{Analysis} try to write down the
          corresponding package(s) for a correct output.".

% --- DTM: This should be in the analisis itself
add_packages_if_needed( shfr ) :-
	!,
	add_package_to_output( assertions ),
	add_package_to_output( nativeprops ).
add_packages_if_needed( A ) :-
	knows_of( regtypes , A ),
	!,
	add_package_to_output( assertions ),
	add_package_to_output( regtypes ).
add_packages_if_needed( _ ) :-
	add_package_to_output( assertions ).

	


%------------------------------------------------------------------------

:- pred transform(-Trans) => transformation
	# "Returns on backtracking all available program transformation identifiers.".
:- pred transform(+Trans) : transformation
	# "Performs transformation @var{Trans} on the current module.".

transform(Trans):- var(Trans), !, transformation(Trans).
transform(Trans):- transform(Trans,_Info).

:- pred transform(+Trans,-Info) 

# "Same as transform(@var{Trans}) but returns information that can be
	used to check the results of the transformation.".

transform(Trans,Info):-
	transformation(Trans), !,
	curr_file(File,_),
	inform_user(['{Transforming ',File]),
	program(Cls,Ds),
	add_action( Trans ),
	transform_(Trans,Cls,Ds,Info),
	inform_user(['}']).
transform(Trans,_Info):-
	inform_user(['{Not a valid program transformation: ',Trans,'}']),
	fail.

transform_(spec,Cls,Ds,Info):- !,
	simpspec(spec,Cls,Ds,Info).
transform_(simp,Cls,Ds,Info):- !,
	simpspec(simp,Cls,Ds,Info).
%% transform_(grain,Cls,Ds,Info):- !,
%%         annotate_granul(Cls,Ds,OutFile,QKey,NewCls,NewDs),
%% 	update_program(NewCls,NewDs).
transform_(vers,Cls,Ds,Info):- !,
	simpspec(vers,Cls,Ds,Info).
transform_(codegen,Cls,Ds,Info):- !,
	simpspec(codegen,Cls,Ds,Info).
transform_(codegen_af,Cls,Ds,Info):- !,
	simpspec(codegen_af,Cls,Ds,Info).
transform_(codegen_min,Cls,Ds,Info):- !,
	simpspec(codegen_min,Cls,Ds,Info).
transform_(slicing,Cls,Ds,Info):- !,
	simpspec(slicing,Cls,Ds,Info).
transform_(codegen_poly,_Cls,_Ds,Info):- !,
	(current_pp_flag(fixpoint,poly_spec) ->
% 	    domain(AbsInt),   % last domain used...
% 	    aidomain(AbsInt), !, 
	    get_all_solutions(Solutions),
	    print_all_solutions(Solutions,Info)
	;
	    true).
transform_(arg_filtering,Cls,Ds,_Info):- !,
	domain(AbsInt),   % last domain used...
	aidomain(AbsInt), !, 
	arg_filtering(Cls,Ds,AbsInt,NCls,NDs),
	replace_program(NCls,NDs).
%% --- put this code in the reader!!
transform_( rem_disj , _Cls , _Ds , _Info ) :- !,
	get_clauses( Cls ),
	remove_disj_and_cond_cl( Cls , NCls , [] , Names ),
	%% --- put this code in the lector
	internal_remember_disj_conj_cls_names( Names ),
	add_clauses( NCls ).
transform_(Tr,Cls,Ds,Info):-
	transformation(Tr,Cls,Ds,Info).
%	domain(AbsInt),   % last domain used...
%	aidomain(AbsInt), !, 
%	codegen_min(Cls,Ds,Info).



simpspec(Spec,Cls,Ds,Info):-
	domain(AbsInt),   % last domain used...
	aidomain(AbsInt), !, 
	simpspec_(Spec,AbsInt,Cls,Ds,TmpCls,TmpDs,Info),
 	decide_update_ai_info_case(Spec,TmpCls,TmpDs,NewCls,NewDs),
 	replace_program(NewCls,NewDs).
simpspec(Spec,_Cls,_Ds,_Info):-
	inform_user(['{Required analysis info not available for ', Spec, '}']),
	fail.

simpspec_(vers,AbsInt,Cls,Ds,NewCls,NewDs,_Info):- !,
	all_versions(Cls,Ds,AbsInt,NewCls,NewDs).
simpspec_(codegen,AbsInt,Cls,Ds,NewCls,NewDs,Info):- !,
	(
	    current_pp_flag(local_control,off)
	-> 
            NewCls = Cls,
	    NewDs = Ds 
	;
	    codegen(AbsInt,NewCls,NewDs,Info)
	).
simpspec_(codegen_af,AbsInt,Cls,Ds,NewCls,NewDs,Info):- !,
	(
	    current_pp_flag(local_control,off)
	-> 
            NewCls = Cls,
	    NewDs = Ds 
	;
	    codegen_af(AbsInt,NewCls,NewDs,Info)
	).
simpspec_(codegen_min,AbsInt,Cls,Ds,NewCls,NewDs,Info):- !,
	(
	    current_pp_flag(local_control,off)
	-> 
            NewCls = Cls,
	    NewDs = Ds 
	;
	    codegen_min(AbsInt,NewCls,NewDs,Info)
	).
simpspec_(slicing,AbsInt,Cls,Ds,NewCls,NewDs,_Info):- !,
	(
	    current_pp_flag(local_control,off)
	-> 
            NewCls = Cls,
	    NewDs = Ds 
	;
	    slicing(AbsInt,NewCls,NewDs)
	).
simpspec_(Spec,AbsInt,Cls,Ds,NewCls,NewDs,_Info):-
	simplify_specialize(AbsInt,Spec,Cls,Ds,NewCls,NewDs).


decide_update_ai_info_case(codegen,Cls,Ds,Cls,Ds):- !.
decide_update_ai_info_case(codegen_af,Cls,Ds,Cls,Ds):-!,
        cleanup_plai(_),
	cleanup_infer_db(_),
	cleanup_seff,
	cleanup_p_abs,
	undoall_types,
	retractall_fact(domain(_)).
decide_update_ai_info_case(codegen_min,Cls,Ds,Cls,Ds):-!,
        cleanup_plai(_),
	cleanup_infer_db(_),
	cleanup_seff,
	cleanup_p_abs,
	undoall_types,
	retractall_fact(domain(_)).
decide_update_ai_info_case(_Spec,TmpCls,TmpDs,NewCls,NewDs):-
	update_ai_info_case(TmpCls,TmpDs,NewCls,NewDs).

:- push_prolog_flag(multi_arity_warnings,off).

:- pred transformation(Transformation,Clauses,Dictionaries,Info)
	: transformation(Transformation)
	# "Performs @var{Transformation} on program @var{Clauses}.".
:- multifile transformation/4.

:- prop transformation(Transformation)
	# "@var{Transformation} is a valid transformation identifier.".
:- multifile transformation/1.

transformation( spec          ).
transformation( simp          ).
transformation( vers          ).
transformation( codegen       ).
transformation( codegen_af    ).
transformation( codegen_poly  ).
transformation( slicing       ).
transformation( arg_filtering ).
transformation( granul        ).
transformation( rtc           ).
transformation( codegen_min   ).

:- pop_prolog_flag(multi_arity_warnings).

%------------------------------------------------------------------------

:- pred acheck_summary(S): var(S) => ctcheck_sum(S)
# "Checks assertions w.r.t. analysis information. Upon success @var{S} 
  is bound to: ok (the compile-time checking process has generated no error 
  nor warning), warning (compile-time checking has not generated any error, 
  but there has been at least one warning) or error (at least one error has 
  been produced).".

:- regtype ctcheck_sum/1.
ctcheck_sum(ok).
ctcheck_sum(warning).
ctcheck_sum(error).


acheck_summary(Sum) :-
	init_ctcheck_sum,
	acheck,
	decide_summary(Sum),!.

decide_summary(Sum) :-
	is_any_false(yes),!,
	Sum = error.
decide_summary(Sum) :-
	is_any_check(yes),
	current_pp_flag(ass_not_stat_eval, ANSE), 
	( ANSE = warning,  Sum = warning
	; ANSE = error,  Sum = error
	; Sum = ok
	),!. 
decide_summary(ok).


:- pred acheck # "Checks assertions w.r.t. analysis information.".

acheck :-
	findall(AbsInt, 
	        ( domain(AbsInt),           % not very nice though...
		  AbsInt \== nf, 
		  AbsInt \== det,
		  aidomain(AbsInt)), 
	Domains),	
	check_assertions(Domains),
	% It triggers check assertions for cost,size, and resources (JNL)
	check_assertions([]).   


acheck(AbsInt):-
	domain(AbsInt),
	aidomain(AbsInt), !, 
	check_assertions([AbsInt]).

% (06/06/07 - JNL) The original version of acheck/0 is thought to check
% assertions wrt PLAI domains. Other domains such as resources, nfg, etc
% should be integrated. This solution should be temporary.
acheck(resources):- 
	check_assertions([]).
acheck(AbsInt):-
	inform_user(['{Analysis ', AbsInt, ' not available for checking}']),
	fail.

/*
check_assertions(Types,Modes):-
	statistics(runtime,_),
	curr_file(File,_),
	inform_user(['{Checking assertions of ',File]),
	perform_pred_ctchecks(Types,Modes),
	perform_pp_ctchecks(Types,Modes),
	statistics(runtime,[_,CTime]),
	inform_user(['{assertions checked in ',CTime, ' msec.}']),
	inform_user(['}']).
*/

check_assertions([]):-!.
/* -- Commented out by EMM: This code is causing duplicated warnings
check_assertions([]):-   % by JNL
	statistics(runtime,_),
	curr_file(File,_),
	inform_user(['{Checking resource assertions of ',File]),
	perform_pred_ctchecks([]),
	statistics(runtime,[_,CTime]),
	inform_user(['{Resource assertions checked in ',CTime, ' msec.}']),
	inform_user(['}']).
*/
check_assertions(Domains):-
	statistics(runtime,_),
	curr_file(File,_),
	inform_user(['{Checking assertions of ',File]),
	perform_pred_ctchecks(Domains),
	modes_analysis(Modes),
	types_analysis(Types),
	perform_pp_ctchecks(Types,Modes),
	statistics(runtime,[_,CTime]),
	inform_user(['{assertions checked in ',CTime, ' msec.}']),
	inform_user(['}']).


modes_analysis(Modes):-
	domain(Modes),
	knows_of(ground,Modes),
	!.
modes_analysis(none):-
	current_pp_flag(verbose_ctchecks,VC),
	(VC == on ->
	    inform_user(['{No mode analysis available for checking}'])
	;
	    true
	).

types_analysis(Types):-
	domain(Types),
	knows_of(regtypes,Types),
	!.
types_analysis(none):-
	current_pp_flag(verbose_ctchecks,VC),
	(VC == on ->
	    inform_user(['{No type analysis available for checking}'])
	;
	    true
	).

%------------------------------------------------------------------------
% perform_pred_ctchecks(Types,Modes):-
% 	current_pp_flag(pred_ctchecks,CT),
% 	decide_pred_ctchecks(CT,Types,Modes).
% decide_pred_ctchecks(none,_Types,_Modes):-!.
% %decide_pred_ctchecks(old,Types,Modes):-!,
% %	assrt_ctchecks_pred:simplify_assertions(Types,Modes).
% decide_pred_ctchecks(New,Types,Modes):-
% 	(New == new; New == new_succ),
% 	ctchecks_pred:simplify_assertions(Types,Modes).


perform_pred_ctchecks(Domains):-
	current_pp_flag(pred_ctchecks,CT),
	decide_pred_ctchecks(CT,Domains).
decide_pred_ctchecks(off,_):-!.
decide_pred_ctchecks(_,Domains):-
	simplify_assertions_all(Domains).


perform_pp_ctchecks(Types,Modes):-
	current_pp_flag(pp_ctchecks,CT),
	decide_pp_ctchecks(CT,Types,Modes).
decide_pp_ctchecks(none,_Types,_Modes):-!.
decide_pp_ctchecks(old,Types,Modes):-!,
	program(Cls,Ds),
	pp_compile_time_prog_types(Cls,Ds,[Types,Modes]).
decide_pp_ctchecks(new,Types,Modes):-
	ctchecks:ctchecks_pp([Types,Modes]).
	
