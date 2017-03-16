% (string-match "pp_flag([ ]*\\(.*\\),[ ]*'\\(\\(\\\\'\\|.\\|\n\\)*\\)'[ ]*).\ncurrent_pp_flags(\\([^,]*\\),\\(.*\\)).\nvalid_flag_values(\\([^,]*\\),\\(.*\\))." 

% "pp_flag(aaa, 'bbasdfsadf' ).
% current_pp_flags(  verbose_ctchecks  , off ).
% valid_flag_values( verbose_ctchecks  , member(_,[off,on])).")

:- module(preprocess_flags,[
	push_pp_flag/2,
	pop_pp_flag/1,
	current_pp_flag/2,
	set_pp_flag/2,
	pp_flag/2,
	pp_flag/1,            % it is a type
	flag_value/1,
	valid_flag_value/2,   % (auto_inteface uses them)
	valid_flag_values/2,  % (auto_inteface uses them)
	dump_flags/1,
	save_config/1,
	restore_config/1,
	remove_config/1,
	show_config/1,
	show_configs/0,
	sublist2/2
	],[assertions,nortchecks,regtypes,isomodes,persdb]).

:- doc(title, "Preprocessing Flags").

:- push_prolog_flag(unused_pred_warnings, no).
%%------------------------------------------------------------------------

:- doc(bug,"Having changed the names of flags, the rest of ciaopp
	should also be changed to use the new names: current solution
	is a kludge.").

%%------------------------------------------------------------------------

:- use_module(library(lists), [sublist/2]).
:- use_module(library(aggregates)).                % for dump_flags
:- use_module(library(write)).

%%------------------------------------------------------------------------

:- trust pred persistent_dir( Key , Dir ) => atm * atm.
:- trust pred persistent_dir( Key , Dir ) :  atm * var.
:- trust pred persistent_dir( Key , Dir ) :  atm * atm.

persistent_dir(dbdir, '~/.ciao.d/ciaopp_flags').

:- persistent( config/2 , dbdir ).

:- trust pred config( A , B ) => atm * list.
:- trust pred config( A , B ) :  atm * list.
:- trust pred config( A , B ) :  atm * var.
:- trust pred config( A , B ) :  var * var.

:- pred current_pp_flags(Name,Value) 
	:: pp_flag * flag_value
	# "Current proprocess flags. These facts can be dynamically updated 
           using @tt{set_pp_flag/2}. Initial facts are flag values by 
           default.".

:- data current_pp_flags/2.


:- discontiguous pp_flag/2.
:- discontiguous current_pp_flags/2.
:- discontiguous valid_flag_values/2.


%:- multifile pp_flag/2 , 
%	     pp_flag/1,
%	     current_pp_flags/2,
%	     valid_flag_values/2.

pp_flag(           output_info    , 'Verbosity of additional info included as comments in output.' ).
current_pp_flags(  output_info    , none ).
valid_flag_values( output_info    , member( _  , [none, medium, high  ] ) ).

%[LD] for forcing value evaluation instead of expression manipulation during cost comparison
pp_flag(           ctchecks_value_evaluation  , 'Controls whether cost comparison using value evaluation
is enforced.' ).
current_pp_flags(  ctchecks_value_evaluation  , off).
valid_flag_values( ctchecks_value_evaluation  , member( _ , [on, off] ) ).
%[/LD] for forcing value evaluation instead of expression manipulation during cost comparison

%[LD] for intervals information
pp_flag(           ctchecks_intervals  , 'Controls whether the output of cost assertion ckecking shows intervals or not.' ).
current_pp_flags(  ctchecks_intervals  , off).
valid_flag_values( ctchecks_intervals  , member( _ , [on, off] ) ).
%[/LD] for intervals information


pp_flag(           dump_pred  , 
 'Whether to include predicate information in .dump files.' ). 
current_pp_flags(  dump_pred  , all ).
valid_flag_values( dump_pred  , member( _  , [all, nodep, off  ] ) ).

pp_flag(           dump_pp    , 'Whether to include program point information in .dump files.' ).
current_pp_flags(  dump_pp    , all ).
valid_flag_values( dump_pp    , member( _  , [all, deps , off  ] ) ).

pp_flag(           dump_ext   , 'No Help Available' ).
current_pp_flags(  dump_ext   , all ).
valid_flag_values( dump_ext   , member( _  , [all, int  , iter ] ) ).

pp_flag(           inter_all          , 'Select which high-level action the preprocessor will perform.' ).
current_pp_flags(  inter_all          , analyze ).
valid_flag_values( inter_all          , member( _ , 
	                                        [
						 analyze, 
						 check_assertions, 
						 check_certificate,
						 optimize] ) ).

pp_flag(           inter_javall          , 'No Help Available' ).
current_pp_flags(  inter_javall          , analyze ).
valid_flag_values( inter_javall          , member( _ , [ analyze ] )).

pp_flag(           inter_ana          , 'No Help Available' ).
current_pp_flags(  inter_ana          , [types,modes,ana_num,ana_nf,ana_det,ana_cost]   ).
valid_flag_values( inter_ana          , sublist2( _ , [
						       types,
						       modes,
						       ana_num,
						       ana_nf,
						       ana_cost,
						       ana_det
						     ] ) ).

pp_flag(           inter_javana          , 'No Help Available' ).
current_pp_flags(  inter_javana          , [java_types,java_modes,java_ana_cost]   ).
valid_flag_values( inter_javana          , sublist2( _ , [
						       java_types,
						       java_modes,
						       java_ana_cost
						     ] ) ).

pp_flag(           inter_optimize     , 'Determines the kind of optimization to perform' ).
current_pp_flags(  inter_optimize     , spec ).
valid_flag_values( inter_optimize     , member( _ , [spec,parallelize,
                                                     slice,poly_spec] ) ).

pp_flag(           para_ann           , 'Decides whether to parallelize the current \
program using a specific annotation algorithm.' ).
current_pp_flags(  para_ann           , mel ).
valid_flag_values( para_ann           , member( _ , [mel, cdg, udg, uoudg,
                                                     uudg, disjwait,
                                                     tgudg, urlp, crlp] )).

pp_flag(           para_iap           , 'Decides whether \
the parallelizer uses only the abstract information before the goals and the \
conjuntion or also the information after each goal and the conjuntion in \
the parallelization. This amounts to using strict independence or \
non-strict independence.' ).
current_pp_flags(  para_iap           , nsiap ).
valid_flag_values( para_iap           , member( _ , [siap, nsiap , both] )).

pp_flag(           para_local         , 'Decides whether \
the parallelizer will use information of local analysis or not.' ).
current_pp_flags(  para_local         , local ).
valid_flag_values( para_local         , member( _ , [none, local] )).

pp_flag(           para_grain         , 'Decides whether a granularity \
analysis will be used to parallelize the current program.' ).
current_pp_flags(  para_grain         , none ).
valid_flag_values( para_grain         , member( _ , [none, gr, gr_res] )).

pp_flag(           para_grain_res         , 'Specifies the resources on which the granularity analysis is applied.' ).
current_pp_flags(  para_grain_res         , [none] ).
valid_flag_values( para_grain_res         , list(_)).

pp_flag(           para_cost          , 'Decides which cost analysis \
will be used by the granularity analysis to parallelize the \
current program.' ).
current_pp_flags(  para_cost          , both ).
valid_flag_values( para_cost          , member( _ , [none, lower,
                                        upper, both] )).

pp_flag(           menu_level         , 'Whether to use the naive or expert menu.' ).
current_pp_flags(  menu_level         , naive ).
valid_flag_values( menu_level         , member( _  , [naive, expert] ) ).

pp_flag(           menu_java_level    , 'Select level of expertise' ).
current_pp_flags(  menu_java_level    , naive ).
valid_flag_values( menu_java_level    , member( _  , [naive] ) ).


pp_flag(           menu_last_config   , 'Last menu stored configuration used' ).
current_pp_flags(  menu_last_config   , none ).
valid_flag_values( menu_last_config   , is_menu_config( _ ) ).


pp_flag(           menu_config_name   , 'Name of the last menu stored configuration' ).
current_pp_flags(  menu_config_name   , none ).
valid_flag_values( menu_config_name   , atom( _ ) ).


pp_flag(           check_config_ana   , 
	                   'Decides whether to configure analysis flags or not' ).
current_pp_flags(  check_config_ana   , off    ).
valid_flag_values( check_config_ana   , member( _ , [on,off] ) ).

pp_flag(           modes              , 'Selects a domain for mode analysis' ).
current_pp_flags(  modes              , shfr    ).
valid_flag_values( modes              , modeanalysis( _ ) ).

pp_flag(           java_modes         , 'Selects a domain for Java mode analysis' ).
current_pp_flags(  java_modes         , none    ).
valid_flag_values( java_modes         , member( _ , [none, java_nullity] )).

pp_flag(           types              , 'Selects a domain for type analysis' ).
current_pp_flags(  types              , eterms  ).
valid_flag_values( types              , typeanalysis( _ ) ).

pp_flag(           java_types         , 'Selects a domain for Java type analysis' ).
current_pp_flags(  java_types         , none  ).
valid_flag_values( java_types         , member( _ , [none,java_cha] )).

pp_flag(           ana_num            , 'Selects a domain for numeric analysis' ).
current_pp_flags(  ana_num            , none  ).
valid_flag_values( ana_num            , numericanalysis( _ )).

pp_flag(           ana_nf             , 'Type of non-failure analysis: monovariant (nfg), or multi-variant (nf)' ).
current_pp_flags(  ana_nf             , none    ).
valid_flag_values( ana_nf             , member( _ , [none, nf, nfg ] )).

pp_flag(           ana_det            , 'Type of determinacy analysis: multi-variant (det)' ).
current_pp_flags(  ana_det            , none    ).
valid_flag_values( ana_det            , member( _ , [none, det] )).

pp_flag(           ana_cost           , 'Type of cost (and size) analysis: lower bounds (steps_lb), upper bounds (steps_ub), both (steps_ualb), complexity order (steps_o), and resources').
current_pp_flags(  ana_cost           , none    ).
valid_flag_values( ana_cost           , member( _ , [none, steps_ub, steps_lb, steps_ualb, steps_o, resources] )).

pp_flag(           java_ana_cost           , 'Type of cost (and size) analysis').
current_pp_flags(  java_ana_cost           , none    ).
valid_flag_values( java_ana_cost           , member( _ , [none, resources] )).

pp_flag(           ana_size           , 'Type of size analysis: lower bounds (size_lb), upper bounds (size_ub), both (size_ualb), or complexity order (size_o)' ).
current_pp_flags(  ana_size           , none    ).
valid_flag_values( ana_size           , member( _ , [none, size_ub,
	                                             size_lb, size_ualb, size_o] )).
pp_flag(           complexity_output  , 'To output the complexity (steps and term size) inferred for predicates in \
terms of just arithmetic functions (funct) or complexity orders of such functions (big_o)').
current_pp_flags(  complexity_output  , funct ).
valid_flag_values( complexity_output  , member(_,[funct,big_o]) ).

pp_flag(           peval_ana          , 'Indicates the abstract domain which must be used during partial evaluation' ).
current_pp_flags(  peval_ana          , pd     ).
valid_flag_values( peval_ana          , modetypeanalysis( _ ) ).

pp_flag(           spec_poly          , 'Controls whether specialization is mono-variant or multi-variant' ).
current_pp_flags(  spec_poly          , off    ).
valid_flag_values( spec_poly          , member( _ , [off,mono,poly] ) ).

pp_flag(           poly_fitness          , 'Specifies fitness function to be used during poly-specialization' ).
current_pp_flags(  poly_fitness          , bytecode    ).
valid_flag_values( poly_fitness          , member( _ , [speedup,bytecode,memory,balance,bounded_size] ) ).

pp_flag(           pcpe_bounded_size    , 'Maximum size of residual program. It can be expressed in bytes, with a suffix (e.g. 5890, 10K, 2M), or as a factor of the size of the original program (e.g. 1.5x)').
current_pp_flags(  pcpe_bounded_size    , '10K'    ).
valid_flag_values( pcpe_bounded_size    , atm(_) ).


pp_flag(           poly_filter_equiv    , 'Filter equivalent candidates').
current_pp_flags(  poly_filter_equiv    , on    ).
valid_flag_values( poly_filter_equiv    , member( _ , [on,off] ) ).

pp_flag(           poly_strategy        , 'Poly-controlled algorithm').
current_pp_flags(  poly_strategy        , all_sols    ).
valid_flag_values( poly_strategy        , member( _ , [all_sols,oracle] ) ).

pp_flag(           poly_pruning        , 'Pruning for all-solutions algorithm').
current_pp_flags(  poly_pruning        , heuristic    ).
valid_flag_values( poly_pruning        , member( _ , [none,heuristic,bnb,both] ) ).

pp_flag(           poly_depth_lim          , 'Numeric value indicating the depth limit bnb pruning. 0 indicates unlimited').
current_pp_flags(  poly_depth_lim          , 3    ).
valid_flag_values( poly_depth_lim          , nnegint(_) ).

pp_flag(           polyvar_pcpe          , 'Consistency heuristics').
current_pp_flags(  polyvar_pcpe          , pred   ).
valid_flag_values( polyvar_pcpe          , member( _ , [pred,modes] ) ).

pp_flag(           poly_modes          , 'Abstract domains for mode-consistency').
current_pp_flags(  poly_modes          , sd    ).
valid_flag_values( poly_modes          , member( _ , [sd,sdl,sd_depth2] ) ).

pp_flag(           aggressivity        , 'Aggressivity of specialization (for naive users)').
current_pp_flags(  aggressivity        , normal    ).
valid_flag_values( aggressivity        , member( _ , [aggressive,normal,conservative] ) ).

pp_flag(           pcpe_evaltime       , 'Amount of time each residual program is to be evaluated (in msecs)').
current_pp_flags(  pcpe_evaltime       , 200    ).
valid_flag_values( pcpe_evaltime       , nnegint(_) ).

pp_flag(           assert_ctcheck     , 'Decides whether to run compile-time checks and how \
the analyses to be used are selected. When auto is selected, CiaoPP tries to select \ 
automatically the appropiate analyses based on the properties that appear in the \
assertions to be checked.' ).
current_pp_flags(  assert_ctcheck     , auto ).
valid_flag_values( assert_ctcheck     , member( _ , [auto, manual, off] ) ).

pp_flag(           testing_ctchecks   , 'when true add special clauses to the program to allow to perform automatic test case testing.' ).
current_pp_flags(  testing_ctchecks   , false ).
valid_flag_values( testing_ctchecks   , member( _ , [true, false] ) ).

pp_flag(           gen_certificate    , 'Generate certificate (for abstraction-carrying code).' ).
current_pp_flags(  gen_certificate    , off ).
valid_flag_values( gen_certificate    ,  member( _ , [on, off] ) ).


pp_flag( reduced_cert , 'Generate reduced certificate (for
abstraction-carrying code). It is still not available in the menu. By
default it is turned off.' ).

current_pp_flags(  reduced_cert   , off ).
valid_flag_values( reduced_cert   ,  member( _ , [on, off] ) ).

/*
pp_flag(           assert_rtcheck     , 'Decides whether to transform assertions into runtime-tests. Possible values are:
           off: do nothing
          main: rt-checks are applied _only_ to the main module
 main_and_deps: rt-checks are applied to the main modules and its dependecies without taking into account the system libraries
           all: rt-checks are applied to whole system' ).
current_pp_flags(  assert_rtcheck     , off ).
valid_flag_values( assert_rtcheck     , member( _ , [off, main, main_and_deps, all])). % none, pred, pp_assrt, pp_code
*/

pp_flag(           compiler           , 'An atom specifiying the compiler executable' ).
current_pp_flags(  compiler           , ciaoc ).
valid_flag_values( compiler           , atm(_) ).


pp_flag(           rt_instrumentation , 
'When exception is sent in run time, the exception handler can get more or less information. If the value of this flag is:

* low: only the run-time test are written.
* medium: additional instrumentation to inform the user which predicate violated the assertion is written.
* high: an stack will be shown when exception is caught (not yet implmented).' ).
current_pp_flags(  rt_instrumentation , low ).
valid_flag_values( rt_instrumentation , member( _ , [low, medium, high])). % none, pred, pp_assrt, pp_code

pp_flag(           rt_inline , 
'Decides between use run-time library or package that expands run-time test inline and save the metacall cost' ).
current_pp_flags(  rt_inline , off ).
valid_flag_values( rt_inline , member( _ , [on,off])).

pp_flag(           optim_comp         , 
 'Whether to perform optimizing compilation using the analysis information \
available (if any).' ).
current_pp_flags(  optim_comp         , none ).
valid_flag_values( optim_comp         , member(_,[none,byte_code,c_code]) ).

pp_flag(           collapse_ai_vers   , 
 'To output all the versions of call/success patterns inferred by analysis or \
just one version (summing-up all of them).' ).
current_pp_flags(  collapse_ai_vers  , on ).
valid_flag_values( collapse_ai_vers  , member(_,[off,on]) ).

pp_flag(           cost_approximation , 'Decides whether to compute upper or lower bounds in cost (steps) or size analysis').
current_pp_flags(  cost_approximation , upper ).
valid_flag_values( cost_approximation , member(_,[lower,upper]) ).

pp_flag(           dbdebug            , 'No Help Available' ).
current_pp_flags(  dbdebug            , off ).
valid_flag_values( dbdebug            , member(_,[off, on]) ).

% *** BE CAREFUL! you cannot do a findall( X, valid_flag_value(opt_unf_depth,X), L)!!!
pp_flag(           depth              , 'The maximum depth of abstractions \
                                         in analyses based on term depth' ).
current_pp_flags(  depth              , 1).
valid_flag_values( depth              , nnegint(_)).

pp_flag(           unf_depth          , 'Numeric value indicating the depth \
                                         limit for unfolding' ). 
current_pp_flags(  unf_depth          ,1 ).
valid_flag_values( unf_depth          , nnegint(_) ).

pp_flag(           unf_bra_fac        , 'Numeric value indicating the maximal \
                                         branching factor for non-leftmost 
                                         unfolding' ). 
current_pp_flags(  unf_bra_fac        ,1 ).
valid_flag_values( unf_bra_fac        , nnegint(_) ).

pp_flag(           sim_ari_exp        , 'Whether we should try to simplify \
arithmetic expressions.' ). 
current_pp_flags(  sim_ari_exp        , pre ).
valid_flag_values( sim_ari_exp        , member(_,[off, pre, post, both])  ).

pp_flag(           dump_ai            ,
'Decides whether to print analysis information about predicates \
(aka true assertions). If it is off dump_ai has no effect.' ).
current_pp_flags(  dump_ai            , on ).
valid_flag_values( dump_ai            , member(_,[off,on])).

pp_flag(           error_log          , 
 'Store error messages in a .err file for the module being preprocessed.' ).
current_pp_flags(  error_log          , off ).
valid_flag_values( error_log          , member(_,[off, on])).

pp_flag(           fixpoint           , 'Controls the fixpoint algorithm to use during analysis' ).
current_pp_flags(  fixpoint           , plai). 
valid_flag_values( fixpoint           , member(_,[plai, dd, di, check_di,
                                                  check_di2, check_di3, 
                                                  check_di4, check_di5,
						  poly_spec, check_reduc_di,
						  bu]) ).

pp_flag(           granularity_threshold, 
 'The threshold on computational cost at which parallel execution pays off' ).
current_pp_flags(  granularity_threshold, 959).
valid_flag_values( granularity_threshold, nnegint(_)).

pp_flag(           multi_success      , 'Whether to allow multivariance \
on successes' ).
current_pp_flags(  multi_success      , off).
valid_flag_values( multi_success      , member(_,[off,on])).

pp_flag(           multi_call      , 'Whether to allow multivariance \
on calls' ).
current_pp_flags(  multi_call      , on).
valid_flag_values( multi_call      , member(_,[off,on])).

pp_flag(           vers               , 'Whether to show multiple versions \
in analysis results' ).
current_pp_flags(  vers               , off ).
valid_flag_values( vers               , member(_,[off,on])).


pp_flag(           pp_info            , 
 'Whether to output analysis information for program points within clauses' ).
current_pp_flags(  pp_info            , off).
valid_flag_values( pp_info            , member(_,[off,on])).

pp_flag(           typedefs_ai        , 'No Help Available' ).
current_pp_flags(  typedefs_ai        , pred).
valid_flag_values( typedefs_ai        , member(_,[rule,pred])).

pp_flag(           typedefs_simp      , 'No Help Available' ).
current_pp_flags(  typedefs_simp      , on).
valid_flag_values( typedefs_simp      , member(_,[off,on])).


pp_flag(           widen              , 'Whether to perform widening' ).
current_pp_flags(  widen              , off).
valid_flag_values( widen              , member(_,[off,on])).

pp_flag(           reuse_fixp_id      , 'Whether to reuse fixpoint \
identifiers. Useful for comparing analysis results with existing ones' ).
current_pp_flags(  reuse_fixp_id      , off).
valid_flag_values( reuse_fixp_id      , member(_,[off,on])).

%% Intermodular analysis activator
pp_flag(           intermod     ,
 'Whether to apply inter-modular analysis techniques, \
such as recovering previous analysis info from related modules' ).
current_pp_flags(  intermod     , off).
valid_flag_values( intermod     , member(_, [off,on,auto])).

%% Menu options to select the modules to analyze in intermodular analysis
pp_flag(           mnu_modules_to_analyze,
 'Selects which modules are to be analyzed during \
intermodular analysis' ).
current_pp_flags(  mnu_modules_to_analyze, current).
valid_flag_values( mnu_modules_to_analyze, member(_, [current,all])).

%% print assertions in low-level format
pp_flag(           low_level_format,
 'Whether to print assertions in low-level format or not.' ).
current_pp_flags(  low_level_format     , off).
valid_flag_values( low_level_format     , member(_, [off,on])).

%% Use check assertions as trusts for imported modules
pp_flag(           use_check_assrt,
 'Whether to use check assertions from imported modules \
or imported libraries as if they were trust assertions.' ).
current_pp_flags(  use_check_assrt     , libraries).
valid_flag_values( use_check_assrt     , member(_, [off,on,libraries])).

%% How success policy is applied.
pp_flag(           success_policy     ,
 'The policy for obtaining success information \
for imported predicates during inter-modular analysis' ).
current_pp_flags(  success_policy     , over_all).
valid_flag_values( success_policy     , member(_, [over_best, over_first, over_all, 
	                                           top, under_first, under_best, 
						   under_all, bottom, bottom_up])). 

pp_flag(ext_policy  , 'Entries and successes for analysis.' ).
current_pp_flags( ext_policy  , assertions ).
valid_flag_values(ext_policy  , member(_,[assertions,registry])).


pp_flag(module_loading  , 'Modules to load at the same time.' ).
current_pp_flags(module_loading  , one ).
valid_flag_values(module_loading  , member(_,[one,all,threshold,threshold_scc])).

%% How the initial guess  is applied.
pp_flag(           initial_guess     ,
 'The policy for obtaining initial guess when computing the  \
analysis of a predicate from the current module' ).
current_pp_flags(  initial_guess     , bottom).
valid_flag_values( initial_guess     , member(_, [botfirst, botbest, botall, 
						   bottom])). 

pp_flag(           entry_policy       , 
 'The policy for obtaining entry call patterns \
for exported predicates during inter-modular analysis' ).
current_pp_flags(  entry_policy       , all).
valid_flag_values( entry_policy       , member(_,[all,top_level,force,force_assrt])). 

pp_flag(           interleave_an_check       , 
 'Whether to interleave analysis and checking \
during modular analysis or not' ).
current_pp_flags(  interleave_an_check       , off).
valid_flag_values( interleave_an_check       , member(_,[on,off])). 

pp_flag(           type_eval          , 
 'Whether to attempt concrete evaluation of types being inferred' ).
current_pp_flags(  type_eval          , off).
valid_flag_values( type_eval          , member(_,[on,off])).

pp_flag(           type_precision     , 
 'To use during type analysis only types defined at visible modules or \
also types inferred anew' ).
current_pp_flags(  type_precision     , all).
valid_flag_values( type_precision     , member(_,[defined,all])).

pp_flag(           type_output        , 
 'To output the types inferred for predicates in \
terms only of types defined at visible modules or including types inferred anew' ).
current_pp_flags(  type_output        , all).
valid_flag_values( type_output        , member(_,[defined,all])).


pp_flag(           global_control     , 'Determines the abstraction function to use during global control' ).
current_pp_flags(  global_control     , hom_emb).
valid_flag_values( global_control     , member(_,[off,offline,hom_emb,hom_emb_num,hom_emb_pt,hom_emb_t,dyn,id,inst])).

pp_flag(           poly_global_control, 'Determines the set of abstraction functions to use during poly controlled global control' ).
current_pp_flags(  poly_global_control, [id]).
valid_flag_values( poly_global_control     , list(_)).


pp_flag(           local_control      , 'Determines the unfolding rule to use during partial evaluation' ).
current_pp_flags(  local_control      , off).
valid_flag_values( local_control      , member(_,[off, orig, inst, det, det_la,
	                                          depth, first_sol,first_sol_d,
						  all_sol, hom_emb,hom_emb_anc,
						  hom_emb_as, df_hom_emb_as, 
						  df_tree_hom_emb, df_hom_emb,
						  decompile,df_hom_emb_as_orig])).

pp_flag(           poly_local_control , 'Determines the set of unfolding rules to use during poly controlled partial evaluation' ).
current_pp_flags(  poly_local_control , [[local_control(det),comp_rule(leftmost),unf_bra_fac(1)],[local_control(df_hom_emb_as),comp_rule(bind_ins_jb),unf_bra_fac(0)]]).
valid_flag_values( poly_local_control      , list(_)).

pp_flag(           comp_rule   , 'The computation rule for the selection of atoms in a goal' ).
current_pp_flags(  comp_rule   , bind_ins_jb).
valid_flag_values( comp_rule   , member(_,[leftmost,safe_jb,bind_ins_jb,no_sideff_jb,jump_builtin,eval_builtin,local_emb]) ).


pp_flag(           rem_use_cls        , 'Decides whether to remove useless clauses' ).
current_pp_flags(  rem_use_cls        , off).
valid_flag_values( rem_use_cls        , member(_,[off, pre, post, both])).

pp_flag(           use_new_rdisj      , 'Decides whether to use the new remove disjuntion transformation' ).
current_pp_flags(  use_new_rdisj      , off).
valid_flag_values( use_new_rdisj      , member(_,[on, off])).

pp_flag(           output_rebuild_djcd, 'Decides whether to rebuild conditions and conjuntions when generating the output.' ).
current_pp_flags(  output_rebuild_djcd, off).
valid_flag_values( output_rebuild_djcd, member(_,[on, off])).

pp_flag(           output_show_tautologies, 'Decides whether to show tautologic assertions or not in the output.' ).
current_pp_flags(  output_show_tautologies, off ).
valid_flag_values( output_show_tautologies, member( _, [on, off] ) ).


pp_flag(           part_conc          , 
'The kind of partial concretization to be performed' ).
current_pp_flags(  part_conc          , off).
valid_flag_values( part_conc          , member(_,[off,mono,multi])).

pp_flag(           abs_spec_defs      , 
'Whether to exploit abstract substitutions while obtaining specialized definitions' ).
current_pp_flags(  abs_spec_defs      , off).
valid_flag_values( abs_spec_defs      , member(_,[off,rem,exec,all])).

pp_flag(           filter_nums        , 
'Whether to filter away numbers during global control in partial evaluation' ).
current_pp_flags(  filter_nums        , off).
valid_flag_values( filter_nums        , member(_,[off,on])).

pp_flag(           hom_emb_nums        , 
'Whether to consider all numbers embedded by other numbers during local control' ).
current_pp_flags(  hom_emb_nums        , off).
valid_flag_values( hom_emb_nums        , member(_,[off,on,types])).

pp_flag(           exec_unif          , 'Whether to execute unifications during specialization time or not' ).
current_pp_flags(  exec_unif          , on).
valid_flag_values( exec_unif          , member(_,[off,on])).

pp_flag(           pres_inf_fail      , 'Whether Infinite Failure should be preserved in the specialized program' ).
current_pp_flags(  pres_inf_fail      , off).
valid_flag_values( pres_inf_fail      , member(_,[off,on])).

pp_flag(           spec_postproc      , 'Whether to Post Process the Results of Analysis' ).
current_pp_flags(  spec_postproc      , on).
valid_flag_values( spec_postproc      , member(_,[off,on])).

pp_flag(           min_crit           , 'Select Minimization Criterion' ).
current_pp_flags(  min_crit           , none).
valid_flag_values( min_crit           , member(_,[none,equal,codemsg,nobindings,bindings,residual])).

pp_flag(           global_trees       , 'Whether to use Global Trees' ).
current_pp_flags(  global_trees       , off).
valid_flag_values( global_trees       , member(_,[off,on])).

pp_flag(           an_orig_def        , 'Whether to Analyze Original Definition' ).
current_pp_flags(  an_orig_def        , off).
valid_flag_values( an_orig_def        , member(_,[off,on])).

pp_flag(           inter_opt_arg_filt , 'Whether to Perform Argument Filtering' ).
current_pp_flags(  inter_opt_arg_filt , on).
valid_flag_values( inter_opt_arg_filt , member(_,[off,on])).

pp_flag(           normalize          , 'Whether to Normalize the Program' ).
current_pp_flags(  normalize          , off).
valid_flag_values( normalize          , member(_,[off,on])).

pp_flag(           prog_lang          , 'Programming Language' ).
current_pp_flags(  prog_lang          , ciao).
valid_flag_values( prog_lang          , member(_,[ciao,java])).


pp_flag(           global_scheduling  , 
 'Global scheduling policy to be used in intermodular analysis' ).
current_pp_flags(  global_scheduling  , naive_bottom_up).
valid_flag_values( global_scheduling  , member(_,[depth_first,
	abs_depth_first, naive_top_down, naive_bottom_up, 
	top_down_preanalysis, bottom_up_preanalysis])).

pp_flag(           widencall          , 'Type of widening performed on abstract procedure calls.' ).
current_pp_flags(  widencall          , com_child).
valid_flag_values( widencall          , member(_,[com_child,onlyfixp,off])).

pp_flag(           variants           , 'Whether to keep analysis variants or not (multi-variant analysis).' ).
current_pp_flags(  variants           , off).
valid_flag_values( variants           , member(_,[off,on])).

pp_flag(           tmp_dir            , 
'Directory for temporary files, or \'source\' if temporary files 
are to be stored where source files reside.' ).
current_pp_flags(  tmp_dir            , source).
valid_flag_values( tmp_dir            , tmp_dir( _ ) ).

pp_flag(           asr_dir            , 
'Directory for asr files, or \'source\' if asr files 
are to be stored where source files reside.' ).
current_pp_flags(  asr_dir            , source ).
valid_flag_values( asr_dir            , asr_dir( _ ) ).

pp_flag(           process_libraries  , 
'Whether to process libraries or not during intermodular \
analysis / specialization.' ).
current_pp_flags(  process_libraries  , off).
valid_flag_values( process_libraries  , member(_,[off,on,no_engine]) ).

pp_flag(           ass_not_stat_eval  , 'When runing compile-time checks, \
some assertions cannot be marked as checked or false. This flag decides \
what do do in those situations: nothing, report a warning or report an error.' ).
current_pp_flags(  ass_not_stat_eval  , warning ).
valid_flag_values( ass_not_stat_eval  , member(_,[off,warning,error])).

pp_flag(           verbose_ctchecks  , 'When this flag is set to on, \
all check assertions which are verified are printed' ).
current_pp_flags(  verbose_ctchecks  , off ).
valid_flag_values( verbose_ctchecks  , member(_,[off,on])).

pp_flag(           verbosity , 'This flag controls, \
the verbosity of ciaopp execution' ).
current_pp_flags(  verbosity , low ).
valid_flag_values( verbosity  , member(_,[off,low,high,very_high])).

pp_flag(           pred_ctchecks  , 'This flags controls whether, \
to perform predicate level compile-time checking and the algorithm to use' ).
current_pp_flags(  pred_ctchecks  , on ).
valid_flag_values( pred_ctchecks  , member( _ , [off,on,on_succ])).

pp_flag(           pp_ctchecks  , 'This flags controls whether, \
to perform program point compile-time checking and the algorithm to use' ).
current_pp_flags(  pp_ctchecks  , old ).
valid_flag_values( pp_ctchecks  , member(_,[none,old,new])).

pp_flag(           clique_widen , 'This flag controls whether to perform \
widening sharing via cliques.' ).
current_pp_flags(  clique_widen , off ).
valid_flag_values( clique_widen , member( _ ,[off,amgu,plai_op])).

pp_flag(           clique_widen_type, 'This flags controls the type of the widening \
sharing based on cliques.' ).
current_pp_flags(  clique_widen_type , cautious ).
valid_flag_values( clique_widen_type , member(_,[inter_1,inter_2,cautious])).

pp_flag(           clique_widen_ub , 'This flag controls the (upper bound) threshold \ 
used for the widening sharing via cliques.').
current_pp_flags(  clique_widen_ub , 250 ).
valid_flag_values( clique_widen_ub , int(_)).

pp_flag(           clique_widen_lb , 'This flag controls the (lower bound) threshold \ 
used for the widening sharing via cliques.').
current_pp_flags(  clique_widen_lb , 200 ).
valid_flag_values( clique_widen_lb , int(_)).

pp_flag(           bshare_option  , 'This flag controls the bit-based representation  \
used by the bshare abstract domain.' ).
current_pp_flags(  bshare_option  , tNSH ).
valid_flag_values( bshare_option  , member(_,[bSH,tSH,tNSH])).

pp_flag(           oo_types_dyn_info, 'When analyzing types in Java-like \ 
  programs, a value [off] relies on variable declaration; [on] forces more \
  precise analysis').  
current_pp_flags(   oo_types_dyn_info,on).
valid_flag_values( oo_types_dyn_info  , member(_,[off,on])).


pp_flag(           fact_info  , 'When this flag is set to on, \
program-point analysis info in facts is stored and output.' ).
current_pp_flags(  fact_info  , off ).
valid_flag_values( fact_info  , member(_,[off,on])).

pp_flag(           ct_modular  , 'When this flag is set to all, \
all the modules are CT checked. Otherwise, one module only.' ).
current_pp_flags(  ct_modular  , curr_mod ).
valid_flag_values( ct_modular  , member(_,[all,curr_mod])).

pp_flag(ct_ext_policy  , 'Entries and successes for CT checking.' ).
current_pp_flags( ct_ext_policy  , assertions ).
valid_flag_values(ct_ext_policy  , member(_,[assertions,registry])).


pp_flag(ct_regen_reg  , 'Whether to regenerate analysis registry \
 while CT checking.' ).
current_pp_flags( ct_regen_reg  , off ).
valid_flag_values(ct_regen_reg  , member(_,[off,on])).


pp_flag(entry_point , 'Whether to use calls assertions as entries.' ).
current_pp_flags(entry_point, entry).
valid_flag_values(entry_point, member(_,[entry,calls,entry_calls])).


pp_flag(ct_mod_iterate  , 'Whether to iterate over all modules \
 to reach the global fixpoint, while CT checking.' ).
current_pp_flags( ct_mod_iterate  , on ).
valid_flag_values(ct_mod_iterate  , member(_,[off,on])).

pp_flag(multivariant_ctchecks  , 'Whether to use multivariant analysis info \
 while CT checking at predicate level.' ).
current_pp_flags( multivariant_ctchecks  , off ).
valid_flag_values(multivariant_ctchecks  , member(_,[off,on])).

pp_flag(ctchecks_output  , 'Whether to generate output after CT checking.' ).
current_pp_flags( ctchecks_output  , off ).
valid_flag_values(ctchecks_output  , member(_,[off,on])).


pp_flag(run_diagnosis  , 'Whether to start diagnosis at program-point \
 CT checking.' ).
current_pp_flags(run_diagnosis  , off ).
valid_flag_values(run_diagnosis  , member(_,[off,on])).

pp_flag(memo_ignore,'List of modules whose memo assertions will be ignored').
current_pp_flags(memo_ignore,none).
valid_flag_values(memo_ignore,member(_,[none,all])).
valid_flag_values(memo_ignore,list(_)).

pp_flag(bind_ins_ignore,'List of modules whose bind_ins assertions will be ignored').
current_pp_flags(bind_ins_ignore,none).
valid_flag_values(bind_ins_ignore,member(_,[none,all])).
valid_flag_values(bind_ins_ignore,list(_)).

pp_flag(error_free_ignore,'List of modules whose error_free assertions will be ignored').
current_pp_flags(error_free_ignore,none).
valid_flag_values(error_free_ignore,member(_,[none,all])).
valid_flag_values(error_free_ignore,list(_)).

pp_flag(sideff_ignore,'List of modules whose sideff assertions will be ignored').
current_pp_flags(sideff_ignore,none).
valid_flag_values(sideff_ignore,member(_,[none,all])).
valid_flag_values(sideff_ignore,list(_)).

pp_flag(eval_ignore,'List of modules whose eval assertions will be ignored').
current_pp_flags(eval_ignore,none).
valid_flag_values(eval_ignore,member(_,[none,all])).
valid_flag_values(eval_ignore,list(_)).

pp_flag(pe_type_ignore,'List of modules whose pe_type assertions will be ignored').
current_pp_flags(pe_type_ignore,none).
valid_flag_values(pe_type_ignore,member(_,[none,all])).
valid_flag_values(pe_type_ignore,list(_)).

pp_flag(gen_emb_atm,'It decides whether we pass to global control a generalization of the embedded atom with its ancestor and its relatives').
current_pp_flags(gen_emb_atm,none).
valid_flag_values(gen_emb_atm,member(_,[none,ancestor,relatives,offline])).

%pp%  pp_flag(           ct_mod_ana  , 'Whether to perform intermodular analysis \
%pp%  while CT checking.' ).
%pp%  current_pp_flags(  ct_mod_ana  , curr_mod ).
%pp%  valid_flag_values( ct_mod_ana  , member(_,[curr_mod, all_rel_mods, monolithic])).

%pp%  pp_flag(           ct_success_policy  , 'Form where take the info about imported \
%pp%  predicates (best available - analysis + assertions, or assertions)' ).
%pp%  current_pp_flags(  ct_success_policy  , assrt ).
%pp%  valid_flag_values( ct_success_policy  , member(_,[assrt,best])).
%pp%  
%pp%  pp_flag(           ct_entry_policy , 'Controls the source of call patterns \
%pp%  of exported predicates (best available - analysis + assertions, or assertions)' ).
%pp%  current_pp_flags(  ct_entry_policy  , assrt ).
%pp%  valid_flag_values( ct_entry_policy  , member(_,[assrt,best])).

tmp_dir(source).
tmp_dir(Dir) :-
	atm(Dir).

asr_dir(source).
asr_dir(Dir) :-
	atm(Dir).


% the following three should be put in common with infer_dom:knows_of/2
% and multifile:analysis/1
modetypeanalysis(X):- modeanalysis(X).
modetypeanalysis(X):- typeanalysis(X), X \== none.


% to allow no mode analysis
modeanalysis(none).

% groundness/sharing
modeanalysis(pd).
modeanalysis(pdb).
modeanalysis(def).
modeanalysis(gr).
modeanalysis(null).
modeanalysis(share).
modeanalysis(shareson).
modeanalysis(shfr).
modeanalysis(shfrson).
modeanalysis(shfrnv).
modeanalysis(son).
modeanalysis(share_amgu).
modeanalysis(share_clique).
modeanalysis(share_clique_1).
modeanalysis(share_clique_def).
modeanalysis(sharefree_amgu).
modeanalysis(shfrlin_amgu).
modeanalysis(sharefree_clique).
modeanalysis(sharefree_clique_def).
% structure
modeanalysis(aeq).
modeanalysis(depth).
modeanalysis(path).
% constraints
modeanalysis(difflsign).
modeanalysis(fr).
modeanalysis(frdef).

% types
% see typeanalysis(X).
typeanalysis(none).
typeanalysis(eterms).
typeanalysis(ptypes).
typeanalysis(svterms).
typeanalysis(terms).
typeanalysis(deftypes).

% numeric analyses
numericanalysis(none).
numericanalysis(polyhedra).
numericanalysis(lsign).


the_same_as( X , P ) :-
	valid_flag_value( P , X ).

valid_alpha(X):-
	float(X),
	X>=0,
	X=<1.

sublist2( X , [L] ) :-
	var( X ), !,
	member( X , L ).

sublist2( X , L ) :-
	sublist( X , L ).

%%------------------------------------------------------------------------
:- pred pp_flag(Name,Help) 
	# "@var{Name} is a valid preprocess flag.@var{Help} is a
          description of what @var{Name} does.".

:- set_prolog_flag( multi_arity_warnings , off ).

%%------------------------------------------------------------------------

% LpDoc is not able to handle this as a reexport:
%:- doc(pp_flag/1,"Valid flags:  @include{preprocess_flags.lpdoc}").
:- regtype pp_flag( Flag )
	# "@var{Flag} is a valid preprocessor flag.".
:- doc(pp_flag/1," The list of valid flags and their values is
          as follows: @include{preprocess_flags.lpdoc}").

:- entry pp_flag( Flag ) : var.
:- entry pp_flag( Flag ) : atm.

pp_flag(output_info).
pp_flag(ctchecks_value_evaluation). %[LD]
pp_flag( ctchecks_intervals). %[LD]
pp_flag(dump_pred).
pp_flag(dump_pp).
pp_flag(dump_ext).
pp_flag(inter_all).
pp_flag(inter_javall).
pp_flag(inter_ana).
pp_flag(inter_javana).
pp_flag(inter_optimize).
pp_flag(para_ann).
pp_flag(para_iap).
pp_flag(para_local).
pp_flag(para_grain).
pp_flag(para_grain_res).
pp_flag(para_cost).
pp_flag(menu_level).
pp_flag(menu_java_level).
pp_flag(menu_last_config).
pp_flag(menu_config_name).
pp_flag(check_config_ana).
pp_flag(modes).
pp_flag(java_modes).
pp_flag(types).
pp_flag(java_types).
pp_flag(ana_num).
pp_flag(ana_nf).
pp_flag(ana_det).
pp_flag(ana_cost).
pp_flag(java_ana_cost).
pp_flag(ana_size).
pp_flag(complexity_output).
pp_flag(peval_ana).
pp_flag(spec_poly).
pp_flag(poly_fitness).
pp_flag(pcpe_bounded_size).
pp_flag(poly_filter_equiv).
pp_flag(poly_strategy).
pp_flag(poly_pruning).
pp_flag(poly_depth_lim).
pp_flag(polyvar_pcpe).
pp_flag(poly_modes).
pp_flag(aggressivity).
pp_flag(pcpe_evaltime).
pp_flag(assert_ctcheck).
pp_flag(testing_ctchecks).
pp_flag(gen_certificate).
pp_flag(reduced_cert).
pp_flag(compiler).
pp_flag(rt_instrumentation).
pp_flag(rt_inline).
pp_flag(optim_comp).
pp_flag(collapse_ai_vers).
pp_flag(cost_approximation).
pp_flag(dbdebug).
pp_flag(depth).
pp_flag(unf_depth).
pp_flag(unf_bra_fac).
pp_flag(sim_ari_exp).
pp_flag(dump_ai).
pp_flag(error_log).
pp_flag(fixpoint).
pp_flag(granularity_threshold).
pp_flag(multi_success).
pp_flag(multi_call).
pp_flag(vers).
pp_flag(pp_info).
pp_flag(typedefs_ai).
pp_flag(typedefs_simp).
pp_flag(widen).
pp_flag(reuse_fixp_id).
pp_flag(intermod).
pp_flag(mnu_modules_to_analyze).
pp_flag(low_level_format).
pp_flag(use_check_assrt).
pp_flag(success_policy).
pp_flag(ext_policy).
pp_flag(module_loading).
pp_flag(initial_guess).
pp_flag(entry_policy).
pp_flag(interleave_an_check).
pp_flag(type_eval).
pp_flag(type_precision).
pp_flag(type_output).
pp_flag(global_control).
pp_flag(poly_global_control).
pp_flag(local_control).
pp_flag(poly_local_control).
pp_flag(comp_rule).
pp_flag(rem_use_cls).
pp_flag(use_new_rdisj).
pp_flag(output_rebuild_djcd).
pp_flag(output_show_tautologies).
pp_flag(part_conc).
pp_flag(abs_spec_defs).
pp_flag(filter_nums).
pp_flag(hom_emb_nums).
pp_flag(exec_unif).
pp_flag(pres_inf_fail).
pp_flag(spec_postproc).
pp_flag(min_crit).
pp_flag(global_trees).
pp_flag(an_orig_def).
pp_flag(inter_opt_arg_filt).
pp_flag(normalize).
pp_flag(prog_lang).
pp_flag(global_scheduling).
pp_flag(widencall).
pp_flag(variants).
pp_flag(tmp_dir).
pp_flag(asr_dir).
pp_flag(process_libraries).
pp_flag(ass_not_stat_eval).
pp_flag(verbose_ctchecks).
pp_flag(verbosity).
pp_flag(pred_ctchecks).
pp_flag(pp_ctchecks).
pp_flag(clique_widen).
pp_flag(clique_widen_type).
pp_flag(clique_widen_ub).
pp_flag(clique_widen_lb).
pp_flag(bshare_option).
pp_flag(oo_types_dyn_info).
pp_flag(fact_info).
pp_flag(ct_modular).
pp_flag(ct_ext_policy).
pp_flag(ct_regen_reg).
pp_flag(entry_point).
pp_flag(ct_mod_iterate).
pp_flag(multivariant_ctchecks).
pp_flag(ctchecks_output).
pp_flag(run_diagnosis).
pp_flag(memo_ignore).
pp_flag(bind_ins_ignore).
pp_flag(error_free_ignore).
pp_flag(sideff_ignore).
pp_flag(eval_ignore).
pp_flag(pe_type_ignore).
pp_flag(gen_emb_atm).


:- set_prolog_flag( multi_arity_warnings , on ).

%%------------------------------------------------------------------------
:- regtype flag_value(V)  
	# "@var{V} is a value for a flag.".

flag_value( X ) :- atm( X ).
flag_value( X ) :- list( X , atm ).


%%------------------------------------------------------------------------
%:- entry valid_flag_values( Name , Value ) : atm * var.

:- pred valid_flag_values(Name,Value_Checker) 
	:: pp_flag * callable
	# "@var{Value_Checker} is a goal that checks that a value given as
           first argument of this term is a valid value for @var{Name}.".


%------------------------------------------------------------------------
:- pred current_pp_flag(Name,?Value)
	: pp_flag(Name) => valid_flag_value(Name,Value)
	# "Preprocess flag @var{Name} has the value @var{Value}.".

current_pp_flag(analysis_info,Value):- !, current_pp_flag(dump_ai,Value).
current_pp_flag(point_info,Value):- !, current_pp_flag(pp_info,Value).
current_pp_flag(part_concrete,Value):- !, current_pp_flag(part_conc,Value).
%
current_pp_flag(Name,Value):-
	current_pp_flags(Name,Value).

%------------------------------------------------------------------------
:- pred set_pp_flag(Name,Value) 
	: ( pp_flag(Name) , valid_flag_value(Name,Value) )
	# "Sets @var{Value} for preprocessor flag @var{Name}.".

set_pp_flag(analysis_info,Value):- !, set_pp_flag(dump_ai,Value).
set_pp_flag(point_info,Value):- !, set_pp_flag(pp_info,Value).
set_pp_flag(part_concrete,Value):- !, set_pp_flag(part_conc,Value).
%
set_pp_flag(Name,Value):-
	ground(Name),
	ground(Value),
	pp_flag(Name,_),
	valid_flag_value(Name,Value),!,  % checking name and value existence.
	data_facts:retract_fact(current_pp_flags(Name,_)),
	data_facts:assertz_fact(current_pp_flags(Name,Value)).

%%------------------------------------------------------------------------
%:- entry valid_flag_value( Name , Value ) : atm * var.
%:- entry valid_flag_value( Name , Value ) : atm * flag_value.

:- prop valid_flag_value(Name,Value) 
	: pp_flag * flag_value
	# "@var{Value} is a valid value for preprocessor flag @var{Name}.".

% more kludges
valid_flag_value(analysis_info,Value):- 
	!,
	valid_flag_value(dump_ai,Value).

valid_flag_value(Name,Value):-
	valid_flag_values(Name,ValGen),
	arg(1,ValGen,Value),           
	call(ValGen).

:- data old_flag/2.

%:- entry push_pp_flag( Flag , Value ) : atm * flag_value.
:- true pred push_pp_flag(Flag, Value)
	   : ( pp_flag(Flag), valid_flag_value(Flag,Value) )
	# "Sets @var{Value} for preprocessor flag @var{Flag}, storing the current
	   value to restore it with @pred{pop_pp_flag/1}.".

push_pp_flag(analysis_info,Value):- !, push_pp_flag(dump_ai,Value).
push_pp_flag(point_info,Value):- !, push_pp_flag(pp_info,Value).
push_pp_flag(part_concrete,Value):- !, push_pp_flag(part_conc,Value).
%
push_pp_flag(Flag, NewValue) :-
        nonvar(Flag),
        current_pp_flag(Flag, OldValue),
	set_pp_flag(Flag,NewValue),
        data_facts:asserta_fact(old_flag(Flag, OldValue)).

%:- entry pop_pp_flag( Flag ) : atm.
:- true pred pop_pp_flag(Flag) : pp_flag
	# "Restores the value of the preprocessor flag @var{Flag}
	   previous to the last non-canceled @pred{push_pp_flag/2} on
	   it.".

pop_pp_flag(analysis_info):- !, pop_pp_flag(dump_ai).
pop_pp_flag(point_info):- !, pop_pp_flag(pp_info).
pop_pp_flag(part_concrete):- !, pop_pp_flag(part_conc).
%
pop_pp_flag(Flag) :-
        nonvar(Flag),
        data_facts:retract_fact(old_flag(Flag, OldValue)),
        set_pp_flag(Flag,OldValue).

%-------------------------------------------------------------------------

:- multifile dump_flags_list/2.

:- entry dump_flags( Flag ) : atm.

:- pred dump_flags(Name) 
	: atm

	# "@var{Name} represent the list of flags to be dumped. To
          associate a name (key) with a list, use
          @pred{dump_flags_list}.".


dump_flags( Name ) :-
	dump_flags_list( Name , List ),
	dump_all_flags( List ).

dump_flags( Name ) :-
	message( error , ['Flag list ' , Name , ' unkown']).



dump_flags_list( all , L ) :-
	findall( X , pp_flag(X,_), L ).



dump_all_flags( [A|B] ) :-
	current_pp_flag( A , V ),
	!,
	display( A ) , display( ' = ' ) , display( V ) , nl ,
	dump_all_flags( B ).

dump_all_flags( [A|B] ) :-
	display( 'unkown flag ' ) , display( A ), nl,
	dump_all_flags( B ).
	
dump_all_flags( [] ).


%%------------------------------------------------------------------------

:- use_module(library(menu(menu_generator)), 
	       [ get_menu_configs/1 ] ).

is_menu_config( V ) :-
	get_menu_configs( X ),
	member( V , [ none | X ] ).

%%------------------------------------------------------------------------


:- entry save_config( Flag ) : atm.

:- pred save_config( Name ) 
	: atm
	# "Save the current flags configuration under the @var{Name} key.".

save_config( Name ) :-
	findall( (A=B) , current_pp_flags( A , B ) , L ),
	save_flags_list( Name , L ).

save_flags_list( Name , List ) :-
	( persdbrt:retract_fact( config( Name , _ ) ) , fail ; true ),
	persdbrt:assertz_fact( config( Name , List ) ).


:- entry remove_config( Flag ) : atm.

:- pred remove_config( Name ) 
	: atm
	# "Remove the configuration stored with the @var{Name} key.".


remove_config( Name ) :-
	persdbrt:retract_fact( config( Name , _ ) ),
	fail.

remove_config( _Name ).



:- pred restore_config( Name ) : atm( Name )

# "Restores the set of flags saved previously under the name of @var{Name}.".

restore_config( Name ) :-
	config( Name , L ),
	restore_flags_list( L ).

%What happens with non existing flags?
restore_flags_list( [] ) :- !.
restore_flags_list( [(A=B)|As] ) :-
	(set_pp_flag(A,B)->true;true),
	 restore_flags_list( As ).


:- pred show_configs 
	# "Show all stored configs.".

show_configs :-
	findall( Name , config( Name , _ ) , L ),
	display( L ), nl.


:- entry show_config( Flag ) : atm.

:- pred show_config( C )
	: atm
	# "Show specific configuration values pointed by @var{C} key.".
	

show_config( Name ) :-
	config( Name , F ),
	show_config_list( F ),
	fail.
show_config( _ ).


show_config_list( [] ) :- !.
show_config_list( [A|B] ) :-
	write( A ), nl,
	show_config_list( B ).

:- pop_prolog_flag(unused_pred_warnings).
