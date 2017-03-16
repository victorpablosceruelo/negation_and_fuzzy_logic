:- module( auto_interface , [
	                  auto_analyze/1,
			  auto_optimize/1,
			  auto_check_assert/1,
	                  auto_analyze/2,
			  auto_optimize/2,
			  auto_check_assert/2,
			  customize/0,
			  customize/1,
			  customize_and_preprocess/0,
			  customize_and_preprocess/1,
			  customize_but_dont_save/1,
			  set_menu_level/1,
			  current_menu_level/1,
			  again/0,
			  set_last_file/1,
			  get_last_file/1,
			  clean_aux_files/1,
                          select_modules/1,
%			  ,opt_menu_branch/2
%			  ,all_menu_branch/2
			  % JNL
			  customize_java/1,
			  customize_and_preprocess_java/1
		       ], [assertions,
		           api(api_menu),
		           fsyntax
			   ] ).


:- use_module(ciaopp(driver), [
	                          acheck_summary/1,
	                          acheck/0,
				  module/1,
				  transform/1,
				  analyze/1
				] ).

:- use_module(ciaopp(printer), [
				  output/0,
				  output/1
				] ).

:- use_module(ciaopp(preprocess_flags), [
				            pop_pp_flag/1,
					    current_pp_flag/2,
					    valid_flag_values/2,
					    pp_flag/2,
					    push_pp_flag/2,
					    set_pp_flag/2,
					    valid_flag_value/2
					 ] ).

%% *** These two for ACC, need to be revised MH
:- use_module(ciaopp(p_unit(p_dump)), [
				            dump/1,
					    restore/1
					 ] ).

:- use_module(ciaopp(plai(fixpo_ops)), [
				            store_previous_analysis/1
					 ] ).

:- use_module(plai(acc_ops), [remove_irrelevant_entries/0]).


:- use_module(auto_interface(optim_comp), [optim_comp/1]).

%% For modular checking
:- use_module(plai(intermod), [
	                         inductive_ctcheck_summary/3,
 				 auto_ctcheck_summary/2, auto_ctcheck_summary/3,
				 modular_analyze/2, valid_mod_analysis/1,
				 cleanreg/1, get_modules_analyzed/1
			     ]).

:- use_module(infer(infer_db),        [domain/1]).
:- use_module(program(assrt_db),      [assertion_read/9, assertion_body/7]).
:- use_module(program(p_unit),        [native_prop/2]).
:- use_module(program(itf_db),        [curr_file/2]).
:- use_module(program(aux_filenames), [is_library/1]).
:- use_module(infer(infer_dom),       [knows_of/2]).
:- use_module(library(lists),         [append/3]).
:- use_module(library(aggregates),    [findall/3]).
:- use_module(library(prolog_sys),    [statistics/2]).
:- use_module(library(system),        [file_exists/2]).
:- use_module(library(messages)).
:- reexport(library(menu(menu_generator)) , [
	                            get_menu_configs/1,
				    save_menu_config/1,
				    remove_menu_config/1,
				    restore_menu_config/1,
				    show_menu_configs/0,
				    show_menu_config/1,
				    get_menu_flag/3,
				    set_menu_flag/3
				      ] ).
:- use_module(library(prompt),[prompt_for_default/2]).
:- use_module(library(filenames), [basename/2]).


:- doc(bug, "1 commented out the question for error file since we
	are generating it in any case (not yet implemented)").

:- doc(bug, "2 when auto_cthecks has the value 'on' (instead of 'auto'), 
	the result of compile_time checking is not fully handled yet").

% HOW TO ADD A MENU QUESTION
% --------------------------
%
% These are the steps that you have to follow to add a question to the
% menu. Basically the idea you have to keep in mind is that every question
% in the menu is composed by a _title_ (the text that user sees when 
% interacting with the menu) a _flag_ (from preproces_flags.pl) and a
% _default value_ for the flag. To identify in "which" menu branch (i.e.,
% if it is the optimization menu, or the analyze menu...) the question
% should be asked, an atom (or functor, explained further) is used
% to classify all questions under the "menu branch". 
%
% So lets say for example that we want to add a question with the
% title 'This is my question' and the flag, my_flag, with default value,
% defvalue to the optimization menu. Our first point is to locate
% what is the atom that identifies the menu branch. In our case the
% atom is 'opt'. If we read the menu line:
%
% opt      , 'Select Optimize'              # inter_optimize - spec.
%
% FOOTNOTE:
% (The question that will appear on the screen corresponding to that
% sentence is:
%
% Select Optimize:            [none, spec, parallelize, slice, poly_spec]
%                             (spec) ? 
%
% NOTE that the values between [ and ] are the values of the 
% inter_opt flag).
%
% we will notice that the opt menu asks for the inter_optimize flag
% (which has spec as a default value). But then, where is our optimize
% branch? Reading the values of inter_optimize we will realize that:
%
% valid_flag_values( inter_optimize , 
%    member( _ , [none,spec,parallelize, slice,poly_spec] ) ).
%
% So, after the opt menu several menu branches can be taken. The glue
% between the different branches is the predicate customize/1:
%
% customize( optimize ) :-
% 	!,
% 	menu( opt , false ),
% 	get_menu_flag( opt , inter_optimize , NM ),
% 	menu_level( L ),
% 	NO =.. [ NM , L ],
% 	customize( NO ).
%
% (NOTE: The predicate menu/2 is the one which reads the menu
% declarations and prints the questions on the screen). So the
% conclusion is that opt menu follows the menu branch which has
% exactly the same name as the values of inter_optimize. At this point
% we have to decide in which sub-branch we want our menu question.
% Lets say that we are interested in the spec branch. The spec branch,
% like many other branches, shares questions with the slice branch,
% that is because instead of an atom we use something like:
%
% ~spsl    , 'Select Abs Specialization'    # spec_poly - off.
%
% which with the functions package is expanded to spec and slice
% atoms.  Now that once we have located the menu declarations, we only
% have to copy whatever question and paste it under the question we
% want our to appear. The format is:
%
% menu_atom , title # flag - default_value.
%
% Two points make things a bit more complex:
% 1.- naive/expert menus
% 2.- Guards
%
% The first point is clear and easy to explain. Instead of an
% atom for the branch, we specify a functor like:
%
% menu_atm( NUMBER )
%
% where NUMBER indicates the menu level (0 for naive, 1 for expert... in
% the future it could be 0=naive, 1=medium, 2=expert). This explains
% lines like:
%
% ~lt(1), 'Multivariant Success' # multi_success - off <- ana_b.
%
% The second point is a little bit more complex. A guard is the field
% after '<-'. The guard itself receives an argument that xis a list
% with the flags that have been selected for asking. The list elements
% are of the form flag=value. If value is a variable it means that the
% question corresponding to that flag has been selected but it has not
% been asked yet. Else, value is the one the user has selected.
%
% For this reason guards are like:
%
% guard cct( X ) :- 
% 	member( assert_ctcheck=Y , X ), 
% 	Y == on.
%
% (NOTE the == !!!)
%
% The "keyword" guard is needed before the guard predicate because the
% guards are rewritten in a special language that allows them to run
% backwards (with a free variable as argument), necessary for the
% conversion to javascript menus.
%
% So in summary, the steps to add a question are:
% 1.- Add a flag to the preprocess_flags.pl file
% 2.- Find out were your question has to take place
% 3.- Copy and paste under the question you would like yours to appear.
% 4.- Add the necessary guards if needed.


:- doc(title,"The CiaoPP user menu interface").
:- doc(author, "David Trallero Mena").

:- doc( doinclude, get_menu_configs/1    ).
:- doc( doinclude, save_menu_config/1    ).
:- doc( doinclude, restore_menu_config/1 ).
:- doc( doinclude, show_menu_configs/0   ).
:- doc( doinclude, show_menu_config/1    ).
:- doc( doinclude, remove_menu_config/1  ).

:- doc( hide, analysis/1           ).
:- doc( hide, menu_default/3       ).
:- doc( hide, menu_opt/6           ).
:- doc( hide, set_menu_level/1     ).
:- doc( hide, current_menu_level/1 ).
:- doc( hide, set_last_file/1      ).
:- doc( hide, get_last_file/1      ).

:- doc( module, 
"This module defines a simplified user-level interface for CiaoPP.  It
 complements the more expert-oriented interface defined in @ref{The CiaoPP
 low-level programming interface}. This is also the interface called
 by the shortcuts available in menus and toolbars in the emacs mode.

 The idea of this interface is to make it easy to perform some
 fundamental, prepackaged tasks, such as checking assertions in
 programs (i.e., types, modes, determinacy, non-failure, cost, etc.),
 performing optimizations such as specialization and parallelization,
 and performing several types of analysis of the program.  The results
 can be observed as new or transformed assertions and predicates in a
 new version of the program.

 In order to use CiaoPP, the user must provide two kinds of
 information: first, a number of preprocessing options must be set if
 necessary in order to configure the system; and then, the action that
 has to be done must be selected (analysis, assertion checking,
 optimization).  Those options are controlled by a set of so-called
 flags.  By default, all flags are initialized to the appropriate
 values in most of the cases.  If the value of any of the flags has to
 be changed by the user, the flag must be changed before performing
 the corresponding action.  There are two ways to change the flag 
 values. The most usual way consists in calling
 @pred{customize_and_preprocess/1} from the CiaoPP top-level shell with the
 file name as argument. In the @apl{emacs} environment this can be done 
 most easily by clicking on the options button in the toolbar or in
 the CiaoPP menus.  It will prompt (with help) for the value of the
 different options and flags.

 The second way to change flag values consist in executing in the
 CiaoPP top-level shell a number of calls to @pred{set_menu_flag/3}
 with the right values, and then calling one of the following 
 predicates:

    @begin{itemize}

    @item @pred{auto_check_assert/1} with the file name as
    argument to @bf{check a program}. 

    @item @pred{auto_optimize/1} with the file name as argument to
    @bf{optimize (transform) a program}. 

    @item @pred{auto_analyze/1} with the file name as argument
    to @bf{analyze a program}. 
    @end{itemize}  

 In the @apl{emacs} environement these actions can be performed most
 easily by clicking on the corresponding button in the toolbar or in
 the CiaoPP menus.

 The customization menus can be made to show more or less detail
 depending on the level of expertise of the user. This can be
 configured in the customization menu itself.
" ).

%%  These appear already as separate predicates in the manual.
%% 
%%  Provisionaly, and until a new branch in the main menu is created,
%%  the predicates listed below are provided to allow the user to manage
%%  the several menu stored configurations:
%% 
%%  @begin{description}
%% 
%%   @item get_menu_configs/1. The argument is instantiated to a list
%%   with all current menu stored configurations.
%% 
%%   @item save_menu_config/1. Saves the current menu flags value refered
%%   in the future by the atom provided as argument.
%% 
%%   @item remove_menu_config/1. Removes a menu configuration referred by
%%   the atom provided as argument.
%% 
%%   @item restore_menu_config/1. Restore a menu configuration referred by
%%   the atom provided as argument.
%% 
%%   @item show_menu_configs/0. List all the atoms that refers to a menu
%%   configuration.
%% 
%%   @item show_menu_config/1. Show the flags values of the meny
%%   configuration referred by the atom provided as argument.
%% 
%% @end{description}

%% What's the point of this?
%% 
%% :- doc(appendix,"The following is a complete listing of all the
%% options that can be configured through the menu with an explanation of
%% what each of them means:
%% 
%% @includedef{menu_opt/6}").
%% 



% :- doc( bug , "1.- When executing check assertions branch, if
%             customize analysis flag was on,
%             @pred{auto_check_assert/1}, got flags values from
%             @tt{analysis} menu instead of @tt{check_assertion} menu."
%             ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% menu HOOKS-GLUE with pp_flags
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- discontiguous hook_menu_default_option/3.
:- discontiguous hook_menu_flag_help/3.
:- discontiguous hook_menu_flag_values/3.
:- discontiguous menu_opt/6.


:- pred hook_menu_flag_values( Menu, Flag, Values) 

# "Menu hook that determines the possible @var{Values} that a @var{Flag} can
  have in menu @var{Menu}. ".


:- pred hook_menu_flag_help( Menu, Flag, Values) 

# "Menu hook that determines the @var{Help} text for a @var{Flag} in
  menu @var{Menu}. ".


:- pred hook_menu_default_option( Menu, Flag, DOpt) 

# "Menu hook that determines the default option @var{DOpt} for the
  @var{Flag} in menu @var{Menu}. ".



:- pred hook_menu_check_flag_value( Menu, Flag, Value) 

# "Menu hook that checks if @var{Value} is a correct option for
  @var{Flag} in menu @var{Menu}. ".


hook_menu_flag_values( selmod , sel_mod_lib , alist( [yes,no] ) ) :- !.
hook_menu_flag_help(   selmod , sel_mod_lib , 
	"Whether select modules from library or not" ) :- !.


hook_menu_flag_values( selmod , _ , alist( [none, c, cr, crt] ) ) :- !.

hook_menu_default_option( selmod , _ , none ) :- !.

hook_menu_flag_help( selmod , _ , 
	"c - compile time\n" ||
        "r - compile and run-time checks\n" ||
        "crt - compile and run-time checks, test cases\n" ) :- !.




hook_menu_flag_values( _ , Flag ,  ask( F ) ) :-
	valid_flag_values( Flag , X ),
	functor( X , F , _ ),
	(F == int ; F == nnegint ; F == atom ; F == atm),
	!.
hook_menu_flag_values( _ , O , alist( OptsList ) ) :-
	findall( F , valid_flag_value( O , F ), OptsList ).


hook_menu_default_option( _ , O , Def ) :-
	current_pp_flag( O , Def ).


hook_menu_flag_help( _ , O , Help ) :-
	pp_flag( O , Help ).


hook_menu_check_flag_value( _ , F , V ) :-
	valid_flag_value( F , V ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



:- multifile analysis/1.

% :- multifile menu_default/3.

% :- multifile menu_opt/6.
% :- meta_predicate menu_opt( ? , ? , ? , pred(1) , pred( 0 ) , pred(2) ).

:- data menu_level/1.

menu_level( 0 ).

menu_level_tr( naive , 0 ).
menu_level_tr( expert , 1 ).

set_menu_level( L ) :-
	set_menu_level( ~menu_level_tr( L ) ),
	!.

set_menu_level( X ) :-
	int( X ),
	!,
	retract_fact( menu_level( _ ) ),
	asserta_fact( menu_level( X ) ).

set_menu_level( X ) :-
	error_message( "~w should be an non-negative integer" , [X] ).
	


current_menu_level( X ) :-
	menu_level( X ).


%% This override the default menu options.;
:- discontiguous( menu_default/3 ).

menu_default( para , ana_det , det ).
menu_default( para , ana_nf  , nfg ).


all_tr( optimize         , opt   ).
all_tr( analyze          , ana   ).
all_tr( check_assertions , check ).
all_tr( A                , A     ).


all_menu_branch( A , B ) :-
	member( inter_all=Br , A ),
	member( menu_level=L , A ),
	all_tr( Br, BrT ),
	( BrT = check_certificate -> A = B ;
	    menu_branch( A , BrT , ~menu_level_tr(L) , B )).


opt_tr( parallelize      , para  ).
opt_tr( poly_spec        , sp_poly  ).
opt_tr( A                , A     ).

opt_menu_branch( A , B ) :-
	member( menu_level=L , A ),
	member( inter_optimize=Br , A ),
	opt_tr( Br , BrT ),
	menu_branch( A , BrT , ~menu_level_tr(L) , B ).



%% ana , atm_title # flag - option : pre_action :: post_action <- guard.

use_cfg, 'Use Saved Menu Configuration' # menu_last_config - none.
save_cfg , 'Menu Configuration Name' # menu_config_name  - none : show_mcfg.

all, 'Select Menu Level'   # menu_level - naive.
all, 'Select Action Group' # inter_all  - analyze :: all_menu_branch.

check   , 'Analysis Domain Selection' # assert_ctcheck  - auto.
%check(1), 'Perform Modular Analysis'    # ct_mod_ana - curr_mod     <- cct.
%check   , 'Perform Modular Checking'    # ct_modular - curr_mod :: mod_check  <- cct.
check   , 'Modules to Check' # ct_modular - curr_mod  :: mod_check  <- cct.
check   , 'Iterate Over Modules' # ct_mod_iterate - on :: post_iter  <- cct_mod.
check(1) ,'Interleave Analysis and Checking'# interleave_an_check - on <- cct_mod_reg.
check(1), 'Related Modules Info'        # ct_ext_policy - assertions <- cct.
check(1), 'Regenerate Analysis Registry'# ct_regen_reg - off::reg_reg <- cct_mod_reg.
check   , 'Report Non-Verified Assrts'  # ass_not_stat_eval - warning <- cct2.
check(1), 'Predicate-Level CT Checks'   # pred_ctchecks      - on   <- cct.
check(1), 'Multivariant CT Checks'   # multivariant_ctchecks - off  <- cct.
check(1), 'Program-Point CT Checks'     # pp_ctchecks        - old  <- cct.
check(1), 'Verbose CT Checking'         # verbose_ctchecks   - off  <- cct.
%check(1), 'Create Error Log File'      # error_log          - off  <- cct.
check   , 'Customize Analysis Flags'    # check_config_ana   - off  <- cct_manual.
check   , 'Generate CT Checking Output' # ctchecks_output    - off.
check   , 'Generate Certificate'        # gen_certificate    - off.
check   , 'Reduced Certificate'         # reduced_cert       - off  <- gencert.
check   , 'Perform Optimizing Compilation'# optim_comp - none.

opt      , 'Type of Optimization'         # inter_optimize - spec :: opt_menu_branch.
~spsl    , 'Select Abs Specialization'    # spec_poly - off.
~spsl(1) , 'Preserve Finite Failure'      # pres_inf_fail-off  <- spec_pif.
~spsl(1) , 'Execute Unif at Spec Time'    # exec_unif - on     <- spec_pif.
~spsl(1) , 'Perform Postprocessing Phase' # spec_postproc - on <- spec_pif.
~spsl    , 'Select Analysis Domain'       # peval_ana         - pd.
~spsl(1) , 'Select Local Control'         # local_control - df_hom_emb_as.
~spsl(1) , 'Select Depth'                 # unf_depth         - 1    <- spec_lcd.
~spsl(1) , 'Select Computation Rule'      # comp_rule - bind_ins_jb  <- spec_lc.
~spsl(1) , 'Select Partial Concretization'# part_conc         - off  <- spec_lc.
~spsl(1) , 'Perform Argument Filtering'   # inter_opt_arg_filt- on   <- spec_lc.
~spsl(1) , 'Select Global Control'        # global_control - hom_emb <- spec_lc.
~spsl(1) , 'Use Global Trees'             # global_trees      - off  <- spec_lc.
spec(1)  , 'Post-minimization'            # min_crit          - none <- spec_lc.
~spsl(1) , 'Abstract Spec Definitions'    # abs_spec_defs     - off  <- spec_lc.
~spsl(1) , 'Remove Useless Clauses'       # rem_use_cls       - off  <- spec_lc.
~spsl(1) , 'Simplify Arithmetic Expressions'# sim_ari_exp     - pre  <- spec_lc.
~spsl(1) , 'Branching Factor Nonleftmost' # unf_bra_fac       - 1    <- spec_lc.
~spsl(1) , 'Select Filter Numbers'        # filter_nums       - off  <- spec_fn.
~spsl(1) , 'Nums Hom Emb in Local Control'# hom_emb_nums      - off  <- spec_hn.

sp_poly     , 'Select Fitness Function'          # poly_fitness       - balance.
sp_poly     , 'Maximum Size of Solution'         # pcpe_bounded_size  - '10K' <- polybounded.
sp_poly     , 'Select Strategy'                  # poly_strategy      - all_sols.
sp_poly     , 'Select Aggressivity'              # aggressivity       - normal.
sp_poly(1)  , 'Select Pruning'                   # poly_pruning       - heuristic <- polystrat.
sp_poly(1)  , 'Select Heuristic'                 # polyvar_pcpe       - pred <- polyheur.
sp_poly(1)  , 'Select Modes Domain'              # poly_modes         - sd <- polyvar.
sp_poly(1)  , 'Select Depth of Pruning'          # poly_depth_lim     - 3 <- polydepth.
sp_poly(1)  , 'Evaluation Time per sol in msecs' # pcpe_evaltime  - 200.
sp_poly(1)  , 'Perform Argument Filtering'       # inter_opt_arg_filt - on.
sp_poly(1)  , 'Perform Post Minimization'        # min_crit           - none.
sp_poly(1)  , 'Select Verbosity in Output Files' # output_info        - medium.

para     ,  'Select Annotation Algorithm' # para_ann          - mel.
para     ,  'Select Type of IAP'          # para_iap          - nsiap.
para     ,  'Select Local Analysis'       # para_local        - local.

~mtype   , 'Modules to analyze'            # mnu_modules_to_analyze - current :: mod_ana <- ana_b.
~lt(1)   , 'Related Modules Info'          # ext_policy - assertions <- ana_b.
~lt(1)   , 'Module Loading Policy'         # module_loading - one <- ana_b.
~lt(1)   , 'Success Policy'                # success_policy - over_all <- ana_b.
~mtypepar, 'Perform Non-Failure Analysis'  # ana_nf    - none :: p_nf     <- ana_pp2.
para     , 'Select Cost Analysis'          # para_cost        - both      <- para_c1.
~mtypepar, 'Select Aliasing-Mode Analysis' # modes            - shfr      <- ana_pp. 
~mtype   , 'Select Shape-Type Analysis'    # types            - eterms    <- ana_b.
para     , 'Select Granularity Analysis'   # para_grain       - none.
~mtypeepar,'Select Numeric Analysis'       # ana_num          - none      <- ana_pp.
~mtype   , 'Perform Widening sharing'      # clique_widen     - off       <- clipre. 
~mtype   , 'Select Type of Widening'       # clique_widen_type- cautious  <- clipre.
~mtype   , 'Select upper bound threshold'  # clique_widen_ub  - 200       <- clipre.
~mtype   , 'Select lower bound threshold'  # clique_widen_lb  - 250       <- clipre.
~mtype   , 'Select Cost Analysis'          # ana_cost         - none      <- ana_b.
~mtypepar, 'Perform Determinism Analysis'  # ana_det          - none      <- ana_pp1.
~lt(1)   , 'Perform Type Eval'             # type_eval        - off       <- ana_g1.
~lt(1)   , 'Select Variants'               # variants         - off       <- ana_g1.
~lt(1)   , 'Select WidenCall'              # widencall        - com_child <- ana_g1.
~lt(1)   , 'Select Type Precision'         # type_precision   - all       <- ana_g2.
~lt(1)   , 'Select Analysis Algorithm'     # fixpoint         - plai      <- ana_gto.
%~lt(1)  , 'Multivariant Success'          # multi_success    - off       <- ana_b.
~mtypeepar,'Multivariant Success'          # multi_success    - off       <- ana_pp.
~lt(1)   , 'Select Local Control'          # local_control    - off       <- ana_lc.
~mtype   , 'Select Global Control'         # global_control   - hom_emb   <- ana_gc.
~mtypeepar,'Entry Point for Analysis'      # entry_point      - entry     <- ana_pp.
~mtypeepar,'Print Program Point Info'      # pp_info          - off       <- ana_pp.
%ana(1)  , 'Multi-variant Analysis Results'# vers             - off       <- ana_b.
~anaepar , 'Multi-variant Analysis Results'# vers             - off       <- ana_pp.
~mtypeepar,'Collapse AI Info'              # collapse_ai_vers - on        <- ana_pp.
~mtype   , 'Select Type Output'            # type_output      - all       <- ana_g2.


guard cct2( X ) :- 
	( member( assert_ctcheck=Y , X ) ->
	    Y \== off 
	;
	    member( inter_all=A , X ),
	    A  == check_assertions,
	    member( menu_level=A1 , X ),
	    A1 == naive
	).

guard cct( X ) :- 
	member( assert_ctcheck=Y , X ), 
	Y \== off.


guard cct_manual( X ) :- 
	member( assert_ctcheck=Y , X ), 
	Y == manual.



% guard cct1(X) :- 
% 	cct(X),
% 	member(menu_level=A1,X),
% 	( A1 == naive -> 
% 	  member(ct_modular=E,X),
% 	  ( E == all -> 
% 	    set_menu_flag(check,ct_ext_policy,registry),
% 	    set_menu_flag(check,mnu_modules_to_analyze,all),
% 	    set_menu_flag(check,ext_policy,registry)
% 	  ; set_menu_flag(check,ct_ext_policy,assertions),
% 	    set_menu_flag(check,mnu_modules_to_analyze,current),
% 	    set_menu_flag(check,ext_policy,assertions)
% 	  )
% 	; true
% 	).

mod_check(X,X) :- 
	member(menu_level=A1,X),
	( A1 == naive -> 
	  member(ct_modular=E,X),
	  ( E == all -> 
	    % set_menu_flag(check,ct_ext_policy,registry),
	    set_menu_flag(check,mnu_modules_to_analyze,all),
	    % set_menu_flag(check,ext_policy,registry),
	    set_menu_flag(check,ct_regen_reg,on)
	  ; % set_menu_flag(check,ct_ext_policy,assertions),
	    set_menu_flag(check,mnu_modules_to_analyze,current)
	    % set_menu_flag(check,ext_policy,assertions)
	  )
	; true
	).


mod_ana(X,X):-
	member(menu_level=A1,X),
	( A1 == naive -> 
	  member(mnu_modules_to_analyze=E,X),
	  member(inter_all=MenuType,X),
	  ( E == all -> 
	    Ext_policy = registry
	  ; Ext_policy = assertions
	  ),
	  ( MenuType == analyze ->
	    set_menu_flag(ana,ext_policy,Ext_policy)
	  ; set_menu_flag(check,ext_policy,Ext_policy)
	  )
	; true
	).


post_iter(X,X) :-
	member(ct_mod_iterate=A,X),
	( A == on ->
	  set_menu_flag(check,ct_ext_policy,registry),
	  set_menu_flag(check,ext_policy,registry),
	  set_menu_flag(check,mnu_modules_to_analyze,all),
	  member(menu_level=A1,X),
	  ( A1 == naive ->  % PP: should be ok in most cases
	    set_menu_flag(check,types,terms)
	  ; true
	  )
	; set_menu_flag(check,ct_ext_policy,assertions),
	  set_menu_flag(check,ext_policy,assertions),
	  set_menu_flag(check,mnu_modules_to_analyze,current)
	).


reg_reg(X,X):-
	member(ct_regen_reg=A,X),
	( A==on ->
	  set_menu_flag(check,mnu_modules_to_analyze,all),
	  set_menu_flag(check,ext_policy,registry)
	; true
	).

guard cct_mod_reg(X) :- 
	cct_mod(X),
	member(ct_ext_policy=Y, X), 
	Y == registry,
	member(ct_mod_iterate=Y1, X), 
	Y1 == on.


guard cct_mod(X) :- 
	cct(X),
	member(ct_modular=Y, X), 
	Y == all.


% True if assert_ctcheck is on (has been selected) OR % it has not
% % been selected (native menu!) and flag value is on.
% guard cct2( X ) :- 
% 	member( assert_ctcheck=Y , X ),
% 	!,
% 	Y == on.
% guard cct2( _ ) :- 
% 	get_menu_flag( check ,  assert_ctcheck , on ).


% guard cct_mod(X) :- 
% 	member(ct_mod_ana=Y, X), 
% 	( Y == monolithic ->
% 	  set_pp_flag(ct_modular,all),
% 	  fail
% 	; true
% 	).
	    
guard ana_b( X )  :- 
	member( inter_all=I , X ),
	(
	    I == check_assertions,
	    member( check_config_ana=Y , X ),
	    Y == on
	;
	    I == analyze
	).

guard nf_not_selected( X ) :-
	(
	    member( ana_nf=NF , X )
	-> 
	    NF == none 
	;
	    true
	).

% Almost everything have this preconditions
ana_b2( X )  :- 
	ana_b( X ),
	nf_not_selected( X ).


guard ana_g1( X )  :- 
	ana_b2( X ),
	member( types=Y , X ), 
	(
	    Y == eterms
	; 
	    Y == svterms
	).

% like ana_g1 but for all types
guard ana_g2( X )  :- 
	ana_b2( X ),
	member( types=Y , X ),
	Y \== none.

guard ana_gto( X ) :- 
	ana_b2( X ),
	member( local_control=Y , X ),
	Y == off.

guard ana_gc( X )  :- 
	ana_g1( X ),
	member( local_control=Y , X ),
	Y \== off.

guard ana_lc( X )  :- 
	ana_b( X ),
	member( types=T, X ),
	T  \== none,
	!.

guard ana_lc( X )  :- 
	ana_b( X ),
	member( modes=M , X ),
	M \== none.

guard para_menu( X ) :-
	member( inter_all=I       , X ),
	member( inter_optimize=I2 , X ),
	I  == optimize,
	I2 == parallelize.

guard ana_pp( X ) :- ana_b( X ). 
guard ana_pp( X ) :- para_menu( X ).
	

guard ana_pp1( X ) :- 
	ana_b( X ).
guard ana_pp1( X ) :- 
	para_menu( X ),
	member(para_ann=Y, X),
	(Y == uoudg).  %  ; Y == uudg ; Y == disjwait).

guard ana_pp2( X ) :- 
	ana_b( X ).
guard ana_pp2( X ) :- 
	para_menu( X ),
	member( para_grain = Y , X ), 
	Y == gr.




guard spec_lc(X)   :- 
	member( local_control=Y , X ), 
	Y \== off.

guard spec_lcd(X)  :- 
	member( local_control=Y , X ),
	( Y == depth         ; 
	  Y == first_sol_d   ; 
	  Y == det_la
	).

guard spec_fn(X)   :- 
	member( global_control=Y , X ), 
	Y == hom_emb.

guard spec_hn(X)   :- 
	member( local_control=Y , X ), 
	Y == df_hom_emb_as.

guard spec_pif(X)  :- 
	member( spec_poly=Y , X ), 
	Y \== off.

guard gencert( X ) :- 
	member( gen_certificate=Y , X ), 
	Y == on.

/*
guard shpre( X ) :-
	vmember( modes=Y, X ),
	(  Y == share_amgu       ; 
	   Y == share_clique     ; 
           Y == sharefree_amgu   ; 
	   Y == shfrlin_amgu     ;  
	   Y == sharefree_clique ).
*/

guard clipre( X )  :- 
	member( modes=Y, X ),
	( Y == share_clique  ;
	  Y == sharefree_clique ).

/*
guard para_d1(X)  :-
	member(para_ann=Y, X),
	(Y == uoudg).  %  ; Y == uudg ; Y == disjwait).

%guard para_t1(X)  :- 
%	member(para_grain=Y, X), 
%	Y == gr.

guard para_n1(X)  :- 
	member(para_grain=Y, X), 
	Y == gr.
*/

guard para_c1(X)  :- 
	member(para_grain=Y, X), 
	Y == gr.

guard polystrat(X)  :- 
	member(poly_strategy=Y, X), 
	Y == all_sols.

guard polybounded(X)  :- 
	member(poly_fitness=F, X), 
	F == bounded_size.


guard polyheur(X)  :- 
	member(poly_pruning=Y, X), 
	(Y == heuristic;
	 Y == both).

guard polyvar(X)  :- 
	member(polyvar_pcpe=Y, X), 
	Y == modes.

guard polydepth(X)  :- 
	member(poly_pruning=Y, X), 
	(Y == bnb;
	 Y == both).   



:- push_prolog_flag( multi_arity_warnings , off ).

% SPecSLice
spsl( spec  ).
spsl( slice ).

spsl( X , spec(  X ) ).
spsl( X , slice( X ) ).

:- pop_prolog_flag( multi_arity_warnings ).

mtype( ana   ).
mtype( check ).

mtypepar( X ) :- mtype( X ).
mtypepar( para ).


mtypeepar( X ) :- mtype( X ).
mtypeepar( para(1) ).

anaepar( ana ).
anaepar( para(1) ).

lt( X , ana(X) ).
lt( X , check(X) ).



%%
%% POST
%%

remove_from_list( A , B , C ) :-
	remove_from_list_( A , B , C ),
	(
	    C = [V|_],
	    var( V )
	-> 
	    !,
	    fail
	;
	    true
	).


remove_from_list_( [] , _ , [] ).

remove_from_list_( [A|B] , A , C ) :-
	remove_from_list( B , A , C ),
	!.

remove_from_list_( [E|B] , A , [E|C] ) :-
	!,
	remove_from_list( B , A , C ).




p_nf( L , LS ) :-
	uni_type( L , Z ),
	vmember( ana_nf=Y , L ),
	(
	    eq( Z , Y , none ),
%	->
	    L = LS
	;
	    remove_from_list( L  , modes=_ , L1 ),
	    remove_from_list( L1 , types=_ , L2 ),

	    append( L2 , [ modes          = shfr   ,
	                   types          = eterms ,
			   type_precision = all    ,
			   type_output    = all    ,
			   widencall      = onlyfixp ] , LS )
        ).

/*
para_post_g1( L , LS ) :-
	uni_type( L , Z ),	
	vmember( para_ann=Y , L ),
	(
	    neq( Z , Y , urlp ),
	    L = LS
	;
	    remove_from_list( L  , para_iap=_ , L1 ),
	    append( L1 , [ para_iap = post ] , LS )
        ).
*/

:- push_prolog_flag(unused_pred_warnings, no).
show_mcfg :-
	get_menu_configs( C ),
	% Note: make sure that this message goes to standard output
	%   (required by ciao-widgets.el)
	note_message( "Current Saved Menu Configurations: ~w" , [ C ] ).
%	message( note , ['Current Saved Menu Configurations: ' , C ] ).
:- pop_prolog_flag(unused_pred_warnings).


% :- set_prolog_flag( multi_arity_warnings , off ).

% true( _ ).
% true( A , A ).

% :- set_prolog_flag( multi_arity_warnings , on  ).

%%%%%%%%%%%% CUSTOMIZE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- data customize__last_file/1.




:- pred set_last_file( File ) : atom( File )

# "Set last file to @var{File}. This option is used by
  customize_and_preprocess, to allow @pred{again/0} to work.".


set_last_file( File ) :-
	retractall_fact( customize__last_file( _  ) ),
	asserta_fact(    customize__last_file( File  ) ).




:- pred get_last_file( File ) : var( File ) => atom( File )

# "@var{File} is the current value of @pred{last_file/1} used by
  @pred{customize_and_preprocess/1} or @pred{customize_and_preprocess/0}.".

get_last_file( File ) :-
	current_fact( customize__last_file( File ) ).

:- pred customize_and_preprocess/0 

# "Select options using @tt{customize/0}, and then call
  @pred{auto_analyze/1}, @pred{auto_optimize/1}, or
  @pred{auto_check_assert/1} (as determined by the selected options)
  on the default file. If no default file is defined, prompt for the
  name of to be processed, which becomes from now on the default
  file.".

customize_and_preprocess :-
	display('(Main) file to be processed:     ('),
	(  get_last_file( File ) 
	-> display(File)
	;  display(none), File = ''),
	display(') ? '),
	prompt_for_default(NewFile,File),
	(  file_exists(NewFile,4) 
	-> customize_and_preprocess( NewFile )
	;  error_message( "~w does not exist or cannot be read", [NewFile] )
	).


:- pred customize_and_preprocess( File )

# "Select options using @tt{customize/0}, and then call
  @pred{auto_analyze/1}, @pred{auto_optimize/1}, or
  @pred{auto_check_assert/1} (as determined by the selected options)
  with @var{File} as argument. @var{File} is from now on the default
  file.".

customize_and_preprocess( File ) :-
	atom( File ),
	customize,
	set_last_file( File ),
	again.

:- pred customize_but_dont_save( Option )

# "Same as customize( @var{Option} ), but menu flags will not be
   modified.".

customize_but_dont_save( Option ) :-
	get_menu_flags( L ),
	customize( Option ),
	restore_menu_flags_list( L ).

:- pred again/0

# "Performs the last actions done by @pred{customize_and_preprocess/1},
   on the last file previously analyzed, checked, or optimized".

again :-
	get_last_file( File ),
	get_menu_flag( all , inter_all , NM ),
	exec_auto( NM , File ).
	

exec_auto( optimize , F ) :-
	!,
	auto_optimize( F ).

exec_auto( analyze , F ) :-
	!,
	auto_interface:auto_analyze( F ).

exec_auto( check_assertions , F ) :-
	!,
	auto_check_assert( F ).

exec_auto( check_certificate , F ) :-
	!,
	auto_check_certificate( F ).

exec_auto( none , _ ) :-
	!.

exec_auto( U , _F ) :- 
	error_message( "Unknown option ~w while executing customize_and_preprocess",
	               [U] ).

:- pred customize/0 

# "Enter an interactive menu to select the preprocessing action
   (analysis / assertion checking / transformation / optimization /
   ...)  to be performed by deafult and the different options (i.e.,
   setting the preprocessor flags).".

customize :-
	customize( all ).

:- pred customize( X )

# "Customize is used for changing the values of a set of flags. These
  flags are grouped into @em{analyze}, @em{check assertions} and
  @em{optimize}. X should take the values: @tt{analyze},
  @tt{check_assertions} or @tt{optimize}.".


customize( all ) :-
	!,
	ask_use_config( USE_CONFIG , Bool ),
	(
	    USE_CONFIG == none
	-> 
	    menu( all , Bool ),
%	    get_menu_flag( all , inter_all  , NM ),
%	    get_menu_flag( all , menu_level , ML ),
%	    set_menu_level( ML ),
%	    customize( NM ),
	    ask_save_menu
	;
	    note_message( "Restoring ~w Menu Configuration..." , [ USE_CONFIG ] ),
	    restore_menu_config( USE_CONFIG )
	).


% customize( optimize ) :-
% 	!,
% 	menu( opt , false ),
% 	get_menu_flag( opt , inter_optimize , NM ),
% 	customize( NM ).

% customize( analyze ) :-
% 	!,
% 	menu_level( L ),
% 	menu( ana, L, false ).

% customize( slice ) :-
% 	!,
% 	menu_level( L ),
% 	menu( slice, L, false ).

% % customize( parallelize ) :-
% % 	!,
% % 	menu_level( L ),
% % 	menu( para, L, false ).

% customize( poly_spec ) :-
% 	!.

% customize( check_assertions ) :-
% 	!,
% 	menu_level( L ),
% 	menu( check,  L, false ).

customize( check_certificate ) :-
	!.

customize( none ) :-
	!.

customize( X ) :-
	atom( X ),
	all_tr( X , XT ),
	( X == XT 
	-> opt_tr( X , XT2 ),
	   X \= XT2
	; XT2 = XT
	),
	menu_level( L ),
	menu_level_tr( LT , L ),
	menu( XT2, L, false , [menu_level=LT,inter_all=X]),
	!.

customize( X ) :-
	menu( X , false ),
	!.

customize( A ) :-
	error_message( "Option ~w not customizable" , [A] ).


%%%%%%%%%%% ADDITIONAL INTERNAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%

:- pred ask_use_config( Config , B ) :: ( atm( Config ) , atm( B ) )

# "In @var{Config} the selected configuration to use is
  returned. @var{B} tells if printing help message if you are going to
  use more menus.".

ask_use_config( USE_CONFIG , false ) :-
	findall( F , valid_flag_value( menu_last_config , F ), OptsList ),
	OptsList = [_,_|_],
	set_menu_flag( use_cfg , menu_last_config , none ),
	menu( use_cfg ),
	get_menu_flag( use_cfg , menu_last_config , USE_CONFIG ),
	!.
ask_use_config( none , true ).


ask_save_menu :- 
	set_menu_flag( save_cfg , menu_config_name , none ),
	menu( save_cfg , false ),
	get_menu_flag( save_cfg , menu_config_name , CONFIG ),
	(
	    CONFIG \== none
	->
	    save_menu_config( CONFIG )
	;
	    true
	).


%%%%%%%%%%% INTERNAL PREDS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

save_flags( Menu ) :-
	get_flag_list( Menu, L ),
	save_flags_list( L , Menu ).

get_flag_list( Menu, L ) :-
	findall( A , (menu_opt${ menu => MM , flag => A }, functor( MM , Menu , _ )) , L ).

save_flags_list( [A|As] , Menu ) :-
	!,
	get_menu_flag( Menu , A , V ),
	push_pp_flag( A , V ),
	save_flags_list( As , Menu ).
save_flags_list( [] , _Menu ).



restore_flags( Menu ) :-
	get_flag_list( Menu, L ),
	restore_auto_flags_list( L ).


restore_auto_flags_list( [A|As] ) :-
	!,
	pop_pp_flag( A ),
	restore_auto_flags_list( As ).
restore_auto_flags_list( [] ).

%%%%%%%%%%%%  AUTO_something %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- push_prolog_flag( multi_arity_warnings , off ).

:- pred auto_analyze( F ) 

# "Analyze the module @var{F} with the current analysis options (use
  @tt{customize(analyze)} to change these options).".

auto_analyze( File ) :-
	auto_interface:auto_analyze( File , _ ).


:- pred auto_analyze( F , OFile ) 

# "Same as @pred{auto_analyze/1} but the output file will be 
  @var{OFile}.".

auto_analyze( File , _OFile ) :-
	\+ (
	    get_menu_flag(ana, mnu_modules_to_analyze, current),
	    get_menu_flag(ana, ext_policy, assertions)
	), !,
	save_flags(ana ),
	get_menu_flag(ana ,inter_ana ,LIST) ,
	exec_mod_analysis_list(LIST, File, ana),
 %%After the analysis, the current module must be analyzed again to do output/0-1.
	module( File ),
	push_pp_flag(intermod,on),
        exec_analysis_list( LIST , ana ),
	pop_pp_flag(intermod),
	(var(OFile) -> 	output ; output( OFile )),
 %%
	set_last_file(File),
	restore_flags(ana),
	!.
auto_analyze( File , OFile ) :-
	module( File ),
	save_flags( ana ),
	get_menu_flag( ana , inter_ana , LIST ) ,
	exec_analysis_list( LIST , ana ),
	get_menu_flag( ana , vers , VERS ) ,
	(VERS == on -> transform( vers ) ; true ),
	(var(OFile) -> 	output ; output( OFile )),
	set_last_file( File ),
	restore_flags( ana ),
	!.
auto_analyze( File , _ ) :-
	error_message( 
        "INTERNAL ERROR: Unexpected error when executing auto_analyze(~w)",
	[File] ).


:- pred auto_check_assert( F )

# "Check the assertions in file @var{F}, with the current options,
  giving errors if assertions are violated (use
  @tt{customize(check_assertions)} to change these options).".

auto_check_assert( File ) :-
	auto_check_assert( File , _ ).


:- pred auto_check_assert( F , OFile )

# "Same as @pred{auto_check_assrt/1} but the output file will be 
  @var{OFile}.".

auto_check_assert( File , OFile ) :-
	save_flags( check ),

	get_menu_flag( check, assert_ctcheck  , CTCHECKS ),
	( CTCHECKS == auto ->
	  auto_sel_dom(File)
	; true ),

	get_menu_flag( check , gen_certificate , GENCERT ),
	(  GENCERT==manual 
	-> 
%% *** This needs to be revised... MH
	   set_pp_flag(dump_pred,nodep),
  	   set_pp_flag(dump_pp,off),
	   set_pp_flag(fixpoint,di)
	;  true ),

	get_menu_flag( check , assert_ctcheck  , CTCHECKS ),
	(   ( CTCHECKS == manual; CTCHECKS == auto)
	->
	    get_menu_flag( ana , inter_ana , LIST ),
	    exec_analysis_list_acheck(File,LIST,ANYERROR),
	    ctcheck_open_module_if_not_ctchecked(File,LIST,ANYERROR)
% %% 	    get_menu_flag( check , verbose_ctchecks , VCT ),
% %% 	    push_pp_flag(verbose_ctchecks, VCT),
%	    acheck
% %	   , pop_pp_flag(verbose_ctchecks)
	;
	  module(File)
	),
	continue_auto_assert_ctchecks(ANYERROR, File, OFile, GENCERT),
	set_last_file( File ),
	restore_flags( check ),
	!.
auto_check_assert( File , _ ) :-
	error_message( 
       "INTERNAL ERROR: Unexpected error when executing auto_check_assert(~w)",
	[File] ).

:- pop_prolog_flag( multi_arity_warnings ).

%% ctcheck_open_module_if_not_ctchecked(+File,+LIST,-ANYERROR)
%% Checks if the module open in emacs has not been processed when
%% checking the whole program (incremental modular ctchecking).
%% In that case, checks it in order to show the resulting buffer.
ctcheck_open_module_if_not_ctchecked(File,LIST,ANYERROR):-
	(
	    current_pp_flag(mnu_modules_to_analyze,all)
	;
	    current_pp_flag(ct_modular,all)
	),
	basename(File,Base),
	get_modules_analyzed(ModList),
	\+ member(Base,ModList),
	!,
	push_pp_flag(mnu_modules_to_analyze,current),
	push_pp_flag(ct_modular,curr_mod),
	exec_analysis_list_acheck(File,LIST,ANYERROR),
	pop_pp_flag(mnu_modules_to_analyze),
	pop_pp_flag(ct_modular).
ctcheck_open_module_if_not_ctchecked(_File,_LIST,_ANYERROR).


	
continue_auto_assert_ctchecks(Err,_,OFile,_):-
	Err == error,!,
	error_message("Errors detected. Further preprocessing aborted."),
	decide_output(OFile).
continue_auto_assert_ctchecks(_ANYERROR,File,OFile,GENCERT):-
	(  GENCERT==on 
	-> 
	   atom_concat(File,'.cert',Cert_Name),
	   inform_user(['{Generating certificate ',Cert_Name]),
	   statistics(runtime,_),
	   (current_pp_flag(reduced_cert,on) -> remove_irrelevant_entries;true),
  	   dump(Cert_Name),
	   statistics(runtime,[_,T]),
	   inform_user(['{certificate saved in ',T, ' msec.}\n}'])
	;  true ),
	get_menu_flag( check , optim_comp  , OPTIMCOMP ),
	(OPTIMCOMP == none ->
         decide_output(OFile) 
	;
	    optim_comp(OPTIMCOMP)
	).


decide_output(OFile) :-
	 current_pp_flag(ctchecks_output, on),!,
	 ( var(OFile) -> output ; output(OFile) ).
decide_output(_).


% ana = current module, ct check = current module	
exec_analysis_list_acheck(File,LIST,ANYERROR) :-
	current_pp_flag(mnu_modules_to_analyze,current),
	current_pp_flag(ct_modular,curr_mod),!,
	( current_pp_flag(assert_ctcheck,auto) ->
	  true
	; module(File)
	),
	current_pp_flag(ct_ext_policy, CT_ext_policy),
	exec_analysis_list_acheck_11(LIST, CT_ext_policy ,ANYERROR).
% ana = current module, ct check = all	
exec_analysis_list_acheck(File,LIST,ANYERROR) :-
	current_pp_flag(mnu_modules_to_analyze,current),
	current_pp_flag(ct_modular,all),!,
	current_pp_flag(ct_ext_policy, CT_ext_policy),
	exec_analysis_list_acheck_1n(File, LIST, CT_ext_policy,ANYERROR).
% ana = all, ct checking = all
exec_analysis_list_acheck(File,LIST,ANYERROR) :-
	current_pp_flag(mnu_modules_to_analyze,all),
	current_pp_flag(ct_modular,all),!,
	push_pp_flag(intermod,on),
	current_pp_flag(ct_ext_policy, CT_ext_policy),
	exec_mod_ct_x(LIST,File,auto,CT_ext_policy,ANYERROR), 
	get_concrete_analyses(LIST,Anals),
	current_prolog_flag(main_module,Main),
	(
	    current_pp_flag(interleave_an_check,off) ->
	    ( 
		Main = '' ->
		auto_ctcheck_summary(Anals,File,ANYERROR)
	    ;
		auto_ctcheck_summary(Anals,ANYERROR)
	    )
	;
	    true
	),
        pop_pp_flag(intermod).
% Intermodular analysis (all) , but CT one module
exec_analysis_list_acheck(File,LIST,ANYERROR) :-  
	current_pp_flag(mnu_modules_to_analyze,all),
	current_pp_flag(ct_modular,curr_mod),
	current_pp_flag(ct_ext_policy, CT_ext_policy),
	exec_mod_ct_x(LIST,File,auto,CT_ext_policy,ANYERROR),
	module(File),
	exec_analysis_list_acheck_11(LIST, CT_ext_policy,ANYERROR).

% one module analysis and one module checking.
exec_analysis_list_acheck_11(LIST, assertions,ANYERROR):-
	!,
	push_pp_flag(intermod,off),
	exec_analysis_list(LIST, check),
	acheck_summary(ANYERROR),
	pop_pp_flag(intermod).
exec_analysis_list_acheck_11(LIST, registry, ANYERROR):-
	push_pp_flag(intermod,on),
	push_pp_flag(entry_policy,force),
	push_pp_flag(success_policy,over_all), % !!! all/botall ?
	exec_analysis_list(LIST, check),
	acheck_summary(ANYERROR),
	pop_pp_flag(entry_policy),
	pop_pp_flag(success_policy),
	pop_pp_flag(intermod).

% one module analysis and all modules checking.
exec_analysis_list_acheck_1n(File, LIST, assertions,ANYERROR):-
	!,
	exec_mod_ct_x(LIST,File,ind,_,ANYERROR).  % inductive
exec_analysis_list_acheck_1n(File, LIST, EP,ANYERROR):-
	exec_mod_ct_x(LIST,File,curr_auto,EP,ANYERROR).


exec_mod_ct_x(Ds,File,X,EP,ANYERROR):-
	conc_ana(Ds,As),
	x_check(X,As,File,EP,ANYERROR).

conc_ana([],[]).
conc_ana([D|Ds],[A|As]):-
	% needs revision PP
%	types_or_modes(D,A),!,
	get_conc_a(D,A),
	conc_ana(Ds,As).
conc_ana([_|Ds],As):-
	conc_ana(Ds,As).


%pp%get_conc_a(D,A):-
%pp%	get_menu_flag(check,assert_ctcheck,auto),!,
%pp%	preferred_ana(D,A).
get_conc_a(D,A):-
	get_menu_flag(check,D,A),
	A \= none,!.
	


x_check(ind,D,File,_,ANYERROR) :-
	current_prolog_flag(main_module,Main),
	( 
	    Main = '' ->
	    inductive_ctcheck_summary(D,File,ANYERROR)
	;
	    inductive_ctcheck_summary(D,Main,ANYERROR)
        ).
x_check(auto,D,File,_EP,_ANYERROR) :-
	current_prolog_flag(main_module,Main),
	( 
	    Main = '' ->
	    ct_modular_analyze(D,File)
	;
	    ct_modular_analyze(D,Main)
        ).

x_check(curr_auto,D,File,_EP,ANYERROR) :-
	current_prolog_flag(main_module,Main),
	( 
	    Main = '' ->
	    auto_ctcheck_summary(D,File,ANYERROR)
	;
	    auto_ctcheck_summary(D,ANYERROR)
	).
ct_modular_analyze(D,File) :-
	current_pp_flag(ct_regen_reg,on),!,
	modular_analyze(D,File).
ct_modular_analyze(_,_).


get_concrete_analyses([],[]).
get_concrete_analyses([D|Ds],As):-
	get_menu_flag(check,D,A),
	( A == none -> 
	  As = As0
	; As = [A|As0]
	),
	get_concrete_analyses(Ds,As0).

/*
types_or_modes(types,Types) :- 	
	get_menu_flag(check, types, Types),
	analysis(Types).
types_or_modes(modes,Modes) :-
	get_menu_flag(check, modes, Modes),
	analysis(Modes).
*/

%% Auto selection of domains for CT checking starts here:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

auto_sel_dom(_) :-
	get_menu_flag(check,ct_modular,all),!,
	get_menu_flag(ana,inter_ana,As),
	decide_analyses_mod(As).

auto_sel_dom(File) :-
	module(File),
	get_menu_flag(ana,inter_ana,As),
	curr_file(_,M),
	determine_needed(As,M,Ds),
	inform_user(['{Analyses needed to check assertions: ',Ds]),
	inform_user(['}']).

needed_to_prove_prop(M, Dom, A) :-
	get_one_prop(M, P),
	needed_to_prove(A, Dom, P).

determine_needed([],_,[]).
determine_needed([A|As],M,[Dom|Ds]) :-
	preferred_ana(A,Dom),
	needed_to_prove_prop(M, Dom, A),!,
	set_menu_flag(check,A,Dom),
	determine_needed(As,M,Ds).
determine_needed([A|As],M,Ds) :-
	set_menu_flag(check,A,none),
	determine_needed(As,M,Ds).

get_one_prop(M,P) :-
	assertion_read(_,M1,check,Kind,Body,_,Base,_,_),
	take_assertion(M,M1,Kind,Base),
	assertion_body(_,_,Call,Succ,Comp,_,Body),
	( member(Prop,Succ) ; member(Prop,Call) ; member(Prop,Comp) ),
	native_prop(Prop,P).

take_assertion(M,M1,Kind,Base) :-
	M \== M1,
	is_library(Base), !, % assume that one does not check libraries with auto
	Kind  == calls.
take_assertion(M,M1,Kind,_Base) :-
	( M == M1; current_pp_flag(ct_modular,all)),
	member(Kind,[comp,success,calls]).

needed_to_prove_def(AnaKind, Ana, P) :-
	preferred_ana(AnaKind, Ana),
	P =.. [F,_|Rest],
	PP =..[F|Rest],
	knows_of(PP,Ana),!.

:- doc(bug, "needed_to_prove/3 is a weird predicate, it must be
	more easy to read. --EMM.").

% a kludge to avoid problems with knows_of/2
needed_to_prove(modes, _, ground(_)) :- !.
needed_to_prove(modes, _, free(_)) :- !.
% This handles most of the cases
needed_to_prove(AnaKind, Dom, P) :- needed_to_prove_def(AnaKind, Dom, P), !.
needed_to_prove(ana_det, _, P) :- needed_to_prove(ana_cost, _, P).
needed_to_prove(ana_det, _, P) :- needed_to_prove(ana_size, _, P).
% but: we need nf for proving lower bounds or disproving upper bounds
needed_to_prove(ana_nf, _, P) :- needed_to_prove(ana_cost, _, P).
needed_to_prove(ana_nf, _, P) :- needed_to_prove(ana_size, _, P).
% ... and types and modes for other stuff
needed_to_prove(types, _, P) :- needed_to_prove(ana_nf, _, P).
needed_to_prove(types, _, P) :- needed_to_prove(ana_cost, _, P).
needed_to_prove(types, _, P) :- needed_to_prove(ana_size, _, P).
needed_to_prove(types, _, regtype(_)).
needed_to_prove(modes, _, P) :- needed_to_prove(ana_nf, _, P).
needed_to_prove(modes, _, P) :- needed_to_prove(ana_cost, _, P).
needed_to_prove(modes, _, P) :- needed_to_prove(ana_size, _, P).
needed_to_prove(ana_cost, _, P) :- 
% because knows_of/2 works differently for size props.
	P =.. [F|As],
	PP =.. [F,_|As], 
	needed_to_prove(ana_size, _, PP).

% Preferred analyses of different kind.
% Cannot use the flags here, as default values of the flags
% are often 'none'
preferred_ana(types,    eterms).
preferred_ana(modes,    shfr).
preferred_ana(ana_num,  polyhedra). %PP: just guessing
preferred_ana(ana_nf,   nfg).
preferred_ana(ana_cost, steps_ualb).
preferred_ana(ana_cost, resources).
preferred_ana(ana_size, size_ualb).
preferred_ana(ana_det,  det).

decide_analyses_mod([]).
decide_analyses_mod([A|As]):-
	decide_one_analysis_mod(A),
	decide_analyses_mod(As).

decide_one_analysis_mod(types) :-
	set_menu_flag(check,types,terms).
decide_one_analysis_mod(modes) :-
	set_menu_flag(check,modes,shfr).
decide_one_analysis_mod(A) :-
	set_menu_flag(check,A,none).


auto_check_certificate(Program) :-
	module(Program),
	atom_concat(Program,'.cert',Cert_Name),
	restore(Cert_Name),
	domain(Domain),
	store_previous_analysis(Domain),
	checker(Checker),
	push_pp_flag(fixpoint,Checker),
	push_pp_flag(widencall,off),
	catch(analyze(Domain),
	      certif_error(X),
              error_message("Certificate and program do not match")),
	pop_pp_flag(fixpoint),
	pop_pp_flag(widencall),
	(var(X)->
	    acheck
	;
	    abort).
	
%% *** This is cheating a little bit... GP
checker(Fixpoint):-
	get_menu_flag( check, reduced_cert  , REDCERT ),

	(REDCERT = off ->
	    Fixpoint = check_di3
	;
	    Fixpoint = check_reduc_di).

:- push_prolog_flag( multi_arity_warnings , off ).

:- pred auto_optimize( F )

# "Optimize file @var{F} with the current options (use
  @tt{customize(optimize)} to change these options).".

auto_optimize( File ) :-
	auto_optimize( File , _ ).


:- pred auto_optimize( F , OFile )

# "Same as @pred{auto_optimize/1} but the output file will be
  @var{OFile}.".

auto_optimize( File , OFile ) :-
	module( File ),
	get_menu_flag( opt , inter_optimize , P ),
	push_pp_flag( dump_ai, off ),
	exec_optimize( P ),
	(P == poly_spec ->
	    true  % poly_spec performs its own outut
	;
	    (var(OFile) -> 
	        output 
	    ; 
		output(OFile)
	    )
	),
	pop_pp_flag( dump_ai ),
	set_last_file( File ),
	!.

auto_optimize( File , _ ) :-
	error_message( 
       "INTERNAL ERROR: Unexpected error when executing auto_optimize(~w)",
	[File] ).

:- pop_prolog_flag( multi_arity_warnings ).


exec_mod_analysis_list(L, File, AnaOrCheck) :-
	get_domain_list(L,AnaOrCheck,DomList,NoValid),
	mod_analyze(DomList, File),
	( 
	    NoValid = [_|_] ->
	    error_message("Modular analysis not available for the following domains: ~w", 
		               [NoValid])
	;
	    true
	).

get_domain_list([],_,[],[]).
get_domain_list([L|Ls],AnaOrCheck,Ds,Ns):-
	get_menu_flag(AnaOrCheck, L, none),  %No analysis to perform.
	!,
	get_domain_list(Ls,AnaOrCheck,Ds,Ns).
get_domain_list([L|Ls],AnaOrCheck,[D|Ds],Ns):-
	get_menu_flag(AnaOrCheck, L, D),
	valid_mod_analysis(D), % Valid modular analysis domain.
	!,
	get_domain_list(Ls,AnaOrCheck,Ds,Ns).
get_domain_list([L|Ls],AnaOrCheck,Ds,[L|Ns]):-
	get_domain_list(Ls,AnaOrCheck,Ds,[Ns]). % Non-valid modular analysis domain.

mod_analyze(Domain, File) :-
	current_pp_flag(ext_policy, ExtPolicy),
	entry_policy_value(ExtPolicy,EP),
	push_pp_flag(entry_policy,EP),
	mod_analyze_(File, Domain),
	pop_pp_flag(entry_policy).

entry_policy_value(assertions,force_assrt).
entry_policy_value(registry,top_level).

% success_policy_value(assertions,top).
% success_policy_value(registry,under_all).

mod_analyze_(_File, Domain):-
	current_pp_flag(mnu_modules_to_analyze,all),
	current_prolog_flag(main_module,Main),
	Main \== '',
	!,
	modular_analyze(Domain,Main).
mod_analyze_(File, Domain):-  
	modular_analyze(Domain, File).

exec_analysis_list( [A|B] , AnaOrCheck ) :-
	get_menu_flag( AnaOrCheck , A , none ),
	!,
	exec_analysis_list( B , AnaOrCheck ).

exec_analysis_list( [A|B] , AnaOrCheck ) :-
	get_menu_flag( AnaOrCheck , A , P ),
	!,
	(
	    analysis( P )
	->
	    (
		analyze( P )
	    ->
	        true
	    ;
		error_message( "There was an error when executing analysis ~w", 
		               [ P ] )
	    )		
	;
	    error_message( "Unknown analysis: ~w" , [ P ] )
	),
	exec_analysis_list( B , AnaOrCheck ).

exec_analysis_list( [] , _ ).



exec_optimize( none ) :-
	!.

exec_optimize( parallelize ) :-
        !,
        save_flags(para),

        get_menu_flag(para, pp_info   , PP   ),
	get_menu_flag(para, vers      , VERS ),
        get_menu_flag(para, para_ann  , Ann  ),
        get_menu_flag(para, para_grain, Gr   ),
        get_menu_flag(para, modes     , Mode ),
        get_menu_flag(para, ana_nf    , NF   ),
        get_menu_flag(para, ana_det   , Det  ),
        get_menu_flag(para, types     , Types),
	(
	    PP == on -> push_pp_flag(dump_ai, on)
	;
	    true
	),
	(
	    Mode == none -> true 
	;
	    analyze(Mode)
	),
	(
	    (Ann == uoudg, Det == det) ->
	    analyze(Types),
	    analyze(Det)
	;
	    true
	),
	(
	    (Ann == uudg, Det == det) ->
	    analyze(Types),
	    analyze(Det)
	;
	    true
	),
	(
	    (Ann == disjwait, Det == det) ->
	    analyze(Types),
	    analyze(Det)
	;
	    true
	),
	(
	    (Ann == tgudg) ->
	     analyze(Types),
	     analyze(steps_ub)
	;
	    true
	),
	(
	    ( Gr \== none,  NF \== none) -> 
	     analyze(Types),
	     analyze(NF) 
	;
	    true
	),
        transform(Ann),
	(
	    VERS == on -> transform(vers)
	;
	    true
	),
        restore_flags(para).

exec_optimize( spec ) :-
	!,
	save_flags( spec ),

	get_menu_flag( spec , local_control , LocalControl ),
	
	(
	    LocalControl \== off
	->
	    push_pp_flag( fixpoint , di )
	;
	    true
	),

	((
	    get_menu_flag( spec , peval_ana , P ) ,
	    analyze( P ),
	    get_menu_flag( spec , spec_poly , SP ),
	    get_menu_flag( spec , min_crit  , Min ),
	    get_menu_flag( spec , inter_opt_arg_filt,AF) ->
	    ( Min == none ->
	      ( SP == off  ->
	         (AF == on ->
		     transform( codegen_af )
		 ; 
		     transform( codegen )
		 )
	      ;
		transform( codegen ),
		decide_transform( SP ),
		(AF = on ->
		    transform( arg_filtering )
		; 
		    true
		)
	      )
	    ;
	      transform( codegen_min )
	    ),
	    restore_flags( spec )
	 ) ->
	    pop_fixpoint_if_needed(LocalControl)
	; 
	    restore_flags( spec ),
	    pop_fixpoint_if_needed(LocalControl),
	    fail
	).

exec_optimize( slice ) :-
	!,
	save_flags( spec ),

	get_menu_flag( spec , local_control , LocalControl ),
	
	(
	    LocalControl \== off
	->
	    set_pp_flag( fixpoint , di )
	;
	    true
	),

	(
	    get_menu_flag( spec , peval_ana , P ) ,
	    analyze( P ),
	    get_menu_flag( spec , spec_poly , SP ),
	    get_menu_flag( spec , inter_opt_arg_filt,AF) ->
	       transform( slicing ),
	       decide_transform( SP ),
	       (AF = on ->
		   transform( arg_filtering )
	       ; 
		   true
	       ),
	       restore_flags( spec )
	 ; 
	     restore_flags( spec ),
	     fail
	 ).


exec_optimize( poly_spec ) :-
        !,
        save_flags(sp_poly),	
        get_menu_flag(sp_poly, aggressivity   , C   ),
	set_spec_strategy(C),
 	push_pp_flag(fixpoint,poly_spec),
	analyze(pd),
	transform(codegen_poly),
	unset_spec_strategy,
	pop_pp_flag(fixpoint),
        restore_flags(sp_poly).

exec_optimize( O ) :-
	error_message( "Unkown optimization: ~w" , [ O ] ).


set_spec_strategy(aggressive):-
	push_pp_flag(poly_global_control,[hom_emb,dyn]),
	push_pp_flag(poly_local_control,[[local_control(det),comp_rule(leftmost),unf_bra_fac(1)],[local_control(df_hom_emb_as),comp_rule(bind_ins_jb),unf_bra_fac(0)]]).
set_spec_strategy(normal):-
	push_pp_flag(poly_global_control,[hom_emb]),
	push_pp_flag(poly_local_control,[[local_control(df_hom_emb_as),comp_rule(bind_ins_jb),unf_bra_fac(0)],[local_control(det),comp_rule(leftmost),unf_bra_fac(1)]]).
set_spec_strategy(conservative):-
	push_pp_flag(poly_global_control,[hom_emb]),
	push_pp_flag(poly_local_control,[[local_control(inst)],[local_control(det),comp_rule(leftmost),unf_bra_fac(1)]]).


unset_spec_strategy:-
	pop_pp_flag(poly_local_control),
	pop_pp_flag(poly_global_control).


pop_fixpoint_if_needed(off):-!.
pop_fixpoint_if_needed(_):-
	pop_pp_flag(fixpoint).



decide_transform( off ) :-
	true. % nothing to to

decide_transform( mono ) :-
	analyze( seff ),
	transform( simp ).

decide_transform( poly ) :-
	analyze( seff ),
	transform( spec ).

:- pred clean_aux_files(File) 
: atom 

# "Deletes any auxiliary file regarding @var{File} or its related
files (e.g., imported modules in a modular program).".

clean_aux_files(File):-
	intermod:cleanreg(File).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Generic menu for selecting per-module rtc
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

selmod , ~selmod_title( M )              # ~selmod_flag(M) - none.
selmod , 'Select Modules from Libraries' # sel_mod_lib - no.
selmod , ~selmod_title( M )              # ~selmod_flag_lib(M) - none <- selmod_lib.

selmod_title( M , Title ) :-
	atom(M),
	atom_concat( 'Module ' , M , Title ).

selmod_flag( M , M ) :-
	rel_modules( RM ),
	member( M , RM ),
	atom_codes( M , LC ),
	\+ selmod_is_lib( LC ).

selmod_flag_lib( L , L ) :-
	rel_modules( RM ),
	member( L , RM ),
	atom_codes( L , LC ),
	selmod_is_lib( LC ).

selmod_is_lib( L ) :-
	( append( _ , "/lib/"     || _ , L ) 
	; append( _ , "/library/" || _ , L ) ),
	!.

rel_modules( [a,'/home/dtm/CiaoDE/ciao/lib/b','algo/library/c',d] ).

:- pred select_modules( _ )

# "Launch a menu to select module dependencies.".

select_modules( _ ) :-
	menu( selmod ).


guard selmod_lib( X ) :-
	member( sel_mod_lib=Y , X ), 
	Y == yes.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% PLEASE READ: The following predicates generate a menu for preprocessing
% of Java programs loaded directly from Emacs. For now, the only option is
% 'analyze' since few Java analyses and options are available. The menu is
% still very naive and the predicates: customize_java/1,
% customize_and_preprocess_java/1, exec_auto_java/2, again_java/2,
% auto_java_analyze/1, auto_java_analyze/2 are almost duplicates of their
% 'ciao' counterparts but I prefer to maintain these copies to avoid
% side-effects and other problems. -JNL

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred customize_java( X )

# "Customize is used for change the values of a set of flags in case of
  java analysis. So far, the value of @var{X} is only @tt{'all'}.".

customize_java( all ) :-
	!,
	ask_use_config( USE_CONFIG , Bool ),
	(
	    USE_CONFIG == none
	-> 
	    menu( javall , Bool ),
	    ask_save_menu
	;
	    note_message( "Restoring ~w Menu Configuration..." , [ USE_CONFIG ] ),
	    restore_menu_config( USE_CONFIG )
	).

customize_java( A ) :-
	error_message( "Option ~w not customizable" , [A] ).

:- pred customize_and_preprocess_java( File )

# "It is like doing @tt{customize_java(all)}, and then calling with
  @var{File} as argument.".

customize_and_preprocess_java( File ) :-
	atom( File ),
	customize_java( all ),
	set_last_file( File ),
	again_java.


:- pred again_java/0

# "Performs the last actions done by @pred{customize_and_preprocess_java/1}, on
   the last file previously processed.".

again_java :-
	get_last_file( File ),
	get_menu_flag( javall , inter_javall , NM ),
	exec_auto_java( NM , File ).


exec_auto_java( analyze , F ) :-
	!,
	auto_interface:auto_java_analyze( F ).

:- push_prolog_flag(multi_arity_warnings, off).

:- pred auto_java_analyze( F ). 

auto_java_analyze( File ) :-
	auto_interface:auto_java_analyze( File , _ ).

:- pred auto_java_analyze( F , OFile ) 

# "Same as @pred{auto_java_analyze/1} but the output file will be generated
  with @var{OFile} name.".

auto_java_analyze( File , OFile ) :-
	module( File ),
	save_flags( javanal ),
	get_menu_flag( javanal , inter_javana , LIST ) ,
	exec_analysis_list( LIST , ana ),
% 	get_menu_flag( ana , vers , VERS ) ,
% 	(VERS == on -> transform( vers ) ; true ),
 	(var(OFile) -> 	output ; output( OFile )),
	set_last_file( File ),
	restore_flags( javanal ),
	!.
auto_java_analyze( File , _ ) :-
	error_message( 
        "INTERNAL ERROR: Unexpected error when executing auto_java_analyze(~w)",
	[File] ).
:- pop_prolog_flag(multi_arity_warnings).

javall_tr( analyze      , javanal ).
javall_tr( A            , A      ).

javall_menu_branch( A , B ) :-
	member( inter_javall   = Br , A ),
	member( menu_java_level= L  , A ),
	javall_tr( Br, BrT ),
	menu_branch( A , BrT , ~menu_level_tr(L) , B ).

javall   , 'Select Menu Level'   # menu_java_level - naive.
javall   , 'Select Action Group' # inter_javall    - analyze :: javall_menu_branch.

javanal  , 'Select Aliasing-Mode Analysis' # java_modes       - none      <- javana. 
javanal  , 'Select Shape-Type Analysis'    # java_types       - none      <- javana.
javanal  , 'Select Resource Analysis'      # java_ana_cost    - resources <- javana.
javanal  , 'Multivariant Success'          # multi_success    - off       <- javana.
javanal  , 'Print Program Point Info'      # pp_info          - off       <- javana.
javanal  , 'Collapse AI Info'              # collapse_ai_vers - on        <- javana.

guard javana( X ) :-
	member( inter_javall=I       , X ),
	I == analyze.

% 	member( prog_lang = P    , X ),
% 	P == java.
