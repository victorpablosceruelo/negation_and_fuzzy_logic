% -----------------------------------------------------------------------
:- reexport(ciaopp(driver)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% plugin-like modules that define analyzers: -- EMM
:- use_module(ciaopp(resources(resources_register)), []).
:- use_module(ciaopp(infercost(infercost_register)), []).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- doc(doinclude,module/1).
:- doc(doinclude,analyze/1).
:- doc(doinclude,acheck/0).
:- doc(doinclude,transform/1).
:- doc(doinclude,analysis/1).
:- doc(doinclude,transformation/1).
:- doc(hide,analyze/2).
:- doc(hide,acheck/1).
:- doc(hide,acheck_all/0).
:- doc(hide,module/2).
:- doc(hide,transform/2).
:- doc(hide,action/1).
:- doc(hide,add_action/1).
:- doc(hide,clean_analysis_info/0).

:- push_prolog_flag(unused_pred_warnings, no).
%:- multifile analysis/1.
%:- multifile transformation/1.
% temporarily repeated here, because LpDoc is not able to include them:
:- impl_defined(analysis/1).
:- prop analysis(Analysis)
	# "@var{Analysis} is a valid analysis identifier.".
:- impl_defined(transformation/1).
:- prop transformation(Transformation)
	# "@var{Transformation} is a valid transformation identifier.".
% these onea, however, should be here on their own right, 
% not in driver.pl (which goes to the internals):
:- doc(analysis/1,"Analyses can be integrated in CiaoPP in an ad-hoc
	way (see the Internals manual), in which the CiaoPP menu would
        not be aware of them. 
        The current analyses supported in the menu are:
   @include{analysis.lpdoc}").
:- doc(transformation/1,"Transformations can be integrated in CiaoPP in 
	an ad-hoc way (see the Internals manual), in which the CiaoPP menu 
        would not be aware of them. 
        The current transformations supported in the menu are:
   @include{transformation.lpdoc}").
% so that it is the last one documented (without documentation):
:- doc(doinclude,help/0).
:- pred help/0.
help.
:- pop_prolog_flag(unused_pred_warnings).

%% From 
:- reexport(ciaopp(preprocess_flags),
	[ current_pp_flag/2,
	  set_pp_flag/2,
	  push_pp_flag/2,
	  pop_pp_flag/1,
	  pp_flag/1,
	  flag_value/1,
	  valid_flag_value/2]).
:- doc(doinclude,current_pp_flag/2).
:- doc(doinclude,set_pp_flag/2).
:- doc(doinclude,pp_flag/1).
% temporarily, because LpDoc is not able to include them:
:- doc(pp_flag/1,"Valid flags:  @include{preprocess_flags.lpdoc}").
%
:- doc(doinclude,push_pp_flag/2).
:- doc(doinclude,pop_pp_flag/1).
:- doc(doinclude,flag_value/1).
:- doc(doinclude,valid_flag_value/2).
:- doc(hide,dump_flags/1).

%% From :- reexport(plai(trace_fixp)
:- doc(hide,trace_fixp/1).

% -----------------------------------------------------------------------
:- reexport(ciaopp(printer)).
:- doc(doinclude,output/1).
:- doc(doinclude,output/0).
:- doc(hide,check_global_props/2).

%% From: :- reexport( program(p_dump) ).
:- doc(hide,dump/1).
:- doc(hide,restore/1).
:- doc(hide,restore/2).
:- doc(hide,show_dump/1).

% -----------------------------------------------------------------------
:- reexport( auto_interface( auto_interface ) ).
:- doc(hide,auto_analyze/1).
:- doc(hide,auto_optimize/1).
:- doc(hide,auto_check_assert/1).
:- doc(hide,customize/1).
:- doc(hide,customize_and_preprocess/1).
:- doc(hide,set_menu_level/1).
:- doc(hide,current_menu_level/1).
:- doc(hide,again/0).
:- doc(hide,set_last_file/1).
:- doc(hide,get_last_file/1).
:- doc(hide,ana_b/1).
:- doc(hide,customize_but_dont_save/1).
:- doc(hide,set_menu_flag/3).
:- doc(hide,get_menu_flag/3).
:- doc(hide,auto_check_assert/2).
:- doc(hide,auto_optimize/2).
:- doc(hide,auto_analyze/2).

%% From: - reexport( ciaopp(menu_generator)
:- doc(hide,get_menu_configs/1).
:- doc(hide,save_menu_config/1).
:- doc(hide,remove_menu_config/1).
:- doc(hide,restore_menu_config/1).
:- doc(hide,show_menu_configs/0).
:- doc(hide,show_menu_config/1).

% -----------------------------------------------------------------------
:- reexport( auto_interface( auto_help ) ).
:- doc(hide,help/1).

% -----------------------------------------------------------------------
:- reexport(typeslib(typeslib),[show_types/0]).
:- doc(hide,show_types/0).

:- reexport( program( p_asr ) , [ show_asr/1 ] ).
:- doc(hide,show_asr/1).

% -----------------------------------------------------------------------
:- doc(hide,dump_ai/1).
:- doc(hide,modes/2).
:- doc(hide,ctcheck/1).
% :- doc(hide,rtcheck/1).
:- doc(hide,check/1).
:- doc(hide,nf/1).
:- doc(hide,upper/1).
:- doc(hide,gu/1).
:- doc(hide,gl/1).
:- doc(hide,mmod/2).
:- doc(hide,mod/2).
:- doc(hide,glob/2).
:- doc(hide,exec_menu_options/2).

% -----------------------------------------------------------------------

%for daVinci:
%:- use_module(infer(infer)).

% res_CT_1:for init_check_flags
:- use_module(library(messages)).

% res_CT_2: for init_check_dir
:- use_module(library(system)).

% -----------------------------------------------------------------------
%jcf-begin%
%jcf% Introduction documentation has been moved to ciaopp_ref_man.pl.
%jcf% Now this module is just another chapter.

:- doc(title,"The CiaoPP low-level programming interface").
:- doc(author, "The CLIP Group").

:- doc(module, "This module includes low-level primitives for
   interacting with CiaoPP.  The exported predicates of this module
   are intended for developers only.").

%jcf-end%
:- doc(appendix, "@include{analysis_more.lpdoc}").

:- doc(bug,"1 The ciaopp version number is now hardwired instead of 
                being automatically updated").
% ------------------------------------------------------------------------

:- initialization(ciaopp_init).

ciaopp_init :-
	display('Ciao Preprocessor (integrated Alpha version)' ), nl,
	display(' | This is an alpha distribution, meant only for testing. Please do let us '), nl,
	display(' | know at ciaopp-bug<at>clip.dia.fi.upm.es any problems you may have.'), nl, nl,
	perform_init_checks.

perform_init_checks :-
	init_check_flags.
perform_init_checks :-
	init_check_dirs.

:- pred init_check_flags # "Some flags define directories which have
   to be accesed. Sometimes these flags are changed by temporary test
   to local direcories and by error the values are transmited to other
   users. This check is to avoid that.".

init_check_dirs :-
	current_pp_flag(asr_dir, D1),
	current_pp_flag(tmp_dir, D2),
	init_check_dirs__([(asr_dir,D1), (tmp_dir,D2)]).

init_check_dirs__([]).
init_check_dirs__([(F,D)|Ds]) :-
	( D == source -> true
	; ( file_exists(D, 7) ->
	      true
	  ; error_message("INTERNAL ERROR: the directory ~w is not "||
			  "accesible (flag: ~w)" , [D,F])
	  )
	),
	init_check_dirs__(Ds).

:- pred init_check_flags # "The intention of this function is just to
   check wether default flag values are allowed. This check is really
   useful when changing flags options.".

init_check_flags :-
	pp_flag(F),
	current_pp_flag(F, V),
	( valid_flag_value(F, V) ->
	    true
	; error_message( 
	    "INTERNAL ERROR: The flag ~w has a value ~w which is not correct",
	    [F,V])
	),
	fail.
init_check_flags.
