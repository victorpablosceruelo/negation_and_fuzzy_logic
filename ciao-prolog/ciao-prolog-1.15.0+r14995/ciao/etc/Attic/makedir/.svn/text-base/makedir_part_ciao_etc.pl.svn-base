% ===========================================================================
:- module(_, _, [ciaopaths, make, fsyntax, assertions, define_flag, regexp]).
% ===========================================================================
:- doc(title,  "Ciao etc Compilation/Installation installer").
:- doc(author, "Edison Mera").
:- doc(module, "This file is part of the CiaoDE installation system.").

% TODO: This file also installs DOTprofile, DOTcsh files
% ===========================================================================

:- use_module(library(terms)).

plutility :=
	fileinfo|
	get_deps|
	pldiff|
	viewpo|
	lpmake|
	cleandirs|
	plindent|
	show_asr|
	show_deps|
	compiler_output|
	synch_actions|
	checkline.

% TODO: strange... enumerated for installation, add a table of exec+kind instead
shscript := 'ciao_get_arch'|'ciao'.

% ============================================================================

:- include(library(lpdist('makedir_SHARE'))).
bundle_id(ciao_etc).
bundle_dname('Ciao (etc) Utilities').
%
bundle_readme_dir := _ :- fail.
bundle_manual_dir := _ :- fail.
%
bundle_readme(_) :- fail.
%
bundle_ins_reg :- fail. % TODO: document (register the bundle on ins?)

% ============================================================================

bundle_basedir := bundle_src(ciao)/etc.

% (hook)
bundle_build <- [] :-
	gen_DOTcshrc,
	gen_DOTprofile,
	build_plutility,
	gen_ciao_get_arch,
	gen_ciao.

% Generate shell initialization files
gen_DOTcshrc :- gen_script(csh).

gen_DOTprofile :- gen_script(sh).

gen_script(Sh) :-
	( Sh = csh -> File = 'DOTcshrc'
	; Sh = sh ->  File = 'DOTprofile'
	),
	normal_message("Creating ~w", [File]),
	wr_template(origin, ~bundle_basedir, File, [
	    'CiaoDocDir' = ~docdir,
	    'CiaoBinDir' = ~ciaobin_dir,
	    'ConfigOptimizingCompiler' = ~optimizing_compiler_string(Sh)
        ]).

:- use_module(library(aggregates), [findall/3]).
build_plutility :-
	findall(P, plutility(P), Ps),
	build_plutilities(Ps).

build_plutilities([]).
build_plutilities([P|Ps]) :-
	normal_message("Building ~w", [P]),
	b_make_exec(ciao, ~bundle_basedir, P),
	build_plutilities(Ps).

% Generate startup script
gen_ciao :-
	normal_message("Building ~w (command line)", ['ciao']),
	wr_template(install_kver(shscript, ciao), ~bundle_basedir, 'ciao', [
	    'ExtraCommands' = ~ciao_extra_commands,
	    'CiaoVersion' = ~bundle_version(ciao),
	    'CiaoSuffix' = ~get_ciao_ext,
	    'CiaoBinDir' = ~ciaobin_dir
        ]),
	b_link(shscript, ciao, 'ciao'),
 	( install_prolog_name(yes) ->
 	    b_link_as(shscript, ciao, 'ciao', 'prolog')
 	; true
 	).

gen_ciao_get_arch :-
	normal_message("Building ~w (command line)", ['ciao_get_arch']),
	% TODO: we build nothing here (just copy) but the user does not want to know
	b_copy(shscript, ciao, ~bundle_basedir, 'ciao_get_arch'),
	b_link(shscript, ciao, 'ciao_get_arch').

% ============================================================================

bundle_register_hook.
bundle_unregister_hook.

% ============================================================================

bundle_install <- [] # "Install Ciao shell utilities." :-
	bundle_install(~instype).

bundle_install(src) :- !.
bundle_install(ins) :-
	b_install_dir(~ciaobin_dir),
	% Copy shell initialization files
	rl_install_copy_and_link(~bundle_basedir, 'DOTprofile'),
	rl_install_copy_and_link(~bundle_basedir, 'DOTcshrc'),
	% Copy startup script
	b_install_copy(shscript, ciao, 'ciao'),
	b_install_link(shscript, ciao, 'ciao'),
	( install_prolog_name(yes) ->
	    b_install_link_as(shscript, ciao, 'ciao', 'prolog')
	; true
	),
	( % (failure-driven loop)
	    ( plutility(BaseFile), Kind = plexe
	    ; shscript(BaseFile), Kind = shscript
	    ),
	    b_install_copy(Kind, ciao, BaseFile),
	    b_install_link(Kind, ciao, BaseFile),
	    %
	    fail
	;
	    true
	).

bundle_uninstall <- :-
	bundle_uninstall(~instype).

bundle_uninstall(src).
bundle_uninstall(ins) :-
	rl_uninstall_link('DOTprofile'),
	rl_uninstall_copy('DOTprofile'),
	rl_uninstall_link('DOTcshrc'),
	rl_uninstall_copy('DOTcshrc'),
	( install_prolog_name(yes) ->
	    b_uninstall_link(shscript, 'prolog')
	; true
	),
	b_uninstall_link(shscript, 'ciao'),
	b_uninstall_copy(shscript, ciao, 'ciao'),
	% TODO: NewUser is a readme file (see makedir_part_ciao;
	%       it should be removed with all the readme files)
	b_uninstall_file(~lib_dir/'NewUser'),
	(
	    ( plutility(BaseFile), K = plexe
	    ; shscript(BaseFile), K = shscript
	    ),
	    b_uninstall_link(K, BaseFile),
	    b_uninstall_copy(K, ciao, BaseFile),
	    fail
	;
	    true
	).

% TODO: Merge
optimizing_compiler_string(Shell) :=
	~optimizing_compiler_string_(Shell, ~optimizing_compiler).
optimizing_compiler_string_(_, no) := "" :- !.
optimizing_compiler_string_(csh, yes) :=
	~flatten(["eval `", ~atom_codes(~ciao_lib_dir), "/optim_comp/ciaotool csh-env`"]).
optimizing_compiler_string_(sh, yes) :=
	~flatten(["eval $(", ~atom_codes(~ciao_lib_dir), "/optim_comp/ciaotool bash-env)"]).

:- use_module(library(llists), [flatten/2]).

% ===========================================================================

cleanetc <- :-
	% TODO: generalize
	( % (failure-driven loop)
	  P = ~plutility,
	    b_clean_link(plexe, P),
	    b_clean_copy(plexe, ciao, P),
	    fail
	; true
	).

% abs_plutility := ~atom_concat([~buildbin_dir, '/', ~plutility, '-', ~bundle_version(ciao), ~get_ciao_ext]).
% 
% cleanetc <- :-
% 	( del_file_nofail(~abs_plutility), fail
% 	; true
% 	).
