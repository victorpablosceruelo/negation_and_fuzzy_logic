:- module(c_itf_internal,
	[process_files_from/7, process_file/7, cleanup_c_itf_data/0,
	 activate_translation/3, module_expansion/9,
	 defines_module/2, exports/5, def_multifile/4, decl/2,
	 uses/2, adds/2, imports_pred/7, imports_all/2, includes/2, loads/2,
	 clause_of/7, defines_pred/3, dyn_decl/4, meta_pred/4,
	 imports_nocheck/4, package/2, exports_pred/3,
	 comp_defines/1, defines/5, uses_file/2,
	 defines/3, multifile/3, imports/5, meta_args/2, discontiguous/3,
	 base_name/2, file_data/3, module_error/1, processed/2,
	 cleanup_itf_cache/0, define_ops/0,
	 % Predicates in the next line to be used for term expanders
	 set_ciaopp_expansion/1, end_goal_trans/1,
	 add_module_check/1, module_error/0, ensure_imported/4,
	 location/3, location/1, location_t/1,
	 opt_suffix/2, default_package/1, default_shell_package/1,
	 handle_exc/1, multifile/1,
	 compute_base_name/4, module_from_base/2,
	 use_mod/3, static_base/1, static_module/1, module_loaded/4,
	 use_mod_user/2,
	 unload_mod/1,
	 make_po1/1,
	 make_wam1/1,
	 make_object1/2,
	 make_po_file/1,
	 false/1, old_file_extension/2, load_compile/1, needs_reload/1,
	 abolish_module/1, interpret_file/1, interpret_module/1,
	 interpret_srcdbg/1,pred_module/2,
	 make_delayed_dynlinks/0, discard_delayed_dynlinks/0,
	 do_initialization/1,
	 restore_defines/5,
	 restore_imports/5,
	 restore_multifile/4
	 ], [assertions, nortchecks, hiord, define_flag]).

:- use_module(library(compiler(translation))).
:- use_module(library(compiler(pl2wam))).
:- use_module(library(compiler(srcdbg)), [srcdbg_expand/6]).
:- use_module(library(compiler(unused_pred_warnings))).
:- use_module(library(compiler(compile_packages))).
:- use_module(library(fastrw)).
:- use_module(library(varnames(complete_dict))).
:- use_module(engine(internals), [
	filetype/3,
	po_filename/2,
	wam_filename/2,
	itf_filename/2,
	so_filename/2,
	product_filename/3,
	find_pl_filename/4,
	opt_suff/1,

	'$open'/3, initialize_module/1, initialized/1,
	'$define_predicate'/2, '$set_property'/2, '$compiled_clause'/4,
	'$compile_term'/2, '$current_clauses'/2, '$insertz'/2, '$abolish'/1,
	'$current_instance'/5, '$erase'/1, '$predicate_property'/3,
	'$unlock_predicate'/1, dynlink/2, dynunlink/1,
	poversion/1, '$qread'/2, '$push_qlinfo'/0, '$pop_qlinfo'/0,
	% Used by mexpand
	module_concat/3, term_to_meta/2]).
:- use_module(library(system), [
	modif_time0/2, modif_time/2, time/1, fmode/2, chmod/2,
	working_directory/2, file_exists/1, file_exists/2, delete_file/1,
	mktemp/2]).
:- use_module(library(aggregates), [findall/3, findall/4]).
:- use_module(library(dynamic),    [wellformed_body/3]).
:- use_module(library(filenames),  [no_path_file_name/2]).
:- use_module(library(strings),    [get_line/1, whitespace0/2]).
:- use_module(library(ctrlcclean), [delete_on_ctrlc/2, ctrlcclean/0]).
:- use_module(library(terms),      [copy_args/3, atom_concat/2]).
:- use_module(library(hiordlib)).
:- use_module(library(lists)).
:- use_module(library(read)).
:- use_module(library(operators)).
:- use_module(library(foreign_interface(build_foreign_interface))). % JFMC
:- use_module(library(compiler(compressed_bytecode))). % OPA
:- use_module(library(assertions(assrt_lib)), [normalize_assertion/9,
	assertion_body/7, assertion_read/9, comps_to_goal/3]).
:- include(library(compiler(basic_compilation_modules))). % EMM

define_flag(single_var_warnings,    [on,  off], on).
define_flag(discontiguous_warnings, [on,  off], on).
define_flag(multi_arity_warnings,   [on,  off], on).
define_flag(verbose_compilation,    [on,  off], off).
define_flag(itf_format,             [f,   r],   f). % f=fast{read,write}, r=prolog terms.
define_flag(compress_lib,           [yes, no],  no).
define_flag(read_assertions,        [yes, no],  yes).
define_flag(use_compile_packages,   [yes, no],  yes).
% runtime checks related flags:
define_flag(runtime_checks,          [yes, no],                no).
define_flag(rtchecks_level,          [inner, exports],         inner).
define_flag(rtchecks_trust,          [yes, no],                yes).
define_flag(rtchecks_entry,          [yes, no],                yes).
define_flag(rtchecks_exit,           [yes, no],                yes).
define_flag(rtchecks_test,           [yes, no],                no).
define_flag(rtchecks_inline,         [yes, no],                no).
define_flag(rtchecks_asrloc,         [yes, no],                yes).
define_flag(rtchecks_predloc,        [yes, no],                yes).
define_flag(rtchecks_callloc,        [no, literal, predicate], predicate).
define_flag(rtchecks_namefmt,        [short, long],            long).
define_flag(rtchecks_abort_on_error, [yes, no],                no).

% Keep asertions after reading
% TODO: This is a temporal hack for assertions/assrt_lib. It needs better integration.
% (e.g. assrt_lib distinguishes main files and components because it
%  was designed for documentation purposes, does not normalize
%  assertions in the same way for them, and only write .asr for
%  compoments)
define_flag(keep_assertions,        [yes, no],  no).

opt_suffix(Old, New) :- Old == New, !,
	current_fact(opt_suff(Old)).
opt_suffix(Old, New) :-
	retract_fact(opt_suff(Old)), 
	asserta_fact(opt_suff(New)).

:- pred time_of_itf_data(Base, Time)
	# "The itf file of source @var{Base}.pl was read at time @var{Time}.".

:- data time_of_itf_data/2.

:- pred already_have_itf(Base)
	# "The itf file of source @var{Base}.pl was already read or generated
	   in this compilation.".

:- data already_have_itf/1.

%% Data coming from/going to itf file

% Needed by dependent files

:- pred defines_module(Base, Module)
	# "The source @var{Base}.pl defines module @var{Module}.".

:- pred direct_export(Base, F, A, DefType, Meta)
	# "The source @var{Base}.pl directly exports predicate @var{F}/@var{A},
	   defined as @var{DefType} (static, implicit, dynamic, data or
	   concurrent) and with meta_predicate declaration @var{Meta}
	   (which can be 0 if it has not).".

:- pred def_multifile(Base, F, A, DynType)
	# "The source @var{Base}.pl defines multifile predicate
	   @var{F}/@var{A}, defined as @var{DynType} (static, dynamic, data
	   or concurrent).".

:- pred decl(Base, Decl)
	# "The source @var{Base}.pl contains the declaration @var{Decl}
	   as an itf-exported new_declaration.".

:- data defines_module/2, direct_export/5, def_multifile/4, decl/2.

:- pred exports(Base, F, A, DefType, Meta)
	# "The source @var{Base}.pl exports predicate @var{F}/@var{A},
	   defined as @var{DefType} (static, implicit, dynamic, data or
	   concurrent) and with meta_predicate declaration @var{Meta}
	   (which can be 0 if it has not).".

exports(Base, F, A, DefType, Meta) :-
	direct_export(Base, F, A, DefType, Meta).
exports(Base, F, A, DefType, Meta) :-
	reexports_pred(Base, File, F, A),
	imports_pred(Base, File, F, A, DefType, Meta, _EndFile).

% Data to follow dependencies

:- pred uses(Base, File)
	# "The source @var{Base}.pl imports from file @var{File}.".

:- pred adds(Base, File)
	# "The source @var{Base}.pl does @decl{ensure_loaded/1} of file
	   @var{File}.".

:- pred reexports_from(Base, File)
	# "The source @var{Base}.pl reexports from file @var{File}.".

:- data uses/2, adds/2, reexports_from/2.

:- pred uses_file(Base, File)
	# "The source @var{Base}.pl uses file @var{File} explicitly through
	    @decl{use_module/1} or @decl{use_module/2} or implicity.".

uses_file(Base, File) :- uses(Base, File).

% Data for dependency check

% NOTE: ImpFile can be 'user'
:- pred imports_pred(Base, ImpFile, F, A, DefType, Meta, EndFile)
	# "The source @var{Base}.pl imports from file @var{ImpFile}
	   predicate @var{F}/@var{A}.  Predicate is defined as
	   @var{DefType} and has meta_predicate declaration @var{Meta}
	   (possibly 0).  @var{EndFile} is '.' if the predicate resides
	   in @var{ImpFile}, otherwise it is the file in which the
	   predicate resides (due to reexportations).  Stored in itf
	   file for dependency check.".

:- pred imports_all(Base, ImpFile)
	# "The source @var{Base}.pl imports all predicates of @var{ImpFile}.".

:- pred reexports(Base, File, F, A)
	# "The source @var{Base}.pl reexports predicate @var{F}/@var{A}
	  from file @var{File}.".

:- pred reexports_all(Base, File)
	# "The source @var{Base}.pl reexports all predicates of @var{File}.".

:- pred includes(Base, File)
	# "The source @var{Base}.pl includes file @var{File}.  Stored in
	   itf file for dependency check.".

:- pred loads(Base, File)
	# "The source @var{Base}.pl does load_compilation_module of file
	   @var{File}.  Stored in itf file for dependency check.".

:- data imports_pred/7, imports_all/2,
	reexports/4, reexports_all/2, includes/2, loads/2.

%% Other data coming from read_record_file/4

% Deleted after file compiled

:- pred clause_of(Base, Head, Body, VarNames, Source, Line0, Line1)
	# "We have read from @var{Base}.pl (or included files) the
	   clause @var{Head} :- @var{Body}, which has variable names
	   @var{VarNames}, and is located in source @var{Source} (that
	   changes if clauses are from an included file) between lines
	   @var{Line0} and @var{Line1}.  In the special case that
	   @var{Head} is a number, @var{Body} is the body of a
	   declaration.".

:- pred package(Base, Package).

:- pred imports_nocheck(Base, Module, F, A)
	# "The source @var{Base}.pl imports predicate @var{F}/@var{A}
	   from module @var{Module} using @decl{import/2}.".

:- pred defines_pred(Base, F, A).

:- pred impl_defines(Base, F, A).

:- pred meta_pred(Base, F, A, Meta).

:- pred dyn_decl(Base, F, A, Decl). % Does not contain multifile preds.

:- data clause_of/7, package/2, defines_pred/3, impl_defines/3, dyn_decl/4,
	meta_pred/4, imports_nocheck/4.

%% Data used by the assertion library

:- pred defines(Base, F, A, DefType, Meta)
	# "The source @var{Base}.pl defines predicate @var{F}/@var{A},
	   defined as @var{DefType} (static, implicit, dynamic, data or
	   concurrent) and with meta_predicate declaration @var{Meta}
	   (which can be 0 if it has not).  Generated by calling
	   @pred{comp_defines/1}.".

:- data defines/5.

:- pred comp_defines(Base)
	# "Can be used in the @tt{TreatP} phase of the compilation
	  process to generate facts of @pred{defines/5} for source
	  @var{Base}.pl".

comp_defines(Base) :-
	defines_pred(Base, F, A),
	  def_type(Base, F, A, DefType),
	  (meta_pred(Base, F, A, Meta) -> true ; Meta = 0),
	  assertz_fact(defines(Base,F,A,DefType,Meta)),
	  fail.
comp_defines(_).




:- pred restore_defines( Base , F , A , DefType , Meta )

# "Given the same arguments obtained from @pred{defines/5}, it
  generates (assert in c_itf DB) all necessary data to offer itf
  functionality.".

restore_defines( Base , F , A , DefType , Meta ) :-
	assertz_fact( defines_pred(Base, F, A) ),
	assertz_fact( defines(Base, F, A) ),
	restore_def_type( DefType , Base , F , A ),
	( 
	    Meta = 0 
	->
	    true
	;
	    assertz_fact( meta_pred(Base, F, A, Meta) )
	).



restore_def_type( implicit , Base, F, A ) :-
	!,
	assertz_fact( impl_defines(Base, F, A) ).
restore_def_type( static, _Base, _F, _A ) :-
	!.
restore_def_type( DefType, Base, F, A ) :-
	assertz_fact( dyn_decl(Base, F, A, DefType) ).


:- pred restore_imports( M , IM , F , A , EndMod )

# "Given the same arguments obtained from @pred{imports/5}, it
  restores c_itf imports DB.".

restore_imports( M , IM , F , A , EndMod ) :-
	assertz_fact( imports( M , IM , F , A , EndMod ) ).


:- pred restore_multifile( M , IM , F , Def )

# "Given the same arguments obtained from @pred{multifile/4}, it
  restores c_itf multifile DB.".


restore_multifile( M, F, A, Def ) :-
	assertz_fact(multifile(M, F, A, Def)).



%% Data used by engine(mexpand) and the .po compiler

:- pred defines(Mod, F, A)
	# "Module @var{Mod} defines predicate @var{F}/@var{A}.".

:- pred multifile(Mod, F, A, DynType)
	# "Module @var{Mod} defines multifile predicate @var{F}/@var{A}
	   defined as @var{DynType}.".

:- pred imports(Mod, Mod2, F, A, EndMod)
	# "Module @var{Mod} imports from module @var{Mod2} predicate
	   @var{F}/@var{A}, which resides in module @var{EndMod}.
	   @var{EndMod} can be different from @var{Mod2} due to
	   reexportation.".

:- pred meta_args(Mod, Meta)
	# "Module @var{Mod} has meta_predicate declaration @var{Meta}.".

:- data imports/5,
	meta_args/2,
	multifile/4,
	defines/3.

:- pred redefining(Module, F, A).

:- data redefining/3.

% Deleted after reading file

:- pred exports_pred(Base, F, A). % Translates to direct_export/5

:- pred multifile_pred(Base, F, A). % Translates to def_multifile/4

:- data exports_pred/3, multifile_pred/3.

:- pred new_decl(Base, Pred, In_itf)
	# "The source @var{Base}.pl has defined the declaration @var{Pred}
	   with a new_declaration directive, @var{In_itf} is 'on' if the
	   declaration is to be included in the .itf file, 'off' otherwise.".

:- data new_decl/3.

:- pred undo_decl(Base, Goal, UndoGoal)
	# "@var{Goal} which have been done while reading @var{Base}.pl is
	   undone with @var{UndoGoal}.".

:- meta_predicate undo_decl(_, goal, goal).
:- data undo_decl/3.

:- pred discontiguous(F, A, Base).
:- pred reading_pred(F, A, Base).
:- pred pred_read(F, A, Base).

:- data discontiguous/3, reading_pred/3, pred_read/3.

% Deleted while generating itf file

:- pred imports_expl(Base, ImpFile, F, A). % Translates to imports_pred

:- data imports_expl/4.

%% Hook

:- pred add_module_check(Pred)
	# "Used by code expanders (loaded with
	   @decl{load_compilation_module/1} declarations) to provide
	   additional checks to be executed before the current file is
	   compiled.  The predicate is called with the base name of the file
	   processed as the first argument, and all solutions will be found.
	   A fact @pred{module_error/0} can be asserted if a condition
	   which should stop the compilation is found.".

:- meta_predicate add_module_check(pred(1)).

add_module_check(Pred) :-
	current_fact(reading_from(Base)), !,
	asserta_fact(expansion_check(Base, Pred)).

:- pred expansion_check(Base, Pred)
	# "@var{Pred} will be executed before file @var{Base}.pl is compiled
	   to make additional checks, with @var{Base} as its first argument.".

:- data expansion_check/2, reading_from/1.

ensure_imported(Base, Module, F, A) :-
	imports_pred(Base, File, F, A, _, _, _),
	file_defines_module(File, Module), !.
ensure_imported(_, Module, F, A) :-
	error_in_lns(_,_,error,
	             ['this module should import ',~~(F/A),' from ',Module]),
	asserta_fact(module_error).

file_defines_module(user, user) :- !.
file_defines_module(File, Module) :-
	base_name(File, BFile),
	defines_module(BFile, Module).

define_ops :-
	op(1150,  fx, [% (public),  % compatibility
	               % (mode),    % compatibility
	               (dynamic),
	               (concurrent),
	               (data),
	               (multifile),
	               (meta_predicate),
	               (discontiguous)]).

:- initialization(define_ops).

% Top-level loop

:- pred process_too(Mode, Base).
:- pred processed(Base, Mode).
:- pred status(Base, Status).

:- data process_too/2, % From use_module/ensure_loaded
	processed/2,
	status/2.

% process_files_from(File, Mode{in,in$,...,po,...}, Type{module,any},
%                    TreatP, StopP, SkipP, RedoP)

:- meta_predicate
	process_files_from(+, +, +, pred(1), pred(1), pred(1), pred(1)).

process_files_from(Files, Mode, Type, TreatP, StopP, SkipP, RedoP) :-
	cleanup_c_itf_data,
	process_files_from_all(Files, Mode, Type, TreatP, StopP, SkipP, RedoP).

:- meta_predicate
	process_files_from_all(+, +, +, pred(1), pred(1), pred(1), pred(1)).

process_files_from_all([],_Mode,_Type,_TreatP,_StopP,_SkipP,_RedoP) :- !.
process_files_from_all([F|Fs], Mode, Type, TreatP, StopP, SkipP, RedoP) :- !,
	process_files_from_(F, Mode, Type, TreatP, StopP, SkipP, RedoP),
	process_files_from_all(Fs, Mode, Type, TreatP, StopP, SkipP, RedoP).
process_files_from_all(File, Mode, Type, TreatP, StopP, SkipP, RedoP) :-
	process_files_from_(File, Mode, Type, TreatP, StopP, SkipP, RedoP).

:- meta_predicate
	process_file(+, +, +, pred(1), pred(1), pred(1), pred(1)).

process_file(File, Mode, Type, TreatP, StopP, SkipP, RedoP) :-
	cleanup_c_itf_data,
	get_base_name(File, Base, Pl, Dir),
	process_file_(Base, Pl, Dir, Mode, Type, TreatP, StopP, SkipP, RedoP).

:- meta_predicate
	process_files_from_(+, +, +, pred(1), pred(1), pred(1), pred(1)).

process_files_from_(File, Mode, Type, TreatP, StopP, SkipP, RedoP) :-
	get_base_name(File, Base, Pl, Dir),
	process_file_(Base, Pl, Dir, Mode, Type, TreatP, StopP, SkipP, RedoP),
	process_remaining_files(Mode, TreatP, StopP, SkipP, RedoP).

:- meta_predicate
	process_remaining_files(+, pred(1), pred(1), pred(1), pred(1)).

process_remaining_files(Mode, TreatP, StopP, SkipP, RedoP) :-
	retract_fact(process_too(Mode, Base)), % Coming from ensure_loaded
	\+ current_fact(processed(Base, Mode)), !,
	  file_data(Base, Pl, Dir),
	  process_file_(Base, Pl, Dir, Mode, any, TreatP, StopP, SkipP, RedoP),
	  process_remaining_files(Mode, TreatP, StopP, SkipP, RedoP).
process_remaining_files(_, _, _, _, _).

:- meta_predicate
	process_file_(+, +, +, +, +, pred(1), pred(1), pred(1), pred(1)).

process_file_(Base, PlName, Dir, Mode, Type, TreatP, StopP, SkipP, RedoP) :-
	check_loop(Base, Check),
	( retract_fact(status(Base, Status)) -> true % already viewed
	; Check = static -> true
	; new_file_status(Base, PlName, Dir, Type, Status)
	),
	process_file_2(Base, Status, PlName, Dir, Mode, Type,
	               TreatP, StopP, SkipP, RedoP),
	asserta_fact(processed(Base, Mode)).

new_file_status(Base, PlName, Dir, Type, Status) :-
	itf_filename(Base, ItfName),
	modif_time0(ItfName, ItfTime),
	modif_time0(PlName, PlTime),
	( ItfTime >= PlTime,
	  read_itf(ItfName, ItfTime, Base, Dir, Type) ->
	      Status = itf_read(ItfName,ItfTime)
	; read_record_file(PlName, Base, Dir, Type),
	  Status = file_read(ItfName)
	).

:- meta_predicate
	process_file_2(+, +, +, +, +, +, pred(1), pred(1), pred(1), pred(1)).

process_file_2(Base, Status, PlName, Dir, Mode, Type, TreatP, StopP, SkipP, RedoP) :-
	( StopP(Base) ->
	    do_not_treat(Status, Base, NewStatus)
	; handle_related_files(Base, Mode),
	  ( SkipP(Base) ->
	      do_not_treat(Status, Base, NewStatus)
	  ; ( Status = file_read(ItfName) ->
	        compute_itf_treat_file(Base, PlName, Dir, ItfName, TreatP)
	    ; ( Status = file_noclauses(ItfName)
	      ; Status = itf_read(ItfName,ItfTime),
	        changed_dependences(Base, ItfTime)
	      ) ->
	        read_record_file(PlName, Base, Dir, Type), % TODO: Type was 'any'
	        compute_itf_treat_file(Base, PlName, Dir, ItfName, TreatP)
	    ; RedoP(Base) -> % Do not regenerate .itf
	        read_record_file(PlName, Base, Dir, Type), % TODO: Type was 'any'
	        check_itf_treat_file(Base, PlName, TreatP)
	    ; true
	    ),
	    NewStatus = itf_ok
	  )
	),
	asserta_fact(status(Base, NewStatus)).

do_not_treat(file_read(ItfName), Base, NewStatus) :- !,
	del_non_itf_data(Base),
	NewStatus = file_noclauses(ItfName).
do_not_treat(Status, _Base, Status).

check_loop(Base, Check) :-
	reading_from(Base),
	!,
	(
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This prevents COMPILER LOOP error if the module is statically linked -- EMM
	    static_base(Base) ->
	    Check = static
% 	    , message(note, ['Using static library ', Base,
% 	                   '.pl instead of the one in sources.'])
	;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	    Check = loop,
	    findall(M, (reading_from(B), defines_module(B, M)), Ms0),
	    reverse(Ms0, Ms),
	    message(error, ['COMPILER LOOP: ', Base,
	            '.pl uses for compilation a file which ',
		    'depends on it: ', Ms, ' -- aborting']),
	     throw(compiler_loop)
	).
check_loop(_, ok).

handle_related_files(Base, Mode):-
	uses_file(Base, File),
	  base_name(File, BFile),
	  \+ current_fact(processed(BFile, Mode)),
	    get_file_itf(BFile),
	    asserta_fact(process_too(Mode, BFile)),
	    follow_reexports(BFile,[BFile]),
	fail.
handle_related_files(Base, Mode):-
	adds(Base, File),
	  base_name(File, BFile),
	  \+ current_fact(processed(BFile, Mode)),
	    asserta_fact(process_too(Mode, BFile)),
	fail.
handle_related_files(_, _).

get_file_itf(Base) :-
	already_have_itf(Base), !.
get_file_itf(Base) :-
	file_data(Base, PlName, Dir),
	new_file_status(Base, PlName, Dir, module, Status),
	asserta_fact(status(Base, Status)).

follow_reexports(Base,Covered) :-
	reexports_from(Base, File), base_name(File, BFile),
	  check_reexportation_loop(BFile, Covered),
	  get_file_itf(BFile),
	  follow_reexports(BFile,[BFile|Covered]),
	fail.
follow_reexports(_, _).

check_reexportation_loop(BFile, Covered) :-
	  ( member(BFile, Covered) ->
	      reverse([BFile|Covered], Loop),
	      message(error,  ['Reexportation loop: ',
	                       Loop,' -- aborting']),
	      throw(compiler_loop)
	  ; true
	  ).

:- meta_predicate compute_itf_treat_file(+, +, +, +, pred(1)).

compute_itf_treat_file(Base, PlName, Dir, ItfName, TreatP) :-
	( compute_itf(Base, PlName, Dir, ItfName) ->
	    treat_file(Base, TreatP)
	; delete_file_data(Base)
	).

:- meta_predicate check_itf_treat_file(+, +, pred(1)).

check_itf_treat_file(Base, PlName, TreatP) :-
	( check_itf_data(Base, PlName) ->
	    treat_file(Base, TreatP)
	; delete_file_data(Base)
	).

compute_itf(Base, PlName, Dir, ItfName) :-
	check_itf_data(Base, PlName), % fails if incorrect imports/exports
	  fmode(PlName, Mode),
	  generate_itf(ItfName, Dir, Mode, Base).

:- meta_predicate treat_file(+, pred(1)).

treat_file(Base, TreatP) :-
	defines_module(Base, M),
	generate_module_data(Base, M),
	( TreatP(Base) -> true
	; message(warning, ['Treatment of ',Base,'(.pl) failed'])
	),
	delete_module_data(M),
	delete_file_data(Base).

cleanup_c_itf_data :-
	del_non_itf_data(_),
	clean_read_record_data(_),
	delete_module_data(_),
	retractall_fact(defines(_,_,_,_,_)),
	retractall_fact(already_have_itf(_)),
	retractall_fact(base_name(_,_)),
	retractall_fact(file_data(_,_,_)),
	retractall_fact(status(_,_)),
	retractall_fact(process_too(_,_)),
	retractall_fact(processed(_,_)),
	retractall_fact(exports_pred(_,_,_)),
	retractall_fact(multifile_pred(_,_,_)),
	retractall_fact(reading_from(_)),
	retractall_fact(module_error),
	retractall_fact(module_error(_)),
	retractall_fact(syntax_error_in(_)).

cleanup_itf_cache :- delete_itf_data(_).

delete_itf_data(Base) :-
	retractall_fact(time_of_itf_data(Base, _)),
	retractall_fact(defines_module(Base,_)),
	retractall_fact(direct_export(Base,_,_,_,_)),
	retractall_fact(def_multifile(Base,_,_,_)),
	retractall_fact(uses(Base,_)),
	retractall_fact(adds(Base,_)),
	retractall_fact(includes(Base,_)),
	retractall_fact(loads(Base,_)),
	retractall_fact(reexports_from(Base, _)),
	retractall_fact(imports_all(Base,_)),
	retractall_fact(imports_pred(Base,_,_,_,_,_,_)),
	retractall_fact(reexports_all(Base, _)),
	retractall_fact(reexports(Base, _, _, _)),
	retractall_fact(decl(Base,_)).

% These are deleted when computing imports_pred/7
delete_aux_data(Base) :-
	retractall_fact(imports_expl(Base, _, _, _)),
	retractall_fact(expansion_check(Base, _)).

delete_file_data(Base) :-
	retractall_fact(clause_of(Base,_,_,_,_,_,_)),
	retractall_fact(package(Base,_)),
	retractall_fact(imports_nocheck(Base,_,_,_)),
	retractall_fact(defines_pred(Base,_,_)),
	retractall_fact(impl_defines(Base,_,_)),
	retractall_fact(meta_pred(Base,_,_,_)),
	retractall_fact(dyn_decl(Base,_,_,_)).

del_non_itf_data(Base) :-
	delete_aux_data(Base),
	delete_file_data(Base).

:- data base_name/2.
:- data file_data/3.

get_base_name(File, Base, PlName, Dir) :-
	base_name(File, Base), !,
	file_data(Base, PlName, Dir).
get_base_name(File, Base, PlName, Dir) :-
	compute_base_name(File, Base, PlName, Dir),
	asserta_fact(base_name(File, Base)),
	( current_fact(file_data(Base, _, _)) ->
	    true
	;
	    asserta_fact(file_data(Base, PlName, Dir))
	).

compute_base_name(File, Base, PlName, Dir) :-
	prolog_flag(fileerrors, OldFE, off),
	( functor(File, _, N), N =< 1,
	  find_pl_filename(File, PlName, Base, Dir),
	  file_exists(PlName) ->
	    set_prolog_flag(fileerrors, OldFE)
	; set_prolog_flag(fileerrors, OldFE),
	  throw(error(existence_error(source_sink,File),
	              absolute_file_name/7-1))
	).

read_record_file(PlName, Base, Dir, Type) :-
	delete_itf_data(Base),
	working_directory(OldDir, Dir),
	now_doing(['Reading ',PlName]),
	asserta_fact(reading_from(Base)),
	read_record_file_(PlName, Base, Type),
	retract_fact(reading_from(Base)),
	gen_exports(Base),
	gen_def_multifile(Base),
	end_doing,
	clean_read_record_data(Base),
	assertz_fact(already_have_itf(Base)),
	working_directory(_, OldDir).

read_record_file_(PlName, Base, Type) :-
	'$open'(PlName, r, Stream),
	skip_shell_lines(Stream),
	read_sentence(Stream, Base, Sentence),
	expand_module_decl(Sentence, Base, Type, module(Module, Exports, Package),
	    Rest),
	(record_module_decl(Sentence, Module, Base, PlName) -> true ; true),
	(Sentence = sentence(_, _, _, Ln0, Ln1) -> true ; Ln0 = 1, Ln1 = 1),
	check_define_module(Base, Module, Ln0, Ln1),
	( Module=user(_), Type==module ->
	    warning_module_missing(Ln0, Ln1)
	; true
	),
	do_use_package(prelude, Base, Module, Ln0, Ln1),
	( nonvar(Package), member(pure, Package) -> 
	    true
	; do_use_package(nonpure, Base, Module, Ln0, Ln1)
	),
	assert_export_list(Exports, Base, Ln0, Ln1),
	process_packages(Package, Module, do_use_package(Base, Module, Ln0, Ln1)),
	( member(Sentence2, Rest) ; read_sentence(Stream, Base, Sentence2) ),
	( Sentence2 = end_of_file(Ln0b, Ln1b) ->
	    process_end_of_file(Base, PlName, Module, Ln0b, Ln1b)
	; process_sentence(Sentence2, Base, PlName, Module),
	  fail
	),
	!,
	%log_translations(Base, Module, term),
	%log_translations(Base, Module, sentence),
	assert_dyn_decl(Base, '$current_module', 1, dynamic, 1, 1),
	assert_dyn_decl(Base, '$meta_args', 2, dynamic, 1, 1),
	assert_dyn_decl(Base, '$imports', 5, dynamic, 1, 1),
	close(Stream).

record_module_decl(sentence((:- module(Module, Exports)), VNs, _, Ln0, Ln1), Module, Base, Pl) :-
	assertz_fact(clause_of(Base, 1, module(_, Exports), VNs, Pl, Ln0, Ln1)).
record_module_decl(sentence((:- module(Module, Exports, Packages)), VNs, _, Ln0, Ln1), Module, Base, Pl) :-
	assertz_fact(clause_of(Base, 1, module(_, Exports, Packages), VNs, Pl, Ln0, Ln1)).

:- meta_predicate process_packages(?, ?, pred(1)).
process_packages(Package0, Module, ProcPackage) :-
	find_compile_packages(Package0, Module, Packages),
	list(Packages, ProcPackage).

rtcheck_inline_package(yes, library(rtchecks(rtchecks_rt_inline))).
rtcheck_inline_package(no,  library(rtchecks(rtchecks_rt_library))).

compile_package_rt(Package0, Package) :-
	( member(rtchecks, Package0) ->
	    current_prolog_flag(rtchecks_inline, Inline),
	    rtcheck_inline_package(Inline, Package)
	)
    ;
	current_prolog_flag(runtime_checks, yes) ->
	( Package = rtchecks
	;
	    current_prolog_flag(rtchecks_inline, Inline),
	    rtcheck_inline_package(Inline, Package)
	).

find_compile_packages(Package0, Module, [Package0|Packages0]) :-
	current_prolog_flag(use_compile_packages, yes),
	!,
	(
	    nonvar(Package0),
	    \+ member(nortchecks, Package0),
	    member(assertions, Package0) ->
	    findall(Package, compile_package_rt(Package0, Package), Packages0,
	        Packages)
	;
	    Packages0 = Packages
	),
	findall(Package, compile_package_db(Module, Package), Packages).
find_compile_packages(Package, _, Package).

process_sentence(Sentence, Base, Pl, Module) :-
	Sentence = sentence(RawData, VNs, Sings, Ln0, Ln1),
	asserta_fact(location(Pl, Ln0, Ln1), Ref),
	( expand_term_to_list(RawData, Module, VNs, DataList) ->
	    process_expanded_data_list(DataList, Base, Module, VNs, Sings,
	      Pl, Ln0, Ln1)
	; true
	),
	erase(Ref).

process_end_of_file(Base, Pl, Module, Ln0, Ln1) :-
	asserta_fact(location(Pl, Ln0, Ln1), Ref),
	( expand_term_to_list(end_of_file, Module, [], Data0),
	  append(DataList, [end_of_file], Data0) ->
	    process_expanded_data_list(DataList, Base, Module, [], [],
	      Pl, Ln0, Ln1)
	; true
	),
	erase(Ref).

process_expanded_data_list(Data0, Base, M, VNs, Sings, Pl, Ln0, Ln1) :-
	( member(Data, Data0),
	  process_expanded_data(Data, Base, M, VNs, Sings, Pl, Ln0, Ln1),
	  fail
	; true
	).

:- export(expand_module_decl/5).
expand_module_decl(Sentence, Base, Type, Decl2, Rest) :-
	% TODO: This code use required by CiaoPP (not used by Ciao)
	%       in p_asr:process_main_files_ when treating
	%       packages instead of modules. I am not sure that
        %       this should be the way to implement it. (JFMC)
	Type = package, !,
	check_include_decl(package, Base, Sentence, Sentences),
	( Sentences = [Sentence2] ->
	    expand_module_decl(Sentence2, Base, any, Decl2, Rest)
	; % TODO: duplicated in the next clause
	  default_package(DefaultPackage),
	  user_module_decl(Base, DefaultPackage, Decl2),
	  Rest = Sentences
	).
expand_module_decl(Sentence, Base, _Type, Decl2, Rest) :-
	( Sentence = sentence((:- Decl), _, _, _, _),
	  normalize_module_decl(Decl, Base, Decl2) ->
	    Rest = []
	; Sentence = sentence((:- Decl), _, _, _, _),
	  Decl = use_package(Packages) ->
	    user_module_decl(Base, Packages, Decl2),
	    Rest = []
	; default_package(DefaultPackage),
	  user_module_decl(Base, DefaultPackage, Decl2),
	  Rest = [Sentence]
	).

normalize_module_decl(Decl, _, Decl) :-	Decl = module(_, _, _), !.
normalize_module_decl(module(Module, Exports), _,
	    module(Module, Exports, Package)) :- !,
	default_package(Package).
% Unknown firts declaration may include package
normalize_module_decl(PackageDecl, Base, module(Module, Exports, Package)) :-
	\+ is_any_known_decl(PackageDecl, Base),
	functor(PackageDecl, Package0, _),
	catch(get_base_name(library(Package0), _, _, _),_,fail), !,
	( arg(1, PackageDecl, Module) -> true ; true ),
	( arg(2, PackageDecl, Exports) ->
	    true
	; Exports = []
	),
	( arg(3, PackageDecl, MorePackages) ->
	    Package = [Package0|MorePackages]
	; Package = Package0
	).

% Module is user(_) if has not module declaration
user_module_decl(Base, Packages, module(user(Base), [], Packages)).

expand_term_to_list(Data0, M, VNs, Data) :-
	primitive_expand_term(Data0, M, VNs, Data1),
	expand_list_tail(Data1, Data).

expand_list_tail(Data1, Data) :-
	( var(Data1) ->
	    Data = []
	; Data1 = [X|Data3] ->
	    Data = [X|Data2],
	    expand_list_tail(Data3, Data2)
	; Data1 = [] ->
	    Data = Data1
	; Data = [Data1]
	).

% JF temporary: This information should be handled like meta_args (and cannot be a term expansion: it leads to a loop in basiccontrol)
primitive_expand_term((:- primitive_meta_predicate(MP)), M, _, Data1) :-
	functor(MP, F, A),
	functor(Pat, F, A),
	Data1 = [(:- meta_predicate(MP)),
	         ('$primitive_meta_predicate'(Pat, M))].
primitive_expand_term(Data0, M, VNs, Data1) :-
	expand_term(Data0, M, VNs, Data1).

clean_read_record_data(Base) :-
	undo_decls(Base),
	retractall_fact(discontiguous(_, _, Base)),
	retractall_fact(reading_pred(_, _, Base)),
	retractall_fact(pred_read(_, _, Base)),
	retractall_fact(new_decl(Base, _, _)),
	retractall_fact(undo_decl(Base, _, _)).

undo_decls(Base) :-
	current_fact(undo_decl(Base, _, UndoGoal)),
	  call(UndoGoal),
	fail.
undo_decls(_).

redo_decls(Base) :-
	findall(Goal, undo_decl(Base,Goal,_), Gs),
	call_list_rev(Gs).

call_list_rev([]).
call_list_rev([G|Gs]) :-
	call_list_rev(Gs),
	call(G).

:- export(read_sentence/3).
read_sentence(Stream, Base, Sentence) :-
	repeat,
	catch(do_read_sentence(Stream, Sentence),
	      error(syntax_error([L0,L1,Msg,ErrorLoc]), _),
	      handle_syntax_error(Base,L0,L1,Msg,ErrorLoc)).

do_read_sentence(Stream, Sentence) :-
	Opts = [ variable_names(VarNames),
		 singletons(Singletons),
		 lines(Ln0, Ln1) ],
	read_term(Stream, Data, Opts),
	( Data = end_of_file ->
	    Sentence = end_of_file(Ln0, Ln1)
	; Sentence = sentence(Data, VarNames, Singletons, Ln0, Ln1)
	),
	!.

% WARNING: Use only a single package, in the form pathalias(packagefile)
:- data default_package/1.
default_package(library(default)).

:- data default_shell_package/1.
default_shell_package(default_for_ciaosh).

check_define_module(Base, M, Ln0, Ln1) :-
	module_from_base(Base, SM),
	( SM = M -> % Allow vars in module declarations
	    check_other_defines(Base, M, Ln0, Ln1)
	; M = user(_) ->
	    true
	; compiler_error(Ln0, Ln1, bad_module(Base, M))
	),
	assertz_fact(defines_module(Base, M)).

check_other_defines(Base, M, Ln0, Ln1) :-
	defines_module(OtherFile, M), !,
	compiler_error(Ln0, Ln1, module_redefined(OtherFile, Base, M)).
check_other_defines(_, _, _, _).

% :- pred read_assertion/6 + not_fails.
read_assertion(Assr, M, S, LB, LE, Dict) :-
	( normalize_assertion(M, Assr, Pred, Status, T, B, S, LB, LE) ->
	  assertz_fact(assertion_read(Pred, M, Status, T, B, Dict, S, LB, LE),
	      Ref),
	  ( current_prolog_flag(unused_pred_warnings, yes) ->
	    assert_upw_assrt(Ref, M)
	  ; true
	  )
	; true % was not an assertion
	).

% note: this predicate handles code before module expansion
process_expanded_data((?- Goal), _, _, _, _, _, _, _) :- !,
	call(Goal), !. % Done at compile time
process_expanded_data((:- Decl), Base, M, VNs,_Sings, Pl, Ln0, Ln1) :- !,
	( is_any_known_decl(Decl, Base) ->
	    ( process_decl(Decl, Base, M, VNs, Ln0, Ln1) -> true
	    ; error_in_lns(Ln0, Ln1, error, ['declaration processing failed ',
		~~(Decl)])
	    )
	; error_in_lns(Ln0, Ln1, error, ['unknown declaration ',~~(Decl)])
	),
	( current_prolog_flag(read_assertions, yes), atom(M) ->
	  read_assertion(Decl, M, Pl, Ln0, Ln1, VNs)
	; true
	),
	( current_prolog_flag(unused_pred_warnings, yes) ->
	  assert_upw_decl(Decl, Base, M, loc(Pl, Ln0, Ln1))
	; true
	),
	assertz_fact(clause_of(Base, 1, Decl, VNs, Pl, Ln0, Ln1)).
process_expanded_data((H :- B), Base, M, VNs, Sings, Pl, Ln0, Ln1) :- !,
	nonvar(H),
	callable(H),
	functor(H, F, A),
	( atom(F) -> true
	; error_in_lns(Ln0, Ln1, error, ['illegal clause']), fail
	),
	( wellformed_body(B, +, B1) -> true
	; error_in_lns(Ln0, Ln1, error, ['malformed body in ',''(F/A)]), fail
	),
	defined_in_source(Base, F, A),
	clause_check(F, A, Base, Ln0, Ln1),
	singleton_check(Sings, F, A, Ln0, Ln1),
	assertz_fact(clause_of(Base, H, B1, VNs, Pl, Ln0, Ln1), Ref),
	( current_prolog_flag(unused_pred_warnings, yes) ->
	  assert_upw_pred(Ref, M)
	; true
	),
	( dyn_decl(Base, F, A, dynamic) ->
	    dynamic_handling(Base, F, A, H, B1, VNs, Pl, Ln0, Ln1)
	; true
	).
process_expanded_data(C, _, _, _, _, _, Ln0, Ln1) :- 
	construct(C), !,
	functor(C, F, A),
	error_in_lns(Ln0, Ln1, error, ['attempt to redefine ',''(F/A)]).
process_expanded_data(F, Base, M, VNs, Sings, Pl, Ln0, Ln1) :-
	process_expanded_data((F:-true), Base, M, VNs, Sings, Pl, Ln0, Ln1).

dynamic_handling(Base, F, A, H, B, VNs, Pl, Ln0, Ln1) :-
	multifile_pred(Base, F, A), !,
	add_multifile_pred(Base, '\3\mfclause', 2),
	assert_dyn_decl(Base, '\3\mfclause', 2, data, Ln0, Ln1),
	assertz_fact(clause_of(Base, '\3\mfclause'(H,B), true,
	                       VNs, Pl, Ln0, Ln1)).
dynamic_handling(Base,_F,_A, H, B, VNs, Pl, Ln0, Ln1) :-
	new_decl(Base, dynamic_handling, _), !,
	% '\3\clause'/2 defined data in the package
	assertz_fact(clause_of(Base, '\3\clause'(H,B), true,
	                       VNs, Pl, Ln0, Ln1)).
dynamic_handling(_Base,_F,_A,_H,_B,_VNs,_Pl,_Ln0,_Ln1).

% WHERE ARE THE MODULES??? -jf
% note: this predicate handles code before module expansion
construct(true).
construct((_ , _)).
construct((_ ; _)).
construct((_ -> _)).
construct((\+ _)).
construct(if(_, _, _)).
construct((_ ^ _)).

is_any_known_decl(D, _) :- is_known_decl(D), !.
is_any_known_decl(D, Base) :- new_decl(Base, D, _), !.

is_known_decl(module(_,_)).
is_known_decl(module(_,_,_)).
is_known_decl(package(_)).
is_known_decl(use_package(_)).
is_known_decl(syntax(_)).
is_known_decl(include(_)).
is_known_decl(export(_)).
is_known_decl(use_module(_,_)).
is_known_decl(use_module(_)).
is_known_decl(import(_,_)).
is_known_decl(ensure_loaded(_)).
is_known_decl(reexport(_)).
is_known_decl(reexport(_,_)).
is_known_decl(meta_predicate(_)).
is_known_decl(multifile(_)).
is_known_decl(data(_)).
is_known_decl(dynamic(_)).
is_known_decl(concurrent(_)).
is_known_decl(impl_defined(_)).
is_known_decl(discontiguous(_)).
is_known_decl(redefining(_)).
is_known_decl(load_compilation_module(_)).
is_known_decl(op(_, _, _)).
is_known_decl(set_prolog_flag(_, _)).
is_known_decl(push_prolog_flag(_, _)).
is_known_decl(pop_prolog_flag(_)).
is_known_decl(new_declaration(_)).
is_known_decl(new_declaration(_, _)).
is_known_decl(add_sentence_trans(_,_)).
is_known_decl(add_term_trans(_,_)).
is_known_decl(add_clause_trans(_,_)).
is_known_decl(add_goal_trans(_,_)).
is_known_decl(initialization(_)).
is_known_decl(on_abort(_)).
% Deprecated declarations (show warning)
is_known_decl(add_sentence_trans(_)).
is_known_decl(add_term_trans(_)).
is_known_decl(add_clause_trans(_)).
is_known_decl(add_goal_trans(_)).

process_decl(module(_,_),_Base,_M,_VNs, Ln0, Ln1) :- !,
	compiler_error(Ln0, Ln1, nonstarting(module,2)).
process_decl(module(_,_,_),_Base,_M,_VNs, Ln0, Ln1) :- !,
	compiler_error(Ln0, Ln1, nonstarting(module,3)).
process_decl(package(_),_Base,_M,_VNs, Ln0, Ln1) :- !,
	compiler_error(Ln0, Ln1, nonstarting_package).
process_decl(use_package(Package), Base, M,_VNs, Ln0, Ln1) :- !,
	do_use_package(Package, Base, M, Ln0, Ln1).
process_decl(syntax(Syntax), Base, M,_VNs, Ln0, Ln1) :- !,
	message(warning,
	        'syntax/1 declaration is obsolete, use use_package/1.'),
	do_use_package(Syntax, Base, M, Ln0, Ln1).
process_decl(include(File), Base, M,_VNs, Ln0, Ln1) :- !,
	do_include(source, File, Base, M, Ln0, Ln1).
process_decl(export(Exports), Base,_M,_VNs, Ln0, Ln1) :- !,
	assert_export_list(Exports, Base, Ln0, Ln1).
process_decl(use_module(File,Imports), Base,_M,_VNs, Ln0, Ln1) :- !,
	do_use_module(File, Imports, Base, Ln0, Ln1).
process_decl(use_module(File), Base,_M,_VNs, Ln0, Ln1) :- !,
	do_use_module(File, all, Base, Ln0, Ln1).
process_decl(import(Module,Imports), Base,_M,_VNs, Ln0, Ln1) :- !,
	do_import(Module, Imports, Base, Ln0, Ln1).
process_decl(ensure_loaded(File), Base,_M,_VNs,_Ln0,_Ln1) :- !,
	get_base_name(File, _, _, _),
	assertz_fact(adds(Base,File)).
process_decl(reexport(File), Base,_M,_VNs, Ln0, Ln1) :- !,
	do_reexport(File, all, Base, Ln0, Ln1).
process_decl(reexport(File,Preds), Base,_M,_VNs, Ln0, Ln1) :- !,
	do_reexport(File, Preds, Base, Ln0, Ln1).
process_decl(meta_predicate(Spec), Base,_M,_VNs, Ln0, Ln1) :- !,
	do_meta_predicate(Spec, Base, Ln0, Ln1).
process_decl(multifile(Spec), Base,_M,_VNs, Ln0, Ln1) :- !,
	do_multifile(Spec, Base, Ln0, Ln1).
process_decl(data(L), Base,_M,_VNs, Ln0, Ln1) :- !,
	do_dyn_decl(L, Base, data, Ln0, Ln1).
process_decl(dynamic(L), Base,_M,_VNs, Ln0, Ln1) :- !,
	do_dyn_decl(L, Base, dynamic, Ln0, Ln1).
process_decl(concurrent(L), Base,_M,_VNs, Ln0, Ln1) :- !,
	do_dyn_decl(L, Base, concurrent, Ln0, Ln1).
process_decl(impl_defined(L), Base,_M,_VNs, Ln0, Ln1) :- !,
	do_impl_defined(L, Base, Ln0, Ln1).
process_decl(discontiguous(L), Base,_M,_VNs, Ln0, Ln1) :- !,
	do_discontiguous(L, Base, Ln0, Ln1).
process_decl(redefining(P), Base,_M,_VNs, Ln0, Ln1) :- !,
	do_redefining(P, Base, Ln0, Ln1).
process_decl(load_compilation_module(File), Base,_M,_VNs,_Ln0,_Ln1) :- !,
	get_base_name(File, BFile, _, _),
	assertz_fact(loads(Base, File)),
	do_load_compilation_module(BFile, File, Base).
process_decl(op(P, F, O), Base,_M,_VNs,_Ln0,_Ln1) :- !,
	do_op(P, F, O, Base).
process_decl(set_prolog_flag(Flag, Value), Base,_M,_VNs, Ln0, Ln1) :- !,
	do_set_pl_flag(Flag, Value, Base, Ln0, Ln1).
process_decl(push_prolog_flag(Flag, Value), Base,_M,_VNs, Ln0, Ln1) :- !,
	do_push_pl_flag(Flag, Value, Base, Ln0, Ln1).
process_decl(pop_prolog_flag(Flag), Base,_M,_VNs, Ln0, Ln1) :- !,
	do_pop_pl_flag(Flag, Base, Ln0, Ln1).
process_decl(new_declaration(S), Base,_M,_VNs, Ln0, Ln1) :- !,
	do_new_decl(S, off, Base, Ln0, Ln1).
process_decl(new_declaration(S, ITF), Base,_M,_VNs, Ln0, Ln1) :- !,
	do_new_decl(S, ITF, Base, Ln0, Ln1).
process_decl(add_sentence_trans(P, Prior), Base, M,_VNs, Ln0, Ln1) :- !,
	do_add_sentence_trans(M, P, Prior, Base, Ln0, Ln1).
process_decl(add_term_trans(P, Prior), Base, M,_VNs, Ln0, Ln1) :- !,
	do_add_term_trans(M, P, Prior, Base, Ln0, Ln1).
% These four processed from clause_of
process_decl(add_clause_trans(_, _),_Base,_M,_VNs,_Ln0,_Ln1) :- !.
process_decl(add_goal_trans(_, _),_Base,_M,_VNs,_Ln0,_Ln1) :- !.
process_decl(initialization(_),_Base,_M,_VNs,_Ln0,_Ln1) :- !.
process_decl(on_abort(_),_Base,_M,_VNs,_Ln0,_Ln1) :- !.
%
process_decl(Decl, _Base, _M,_VNs, Ln0, Ln1) :-
	deprecated_decl_error(Decl, Msg), !,
	functor(Decl, F, A),
	compiler_error(Ln0, Ln1, deprecated_decl_error(F, A, [' '|Msg])).
% User-defined declarations
process_decl(D, Base,_M,_VNs,_Ln0,_Ln1) :-
	new_decl(Base, D, ITF), !,
	( ITF = on -> assertz_fact(decl(Base, D)) ; true).

% Deprecated declarations where recovery is not possible
deprecated_decl_error(add_clause_trans(_), 'Use add_clause_trans/2').
deprecated_decl_error(add_goal_trans(_), 'Use add_goal_trans/2').
deprecated_decl_error(add_sentence_trans(_), 'Use add_sentence_trans/2').
deprecated_decl_error(add_term_trans(_), 'Use add_term_trans/2').

do_use_package(F, Base, Module, Ln0, Ln1) :-
	% Disable this type of warnings in packages:
	do_push_pl_flag(unused_pred_warnings, no, Base, Ln0, Ln1),
	do_use_package_(F, Base, Module, Ln0, Ln1),
	do_pop_pl_flag(unused_pred_warnings, Base, Ln0, Ln1).

do_use_package_([], _, _, _, _) :- !.
do_use_package_([F|Fs], Base, Module, Ln0, Ln1) :- !,
	do_use_package_(F, Base, Module, Ln0, Ln1),
	do_use_package_(Fs, Base, Module, Ln0, Ln1).
do_use_package_(F, Base, Module, Ln0, Ln1) :-
	package_file(F, P), !,
	( current_fact(package(Base,P)) -> true
	; assertz_fact(package(Base,P)),
	  do_include(package, P, Base, Module, Ln0, Ln1)
	).
do_use_package_(F, _, _, Ln0, Ln1) :-
	compiler_error(Ln0, Ln1, bad_package_file(F)).

package_file(F, P) :-
	atom(F) -> P = library(F)
	;
	functor(F,_,1) -> P = F.

% Type can be 'source' for included files, 'package' for packages.
do_include(Type, File, Base, Module,_Ln0,_Ln1) :-
	nonvar(File),
	get_base_name(File, SourceBase, SourceFile, _), !,
	assertz_fact(includes(Base, File)),
	now_doing_include(Type, SourceFile),
	'$open'(SourceFile, r, Stream),
	read_sentence(Stream, Base, Sentence),
	check_include_decl(Type, SourceBase, Sentence, Rest),
	% TODO: Merge with read_record_file_/3
	( member(Sentence2, Rest) ; read_sentence(Stream, Base, Sentence2) ),
	( Sentence2 = end_of_file(_, _) ->
	    true
	; process_sentence(Sentence2, Base, SourceFile, Module),
	  fail
	),
	!,
	close(Stream),
	end_doing.
do_include(_Type, File,_Base,_Module, Ln0, Ln1) :-
	compiler_error(Ln0, Ln1, bad_file(File)).

now_doing_include(source, SourceFile) :- now_doing(['Including ',SourceFile]).
now_doing_include(package, SourceFile) :- now_doing(['Using package ',SourceFile]).

% Check that packages contains the right declarations. Nothing is
% required for included source.
check_include_decl(source, _, Sentence, [Sentence]).
check_include_decl(package, SourceBase, Sentence, Sentences) :-
	( Sentence = sentence(Data, _, _, Ln0, Ln1),
	  Data = (:- package(M)) ->
	    Sentences = [],
	    module_from_base(SourceBase, SM),
	    ( SM = M -> % Allow vars in package declarations
	        true
	    ; compiler_error(Ln0, Ln1, bad_package(SourceBase, M))
	    )
	; % Do not consume the sentence, it is not a valid package declaration
          Sentences = [Sentence],
	  sentence_lines(Sentence, Ln0, Ln1),
	  warning_package_missing(Ln0, Ln1)
	).
	
sentence_lines(sentence(_,_,_,Ln0,Ln1), Ln0, Ln1).
sentence_lines(end_of_file(Ln0,Ln1), Ln0, Ln1).
	
warning_package_missing(L0, L1) :-
	error_in_lns(L0, L1, warning,
	             ['Source used as package without package declaration']).

assert_export_list(All, Base, _Ln0,_Ln1) :-
	var(All), !,
	assertz_fact(exports_pred(Base, all, all)).
assert_export_list([Exp|Exports], Base, Ln0, Ln1) :- !,
	assert_export(Exp, Base, Ln0, Ln1),
	assert_export_list(Exports, Base, Ln0, Ln1).
assert_export_list([],_Base,_Ln0,_Ln1) :- !.
assert_export_list(Exp, Base, Ln0, Ln1) :-
	assert_export(Exp, Base, Ln0, Ln1).

assert_export(F/A, Base,_Ln0,_Ln1) :-
	atom(F), integer(A), !,
	assertz_fact(exports_pred(Base, F, A)).
assert_export(Spec,_Base, Ln0, Ln1) :-
	compiler_error(Ln0, Ln1, bad_export(Spec)).

do_use_module(UsedFile, Imports, Base, Ln0, Ln1) :-
	nonvar(UsedFile),
	( UsedFile = user -> true
	; get_base_name(UsedFile, UsedBase, _, _),
	  ( UsedBase = Base ->  Ignore = true
	  ; current_fact(uses(Base, UsedFile)) -> true
	  ; assertz_fact(uses(Base, UsedFile))
	  )
	), !,
	( nonvar(Ignore) -> true
	; store_imports(Imports, UsedFile, Base, Ln0, Ln1)
	).
do_use_module(UsedFile,_Imports,_Base, Ln0, Ln1) :-
	compiler_error(Ln0, Ln1, bad_use_module(UsedFile)).

store_imports(all, user,_Base, Ln0, Ln1) :- !,
	compiler_error(Ln0, Ln1, all_user).
store_imports(all, File, Base,_Ln0,_Ln1) :- !,
	assertz_fact(imports_all(Base, File)).
store_imports(Imports, File, Base, Ln0, Ln1) :-
	store_import_list(Imports, File, Base, Ln0, Ln1).

store_import_list([I|Is], File, Base, Ln0, Ln1) :- !,
	store_import(I, File, Base, Ln0, Ln1),
	store_import_list(Is, File, Base, Ln0, Ln1).
store_import_list([], _, _, _, _) :- !.
store_import_list(Bad, _, _, Ln0, Ln1) :-
	compiler_error(Ln0, Ln1, bad_import_list(Bad)).

store_import(F/A, File, Base, _, _) :-
	atom(F), integer(A), !,
	assertz_fact(imports_expl(Base, File, F, A)).
store_import(Bad, _, _, Ln0, Ln1) :-
	compiler_error(Ln0, Ln1, bad_import_spec(Bad)).

do_import(Module, Imports, Base, Ln0, Ln1) :-
	atom(Module), !,
	store_import_nocheck_list(Imports, Module, Base, Ln0, Ln1).
do_import(Module, _, _, Ln0, Ln1) :-
	compiler_error(Ln0, Ln1, bad_import(Module)).

store_import_nocheck_list([I|Is], Module, Base, Ln0, Ln1) :- !,
	store_import_nocheck(I, Module, Base, Ln0, Ln1),
	store_import_nocheck_list(Is, Module, Base, Ln0, Ln1).
store_import_nocheck_list([], _, _, _, _) :- !.
store_import_nocheck_list(Bad, _, _, Ln0, Ln1) :-
	compiler_error(Ln0, Ln1, bad_import_list(Bad)).

store_import_nocheck(F/A, Module, Base, _, _) :-
	atom(F), integer(A), !,
	assertz_fact(imports_nocheck(Base, Module, F, A)).
store_import_nocheck(Bad, _, _, Ln0, Ln1) :-
	compiler_error(Ln0, Ln1, bad_import_spec(Bad)).

do_reexport(UsedFile, Preds, Base, Ln0, Ln1) :-
	nonvar(UsedFile),
	get_base_name(UsedFile, _, _, _), !,
	( current_fact(uses(Base, UsedFile)) -> true
	; assertz_fact(uses(Base, UsedFile))
	),
	( current_fact(reexports_from(Base, UsedFile)) -> true
	; assertz_fact(reexports_from(Base, UsedFile))
	),
	store_reexports(Preds, UsedFile, Base, Ln0, Ln1).
do_reexport(UsedFile,_Preds,_Base, Ln0, Ln1) :-
	compiler_error(Ln0, Ln1, bad_file(UsedFile)).

store_reexports(all, File, Base,_Ln0,_Ln1) :- !,
	assertz_fact(reexports_all(Base, File)).
store_reexports(Preds, File, Base, Ln0, Ln1) :-
	store_reexport_list(Preds, File, Base, Ln0, Ln1).

store_reexport_list([P|Ps], File, Base, Ln0, Ln1) :- !,
	store_reexport(P, File, Base, Ln0, Ln1),
	store_reexport_list(Ps, File, Base, Ln0, Ln1).
store_reexport_list([], _, _, _, _) :- !.
store_reexport_list(Bad, _, _, Ln0, Ln1) :-
	compiler_error(Ln0, Ln1, bad_import_list(Bad)).

store_reexport(F/A, File, Base, _, _) :-
	atom(F), integer(A), !,
	assertz_fact(reexports(Base, File, F, A)).
store_reexport(Bad, _, _, Ln0, Ln1) :-
	compiler_error(Ln0, Ln1, bad_import_spec(Bad)).

do_meta_predicate(V, _, Ln0, Ln1) :- var(V), !,
	compiler_error(Ln0, Ln1, bad_meta_predicate(V)).
%do_meta_predicate((Spec0,Spec), Base, Ln0, Ln1) :- !,
%        do_meta_predicate(Spec0, Base, Ln0, Ln1),
%        do_meta_predicate(Spec, Base, Ln0, Ln1).
do_meta_predicate(Spec, Base,_Ln0,_Ln1) :-
	functor(Spec, F, A),
	atom(F), integer(A),
	functor(NSpec, F, A),
	normalize_meta_args(1, A, Spec, NSpec), !,
	assertz_fact(meta_pred(Base,F,A,NSpec)).
do_meta_predicate(Bad, _, Ln0, Ln1) :-
	compiler_error(Ln0, Ln1, bad_meta_predicate(Bad)).


normalize_meta_args(N, A, _, _):- N>A, !.
normalize_meta_args(N, A, Spec, NSpec):-
	arg(N, Spec, X),
	normalize_meta_arg(X, NX),
	arg(N, NSpec, NX),
	N1 is N+1,
	normalize_meta_args(N1, A, Spec, NSpec).

normalize_meta_arg('?', '?'). % A variable is valid also!
normalize_meta_arg('-', '?').
normalize_meta_arg('+', '?').
normalize_meta_arg(':', goal).
normalize_meta_arg(addmodule, addmodule(?)).
normalize_meta_arg(addmodule(X), addmodule(Xn)) :-
	normalize_meta_arg(X, Xn).
normalize_meta_arg(X, X) :- real_meta_arg(X).

real_meta_arg(goal).
real_meta_arg(clause).
real_meta_arg(retracting_clause).
real_meta_arg(fact).
real_meta_arg(spec).
real_meta_arg(pred(N)) :- integer(N), N>=0, N=<255.
real_meta_arg(list(X)) :- real_meta_arg(X).
real_meta_arg(addterm(X)) :- real_meta_arg(X).

do_multifile(Spec, Base, Ln0, Ln1) :-
	sequence_contains(Spec, bad_spec_error(multifile, Ln0, Ln1), F, A),
	  ( retract_fact(defines_pred(Base,F,A)) -> true ; true ),
	  add_multifile_pred(Base,F,A),
	fail.
do_multifile(_, _, _, _).

add_multifile_pred(Base,F,A) :-
	  ( current_fact(multifile_pred(Base,F,A)) -> true
	  ; assertz_fact(multifile_pred(Base,F,A))
	  ).

do_dyn_decl(Spec, Base, Decl, Ln0, Ln1) :-
	sequence_contains(Spec, bad_spec_error(Decl, Ln0, Ln1), F, A),
	  defined_in_source(Base, F, A),
	  assert_dyn_decl(Base, F, A, Decl, Ln0, Ln1),
	fail.
do_dyn_decl(_, _, _, _, _).

assert_dyn_decl(Base, F, A, Decl, Ln0, Ln1) :-
	dyn_decl(Base, F, A, Decl2), !,
	( Decl2 = Decl -> true
	; compiler_error(Ln0, Ln1, incompatible_decl(F,A,Decl,Decl2))
	).
assert_dyn_decl(Base, F, A, Decl,_Ln0,_Ln1) :-
	assertz_fact(dyn_decl(Base, F, A, Decl)).


do_impl_defined(SL, Base, Ln0, Ln1) :-
	sequence_contains(SL, bad_spec_error(impl_defined, Ln0, Ln1), F, A),
	  defined_in_source(Base, F, A),
	  asserta_fact(impl_defines(Base, F, A)),
	fail.
do_impl_defined(_, _, _, _).

bad_spec_error(Spec, Decl, Ln0, Ln1) :-
	compiler_error(Ln0, Ln1, badly_formed(Decl,Spec)).

defined_in_source(Base, F, A) :-
	multifile_pred(Base, F, A), !.
defined_in_source(Base, F, A) :-
	defines_pred(Base, F, A), !.
defined_in_source(Base, F, A) :-
	assertz_fact(defines_pred(Base,F,A)).

do_discontiguous(L, Base, Ln0, Ln1) :-
	sequence_contains(L, bad_spec_error(discontiguous, Ln0, Ln1), F, A),
	  asserta_fact(discontiguous(F,A,Base)),
	fail.
do_discontiguous(_, _, _, _).

do_redefining(F/A, Base, _, _) :- !,
	defines_module(Base, M),
	asserta_fact(redefining(M, F, A)).
do_redefining(Bad,_Base, Ln0, Ln1) :-
	error_in_lns(Ln0, Ln1, error,
	             ['bad predicate indicator pattern ',~~(Bad)]).

do_load_compilation_module(BFile, _, _) :-
	in_mode(In),
	processed(BFile, In), !.
do_load_compilation_module(_, File, Base) :-
	undo_decls(Base),
	this_module(M),
	use_mod(File, all, M),
	redo_decls(Base).

:- data in_mode/1.
in_mode(in).

new_in_mode(NIn) :-
	in_mode(In), !,
	atom_concat(In, $, NIn),
	asserta_fact(in_mode(NIn)).

del_in_mode(In) :-
	retract_fact(in_mode(In)).

do_op(P, F, O, Base) :-
	( ensure_op_undone(P, F, O, Base),
	  op(P, F, O), ! % This can give errors
	; true).

ensure_op_undone(Prec, F, Ops, Base) :-
	integer(Prec), 0=<Prec, Prec=<1200,
	nonvar(F),
	op_type(F, T),
	atom_or_atom_list(Ops), !,
	ensure_ops_undone(Ops, F, T, Prec, Base).
ensure_op_undone(_, _, _, _). % do not fail to give errors

ensure_ops_undone([Op|Ops], F, T, Prec, Base) :- !,
	ensure_ops_undone(Op, F, T, Prec, Base),
	ensure_ops_undone(Ops, F, T, Prec, Base).
ensure_ops_undone([], _, _, _, _) :- !.
ensure_ops_undone(Op, F, T, Prec, Base) :-
	( current_op(CPrec, CF, Op), op_type(CF, T) ->
	  asserta_fact(undo_decl(Base,op(Prec,F,Op),op(CPrec,CF,Op)))
	; asserta_fact(undo_decl(Base,op(Prec,F,Op),op(0,F,Op)))
	).

op_type(fy, pre).
op_type(fx, pre).
op_type(yfx, in).
op_type(xfy, in).
op_type(xfx, in).
op_type(yf, post).
op_type(xf, post).

atom_or_atom_list(A) :- atom(A), !.
atom_or_atom_list([A|L]) :-
	atom(A),
	atom_or_atom_list(L).

do_set_pl_flag(Flag, Value, Base, Ln0, Ln1) :-
	( prolog_flag(Flag, Old, Value) ->
	    asserta_fact(undo_decl(Base, set_prolog_flag(Flag,Value),
	                                 set_prolog_flag(Flag,Old)))
	; warning_failed_decl(Ln0, Ln1, set_prolog_flag(Flag, Value))
	).

do_push_pl_flag(Flag, Value, Base, Ln0, Ln1) :-
	( push_prolog_flag(Flag, Value) ->
	    asserta_fact(undo_decl(Base, push_prolog_flag(Flag,Value),
	                                 pop_prolog_flag(Flag)))
	; warning_failed_decl(Ln0, Ln1, push_prolog_flag(Flag, Value))
	).

do_pop_pl_flag(Flag, Base, Ln0, Ln1) :-
	( current_prolog_flag(Flag, Value),
	  pop_prolog_flag(Flag) ->
	    asserta_fact(undo_decl(Base, pop_prolog_flag(Flag),
	                                 push_prolog_flag(Flag,Value)))
	; warning_failed_decl(Ln0, Ln1, pop_prolog_flag(Flag))
	).

do_add_sentence_trans(M, P, Prior, Base, Ln0, Ln1) :-
	( add_sentence_trans_and_init(M, P, Prior) ->
	    asserta_fact(undo_decl(Base, add_sentence_trans(M, P, Prior),
	                                 del_sentence_trans(M)))
	; warning_failed_decl(Ln0, Ln1, add_sentence_trans(P, Prior))
	).

do_add_term_trans(M, P, Prior, Base, Ln0, Ln1) :-
	% TODO: if predicate P does not exist, compilation complains a lot of times
	( add_term_trans(M, P, Prior) ->
	    asserta_fact(undo_decl(Base, add_term_trans(M, P, Prior),
	                                 del_term_trans(M)))
	; warning_failed_decl(Ln0, Ln1, add_term_trans(P, Prior))
	).

warning_failed_decl(Ln0, Ln1, Decl) :-
	error_in_lns(Ln0, Ln1, warning, [Decl,' - declaration failed']).

do_new_decl(S, ITF, Base, Ln0, Ln1) :-
	( S = F/A, functor(D, F, A) ->
	    asserta_fact(new_decl(Base, D, ITF))
	; compiler_error(Ln0, Ln1, badly_formed(new_declaration, S))
	).

exports_meta(Base, F, A) :-
	meta_pred(Base, F, A1, Meta),
	meta_inc_args(Meta, A1, A),
	exports_pred(Base, F, A1).

exports_meta_pred(Base, F, A) :- exports_pred(Base, F, A).
exports_meta_pred(Base, F, A) :- exports_meta(Base, F, A).

clause_check(F, A, Base,_Ln0,_Ln1) :-
	reading_pred(F, A, Base), !.
clause_check(F, A, Base, Ln0, Ln1) :-
	current_prolog_flag(multi_arity_warnings, on),
	pred_read(F, A0, Base),
	A0 =\= A,
	\+ (exports_meta_pred(Base, F, A), exports_meta_pred(Base, F, A0)),
	error_in_lns(Ln0, Ln1, warning, ['predicate ',~~(F/A),
	             ' is already defined with arity ',A0]),
	fail.
clause_check(F, A, Base, Ln0, Ln1) :-
	already_read_pred(F, A, Base),
	current_prolog_flag(discontiguous_warnings, on),
	\+ discontiguous(F, A, Base),
	error_in_lns(Ln0, Ln1, warning,
	             ['clauses of ',~~(F/A),' are discontiguous']),
	fail.
clause_check(F, A, Base,_Ln0,_Ln1) :-
	retractall_fact(reading_pred(_,_,Base)),
	asserta_fact(reading_pred(F,A,Base)).

already_read_pred(F, A, Base) :- pred_read(F, A, Base), !.
already_read_pred(F, A, Base) :- asserta_fact(pred_read(F,A,Base)), fail.

singleton_check(Singletons, F, A, Ln0, Ln1) :-
	current_prolog_flag(single_var_warnings, on), !,
	no_underlines(Singletons, BadSingletons),
	singleton_check1(BadSingletons, F, A, Ln0, Ln1).
singleton_check(_, _, _, _, _).

no_underlines([], []).
no_underlines([N=_|Eqs], Ns) :-
	( atom_concat('_', _, N) ->
	      no_underlines(Eqs, Ns)
	; Ns = [N|Ns_],
	  no_underlines(Eqs, Ns_)
	).

singleton_check1([], _, _, _, _) :- !.
singleton_check1(BadSingletons, F, A, Ln0, Ln1) :-
	error_in_lns(Ln0, Ln1, warning,
	  [BadSingletons,' - singleton variables in ',~~(F/A)]).

gen_exports(Base) :-
	exports_pred(Base, all, all), !,
	retractall_fact(exports_pred(Base, _, _)),
	( defines_pred(Base, F, A),
	    gen_export(Base, F, A),
	  fail
	; true
	).
gen_exports(Base) :-
	retract_fact(exports_pred(Base, F, A)),
	  ( multifile_pred(Base, F, A) ->
	      error_in_lns(_,_,warning,
	                   ['no need to export multifile predicate ',~~(F/A)])
	  ; gen_export(Base, F, A)
	  ),
	fail.
gen_exports(_Base).

gen_export(Base, F, A) :-
	def_type(Base, F, A, DefType),
	(meta_pred(Base, F, A, Meta) -> true ; Meta = 0),
	assertz_fact(direct_export(Base,F,A,DefType,Meta)).

def_type(Base, F, A, DefType) :-
	( dyn_decl(Base, F, A, DefType) -> true
	; impl_defines(Base, F, A) -> DefType = implicit
	; DefType = static
	).

gen_def_multifile(Base) :-
	retract_fact(multifile_pred(Base, F, A)),
	  ( retract_fact(dyn_decl(Base,F,A,DynType)) -> true
	  ; DynType = static
	  ),
	  assertz_fact(def_multifile(Base,F,A,DynType)),
	fail.
gen_def_multifile(_).

:- data module_error/0, module_error/1.

% fails if incorrect imports/exports
check_itf_data(Base, PlName) :-
	now_doing(['Checking interface data of ',PlName]),
	gen_imports(Base),
	check_exports(Base),
	check_multifile(Base),
	do_expansion_checks(Base),
	\+ current_fact(module_error),
	\+ current_fact(syntax_error_in(Base)),
	end_doing.
check_itf_data(Base, _) :-
	retractall_fact(module_error),
	assertz_fact(module_error(Base)),
	end_doing,
	message(['{Compilation aborted}']),
	signal_compilation_error,
	fail.

gen_imports(Base) :-
	( imports_all(Base, File) ; reexports_all(Base, File) ),
	base_name(File, BFile),
	( direct_export(BFile, F, A, DefType, Meta),
	  EndFile = '.'
	; indirect_export(BFile, F, A, DefType, Meta, EndFile)
	),
	asserta_fact(imports_pred(Base, File, F, A, DefType, Meta, EndFile)),
	fail.
gen_imports(Base) :-
	( retract_fact(imports_expl(Base, File, F, A))
	; reexports(Base, File, F, A)
	),
	base_name_or_user(File, BFile),
	( exports_thru(BFile, F, A, DefType, Meta, EndFile) ->
	    asserta_fact(
	      imports_pred(Base, File, F, A, DefType, Meta, EndFile)
	    )
	; defines_module(BFile, IM),
	  interface_error(not_exported(IM,F/A))
	),
	fail.
gen_imports(_).

base_name_or_user(user, Base) :- !, Base = user.
base_name_or_user(File, Base) :- base_name(File, Base).

exports_thru(user,_F,_A, DefType, Meta, EndFile) :- !,
	DefType = static, Meta = 0, EndFile = '.'.
exports_thru(BFile, F, A, DefType, Meta, '.') :-
	direct_export(BFile, F, A, DefType, Meta).
exports_thru(BFile, F, A, DefType, Meta, EndFile) :-
	indirect_export(BFile, F, A, DefType, Meta, EndFile).

indirect_export(BFile, F, A, DefType, Meta, EndFile) :-
	reexports_pred(BFile, MFile, F, A),
	base_name(MFile, BMFile),
	( direct_export(BMFile, F, A, DefType, Meta), EndFile = MFile
	; indirect_export(BMFile, F, A, DefType, Meta, EndFile)
	),
	\+ direct_export(BFile, F, A, _, _).

reexports_pred(Base, File, F, A) :-
	reexports(Base, File, F, A).
reexports_pred(Base, File,_F,_A) :-
	reexports_all(Base, File).

check_exports(Base) :-
	direct_export(Base, F, A, _, Meta),
	  meta_inc_args(Meta, A, A1),
	  ( defines_pred(Base, F, A1) -> true
	  ; error_in_lns(_,_,warning, ['exported predicate ',~~(F/A),
	                 ' is not defined in this module'])
	  ),
	fail.
check_exports(_).

check_multifile(Base) :-
	def_multifile(Base, F, A, T),
	def_multifile(Base1, F, A, T1),
	T \== T1,
	defines_module(Base1, M1),
	interface_error(incompatible_multifile(F,A,T,T1,M1)),
	fail.
check_multifile(_).

interface_error(Error) :-
	compiler_error(_, _, Error),
	asserta_fact(module_error).

do_expansion_checks(Base) :-
	retract_fact(expansion_check(Base, Pred)),
	  Pred(Base),
	fail.
do_expansion_checks(_).

generate_itf(ItfName, Dir, Mode, Base) :-
	prolog_flag(fileerrors, OldFE, off),
	( stream_of_file(ItfName, Dir, Stream, Ref) ->
	    current_output(CO),
	    set_output(Stream),
	    itf_version(V),
	    current_prolog_flag(itf_format, Format),
	    display_term(v(V,Format)),
	    write_itf_data_of(Format, Base),
	    set_output(CO),
	    close(Stream),
	    chmod(ItfName, Mode),
	    erase(Ref)
	; message(warning, ['cannot create ',ItfName])
	),
	time(Now),
	assertz_fact(time_of_itf_data(Base,Now)),
	set_prolog_flag(fileerrors, OldFE).

write_itf_data_of(Format, Base) :-
	itf_data(ITF, Base, _, Fact),
	  current_fact(Fact),
	    do_write(Format, ITF),
	fail.
write_itf_data_of(_, _).

do_write(f,Term) :- fast_write(Term).
do_write(r,Term) :- display_term(Term).

read_itf(ItfName, ItfTime, Base, Dir, Type) :-
	working_directory(OldDir, Dir),
	( true ; working_directory(_, OldDir), fail ),
	( current_fact(time_of_itf_data(Base, ItfDataTime)),
	  ItfDataTime >= ItfTime ->
	    base_names_in_itf(ItfName, Base)
	; do_read_itf(ItfName, Base)
	),
	defines_module(Base, M),
	( M = user(_), Type == module ->
	    warning_module_missing(_, _)
	; true),
	end_doing, !,
	assertz_fact(already_have_itf(Base)),
	working_directory(_, OldDir).

base_names_in_itf(ItfName, Base) :-
	now_doing(['Checking data of ',ItfName]),
	( uses(Base, File)
	; adds(Base,File)
	; includes(Base,File)
	; loads(Base,File)
	; imports_pred(Base, _, _, _, _, _, File)
	),
	do_get_base_name(File),
	fail.
base_names_in_itf(_,_).

do_read_itf(ItfName, Base) :-
	delete_itf_data(Base),
	'$open'(ItfName, r, Stream),
	current_input(CI),
	set_input(Stream),
	( itf_version(V),
	  read(v(V,Format)), !
	; set_input(CI),
	  close(Stream),
	  fail
	),
	now_doing(['Reading ',ItfName]),
	read_itf_data_of(Format,Base),
	set_input(CI),
	time(Now),
	close(Stream),
	assertz_fact(time_of_itf_data(Base,Now)).

read_itf_data_of(Format,Base) :-
	repeat,
	  do_read(Format,ITF),
	( ITF = end_of_file, !
	; itf_data(ITF, Base, File, Fact),
	  do_get_base_name(File),
	  assertz_fact(Fact),
	  fail
	).

do_read(f,Term) :- fast_read(Term), ! ; Term = end_of_file.
do_read(r,Term) :- read(Term).

% Catch file errors now
do_get_base_name('.') :- !.
do_get_base_name(user) :- !.
do_get_base_name(File) :- get_base_name(File, _, _, _).

itf_version(3).

:- meta_predicate itf_data(?, ?, ?, fact).

itf_data(m(M),             Base, user, defines_module(Base,M)).
itf_data(e(F,A,Def,Meta),  Base, user, direct_export(Base,F,A,Def,Meta)).
itf_data(m(F,A,Def),       Base, user, def_multifile(Base,F,A,Def)).
itf_data(u(File),          Base, File, uses(Base,File)).
itf_data(e(File),          Base, File, adds(Base,File)).
itf_data(n(File),          Base, File, includes(Base,File)).
itf_data(l(File),          Base, File, loads(Base,File)).
% The following five has File in uses/2
itf_data(h(File),          Base, user, reexports_from(Base,File)).
itf_data(i(File,F,A,Df,Mt,EF),Base, EF,imports_pred(Base,File,F,A,Df,Mt,EF)).
itf_data(i(File),          Base, user, imports_all(Base,File)).
itf_data(r(File,F,A),      Base, user, reexports(Base,File,F,A)).
itf_data(r(File),          Base, user, reexports_all(Base,File)).
itf_data(d(Decl),          Base, user, decl(Base,Decl)).

generate_module_data(Base, M) :-
	defines_pred(Base, F, A),
	  assertz_fact(defines(M, F, A)),
	fail.
generate_module_data(Base, M) :-
	def_multifile(Base, F, A, Def),
	  assertz_fact(multifile(M, F, A, Def)),
	fail.
generate_module_data(Base, M) :-
	meta_pred(Base, _, _, Meta),
	  assertz_fact(meta_args(M, Meta)),
	fail.
generate_module_data(Base, M) :-
	imports_pred(Base, File, F, A, _, Meta, EndFile),
	  file_defines_module(File, IM),
	  ( EndFile = '.' -> EM = IM ; file_defines_module(EndFile, EM) ),
	  assertz_fact(imports(M, IM, F, A, EM)),
	  Meta \== 0,
	  assertz_fact(meta_args(EM, Meta)),
	fail.
generate_module_data(Base, M) :-
	imports_nocheck(Base, IM, F, A),
	  assertz_fact(imports(M, IM, F, A, IM)),
	fail.
generate_module_data(_, _).

delete_module_data(M) :-
	% JFMC & EMM
	( current_prolog_flag(keep_assertions, yes) ->
	    true
	; retractall_fact(assertion_read(_, M, _, _, _, _, _, _, _))
	),
	%
	retractall_fact(defines(M,_,_)),
	retractall_fact(multifile(M,_,_,_)),
	retractall_fact(meta_args(M,_)),
	retractall_fact(imports(M,_,_,_,_)),
	retractall_fact(redefining(M, _, _)).

% Deftype not checked because is not exact in builtin modules
changed_dependences(Base, _) :-
	imports_pred(Base, File, F, A,_DefType, Meta, EndFile),
	base_name(File, BFile),
	\+ exports_thru(BFile, F, A,_DefType2, Meta, EndFile).
changed_dependences(Base, _) :-
	( imports_all(Base, File); reexports_all(Base, File) ),
	base_name(File, BFile),
	( direct_export(BFile, F, A, _, _)
	; indirect_export(BFile, F, A, _, _, _) ),
	\+ imports_pred(Base, File, F, A, _, _, _).
changed_dependences(Base, ItfTime) :-
	includes(Base, File),
	base_name(File, BFile),
	file_data(BFile, PlName, _),
	modif_time(PlName, PlTime),
	PlTime > ItfTime.
changed_dependences(Base, ItfTime) :-
	loads(Base, File),
	base_name(File, Base2),
	( file_data(Base2, PlName2, _),
	  modif_time(PlName2, PlTime2),
	  PlTime2 > ItfTime
	; itf_filename(Base2, ItfName),
	  modif_time0(ItfName, ItfTime2),
	  ItfTime2 > ItfTime
	).

:- meta_predicate sequence_contains(+, pred(1), -, -).

sequence_contains(V, BadP, _, _) :- var(V), !,
	BadP(V), fail.
sequence_contains([], _, _, _) :- !, fail.
sequence_contains([S|Ss], BadP, F, A) :- !,
	( sequence_contains(S, BadP, F, A)
	; sequence_contains(Ss, BadP, F, A)
	).
sequence_contains((S,Ss), BadP, F, A) :- !,
	( sequence_contains(S, BadP, F, A)
	; sequence_contains(Ss, BadP, F, A)
	).
sequence_contains(F/A, _, F, A) :-
	atom(F), integer(A), !.
sequence_contains(S, BadP, _, _) :-
	BadP(S), fail.


module_from_base(B, M) :-
	opt_suff(Opt),
	(atom_concat(BB,Opt,B), ! ; BB = B),
	atom_codes(BB, BS),
	no_path_file_name(BS, MS),
	atom_codes(M, MS).

skip_shell_lines(Stream) :-
	peek_code(Stream, 0'#), !,
	current_input(OldIn),
	set_input(Stream),
	skip_code(10),
	get_line(Line),
	skip_lines_until_blank(Line),
	set_input(OldIn).
skip_shell_lines(_).

skip_lines_until_blank(Line) :-
	whitespace0(Line, []), !.
skip_lines_until_blank(_) :-
	get_line(Line),
	skip_lines_until_blank(Line).

handle_exc(cannot_create(File)) :- !,
	message(error, ['Unable to create ',File,' - aborting...']), 
	signal_compilation_error.
handle_exc(error(Error, _)) :-
	handle_error(Error), !, signal_compilation_error.
handle_exc(unintercepted_signal(control_c)) :- !,
	ctrlcclean, signal_compilation_error.
handle_exc(compiler_loop) :- !, 
	signal_compilation_error, 
	fail.
handle_exc(Error) :- throw(Error).

handle_error(existence_error(source_sink,File)) :-
	error_in_lns(_,_,error, ['File ',File,' not found - aborting...']).
handle_error(permission_error(open,source_sink,File)) :-
	message(error, ['Cannot open ',File,' - aborting...']).

warning_module_missing(L0, L1) :-
	error_in_lns(L0, L1, warning,
	             ['Source used as module without module declaration']).

compiler_error(L0, L1, Error) :-
	compiler_error_data(Error, Message),
	error_in_lns(L0, L1, error, Message).

compiler_error_data(module_missing,
	['Source used as module without module declaration']).
compiler_error_data(bad_module(_Base, M),
	['Bad module ',M,' in module declaration']).
compiler_error_data(bad_package(_Base, M),
	['Bad package ',M,' in package declaration']).
compiler_error_data(badly_formed(Decl, Spec),
	['Bad predicate indicator ',~~(Spec),' in ',Decl,' directive']).
compiler_error_data(nonstarting(F,A),
	['Declaration ',~~(F/A),' not starting file']).
compiler_error_data(nonstarting_package,
	['Declaration package/1 not starting file, probably a package is being included as a raw file']).
compiler_error_data(bad_package_file(F),
	['Bad package file ',~~(F)]).
compiler_error_data(bad_export(Spec),
	['Bad predicate indicator ',~~(Spec),' in export']).
compiler_error_data(bad_use_module(ModuleFile),
	['Bad/unreadable file ',ModuleFile,' in use_module declaration']).
compiler_error_data(bad_file(File),
	['Bad/unreadable file ',File,' in declaration']).
compiler_error_data(all_user,
	['Attempt to import all user predicates']).
compiler_error_data(bad_import_list(Bad),
	['Bad import/reexport list ',~~(Bad)]).
compiler_error_data(bad_import_spec(Spec),
	['Bad predicate indicator ',~~(Spec),' in import/reexport']).
compiler_error_data(bad_import(Module),
	['Bad module ',Module,' in import declaration']).
compiler_error_data(bad_meta_predicate(Spec),
	['Bad meta_predicate specification ',~~(Spec)]).
compiler_error_data(not_exported(IM,F/A),
	['imported predicate ',~~(F/A),' not exported by ',IM]).
compiler_error_data(incompatible_multifile(F,A,T,T1,M1),
	['multifile predicate ',~~(F/A),' is defined ',T,
	 ' while in module ',M1,' is defined ',T1]).
compiler_error_data(incompatible_decl(F,A,Decl,Decl2),
	['predicate ',~~(F/A),' is being defined ',Decl,
	               ' but it was already defined ',Decl2]).
compiler_error_data(module_redefined(OtherFile, _SourceFile, M),
	['Module ',M,' already defined in source ',OtherFile]).
compiler_error_data(deprecated_decl_error(F,A,Msg),
	['Declaration ',~~(F/A), ' is deprecated.'|Msg]).

%%% --- Compilation to .po --- %%%

false(_) :- fail.

% JF: extension is not enough... 
old_file_extension(Base, Ext) :-
	( filetype(Type, Ext, _) ->
	    product_filename(Type, Base, ExtName)
	; display('Unknown ext '), display(Ext), nl, halt
	),
	modif_time0(ExtName, ExtTime),
	itf_filename(Base, ItfName),
	modif_time0(ItfName, ItfTime),
	ExtTime < ItfTime.

% ---------------------------------------------------------------------------

make_po1(File) :-
	process_file(File, po, any, make_po_file, false, false,
	             po_older_than_itf),
	base_name(File, Base),	     
	make_gluecode(Base).

make_wam1(File) :-
	process_file(File, wam, any, make_wam_file, false, false,
	             wam_older_than_itf).

make_object1(po,  File) :-
	make_po1(File).
make_object1(wam, File) :-
	make_wam1(File).

po_older_than_itf(Base) :-
	po_filename(Base, PoName),
	file_older_than_itf(Base, PoName).

wam_older_than_itf(Base) :-
	wam_filename(Base, WamName),
	file_older_than_itf(Base, WamName).

file_older_than_itf(Base, File) :-
	modif_time0(File, FileTime),
	itf_filename(Base, ItfName),
	modif_time0(ItfName, ItfTime),
	FileTime < ItfTime.

make_po_file(Base) :-
	make_file(Base, po_filename,  ql(unprofiled), 'Compiling ').

make_wam_file(Base) :-
	make_file(Base, wam_filename, wam,            'Assembling ').

:- meta_predicate make_file(+, pred(2), +, +).

make_file(Base, FilePred, Mode, Message) :-
	file_data(Base, Source, Dir),
	defines_module(Base, Module),
	FilePred(Base, PoName),
	now_doing([Message,Source]),
	make_file2_(PoName, Mode, Base, Dir, Module, Source),
	end_doing.

make_file2_(PoName, Mode, Base, Dir, Module, Source) :-
	current_prolog_flag(compress_lib,no),
	make_file3_(PoName, Mode, Base, Dir, Module, Source, Ref, Ok),
	fmode(Source, FMode),
	chmod(PoName, FMode),
	erase(Ref),
	check_ok(PoName, Ok).
make_file2_(PoName, Mode, Base, Dir, Module, Source) :- %OPA
	current_prolog_flag(compress_lib,yes),      
	open(PoName,write,Out),
	mktemp('tmpciaoXXXXXX', TmpFile),
	make_file3_(TmpFile, Mode, Base, Dir, Module, Source, Ref, Ok),
	open(TmpFile,read,TmpStream),
	current_output(So),
	set_output(Out),
	( current_prolog_flag(verbose_compilation,off), !
	; message(['{Compressing library}'])
	),
	compressLZ(TmpStream),
	close(TmpStream),
	close(Out),
	set_output(So),
	fmode(Source, FMode),
	chmod(PoName, FMode),
	erase(Ref),
	delete_file(TmpFile),
	check_ok(PoName, Ok).
make_file2_(PoName, _, _, _, _, _) :-
	( file_exists(PoName) ->
	    message(warning, ['Unable to update ',PoName,
	                      ' - using existing file'])
	; throw(cannot_create(PoName))
	).

make_file3_(PoName, Mode, Base, Dir, Module, Source, Ref, Ok) :-
	stream_of_file(PoName, Dir, Out, Ref), !, % May fail
	reset_counter(Module),
	set_compiler_mode_out(Mode, Out),
	compiler_pass(Source, Base, Module, Mode, Ok),
	retractall_fact(incore_mode_of(_, _)),
	close(Out).

check_ok(PoName, Ok) :-
	( Ok = yes ->
	    true
	; % File had errors, remove .po
	  delete_file(PoName) % todo: improve error handling
	).

stream_of_file(Path, Dir, Stream, Ref) :-
	file_exists(Dir, 2), % Write permission
	( file_exists(Path) -> delete_file(Path) ; true ),
	delete_on_ctrlc(Path, Ref),
	'$open'(Path, w, Stream).

call_goal_trans(T) :-
	arg(1, T, end_of_file),
	'$meta_call'(T),
	!.

end_goal_trans(M) :-
	goal_trans(M, T, _),
	call_goal_trans(T),
	fail.
end_goal_trans(_).

check_assertions_syntax(M) :-
	current_fact(assertion_read(_P, M, _S, T, B, Dict, S, LB, LE), ARef),
%	erase(ARef),
	( T \== modedef, T \== test -> % Skip tests assertions, not processed here
	    asserta_fact(location(S, LB, LE), LRef),
	    assrt_module_expansion(M, T, B, Dict, Def, H, Props),
	    erase(LRef),
	    retract_fact(upw_assrt(ARef, M)),
	    Loc = loc(S, LB, LE),
	    record_assrt_dependency(M, T, Def, Loc, H, Props)
	;
	    retract_fact(upw_assrt(ARef, M))
	),
	fail.
check_assertions_syntax(_).

warn_not_defined(decl, no) :- !.
warn_not_defined(_,    yes).

check_not_defined(no, yes, F, N, M) :-
	!,
	module_warning(not_defined(F, N, M)).
check_not_defined(_, _, _, _, _).

assrt_module_expansion(M, Type, Body, Dict, Defined, H, Props) :-
	assertion_body(PD, Co, Ca, Su, Cp, _Cm, Body),
	functor(PD, F, N),
	assr_head_expansion(PD, M, F, N, MH, H, Defined),
	warn_not_defined(Type, WND),
	check_not_defined(Defined, WND, F, N, M),
% 	Trick to avoid warnings about module qualification:
	( Defined == no -> G = true
	; MH == multifile -> G = PD
	; G = MH:PD
	),
% 	Trick to avoid duplicated warnings about undefined predicates:
	comps_to_goal(Cp, Cp1, G),
% 	This makes the syntax checking:
	map([Co, Ca, Su, [Cp1]], map(expand_subbody(M, Dict)), Props).

% Head expansion for assertions
assr_head_expansion(A, M, F, N, MQ, NA, Defined) :-
	( mexpand_multifile(M, F, N) ->
	    MA = multifile, MQ = MA, Defined = yes
	; mexpand_defines(M, F, N) ->
	      MA = M, MQ = M, Defined = yes
	; mexpand_imports(M, IM, F, N, EM) ->
	    MA = EM, MQ = IM, Defined = yes
	; MA = M, MQ = M, Defined = no
	),
	module_concat(MA, A, NA).

expand_subbody(C, M, Dict, EC) :-
	expand_head_body(in_assertion_body, C, M, Dict, asr, _, EC).
% 	asbody_to_conj(CO, EC).

compiler_pass(Source, Base, Module, Mode, Ok) :-
	del_compiler_pass_data,
	asserta_fact(compiling_src(Source)),
	compile_multifile_decls(Base, Module, Mode),
	compile_dyn_decls(Base, Module, Mode),
	activate_translation(Base, Module, add_clause_trans),
	activate_translation(Base, Module, add_goal_trans),
	%log_translations(Base, Module, clause),
	%log_translations(Base, Module, goal),
	compile_goal_decl(initialization, Base, Module, Mode),
	compile_goal_decl(on_abort, Base, Module, Mode),
	compile_clauses(Base, Module, Mode),
	( current_prolog_flag(unused_pred_warnings, yes) ->
	  record_imports_dependencies(Base, Module)
	; true
	),
	( current_prolog_flag(read_assertions, yes) ->	
	  check_assertions_syntax(Module)
	; true
	),
	unused_pred_warnings(Base, Module),
	(current_fact(mexpand_error) -> Ok = no ; Ok = yes),
	( Ok = yes ->
	    compile_ldlibs(Base, Module, Mode),
	    compile_dependences(Base, Module, Mode),
	    include_module_data(Mode, Module)
	; 
	    message(error, ['Aborted module compilation']), 
	    signal_compilation_error
	),
	end_goal_trans(Module),
	del_clause_trans(Module),
	del_goal_trans(Module),
	end_brace_if_needed,
	cleanup_upw_db(Module),
	cleanup_compilation_data.

activate_translation(Base, Module, Name) :-
	% translation with priority 
	functor(Decl, Name, 2),
	arg(1, Decl, Pred),
	arg(2, Decl, Prior),
	clause_of(Base, 1, Decl, _, Src, Ln0, Ln1),
	  functor(Goal, Name, 3),
	  arg(1, Goal, Module),
	  arg(2, Goal, Pred),
	  arg(3, Goal, Prior),
	  do_add_trans(Goal, Decl, Src, Ln0, Ln1),
	  fail.
activate_translation(_, _, _).

do_add_trans(Goal, Decl, Src, Ln0, Ln1) :-
	( call_trans(Goal) -> true
	; message(['{In ',Src]),
	  message_lns(warning, Ln0, Ln1,
	              [Decl,' - declaration failed']),
	  message('}')
	).

% (Avoid meta-expansions)
call_trans(add_clause_trans(M, Tr, Prior)) :- add_clause_trans(M, Tr, Prior).
call_trans(add_goal_trans(M, Tr, Prior)) :- add_goal_trans(M, Tr, Prior).

:- data mexpand_error/0.
set_mexpand_error :-
	( current_fact(mexpand_error) -> true
	; assertz_fact(mexpand_error)
	).

:- data runtime_module_exp/0.

uses_runtime_module_expansion :- % Called from engine(mexpand)
	( current_fact(runtime_module_exp) -> true
	; asserta_fact(runtime_module_exp)
	).

del_compiler_pass_data :-
	retractall_fact(mexpand_error),
	retractall_fact(runtime_module_exp),
	retractall_fact(location(_,_,_)),
	retractall_fact(compiling_src(_)),
	retractall_fact(last_error_in_src(_)).

compile_multifile_decls(Base, Module, Mode) :-
	def_multifile(Base, F, A, D),
	  module_concat(multifile, F, MF),
	  functor(MP, MF, A),
	  compile_decl(Mode, multifile, MP, MF, A, Module),
	  low_dyn_decl(D, D0),
	    compile_decl(Mode, D0, MP, MF, A, Module),
	fail.
compile_multifile_decls(_, _, _).

compile_dyn_decls(Base, Module, Mode) :-
	dyn_decl(Base, F, A, D),
	  low_dyn_decl(D, D0),
	  module_concat(Module, F, MF),
	  functor(MP, MF, A),
	  compile_decl(Mode, D0, MP, MF, A, Module),
	fail.
compile_dyn_decls(_, _, _).

low_dyn_decl(data,       dynamic).
low_dyn_decl(dynamic,    dynamic).
low_dyn_decl(concurrent, concurrent).

compile_clauses(Base, Module, Mode) :-
	expand_clause(0, 0, Module, _, _, _), % Translator initialization
	current_fact(clause_of(Base,H,B,Dict,Src,Ln0,Ln1), CRef),
	  erase(CRef),
	  Loc = loc(Src,Ln0,Ln1),
	  asserta_fact(location(Src,Ln0,Ln1), Ref),
	  ( number(H) ->
	    true
	  ; module_expansion(H, B, Module, Dict, Mode, _, _, H2, B2),
	    record_pred_dependency(CRef, Module, H2, B2, Loc),
	    compile_clause(Mode, H2, B2, Module)
	  ),
	  erase(Ref),
	fail.
compile_clauses(_, _, _).

module_expansion(H, B, Module, Dict, Mode, H0, B0, H2, B2) :-
	expand_clause(H, B, Module, Dict, H0, B0),
	expand_head_body(H0, B0, Module, Dict, Mode, H2, B2).

expand_head_body(H0, B0, Module, Dict, Mode, H2, B2) :-
	( Mode = interpreted,
	  current_fact(interpret_srcdbg(Module)) ->
	  srcdbg_expand(H0,B0,H1,B1,Dict,Dict1),
	  Dbg = yes
	; H1 = H0, B1 = B0, Dbg = no
	),
	head_expansion(H1, Module, H2),
	body_expansion(B1, Module, -, compile, B2),
	(
	    Dbg = yes ->
	    complete_dict(H2-B2, Dict, [Dict1], Dict2),
	    % Variables generated by the compiler placed in Dict1
	    reverse(Dict2, Dict1)
	;
	    Dict1 = Dict
	).

compile_goal_decl(DN, Base, Module, Mode) :-
	functor(Decl, DN, 1),
	findall(loc(Decl,Src,Ln0,Ln1),
	        clause_of(Base, 1, Decl, _Dict, Src, Ln0, Ln1),
	        Decls),
	compile_goal_decls(Decls, DN, Module, Mode).

compile_goal_decls([], _, _, _) :- !.
compile_goal_decls(Decls, DN, Module, Mode) :-
	functor(DeclM, DN, 1),
	arg(1, DeclM, Module),
	compile_goal_decls_(Decls, DeclM, Module, Mode).

compile_goal_decls_([], _, _, _).
compile_goal_decls_([loc(Decl,Src,Ln0,Ln1)|_], DeclM, Module, Mode) :-
	asserta_fact(location(Src,Ln0,Ln1), Ref),
	arg(1, Decl, Goal),
	body_expansion(Goal, Module, -, compile, Goal1),
	( DeclM = initialization(XX) ->
	    DeclM2 = 'multifile:$initialization'(XX)
	; DeclM = on_abort(XX) ->
	    DeclM2 = 'multifile:$on_abort'(XX)
	; fail
	),
	( current_prolog_flag(unused_pred_warnings, yes) ->
	  record_pred_dependency(Module, DeclM2, Goal1, loc(Src,Ln0,Ln1))
	; true
	),
	compile_clause(Mode, DeclM2, Goal1, Module),
	erase(Ref),
	fail.
compile_goal_decls_([_|Decls], DeclM, Module, Mode) :-
	compile_goal_decls_(Decls, DeclM, Module, Mode).

compile_ldlibs(Base, Module, Mode) :-
	findall('internals:load_lib'(M, F), module_lib(Base, F, M), LLibs),
	list_to_conjunction(LLibs, LoadLibs),
	compile_clause(Mode, 'multifile:$ldlibs'(Module), LoadLibs, Module).

module_lib(Base, Lib, LibMod) :-
	Lib = library(_),
	uses_or_adds(Base, Lib),
	base_name(Lib, LibBase),
	defines_module(LibBase, LibMod).

uses_or_adds(Base, File) :- uses(Base, File).
uses_or_adds(Base, File) :- adds(Base, File).

list_to_conjunction([X],X):- !.
list_to_conjunction([X|More],'basiccontrol:,'(X,Next)):-
	list_to_conjunction(More,Next).
list_to_conjunction([],'basiccontrol:true').

compile_dependences(Base, Module, Mode) :-
	uses_or_adds(Base, File2),
	  base_name(File2, Base2), defines_module(Base2, Module2),
	  compile_clause(Mode, 'multifile:$u'(Module, Module2), 'basiccontrol:true', Module),
	fail
      ; true.

include_module_data(Mode, Module) :-
	compile_clause(Mode, 'multifile:$current_module'(Module), 'basiccontrol:true', Module),
	include_meta_args(Mode, Module),
	retract_fact(runtime_module_exp), !,
	include_runtime_data(Mode, Module).
include_module_data(_, _).

include_meta_args(Mode, M) :-
	meta_args(M, P),
	  compile_clause(Mode, 'multifile:$meta_args'(M, P), 'basiccontrol:true', M),
	fail.
include_meta_args(_, _).

include_runtime_data(Mode, M) :-
	imports(M, IM, F, N, EM),
	  compile_clause(Mode, 'multifile:$imports'(M, IM, F, N, EM), 'basiccontrol:true', M),
	fail.
include_runtime_data(Mode, M) :-
	multifile(M, F, N),
	  compile_clause(Mode, 'multifile:$multifile'(M, F, N), 'basiccontrol:true', M),
	fail.
include_runtime_data(Mode, M) :-
	defines(M, F, N),
	  compile_clause(Mode, 'multifile:$defines'(M, F, N), 'basiccontrol:true', M),
	fail.
include_runtime_data(_, _).

/*
include_decl(Mode, Decl, F, A) :-
	((Mode = ql(Pr) ; Mode = incoreql(Pr)) ->
	    functor(P, F, A),
	    proc_declaration_in_mode(ql(Pr), Decl, P, F, A)
	; Mode = wam ->
	    functor(P, F, A),
	    proc_declaration_in_mode(wam, Decl, P, F, A)
	; true
	).
*/

:- data incore_mode_of/2. % Predicate HEAD was compiled with MODE, one of
	                  % {interpreted, incore(unprofiled), incore(profiled)}
	                  % or multifile(Mode) been Mode one of that

:- data pred_module/2. % Predicate HEAD was loaded from MODULE
	               % (does not include multifile predicates)

compile_decl(ql(_), Decl, P, F, A,_Module) :- !,
	proc_declaration(Decl, P, F, A). % in pl2wam
compile_decl(wam, Decl, P, F, A,_Module) :- !,
	proc_declaration(Decl, P, F, A).
compile_decl(incoreql(Pr), Decl, P, F, A, Module) :- !,
	proc_declaration_in_mode(ql(Pr), Decl, P, F, A),
	incore_decl_pred(Decl, P, F, A, incore(Pr), Module).
compile_decl(Mode, Decl, P, F, A, Module) :-
	incore_decl_pred(Decl, P, F, A, Mode, Module).

incore_decl_pred(Decl, P, F, A,_Mode,_Module) :-
	incore_mode_of(P, _), !,
	error_in_lns(_,_,error, [Decl,' declaration of ',~~(F/A),' too late']).
incore_decl_pred(multifile, P, F, A, Mode, Module) :- !,
	( '$predicate_property'(P, _, Bits) ->
	    check_multifile_type(Module, F, A, Bits)
	; incore_internal_mode(Mode, IM),
	  '$define_predicate'(F/A, IM),
	  '$set_property'(P, multifile)
	).
incore_decl_pred(Dynamic, P,_F,_A,_Mode,_Module) :- % dynamic or concurrent
	incore_mode(interpreted, P, Mode),
	( Mode = interpreted ->
	      '$set_property'(P, Dynamic)
	; Mode = multifile(interpreted) ->
	      true
	; '$set_property'(P, Dynamic) ->
	      retract_fact(incore_mode_of(P, _)),
	      asserta_fact(incore_mode_of(P, multifile(interpreted)))
	; true % errors are given by preceding clause
	).

check_multifile_type(Module, F, A, Bits) :-
	multifile_pred(F, F_),
	multifile(Module, F_, A, Dyn),
	low_dyn_decl(Dyn, DynMode),
	\+ dynmode_has_bit(DynMode, Bits), !,
	error_in_lns(_,_,error, ['Multifile predicate ',~~(F_/A),' defined ',
	             DynMode,' in ',Module,' but currently is not']).
check_multifile_type(_, _, _, _).

dynmode_has_bit(dynamic, Bits) :- Bits/\2 =:= 2.
dynmode_has_bit(concurrent, Bits) :- Bits/\1 =:= 1.

incore_internal_mode(interpreted, interpreted).
incore_internal_mode(incore(Profiling), Profiling).

compile_clause(ql(_), Head, Body, _Module) :- !,
	compile_clause(Head, Body). % in pl2wam
compile_clause(wam, Head, Body, _Module) :- !,
	compile_clause(Head, Body). % in pl2wam
compile_clause(incoreql(Pr), Head, Body, Module) :- !,
	incore_mode(incore(Pr), Head, Mode),
	( Mode = incore(Pr) ->
	      compile_clause(Head, Body) % in pl2wam
	; compile_clause_in_mode(ql(Pr), Head, Body),
	  % multifile or interpreted
	  compile_clause_incore(Mode, Head, Body, Module)
	).
compile_clause(Mode, Head, Body, Module) :- % incore(Pr) or interpreted
	incore_mode(Mode, Head, NewMode),
	compile_clause_incore(NewMode, Head, Body, Module).
	      
compile_clause_incore(multifile(Mode), Head, Body, Module) :- !,
	add_multifile_clause(Head, Body, Module, Mode).
compile_clause_incore(interpreted, Head, Body, _) :- !,
	assert_clause(Head, Body).
compile_clause_incore(Mode, Head, Body, _) :-
	compile_clause_in_mode(Mode, Head, Body).

add_multifile_clause(H, B, M, Mode) :-
	get_expanded_multifile(H, M, NH, Mode),
	functor(NH, _, A),
	arg(A, NH, M),
	assert_clause(NH, B).

:- data expanded_multifile/2. % Multifile predicate HEAD is expanded with
	                      % NEWPRED
	
get_expanded_multifile(H, _, P, _) :- expanded_multifile(H, P), !.
get_expanded_multifile(H, M, P, Mode) :-
	functor(H, F, A),
	( atom(M) -> atom_concat([F, '$', M, '$ex$'], Base)
	; atom_concat(F, '$user$ex$', Base)
	),
	new_mp_name(Base, N),
	A1 is A+1,
	functor(H0, F, A),
	functor(P, N, A1),
	copy_args(A, H0, P),  % Last arg of P is module name
	assertz_fact(expanded_multifile(H0, P)),
	compile_clause_incore(Mode, H0, P, _),
	H = H0, % copy args from head
	'$define_predicate'(N/A1, interpreted),
	'$set_property'(P, dynamic).

:- data mp_index/2.

new_mp_name(Base, P) :-
	( retract_fact(mp_index(Base, I)) -> true ; I = 0),
	I1 is I+1,
	asserta_fact(mp_index(Base, I1)),
	number_codes(I, N),
	atom_codes(Base, S),
	append(S, N, PS),
	atom_codes(P, PS).

% Determines the compilation mode of a predicate
incore_mode(_, Head, Mode) :- incore_mode_of(Head, Mode), !.
incore_mode(CurrMode, Head, Mode) :-
	functor(Head, F, A),
	functor(Head0, F, A),
	( '$predicate_property'(Head0, Enter, Bits) ->
	  % Existing predicate, should be multifile or from user
	  ( Bits/\8 =:= 8 -> % multifile
	        mode_from_enter(Enter, MMode),
	        Mode = multifile(MMode)
	  ; ( retract_fact(pred_module(Head0, Module)) ->
	          Place = Module
	    ; Place = 'this executable'
	    ),
	    error_in_lns(_,_,warning, ['predicate ',~~(F/A),' from ',Place,
	                 ' is being redefined']),
	    '$abolish'(Head0),
	    incore_internal_mode(CurrMode, IM),
	    '$define_predicate'(F/A, IM),
	    Mode = CurrMode
	  )
	; incore_internal_mode(CurrMode, IM),
	  '$define_predicate'(F/A, IM),
	  Mode = CurrMode
	),
	asserta_fact(incore_mode_of(Head0, Mode)).

mode_from_enter(0, incore(unprofiled)). % COMPACTCODE            
mode_from_enter(1, incore(unprofiled)). % COMPACTCODE_INDEXED    
mode_from_enter(2, incore(profiled)).   % PROFILEDCODE           
mode_from_enter(3, incore(profiled)).   % PROFILEDCODE_INDEXED   
mode_from_enter(8, interpreted).        % INTERPRETED            

%%% --- Module expansion related --- %%%

% This predicate is called from mexpand
module_warning(Error) :-
	( put_doing,
	  current_fact(location(Src,L0,L1)) ->
	    Location = location(Src,L0,L1)
	; Location = none
	),
	( Location = none, ciaopp_expansion ->
	    true % ignore
	; module_warning_mess(Error, Type, MessL),
	  ( Type = error -> set_mexpand_error ; true ),
	  ( Location = location(Src,L0,L1) ->
	      put_src_if_needed(Src),
	      message_lns(Type, L0, L1, MessL)
	  ; message(Type, MessL)
	  )
	).

module_warning_mess(not_defined(F, N,_M), error,
	['Predicate ',~~(F/N),' undefined in source']).
module_warning_mess(not_imported(F, N,_M, QM), error,
	['Bad module qualification of ',~~(F/N),
	 ', predicate not imported from module ',QM]).
module_warning_mess(imported_needs_qual(F, N, M), warning,
	['Unqualified predicate call to ',~~(F/N),
	 ' assumed to local version, calls to predicate imported from ',M,
	 ' must be qualified']).
module_warning_mess(imported_needs_qual(F, N, M0, M), warning,
	['Unqualified predicate call to ',~~(F/N),
	 ' assumed to module ',M,', calls to predicate imported from ',M0,
	 ' must be qualified']).
module_warning_mess(bad_pred_abs(PA), error,
	['Bad predicate abstraction ',~~(PA),
	 ' : head functor should be ''''']).
module_warning_mess(big_pred_abs(PA,N), error,
	['Predicate abstraction ',~~(PA),
	 ' has too many arguments: should be ',N]).
module_warning_mess(short_pred_abs(PA,N), error,
	['Predicate abstraction ',~~(PA),
	 ' has too few arguments: should be ',N]).

:- data compiling_src/1, last_error_in_src/1.

put_src_if_needed(Src) :-
	current_fact(last_error_in_src(Src0), Ref), !,
	( Src = Src0 -> true
	; erase(Ref),
	  message('}'),
	  put_src_if_needed(Src)
	).
put_src_if_needed(Src) :-
	current_fact(compiling_src(Src)), !.
put_src_if_needed(Src) :-
	message(['{In ',Src]),
	asserta_fact(last_error_in_src(Src)).

end_brace_if_needed :-
	( retract_fact(last_error_in_src(_)) ->
	    message('}')
	; true
	).

% ---------------------------------------------------------------------------
% Enable or disable custom module expansion required by ciaopp
:- data ciaopp_expansion_enabled/0.

set_ciaopp_expansion(true) :-
	current_fact(ciaopp_expansion_enabled), !.
set_ciaopp_expansion(true) :-
	asserta_fact(ciaopp_expansion_enabled).
set_ciaopp_expansion(false) :-
	retractall_fact(ciaopp_expansion_enabled).

ciaopp_expansion :- current_fact(ciaopp_expansion_enabled).

:- include(engine(mexpand)).
% ---------------------------------------------------------------------------

mexpand_meta_args(M, P, Primitive) :-
	meta_args(M, P),
	% JF: WRONG!!! Should be a data fact, like meta_args...
	( '$primitive_meta_predicate'(P, M) ->
	    Primitive = true
	; Primitive = fail
	).
mexpand_imports(M, IM, F, N, EM) :-
	imports(M, IM, F, N, EM).
mexpand_defines(M, F, N) :-
	defines(M, F, N).
mexpand_defines(M, F, N) :-
	functor(Meta, F, N),
	meta_args(M, Meta),
	meta_inc_args(Meta, N, N1),
	defines(M, F, N1).
mexpand_multifile(M, F, N) :-
	multifile(M, F, N).

multifile(M, F, N) :- multifile(M, F, N, _DynMode).

%% EXPANSION: heads

head_expansion('$:'(H), _, H) :- !. % For use by code translators
head_expansion(H, M, NH) :-
	functor(H, F, N),
	head_expansion0(H, F, N, M, H1),
	meta_expansion_keep_arity(F, N, H1, M, NH).

meta_expansion_keep_arity(F, N, H, M, NH) :-
	possibly_meta_expansion_head(F, N, H, M, M, NH, no, no),
	functor(NH, _, N), % addmodule meta expansion not applied 
	!.
meta_expansion_keep_arity(_, _, H, _, H).

possibly_meta_expansion_head(F, N, A1, M, RM, NA, G, G_) :-
	functor(Meta, F, N),
	% JF: it does not take into account expand_inside/2
	mexpand_meta_args(RM, Meta, Primitive), !,
	functor(A1, F_, N_),
	assertz_fact(head_expansion, Ref),
	meta_expansion_args(1, N_, A1, M, compile, Meta, Primitive,
	                    NAL, G, G_),
	erase(Ref),
	NA =.. [F_|NAL].
possibly_meta_expansion_head(_F,_N, A1,_M,_RM, A1, G, G). % No meta expansion

head_expansion0(H, F, N, M, NH):-
	multifile(M, F, N), !,
	module_concat(multifile, H, NH).
head_expansion0(H, _, _, M, NH):-
	module_concat(M, H, NH).

%%% --- Dynamic incore loading --- %%%

:- set_prolog_flag(multi_arity_warnings, off).

:- meta_predicate
	multifile(addmodule).

multifile(F/A, Mod) :-
	functor(_P, F, A), % Check if valid
	compile_clause(interpreted, 'multifile:$multifile'(Mod, F, A), 'basiccontrol:true', Mod).

:- set_prolog_flag(multi_arity_warnings, on).

:- data module_loaded/4. % MODULE was loaded from SOURCE at TIME with MODE
	                 % one of {interpreted, unprofiled, profiled}

static_module(M) :-
	current_module(M),
	\+ module_loaded(M, _, _, _).

static_base(Base) :-
	defines_module(Base, Module),
	static_module(Module).


use_mod(File, Imports, ByThisModule) :-
	use_mod_common(File, module, Imports, ByThisModule).

% JF[]
use_mod_common(File, Type, Imports, ByThisModule) :-
	Fake_Base = executable(ByThisModule, File),
	store_imports(Imports, File, Fake_Base, 0, 0),
	new_in_mode(In),
	process_files_from_(File, In, Type,
	                    load_compile, static_base, false, needs_reload),
	del_in_mode(In),
	gen_imports(Fake_Base),
	retractall_fact(imports_all(Fake_Base, _)),
	( current_fact(module_error) ->
	    message(['{Compilation aborted}']),
	    signal_compilation_error,    
	    retractall_fact(module_error),
	    retractall_fact(imports_pred(Fake_Base, _, _, _, _, _, _)),
	    discard_delayed_dynlinks
	; base_name(File, Base),
	  defines_module(Base, Module),
	  include_dyn_imports(ByThisModule, Module, Fake_Base),
	  ( make_delayed_dynlinks -> true % JFMC
	  ; message(['{Dynamic link failed}'])
	  ),
	  do_initialization(Module)
	).

% JF[]
use_mod_user(File, ByThisModule) :-
	use_mod_common(File, any, all, ByThisModule).

%% Old Code JF[]
% use_mod_user(File) :-
%         process_files_from_(File, in, any, 
%                           load_compile, static_base, false, needs_reload),
%       ( make_delayed_dynlinks -> true % JFMC
%       ; message(['{Dynamic link failed}']),
%         fail
%       ), !,
%         base_name(File, Base),
%         defines_module(Base, Module),
%         do_initialization(Module).
% use_mod_user(_) :- !, % JFMC
%       discard_delayed_dynlinks,
%       fail.

:- data needs_ini/1.

do_initialization(Module) :-
	findall(M, retract_fact(needs_ini(M)), L),
	comps_needs_ini(L),
	initialize_module(Module).

comps_needs_ini([]).
comps_needs_ini([M|Ms]) :-
	'$u'(N,M),
	retract_fact(initialized(N)), !,
	comps_needs_ini([N,M|Ms]).
comps_needs_ini([_|Ms]) :-
	comps_needs_ini(Ms).

:- data dyn_imports/2. % MODULE was imported dynamically from OTHERMODULE

include_dyn_imports(M, IM, B) :-
	retract_fact(imports_pred(B, _, F, N, _, _, E)),
	( E = '.' -> EM = IM ; file_defines_module(E, EM) ),
%          assert_clause(imports(M, IM, F, N, EM), 'basiccontrol:true'),
	  assert_clause('multifile:$imports'(M, IM, F, N, EM), 'basiccontrol:true'),
	fail.
include_dyn_imports(M, IM, _) :-
	asserta_fact(dyn_imports(M, IM)).

:- data load_action/4.

load_compile(Base) :- % JFMC
	load_so(Base),
	fail.
load_compile(Base) :-
	retract_fact(load_action(Base, Module, Mode, Load_Action)), !,
	do_load_action(Base, Module, Mode, Load_Action).
load_compile(Base) :-
	compute_load_action(Base, Module, Mode, Load_Action), !,
	do_load_action(Base, Module, Mode, Load_Action).
load_compile(_).

needs_reload(Base) :- % JFMC
	compute_load_action(Base, Module, Mode, Load_Action),
	asserta_fact(load_action(Base, Module, Mode, Load_Action)), !.
needs_reload(Base) :-
	load_so(Base),
	fail.

% Success if we need to read source and recompile, loads object and fails
% if we don't need to recreate object, else fails
compute_load_action(Base, Module, Mode, Load_Action) :-
	defines_module(Base, Module),
	itf_filename(Base, ItfName),
	modif_time0(ItfName, ItfTime),
	file_data(Base, PlName, Dir),
	compilation_mode(PlName, Module, Mode),
	( Mode = interpreted(_) ->
	    ( modif_time0(PlName, PlTime),
	      ( PlTime >= ItfTime -> FTime = PlTime ; FTime = ItfTime),
	      not_changed(Module, Base, Mode, FTime) -> fail
	    ; Load_Action =
	          load_interpreted(PlName, Base, Module)
	    )
	; po_filename(Base, PoName),
	  modif_time0(PoName, PoTime),
	  ( PoTime < ItfTime ->
	      \+ not_changed(Module, Base, fail, _), % to give message
	      Load_Action =
	          load_make_po(Base, PlName, Dir, PoName, Mode, Module)
	  ; not_changed(Module, Base, Mode, PoTime) -> fail
	  ; abolish_module(Module),
	    generate_multifile_data(Base, Module),
	    qload_dyn(PoName, Module),
	    retractall_fact(multifile(_,_,_,_)),
	    asserta_fact(needs_ini(Module)),
	    module_loaded_now(Module, Base, Mode),
	    fail
	  )
	).

generate_multifile_data(Base, M) :-
	def_multifile(Base, F, A, Def),
	  assertz_fact(multifile(M, F, A, Def)),
	fail.
generate_multifile_data(_,_).

% JFMC - Delayed dynamic link.

:- data delayed_dynlink/3.
:- data dynlink_error/0.

/****************************************************************** 
 BUG: (???) THIS CODE DOESN'T WORK, the retract_fact makes weird things...

make_delayed_dynlinks :-
	retract_fact(delayed_dynlink(Base, Module, Decls)),
	  so_filename(Base, SoName),
	  ( build_foreign_interface_explicit_decls(Base, Decls),  
	    file_exists(SoName),
	    check_dynlink(SoName, Module) ->
	    true
	  ; abolish_module(Module),
	    set_fact(dynlink_error)
	  ),
	fail.
make_delayed_dynlinks :-
	\+ retract_fact(dynlink_error).
****************************************************/

make_delayed_dynlinks :-
	retractall_fact(dynlink_error),
	findall(delayed_dynlink(Base, Module, Decls), delayed_dynlink(Base, Module, Decls), Xs),
	retractall_fact(delayed_dynlink(_, _, _)),
	make_delayed_dynlinks_2(Xs),
	\+ retract_fact(dynlink_error).

make_delayed_dynlinks_2([delayed_dynlink(Base, Module, Decls)|Xs]) :- !,
	make_delayed_dynlink(Base, Module, Decls),
	make_delayed_dynlinks_2(Xs).
make_delayed_dynlinks_2([]) :- !.

make_delayed_dynlink(Base, Module, Decls) :-
	so_filename(Base, SoName),
	( build_foreign_interface_explicit_decls(Base, Decls) -> true ; fail ),
	file_exists(SoName), 
	check_dynlink(SoName, Module),
	!.
make_delayed_dynlink(_, Module, _) :-
	abolish_module(Module),
	set_fact(dynlink_error).

discard_delayed_dynlinks :-
	retractall_fact(delayed_dynlink(_, _, _)).

:- data foreign_library/2.

check_dynlink(SoName, Module) :-
	debug(['Calling ',check_dynlink(SoName, Module)]),
	current_fact(foreign_library(Module, LastSoTime)), !,
	modif_time(SoName, SoTime),
	( SoTime > LastSoTime ->
	    retract_fact(foreign_library(Module, LastSoTime)),
	    dynlink(SoName, Module),
	    assertz_fact(foreign_library(Module, SoTime))
	; true
	).
check_dynlink(SoName, Module) :-
	debug(['First time',''(check_dynlink(SoName, Module))]),
	modif_time(SoName, SoTime),
	dynlink(SoName, Module),
	assertz_fact(foreign_library(Module, SoTime)),
	debug(['Asserted ',foreign_library(Module, SoTime)]),
	debug(['Ended check_dynlink']).

check_dynunlink(Module) :-
	retract_fact(foreign_library(Module, _)),
	dynunlink(Module).

% Make foreign interface gluecode (if necessary)
:- use_module(exemaker, [needs_interface/2]).
make_gluecode(Base) :- % JFMC
	findall(X, decl(Base, X), Decls),
	( do_interface(Decls) ->
	    assertz_fact(needs_interface(Base, Decls))
	; true
	).

load_so(Base) :- % JFMC
	\+ current_fact(delayed_dynlink(Base, _, _)),
	findall(X, decl(Base, X), Decls),
	( do_interface(Decls) ->
	    defines_module(Base, Module),
	    asserta_fact(delayed_dynlink(Base, Module, Decls))
	; so_filename(Base, SoName),
	  file_exists(SoName),
	  defines_module(Base, Module),
%?          abolish_module(Module),
%?          assertz_fact(current_module(Module)),
	  check_dynlink(SoName, Module)
	).

:- meta_predicate do_load_action(?, ?, ?, goal).

do_load_action(Base, Module, Mode, Goal) :-
	abolish_module(Module),
	call(Goal),
	asserta_fact(needs_ini(Module)),
	module_loaded_now(Module, Base, Mode).

not_changed(Module, Base, Mode, Time) :-
	module_loaded(Module, OldBase, LoadTime, OldMode),
	( OldBase = Base -> true
	; message(note,
	          ['redefining module ',Module,' from ',OldBase,' to ',Base]),
	  fail
	),
	OldMode = Mode,
	LoadTime >= Time.

module_loaded_now(Module, Base, Mode) :-
	retractall_fact(module_loaded(Module, _, _, _)),
	time(Now),
	assertz_fact(module_loaded(Module, Base, Now, Mode)).

:- trust pred do_on_abolish(G) : callable(G).
:- multifile do_on_abolish/1.

do_on_abolish(Head) :- retract_fact(pred_module(Head, _M)).

:- data renamed_multifile/4. % Predicate HEAD was renamed to F/A in MODULE

abolish_module(Module) :-
	retract_fact(initialized(Module)),
	fail.
abolish_module(Module) :-
	retract_fact(dyn_imports(M, Module)),
	retract_clause('multifile:$imports'(M, Module, _, _, _), 'basiccontrol:true'),
	fail.
abolish_module(Module) :-
	expanded_multifile(_, P),
	functor(P, _, A),
	arg(A, P, Module),
	retract_clause(P, _),
	fail.
abolish_module(Module) :-
	retract_fact(renamed_multifile(_, F, A, Module)),
	functor(Pred, F, A),
	'$abolish'(Pred),
	fail.
abolish_module(Module) :-
	retract_fact(pred_module(Pred, Module)),
	'$abolish'(Pred),
	fail.
abolish_module(Module) :-
	check_dynunlink(Module), % JFMC
	fail.
abolish_module(_).

unload_mod(File) :-
	find_pl_filename(File, _, Base, _),
	retract_fact(module_loaded(Module, Base, _, _)),
	abolish_module(Module),
	delete_itf_data(Base).

:- data interpret_file/1.   % SOURCE will be interpreted when loaded
:- data interpret_module/1. % MODULE will be interpreted when loaded
:- data interpret_srcdbg/1. % SRCDBG will be expanded when loaded

compilation_mode(_, Module, interpreted(srcdbg)) :- 
	current_fact(interpret_srcdbg(Module)), !.
compilation_mode(Source, Module, interpreted(raw)) :-
	(interpret_file(Source) ; interpret_module(Module)), !.
compilation_mode(_, _, Profiling) :-
	current_prolog_flag(compiling, Profiling).

:- push_prolog_flag(unused_pred_warnings, no).
load_interpreted(Source, Base, Module) :-
	now_doing(['Consulting ',Source]),
	compiler_pass(Source, Base, Module, interpreted, _Ok),
	end_doing,
	% todo: unload module if Ok is not 'yes'
	compute_pred_module(Module).

load_make_po(Base, Source, Dir, PoName, Profiling, Module) :-
	now_doing(['Compiling ',Source]),
	( stream_of_file(PoName, Dir, Out, Ref) ->
	    Mode = incoreql(Profiling),
	    reset_counter(Module),
	    set_compiler_mode_out(Mode, Out),
	    compiler_pass(Source, Base, Module, Mode, Ok),
	    close(Out),
	    fmode(Source, FMode),
	    chmod(PoName, FMode),
	    erase(Ref),
	    check_ok(PoName, Ok)
	; Mode = incore(Profiling),
	  reset_counter(Module),
	  set_compiler_mode(Mode),
	  % todo: unload module if Ok is not 'yes'
	  compiler_pass(Source, Base, Module, Mode, _Ok)
	),
	end_doing,
	compute_pred_module(Module).
:- pop_prolog_flag(unused_pred_warnings).

compute_pred_module(M) :-
	retract_fact(incore_mode_of(Head, Mode)),
	  \+ Mode = multifile(_),
	    asserta_fact(pred_module(Head, M)),
	fail.
compute_pred_module(_).

%%% --- Po loading --- %%%

qload_dyn(File, Module) :-
	now_doing(['Loading ',File]),
	'$push_qlinfo',
	'$open'(File, r, Stream),            % Gives errors
	( qload_dyn_s(Stream, Module) -> true
	; error_in_lns(_,_,warning, [File,' - wrong .po version number'])
	),
	'$pop_qlinfo',
	cleanup_compilation_data,
	close(Stream),
	end_doing,
	compute_pred_module(Module).

qload_dyn_s(Stream, Module) :-
	'$qread'(Stream, Version),
	poversion(Version),
	repeat,
	  '$qread'(Stream, Goal),
	( Goal= -1
	; ql_step(Goal, Module), fail
	), !.

ql_step('internals:$define_predicate'(N/A, Profiling), Module) :-
	functor(Head, N, A), !,
	incore_mode(incore(Profiling), Head, Mode),
	handle_multifile_ql(Mode, Head, N, A, Profiling, Module).
ql_step('internals:$define_predicate'(Pred, Profiling), Module) :-
	ql_basepred(Pred, Base), !,
	( renamed_multifile(Base, N, A, Module) ->
	    subst_basepred(Pred, N/A, NewPred),
	    '$define_predicate'(NewPred, Profiling)
	; '$define_predicate'(Pred, Profiling)
	).
ql_step('internals:$set_property'(Head,Prop), Module) :-
	ql_set_prop(Prop, Head, Module), !.
ql_step('internals:$interpreted_clause'(F/A,(H :- B)), Module) :-
	functor(Pred, F, A), !,
	( renamed_multifile(Pred, N,_A, Module) ->
	    functor(NH, N, A),
	    copy_args(A, H, NH),
	    assert_clause(NH, B)
	; assert_clause(H, B)
	).
ql_step('internals:$compiled_clause'(Pred,Obj,Mode,Data), Module) :-
	ql_basepred(Pred, Base),  !,
	( renamed_multifile(Base, N, A, Module) ->
	    subst_basepred(Pred, N/A, NewPred),
	    '$compiled_clause'(NewPred, Obj, Mode, Data)
	; '$compiled_clause'(Pred,Obj,Mode,Data)
	).
/* if gauge
ql_step(install_clause_model(Pred/I,Counters),_Module) :-
	ql_basepred(Pred, Base), !,
	incore_mode_of(Base, incore(profiled)), 
	install_clause_model(Pred/I, Counters).
ql_step(install_insn_model(Pred/I,Counters),_Module) :-
	ql_basepred(Pred, Base), !,
	incore_mode_of(Base, incore(profiled)), 
	install_insn_model(Pred/I, Counters).
*/
ql_step(Goal, _) :-
	error_in_lns(_,_,warning, ['Invalid po item ',Goal]).

ql_set_prop(multifile, Head, Module) :-
	functor(Head, F, A),
	( '$set_property'(Head, multifile) ->
	    ( multifile_pred(F, F_),
	      multifile(Module, F_, A, Dyn),
	      low_dyn_decl(Dyn, Dynamic),
	      member(Dynamic, [dynamic, concurrent]) ->
	        '$set_property'(Head, Dynamic),
	        IM = interpreted,
	        OldMode = incore(_),
	        NewMode = multifile(interpreted)
	    ; OldMode = incore(IM),
	      NewMode = multifile(OldMode)
	    ),
	    retract_fact(incore_mode_of(Head, OldMode)),
	    asserta_fact(incore_mode_of(Head, NewMode)),
	    handle_multifile_ql(NewMode, Head, F, A, IM, Module)
	; '$predicate_property'(Head, _, Bits) ->
	    check_multifile_type(Module, F, A, Bits)
	; true
	).
ql_set_prop(Prop, Head, Module) :-
	contains1([dynamic, concurrent], Prop),
	( '$set_property'(Head, Prop) -> true
	; renamed_multifile(Head, N, A, Module) ->
	    retract_fact(incore_mode_of(Head, _)),
	    asserta_fact(incore_mode_of(Head, multifile(interpreted))),
	    functor(NHead, N, A),
	    '$set_property'(NHead, Prop)
	).

handle_multifile_ql(incore(_), _, _, _, _, _).
handle_multifile_ql(multifile(Mode), Head, F, A, IM, Module) :-
	( atom(Module) -> atom_concat([F, '$', Module, '$ql$'], Base)
	; atom_concat(F, '$user$ql$', Base)
	),
	new_mp_name(Base, N),
	functor(R, N, A),
	copy_args(A, Head, R),
	add_multifile_clause(Head, R, Module, Mode),
	'$define_predicate'(N/A, IM),
	asserta_fact(renamed_multifile(Head, N, A, Module)).


ql_basepred((F/_-_)/_, Base) :- !,
	ql_basepred(F, Base).
ql_basepred(N/A, Head) :-
	functor(Head, N, A).

subst_basepred((F/N-M)/L, R, (NF/N-M)/L) :- !,
	subst_basepred(F, R, NF).
subst_basepred(_, R, R).

%%% --- Error messages --- %%%

:- data doing_what/1, doing_written/1.

now_doing(M) :-
	current_prolog_flag(verbose_compilation, VF),
	now_doing_(VF, M).

now_doing_(on, M)  :- message(['{'| M]).
now_doing_(off, M) :- asserta_fact(doing_what(M)).

end_doing :-
	current_prolog_flag(verbose_compilation, VF),
	end_doing_(VF).

end_doing_(on)  :- message('}').
end_doing_(off) :-
	retract_fact(doing_what(M)), !,
	( retract_fact(doing_written(M)) ->
	    message('}')
	; true
	).

put_doing :-
	current_prolog_flag(verbose_compilation, VF),
	put_doing_(VF).

put_doing_(on).
put_doing_(off) :-
	current_fact(doing_what(M)), !,
	( doing_written(M) -> true
	; asserta_fact(doing_written(M)),
	  message(['{'| M])
	).
put_doing_(off).

error_in_lns(L0, L1, Type, Msg) :-
	put_doing,
	( var(L0) -> message(Type, Msg)
	; message_lns(Type, L0, L1, Msg)
	).

:- data syntax_error_in/1.

handle_syntax_error(Base,L0,L1,Msg,ErrorLoc) :-
	assertz_fact(syntax_error_in(Base)),
	error_in_lns(L0, L1, error, ['syntax error: ',[](Msg),'\n'| ErrorLoc]),
	fail.

%%% --- Misc Predicates --- %%%

multifile_pred(MF, F) :-
	atom_concat('multifile:', F, MF).

assert_clause(Head, Body) :-
	'$compile_term'([Head|Body], Ptr), 
	'$current_clauses'(Head, Root),
	'$insertz'(Root, Ptr).

retract_clause(Head, Body) :-
	'$current_clauses'(Head, Root), 
	'$current_instance'(Head, Body, Root, Ptr, no_block),
	'$erase'(Ptr),
	'$unlock_predicate'(Root).

:- prop location_t/1 + regtype # "Identifies a source line range in a file.
   @includedef{location_t/1}".

location_t(loc(File, L1, L2)) :- atm(File), int(L1), int(L2).

:- pred location/3 => atm * int * int.

:- data location/3.

:- pred location(Loc) => location_t(Loc) # "Unifies @var{Loc} with the
	locator of the current source line range.  If this information
	can not be obtained, leaves @var{Loc} uninstantiated.".

location(Loc) :- (location(Src, Ln0, Ln1) -> Loc = loc(Src, Ln0, Ln1) ; true).


signal_compilation_error :- 
	send_silent_signal(compilation_error).

