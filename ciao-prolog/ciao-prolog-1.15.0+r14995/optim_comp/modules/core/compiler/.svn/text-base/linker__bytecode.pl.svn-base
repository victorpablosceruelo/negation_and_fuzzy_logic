:- class(linker__bytecode, [], [compiler(complang)]).

:- use_module(library(aggregates), [findall/3]).
:- use_module(library(system), [file_exists/1, fmode/2, chmod/2, delete_file/1]).
:- use_module(library(lists), [append/3]).
:- use_module(library(strings), [write_string/2]).
:- use_module(library(write), [writeq/2, write/2]).

:- use_module(compiler(frontend), [load_absmach/0]).
:- use_module(compiler(module_bin)).
:- use_module(compiler(module_sym)).
:- use_module(compiler(module_itf)).
:- use_module(compiler(module_ideps)).
:- use_module(compiler(module_ipexp)).
:- use_module(compiler(errlog)).
:- use_module(compiler(open_and_protect)).
:- use_module(compiler(memoize)).
:- '$trust_statemodel'(memoize, single).
:- use_module(compiler(store)).
:- use_module(compiler(global_pass)).

:- use_module(engine(rt_exp), ['$user_module_id'/1]).

% ---------------------------------------------------------------------------
% Settings for executable creation

:- pred dynamic_search_path(Path) # "Pattern that specifies which libraries are loaded dynamically".
:- public data dynamic_search_path/1.

% Define the flags used to set linker options
:- static multifile define_flag/3.
define_flag(executables, [static, eagerload, lazyload], eagerload).
define_flag(self_contained,atom,none).

:- static get_flags/1.
get_flags(ExecMode) :-
        current_prolog_flag(executables, ExecMode),
% TODO: reactivate?	
%        current_prolog_flag(self_contained, TargetEng),
	true.

% TODO: lazy load has been disabled

% ---------------------------------------------------------------------------
% Module information of a given Spec (Module, Exports...)
% TODO: think in a cache mechanism...

% TODO: this predicate is only used in emit_init__main... optimize! (For example take the module at the same time than the linker checks if the main module exports main/0 or main/1) ... it is also used in linker__bootstrap, check
{
:- fluid memo :: memoize.
:- public static get_spec_module/2.
get_spec_module(Spec, Module) :-
	IDeps = ~module_ideps.from_spec(Spec),
	IDeps.get1_imported_fast(Spec, Module),
	'$inst_destroy'(IDeps).
}.

:- pred bin_bits(Spec, Bits) # "The QL or SO needed to load the module @var{Spec}".
:- data bin_bits/2.
:- pred bin_c_source(CSourceSpec). 
:- data bin_c_source/1.
:- pred bin_c_header(CHeaderSpec). 
:- data bin_c_header/1.
:- pred bin_native_h(Spec). 
:- data bin_native_h/1.

read_bins(Specs) :-
	Memo = ~gmemo,
	member(Spec, Specs),
	  call((
            memo :: memoize <- Memo,
	    Bin = ~module_bin.from_spec(Spec)
	  )),
	  QL = ( Bin.contains_bytecode ? 1 | 0 ),
	  SO = ( Bin.contains_native ? 2 | 0 ),
	  Bits is QL \/ SO,
	  add(bin_bits(Spec, Bits)),
	  ( Bin.contains_native_h ->
	      add(bin_native_h(Spec))
	  ; true
	  ),
	  ( Bin.native_include_c_source(CSourceSpec),
	    ( bin_c_source(CSourceSpec) -> true ; add(bin_c_source(CSourceSpec)) ),
	    fail
	  ; true
	  ),
	  ( Bin.native_include_c_header(CHeaderSpec),
	    ( bin_c_header(CHeaderSpec) -> true ; add(bin_c_header(CHeaderSpec)) ),
	    fail
	  ; true
	  ),
	  '$inst_destroy'(Bin),
	  fail.
read_bins(_Specs).

% ---------------------------------------------------------------------------

:- attr gmemo :: memoize.

{
:- fluid memo :: memoize.
:- public static link/3.
% TODO: returning NativeSpecs is a bit dirty...
link(Specs, ExecName, NativeInfo) :-
	This = ~new,
	Ok = ( This.link__1(Specs, ExecName, NativeInfo) ? yes | no ),
	'$inst_destroy'(This),
	Ok = yes.
}.

% Constructor
:- constructor new_/0.
new_.

{
:- fluid memo :: memoize.
link__1(Specs, ExecName, NativeInfo) :-
	% -- Get configuration
	~memo = ~gmemo,
	get_flags(ExecMode),
	% Set default dynamic seach paths
	( ExecMode = eagerload ->
	    % TODO: a hack!! configure looking at library paths?
	    add(dynamic_search_path([toplevel|_])),
	    add(dynamic_search_path([compiler|_])),
	    add(dynamic_search_path([engine|_])),
	    add(dynamic_search_path([library|_]))
	; true
	),
	% Link
	( link__2(Specs, ExecName, ExecMode, NativeInfo) ->
	    true
	; gmemo.errs.compiler_error(executable_generation_aborted),
	  fail
	).
}.

link__2(Specs, ExecName, ExecMode, NativeInfo) :-
	memo :: memoize <- ~gmemo,
	
	Specs = [MainSpec|_],

	% Check the executable name
	% TODO: if exec name is not given, the executable gets the same name than the main module
	( var(ExecName) ->
	    errlog:bug(['default executable name is not supported yet']),
	    halt
	; true
	),
	eval_file(prolog_source(MainSpec), PlName),
	( PlName = ExecName ->
	    gmemo.errs.compiler_error(refuse_to_overwrite_source(PlName)),
	    fail
	; true
	),
	
	global_pass:transitive_compile(Specs, ProcessedSpecs),

	% Read bins
	read_bins(ProcessedSpecs),

	% Select dynamic and static modules
	split_dynamic(ProcessedSpecs, ExecMode, EagerSpecs, StaticSpecs),

	% Collect native specs
        % TODO: use only the static ones!!
	findall(NativeSpec, ( bin_bits(NativeSpec, Bits), Bits /\ 2 =:= 2 ), NativeSpecs),
	findall(NativeHSpec, bin_native_h(NativeHSpec), NativeHSpecs),
	findall(CHeaderSpec, bin_c_header(CHeaderSpec), CHeaderSpecs),
	findall(CSourceSpec, bin_c_source(CSourceSpec), CSourceSpecs),

	% Collect native source requirements
	frontend:load_absmach,
	'$absmach'(Absmach),
	Require64 = ( Absmach.use_opt(pointer64) ? yes | no ),

	% Create init
        get_executable_entry_point(MainSpec, MaybeExecutableEntryPoint),
        create_init(MainSpec, MaybeExecutableEntryPoint, StaticSpecs, StaticSpecs2, EagerSpecs, NativeSpecs, NativeSpecs2),

	% Aggregate all native info
	NativeInfo = native(Require64, NativeSpecs2, NativeHSpecs, CSourceSpecs, CHeaderSpecs),

	% Link files
        create_exec(ExecName, StaticSpecs2),
	% Copy read permissions to executable
        fmode(PlName, M0),
        M1 is M0 \/ ((M0 >> 2) /\ 0o111),
        chmod(ExecName, M1).

% Split the Specs list in dynamic or static modules
split_dynamic([], _ExecMode, [], []) :- !.
split_dynamic([Spec|Specs], ExecMode, DynamicSpecs, StaticSpecs) :- !,
	( is_dynamic(ExecMode, Spec) ->
	    DynamicSpecs = [Spec|DynamicSpecs0],
	    StaticSpecs = StaticSpecs0
	; DynamicSpecs = DynamicSpecs0,
	  StaticSpecs = [Spec|StaticSpecs0]
	),
	split_dynamic(Specs, ExecMode, DynamicSpecs0, StaticSpecs0).

is_dynamic(eagerload, Spec) :-
	\+ tempspec__is_init(Spec),
	( dynamic_search_path(Spec0), Spec = Spec0 ), !.

get_executable_entry_point(MainSpec, MaybeExecutableEntryPoint) :-
	get_main_predicate(MainSpec, MaybeExecutableEntryPoint),
	( MaybeExecutableEntryPoint = no ->
	    gmemo.errs.compiler_error(main_not_exported(MainSpec)),
	    fail
	; true
	).

% Only main/0 or main/1 are valid main predicates
get_main_predicate(MainSpec, MaybeExecutableEntryPoint) :-
	memo :: memoize <- ~gmemo,
	Sym = ~module_sym.from_spec(MainSpec),
	MaybeExecutableEntryPoint =
            ( Sym.exports(main, 0, _) ? main0
	    | Sym.exports(main, 1, _) ? main1
	    | Sym.exports('$purest_init', 0, _) ? purest_init % TODO: a temporal kludge
	    | no
	    ),
	'$inst_destroy'(Sym).

% ---------------------------------------------------------------------------
% Temporary specs
% TODO: use a non-user name (not init)
% TODO: use something different for init module... (module(init(Spec)) CANNOT BE EQUAL TO module(Spec))

:- static tempspec__init/2.
tempspec__init(_Spec, ['__tmp__',internal_init]).

:- static tempspec__is_init/1.
tempspec__is_init(['__tmp__',internal_init]).

% ---------------------------------------------------------------------------
% Creation of the init module.
% The init module loads and initializes imported libraries and calls the
% executable entry point.
% note: modules imported in the init module will not be treated
% TODO: why not use a lower level access to the compiler to generate this module?

:- use_module(compiler(write_c), [encode_symbol_a/2]).

create_init(_MainSpec, MaybeExecutableEntryPoint, StaticSpecs, StaticSpecs2, _EagerSpecs, NativeSpecs, NativeSpecs2) :-
	MaybeExecutableEntryPoint = purest_init, !,
	StaticSpecs2 = StaticSpecs,
	NativeSpecs2 = NativeSpecs.
create_init(MainSpec, MaybeExecutableEntryPoint, StaticSpecs, StaticSpecs2, EagerSpecs, NativeSpecs, NativeSpecs2) :-
	memo :: memoize <- ~gmemo,
	call((
          code :: accum(Code),
	  emit_init(MainSpec, MaybeExecutableEntryPoint, StaticSpecs, EagerSpecs, NativeSpecs)
        )),
	tempspec__init(MainSpec, InitSpec),
        save_source(Code, InitSpec),
	InitBin = ~module_bin.from_spec(InitSpec),
	( InitBin.contains_native ->
	    NativeSpecs2 = [InitSpec|NativeSpecs]
	; NativeSpecs2 = NativeSpecs
	),
	StaticSpecs2 = [InitSpec|StaticSpecs],
	'$inst_destroy'(InitBin).

{
:- fluid code :: accum.

emit_init(MainSpec, MaybeExecutableEntryPoint, _StaticSpecs, EagerSpecs, NativeSpecs) :-
	code.add((:- module(_, [], [pure]))),
	code.add((:- use_module(engine(basiccontrol)))),
	code.add((:- use_module(engine(term_basic)))),
        emit_init__main(MainSpec, MaybeExecutableEntryPoint),
	% note: interpreted predicates make smaller executables
	code.add((:- dynamic('$eager_load'/2))), 
	emit_init__eager_load(EagerSpecs),
	emit_init__native(NativeSpecs).

% TODO: there is a problem in the initialization of modules,
%   suppose that more than one spec is specified in the linker object;
%   then '$main_module'/1 is not enough to launch the initialization of 
%   all those modules because there may not be any link to them
%   (i.e. when they only define multifile predicates)

emit_init__main(_MainSpec, no) :- !.
emit_init__main(MainSpec, MaybeExecutableEntryPoint) :-
	memo :: memoize <- ~gmemo,
	get_spec_module(MainSpec, MainModule),
	code.add((:- export('$main_module'/1))),
	code.add('$main_module'(MainModule)),
	code.add((:- export('$main_entry'/1))),
	( MaybeExecutableEntryPoint = main0 ->
	    MainPred = main/0,
	    MainEntry = ('$main_entry'(_) :- main)
	; MaybeExecutableEntryPoint = main1 ->
	    MainPred = main/1,
	    MainEntry = ('$main_entry'(Args) :- main(Args))
	; fail
	),
	store:denormalized_spec(MainSpec, MainUspec),
	( '$user_module_id'(MainModule) -> % TODO: check
	    code.add((:- ensure_loaded(MainUspec))),
	    code.add((:- import(user, [MainPred])))
	; code.add((:- use_module(MainUspec, [MainPred])))
	),
	code.add(MainEntry).

% table of module specs that must be loaded dynamically
emit_init__eager_load([]).
emit_init__eager_load([Spec|Specs]) :-
	( bin_bits(Spec, Bits) -> true ),
	code.add('$eager_load'(Spec, Bits)),
	emit_init__eager_load(Specs).

% TODO: use ptoc__impcomp, use better qualified names, imports?
emit_init__native([]) :- !. % no native specs, do nothing
emit_init__native(NativeSpecs) :-
	Modules = ~spec_modules(NativeSpecs),
	emit_init__native__protos(Modules),
	Code = ~emit_init__native__code(Modules),
	code.add((:- '$pragma'(ip((:- pred init_everything/0 + lowentry(det, [], 'init_everything') + prop(with_worker)))))),
	code.add((:- '$pragma'(ip((init_everything :- Code))))).

% TODO: use ptoc__impcomp, use better qualified names, imports?
emit_init__native__protos([]).
emit_init__native__protos([Module|Modules]) :-
	InitName = ~atom_concat(Module, '__init'),
	CName = q(~encode_symbol_a(Module), init),
	code.add((:- '$pragma'(ip((:- pred InitName/0 + lowentry(det, [], CName) + prop(with_worker) + prop(foreign_imported)))))),
	emit_init__native__protos(Modules).
}.

emit_init__native__code([]) := true.
emit_init__native__code([Module|Modules]) := (R, R0) :-
	InitName = ~atom_concat(Module, '__init'),
	R = InitName,
	R0 = ~emit_init__native__code(Modules).

spec_modules([]) := [].
spec_modules([Spec|Specs]) := [Module|Modules] :-
	memo :: memoize <- ~gmemo,
	get_spec_module(Spec, Module),
	Modules = ~spec_modules(Specs).

% ---------------------------------------------------------------------------
% Linker (only links, without any check or processing)

% precondition: all the specified files exist
create_exec(ExecName, PrologQlSpecs) :-
	get_prolog_bytecode_names(PrologQlSpecs, PrologQlNames),
        ( file_exists(ExecName) -> delete_file(ExecName) ; true ),
	open_and_protect(ExecName, OutputStream, Ref),
	create_exec__header(OutputStream),
	dump_prolog_bytecodes(PrologQlNames, OutputStream),
	close(OutputStream),
        end_protect(Ref).

get_prolog_bytecode_names([Spec|Specs], [Name|Names]) :-
	% TODO: ref the Bin to ensure that the modules have bytecode
	memo :: memoize <- ~gmemo,
	eval_file(compile(Spec), emu, Name),
	get_prolog_bytecode_names(Specs, Names).
get_prolog_bytecode_names([], []).

create_exec__header(OutputStream) :-
	header(Header),
	write_string(OutputStream, Header).

% TODO: recover the built-in path (but make it optional)
:- static header/1.
header := "#!/bin/sh\n"||
	  "exec \"${CIAOLOADER}\" $0 \"$@\"\n"||
          [12]. % mark the end

% Concat a list of files

:- static dump_prolog_bytecodes/2.
dump_prolog_bytecodes([Name|Names], OutputStream):-
	open(Name, read, InputStream),
	current_output(OldOutputStream),
	set_output(OutputStream),
        '$copy_stdout'(InputStream),
	set_output(OldOutputStream),
	close(InputStream),
	dump_prolog_bytecodes(Names, OutputStream).
dump_prolog_bytecodes([], _).

% ------------------------------------------------------------------

save_source(Code, Spec) :-
	store:addr_new(prolog_source(Spec), Name),
        ( file_exists(Name) -> delete_file(Name) ; true ),
	open_and_protect(Name, OutputStream, Ref),
	save_source__dump(Code, OutputStream),
	close(OutputStream),
        end_protect(Ref),
	% note: force update (seconds resolution is not enough)
	gmemo.fake_updated(prolog_source(Spec)).

:- static save_source__dump/2.
save_source__dump([], _Stream).
save_source__dump([X|Xs], Stream) :-
	writeq(Stream, X), write(Stream, '.'), nl(Stream),
	save_source__dump(Xs, Stream).
