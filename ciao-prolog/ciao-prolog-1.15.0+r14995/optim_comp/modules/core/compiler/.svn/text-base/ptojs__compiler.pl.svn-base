% Most important differences w.r.t. Prolog (not all of them are OK):
%
%   - classes (instantiable modules)
%       - methods (predicates with implicit 'self' argument)
%       - (?) Python-like creation of objects (no 'new' operator)
%             [TODO: sure?]
%       - (?) No static methods (if it is static, it should be outside the class)
%             [TODO: sure?]
%       - (?) No apply syntax for 0-ary functions (e.g. foo() ). How can names 
%         be defined then?
%
%   - basalcode: code at the level of the emulator (like ImProlog)
%       (basal: "forming or belonging to a bottom layer or base")
%       
%   - string data type:
%       The use of atoms as strings complicate programs, analysis and compilation.
%       For that, there is a separate string data type.
%   - atom-based module system:
%       Now that the use of atoms is restricted to data constructors and predicates,
%       it makes sense module-expanding them.
%   - functions:
%       Some term constructors can be customized as predicate calls in functional 
%       notation.

:- use_module(library(lists)).
%:- use_module(library(lists), [append/3, length/2]).

:- use_module(library(strings)).
:- use_module(library(aggregates)).
:- use_module(library(read)).

:- use_module(compiler(errlog)).
:- use_module(compiler(store)).

:- include(compiler(ptojs__errlog)). % (defines error/1)

:- use_module(library(string_type(string_type_rt))).

:- use_module(compiler(memoize)).
:- use_module(compiler(frontend_ptojs)).

:- use_module(compiler(list_common)).
:- use_module(compiler(cdic_)). :- '$trust_statemodel'(cdic, pair).
:- use_module(compiler(array_)). :- '$trust_statemodel'(array, pair).

:- use_module(engine(internals), ['$global_vars_set'/2, '$global_vars_get'/2]).

:- include(compiler(psymbol__interface)). % TODO: move to both ptojs__codegen and ptojs__jsgen (transform as modules before)
:- include(compiler(ptojs__codegen)).
:- include(compiler(ptojs__dump)).
:- include(compiler(ptojs__jsgen)).

compile_mod(Input) :-
	Errs = ~errlog.new,
	Errs.add(verbose(off)),
	memo :: memoize <- ~memoize.new(Errs),
	memo.enter,
	Ok = ( compile_mod_(Input) ? yes | no ),
	memo.leave,
	memo.delete,
	Errs.delete,
	Ok = yes.

{
:- fluid memo :: memoize.
compile_mod_(Name) :-
	Errs = ~memo.errs,
	'$global_vars_get'(3, OldErrs),
	'$global_vars_set'(3, Errs),
	prepare_usermod,
	RootSpec = '.', % TODO: unsure
	store:find_source(Name, relpath(RootSpec), Spec),
	ModuleR = ~eval_split_mod(Spec), % TODO: use 'memo'
	compile_and_link(ModuleR),
        '$global_vars_set'(3, OldErrs).
}.

% Prepare the usermod 
prepare_usermod :-
	RootR = ~module_s.lookup_module(root),
	_ = ~RootR.query_nested_module('user').

:- use_module(compiler(global_pass__ptojs)).

% TODO: look at module dependencies
{
:- fluid memo :: memoize.
:- meta_predicate compile_and_link(module_s).
compile_and_link(ModuleR) :-
	% TODO: for executables, make sure that engine(internals) is compiled and included
	TopModuleRs = ~append(~reachable_module_set([ModuleR]), [~module_s.lookup_module('user')]),
	% Compile all modules into middle level code (dump .dyn.js code)
	compile_top_modules(TopModuleRs),
	% --
	% The header code for ModuleR
	HeaderJS = ~header,
	% Prepare init module for ModuleR
	InitFsId = ~init_mod(TopModuleRs, ModuleR),
	% Concatenate the header code, all .dyn.js, and the init .dyn.js code
	InitModJS = ~fsR(dyn(js, InitFsId)),
	Files = ~append([HeaderJS| ~dyn_js_names(TopModuleRs)], [InitModJS]),
	trust(ModuleR instance_of module_s),
	FsId = ~fs_unique_id(ModuleR),
	concat_single_js(Files, FsId).

header := HeaderJS :-
	store:find_source(engine('ciao_runtime.js'), relpath('.'), HeaderSpec),
	% TODO: This is not a prolog_source, but it works because sources without .pl extensions are accepted
	eval_file(prolog_source(HeaderSpec), HeaderJS). 

init_mod(TopModuleRs, ModuleR) := InitFsId :-
	trust(ModuleR instance_of module_s),
	InitFsId = ~atom_concat(~fs_unique_id(ModuleR), '__init'),
	Mod = ~ModuleR.get_name, % TODO: not the right solution
	prepare_root_module(Mod),
	compile_init_module(TopModuleRs, InitFsId, ModuleR).
}.

% TODO: Like linker__bytecode:create_init/7

% Names of .dyn.js for top modules
dyn_js_names([]) := [].
dyn_js_names([ModuleR|Cs]) := [N|Ns] :-
	trust(ModuleR instance_of module_s),
	N = ~fsR(dyn(js, ~fs_unique_id(ModuleR))),
	Ns = ~dyn_js_names(Cs).

% ---------------------------------------------------------------------------
% Concatenate input files into a single JS file

concat_single_js(Files, FsId) :-
	OutName = ~fsR(final_js(FsId)),
	'$open'(OutName, w, OutStream),
	concat_files(Files, OutStream),
	close(OutStream).

% (like linker__bytecode:dump_prolog_bytecodes/2)
:- static concat_files/2.
concat_files([File|Files], OutputStream):-
	open(File, read, InputStream),
	current_output(OldOutputStream),
	set_output(OutputStream),
        '$copy_stdout'(InputStream),
	set_output(OldOutputStream),
	close(InputStream),
	concat_files(Files, OutputStream).
concat_files([], _).

:- meta_predicate fs_unique_id(module_s, ?).
% The unique identifier, valid as a file name, of the module
% TODO: Not correct
fs_unique_id(Module) := ~Module.get_name.

% ---------------------------------------------------------------------------

compile_top_modules([]).
compile_top_modules([ModuleR|ModuleRs]) :-
	trust(ModuleR instance_of module_s),
        call(( middefs :: accum(Defs), midcomp_module(ModuleR) )),
	FsId = ~fs_unique_id(ModuleR),
	compile_middefs(normal, FsId, Defs),
	%
	compile_top_modules(ModuleRs).

% ---------------------------------------------------------------------------

% Special case for the 'init mod'
compile_init_module(TopModuleRs, InitFsId, MainModuleR) :-
	call((
	    middefs :: accum(InitDefs),
	    % TODO: the root module should never be compiled...
	    RootR = ~module_s.lookup_module(root),
	    midcomp_module(RootR),
	    emit_exec_entry(TopModuleRs, MainModuleR)
	)),
	compile_middefs(init, InitFsId, InitDefs).

% ---------------------------------------------------------------------------

compile_middefs(Kind, FsId, Defs0) :-
	dump_middefs(FsId, Defs0, 'mi1'), % (for debugging)
	call(( middefs :: accum(Defs2), binarize(Defs0) )),
	dump_middefs(FsId, Defs2, 'mi2'), % (for debugging)
	% Translate the middle-level definitions into JS
	middefs_to_js(Kind, FsId, Defs2).

% ===========================================================================

clean_compiler_state :-
	( member(ModuleR, ~all_modules),
	  trust(ModuleR instance_of module_s),
	  ModuleR.clean,
	  fail
	; true
	).

% ---------------------------------------------------------------------------

% TODO: distinguish between 'all loaded modules', a module and all its
%       dependencies, a module and all its recursive dependencies, etc.
all_modules := ModuleRs :-
	findall(ModuleR, module__def(ModuleR), ModuleRs).


