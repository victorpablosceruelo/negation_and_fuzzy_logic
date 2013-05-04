:- module(_, [], [compiler(complang)]).

:- use_module(compiler(module_deps)).
:- use_module(compiler(module_itf)).
:- use_module(compiler(module_exp)).
:- use_module(compiler(module_ideps)).
:- use_module(compiler(errs)).
:- use_module(compiler(linker__bytecode)).
:- use_module(compiler(linker__bootstrap)).
:- use_module(compiler(memoize)).
:- use_module(compiler(store)).

:- use_module(compiler(dynload), [use_module/2]). % --exec
:- use_module(library(system), [system/2]).
:- use_module(library(terms), [atom_concat/2]).

% TODO: document: to be safe, dynamic use_module is forbidden in static comp
% TODO: document: this module is loaded to register memoize actions
:- use_module(compiler(all_actions), []).
% TODO: document: this module is loaded to register compilation modules
:- use_module(compiler(basic_compilation_modules), []).

:- export(main/1).
main(Args) :-
        get_opts(Cmd, Args, []),
        ( cmd(Cmd) -> true
	; halt(-1)
	).

get_opts(Cmd) -->
	% verbose compilation
        ['--verbose'], !, { option__set_verbose },
        get_opts(Cmd).
get_opts(Cmd) -->
	% analize all modules
        ['--analyze-all'], !, { option__set_analyze_all },
        get_opts(Cmd).
get_opts(Cmd) -->
	% dead instructions
	% TODO: change name
        ['--dead', Name], !, { get_dead(Name) },
        get_opts(Cmd).
get_opts(do(Action, Spec0)) -->
        ['--do', Action, Spec0], !.
get_opts(dynexec(ExecName, Specs0)) --> 
	['--dynexec', ExecName], !, tail(Specs0).
get_opts(link(ExecName, Specs0)) --> 
	['--link', ExecName], !, tail(Specs0).
get_opts(bootstrap(ExecName, Specs0)) -->
	['--bootstrap', ExecName], !, tail(Specs0).
get_opts(recursive_deps(Specs0)) -->
	['--recursive-deps'], tail(Specs0), !.
get_opts(recursive_archbin_update(Specs0)) -->
	['--recursive-archbin-update'], tail(Specs0), !.

cmd(do(Action, Spec0)) :-
        store:find_source(Spec0, relpath('.'), Spec),
	functor(ActionG, Action, 1),
	arg(1, ActionG, Spec),
	comp:eval0(ActionG).
cmd(link(ExecName, Specs0)) :-
	store:find_sources(Specs0, relpath('.'), Specs),
	link(Specs, ExecName).
cmd(dynexec(ExecName, Specs0)) :-
	store:find_sources(Specs0, relpath('.'), Specs),
	dynexec(Specs, ExecName).
cmd(bootstrap(ExecName, Specs0)) :-
	store:find_sources(Specs0, relpath('.'), Specs),
	bootstrap(Specs, ExecName).
cmd(recursive_deps(Specs0)) :-
	store:find_sources(Specs0, relpath('.'), Specs),
	recursive_deps(Specs).
cmd(recursive_archbin_update(Specs0)) :-
	store:find_sources(Specs0, relpath('.'), Specs),
	recursive_archbin_update(Specs).

:- data option__verbose/1.
option__verbose(off).
option__set_verbose :- set_fact(option__verbose(on)).

:- data option__analyze_all/1.
option__analyze_all(off).
option__set_analyze_all :- set_fact(option__analyze_all(on)).

:- include(compiler(options__interface)).
global__options(Opts) :-
	option__analyze_all(on),
	!,
	Opts = [(:- '$pragma'(analyze_all))].

tail(X, X, []). % X is the rest of tokens

eval0(ActionG) :-
	errs:create(Errs),
        option__verbose(Verbose),
	'$trust_metatype'(Errs, instmod(errs)),
	Errs.add(verbose(Verbose)),
	memoize:create(Errs, Memoize),
	'$trust_metatype'(Memoize, instmod(memoize)),
	Memoize.enter,
	( Memoize.eval0(ActionG) -> Ok = yes ; Ok = no ),
	Memoize.leave,
	Memoize.delete,
	Errs.delete,
	Ok = yes.

link(Specs, ExecName) :-
	errs:create(Errs),
        option__verbose(Verbose),
	'$trust_metatype'(Errs, instmod(errs)),
	Errs.add(verbose(Verbose)),
	memoize:create(Errs, Memoize),
%define_flag(executables, [static, eagerload, lazyload], eagerload).
%define_flag(self_contained,atom,none).
%define_flag(compress_exec,[yes,no],no).
%	set_prolog_flag(compress_exec, yes),
	set_prolog_flag(executables, eagerload),
	'$trust_metatype'(Memoize, instmod(memoize)),
	Memoize.enter,
	( linker__bytecode:link(Memoize, Specs, ExecName, _NativeInfo) ->
	    Ok = yes
	; Ok = no
	),
	Memoize.leave,
	Memoize.delete,
	Errs.delete,
	Ok = yes.

dynexec(Specs, ExecName) :-
	errs:create(Errs),
        option__verbose(Verbose),
	'$trust_metatype'(Errs, instmod(errs)),
	Errs.add(verbose(Verbose)),
	memoize:create(Errs, Memoize),
%define_flag(executables, [static, eagerload, lazyload], eagerload).
%define_flag(self_contained,atom,none).
%define_flag(compress_exec,[yes,no],no).
%	set_prolog_flag(compress_exec, yes),
	set_prolog_flag(executables, eagerload),
	'$trust_metatype'(Memoize, instmod(memoize)),
	Memoize.enter,
	( recursive_archbin_update__2(Memoize, Specs),
	  linker__bytecode:link(Memoize, Specs, ExecName, _NativeInfo) ->
	    Ok = yes
	; Ok = no
	),
	Memoize.leave,
	Memoize.delete,
	Errs.delete,
	Ok = yes.

:- use_module(compiler(dynload), ['$only_static'/0]).
bootstrap(Specs, ExecName) :-
	errs:create(Errs),
        option__verbose(Verbose),
	'$trust_metatype'(Errs, instmod(errs)),
	Errs.add(verbose(Verbose)),
	asserta_fact(dynload:'$only_static'), % TODO: document!!!???
	memoize:create(Errs, Memoize),
	'$trust_metatype'(Memoize, instmod(memoize)),
	Memoize.enter,
	( linker__bootstrap:make_bootstrap(Memoize, Specs, ExecName) -> Ok = yes ; Ok = no ),
	Memoize.leave,
	Memoize.delete,
	Errs.delete,
	Ok = yes.

% ---------------------------------------------------------------------------

:- use_module(compiler(global_pass)).
:- use_module(library(write)).

recursive_deps(Specs) :-
	errs:create(Errs),
        option__verbose(Verbose),
	'$trust_metatype'(Errs, instmod(errs)),
	Errs.add(verbose(Verbose)),
	memoize:create(Errs, Memoize),
	'$trust_metatype'(Memoize, instmod(memoize)),
	Memoize.enter,
	( global_pass:reachable_module_set(Specs, Memoize, ProcessedSpecs),
	  member(P, ProcessedSpecs),
	  store:denormalized_spec(P, P2),
	  writeq(P2), nl,
	  fail
	; true
	),
	Memoize.leave,
	Memoize.delete,
	Errs.delete.

% ---------------------------------------------------------------------------

:- use_module(compiler(global_pass)).

% TODO: why?!?!?!?
% TODO: DOES NOT WORK!
recursive_archbin_update(Specs) :-
	errs:create(Errs),
        option__verbose(Verbose),
	'$trust_metatype'(Errs, instmod(errs)),
	Errs.add(verbose(Verbose)),
	memoize:create(Errs, Memoize),
	'$trust_metatype'(Memoize, instmod(memoize)),
	Memoize.enter,
	recursive_archbin_update__2(Memoize, Specs),
	Memoize.leave,
	Memoize.delete,
	Errs.delete.

recursive_archbin_update__2(Memoize, Specs) :-
	global_pass:transitive_archcompile_stop(Specs, Memoize, in_loader, _ProcessedSpecs).

% TODO: incomplete, it should be all the loader modules
in_loader([engine|_]). 

% ---------------------------------------------------------------------------

% Load dead instructions info
% TODO: this code is TEMPORAL for the sabsmach paper...
% TODO: we should create a emulator later... after composing the bytecode
%   in a single file, not in two steps...

:- use_module(library(read)).

:- multifile(unused_opcode/1).
:- data(unused_opcode/1).
:- multifile(used_opcode/1).
:- data(used_opcode/1).
:- multifile(comp_option/1).
:- data(comp_option/1).

get_dead(Name) :-
        '$open'(Name, r, Stream),
	repeat,
	  read(Stream, X),
	  ( X = end_of_file
	  ; get_dead__2(X),
	    fail
	  ), !,
	close(Stream).

get_dead__2(comp_option(X)) :-
	assertz_fact(comp_option(X)).
get_dead__2(used_opcode(X)) :-
	assertz_fact(used_opcode(X)).
get_dead__2(unused_opcode(X)) :-
	assertz_fact(unused_opcode(X)).
