:- module(comp_js, [], [compiler(complang)]).

:- doc(title, "Ciao compiler using the JavaScript backend").
:- doc(author, "Jose F. Morales").

:- doc(module, "Compile using the (alpha) JavaScript back-end for Ciao/OptimComp.").

% TODO: mimick comp.pl

:- use_module(compiler(module_jsexp_)).
:- include(compiler(ptojs__compiler)).
:- include(compiler(linker__ptojs)).

% ===========================================================================

% Help on options:
% 
% TODO: share with optim_comp
%
% --target-platform PLATFORM
%
%     The target platform specifies the platform the code will be
%     built and run against. E.g., NodeJS, V8 shell, some browser.
%
% TODO: at this moment this is just an atom
% TODO: see Eclipse IDE for detailed description of target platforms
% TODO: generalize for all backends

:- include(compiler(optparse)).

:- use_module(compiler(store)).
:- use_module(library(system), [getenvstr/2]).

% Include all compiler capabilities (necessary for 'use_module')
% (due to 'multifile' definitions that cannot be loaded in other way)
% TODO: Find a nicer way?
:- use_module(compiler(all_actions), []).
:- use_module(compiler(dynload), [use_module/2]).

% ---------------------------------------------------------------------------
% Enable/disable the library store definitions for cross-compilation
% TODO: when merging compiler/frontend.pl, this will include more than just paths
% TODO: temporal; override 'core' libraries with versions for ptojs
% TODO: add different 'store' contexts so that this is not necessary

:- data cross_compiling/0.

:- multifile disable_cross_comp_store/0.
disable_cross_comp_store :-
%	display(user_error, disabling_cross_comp_store), nl(user_error),
	( retract_fact(cross_compiling) -> true ; true ),
        ciao_lib_dir(Path0),
	store:del_source_path(~atom_concat(Path0, '/optim_comp/modules/core_ptojs')).
	
:- multifile enable_cross_comp_store/0.
enable_cross_comp_store :-
%	display(user_error, enabling_cross_comp_store), nl(user_error),
	( assertz_fact(cross_compiling) -> true ; true ),
        ciao_lib_dir(Path0),
	store:adda_source_path(~atom_concat(Path0, '/optim_comp/modules/core_ptojs')).

:- include(compiler(options__interface)).
:- use_module(library(aggregates)).
global__options(Opts) :-
	% Those options are only valid when doing cross compilation
	% TODO: make it nicer, this is a hack
	cross_compiling,
	!,
	findall(Opt, global__option(Opt), Opts).

global__option(Opt) :-
	optparse.get_optval(OptName, Val),
	\+ OptName = out_dir,
	Pragma =.. [OptName, Val],
	Opt = (:- '$pragma'(Pragma)).

% ---------------------------------------------------------------------------

:- public main/1.
main(Args) :-
	enable_cross_comp_store,
	%
        optparse.set_opttype(target_platform, atom),
	%
	optparse.parse_args(Args),
	% 
	set_out_dir,
	~optparse.get_args = [Input],
	compile_mod(Input),
        clean_compiler_state.

set_out_dir :-
	( getenvstr('CIAOCACHE', CacheDir0) ->
	    atom_codes(CacheDir, CacheDir0)
	; display(user_error, 'Error: cannot run without a definition for CIAOCACHE'),
	  nl(user_error),
	  fail
	),
	atom_concat(CacheDir, '/js-out', Path),
	optparse.set_optval(out_dir, Path).

:- multifile get_out_dir/1. % TODO: find a better way (comp_js.pl)
:- '$ctxprj'(get_out_dir/1, []).
get_out_dir(Path) :-
	optparse.get_optval(out_dir, Path).

% ===========================================================================
% TO-DO list

% TODO: define A=:=B as A.'$equiv'(B), use that in the measures.pl 
%   example to test equivalence

% TODO: make sure inheritance works (e.g. measures.pl)
% TODO: replace instanceof by kind_of (done?), implement instance_of
% TODO: make automatic 'new' optional
% TODO: custom class attributes must be unified! make them alias for arg0, arg1, etc. (generalization of argnames)
% TODO: relate the object model to Flora-2 F-Logic (e.g. C = ~John.children, ~C.born < ~John.yearOfAge(30))
% TODO: if two objects of the same class have attributes whose logical value is an identifier (i.e. data / mutref), then they only unify if they are exactly the same object
