% TODO: document: Incremental loading of single modules
:- module(_, [], [compiler(complang)]).

:- use_module(engine(basiccontrol)).
:- use_module(engine(term_basic)).

:- use_module(compiler(memoize)).
:- use_module(compiler(store)).
:- use_module(compiler(errlog)).
:- use_module(compiler(module_archbin)).

% ===========================================================================

:- doc(section, "Register action in 'memoize' (for incremental compilation)").

:- include(.(memoize__callback)).

action__input(load(Spec), [archcompile(Spec)]).
action__do(load(Spec), []) :-
	load_module(Spec).

% (make sure that compilation output files are defined)
:- use_module(compiler(frontend__outfiles)).

% ===========================================================================

% ---------------------------------------------------------------------------
% Single module loading (without dependencies or import table updates)
% TODO: compiler(dynload) deals with dependencies and metadata updates...

:- use_module(engine(dynlink)).

{
:- fluid memo :: memoize.

% Load a module (called by memoize object)
load_module(Spec) :-
	% Obtain result of archcompile
	Archbin = ~module_archbin.from_spec(Spec),
	HasBytecode = ( Archbin.contains_bytecode ? yes | no ),
	HasNative = ( Archbin.contains_native ? yes | no ),
	'$inst_destroy'(Archbin),
	% Load the module
	load_module__internal(Spec, HasBytecode, HasNative),
	!.
load_module(_Spec) :- % something failed...
	Errs = ~memo.errs,
	Errs.add_module_error,
	fail.
}.

% Load the bytecode and/or native code of a given module
:- public load_module__internal/3.
:- use_module(engine(rt_exp)).
load_module__internal(Spec, HasBytecode, HasNative) :-
	( spec_to_key(Spec, SpecKey),
	  current_speckey_module(SpecKey, Module),
	  static_module(Module) -> % ignore static modules
	    true
	; ( HasBytecode = yes ->
	      store:addr(compile__emu(Spec), BytecodeName)
	  ; BytecodeName = ''
	  ),
	  ( HasNative = yes ->
	      store:addr(archcompile__so(Spec), NativeName)
	  ; NativeName = ''
	  ),
	  load_module__nocheck(BytecodeName, NativeName)
	).
