:- module(_, _, [pure]).

:- use_module(engine(basiccontrol)).
:- use_module(engine(hiord_rt)).
:- use_module(compiler(dynload)).
:- use_module(compiler(action__load)).
:- use_module(engine(internals), [initialize_module/1]).

main(['--load', File]) :-
	load(File).
main(['--recursive-deps'|Specs0]) :-
	factory:normalized_specs(Specs0, Specs), recursive_deps(Specs).
main(['--recursive-load'|Specs0]) :-
	factory:normalized_specs(Specs0, Specs), recursive_load(Specs).
main(['--recursive-load-init'|Specs0]) :-
	factory:normalized_specs(Specs0, Specs), recursive_load(Specs),
	do_initialization(shell). % TODO: hmmm ....
main(['--recursive-load-fastinit'|Specs0]) :-
	factory:normalized_specs(Specs0, Specs), recursive_load(Specs),
	initialize_module(shell). % TODO: hmmm ....
%main([Module, Pred]) :-
%        use_module(Module, [Pred/0]), Pred.
main([Module, Pred|Args]) :-
        use_module(Module, [Pred/1]), call(Pred, Args).

:- use_module(engine(term_basic)).
:- use_module(engine(basic_props)).
:- use_module(engine(io_basic)).
:- use_module(compiler(errs)).
:- use_module(compiler(factory)).
:- use_module(compiler(global_pass)).
:- use_module(library(write)).

recursive_deps(Specs) :-
	errs:create(Errs),
        Verbose = off,
	Errs/errs:add_verbose(Verbose),
	factory:create(Errs, Factory),
	( global_pass:reachable_module_set(Specs, Factory, _ProcessedSpecs) ->
	    true
	; true
	),
%	  member(P, ProcessedSpecs),
%	  writeq(spec(P)), write('.'), nl,
	Factory/factory:unify_errs(Errs), Errs/errs:summary,
	Factory/factory:delete,
	Errs/errs:delete.

recursive_load(Specs) :-
	errs:create(Errs),
        Verbose = off,
	Errs/errs:add_verbose(Verbose),
	factory:create(Errs, Factory),
	( global_pass:reachable_module_set(Specs, Factory, ProcessedSpecs),
	  member(P, ProcessedSpecs),
	  Element = element(loaded_module, P),
%%	  Element = element(prolog_archbin, P),
%	  display(k1(P)), nl,
	  Factory/factory:element_update(Element),
%	  display(k), nl,
	  fail
	; true
	),
	Factory/factory:unify_errs(Errs), Errs/errs:summary,
	Factory/factory:delete,
	Errs/errs:delete.

:- use_module(engine(dynlink__bytecode), [link_pack/1]).

load(File) :-
	link_pack(File),
	errs:create(Errs),
        Verbose = off,
	Errs/errs:add_verbose(Verbose),
	factory:create(Errs, Factory),
	( '$meta_call'('internal_init:$eager_load'(Spec)),
	  Element = element(loaded_module, Spec),
	  Factory/factory:element_update(Element),
	  fail
	; true
	),
	Factory/factory:unify_errs(Errs), Errs/errs:summary,
	Factory/factory:delete,
	Errs/errs:delete,
	'$meta_call'('internal_init:boot').
