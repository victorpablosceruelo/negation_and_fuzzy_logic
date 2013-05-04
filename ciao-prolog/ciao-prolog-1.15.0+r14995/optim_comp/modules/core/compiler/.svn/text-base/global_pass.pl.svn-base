:- module(_, [], [compiler(complang)]).

% TODO: add actions here (not only expand... do compile before so that ref are reused)

:- use_module(compiler(module_ideps)).
:- use_module(compiler(errlog)).
:- use_module(compiler(memoize)).
:- use_module(compiler(store)).

:- use_module(library(dict)).

% TODO: offer versions of use transitive_do directly in code?
{
:- fluid memo :: memoize.
:- public reachable_module_set/2.
% Set of modules that are reachable from RootSpecs
reachable_module_set(RootSpecs, Specs) :-
	transitive_do(RootSpecs, none, none, Specs).

:- '$ctxprj'(none/1, []).
none(_) :- fail.

:- public transitive_compile/2.
transitive_compile(RootSpecs, Specs) :-
	transitive_do(RootSpecs, compile, none, Specs).

:- public meta_predicate transitive_compile_stop(?, pred(1), ?).
transitive_compile_stop(RootSpecs, Stop, Specs) :-
	transitive_do(RootSpecs, compile, Stop, Specs).

:- public meta_predicate transitive_archcompile_stop(?, pred(1), ?).
transitive_archcompile_stop(RootSpecs, Stop, Specs) :-
	transitive_do(RootSpecs, archcompile, Stop, Specs).

% TODO: Rewrite as an abstract class
:- meta_predicate transitive_do(?, ?, pred(1), ?).
transitive_do(RootSpecs, Act, Stop, Specs) :-
	visited :: specset <- ~visited.empty,
	act :: any <- Act,
	stop :: any <- Stop,
	specs :: revaccum(Specs),
	transitive_do__2(RootSpecs).
{
    % TODO: improve indexing of visited/1?
    :- fluid visited :: specset. % modules that have been visited 
    :- fluid act :: any.
    :- fluid stop :: any.
    :- fluid specs :: revaccum.
    transitive_do__2(RootSpecs) :-
	maplist(([specs] -> ''(Spec) :-
	  ( visited.marked(Spec) ->
	      true
	  ; visited.mark(Spec),
	    ( Stop = ~stop,
	      '$trust_metatype'(Stop, pred(1)),
	      Stop(Spec) ->
	        true
 	    ; specs.insert0(Spec),
	      Memo = ~memo,
 	      Act = ~act,
	      ( Act = none ->
	          true
	      ; functor(ActG, Act, 1),
	        arg(1, ActG, Spec),
	        Memo.eval0(ActG)
	      ),
	      IDeps = ~module_ideps.from_spec(Spec),
	      ImportedSpecs = ~imported_list(IDeps),
	      '$inst_destroy'(IDeps),
	      transitive_do__2(ImportedSpecs)
	    )
          )
        ), RootSpecs).
}.
}.

:- class specset {
    % TODO: Do not use the 'spec', use the module 'id' which should be unique.
    % A set of module 'spec'
    :- '$raw_state'.
    :- '$statemodel'(single).

    % TODO: missing instance_of__/1

    :- constructor empty_/0.
    empty_.

    mark(Spec) :-
        spec_to_key(Spec, SpecKey),
        dic_lookup(~self, SpecKey, yes).

    marked(Spec) :-
        spec_to_key(Spec, SpecKey),
        dic_get(~self, SpecKey, _).
}.

% TODO: add interface to treat those as tables: insert elements, lookup elements, get elements, get element list, etc.
:- use_module(library(aggregates), [findall/3]).
imported_list(IDeps) := ImportedSpecs :-
	findall(ImportedSpec, global_pass:imported(IDeps, ImportedSpec), ImportedSpecs).
imported(IDeps, ImportedSpec) :-
	trust(IDeps instance_of module_ideps),
	IDeps.imported(_, ImportedSpec).
