:- module(global_pass__ptojs, [], [complang]).

% ===========================================================================
% TODO: duplicate of compiler/global_pass.pl (dependencies are handled
%   in a different way, merge)

:- use_module(library(assoc_yap)).
:- use_module(compiler(module_jsexp_)).
% TODO: BUG - import necessary for module_jsexp_
:- use_module(compiler(cdic_)). :- '$trust_statemodel'(cdic, pair).

% TODO: do not include interfaces!
:- include(compiler(psymbol__interface)).

% A set of modules
% TODO: This requires objects of the @class{module} to be identifiers
% (it will not work otherwise)
:- class module_set {
    % A set of module 'spec'
    :- '$raw_state'.
    :- '$statemodel'(pair).

    % TODO: missing instance_of__/1

    :- constructor empty_/0.
    empty_ :- ~self = ~empty_assoc.

    mark(Module) :-
        self <- ~put_assoc(Module, ~self, yes).

    :- constant marked/1.
    marked(Module) :-
        get_assoc(Module, ~self, _).
}.

:- public reachable_module_set/2.
% @var{Modules} is the list of (top enclosing) modules reachable from
% @var{StartModules}
% TODO: use top_enclosing_module for imports (but think when)
reachable_module_set(StartModules) := Modules :-
	visited :: module_set <- ~visited.empty,
	mds :: revaccum(Modules),
	reachable_module_set__2(StartModules).
{
    :- fluid visited :: module_set.
    :- fluid mds :: revaccum.
    % Visit all modules in StartModules (including nested), following import dependencies.
    % Only the 'top enclosing' is marked.
    reachable_module_set__2(StartModules) :-
	maplist(([visited, mds] -> ''(ModuleR) :-
	  trust(ModuleR instance_of module_s),
	  ( visited.marked(ModuleR) ->
	      true
	  ; visited.mark(ModuleR),
 	    mds.insert0(ModuleR),
	    reachable_module_set__2(~ModuleR.imported_list),
	    reachable_module_set__nested(~ModuleR.nested_module_list)
          )
        ), StartModules).

    reachable_module_set__nested(NestedModules) :-
	maplist(([visited, mds] -> ''(ModuleR) :-
	  trust(ModuleR instance_of module_s),
	  reachable_module_set__2(~ModuleR.imported_list),
	  reachable_module_set__nested(~ModuleR.nested_module_list)
        ), NestedModules).
}.
