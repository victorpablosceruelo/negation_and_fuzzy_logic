:- module(rt_exp, [], [assertions, pure]).
%% Run-time module (and goal) expansions
% TODO: includes run-time program changes... move that part to other module?
% TODO: implement in C

:- '$native_include_c_source'(.(rt_exp)).

:- use_module(engine(basiccontrol)).
:- use_module(engine(term_basic)).
:- use_module(engine(term_typing)).
:- use_module(engine(term_compare)).
:- use_module(engine(arithmetic)).
:- use_module(engine(atomic_basic)).
:- use_module(engine(data_facts)).

% Use 'complang mini' to minimize dependencies at the runtime 
:- use_package(compiler(complang_mini)).
:- use_module(compiler(compiler_object__rt)).

% ---------------------------------------------------------------------------
% Translation hooks (only 'goal' translations)
% TODO: make it even simpler (if possible)

:- export(add_trans_hook/4).
:- include(compiler(trans_hook_db)).
%     [add_trans_hook/4, del_trans_hook/1, pqueue_values/2]
:- data translation_hook/3.
{
:- fluid m :: any.
get_translation_hook(Kind, KVs) :-
	current_fact(translation_hook(~m, Kind, KVs)).
set_translation_hook(Kind, KVs) :-
	M = ~m,
	retractall_fact(translation_hook(M, Kind, _)),
	assertz_fact(translation_hook(M, Kind, KVs)).
add_translation_hook(Kind, KVs) :-
	assertz_fact(translation_hook(~m, Kind, KVs)).
del_translation_hook(Kind) :-
	retractall_fact(translation_hook(~m, Kind, _)).
% Marks/unmarks translations in the module (speed-up metacalls when no
% goal translations are necessary).
mark_trans_hook(goal) :- !, '$mod_setgoaltrans'(~m, 1).
mark_trans_hook(_).
unmark_trans_hook :- '$mod_setgoaltrans'(~m, 0).
:- '$props'('$mod_setgoaltrans'/2, [impnat=cbool(prolog_module_setgoaltrans)]).
}.

:- use_module(compiler(translation_common)).
% (necessary in trans_hook_db)
append([], L, L).
append([E|Es], L, [E|R]) :- append(Es, L, R).

% ---------------------------------------------------------------------------

% TODO: clean entries that are not longer needed
:- dynamic('$meta_args'/1).
:- multifile('$meta_args'/1).
:- dynamic('$context'/3).
:- multifile('$context'/3).
% TODO: implement as a hashtab
:- dynamic('$specmod'/2).
:- multifile('$specmod'/2).

:- compilation_fact(mexpand_runtime_db).
:- include(compiler(mexpand)).

% TODO: implement a 'change functor' in C, use it only when the context is none (if necessary implement also versions that adds the context, and versions for predicate abstractions)

mexpand__goal_trans(M, T) :-
	m :: any <- M,
	get_translation_hook(goal, KV), % (nondet)
	pqueue_values([KV], [T]).
mexpand__fun_eval(_, _) :- fail. % TODO: enable it by recognizing fun_eval at toplevel?
mexpand__eval_arith :- fail. % TODO: enable it by recognizing fun_eval at toplevel?
mexpand__option(functional_expand) :- fail. % TODO: enable it by recognizing fun_eval at toplevel?
mexpand__uses_rt_metacast(_,_,_).
mexpand__uses_hiord.
mexpand__uses_hiord_pred(_, _).

mexpand__error(Error) :-
	throw('$rt_exp'(Error)).
% TODO: catch those errors in toplevel
%	( Error = not_defined(_, _, _) ->
%	    '$unknown'(fail,fail)
%	; error_messages:runtime_error(Error, Type, Message),
%	  message(Type, Message)
%	).

% ---------------------------------------------------------------------------
% -- dynamic module interface change --
% TODO: move to other file?!?! expansion was read-only...

:- use_module(library(prolog_sys), [new_atom/1]).
:- use_module(engine(exceptions)).

:- export(current_module/1).
current_module(Module) :- '$mod_current'(Module).

:- export(current_speckey_module/2).
current_speckey_module(SpecKey, Module) :- '$specmod'(SpecKey, Module).

:- export(current_predicate/1).
:- '$context'(current_predicate/1, module).
current_predicate(F/A) :-
	'$module'(M),
	current_predicate(F/A, M).

:- export(current_predicate/2).
% TODO: this 'module' is not used, but it is needed to avoid an internal name clash with the current_predicate/1 (we need to solve it...)
% TODO: do not use module concat? -> it will be more flexible but if we do that we could only inspect the modules that uses runtime expansions... 
:- '$context'(current_predicate/2, module).
current_predicate(F/A, M) :-
	'$mod_current'(M),
        '$module_concat'('', M, MPref),
        '$predicate_property'(MF/A, _, _),
        atom_concat(MPref, F, MF).

:- export('$current_predicate'/2).
% TODO: low level version... used in library(debugger)...
:- '$props'('$current_predicate'/2, [impnat=cbool(current_predicate)]).

% TODO: document... moved from old c_itf... I am not sure if it is the place
% TODO: fix multifile+data... how?
% TODO: multifile+data is strange... first multifile must be called and then data
:- export(multifile/1).
:- '$context'(multifile/1, module).
%:- true pred multifile(+Spec).
multifile(F/A) :-
	'$module'(M),
	multifile__2(F/A, M). 

multifile__2(F/A, M) :-
        ( ( '$mod_defines'(M, F, A, MF) ;
	    '$module_concat'(F, M, MF), '$predicate_property'(MF/A, _, _) ) ->
	    % TODO: the predicate may actually exists and the definition entry be mssing... (if the module does not uses runtime expansions...)
	    % check that it is multifile
	    '$predicate_property'(MF/A, _, Prop),
	    Prop /\ 2'100 =:= 2'100
	; '$module_concat'(F, multifile, MF),
	  '$mod_set_defines'(M, F, A, MF)
	  % note: only the name is set...
	).

% TODO: make it work with multifile predicates?
% TODO: change documentation of dynamic, data and concurrent: now they also check if predicate is defined as dynamic, data or concurrent
:- export(dynamic/1).
:- '$context'(dynamic/1, module).
dynamic(F/A) :-
	'$module'(M),
	ensure_pred_props(F, M, A, 2'10, 2'01, dynamic/1).

:- export(data/1).
:- '$context'(data/1, module).
% By now identical to dynamic
data(F/A) :-
	'$module'(M),
	ensure_pred_props(F, M, A, 2'10, 2'01, data/1).

:- export(concurrent/1).
:- '$context'(concurrent/1, module).
concurrent(F/A) :-
	'$module'(M),
	ensure_pred_props(F, M, A, 2'01, 2'11, concurrent/2).

% ensure that the predicate is defined
% note: creates a new name if predicate name is not instantiated
% TODO: make it work with multifile predicates?
define(F, A, M, MF) :-
        '$mod_defines'(M, F, A, MF), !.
define(F, A, M, MF) :-
	( var(F) -> new_atom(F) ; atom(F) ),
        '$module_concat'(F, M, MF),
        % TODO: update module->defines if predicate does not use runtime expansions?
        % TODO: do nothing if the predicate is multifile?
	% TODO: the predicate may actually exists and the definition entry be mssing... (if the module does not uses runtime expansions...)
        '$mod_set_defines'(M, F, A, MF).

ensure_pred_props(F, M, A, B1, B2, Goal) :-
	define(F, A, M, MF),
	( '$predicate_property'(MF/A, _, Prop) ->
	    ( Prop/\B1 =:= B1 -> true
	    ; throw(error(permission_error(modify, static_procedure, MF/A), Goal))
	    )
	; '$define_predicate'(MF/A, B2)
	). 

:- export('$predicate_property'/3).
% TODO: hmmm why export?
:- '$props'('$predicate_property'/3, [impnat=cbool(predicate_property)]).

:- export('$check_dynamic'/2).
% TODO: this checks that a predicate is dynamic...
% TODO: used in library(dynamic), make it behave more like dynamic/1 if you want automatic insertion of dynamic predicates...
'$check_dynamic'(X, _Goal) :-
	functor(X, MF, A),
	'$predicate_property'(MF/A, _, Prop), !,
	B1 = 2'10,
	Prop/\B1 =:= B1.

:- export('$define_predicate'/2).
% TODO: compiler_object__rt uses it... hide!
:- '$props'('$define_predicate'/2, [impnat=cbool(define_predicate)]).

% ---------------------------------------------------------------------------

:- export(exported/2).
exported(F/A, M) :-
	'$mod_exports'(M, F, A, _).

% ---------------------------------------------------------------------------

:- export(update_dyn_imports/3).
update_dyn_imports(M, Imports, IM) :-
	purge_imports(M, IM),
	include_imports(M, Imports, IM),
	'$add_mod_uses'(M, IM).

:- '$props'('$add_mod_uses'/2, [impnat=cbool(module_add_uses)]).

% Include new imports
include_imports(M, Imports, IM) :-
  	Imports = all, !,
  	add_imports_all(M, IM).
include_imports(M, Imports, IM) :-
	( member(F/A, Imports),
	    ( '$mod_exports'(IM, F, A, MF) ->
	        add_imports(M, IM, F, A, MF)
	    ; true
	    ),
	    fail
        ; true
	).

member(X, [X|_]).
member(X, [_|Xs]) :- member(X, Xs).

% add_imports_all(M, IM)
:- '$props'(add_imports_all/2, [impnat=cbool(prolog_add_imports_all)]).

% add_imports(M, IM, F, A, MF).
:- '$props'(add_imports/5, [impnat=cbool(prolog_add_imports)]).

% Remove imports entries no longer exported
% TODO: check
% ?- use_module(a, [b/0]).
% (a changes and does not exports b/0)
% ?- use_module(a, [c/0]).
% is b/0 still accessible from the top level?
% TODO: it should purge all the imports of all the modules that imports IM, not only M
% purge_imports(M, IM)
:- '$props'(purge_imports/2, [impnat=cbool(prolog_purge_imports)]).

:- export(remove_dyn_imports/2).
remove_dyn_imports(M, IM) :-
        '$del_mod_uses'(M, IM), !,
	remove_imports(M, IM).
remove_dyn_imports(_, _).

:- '$props'('$del_mod_uses'/2, [impnat=cbool(module_del_uses)]).
% Remove all the imports of IM from M
% remove_imports(M, IM)
:- '$props'(remove_imports/2, [impnat=cbool(prolog_remove_imports)]).

% ---------------------------------------------------------------------------

% TODO: also used in the compiler... move to other module?
:- export('$user_module_id'/1).
:- '$props'('$user_module_id'/1, [impnat=cbool(prolog_user_module_id)]).

:- export('$new_user_module_id'/2).
:- '$props'('$new_user_module_id'/2, [impnat=cbool(prolog_new_user_module_id)]).

:- export('$module_concat'/3).
:- '$props'('$module_concat'/3, [impnat=cbool(prolog_module_concat)]).

% ---------------------------------------------------------------------------

:- export('$mod_exports'/4).
:- '$props'('$mod_exports'/4, [impnat=cbool(prolog_module_exports)]).

:- export('$mod_defines'/4).
:- '$props'('$mod_defines'/4, [impnat=cbool(prolog_module_defines)]).

:- export('$mod_set_defines'/4).
:- '$props'('$mod_set_defines'/4, [impnat=cbool(prolog_module_set_defines)]).

:- export('$mod_current'/1).
:- '$props'('$mod_current'/1, [impnat=cbool(current_module)]).

% ---------------------------------------------------------------------------
% OO extensions
% TODO: actually defined in dynlink.c... unify both modules

:- export('$oo_class_v'/3).
:- '$props'('$oo_class_v'/3, [impnat=cbool(prolog_class_v)]).
:- export('$oo_class_i'/3).
:- '$props'('$oo_class_i'/3, [impnat=cbool(prolog_class_i)]).
:- export('$oo_attrtype_v'/3).
:- '$props'('$oo_attrtype_v'/3, [impnat=cbool(prolog_attrtype_v)]).
:- export('$oo_attrtype_i'/4).
:- '$props'('$oo_attrtype_i'/4, [impnat=cbool(prolog_attrtype_i)]).

% ---------------------------------------------------------------------------

:- export(do_initialization/1).
% do_initialization(M)
% # Initialize module M and all dependand modules
% TODO: write module ending hooks
do_initialization(M) :-
	is_initialized_module(M), !.
do_initialization(M) :-
	set_initialized_module(M), % avoid loops
        ( module_uses(M, IM), % initialize dependant modules
	    do_initialization(IM),
            fail
	; '$initialization'(M), % execute and fail
	  fail
	; true
	).

:- '$props'(module_uses/2, [impnat=cbool(module_uses)]).

:- '$props'(is_initialized_module/1, [impnat=cbool(prolog_is_initialized_module)]).
:- '$props'(set_initialized_module/1, [impnat=cbool(prolog_set_initialized_module)]).
