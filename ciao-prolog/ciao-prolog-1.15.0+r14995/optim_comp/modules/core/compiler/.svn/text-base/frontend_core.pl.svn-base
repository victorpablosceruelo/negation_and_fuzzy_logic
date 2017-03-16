% Front-end definitions for the core language
% TODO: This can be split in smaller pieces

% ===========================================================================

:- doc(section, "Definition of Module Handlers").

% TODO: this should be extensible with hooks
% TODO: merge with the module system documentation (extract it from here)

% Packages and mixins

mod__nested(mixin, mod_mixin). % TODO: are those nested packages?

mod__treatDom(package(_)).
mod__treat(package(_), mod_package, package) :- !.

% Instantiable modules (classes and interfaces)

mod__nested(class, mod_class).

mod__treatDom(class(_)).
mod__treat(class(Name), mod_class, module(Name, [], [])) :- !.

mod__treatDom(class(_,_)).
mod__treat(class(Name, Exports), mod_class, module(Name, Exports, [])) :- !.

mod__treatDom(class(_,_,_)).
mod__treat(class(Name, Exports, Packages), mod_class, module(Name, Exports, Packages)) :- !.

mod__nested(interface, mod_interface).

% Static modules (default case)

mod__nested(module, mod_static).

mod__treatDom(module(_,_)).
mod__treat(module(Module, Exports), mod_static, module(Module, Exports, Package)) :- !,
        default_package(Package).

% TODO: Make it user customizable?
% TODO: Synchronize with trunk/. It is using the 'default' package
%   instead, and 'classic' seems out of data in trunk/ (it does not
%   contain the ':- package' declaration).
default_package([classic]).

mod__treatDom(module(_,_,_)).
mod__treat(Decl, mod_static, Decl) :- Decl = module(_, _, _), !.

% ===========================================================================

:- doc(section, "Support for compilation modules").
% TODO: Make it work for nested modules

:- use_module(compiler(translation_module_holder)).

{
:- extends modread_ctx.
do_load_compilation_module(CompSpec) :-
	Module = ~top_envmod, % TODO: use def_envmod
	Module.compilation_module(CompSpec), % TODO: poor indexing
	!.	
:- if(use_backend(js)).
% TODO: use different 'store' states, see comp_js.pl for more details
do_load_compilation_module(CompSpec) :-
	Module = ~top_envmod, % TODO: use def_envmod
	Module.add_compilation_module(CompSpec),
	( store:denormalized_spec(CompSpec, CompUspec),
	  disable_cross_comp_store,
	  translation_module_holder:do_use_module(CompUspec) ->
	  enable_cross_comp_store ->
	    true
	; ft_error(load_compilation_module_failed(CompSpec))
	).
:- else.
do_load_compilation_module(CompSpec) :-
	Module = ~top_envmod, % TODO: use def_envmod
	Module.add_compilation_module(CompSpec),
	( store:denormalized_spec(CompSpec, CompUspec),
	  translation_module_holder:do_use_module(CompUspec) ->
	    true
	; ft_error(load_compilation_module_failed(CompSpec))
	).
:- endif.
}.

:- if(use_backend(js)).
{
:- '$all_static'.
:- multifile disable_cross_comp_store/0.
:- multifile enable_cross_comp_store/0.
}.
:- endif.

% TODO: Fix translations: they must be automatically enabled for all nested modules
%       (unless otherwise specified)
% TODO: Make sure that the bug solved in 'r5355' is not present in this code.

{
:- extends modread_ctx.
do_add_sentence_trans(P, Prior) :-
	M = ~((~top_envmod).get_name), % TODO: use def_envmod and the module identifier
        ( translation_module_holder:do_meta_exp_spec(P, P2),
	  add_trans_hook(M, sentence, P2, Prior) ->
	    % For initialization
            syntr__init(M, P2)
        ; warning_failed_decl(add_sentence_trans(P, Prior))
        ).

do_add_term_trans(P, Prior) :-
	M = ~((~top_envmod).get_name), % TODO: use def_envmod and the module identifier
        ( translation_module_holder:do_meta_exp_spec(P, P2),
	  add_trans_hook(M, term, P2, Prior) ->
	    true
        ; warning_failed_decl(add_term_trans(P, Prior))
        ).
}.

:- doc(subsection, "Translation hooks (term and sentence)").
% TODO: duplicated in toplevel.pl

{
:- extends modread_ctx.

:- redefining(add_trans_hook/4).
:- redefining(del_trans_hook/1).
:- redefining(pqueue_values/2).
:- redefining(get_translation_hook/2).
:- redefining(set_translation_hook/2).
:- redefining(add_translation_hook/2).
:- redefining(del_translation_hook/2).
:- redefining(mark_trans_hook/1).
:- redefining(unmark_trans_hook/0).
:- include(compiler(trans_hook_db)).
%     [add_trans_hook/4, del_trans_hook/1, pqueue_values/2]
get_translation_hook(Kind, Tr) :-
	Module = ~top_envmod, % TODO: use def_envmod
	Module.get_translation_hook(Kind, Tr).
set_translation_hook(Kind, Tr) :-
	Module = ~top_envmod, % TODO: use def_envmod
	Module.set_translation_hook(Kind, Tr).
add_translation_hook(Kind, Tr) :-
	Module = ~top_envmod, % TODO: use def_envmod
	Module.add_translation_hook(Kind, Tr).
del_translation_hook(Kind) :-
	Module = ~top_envmod, % TODO: use def_envmod
	Module.del_translation_hook(Kind).
mark_trans_hook(_).
unmark_trans_hook.

:- include(compiler(syntactic_translation)).
% [syntr__expand_query/3, syntr__expand_sentence/3].
}.

% ---------------------------------------------------------------------------

:- doc(subsection, "Directives to add translation hooks (term and sentence)").

% ---------------------------------------------------------------------------
% load_compilation_module/1 - Load a compilation module

% TODO: Make sentences optional, so that we can forbid user modules
%       (or contexts) from loading compilation modules (which can be
%       dangerous).
decl__treatDom(load_compilation_module(_)).
decl__treat(load_compilation_module(CompSpec0)) :- !,
	CompSpec = ~find_source(CompSpec0),
        do_load_compilation_module(CompSpec).

% ---------------------------------------------------------------------------
% add_sentence_trans/2 - Add a sentence translation hook

decl__treatDom(add_sentence_trans(_, _)).
decl__treat(add_sentence_trans(P, Prior)) :- !,
        do_add_sentence_trans(P, Prior).

% ---------------------------------------------------------------------------
% add_term_trans/2 - Add a term translation hook

decl__treatDom(add_term_trans(_, _)).
decl__treat(add_term_trans(P, Prior)) :- !,
        do_add_term_trans(P, Prior).

% ---------------------------------------------------------------------------
% add_goal_trans/2 - Add a goal translation hook

decl__treatDom(add_goal_trans(_, _)).
:- if(use_backend(bc)).
decl__treat(add_goal_trans(P, Prior)) :- !,
	% note: treated in the 'compile' part
	Module = ~top_envmod, % TODO: use def_envmod
	Module.add1_pragma(add_trans_hook(goal, P, Prior)).
:- elif(use_backend(js)).
% TODO: do not do it in this way
decl__treat(add_goal_trans(P, Prior)) :- !,
	% note: treated in the 'compile' part
	M = ~((~top_envmod).get_name), % TODO: use def_envmod and the module identifier
        ( translation_module_holder:do_meta_exp_spec(P, P2),
	  add_trans_hook(M, goal, P2, Prior) ->
	    true
        ; warning_failed_decl(add_goal_trans(P, Prior))
        ).
:- endif.

% ---------------------------------------------------------------------------
% add_clause_trans/2 - Add a clause translation hook

decl__treatDom(add_clause_trans(_, _)).
decl__treat(add_clause_trans(P, Prior)) :- !,
	% note: treated in the 'compile' part
	Module = ~top_envmod, % TODO: use def_envmod
	Module.add1_pragma(add_trans_hook(clause, P, Prior)).

% ===========================================================================

:- doc(section, "Extensible Declarations").
% TODO: missing hooks for handling them

% ---------------------------------------------------------------------------
% new_declaration/1 - Declare a new declaration

decl__treatDom(new_declaration(_)).
decl__treat(new_declaration(S)) :- !,
        do_new_decl(S, private).

decl__treatDom(new_declaration(_, _)).
decl__treat(new_declaration(S, DeclVisibility)) :- !,
        do_new_decl(S, DeclVisibility).

{
:- extends modread_ctx.
do_new_decl(S, DeclVisibility) :-
        ( S = F/A, functor(D, F, A), new_decl_visibility(DeclVisibility) ->
	    Module = ~top_envmod, % TODO: use def_envmod
	    Module.add1_new_decl(D, DeclVisibility)
        ; ft_error(bad_newdecl(S, DeclVisibility))
        ).
}.

new_decl_visibility(X) :- var(X), !, fail.
new_decl_visibility(public).
new_decl_visibility(private).

% ===========================================================================

:- doc(section, "Predicates and Fields of Modules (and Classes)").

% ---------------------------------------------------------------------------
% Symbol modifiers

% TODO: Declare when a modifier is valid for a symbol different than
%   predicates.

% Export predicates (also see ":- export/1")
symmodif__def(public) :- get_pragma(class_expand), !.
symmodif__set(public, SymSpec) :- !,
	do_export(SymSpec). % Note: not only predicates

symmodif__def(static) :- get_pragma(class_expand), !.
symmodif__set(static, N/A) :- !,
	FunctorR = ~pred_ref_ac(N, A),
	FunctorR.set_prop(static).

% TODO: not in ptojs
symmodif__def(constant) :- get_pragma(class_expand), !.
symmodif__set(constant, N/A) :- !,
	FunctorR = ~pred_ref_ac(N, A),
	FunctorR.set_prop(constant).

symmodif__def(virtual) :- get_pragma(class_expand), !.
symmodif__set(virtual, N/A) :- !,
	FunctorR = ~pred_ref_ac(N, A),
	FunctorR.set_prop(virtual).

% ---------------------------------------------------------------------------
% Constructors, initializations, destructors, etc.

% TODO: not in ptojs
symdecl__treatDom(constructor(_)) :- get_pragma(class_expand), !.
symdecl__treat(constructor(N/A)) :- !,
	Module = ~def_envmod,
	do_constructor(Module, N, A).
symdecl__symspec(constructor(N/A), N/A) :- !.

% initialization/1 - Initialization hook
% TODO: not in ptojs
decl__treatDom(initialization(_)).
decl__treat(initialization(Goal)) :-
	Module = ~def_envmod,
	MId = ~Module.get_name, % TODO: use an 'interface' for this
        envmod.add_clause('$initialization'(MId), Goal).

% on_abort/1 - Abort hook
% TODO: not in ptojs
decl__treatDom(on_abort(_)).
decl__treat(on_abort(Goal)) :-
	Module = ~def_envmod,
	MId = ~Module.get_name, % TODO: use an 'interface' for this
        envmod.add_clause('$on_abort'(MId), Goal).

% ---------------------------------------------------------------------------
% redefining/1 - Declare a predicate redefinition

% TODO: this is similar to 'override' but for symbols coming from
%       imported modules, not for inherited predicates

% TODO: Transform into a symmodif?
decl__treatDom(redefining(_)).
decl__treat(redefining(Predspec)) :- !,
	% TODO: not working in ptojs
        do_redefining(Predspec).

{
:- extends modread_ctx.
do_redefining(SymSpec) :- nonvar(SymSpec), SymSpec = F/A, atom(F), integer(A), !,
	(~def_envmod).add1_def_redefining(F, A).
do_redefining(SymSpec) :-
        ft_error(bad_redefining(SymSpec)).
}.

% ---------------------------------------------------------------------------
% multifile/1 - Declare a predicate as a multifile

symmodif__def(multifile).
symmodif__set(multifile, N/A) :- !,
	% TODO: not working in ptojs
	Pred = ~pred_ref_ac(N, A),
	Pred.set_prop(multifile).

% ---------------------------------------------------------------------------
% data/1 dynamic/1 concurrent/1
% Set the preddef of a predicate to Def using Decl declaration (Decl
% is used to emit error messages)

symmodif__def(data).
symmodif__set(data, N/A) :- !,
	Pred = ~pred_ref_ac(N, A),
	Pred.set_def(data).

symmodif__def(dynamic).
symmodif__set(dynamic, N/A) :- !,
	% TODO: not working in ptojs
	Pred = ~pred_ref_ac(N, A),
	Pred.set_def(dynamic).

symmodif__def(concurrent).
symmodif__set(concurrent, N/A) :- !,
	% TODO: not working in ptojs
	Pred = ~pred_ref_ac(N, A),
	Pred.set_def(concurrent).

% ---------------------------------------------------------------------------
% Explicit local symbols (atom-based module system)
% TODO: enable only for 'atom based'

% TODO: Transform into a symmodif?
decl__treatDom(local(_)) :- get_pragma(class_expand), !.
decl__treat(local(NA)) :- nonvar(NA), NA = N/A, !,
	mod_set_local(N, A).

% ---------------------------------------------------------------------------
% Explicit partial applications
% TODO: enable only for 'atom based'; find a better solution
% TODO: I do not like this idea, but it may be necessary...
%       (unless we give ambiguity errors for partial applications)

% partial(F/A, G/B), with A>=B, declares G/B as a partial application of F/A.
%
%   That is, given GT=g(X1...Xb), GT(Xb+1...Xa) and f(X1...Xb, Xb+1...Xa) 
%   should be equivalent. The number of missing arguments are A-B.
decl__treatDom(partial(_, _)) :- get_pragma(class_expand), !.
decl__treat(partial(FA, GB)) :-
	nonvar(FA), FA = F/A,
	nonvar(GB), GB = G/B,
	A >= B,
	!,
	mod_set_partial(F, A, G, B).

% ---------------------------------------------------------------------------
% Deprecated impl_defined/1

% TODO: deprecated, remove in the future
symmodif__def(impl_defined).
symmodif__set(impl_defined, N/A) :- !,
% 	Pred = ~pred_ref_ac(N, A),
% 	Pred.set_def(unknown).
       ft_error(deprecated_impl_defined(N, A)).

% ---------------------------------------------------------------------------
% '$default_preddef'/1 - internal declaration % TODO: document

% TODO: Problem: when multiple props are defined and impnat is the
%       last one...

decl__treatDom('$default_preddef'(_)).
decl__treat('$default_preddef'(Def)) :- !,
	% TODO: check that it has not been changed twice...
	Module = ~def_envmod,
	( Module.get_prop(default_preddef(Def0)) ->
	    ( Def0 == Def -> true
	    ; % TODO: error, not a bug
	      ft_error(bug(default_preddef_mismatch(Def0, Def)))
	    )
	; Module.set_prop(default_preddef(Def))
	).

% ----------------------------------------------------------------
% meta_predicate/1 - metapredicate information for a predicate

symdecl__treatDom(meta_predicate(_)).
symdecl__treat(meta_predicate(Meta)) :- !,
        do_meta_predicate(Meta).
symdecl__symspec(meta_predicate(Meta), NA) :- !,
	functor(Meta, N, A), NA = N/A.

{
:- extends modread_ctx.
do_meta_predicate(Meta) :- var(Meta), !,
        ft_error(bad_meta_predicate(Meta)).
do_meta_predicate(Meta0) :-
        functor(Meta0, N, A),
	Pred = ~pred_ref_ac(N, A),
        functor(Meta, '$meta$', A),
        normalize_meta_args(1, A, Meta0, Meta), !,
        ( Pred.get_prop(meta(Meta2)) ->
	    ( Meta2 == Meta ->
	        true
	    ; ft_error(conflicting_meta_predicate(Meta))
	    )
	; Pred.set_prop(meta(Meta))
	).
do_meta_predicate(Meta) :-
        ft_error(bad_meta_predicate(Meta)).

normalize_meta_args(N, A, _, _):- N>A, !.
normalize_meta_args(N, A, Predspec, NPredspec):-
        arg(N, Predspec, X),
        normalize_meta_arg(X, NX),
        arg(N, NPredspec, NX),
        N1 is N+1,
        normalize_meta_args(N1, A, Predspec, NPredspec).

normalize_meta_arg(X, X) :- var(X), !, X = '?'. % A variable is valid also!
normalize_meta_arg('?', '?') :- !. 
normalize_meta_arg('-', '?') :- !. 
normalize_meta_arg('+', '?') :- !. 
normalize_meta_arg(':', goal) :- !. 
normalize_meta_arg(X, X) :- X = primitive(X0), !, real_meta_arg(X0).
normalize_meta_arg(X, X) :- real_meta_arg(X).

% TODO: rename meta_arg by metatype
% TODO: metatypes should be qualified too (like any program symbol)
real_meta_arg(goal) :- !.
real_meta_arg(goal(_Context)) :- !. % TODO: check context!
real_meta_arg(clause) :- !.
real_meta_arg(fact) :- !.
real_meta_arg(spec) :- !.
real_meta_arg(pred(N)) :- !, integer(N), N>=0, N=<255.
real_meta_arg(pred(N, _Context)) :- !, integer(N), N>=0, N=<255. % TODO: check context!
real_meta_arg(list(X)) :- !,
        real_meta_arg(X).
% new (JF)
real_meta_arg(out(X)) :- !, % TODO: document? This is necessary to say that the variable is not instantiated on input, but on output...
	real_meta_arg(X).
real_meta_arg(_X) :- get_pragma(class_expand), !. % TODO: check that _X is actually a class (later)
}.

% ---------------------------------------------------------------------------

:- doc(section, "Definitions for packages and included files").

% ---------------------------------------------------------------------------
% use_package/1 - Include a package

decl__treatDom(use_package(_)).
decl__treat(use_package(PackageSpecs)) :- !,
        do_use_package(PackageSpecs).

% TODO: this accepts things like [a|k]... ugly
% TODO: syncronize with official version: packages included twice are ignored
% TODO: check errors in input parameter?
{
:- extends modread_ctx.
do_use_package([]) :- !.
do_use_package([PackageSpec|PackageSpecs]) :- !,
        do_use_package(PackageSpec),
        do_use_package(PackageSpecs).
do_use_package(PackageSpec0) :-
	PackageSpec = ~find_package(PackageSpec0),
	treat_include(mod_package, PackageSpec).
}.

% ---------------------------------------------------------------------------
% include/1 - Include a file

decl__treatDom(include(_)).
decl__treat(include(Spec0)) :- !,
	Spec = ~find_source(Spec0),
        treat_include(mod_include, Spec).

% ===========================================================================

:- doc(section, "Declaration of Module/Class State, Imports, Exports, ...").
% TODO: Not the right name; module properties?

% ---------------------------------------------------------------------------
% Module/class state model

% TODO: put the kind inside
% Note: with '$raw_state', the 'instance_of__' method must be written
%       generated manually.
decl__treatDom('$raw_state').
decl__treat('$raw_state') :-
	% TODO: emit error if not a class
	Module = ~def_envmod,
	Module.set_prop(raw_state).

% Different models for state encoding:
%
%   single:
%     The value is the state. No state transition.
%   pair:
%     The value is the state. Similar to a Kripke frame (<W,r>),
%     state transition systems, etc., where each method changes
%     the state.
%   (MISSING) mut ref:
%     The value is a reference (a name) relative to the global
%     mutable store. 
%   (MISSING) data ref:
%     The value is a reference (a name) relative to the global
%     predicate-db store. 
%       
{
:- fluid memo :: memoize.
check_module_statemodel(void) :- !. % TODO: What could it mean?
check_module_statemodel(single) :- !.
check_module_statemodel(pair) :- !.
check_module_statemodel(X) :- ft_error(bad_module_statemodel(X)).
}.

% TODO: infer from the state model of the fields
decl__treatDom('$statemodel'(_)).
decl__treat('$statemodel'(StateModel)) :-
	check_module_statemodel(StateModel),
	(~def_envmod).add_statemodel(StateModel).

decl__treatDom('$trust_statemodel'(_, _)).
decl__treat('$trust_statemodel'(/*QC*//*add qualification here*/Name, StateModel)) :-
	% TODO: map from Name to Id
	(~def_envmod).add_trust_module_statemodel(/*QC*/Name, StateModel).

% ---------------------------------------------------------------------------
% Module/class attributes

% TODO: normalize and check for errors before processing
% (MType: meta-type, for attributes and fluid vars)
symdecl__treatDom((attr _)) :- get_pragma(class_expand), !.
symdecl__treat((attr Name :: MType # _Doc)) :- !, do_field(Name, MType).
symdecl__treat((attr Name :: MType)) :- !, do_field(Name, MType).
symdecl__treat((attr Name)) :- !, do_field(Name, any).
symdecl__symspec((attr Name :: _ # _), Name) :- !.
symdecl__symspec((attr Name :: _), Name) :- !.
symdecl__symspec((attr Name), Name) :- !.

{
:- extends modread_ctx.
do_field(FieldName, MType) :-
	% TODO: emit error if not a class
	% TODO: mtype is ignored in JS backend
	Module = ~def_envmod,
	Module.add1_field(FieldName, MType).
}.

% ---------------------------------------------------------------------------
% Import/(re)export symbols from/to modules/classes

decl__treatDom(use_module(_, _)).
decl__treat(use_module(UsedSpec0,Imports)) :- !,
	UsedSpec = ~find_source(UsedSpec0),
        do_using(mod_static, UsedSpec, Imports).

decl__treatDom(use_module(_)).
decl__treat(use_module(UsedSpec0)) :- !,
        UsedSpec = ~find_source(UsedSpec0),
	do_using(mod_static, UsedSpec, all).

decl__treatDom(use_class(_, _)) :- get_pragma(class_expand), !.
decl__treat(use_class(UsedSpec0, Imports)) :- !,
        UsedSpec = ~find_source(UsedSpec0),
	do_using(mod_class, UsedSpec, Imports).

decl__treatDom(use_class(_)) :- get_pragma(class_expand), !.
decl__treat(use_class(UsedSpec0)) :- !,
        UsedSpec = ~find_source(UsedSpec0),
	do_using(mod_class, UsedSpec, all).

decl__treatDom(ensure_loaded(_)).
decl__treat(ensure_loaded(UsedSpec0)) :- !,
	UsedSpec = ~find_source(UsedSpec0),
	( nonvar(UsedSpec) ->
	    % TODO: use a nicer way to store this?
	    (~def_envmod).add1_should_be_usermod(UsedSpec)
	; true
	), 
        do_using(ensure_loaded, UsedSpec, []). % TODO: why []?

% import/2 - Import predicates from a module without checks
decl__treatDom(import(_, _)).
decl__treat(import(Module,Imports)) :- !,
        do_using(import, Module, Imports).

decl__treatDom(reexport(_)).
decl__treat(reexport(UsedSpec0)) :- !,
	UsedSpec = ~find_source(UsedSpec0),
        do_using(reexport, UsedSpec, all).

decl__treatDom(reexport(_, _)).
decl__treat(reexport(UsedSpec0,Preds)) :- !,
	UsedSpec = ~find_source(UsedSpec0),
	do_using(reexport, UsedSpec, Preds).

% export/1 - Export predicates or symbols
% NOTE: Not exactly like 'public'. This allows ':- export(_)'.
decl__treatDom(export(_)).
decl__treat(export(Exports)) :- !, do_export(Exports).

% ---------------------------------------------------------------------------
% Extension (inheritance or mixin inclusion) of (nested) modules
% TODO: use a different keyword for mixins?

decl__treatDom(extends(_)) :- get_pragma(class_expand), !.
decl__treat(extends(Base)) :- !,
	Module = ~envmod,
	trust(Module instance_of module_s),
	% TODO: check extending this kind of module is meaningful
	% (no checking is performed at this point, se we do not know the base)
	Module.add_extends(Base).

% ===========================================================================
% Blocks of ImProlog code

% TODO: use normal scopes and a module property (a-la '$all_static')

decl__treatDom('$improlog_begin').
decl__treat('$improlog_begin') :- !,
	modreadEnv.add_reading_improlog.

{
:- extends modread_ctx.
treat_improlog_sentence(Sentence) :-
	Sentence = sentence(RawData, _VNs, _Sings, _Ln0, _Ln1),
	( RawData = (:- '$improlog_end') ->
	    modreadEnv.del_reading_improlog
	; % TODO: not a good idea, merge representation
	  (~def_envmod).add_improlog_sentence(RawData)
	).
}.

% ===========================================================================

:- doc(section, "Compiler/system options and syntactic extensions for this module").

% op/3 - Define an operator
decl__treatDom(op(_, _, _)).
decl__treat(op(P, F, O)) :- !,
        ( flagcontext__do(op(P, F, O)) -> true % This can give errors
        ; true
	).

% set_prolog_flag/2 - Set a compilation flag
decl__treatDom(set_prolog_flag(_, _)).
decl__treat(set_prolog_flag(Flag, Value)) :- !,
        ( flagcontext__do(set_prolog_flag(Flag, Value)) ->
	    true
        ; warning_failed_decl(set_prolog_flag(Flag, Value))
        ).

% push_prolog_flag/2 - Push a compilation flag
decl__treatDom(push_prolog_flag(_, _)).
decl__treat(push_prolog_flag(Flag, Value)) :- !,
        ( flagcontext__do(push_prolog_flag(Flag, Value)) ->
	    true
        ; warning_failed_decl(push_prolog_flag(Flag, Value))
        ).

% pop_prolog_flag/1 - Pop a compilation flag
decl__treatDom(pop_prolog_flag(_)).
decl__treat(pop_prolog_flag(Flag)) :- !,
        ( flagcontext__do(pop_prolog_flag(Flag)) ->
	    true
        ; warning_failed_decl(pop_prolog_flag(Flag))
        ).

% TODO: use 'prolog_flag' instead of 'pragma'?
decl__treatDom('$pragma'(_)).
decl__treat('$pragma'(Pragma)) :- !,
	( valid_pragma(Pragma) ->
	    (~def_envmod).add1_pragma(Pragma)
	; warning_failed_decl('$pragma'(Pragma))
	).

:- if(use_backend(bc)).
% TODO: declare in source?
valid_pragma(_).
:- elif(use_backend(js)).
% TODO: declare in source?
valid_pragma(shell_exec).
valid_pragma(class_expand).
valid_pragma(functional_expand).
valid_pragma(allow_js_lang).
% Undeclared terms that are not predicate symbols are usermod terms 
valid_pragma(user_term_on_undefined).
valid_pragma(target_platform(_)). % (for code generation options)
:- endif.

% ===========================================================================

:- doc(section, "Debugger support").

% '$insert_debug_info'/0 - include debug info in each predicate
decl__treatDom('$insert_debug_info').
decl__treat('$insert_debug_info') :-
	Module = ~def_envmod,
	Module.add1_pragma(insert_debug_info),
	MId = ~Module.get_name, % TODO: use an 'interface' for this
        envmod.add_clause('$mod_srcdbg'(MId), true).

% ===========================================================================

:- doc(section, "Functional expansion (extension)").

% TODO: It is not easy making 'fun_eval' exportable. The biggest
%   problem is that 'M:foo' cannot be cheaply evaluated if 'M' is 
%   unknown and 'foo/0' has the 'fun_eval' property.
  
decl__treatDom(fun_eval(_Spec)) :- get_pragma(functional_expand), !.
decl__treat(fun_eval(Spec)) :- !,
	Module = ~def_envmod,
        ( Spec = F/A, atom(F), number(A) ->
            Module.add1_fun_eval(F, A)
        ; Spec = arith(Bool), ( Bool == true ; Bool == false ) ->
            Module.set_eval_arith(Bool)
        ; ft_error(bad_fun_eval_spec(Spec))
        ).

% ===========================================================================

:- doc(section, "Forbid/allow (re)definition of some symbols").

% TODO: Those are not exactly symbol modifiers, since no predicate
%   is actually defined.

% '$forbid_def'/1 - internal declaration
% Forbid the declaration of a predicate (any clause with the same name
% will raise an error)
% TODO: This includes F/A in the 'not definable' set. (<- check)
symmodif__def('$forbid_def').
symmodif__set('$forbid_def', F/A) :-
	(~def_envmod).set_definable(F, A, no).

% '$allow_def'/1 - Allow the declaration of a previously forbidden predicate
% TODO: This removes F/A from the 'not definable' set. (<- check)
symmodif__def('$allow_def').
symmodif__set('$allow_def', F/A) :-
	(~def_envmod).set_definable(F, A, yes).

% ===========================================================================

:- doc(section, "Goals for conditional code").

:- use_module(compiler(frontend), [compiler_version/1]). % TODO: move compiler_version/1 to other file?

cond__evalDom(current_prolog_flag(_, _)) :- !.
cond__eval(current_prolog_flag(Flag, Value)) :- !,
	current_prolog_flag(Flag, Value).

cond__evalDom('$with_compiler_version'(_)).
cond__eval('$with_compiler_version'(V)) :- !,
	frontend:compiler_version(V).

% (throw 'peval_unknown' exception if the goal cannot be evaluated at
% compile time)
cond__evalDom(backend(_)).
cond__eval(backend(B)) :- !,
	( atom(B) ->
	    opts__backend(B)
	; throw(peval_unknown) % TODO: not really, I could test later
	).

cond__evalDom(target_platform(_)).
cond__eval(target_platform(B)) :- !,
	( atom(B) ->
	    opts__platform(B)
	; throw(peval_unknown) % TODO: not really, I could test later
	).

{
:- extends modread_ctx.
:- if(use_backend(bc)).
% TODO: merge with compiler/frontend_ptojs.
opts__backend(c_based).
opts__platform(unknown).
:- elif(use_backend(js)).
opts__backend(js_backend).
opts__platform(B) :-
	( get_pragma(target_platform(B0)) ->
	    B = B0
	; fail %throw(peval_unknown)
	).
:- endif.
}.

% ---------------------------------------------------------------------------

cond__evalDom(defined(_)) :- !.
cond__eval(defined(NA)) :- nonvar(NA), NA = N/A, atom(N), number(A), !,
	modreadEnv.get_condcomp_def(N, A).
cond__eval(defined(N)) :- atom(N), !,
	modreadEnv.get_condcomp_def(N, 0).

cond__evalDom(X) :- functor(X, F, A), modreadEnv.get_condcomp_def(F, A), !.
cond__eval(X) :-
	modreadEnv.get_condcomp_fact(X), !. % TODO: always cut?

% compilation_fact/1 - add a compilation fact (for condcomp conditions)
% TODO: too verbose?	
% TODO: missing error checks 
decl__treatDom(compilation_fact(_)) :- !.
decl__treat(compilation_fact(Fact)) :- !,
	functor(Fact, F, A),
	modreadEnv.add1_condcomp_def(F, A),
	modreadEnv.add_condcomp_fact(Fact).

% ===========================================================================

:- doc(section, "Clause sanity checks").

% TODO: this should be a package (see the source)
:- include(compiler(clause_check)).

