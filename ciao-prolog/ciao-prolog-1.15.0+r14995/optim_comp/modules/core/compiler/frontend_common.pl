:- doc(title, "Compiler front-end").
% This is the compiler front end for the (extensible) Ciao language.

% TODO: This part also seems to contains part of the middle-end
%   (analysis).

% TODO: Invalid module name stops compilation without showing any
%   error. Fix that.

:- use_module(compiler(memoize)).
:- '$trust_statemodel'(memoize, single).
:- use_module(compiler(errlog)).
:- use_module(compiler(store)).

:- use_module(library(lists)).
:- use_module(compiler(list_common)).

:- if(use_backend(js)).
% TODO: do not include interfaces!
:- include(compiler(psymbol__interface)).
:- endif.

% ===========================================================================
:- doc(section, "The current compiler version").

% It can be used in conditional code to test incremental changes in
% the compiler source without intermediate compiler promotions:
%
%   :- if('$with_compiler_version'(VPlus1)).
%     << code using the new feature >>
%   :- else.
%     << previous code >>
%   :- endif.

:- if(use_backend(bc)).
:- public compiler_version/1.
compiler_version(108).
:- endif.

% ===========================================================================
:- doc(section, "Register actions in 'memoize' (for incremental compilation)").
% TODO: Missing for JS backend.

:- if(use_backend(bc)).
:- include(.(memoize__callback)).

% ---------------------------------------------------------------------------
:- doc(subsection, "Action for 'split' part").

% TODO: Rename the 'split' output as 'split__deps'; make it explicit
action__input(split(Spec), [prolog_source(Spec)|InElements0]) :-
	memo.last_value(split(Spec), Deps),
	% From module_deps, obtain the list of input elements that
	% where used to generate the existing output. Fail if
	% recompilation is necessary.
	trust(Deps instance_of module_deps),
	ModuleName = ~Deps.defines_module,
	get_options(Spec, ModuleName, Opts),
	( Deps.options(Opts2) -> true ; Opts2 = [] ),
	( Opts = Opts2 -> % TODO: Missing: implement 'equivalent_options' (e.g., sort the lists)
	    % options have not changed	
	    ISpecs = ~Deps.include__list,
	    maplist((''(ISpec, InElement) :-
              InElement = prolog_package(ISpec)
            ), ISpecs, InElements0),
	    Ok = yes
	; Ok = no
	),
	'$inst_destroy'(Deps),
	Ok = yes.
action__output(split__src(Spec), split(Spec)).
action__output(split__itf(Spec), split(Spec)).
action__do(split(Spec), Ret) :-
	split(Spec, Ret).

:- include(compiler(store__callback)).

% syntactically preprocessed source code of a module
filetype__ext(split__src, '.pli').
filetype__kind(split__src, portable_object).
% public interface of a module
filetype__ext(split__itf, '.itf').
filetype__kind(split__itf, portable_object).
% dependences of a module
filetype__ext(split, '.deps').
filetype__kind(split, portable_object).

% ---------------------------------------------------------------------------
:- doc(subsection, "Action for 'compile' part").

:- use_module(compiler(module_ideps)).

% TODO: action__input could be lazy (at least checking if split(Spec)
%       is outdated before the rest of dependencies are loaded will
%       speed-up it a little bit; however, I believe that the
%       important case is checking that some code does not need
%       recompilation.
action__input(compile(Spec), [split(Spec)|InElements0]) :-
	memo.last_value(expand__ideps(Spec), IDeps),
	trust(IDeps instance_of module_ideps),
	IMods = ~IDeps.imported__keys,
	maplist((''(IMod, InElement) :-
          IDeps.imported(IMod, ISpec),
	  InElement = split(ISpec)
	), IMods, InElements0),
	'$inst_destroy'(IDeps).
action__output(compile__emu(Spec), compile(Spec)).
action__output(compile__c(Spec), compile(Spec)).
action__output(expand__ideps(Spec), compile(Spec)).
action__output(expand__sym(Spec), compile(Spec)).
action__do(compile(Spec), Ret) :-
	compile(Spec, Ret).

% (make sure that compilation output files are defined)
:- use_module(compiler(frontend__outfiles)).

:- elif(use_backend(js)).
% ===========================================================================
% TODO: only in compiler/frontend_ptojs.pl, because we don't use 'memo' yet.
{
:- fluid memo :: memoize.
:- public meta_predicate eval_split_mod(?, out(module_s)).
eval_split_mod(Spec) := ModuleR :-
	% TODO: used modules are nested into 'root' (to make
	%   hierarchical, allow part of the lib path being reflected
	%   here).
	RootModule = ~module_s.lookup_module(root),
	store:spec_to_default_module(Spec, Mod),
	ModuleR = ~RootModule.query_nested_module(Mod),
	( ModuleR.get_prop(status(_)) ->
	    % The module has already been read, do nothing
	    true
	; ModuleR.set_prop(status(reading)),
	  _ = ~split(Spec),
	  ModuleR.set_prop(status(ok))
	).
}.
:- endif.

% ===========================================================================
:- doc(section, "Definition of modules").

:- if(use_backend(bc)).
:- use_module(compiler(module_pli)).
:- '$trust_statemodel'(module_pli, single).
% TODO: necessary?
:- use_module(compiler(module_exp)).
:- '$trust_statemodel'(module_exp, single).
:- elif(use_backend(js)).
:- use_module(compiler(module_jsexp_)).
% TODO: BUG - import necessary for module_jsexp_
:- use_module(compiler(cdic_)). :- '$trust_statemodel'(cdic, pair).
:- endif.

% ---------------------------------------------------------------------------	

% New top module associated to a library @var{Spec}
:- if(use_backend(bc)).
new_top_module(Spec) := ModuleR :-
	ModuleR = ~module_s.new,
	~ModuleR.mod_spec = Spec.
:- elif(use_backend(js)).
% TODO: This is wrong, you should not use the 'root' module
% TODO: Do not use 'Mod' as identifier (e.g., you may be reading a
%   user module). The module 'name' should not be fixed at this point
%   (see check_module_name, etc. later)
new_top_module(Spec) := ModuleR :-
	store:spec_to_default_module(Spec, Mod),
	%
	% TODO: Do not register here, use memo.
	RootR = ~module_s.lookup_module(root),
	( RootR.get_prop(file_spec(_)) ->
	    % TODO: Kludge, not the first module
	    OtherMod = yes
	; % TODO: Same file_spec than ModuleR?
	  RootR.set_prop(file_spec(Spec)),
	  RootR.set_prop(static_noself),
	  RootR.add_extends(static_base),
	  OtherMod = no
	),
	ModuleR = ~RootR.query_nested_module(Mod),
	ModuleR.set_prop(file_spec(Spec)),
	%
	( OtherMod = yes ->
	    true
	; RootR.add_using(mod_static, Spec), % TODO: Necessary?
	  RootR.add_import(Mod) % TODO: Necessary?
	).
:- endif.

% ---------------------------------------------------------------------------	

:- meta_predicate module_to_id(module_s, ?).
% Transform from module_s to cheap identifiers that can be asserted
% TODO: current objects may be really heavy, find a better solution
:- if(use_backend(bc)).
module_to_id(Module) := ~Module.get_id.
:- elif(use_backend(js)).
module_to_id(X) := X.
:- endif.

% ---------------------------------------------------------------------------
:- doc(subsection, "Global and per-module options").

:- include(compiler(options__interface)).
get_options(Spec, Module, Opts) :-
	( global__options(Opts1) -> true ; Opts1 = [] ),
	( module__options(Spec, Module, Opts2) -> true ; Opts2 = [] ),
	append(Opts1, Opts2, Opts).

% ===========================================================================
:- doc(section, "Front-end compilation environment").
% Environment information for the module processing front-end.

:- mixin errlog_ctx {
    % Require 'memo', just for the capability of reporting errors
    % TODO: 'errs' could be enough
    :- fluid memo :: memoize.
}.

{
:- extends errlog_ctx.
:- meta_predicate get_errs(out(errlog)).
get_errs := ~memo.errs.
}.

:- mixin memo_ctx {
    % Require 'memo'
    % TODO: also implies errlog_ctx
    :- fluid memo :: memoize.
}.

:- mixin modpass_ctx {
    % Environment for compilation pass over a module
    :- extends memo_ctx.
    :- fluid envmod :: module_s.
    % :- pred envmod(M) # "@var{M} is the environment module. The
    %    predicates defined in this module will have it the resolution
    %    scope for symbol binding.  var{M} can be an open-anonymous
    %    nested module. In such case, predicates will be automatically
    %    registered in @pred{def_envmod} and this module will be used as
    %    definition".
}.

:- mixin modread_ctx {
    % Environment for reading (and postprocessing) of a module
    :- extends modpass_ctx.
    :- fluid modreadEnv :: modread_env.
}.
% In a 'errlog_ctx' context, create a new modread_ctx
% TODO: (it should not be necessary)
:- '$def_binder'(new_modread_ctx(M), (
    envmod :: module_s <- M,
    modreadEnv :: modread_env <- ~modread_env.from_db(M)
)).
% In a 'modread_ctx' context, selects a new 'envmod'.
:- '$def_binder'(switch_envmod(M), (
    envmod :: module_s <- M
)).

:- class modread_env {
    % Status for module reading (shared between a top module and all
    % its nested modules)
    :- attr id :: any.

    :- constructor from_db_/1.
    % TODO: Allow 'meta_predicate' for 'constructor'.
    from_db_(M) :-
%        display(user_error, fdb(M)), nl(user_error),
        trust(M instance_of module_s),
	~id = ~spec_to_key(~M.mod_spec).

    % (from compiler/frontend_core)
    :- public get_reading_improlog/0.
    get_reading_improlog :- current_fact(reading_improlog(~id)).
    :- public add_reading_improlog/0.
    add_reading_improlog :- assertz_fact(reading_improlog(~id)).
    :- public del_reading_improlog/0.
    del_reading_improlog :- retractall_fact(reading_improlog(~id)).
    % (from compiler/frontend_core)
    :- public get_condcomp_def/2.
    get_condcomp_def(A,B) :- current_fact(condcomp_def(A,B,~id)).
    :- public add1_condcomp_def/2.
    add1_condcomp_def(A,B) :-
        Id = ~id,
        ( current_fact(condcomp_def(A,B,Id)) -> true
	; assertz_fact(condcomp_def(A,B,Id))
	).
    :- public get_condcomp_fact/1.
    get_condcomp_fact(A) :- current_fact(condcomp_fact(A,~id)).
    :- public add_condcomp_fact/1.
    add_condcomp_fact(A) :- assertz_fact(condcomp_fact(A,~id)).
    % (from clause_check)
    % :- pred discontiguous(PredId).
    :- public get_discontiguous/1.
    get_discontiguous(A) :- current_fact(discontiguous(A,~id)).
    :- public add1_discontiguous/1.
    add1_discontiguous(A) :-
        Id = ~id,
        ( current_fact(discontiguous(A,Id)) -> true
	; assertz_fact(discontiguous(A,Id))
	).
    % :- pred latest_read_pred(PredId) # "The lastest read clause was of predicate @var{PredId}".
    :- public get_latest_read_pred/1.
    get_latest_read_pred(A) :- current_fact(latest_read_pred(A,~id)).
    :- public add_latest_read_pred/1.
    add_latest_read_pred(A) :- assertz_fact(latest_read_pred(A,~id)).
    :- public del_latest_read_pred/1.
    del_latest_read_pred(A) :- retractall_fact(latest_read_pred(A,~id)).

    :- public clear/0.
    clear :-
        Id = ~id,
	retractall_fact(reading_improlog(Id)),
	retractall_fact(condcomp_def(_,_,Id)),
	retractall_fact(condcomp_fact(_,Id)),
	retractall_fact(discontiguous(_,Id)),
	retractall_fact(latest_read_pred(_,Id)).
}.
% (from compiler/frontend_core)
:- data reading_improlog/1.
% (from compiler/frontend_core)
:- data condcomp_def/3.
:- data condcomp_fact/2.
% (from clause_check)
:- data discontiguous/2.
:- data latest_read_pred/2.

{
:- extends modread_ctx.
:- meta_predicate top_envmod(out(module_s)).
top_envmod := ~envmod.top_enclosing_module.
}.

{
:- extends modread_ctx.
:- meta_predicate def_envmod(out(module_s)).
% TODO: use only envmod, introduce find_def_enclosing in some operations? (not sure)
%       (this could later be removed by really opening the smaller nested modules)
% :- pred def_envmod(M) # "@var{M} is the @em{definition environment
%    module}. All definitions under this scope will be registered in
%    this module. It is calculated as the nearest enclosing
%    (non-anonymous) module from ~envmod."
def_envmod := ~envmod.find_def_enclosing.
}.

{
:- extends modread_ctx.
% Pragma
get_pragma(X) :-
	Module = ~def_envmod, % TODO: use enclosing_star
	Module.pragma(X).
}.

{
% pred_ref for ~envmod
% TODO: make this the default one?
:- extends modread_ctx.
:- meta_predicate pred_ref_ac(?, ?, out(predicate_s)).
pred_ref_ac(F, A) := ~envmod.pred_ref(F, A).
}.

{
% pred_ref_noreg for ~def_envmod
% TODO: can it be envmod?
% TODO: make this the default one?
:- extends modread_ctx.
:- meta_predicate pred_ref_noreg_ac(?, ?, out(predicate_s)).
pred_ref_noreg_ac(F, A) := ~((~def_envmod).pred_ref_noreg(F, A)).
}.

% ---------------------------------------------------------------------------	

% TODO: Move to module_common.pl
:- public meta_predicate enum_nested_star(module_s, out(module_s)).
% Obtain the post-order traversal (childs first, root later) of Module
% and all its nested modules.
%
% From @var{Module0}, we can reach @var{Module} using nested* (0 or more
% steps in the nested relation). 
enum_nested_star(Module0) := Module :-
	% Traverse nested modules
	Module = ~enum_nested_star(~Module0.get_nested_module(_)).
enum_nested_star(Module) := Module. % or return itself

:- if(use_backend(bc)).
:- public meta_predicate enum_nested_star_itf(module_s_itf, out(module_s_itf)).
% (equivalent for module_s_itf)
enum_nested_star_itf(Module0) := Module :-
	% Traverse nested modules
	Module = ~enum_nested_star_itf(~Module0.get_nested_module(_)).
enum_nested_star_itf(Module) := Module. % or return itself
:- endif.

% ---------------------------------------------------------------------------	

{
:- extends modread_ctx.
:- if(use_backend(bc)).
% TODO: missing
mod_set_partial(_, _, _, _).
:- elif(use_backend(js)).
mod_set_partial(F, A, G, B) :-
	P is A - B, % missing arguments
	functor(PT, '', P), PT =.. [_|PXs],
	functor(GT, G, B), GT =.. [_|GXs],
	append(GXs, PXs, FXs),
	FT =.. [F|FXs],
	do_predabs(GT, PXs, FT).

do_predabs(PartialHead, AppliedArgs, FullHead) :-
        functor(PartialHead, N, A),
	FunctorR = ~pred_ref_ac(N, A),
        ( FunctorR.needs_self ->
	    % Add extra argument for Self (if we are defining a method)
	    PartialHead =.. [_|As],
	    As2 = [Self|As],
	    PartDef = method(Self, FullHead)
	; PartialHead =.. [_|As2],
	  PartDef = not_method(FullHead)
	),
	FunctorR.set_prop(specialdef(As2, partial_def(AppliedArgs, PartDef))),
	FunctorR.set_code(icode(a, As2, or([['\6\predid_apply'(~predicate_s.from_id('$nodef/0'), [])]]))).
:- endif.
}.

{
:- extends modread_ctx.
:- if(use_backend(bc)).
% TODO: missing
mod_set_local(_, _).
:- elif(use_backend(js)).
mod_set_local(N, A) :-
	Module = ~def_envmod,
        _ = ~Module.local_functor_ref(N, A).
:- endif.
}.

% ---------------------------------------------------------------------------

{
:- extends modread_ctx.
:- if(use_backend(bc)).
do_constructor(Module, F0, A) :-
	trust(Module instance_of module_s),
	% TODO: emit error if not a class
	( atom_concat(FunName, '_', F0) ->
	    % TODO: is there a better way? I would not like to create a new symbol
	    Module.add1_constr(F0, A, FunName)
	; ft_error(bad_class_ctor_name(~Module.get_id, F0, A))
	).
:- elif(use_backend(js)).
% TODO: missing
do_constructor(_, _, _).
:- endif.
}.

% ---------------------------------------------------------------------------

% TODO: 'do_using' and 'extends' are similar
% TODO: missing use_class/2, use_class/1, for use_backend(bc)
% TODO: Define the type for the first argument (it extends module modifiers)
{
:- extends modread_ctx.
do_using(import, ModuleName, Imports) :- !,
        do_import_nocheck(ModuleName, Imports).
:- if(use_backend(bc)).
do_using(ensure_loaded, Spec, Imports) :-
	( nonvar(Spec) ->
	    % TODO: use a nicer way to store this?
	    Module = ~def_envmod,
	    Module.add1_should_be_usermod(Spec)
	; true
	), 
        do_import(false, mod_static, Spec, Imports).
:- elif(use_backend(js)).
% TODO: 'mod_include' files are processed several times; 
%       we should at least prevent recursive includes
% TODO: _Imports is ignored
do_using(ensure_loaded, _, _) :- !,
	trace(ensure_loaded_not_implemented).
:- endif.
do_using(reexport, Spec, Imports) :- !,
        do_import(true, mod_static, Spec, Imports).
do_using(Modifier, Spec, Imports) :-
        do_import(false, Modifier, Spec, Imports).
}.

{
:- extends modread_ctx.
% Use a module of the expected module @var{Modifier}.
:- if(use_backend(bc)).
do_import(Reexport, mod_static, UsedSpec, Imports) :-
	do_reexport :: any <- Reexport,
	imported_spec :: any <- UsedSpec,
	( var(UsedSpec) ->
	    ft_error(bad_use_module(UsedSpec))
	; true
	),
	do_import_(Imports).
{
:- fluid do_reexport :: any.
:- fluid imported_spec :: any.

% TODO: poor indexing of add1(~imported_spec), use unique identifiers for 'envmod.mod_spec'
do_import_(Imports) :-
	Module = ~def_envmod,
	( store:spec_equal(~imported_spec, ~Module.mod_spec) ->
	    true
	; Module.add1_import(~imported_spec),
	  ( ~do_reexport = true -> Module.add1_reexport(~imported_spec) ; true ),
	  store_imports(Imports)
        ).

store_imports(all) :- !,
	Module = ~def_envmod,
	( ~do_reexport = true -> Module.add_reexports_all(~imported_spec) ; true ),
	Module.add_imports_all(~imported_spec).
store_imports(Imports) :-
        store_import_list(Imports).

store_import_list([I|Is]) :- !,
        store_import(I),
        store_import_list(Is).
store_import_list([]) :- !.
store_import_list(Bad) :-
        ft_error(bad_import_list(Bad)).

store_import(F/A) :- atom(F), integer(A), !,
	Module = ~def_envmod,
	( ~do_reexport = true ->
	    Module.add_reexports(~imported_spec, F, A)
	; true
	),
	Module.add_imports(~imported_spec, F, A).
store_import(Bad) :-
        ft_error(bad_import_spec(Bad)).
}.
:- elif(use_backend(js)).
do_import(Reexport, Modifier, Spec, Imports) :- \+ Imports = all, !,
	trace(using_prj_not_implemented(Modifier, Spec, Imports)),
	do_import(Reexport, Modifier, Spec, all). % TODO: not correct
do_import(_Reexport, Modifier, Spec, _Imports) :-
	Module = ~def_envmod,
        Module.get_using(Modifier, Spec), !. % already used in this context
do_import(_Reexport, Modifier, Spec, _Imports) :-
	Module = ~def_envmod,
	Module.add_using(Modifier, Spec),
	( ( Modifier = mod_static ; Modifier = mod_class ) ->
	    true
	; throw(bad_using(Modifier, Spec))
	),
	ModuleR = ~eval_split_mod(Spec),
	FoundModifier = ( ModuleR.get_prop(static) ? mod_static | mod_class ),
	%
	( Modifier = FoundModifier -> true
	; ft_error(module_modifier_mismatch(Modifier, FoundModifier))
	),
	% TODO: check 'Modifier' later (much later)
	store:spec_to_default_module(Spec, Mod),
	Module.add_import(Mod).
:- endif.
}.

{
:- extends modread_ctx.
:- if(use_backend(bc)).
do_import_nocheck(Module, Imports) :-
        ( atom(Module) ->
	    true
	; ft_error(bad_import(Module))
	),
	mod :: any <- Module,
        store_import_nocheck_list(Imports).
{
:- fluid mod :: any.

store_import_nocheck_list([I|Is]) :- !,
        store_import_nocheck(I),
        store_import_nocheck_list(Is).
store_import_nocheck_list([]) :- !.
store_import_nocheck_list(Bad) :-
        ft_error(bad_import_list(Bad)).

store_import_nocheck(F/A) :- atom(F), integer(A), !,
	% TODO: poor indexing
	Module = ~def_envmod,
	Module.add1_imports_nocheck(~mod, F, A).
store_import_nocheck(Bad) :-
        ft_error(bad_import_spec(Bad)).
}.
:- elif(use_backend(js)).
% TODO: missing
do_import_nocheck(_, _) :-
	trace(import_not_implemented).
:- endif.
}.

% ---------------------------------------------------------------------------

{
:- extends modread_ctx.
do_export(All) :- var(All), !,
	Module = ~def_envmod,
        Module.set_prop(exports_all).
do_export([Exp|Exports]) :- !,
        do_export__2(Exp),
        do_export(Exports).
do_export([]) :- !.
do_export(Exp) :-
        do_export__2(Exp).

do_export__2(F) :- atom(F), !, % export other symbol (modules or attributes)
	Module = ~def_envmod,
	Module.add1_exported_sym(F).
do_export__2(F0/A) :- atom(F0), integer(A), !,
	% TODO: Define add_export_pred, unresolved; add 'exported' property later
	Pred = ~pred_ref_ac(F0, A),
        Pred.set_prop(exported).
do_export__2(SymSpec) :-
        ft_error(bad_export(SymSpec)).
}.

% ---------------------------------------------------------------------------	
:- doc(subsection, "Local (pli) resolution of classes (for flatmod_expansion)").

:- if(use_backend(bc)).
% TODO: we should not do any resolution at this step (it should be delayed)
{
:- extends modread_ctx.
pli_resolve_classes([]) := [].
pli_resolve_classes([C|Cs]) := [~pli_resolve_class(C) | ~pli_resolve_classes(Cs)].

:- meta_predicate pli_resolve_class(?, out(module_s)).
pli_resolve_class(Name) := Module :-
	Module00 = ~top_envmod, % TODO: use enclosing_star + find, not nested_star + find
	Module0 = ~Module00.nested_star_find_class(Name),
	!,
	Module = Module0.
pli_resolve_class(Name) := _ :-
	% TODO: problem: all this code should be done later; it cannot resolve imported classes with this definition
	ft_error(bug(cannot_resolve_class_locally(Name))),
	fail.

:- meta_predicate pli_resolve_class_dyn(?, out(module_s)).
pli_resolve_class_dyn(Name) := Module :-
	Module = ~pli_resolve_class(Name),
	( Module.is_class_or_interface ->
	    true
	; ft_error(bug(resolved_is_not_class(Name, ~Module.get_id)))
	).
}.
:- elif(use_backend(js)).
% TODO: missing
:- endif.

% ---------------------------------------------------------------------------

:- if(use_backend(bc)).
% Environment information for the module compilation.
:- mixin modcomp_ctx {
    % Environment for compilation of a module
    :- fluid modcompEnv :: modcomp_env.
}.

:- use_module(compiler(module_deps)).
:- '$trust_statemodel'(module_deps, single).
:- use_module(compiler(module_itf)).
:- '$trust_statemodel'(module_itf, single).
:- use_module(compiler(module_pli)).
:- '$trust_statemodel'(module_pli, single).
:- use_module(compiler(module_sym)).
:- '$trust_statemodel'(module_sym, single).
:- use_module(compiler(module_ideps)).
:- '$trust_statemodel'(module_ideps, single).
:- use_module(compiler(module_exp)).
:- '$trust_statemodel'(module_exp, single).
:- use_module(compiler(module_bin)).

:- '$trust_statemodel'(modcomp_env, single). % TODO: This should not be necessary
:- class modcomp_env {
    :- attr id :: any.

    :- attr spec :: any.
    :- attr pli :: module_pli.
    :- attr itf :: module_itf.
    :- attr deps :: module_deps.
    :- attr exp :: module_exp.
    :- attr sym :: module_sym.
    :- attr ideps :: module_ideps.
    :- attr mod_itf :: u_dic. % (private)

    :- constructor new_/1.
    new_(Spec) :-
        ~spec = Spec,
	~id = ~spec_to_key(Spec).

    clear :-
        Id = ~id,
        retractall_fact(has_mod_info(_,Id)),
        retractall_fact(uses_runtime_data(Id)).
}.
:- data has_mod_info/2.
:- pred uses_runtime_data/1 # "The module does runtime expansions (and
   thus it needs some predicate info at runtime)".
:- data uses_runtime_data/1.

{
:- extends modcomp_ctx.
% TODO: Use a dict?
% the dictionary of itfs
:- meta_predicate get_mod_itf(?, out(module_itf)).
get_mod_itf(Module) := ~modcompEnv.mod_itf.get(Module).
set_mod_itf(Module, Itf) :-
	Id = ~modcompEnv.id,
	assertz_fact(has_mod_info(Module, Id)),
	modcompEnv.mod_itf.lookup(Module, Itf).
mod_itf__keys := Modules :-
	Id = ~modcompEnv.id,
	findall(Module, has_mod_info(Module, Id), Modules).
}.

{
:- extends modcomp_ctx.
get_uses_runtime_data :-
	Id = ~modcompEnv.id,
        uses_runtime_data(Id).
del_uses_runtime_data :-
	Id = ~modcompEnv.id,
        retractall_fact(uses_runtime_data(Id)).
mark_uses_runtime_data :-
	Id = ~modcompEnv.id,
	( uses_runtime_data(Id) ->
	    true
	; assertz_fact(uses_runtime_data(Id))
	).
}.
:- elif(use_backend(js)).
:- endif.

% ===========================================================================
:- doc(section, "Static callbacks (for compatibility with translations)").
% NOTE: Translations should run in different contexts. 

% TODO: uses logical global variables
:- use_module(engine(internals), ['$global_vars_set'/2, '$global_vars_get'/2]).

% TODO: deprecate?
:- public callback__splitting_module/1.
callback__splitting_module(Name) :-
	ModuleR = ~static_mod,
	% TODO: is correct not duplicating it? 
	Name = ~ModuleR.get_name.

% TODO: This can only be called from hooks called from the 'split' part
:- public meta_predicate callback__add_module_check(pred(1)).
:- pred callback__add_module_check(Pred)
        # "Used by code expanders (loaded with
           @decl{load_compilation_module/1} declarations) to provide
           additional checks to be executed before the current file is
           compiled.  The predicate is called with the base name of the file
           processed as the first argument, and all solutions will be found.
           A fact @pred{module_error/0} can be asserted if a condition
           which should stop the compilation is found.".
callback__add_module_check(Pred) :-
	ModuleR = ~static_mod,
	% TODO: is correct not duplicating it? 
	ModuleR.add1_expansion_check(Pred).

:- public callback__error/0.
callback__error :-
	Errs = ~static_errs,
	Errs.add_module_error.

% TODO: document: this predicate must be used by syntactic expansions to report errors
:- public callback__error/1.
callback__error(Error) :-
	Errs = ~static_errs,
	Errs.compiler_error(Error).

:- public callback__loc/3.
callback__loc(Src, Ln0, Ln1) :-
	static_errs(Errs),
	Errs.get1_loc([], Loc),
	Loc = loc(_, Src, Ln0, Ln1).

:- meta_predicate static_errs(out(errlog)).
static_errs := Errs :-
	'$global_vars_get'(4, static_data(Errs, _ModuleR)),
	!.
static_errs(Errs) :-
	'$global_vars_get'(2, static_data(Memoize, ModcompEnv)),
	memo :: memoize <- Memoize,
	modcompEnv :: modcomp_env <- ModcompEnv,
	Errs = ~get_errs.

:- meta_predicate static_mod(out(module_s)).
static_mod := ModuleR :-
	'$global_vars_get'(4, static_data(_Errs, ModuleR)).

{
:- extends errlog_ctx.
set_static_data(OldStaticData, ModuleR) :-
	'$global_vars_get'(4, OldStaticData),
	StaticData = static_data(~get_errs, ModuleR),
	'$global_vars_set'(4, StaticData).
}.

recover_static_data(OldStaticData) :-
	'$global_vars_set'(4, OldStaticData).

% TODO: Rename as callback__error.
:- public error/1.
:- redefining(error/1).
error(Error) :-
	'$global_vars_get'(3, Errs),
	trust(Errs instance_of errlog),
	Errs.compiler_error(Error).

{
:- extends errlog_ctx.
% A simple error
ft_error(Error) :- % TODO: rename by 'error/1'
	(~get_errs).compiler_error(Error).
}.

% ---------------------------------------------------------------------------

% TODO: This can only be called from hooks called from the 'compile' part
:- public callback__ensure_imported/4.
callback__ensure_imported(_ModcompEnv, Module, F, A) :-
	% trust(ModcompEnv instance_of modcomp_env),
	ensure_imported__x(Module, F, A).

% TODO: remove the wrapper predicate
% TODO: use the same data that is used in module expansion (it's cheaper)
ensure_imported__x(Module, F, A) :-
	errlog:bug(['ensure_imported__x(', Module, ',', F, ',', A, ') is not working, halting']), halt. % TODO: fix!!!?!?!
ensure_imported__x(Module, F, A) :-
	static_errs(Errs),
	Errs.compiler_error(should_import(F, A, Module)).

% ===========================================================================
% TODO: only in compiler/frontend_ptojs.pl
% TODO: 'root' module should not exist and this should not be necessary

:- if(use_backend(bc)).
:- elif(use_backend(js)).
{
:- fluid memo :: memoize.
:- public meta_predicate prepare_root_module(?).
prepare_root_module(Mod) :-
	Ok = ( prepare_root_module_(Mod) ? yes | no ),
	Ok = yes.

prepare_root_module_(Mod) :-
	RootR = ~module_s.lookup_module(root),
	RootR.set_prop(static_noself),
	RootR.add_extends(static_base),
	%
	envmod :: module_s <- RootR,
	modreadEnv :: modread_env <- ~modread_env.from_db(RootR),
	%
	( define_root_module(Mod),
	  flatten_module(RootR) ->
	    Ok = yes
	; Ok = no
	),
	modreadEnv.clear,
	Ok = yes.
}.

{
:- extends modread_ctx.
define_root_module(Mod) :-
	% TODO: remove 'treat_decl', read as a module intead?
	treat_decl(use_package(nonpure), []),
	treat_decl(use_package([oo_syntax, hiord, assertions, fsyntax, mutables, string_type]), []),
	% TODO: including it here does not work, see 'nonpure.pl' and ptojs__compiler:compile_and_link/1
	% treat_decl(use_module(engine(internals)), []),
	%
	treat_clause('__call_main__', ~mcall(Mod, 'main'), [], []). % TODO: move to the linker
}.

:- endif.

% ===========================================================================
:- doc(section, "Driver for compilation passes").

% TODO: Write a more complete driver for compilation passes (rather than a hiord pred or binder)
{
:- extends modread_ctx.
module_pass(Pass) :-
        ( % (failure-driven loop)
	  M = ~enum_nested_star(~top_envmod),
	    % TODO: add the module as location; add location in definitions
	    (~get_errs).add_location('', ''),
	    modpass__do(Pass, M),
	    (~get_errs).del_location,
	    fail
	; true
	).
}.

% Handler for module compilation pass
:- discontiguous(modpass__do/2).
{
    :- extends modread_ctx.
    :- meta_predicate modpass__do(?, module_s).
    % TODO: '$props' is a trick to get the predicate defined in this scope (better use ':- pred')
    :- '$props'(modpass__do/2, []).
}.

% ===========================================================================
:- doc(section, "Module reading front-end").
% Read a module and postprocess it.

:- use_module(compiler(flagcontext)).
:- use_module(compiler(meta_syntax)).

% ---------------------------------------------------------------------------

{
:- if(use_backend(bc)).
:- extends memo_ctx.
:- elif(use_backend(js)).
% TODO: Try to avoid this.
:- fluid memo :: memoize.
:- public meta_predicate split(?, out(module_s)).
:- endif.

split(Spec, Ret) :-
	eval_file(prolog_source(Spec), PlName),
	split_(Spec, PlName, Ret).

split_(Spec, PlName, Ret) :-
	SplitModuleR = ~module_read_and_split(Spec, PlName), % TODO: better, return module here
        ( memo.errs.get1_module_error ->
	    % TODO: check compiler/frontend_ptojs (it is not cleaning nested modules)
	    clean_all(SplitModuleR),
	    fail
	; Ret = ~get_ret(SplitModuleR)
	).

% TODO: 'split' is not a good name (source -> ('itf' + 'pli'))
%:- meta_predicate module_read_and_split(?, ?, out(module_s)).
module_read_and_split(Spec, PlName) := SplitModuleR :-
	% Parse the file (processing declarations)
	Module = ~module_read(Spec, PlName),
	SplitModuleR = ~project_itf(Module).
}.

% ---------------------------------------------------------------------------
:- doc(subsection, "Read a module from a file").

{
:- extends memo_ctx.
:- meta_predicate module_read(?, ?, out(module_s)).
module_read(Spec, PlName) := ModuleR :-
	ModuleR = ~new_top_module(Spec),
	new_modread_ctx(ModuleR, module_read_(PlName)).
}.

{
:- extends modread_ctx.
module_read_(PlName) :-
	Errs = ~get_errs,
	% TODO: generalize this for nested modules
	Errs.add_src(PlName),
	set_static_data(OldStaticData, ~envmod), % TODO: hmmm...
	flagcontext__new(OldFlags), % TODO: 'push' for nested
	define_ops, % TODO: not for nested
	%
        process_file(PlName),
	treat_end_sentence, % (for expansion hooks) % TODO: do once per nested! (just when the nested has been read)
	modreadEnv.clear,
	recover_static_data(OldStaticData), % TODO: hmmm...
	flagcontext__switch(OldFlags), % TODO: 'pop' for nested
	Errs.del_src,
	% --
	Errs.add_src(PlName),
	split_module_passes,
	Errs.del_src.
}.

% ---------------------------------------------------------------------------
:- doc(subsection, "Read and process all the sentences from a file").

:- use_module(compiler(read_source)).

{
:- extends modread_ctx.
process_file(PlName) :-
	'$open'(PlName, r, Stream),
	process_source(file(Stream), yes),
	close(Stream),
	!.
process_file(_) :-
	% TODO: if this happens there is a compiler bug
	ft_error(bug(process_file_should_not_fail)).

process_source(Source, RequiredModuleDecl) :-
	errs :: errlog <- ~get_errs,
	( Source = sentences(Sentences) ->
	    % TODO: RequiredModuleDecl is ignored, it shouldn't
	    modread_ctx__from_list(Sentences, process_sentences)
	; Source = file(Stream) ->
	    modread_ctx__from_stream(Stream, process_sentences_(RequiredModuleDecl, _))
	; fail
	).
}.

{
:- extends modread_stream_ctx.
% Process the sentences in 'envmod'
:- if(use_backend(bc)).
process_sentences_(RequiredModuleDecl, RequiredModifier) :-
	RequiredModifier = mod_package,
	Module = ~def_envmod,
	Module.is_class_or_interface,
	!,
%	display(user_error, including_package_from_dyn(~Module.get_id)), nl(user_error),
	% Packages are included in an static open-anon block
	% (so that local definitions are defined 'static')
	Module2 = ~new_open_anon,
	Module2.set_prop(static_scope), % (special scope)
	switch_envmod(Module2, process_sentences__(RequiredModuleDecl, RequiredModifier)).
:- endif.
process_sentences_(RequiredModuleDecl, RequiredModifier) :-
        process_sentences__(RequiredModuleDecl, RequiredModifier).

process_sentences__(RequiredModuleDecl, RequiredModifier) :-
	skip_shell_lines,
	process_module_sentence(RequiredModuleDecl, RequiredModifier, Rest),
	( Rest = empty_file -> % empty file, no more sentences
	    true
	; treat_sentence_list(Rest),
	  process_sentences
	).
}.

:- include(compiler(process_sentences)).

{
:- fluid errs :: errlog.
process_sentences__error(Error) :-
	errs.compiler_error(Error).
}.	

% ---------------------------------------------------------------------------
:- doc(subsection, "Conditional code").

{
:- extends modread_ctx.
% Evaluation of conditional goals in (:- if(_)) directives
% (called from process_sentences/0)
process_cond(Cond, Status) :-
	catch(process_cond_(Cond, Status), peval_unknown, Status = unknown).

process_cond_(Cond) := peval_cond(Cond) ? true | fail.

% (throw 'peval_unknown' exception if the goal cannot be evaluated at
% compile time)
peval_cond(X) :- var(X), !, throw(peval_unknown).
peval_cond(X) :- cond__evalDom(X), !, cond__eval(X).
peval_cond((A, B)) :- !,
	peval_cond(A), peval_cond(B).
peval_cond((A ; B)) :- !,
	( peval_cond(A) ; peval_cond(B) ).
peval_cond((\+ A)) :-
	\+ peval_cond(A).
peval_cond(true) :- !, true.
peval_cond(fail) :- !, fail.
peval_cond(_) :- throw(peval_unknown).
}.

:- discontiguous cond__evalDom/1. % in the domain of cond__eval/1
:- discontiguous cond__eval/1. % evaluate goal
{
    :- extends modread_ctx.
    % Hooks for definition of handlers for conditional goals
    % TODO: '$props' is a trick to get the predicate defined in this scope (better use ':- pred')
    :- '$props'(cond__evalDom/1, []).
    :- '$props'(cond__eval/1, []).
}.

% TODO: missing module qualification
% TODO: merge with peval_goal from ptoc__impcomp
% TODO: define as trans_hooks?

% ---------------------------------------------------------------------------
:- doc(subsection, "Process module/class/etc. declaration").

{
:- extends modread_stream_ctx.
% Process and normalize the module declaration
% note: RequiredModifier may be uninstantiated
% TODO: it does not convince me...
process_module_sentence(no, _, []) :- !.
process_module_sentence(yes, RequiredModifier, Rest) :-
        read_module_decl(Decl, FoundModifier, Ln0, Ln1, Rest0),
	% TODO: duplicated (but for modules, not packages)
	( RequiredModifier = FoundModifier ->
	    true
	; ft_error(module_modifier_mismatch(RequiredModifier, FoundModifier))
	),
	( RequiredModifier = mod_package ->
	    Rest = Rest0
	; maybe_user_module(FoundModifier, Decl, Rest0, Decl2, Rest),
	  Decl2 = module(Name, Exports, Package0) ->
	    % We have read a module
	    errs.add_location(Ln0, Ln1),
	    RequiredModifier2 =
	      ( RequiredModifier = unknown ? mod_static % no module declaration
	      | RequiredModifier
	      ),
	    set_top_module_name_and_modifier(Name, RequiredModifier2),
	    normalize_package_list(Package0, Package),
	    get_options(~envmod.mod_spec, Name, Opts),
	    do_options(Opts),
	    do_use_package(Package),
            do_export(Exports),
	    errs.del_location
	; throw(bug_in_module_sentence)
	).
}.

{
:- extends modread_ctx.
% Transform 'unknown' into a 'user module'
% TODO: configure this
maybe_user_module(unknown, _, Rest0, Decl2, Rest) :- !,
	  % (a user module)
	  ( Rest0 = [S0], S0 = (:- Decl), Decl = use_package(Packages) ->
	      % keep this sentence for packages
	      % TODO: This is necessary to define 'pure' user modules;
	      %       does it make sense? It makes things more complex.
	      Rest = []
	  ; default_package(DefaultPackage),
	    Packages = DefaultPackage,
	    Rest = Rest0
	  ),
	  user_module_decl(Packages, _, Decl2).
maybe_user_module(_, Decl, Rest, Decl, Rest) :- !. % OK
}.

{
:- extends modread_stream_ctx.
% Read and normalize the module declaration.
% If there is no module declaration, 'Rest' contains the sentence that
% has been read.
read_module_decl(Decl, FoundModifier, Ln0, Ln1, Rest) :-
	( fetch_sentence(Sentence) ->
	    norm_module_decl(Sentence, Decl, FoundModifier, Ln0, Ln1, Rest)
	; % Empty file, no more sentences
	  % % TODO: Default package is not necessary here, right?
	  % default_package(DefaultPackage),
	  Ln0 = 1,
	  Ln1 = 1,
	  FoundModifier = unknown,
	  Decl = unknown,
	  Rest = empty_file
	  % Rest = empty_file,
	  % user_module_decl(DefaultPackage, FoundModifier, Decl)
	).
}.

{
:- extends modread_ctx.
% Normalize a module declaration to obtain the kind, list of exports,
% and list of packages.
norm_module_decl(Sentence, Decl2, FoundModifier, Ln0, Ln1, Rest) :-
	Sentence = sentence(S0, _, _, Ln0, Ln1),
	( S0 = (:- Decl), norm_module_decl_(Decl, FoundModifier, Decl2) ->
	    Rest = []
	; % No module nor class declaration found
	  FoundModifier = unknown,
	  Decl2 = unknown,
	  Rest = [Sentence]
	).

norm_module_decl_(Decl, FoundModifier, Decl2) :- mod__treatDom(Decl), !,
	mod__treat(Decl, FoundModifier, Decl2).
% Allow ":- <Pkg>(...)" as syntactic sugar for ":- module(..., [<Pkg>])".
norm_module_decl_(PackageDecl, mod_static, module(ModuleName, Exports, Package)) :-
	\+ is_basic_decl(PackageDecl),
        functor(PackageDecl, Package0, _),
        defines_a_package(Package0), !,
        ( arg(1, PackageDecl, ModuleName) -> true ; true ),
        ( arg(2, PackageDecl, Exports) ->
	    true
        ; Exports = []
        ),
        ( arg(3, PackageDecl, MorePackages) ->
            Package = [Package0|MorePackages]
        ; Package = Package0
        ).
}.

{
:- extends modread_ctx.
defines_a_package(Spec0) :-
	Spec = ~find_package(Spec0),
        store:addr(prolog_package(Spec), _Name).
}.

:- use_module(engine(rt_exp), 
	['$user_module_id'/1, '$new_user_module_id'/2]).

{
:- extends modread_ctx.
user_module_decl(Package, mod_static, module(M, _, Package)) :-
	Key = ~spec_to_key(~envmod.mod_spec),
	'$new_user_module_id'(Key, M).
}.

% TODO: 'pure' and '$purest' are not good names!
normalize_package_list(Package0, Package) :-
	( nonvar(Package0), select('$purest', Package0, Package1) ->
	    % does not even include the prelude
	    Package = Package1
	; nonvar(Package0), member(pure, Package0) -> 
	    Package = [prelude|Package0]
	; var(Package0) ->
	    Package = [prelude,nonpure]
	; Package = [prelude,nonpure|Package0]
	).

% ---------------------------------------------------------------------------

% Check and assign the module name
{
:- extends modread_ctx.
set_top_module_name_and_modifier(Name, Modifier) :-
	check_module_name(Name, Modifier),
	envmod.set_name_top(Name),
	envmod.set_modifier(Modifier).

check_module_name(Name, _) :-
        '$user_module_id'(Name), !. % (implies nonvar too)
check_module_name(Name, Modifier) :-
	SM = ~spec_to_default_module(~envmod.mod_spec),
	( var(Name) ->
	    % If Name is '_' in a module declaration, give a name
	    % automatically.
	    Name = SM
	; SM = Name ->
            true
	; ft_error(bad_module_decl_name(Modifier, SM, Name))
        ).
}.

% Store the options in the module (for checks for incremental
% compilation), and process them.
% TODO: They are plain sentences; do in other way?
{
:- extends modread_ctx.
do_options(Opts) :-
	envmod.add_options(Opts), % (remember them)
	do_options__2(Opts). % (process them)

do_options__2([]).
do_options__2([X|Xs]) :-
	treat_expanded_sentence(X, [], []),
	do_options__2(Xs).
}.

% ---------------------------------------------------------------------------
:- doc(subsection, "Treat language sentences").

{
:- extends modread_ctx.
treat_sentence_list([]).
treat_sentence_list([S|Ss]) :-
	( treat_sentence(S) -> true
	; bug(failed_sentence(S))
	),
	treat_sentence_list(Ss).

treat_sentence(Sentence) :- modreadEnv.get_reading_improlog, !,
	% TODO: Temporal hack to allow inlined ImProlog code.
	% TODO: Set as a pragma for a scope instead.
	treat_improlog_sentence(Sentence).
treat_sentence(Sentence) :-
	% Nested modules (and classes)
	Sentence = sentence(RawData, _VNs, _Sings, _Ln0, _Ln1),
	nonvar(RawData),
	RawData = (:- Decl),
        extract_symmodifs(Decl, SymModifs, Decl0),
	nested_decl(Decl0, Name, Keyword, Sentences),
	mod__nested(Keyword, Modifier),
	!,
	% TODO: do error checking (not all symmodif are valid for nested)
	apply_symmodifs_to_symspec(SymModifs, Name),
	treat_nested(Name, Modifier, Sentences).
treat_sentence(Sentence) :-
	Sentence = sentence(RawData, VNs, Sings, Ln0, Ln1),
	(~get_errs).add_location(Ln0, Ln1),
	syntr__expand_sentence(RawData, VNs, DataList),
	treat_expanded_sentence_list(DataList, VNs, Sings),
	(~get_errs).del_location.

% Invoke end-of-module sentence hook and process its output
treat_end_sentence :-
%	add_missing_classes,
	(~get_errs).add_location('', ''),
	syntr__expand_end(DataList),
	treat_expanded_sentence_list(DataList, [], []),
	(~get_errs).del_location.

treat_expanded_sentence_list([], _VNs, _Sings).
treat_expanded_sentence_list([S|Ss], VNs, Sings) :-
	treat_expanded_sentence(S, VNs, Sings),
	treat_expanded_sentence_list(Ss, VNs, Sings).

treat_expanded_sentence(X, _VNs, _Sings) :- nonvar(X), anon_curly_block(X, Sentences), !,
	% Define a an open-anonymous nested module (see new_open_anon for its definition)
	% TODO: change the syntax?
	treat_nested('', mod_open_anon, Sentences).
treat_expanded_sentence((:- Decl), VNs, _Sings) :- !,
	treat_decl(Decl, VNs).
treat_expanded_sentence(Clause0, VNs, Sings) :- !,
	( Clause0 = (Head :- Body) ->
	    true
	; Head = Clause0,
	  Body = true
	),
	treat_clause(Head, Body, VNs, Sings).

% Treat (:- _) sentences.
treat_decl(Decl, _VNs) :- mod__treatDom(Decl), !,
	% Misplaced (not-nested) module declaration (e.g., module/2,
	% module/3, etc.).  They can only appear the first in a file.
	functor(Decl, F, A),
	ft_error(nonstarting(F, A)).
treat_decl(Decl, _VNs) :- decl__treatDom(Decl), !,
	( decl__treat(Decl) -> true
	; ft_error(decl_failed(Decl))
	).
treat_decl(Decl, _VNs) :- symdecl__treatDom(Decl), !,
	( symdecl__treat(Decl) -> true
	; ft_error(decl_failed(Decl))
	).
treat_decl(Decl, _VNs) :- symmodifdecl__treatDom(Decl), !,
	( symmodifdecl__treat(Decl) -> true
	; ft_error(decl_failed(Decl))
	).
treat_decl(Decl, VNs) :- newdecl__treatDom(Decl), !,
	newdecl__treat(Decl, VNs).
treat_decl(Decl, _VNs) :-
	ft_error(unknown_decl(Decl)).
}.

{
:- extends modread_ctx.
% A basic declaration (neither module, nested, nor user defined)
is_basic_decl(Decl) :-
	( decl__treatDom(Decl)
	; symdecl__treatDom(Decl)
	; symmodifdecl__treatDom(Decl)
	), !.
}.

% ---------------------------------------------------------------------------
:- doc(subsection, "Treat user-defined declarations").

% TODO: Need more complex declaration visibility rules for
%   declarations? (I cannot make visible in the .itf a
%   declaration which talks about private predicates, for
%   example.

{
:- extends modread_ctx.
% @var{Decl} is a user defined declaration (e.g., assertions)
newdecl__treatDom(Decl) :-
	% TODO: Use enclosing_star for user declarations.
	(~def_envmod).new_decl(Decl, _DeclVisibility).

newdecl__treat(Assrt, VNs) :-
        % TODO: Redefine as a plug-in; use priorities to make sure
        %   that this is executed after 'regtypes'.
        normalize_if_assertion_body(Assrt,AssrtStatus,AssrtType,AssrtBody), !,
	treat_assertion(AssrtStatus, AssrtType, AssrtBody, VNs).
newdecl__treat(Decl, VNs) :-
	Loc = ~((~get_errs).get1_loc(VNs)),
	(~def_envmod).add_decl(Decl, Loc).
}.

% ---------------------------------------------------------------------------
:- doc(subsection, "Treat nested modules").

{
:- extends modread_ctx.
% Treat a nested module
treat_nested(Name, Modifier, Sentences) :-
	Module = ~new_nested(Name, Modifier),
	switch_envmod(Module, process_source(sentences(Sentences), no)).

:- meta_predicate rec_for_nested(?, ?, out(module_s)).
new_nested(Name, Modifier) := Module :-
	( atom(Name) -> true
        ; ft_error(bad_module_name(Modifier, Name)) % TODO: rename by bad_nested_name?
	),
	( Modifier = mod_open_anon, Name = '' ->
	    Module = ~new_open_anon
	; Module = ~new_nested_mod(Name, Modifier)
	).

:- meta_predicate new_nested_mod(?, ?, out(module_s)).
% TODO: Modifier=mod_static is not implemented for BC backend (FIX).
% TODO: Clauses are not correctly treated in mixins.
new_nested_mod(Name, Modifier) := Nested :-
	M = ~def_envmod,
	Nested = ~M.query_nested_module(Name),
	Nested.set_modifier(Modifier).

:- meta_predicate new_open_anon(out(module_s)).
new_open_anon := Nested :-
	M = ~envmod,
	trust(M instance_of module_s),
	Nested = ~M.query_anon_module, % TODO: missing in JS backend
	Nested.set_modifier(mod_open_anon).
}.

% ---------------------------------------------------------------------------
:- doc(subsection, "Inclusion of files").

% (predicate exported for declaration handlers)
%
% Include a module with the specified @var{Modifier}. @var{Modifier}
% can be one of:
%
%  - mod_include: plain sentences that are copied into the source module
%  - mod_package: a package
%
% Important: when including a package, the clauses are inserted in an
%   open-anon block with '$all_static' enabled.
%
% TODO: use a 'include stack' to avoid inclusion loops

{
:- extends modread_ctx.

treat_include(Modifier, Spec) :- Modifier = mod_package,
	Module = ~def_envmod,
        Module.get_using(Modifier, Spec),
	!. % package already included
treat_include(Modifier, Spec) :-
	Module = ~def_envmod,
	Module.add1_using(Modifier, Spec),
	% TODO: first argument of 'protect' is wrong (the command is not not always 'include')
	(~get_errs).protect(include(Spec), treat_include__1(~memo, ~envmod, Modifier, Spec)).
}.

% TODO: this aux predicate is required because of protect/2
% TODO: Use protect in class and anonymous block reader
treat_include__1(Memoize, ModuleR, Modifier, Spec) :-
	envmod :: module_s <- ModuleR,
	modreadEnv :: modread_env <- ~modread_env.from_db(ModuleR),
	memo :: memoize <- Memoize,
	treat_include__2(Modifier, Spec).
{
:- extends modread_ctx.
treat_include__2(Modifier, Spec) :-
	eval_file(prolog_source(Spec), Name),
	%
	'$open'(Name, r, Stream),
	call((
	  errs :: errlog <- ~get_errs,
	  errs.add_src(Name),
	  %
	  process :: any <- true,
	  stream :: any <- Stream,
	  in :: m_any <- [],
	  process_sentences_((Modifier = mod_include ? no | yes), Modifier)
        )),
	%
	close(Stream).
}.

% ---------------------------------------------------------------------------
:- doc(subsection, "Treat clauses").

{
:- extends modread_ctx.

% process (_ :- _) sentences
treat_clause(H, _, _, _) :-
	number(H), !,
	% TODO: be more descriptive in this error
	ft_error(illegal_clause).
treat_clause(H0, B, VNs, Sings) :-
	FunExp = (get_pragma(functional_expand) ? yes | no), % TODO: replace by an identifier of the input language for the body?
	head_id(FunExp, H0, F0, A),
	valid_clause(F0, A, B, B1),
	Pred = ~pred_ref_ac(F0, A),
	clause_check(Pred, Sings),
	Loc = ~((~get_errs).get1_loc(VNs)),
	Pred.add_clause(H0, B1, Loc, FunExp).

% The clause F0/A with body B is valid:
%  - its head is not forbidden
%  - its body is well-formed
valid_clause(F0, A, B, B1) :-
	Module = ~top_envmod, % TODO: use enclosing_star?
	( Module.not_definable(F0, A) ->
	    ft_error(redefine_pred(F0, A))
	; true
	),
	% TODO: wellformed_body replaces X var in goal positions by
	%       call(X) this is not necessary if the rest of the
	%       compiler is OK, and this is not enough if you take
	%       other translations into account.
        ( wellformed_body(B, +, B1) ->
	    true
	; ft_error(malformed_body(F0, A))
	).

}.

% TODO: This acts as a 'pre expansion' of the head for funcional
%   expansion, which is used just to classify the clause for its
%   predicate. It could be done in other way (or not).
head_id(_, H, _, _) :- var(H), !, fail.
head_id(yes, (H := _), F, A) :- !,
	functor(H, F, A0), A is A0 + 1.
head_id(_, H, F, A) :-
	functor(H, F, A).

:- use_module(library(dynamic), [wellformed_body/3]).

% ===========================================================================
:- doc(section, "Default operators").
% TODO: Move to a file defining operators (like prelude.pl)?
%   Other operators are defined in complang_mini.pl

%:- initialization(define_ops).
define_ops :-
        flagcontext__do(op(1150,fx,[% (public),  % compatibility
                                    % (mode),    % compatibility
                                    (dynamic),
                                    (concurrent),
                                    (data),
                                    (multifile),
                                    (meta_predicate),
                                    (discontiguous)])).

% ===========================================================================
:- doc(section, "Helper predicates for the front-end").

{
:- fluid stream :: any.
% Skip shell script lines
skip_shell_lines :-
        peek_code(~stream, 0'#), !,
        current_input(OldIn),
        set_input(~stream),
        skip_code(10),
        get_line(Line),
        skip_lines_until_blank(Line),
        set_input(OldIn).
skip_shell_lines.
}.

:- use_module(library(strings), [get_line/1, whitespace0/2]).

skip_lines_until_blank(Line) :-
        whitespace0(Line, []), !.
skip_lines_until_blank(_) :-
        get_line(Line),
        skip_lines_until_blank(Line).

% ---------------------------------------------------------------------------

{
:- extends modread_ctx.

find_source(RelUspec) := Spec :-
	nonvar(RelUspec),
	FromSpec = ~((~top_envmod).mod_spec),
	store:find_source(RelUspec, relspec(FromSpec), Spec).

find_package(RelUspec) := Spec :-
	nonvar(RelUspec),
	FromSpec = ~((~top_envmod).mod_spec),
	store:find_package(RelUspec, relspec(FromSpec), Spec).
}.

% ---------------------------------------------------------------------------

% Warn about failed declaration
{
:- extends errlog_ctx.
warning_failed_decl(Decl) :-
        ft_error(decl_failed_warning(Decl)).
}.

% ===========================================================================
:- doc(section, "Interface for front-end handlers").

% ---------------------------------------------------------------------------
:- doc(subsection, "Handlers for module declarations").
% This is an extensible interface for different kind of nested
% modules.

:- discontiguous mod__treatDom/1.
:- discontiguous mod__treat/3. % Treat module declaration

% :- pred mod__nested(Keyword, Modifier) # "@var{Keyword} declares a
%    nested module with modifier @var{Modifier}".
:- discontiguous mod__nested/2.

% ---------------------------------------------------------------------------
:- doc(subsection, "Handlers for declarations").

% TODO: Think about declarations as goals that work at the program
%   meta level. This seems a simple and powerful. Just do symbolic
%   reasoning and partial evaluation.
%
%   Why don't specify it with assertions? E.g.,
%     :- declaration public/1 # "".
%     public(X) :- ...
%
%   Problem: code live at different levels (the same than 'basal' in
%   the JS backend).

% Other declaration handlers (not symdecl nor symmodifdecl)
:- discontiguous(decl__treatDom/1).
:- discontiguous(decl__treat/1).
{
    :- extends modread_ctx.
    % TODO: '$props' is a trick to get the predicate defined in this scope (better use ':- pred')
    :- '$props'(decl__treatDom/1, []).
    :- '$props'(decl__treat/1, []).
}.

% Symbol declaration handlers (i.e., declarations associated to a
% predicate, module, etc. symbol)
:- discontiguous(symdecl__treatDom/1).
:- discontiguous(symdecl__treat/1).
:- discontiguous(symdecl__symspec/2). % The associated symspec
{
    :- extends modread_ctx.
    % TODO: '$props' is a trick to get the predicate defined in this scope (better use ':- pred')
    :- '$props'(symdecl__treatDom/1, []).
    :- '$props'(symdecl__treat/1, []).
    :- '$props'(symdecl__symspec/2, []).
}.

% Declaration handler for symbol modifiers, plus optional symbol declaration.
{
    :- extends modread_ctx.
    symmodifdecl__treatDom(Decl) :-
        functor(Decl, N, 1),
	symmodif__def(N),
	!.
    symmodifdecl__treat(Decl) :-
        extract_symmodifs(Decl, SymModifs, Decl0),
        ( % (failure-driven loop)
	  declexpr_member(Decl0, Decl1),
	    ( symdecl__treatDom(Decl1),
	      symdecl__symspec(Decl1, SymSpec) -> % A symdecl
	        symdecl__treat(Decl1)
	    ; % Assume a symspec (e.g., bar or foo/2)
	      SymSpec = Decl1,
	      check_symspec(SymSpec)
	    ),
	    % Apply the symmodifs to the symspec
	    apply_symmodifs_to_symspec(SymModifs, SymSpec),
	    fail
	; true
	).

    % TODO: (Use this also for nested modules)
    % Extract the list of symbol modifiers from a declaration.
    % (e.g., "public static foo/1" => [public, static] and foo/1)
    extract_symmodifs(Decl, [X|Xs], Rest) :-
	functor(Decl, X, 1), % Modifiers are prefix ops
	symmodif__def(X),
	arg(1, Decl, Decl0),
	!,
	extract_symmodifs(Decl0, Xs, Rest).
    extract_symmodifs(Decl, [], Decl).

    apply_symmodifs_to_symspec(SymModifs, SymSpec) :-
        ( % (failure-driven loop)
	  member(SymModif, SymModifs),
	    symmodif__set(SymModif, SymSpec),
	    fail
	; true
	).
}.

{
:- extends errlog_ctx.
check_symspec(SymSpec) :- % for modules, classes, attributes, etc. 
	atom(SymSpec), !.
check_symspec(SymSpec) :- % for predicates
	nonvar(SymSpec), SymSpec = F/A, atom(F), integer(A), !.
check_symspec(SymSpec) :-
	ft_error(bad_symspec_decl(SymSpec)).
}.

% TODO: With this definition, declexpr_member(a, [b|a]) is true. This
%   is an ugly notation that should not be accepted.
declexpr_member(X, X) :- var(X), !.
declexpr_member([], _) :- !, fail.
declexpr_member([X|Xs], Y) :- !,
        ( declexpr_member(X, Y)
        ; declexpr_member(Xs, Y)
        ).
declexpr_member((A,B), Y) :- !,
        ( declexpr_member(A, Y)
        ; declexpr_member(B, Y)
        ).
declexpr_member(X, X) :- !.

% ---------------------------------------------------------------------------
:- doc(subsection, "Handlers for symbol (predicate, etc.) modifiers").

% Handlers for symol modifiers
:- discontiguous(symmodif__def/1).
:- discontiguous(symmodif__set/2).
{
    :- extends modread_ctx.
    % TODO: '$props' is a trick to get the predicate defined in this scope (better use ':- pred')
    :- '$props'(symmodif__def/1, []).
    :- '$props'(symmodif__set/2, []).
}.

% ===========================================================================
:- doc(section, "Language definitions for the front-end").
% Those definitions declare handlers for the front-end, that will
% define what is accepted input language and how it is processed.

% TODO: Redefine as special packages, move to a separate lang/
%   directory.

:- include(compiler(frontend_core)).

% TODO: Make it work for the JS backend too.
:- if(use_backend(bc)).
:- include(compiler(dynamic_binding)).
:- elif(use_backend(js)).
:- endif.

% TODO: The high-level part could be reused for JS too, the low-level
%   part should be in a package.

% (handler for module compilation pass)
modpass__do(foreign_process_assertions, Module) :- foreign_process_assertions(Module).
:- if(use_backend(bc)).
:- include(compiler(foreign_c)).
:- elif(use_backend(js)).
foreign_process_assertions(_).
:- include(compiler(foreign_js)).
:- endif.

% TODO: Make it work for the JS backend too.
:- if(use_backend(bc)).
:- include(compiler(internal_props)).
:- elif(use_backend(js)).
:- endif.

% TODO: Make it work for the BC backend too, move to a package.
:- if(use_backend(bc)).
:- elif(use_backend(js)).
:- include(compiler(application_resources)).
:- endif.

% ===========================================================================
:- doc(section, "Compilation passes (during 'split', after reading)").

% TODO: Add an 'interface expansion' pass, but in
%       the 'compile' part. That is, in the 'compile' part we need a
%       processing step that takes an interface (projection) and
%       precomputes it for compilation. Currently this step is not
%       done. Note that, since some definitions on an interface may
%       depend on others (e.g., predicates using some type), we may
%       have the 'recursive dependency' problem in *module signatures*
%       (also present in ML). Special care must be taken to handle
%       this situation.

{
:- extends modread_ctx.
split_module_passes :-
	module_pass(infer_statemodel),
	module_pass(infer_ctx),
	%
	module_pass(gen_unfold_info), % TODO: not here, probably in interface export?
	% Post-process the modules
	module_pass(flatten_module),
	% (after defined predicates are known)
	module_pass(norm_assertions),
	module_pass(clean_assertions_postnorm),
	% (after assertions are normalized)
	module_pass(foreign_process_assertions),
	% Check properties
	module_pass(check_exports),
        % Infer (other) predicate properties
        module_pass(infer_pred_props).
}.

% ===========================================================================
:- doc(section, "Assertion handling and normalization").

:- use_module(library(aggregates), [findall/3]).
:- use_module(compiler(assertions__syntactic)). % normalize_if_assertion_body/4 
:- use_module(compiler(assertions__common)). % assertion_body/7

{
% Note: assertions are not 'definition points' for predicates; that
% means that we will look for the predicate definition later (like for
% export/1)
:- extends modread_ctx.
treat_assertion(_AssrtStatus, AssrtType, AssrtBody0, _VNs) :- AssrtType = modedef, !,
	% Declare a mode
	% TODO: modedef are special, global and not defined in any
	%       module or class scope
	do_modedef(AssrtBody0).
treat_assertion(AssrtStatus, AssrtType, AssrtBody0, VNs) :-
	% We will save the assertion and its definition place so that
	% we can process them later.
	Loc = ~((~get_errs).get1_loc(VNs)),
	% TODO: associate assertions to the (nested) module (this is a trick)
	Module = ~def_envmod,
	Module.add_assertion(a(AssrtStatus, AssrtType, AssrtBody0, Loc)).
}.

% (handler for module compilation pass)
modpass__do(norm_assertions, Module) :- norm_assertions(Module).

% TODO: Define modpass_ctx; this should not be modread_ctx
{
:- extends modread_ctx.
:- meta_predicate norm_assertions(module_s).
% Retract all assertions and assert them normalized.
% TODO: generalize for other properties?
norm_assertions(Module) :-
	Module.is_open_anon_or_mixin,
	!.
norm_assertions(Module) :-
	( % (failure-driven loop)
	  Module.get_assertion(Assrt),
	    Module.del_assertion(Assrt), % TODO: define a 'map' operation on 'data' preds? 
	    switch_envmod(Module, do_normalize_assertion(Assrt)),
	    fail
	; true
	).
}.

{
:- extends modread_ctx.
% Normalize assertion:
%  - once modedef declarations have been read
%  - once all locally defined predicates are known
%
% Note: assertions are not definition points, so this cannot be
%       associated to the predicate (as other predicate properties),
%       and we cannot use @pred{pred_ref_ac/2}.
%
% (wrapper)
do_normalize_assertion(Assrt0) :-
	Assrt = ~mod_do_norm_assertion(Assrt0),
	Module = ~def_envmod,
	mod_add_norm_assertion(Module, Assrt).

:- if(use_backend(bc)).
mod_do_norm_assertion(Assrt0) := ~normalize_assertion(Assrt0).
:- elif(use_backend(js)).
mod_do_norm_assertion(X) := X. % TODO: missing
:- endif.
}.

:- if(use_backend(bc)).
% Include a normalized assertion in @var{Module}
% TODO: really associate them to the module, right now it just expands its name
mod_add_norm_assertion(Module, Assrt) :-
	trust(Module instance_of module_s),
	Assrt = a(AssrtStatus, AssrtType, AssrtBody0, Loc),
	assertion_head_id(AssrtBody0, AssrtF0, AssrtA),
	AssrtPred = ~Module.pred_ref_noreg(AssrtF0, AssrtA),
	%
	AssrtF = ~AssrtPred.f,
	subst_assertion_head_id(AssrtBody0, AssrtF, AssrtBody),
	%
	% TODO: do it later
	( AssrtType = prop ->
	    AssrtPred.set_prop(is_prop)
	; true
	),
	%
	Module.add_assertion(a(AssrtStatus, AssrtType, AssrtBody, Loc)).

assertion_head_id(AssrtBody, N, A) :-
	assertion_body(Head, _, _, _, _, _, AssrtBody),
	head_id(no, Head, N, A). % TODO: include FunExp (like in clauses) as part of Assrt?

subst_assertion_head_id(AssrtBody0, N, AssrtBody) :-
	assertion_body(Head0, A, B, C, D, E, AssrtBody0),
	assertion_body(Head, A, B, C, D, E, AssrtBody),
	subst_head_name(Head0, N, Head).

subst_head_name(H0, F, H) :-
	H0 =.. [_|As], H =.. [F|As].

:- elif(use_backend(js)).

% TODO: Add assertion before normalization.
% TODO: Assertions are not kept in the module (we may want to do it).
mod_add_norm_assertion(Module, Assrt) :-
        trust(Module instance_of module_s),
	Assrt = a(_AssrtStatus, AssrtType, AssrtBody0, _Loc),
	AssrtType = pred,
	!,
	% AssrtType = pred,
	assertion_body(Head, TypesProps, _, _, Comps, _, AssrtBody0),
	conj_to_list(Comps, Props0),
	filter_unsupported_props(Props0, Props1),
	( TypesProps = true ->
	    Props = Props1
	; prod_to_list(TypesProps, TypesProps1),
	  Props = [argstype(TypesProps1)|Props1]
	),
	functor(Head, N, A),
	FunctorR = ~Module.pred_ref(N, A),
	FunctorR.parse_props(Props).
mod_add_norm_assertion(_Module, Assrt) :-
	trace(unsupported_assrt_type(Assrt)).

% TODO: better way to do this?
filter_unsupported_props([], []). 
filter_unsupported_props([X|Xs], Ys) :- unsupported_prop(X), !,
	filter_unsupported_props(Xs, Ys).
filter_unsupported_props([X|Xs], [X|Ys]) :-
	filter_unsupported_props(Xs, Ys).

unsupported_prop(true).

:- endif.

% (handler for module compilation pass)
modpass__do(clean_assertions_postnorm, Module) :- clean_assertions_postnorm(Module).

:- meta_predicate clean_assertions_postnorm(module_s).
% Clean modedef
% (that information is not necessary once all the nested
% modules has been normalized)
clean_assertions_postnorm(Module) :-
	Module.is_open_anon_or_mixin,
	!.
clean_assertions_postnorm(Module) :-
	( % (failure-driven loop)
	  Module.del_modedef(_, _),
	    fail
	; true
	).

:- include(compiler(assertions__nprops)).

% ===========================================================================
:- doc(section, "Interface checking").
% Check that the module implementation matches the declared module interface.
% TODO: Include implementation of interfaces here?

% (handler for module compilation pass)
modpass__do(check_exports, Module) :- check_exports(Module).
{
:- extends modread_ctx.
:- meta_predicate check_exports(module_s).
% Check that exported predicates are defined and are not multifile
check_exports(M) :-
	% TODO: not only for the top module
	\+ M.get_prop(exports_all),
	!,
	( % (failure-driven loop)
	  M.enum_exported_preds(Pred),
	    ( \+ Pred.is_defined ->
	        ft_error(exported_not_defined(~Pred.name_spec))
	    ; true
	    ),
	    ( Pred.get_prop(multifile) ->
	        ft_error(export_multifile(~Pred.name_spec))
	    ; true
	    ),
	    fail
	; true
	).
check_exports(_).
}.

% ===========================================================================
:- doc(section, "Inference of the statemodel and ctx of a module").

% (handler for module compilation pass)
modpass__do(infer_statemodel, Module) :- infer_statemodel(Module).
:- if(use_backend(bc)).
% Compute 'statemodel' for each (nested) module
% TODO: Add better syntax to fix the statemodel in the module signature
% TODO: Implement better analysis (do a global pass)
% TODO: this should be delayed to the 'compile' part, to avoid pli_resolve_classes
% TODO: bug: I need to compute the info of the top module before
{
:- extends errlog_ctx.
infer_statemodel(Module) :-
	trust(Module instance_of module_s),
	Module.is_class_or_interface,
	!,
	compute_module_statemodel(Module).
infer_statemodel(_).

% TODO: Fix the recursive case.
% TODO: Replace 'trust_module_statemodel' by a way to export the
%       statemodel.  This is necessary when there is no information in
%       the class signature (methods and exported fields) to infer it.
compute_module_statemodel(Module) :-
	trust(Module instance_of module_s),	
	( Module.statemodel(StateModel) ->
	    % Already computed or predefined
	    ( StateModel = pending ->
	        % TODO: The recursive case not implemented, fix.
	        throw(loop_while_computing_statemodel(Module))
	    ; true
	    )
	; % State model for the instances of the class:
          %  - 'pair' if at least one attribute is 'pair'
	  %  - 'single' otherwise
	  Module.set_statemodel(pending), % temporally mark it as pending
	  StateModel = ( Module.field(_, FClass0),
		         new_modread_ctx(Module, class_has_mutable_statemodel(FClass0)) ?
		           pair % at least one field is mutable
		       | single
		       ),
          ( StateModel = pair, Module.is_top_module ->
	      % TODO: implement
	      throw(top_module_with_pair_statemodel_not_yet_supported(Module))
	  ; true
	  ),
	  Module.set_statemodel(StateModel)
	).
}.

{
:- extends modread_ctx.
% TODO: partially duplicated in mexpand -- this one is using module_s instead of module_x
% note: 'mutable' kind of a field is required to determine if the
% container class is of mutable kind too and whether the 'set' method
% for the field is required
class_has_mutable_statemodel(/*QC*/Class) :-
	( Class = 'blt__m_any' -> true
	; Class = 'blt__any' -> fail
	; Mod = ~top_envmod, % TODO: not the right place
	  Mod.trust_module_statemodel(/*QC*/Class, StateModel2) ->
	    % TODO: check that the information is coherent with what can be inferred
	    StateModel2 = pair
	; Module = ~pli_resolve_class(Class),
	  compute_module_statemodel(Module), % Make sure that the statemodel is computed
	  ( Module.statemodel(StateModel2) ->
	      StateModel2 = pair
	  ; % TODO: Detect kind for exported classes
	    ft_error(bug(unknown_statemodel_for(Class))),
	    fail
	  )
	).
}.
:- elif(use_backend(js)).
infer_statemodel(_). % TODO: merge
:- endif.

% (handler for module compilation pass)
modpass__do(infer_ctx, Module) :- infer_ctx(Module).
:- if(use_backend(bc)).
{
:- extends errlog_ctx.
% Compute 'ctx' of each (nested) module (including open-anonymous and mixin)
% TODO: this should be delayed to the 'compile' part, to avoid pli_resolve_classes
infer_ctx(Module) :-
	trust(Module instance_of module_s),
	Module.is_open_anon_or_mixin,
	!,
	connect_open_ctx(Module),
	extends_anon_or_mixin(Module).
infer_ctx(Module) :-
	trust(Module instance_of module_s),
	\+ Module.is_class_or_interface, !.
infer_ctx(Module) :-
	compute_module_ctx(Module).
}.

compute_module_ctx(Module) :- % (except for mixin)
	trust(Module instance_of module_s),
	Module.statemodel(StateModel),
	SelfName = ~self_name,
	( Module.is_interface ->
	    % TODO: it fails if the metatype is ClassName (why?)
	    ClassR = ( StateModel = pair ? 'blt__m_any' | 'blt__any' ),
	    CtxDef1 = '\6\fluidsig'(SelfName, ClassR, none, yes)
	; ClassR = ~Module.get_id,
	  CtxDef1 = '\6\fluidsig'(SelfName, ClassR, none, no)
	),
	Module.addctx(CtxDef1).

% Connect the pEnvSig of the module with the pEnvSig of its enclosing module
% (currently, only for mod_open_anon)
:- meta_predicate connect_open_ctx(module_s).
connect_open_ctx(Module) :-
	Module.is_open_anon,
	!,
	( PrevModule = ~Module.enclosing_module,
	  \+ PrevModule.get_prop(static_scope), % (not inside $all_static open-anon)
	  \+ (PrevModule.is_top_module, PrevModule.is_static )
	->
	    % Connect with the enclosing scope
	    % TODO: do it dynamically (i.e., maintain a scope chain, or at least compute ctx later
	    Module.incctx(~module_to_id(PrevModule)) % TODO: passing an 'id' is not very nice
	; Module.incctx(none) % TODO: necessary?
	).
connect_open_ctx(_).

{
:- extends errlog_ctx.
extends_anon_or_mixin(Module) :-
	trust(Module instance_of module_s),
	m :: module_s <- Module,
	Bases0 = ~Module.all_extends,
	new_modread_ctx(Module, Bases = ~pli_resolve_classes(Bases0)),
        maplist((''(X) :- scope_extend(X)), Bases).
{
:- fluid m :: module_s.
scope_extend(Base) :-
	trust(Base instance_of module_s),
	( Base.is_mixin ->
	    m.incctx(~Base.get_id) % TODO: passing an 'id' is not very nice
	; % TODO: error, not a bug
	  % TODO: this could be relaxed, as mixins could be extended with other kind, as long as the mixin is used in the right place
	  ft_error(bug(anon_or_mixin_can_only_be_extended_with_mixin(~m.get_id, ~Base.get_id)))
	).
}.
}.
:- elif(use_backend(js)).
infer_ctx(_). % TODO: merge
:- endif.

% ===========================================================================
:- doc(section, "Propagation of predicate definitions for unfolding").
% TODO: This should not be here and this should not be restricted to
%       just methods (this should be part of interface projection).
%
% TODO: This should be partially automatic and configurable (there is
%       a trade-off between incremental compilation).

% (handler for module compilation pass)
modpass__do(gen_unfold_info, Module) :- gen_unfold_info(Module).
:- if(use_backend(bc)).
gen_unfold_info(Module) :-
	trust(Module instance_of module_s),
	Module.is_static_or_class_or_interface,
	!,
	( % (failure-driven loop)
	  Module.enum_defined_preds(Pred),
	    pred_gen_unfold_info(Module, Pred),
	    fail
	; true
	).
gen_unfold_info(_).

% TODO: generalize
pred_gen_unfold_info(Module, Pred) :-
	trust(Module instance_of module_s),
	trust(Pred instance_of predicate_s),
	Pred.get_prop(unfold(ArgsTemp)),
	!,
	% the predicate was marked for unfolding
	Clauses = ~findall(c(Head, Body), Pred.get_clause(Head, Body, _)),
	( Clauses = [c(Head, Body)], % TODO: generalize
	  Head =.. [_|Args] ->
	    ( ~length(Args) = ~length(ArgsTemp) ->
	        true
	    ; throw(unfold_arity_mismatch(~Pred.name_spec, unfold(ArgsTemp)))
	    ),
	    % Remove the clause(s)
	    Pred.erase_clauses,
	    % Move the clause to the predicate definition for unfolding
	    F = ~Pred.f, A = ~Pred.a,
	    Module.set_method_code(F, A, Args, ArgsTemp, Body)
	; throw(bug_unfold_with_more_than_one_clause(Clauses)) % TODO: not supported yet
	).
pred_gen_unfold_info(_, _).
:- elif(use_backend(js)).
gen_unfold_info(_).
:- endif.

% ===========================================================================
:- doc(section, "Infer (other) predicate properties").

% (handler for module compilation pass)
modpass__do(infer_pred_props, Module) :- infer_pred_props(Module).
:- if(use_backend(bc)).
{
:- extends modread_ctx.
:- meta_predicate infer_pred_props(module_s).
infer_pred_props(Module) :-
	( % (failure-driven loop)
	  % Enum all defined predicates
	  Module.enum_defined_preds(Pred),
	    infer_pred_props_(Pred),
	    fail
	; true
	).
}.

infer_pred_props_(Pred) :-
	trust(Pred instance_of predicate_s),
	Module0 = ~Pred.owner_module,
	trust(Module0 instance_of module_s),
	%
	( Pred.get_prop(def(Def)) -> true
	; ( Module0.get_prop(default_preddef(DefaultPreddef)) ->
	      true
	  ; DefaultPreddef = bytecode
	  ),
	  Def = DefaultPreddef
	),
	( Pred.get_prop(meta(Meta)) -> true
	; Meta = 0
	),
	%
	DefMod = ~Module0.find_def_enclosing,
	trust(DefMod instance_of module_s),
	Visibility = ( Pred.get_prop(multifile) ? vs_multifile
                     | ( DefMod.get_prop(exports_all) ; Pred.get_prop(exported) ) ? vs_public
                     | vs_private
                     ),
        IsProp = ( Pred.get_prop(is_prop) ? true | false ),
        ( Pred.get_prop(owner_module_ctx(Context0)) -> true
        ; Context0 = none
        ),
        Context1 = ( Pred.get_prop(context_prj(Prj)) ? 
                       modif(Context0, prj(Prj))
                   | Context0
                   ),
        Context = ( Pred.get_prop(static) ?
	              modif(Context1, selfprj(void))
		  | Pred.get_prop(constant) ?
	              modif(Context1, selfprj(use))
		  | Context1
		  ),
	Pred.set_pub_props(pub_props(Def, Meta, Visibility, IsProp, Context)).
:- elif(use_backend(js)).
infer_pred_props(_).
:- endif.

% ===========================================================================
:- doc(section, "Flattening of modules/classes to kernel code").
% TODO: This part could probably go into action__compile. One part is
% syntactic, but the other is semantic (e.g., auxiliary predicates for
% constructors, accessors, etc.).

:- include(compiler(flatmod_expansion)).

% ===========================================================================
:- doc(section, "Projections of module_s").
% Projections (interface and dependencies) for separate
% compilation. The projections are computed as the last part of the
% 'split' part and used in the 'compile' part.

:- if(use_backend(bc)).
% The 'itf' and 'deps' projections of the module
:- use_module(compiler(module_itf)).
:- '$trust_statemodel'(module_itf, single).
:- use_module(compiler(module_deps)).
:- '$trust_statemodel'(module_deps, single).

% Structure containing the module and its projections
:- public class module_s_prjs {
    :- public attr pli :: module_pli # "Reference to pli".
    :- attr itf :: module_itf # "Reference to itf".
    :- attr deps :: module_deps # "Reference to deps".

    :- constructor new_/1.
    new_(Pli) :-
        ~pli = Pli,
	~deps = ~module_deps.new,
        ~itf = ~module_itf.new.

    :- public clean_temp/0.
    clean_temp :-
        pli.clean_temp.

    :- public clean/0.
    % clean the permanent part
    clean :-
	'$inst_destroy'(~pli),
	'$inst_destroy'(~itf),
	'$inst_destroy'(~deps).

    % (Ret for action__do)
    % This is a projection of the module data for the 'split' pass
    ret := Ret :-
        Spec = ~pli.spec,
	Ret = [split__src(Spec) = ~pli,
	       split__itf(Spec) = ~itf,
	       split(Spec) = ~deps].
}.
:- endif.

:- public clean_temp/1.
:- if(use_backend(bc)).
clean_temp(SplitModuleR) :-
	trust(SplitModuleR instance_of module_s_prjs),
	SplitModuleR.clean_temp.
clean_all(SplitModuleR) :-
	trust(SplitModuleR instance_of module_s_prjs),
	SplitModuleR.clean.
get_ret(SplitModuleR) := Ret :-
	trust(SplitModuleR instance_of module_s_prjs),
	Ret = ~SplitModuleR.ret.
:- elif(use_backend(js)).
clean_temp(ModuleR) :-
	trust(ModuleR instance_of module_s),
	ModuleR.clean_temp.
clean_all(ModuleR) :-
	trust(ModuleR instance_of module_s),
	ModuleR.clean.
get_ret(ModuleR) := Ret :-
	trust(ModuleR instance_of module_s),
	Ret = ~ModuleR.ret.
:- endif.

:- if(use_backend(bc)).
:- meta_predicate project_itf(module_s, ?).
% TODO: only works for the top module at this moment, including
%       information from all the modules
project_itf(Module) := SplitModuleR :-
	m_s_prjs :: module_s_prjs <- SplitModuleR,
	SplitModuleR = ~module_s_prjs.new(~Module.pli),
	project_itf_(Module),
	clean_temp(SplitModuleR). % clean temporal data not necessary hereafter
{
:- fluid m_s_prjs :: module_s_prjs.

:- meta_predicate project_itf_(module_s).
project_itf_(Mod) :-
	% Project 'pli' into 'deps'
	Pli = ~Mod.pli, Deps = ~m_s_prjs.deps,
	( Pli.defines_module(X), Deps.add(defines_module(X)), fail
	; Pli.options(X), Deps.add(options(X)), fail
	; Pli.include(X), Deps.add(include(X)), fail
	; Pli.import(X), Deps.add(import(X)), fail
	; Pli.transform(X), Deps.add(transform(X)), fail
	).
% Project 'pli' into 'itf'
project_itf_(Mod) :-
	Pli = ~Mod.pli, Itf = ~m_s_prjs.itf,
	( Pli.defines_module(X), Itf.add(defines_module(X)), fail
	; Pli.reexport(X), Itf.add(reexport(X)), fail
	; Pli.'$reexports_all'(Spec), Itf.add('$reexports_all'(Spec)), fail
	; Pli.'$reexports'(Spec, F, A), Itf.add('$reexports'(Spec, F, A)), fail
	).
project_itf_(M0) :-
	% Enum all defined predicates
	trust(M0 instance_of module_s),
	enum_nested_star(M0, M), % (enumerate modules)
	trust(M instance_of module_s),
	M.enum_defined_preds(Pred), % (enumerate predicates)
	  % Note: this may assert exported_sym, so it has to be executed before the next clause
	  project_pred_itf(Pred),
        fail.
% TODO: Enumerate all nested, and associate 'exported' as a module
%   property, do not call pli_resolve_class here.
project_itf_(Module) :-
	_ = ~project_itf_nested_star(Module),
	fail.
project_itf_(Module) :- % TODO: integrate with previous pass
	Module.exported_sym(Name), % (for each exported scope)
	  ( ModuleS = ~Module.nested_star_find_class(Name) -> % TODO: implement in a better way...
	      ModuleSItf = ~module_s_itf_from_module_s(ModuleS),
	      copy_module_s_itf_from_module_s(ModuleSItf, ModuleS)
	  ; throw(bug(get_exported_sym__notfound(Name)))
	  ),
	fail.
project_itf_(Module) :-
	Module.pli.exported_binder(F, A),
	  functor(Head, F, A), 
	  Module.pli.binder_def(Head, Def),
	  m_s_prjs.itf.add(binder_def(Head, Def)),
	fail.
project_itf_(Module) :-
	% Export user-defined declarations
	% Note: the current approach is very limited
	% TODO: this feature does not seem to be used anywhere (for assertions?)
	Module.pli.decl(Decl, _),
	  ( Module.pli.new_decl(Decl, DeclVisibility),
	    DeclVisibility = public ->
	      m_s_prjs.itf.add(decl(Decl))
	  ; true
	  ),
	  fail.
project_itf_(_Module).

% Copy the nested structure from module_s to module_s_itf
:- meta_predicate project_itf_nested_star(module_s, out(module_s_itf)).
project_itf_nested_star(Module0) := Module0Itf :-
	Module0Itf = ~module_s_itf_from_module_s(Module0),
	% Process nested
	( % (failure-driven loop)
	  NestedS = ~Module0.get_nested_module(Name),
	    ( Module0.exported_sym(Name) -> % (only if exported)
	        NestedSItf = ~project_itf_nested_star(NestedS),
		Module0Itf.add_nested_module(Name, NestedSItf)
	    ; true
	    ),
	    fail
	; true
	).
}.

{
:- fluid m_s_prjs :: module_s_prjs.
% Obtain the module_s_itf for the given module_s
% (each one registers data in a different database)
:- meta_predicate module_s_itf_from_module_s(module_s, out(module_s_itf)).
module_s_itf_from_module_s(ModulePli) := ModuleItf :-
	ModuleItf = ~module_s_itf.from_id(~m_s_prjs.itf, ~ModulePli.get_id).
}.

{
:- fluid m_s_prjs :: module_s_prjs.
% Propagate predicate properties into the module interface
project_pred_itf(Pred) :-
	trust(Pred instance_of predicate_s),
	Pred.get_pub_props(PubProps),
	PubProps = pub_props(_, _, Visibility, _, _),
	% Exported preds go to the itf too
	( Visibility = vs_public ->
	    % Mark Context0 as exported if not exported
	    % TODO: simplify, define an 'internal' export
	    % TODO: Do the module expansion of the ITF *after collecting them*; that could make this simpler
	    % TODO: Export types for meta_predicate, etc. (also assertions)
	    ( Pred.get_prop(owner_module_ctx(Context0)) -> true
	    ; Context0 = none
	    ),
	    ( Context0 = none -> true
	    ; Context0 = module -> true
	    ; ensure_exported_module(~Pred.envmod_ctx)
	    ),
	    % Export the predicate
	    % TODO: define predicate_s_itf
	    F = ~Pred.f, A = ~Pred.a,
	    m_s_prjs.itf.add(export(F, A, PubProps))
	; true
	).
}.

% TODO: We need two levels of exports: user-visible exports and
%       compiler-visible exports. In this case (and in $unfold) we
%       want to export the definition but we do not want users to
%       import or access it directly.
%
% TODO: 'Name' in exported_sym is unqualified, fix (you may be exporting the wrong symbol)
ensure_exported_module(Module) :-
	trust(Module instance_of module_s),
	Name = ~Module.get_name,
	DefMod = ~Module.find_def_enclosing,
	trust(DefMod instance_of module_s),
	( DefMod.exported_sym(Name) ->
	    % TODO: see problem above about exported_sym
	    true % already exported
	; % TODO: this must be a special export; options: export automatically
	  %       open-anon and warn when other elements are exported
	  DefMod.add1_exported_sym(Name),
	  % TODO: Do not include in 'top_enclosing_module'!! (see "integrate with previous pass")
	  TopMod = ~Module.top_enclosing_module,
	  trust(TopMod instance_of module_s),
	  TopMod.add1_exported_sym(Name)
	).

copy_module_s_itf_from_module_s(ModuleSItf, ModuleS) :-
	trust(ModuleS instance_of module_s),
	trust(ModuleSItf instance_of module_s_itf),
	% TODO: Copy only the nested modules that are being exported.
	( ModuleS.get_name(A), ModuleSItf.set_name(A), fail ; true ),
%	( ModuleS.get_nested_module(A, B), ModuleSItf.add_nested_module(A, B), fail ; true ),
	( ModuleS.get_modif(Modifier), ModuleSItf.add_modif(Modifier), fail ; true ),
	( ModuleS.statemodel(A), ModuleSItf.add_statemodel(A), fail ; true ),
	( ModuleS.selfname(A), ModuleSItf.add_selfname(A), fail ; true	),
	( ModuleS.extendsR(A), ModuleSItf.add_extendsR(A), fail ; true ),
	( ModuleS.field(A, B), ModuleSItf.add_field(A, B), fail ; true ),
	( ModuleS.data(A, B, C), ModuleSItf.add_data(A, B, C), fail ; true ),
	( ModuleS.method(A, B, C), ModuleSItf.add_method(A, B, C), fail ; true ),
	( ModuleS.method_code(A, B, C, D, E), ModuleSItf.add_method_code(A, B, C, D, E), fail ; true ),
	( ModuleS.virtual(A, B, C, D, E), ModuleSItf.add_virtual(A, B, C, D, E), fail ; true ),
        ( ModuleS.getctx(Def), ModuleSItf.addctx(Def), fail ; true ).
:- elif(use_backend(js)).
% TODO: MISSING
project_itf(R) := R :-
	clean_temp(R). % clean temporal data not necessary hereafter
:- endif.

% ===========================================================================
:- doc(section, "Compilation pass (after 'split')").

:- if(use_backend(bc)).

% ===========================================================================

:- use_module(compiler(ptoc__props)).

:- use_module(compiler(translation_module_holder)).

%:- use_module(compiler(compiler__profile)). % TODO: comment...
:- use_module(compiler(open_and_protect)).
% For absmach definitions
:- use_module(compiler(module_ipexp)).
% Intermediate code generation and transformations
:- use_module(compiler(compiler__expand)).
:- use_module(compiler(bytecode__compiler)).
:- use_module(compiler(ptoc__analyze)).
:- use_module(compiler(ptoc__lowcomp)).
:- use_module(compiler(ptoc__ins)).
% To write native code
:- use_module(compiler(ptoc__impcomp)).
:- use_module(compiler(foreign__gluecode)).

:- use_module(compiler(sht_analyzer)).

:- use_module(library(dict)).

:- use_module(library(aggregates), [findall/3]).

:- include(.(aux_profile_ctx__disabled)).
%:- include(.(aux_profile_ctx)).

:- include(.(absint__interface)).

% TODO: Recover lazy modules: in 'compiler' part, write a lazy emu
%   that writes everything but the static code (include the
%   initialization!!  that must not change in order to preserve the
%   semantic) and replaces the static code by module read hooks; note
%   that modules with initialization cannot be lazy...

% ---------------------------------------------------------------------------
:- doc(section, "Compile").

{
:- fluid memo :: memoize.

% TODO: Simplify module_exp? avoid data copy?
compile(Spec, Ret) :-
	% Get expanded code and declarations, and compile
	modcompEnv :: modcomp_env <- ModcompEnv,
	ModcompEnv = ~modcomp_env.new(Spec),
	IDeps = ~module_ideps.new, IDeps = ~modcompEnv.ideps,
	Sym = ~module_sym.new, Sym = ~modcompEnv.sym,
	Exp = ~module_exp.new, Exp = ~modcompEnv.exp,
	expand__2(Preds),
        ( (~get_errs).get1_module_error ->
	    '$inst_destroy'(Exp),
	    '$inst_destroy'(Sym),
	    '$inst_destroy'(IDeps),
	    modcompEnv.clear,
	    Ok = no
	; Ok = yes
	),
	% Free objects not used during codegen
	free_input,
	Ok = yes,
        % Code generation
	load_absmach,
	codegen(Spec, Preds, Bin),
	% TODO: use gc
	'$inst_destroy'(Exp),
	modcompEnv.clear,
	% Set bin
	Ret = [expand__ideps(Spec)=IDeps,
	       expand__sym(Spec)=Sym,
	       compile(Spec)=Bin].
}.

% ---------------------------------------------------------------------------
% Module-expansion (filling module_exp)

{
:- extends modcomp_ctx.
:- extends errlog_ctx.
%:- use_module(compiler(compiler__profile)).
%expand__2 :- Spec = ~modcompEnv.spec, profile_elapsed(i(Spec), read_input), !, profile_elapsed(e(Spec), expand__3).
expand__2(Preds) :-
	read_input, !,
	'$global_vars_get'(2, OldStaticData),
	'$global_vars_set'(2, static_data(~memo, ~modcompEnv)),
	expand__3(Preds),
	'$global_vars_set'(2, OldStaticData).
expand__2([]).
}.

{
:- extends modcomp_ctx.
:- extends errlog_ctx.
expand__3(Preds) :-
	m_s :: module_s <- ~modcompEnv.pli.get_top_module,
	% TODO: why?? strange... the first imported is oneself...
        spec_defines_module(~modcompEnv.spec, Module),
	Module = ~modcompEnv.exp.defines_module,
	% Process module_itf and related module_itf
	% TODO: run profile again...
	%
        gen_nested_info(Module),
	%
        gen_imports(Module), % profile: 2nd most costly
	%
        gen_defines(Module), % profile: 5th most costly
	%
	gen_exports(Module),
	%
	gen_inline_code(Module),
	%
	% Process module_pli
        activate_translation(Module),
        do_expansion_checks, % must be done here after translation modules are loaded
	exp_preds(Module, Preds, Metadatas), % profile: 1st most costly
	Metadatas = ~modcompEnv.exp.metadatas,
	expand__other,
	deactivate_translation(Module),
	%
	del_uses_runtime_data,
	%
	delete_module_data.
}.

{
:- extends modcomp_ctx.
expand__other :-
	% gen native include 
	% TODO: not sure...
	copy_from_pli_to_exp(native_include_c_source(_)),
	copy_from_pli_to_exp(native_include_c_header(_)),
	copy_from_pli_to_exp(native_inline(_)),
	% copy pragmas
	% TODO: not every pragma is needed in the module_exp... it could be refined
	copy_from_pli_to_exp(pragma(_)).
}.

{
:- extends modcomp_ctx.
copy_from_pli_to_exp(Data) :-
	( modcompEnv.pli.get__(Data),
	  modcompEnv.exp.add(Data),
	  fail
	; true
	).
}.

% ---------------------------------------------------------------------------
:- doc(subsection, "Read module_s_prjs and module_s_itf of imported modules").

{
:- extends modcomp_ctx.
:- extends errlog_ctx.
% TODO: on failure itfs are not correctly removed from memory!
read_input :-
	Spec = ~modcompEnv.spec,
	Pli = ~module_pli.from_spec(Spec),
	Pli = ~modcompEnv.pli,
	Deps = ~module_deps.from_spec(Spec),
	Deps = ~modcompEnv.deps,
	read_itf(Spec),
	spec_defines_module(Spec, Module),
	Itf = ~get_mod_itf(Module),
	Itf = ~modcompEnv.itf,
	read_imported_itf(Deps).
}.

{
:- extends modcomp_ctx.
:- extends errlog_ctx.
read_imported_itf(Deps) :-
	trust(Deps instance_of module_deps),
	ImportedSpecs = ~Deps.import__list,
	maplist((''(ImportedSpec) :- read_itf(ImportedSpec)), ImportedSpecs).
}.

{
:- extends modcomp_ctx.
:- extends errlog_ctx.
% Read the itfs of a module (and the modules that it reexports)
read_itf(Spec) :-
	covered :: m_any <- [],
	m_s :: module_s <- ~modcompEnv.pli.get_top_module,
	read_itf__2(Spec).
{
:- fluid m_s :: module_s.
:- fluid covered :: m_any.
:- '$ctxprj'(read_itf__2/1, [memo, modcompEnv, m_s, u(covered)]).
% TODO: 'u(covered)' instead of 'covered' in read_itf__2/1 is
%       mandatory.  Because on recursive calls I want the previous
%       state to be preserved; find a better way to indicate it.
read_itf__2(Spec) :-
	spec_defines_module(Spec, Module),
	_ = ~get_mod_itf(Module), !. % stop if the node has been visited
read_itf__2(Spec) :-
	Itf = ~module_itf.from_spec(Spec),
	Module = ~Itf.defines_module,
	% register this Itf (it will be unloaded if any of the following tests fails)
	set_mod_itf(Module, Itf),
	check_reexport_loop(Module),
	check_module_clash(Module),
	check_user_module(Spec, Module),
	modcompEnv.ideps.add(imported(Module, Spec)),
	covered <- [Module|~covered],
        % Read itf of reexported modules
        RSpecs = ~Itf.reexport__list,
	maplist((''(S) :- read_itf__2(S)), RSpecs).

% TODO: bad indexing
% TODO: check that it works...
:- '$ctxprj'(check_reexport_loop/1, [memo, u(covered)]).
check_reexport_loop(Module) :-
	( member(Module, ~covered) ->
	    reverse([Module|~covered], Loop),
	    ft_error(reexport_loop(Loop)),
	    fail
	; true
	).
}.
}.

{
:- extends modcomp_ctx.
free_input :-
	Pli = ~modcompEnv.pli,
	( nonvar(Pli) -> '$inst_destroy'(Pli) ; true ),
	Deps = ~modcompEnv.deps,
	( nonvar(Deps) -> '$inst_destroy'(Deps) ; true ),
	Keys = ~mod_itf__keys,
	maplist((''(Module) :-
	  Itf = ~get_mod_itf(Module),
	  '$inst_destroy'(Itf)
        ), Keys).
}.

{
:- extends modcomp_ctx.
:- extends errlog_ctx.
check_module_clash(Module) :-
	modcompEnv.ideps.imported(Module, OtherSpec),
	!,
	ft_error(module_redefined(OtherSpec, Module)),
	fail.
check_module_clash(_).
}.

{
:- extends modcomp_ctx.
:- extends errlog_ctx.
:- fluid m_s :: module_s.
check_user_module(Spec, _) :- % ok if that is the main itf
	Spec = ~modcompEnv.spec, !.
% check if the module is loaded correctly (ensure_loaded iff usermod)
check_user_module(Spec, Module) :-
	LoadedAsUsermod = ( m_s.get_should_be_usermod(Spec) ? yes | no ),
	IsUsermod = ( '$user_module_id'(Module) ? yes | no ),
	( LoadedAsUsermod = IsUsermod ->
	    true
	; ft_error(usermod_import(Spec, IsUsermod, LoadedAsUsermod))
	).
}.

% ---------------------------------------------------------------------------
:- doc(subsection, "Expansion checks").

% (e.g., for callback__ensure_imported/4)

{
:- extends modcomp_ctx.
do_expansion_checks :-
	ModcompEnv = ~modcompEnv,
	modcompEnv.pli.expansion_check(Pred),
	  '$trust_metatype'(Pred, pred(1)),
          Pred(ModcompEnv),
        fail.
do_expansion_checks.
}.

% ---------------------------------------------------------------------------

{
:- extends modcomp_ctx.
spec_defines_module(Spec, Module) :-
	% TODO: poor indexing?
	modcompEnv.ideps.get1_imported_fast(Spec, Module).
}.

% ---------------------------------------------------------------------------
% Process module_itf and related module_itf specified in module_pli

{
:- extends modcomp_ctx.
% register declaration of exported and reexported predicates
gen_exports(Module) :-
	exports_thru(Module, predsign(F, A, _PubProps, EM)),
	  trust(EM instance_of module_x), % (is_top_module)
	  ~pred_x.f(MF,_) = ~EM.pred_ref(F, A),
	  ( Module = EM -> true ; assertz_fact(reexports(F, A, MF)) ),
	  modcompEnv.sym.add(exports(F, A, MF)),
	fail.
gen_exports(_).
}.

{
:- extends modcomp_ctx.
:- extends errlog_ctx.
% register declaration of imported predicates
% TODO: use a more direct way... (you already have the info in PubProps)
gen_imports(Module) :-
	m_s :: module_s <- ~modcompEnv.pli.get_top_module,
	( % (failure-driven loop)
	  gen_imports__2(IM, PredSign, All),
	    % Include in local database to do module expansion
            set_imp_pub_props(IM, PredSign, All),
	    % Include in the module_exp
	    exp_add_import(Module, IM, PredSign),
            fail % loop
	; true
	).
{
:- fluid m_s :: module_s.
gen_imports__2(IM, PredSign, all) :-
	modcompEnv.pli.get_imports_all(ImportedSpec),
	  spec_defines_module(ImportedSpec, IM),
          exports_thru(IM, PredSign).
gen_imports__2(IM, PredSign, not_all) :-
	modcompEnv.pli.get_imports(ImportedSpec, F, A),
	  spec_defines_module(ImportedSpec, IM),
	  PredSign = predsign(F, A, _, _), % (only want solutions for F/A from exports_thru/2)
          ( exports_thru(IM, PredSign) ->
	      true
	  ; ft_error(not_exported(IM,F/A)),
	    fail
	  ).
gen_imports__2(IM, PredSign, All) :-
	m_s.get_imports_nocheck(IM, F, A),
	  EM = IM,
	  All = not_all,
	  % assume this pub_props for nocheck imports
	  PubProps = ~pub_props__default,
	  PredSign = predsign(F, A, PubProps, EM).
}.
}.

{
:- extends modcomp_ctx.
exp_add_import(Module, IM, predsign(F, A, PubProps, EM)) :-
	call((
          m :: any <- Module, % TODO: 'm' should not be required (this is our current module)
	  qualm :: any <- qual_m(IM),
	  mexpand__spec(F/A, MF2/A2)  % obtain resolved name for "IM:F/A"
        )),
	Exp = ~modcompEnv.exp,
	Exp.add(expanded_import(MF2, A2, PubProps)), % TODO: include _All?
	( IM = EM -> true
	; Exp.add(expanded_import_from(MF2, A2, IM))
	).
}.

{
:- extends modcomp_ctx.
exports_thru(FromM, predsign(F, A, PubProps, EM)) :-
	from_itf :: any <- ~get_mod_itf(FromM),
	exports_thru__2(F, A, PubProps, FromM, EM).
{
:- fluid from_itf :: module_itf.
exports_thru__2(F, A, PubProps, FromM, EM) :-
	EM = FromM,
	from_itf.export(F, A, PubProps),
	PubProps = pub_props(_, _, Visibility, _, _),
	\+ Visibility = vs_multifile.
exports_thru__2(F, A, PubProps, _FromM, EM) :-
	( from_itf.get_reexports(IndirectSpec, F, A)
	; from_itf.get_reexports_all(IndirectSpec)
	),
	spec_defines_module(IndirectSpec, IndirectM),
        exports_thru(IndirectM, predsign(F, A, PubProps, EM)),
	% but not directly exported... (avoid duplicates)
        \+ ( from_itf.export(F, A, PubProps2),
	     PubProps2 = pub_props(_, _, Visibility, _, _),
	     \+ Visibility = vs_multifile
	   ).
}.
}.

% ---------------------------------------------------------------------------
% TODO: check -> module__ctx must be asserted before any module expansion is performed
% TODO: move binder_def elsewhere (it is more related to unfolded predicate definitions)

{
:- extends modcomp_ctx.
gen_nested_info(Module) :-
	% TODO: find a way to group data
	( M0 = ~modcompEnv.pli.get_top_module,
	  trust(M0 instance_of module_s),
	  enum_nested_star(M0, ModuleS), % (enumerate modules)
	  trust(ModuleS instance_of module_s),
            module_x.copy_from_module_s(ModuleS, Module)
	; modcompEnv.pli.binder_def(Head, Def),
	    assertz_fact(frontend:binder_def(Head, Def))
	),
	  fail. % (loop)
gen_nested_info(_Module) :-
	modcompEnv.ideps.imported(IM, _),
	  Itf = ~get_mod_itf(IM),
	  ( M0 = ~Itf.get_top_module,
	    trust(M0 instance_of module_s_itf),
	    enum_nested_star_itf(M0, ModuleS), % (enumerate modules)
	    trust(ModuleS instance_of module_s_itf),
	      module_x.copy_from_module_s_itf(ModuleS, IM)
	  ; Itf.binder_def(Head, Def), % (exported binder)
	      assertz_fact(frontend:binder_def(Head, Def))
          ),
	  fail. % (loop)
gen_nested_info(_Module).
}.

% ---------------------------------------------------------------------------
% Process module_pli (once the module_itf and related module_itf are processed)

set_pub_props(F, A, Module, PubProps) :-
	trust(Module instance_of module_x), % (is_top_module)
	~pred_x.f(MF,_) = ~Module.pred_ref(F, A),
	assertz_fact(pdecl(F, A, MF)),
	set_pinfo(MF, A, PubProps).

% TODO: merge with previous one?
set_mult_pub_props(F, A, PubProps) :-
	Module = 'multifile', % TODO: I am not sure about this...
	trust(Module instance_of module_x), % (is_top_module)
	~pred_x.f(MF,_) = ~Module.pred_ref(F, A),
	assertz_fact(pdecl(F, A, MF)),
	set_pinfo(MF, A, PubProps).

% TODO: Visibility is always different to vs_multifile here!!
set_imp_pub_props(IM, predsign(F, A, PubProps, EM), All) :-
	trust(EM instance_of module_x), % (is_top_module)
	~pred_x.f(MF,_) = ~EM.pred_ref(F, A),
        assertz_fact(imports(F, A, IM, EM, All, MF)),
	set_pinfo(MF, A, PubProps).

% TODO: pinfo A is the arity before context expansion, check all the uses of pinfo and the runtime uses of pinfo!! 
set_pinfo(MF, A, PubProps) :-
	PubProps = pub_props(_, Meta, _, IsProp, Context),
	assertz_fact(pinfo(MF, A, Meta, IsProp, Context)).

:- meta_predicate module_x_from_module_s(module_s, out(module_x)).
module_x_from_module_s(ModuleS) := ModuleX :-
	ModuleX = ~ModuleS.get_id.

:- meta_predicate module_x_from_module_s_itf(module_s_itf, out(module_x)).
module_x_from_module_s_itf(ModuleS) := ModuleX :-
	ModuleX = ~ModuleS.get_id.

{
:- extends modcomp_ctx.
gen_inline_code(Module) :-
	( ( modcompEnv.ideps.imported(IM, _),
	    Itf = ~get_mod_itf(IM),
	    M0 = ~Itf.get_top_module,
	    trust(M0 instance_of module_s_itf),
	    enum_nested_star_itf(M0, ModuleS), % (enumerate modules)
	    trust(ModuleS instance_of module_s_itf),
	      ModuleS.method_code(RF, A, Args, ArgsTemp, Code),
	      ModuleX = ~module_x_from_module_s_itf(ModuleS)
	  ; M1 = ~modcompEnv.pli.get_top_module,
	    trust(M1 instance_of module_s),
	    enum_nested_star(M1, ModuleS2), % (enumerate modules)
	    trust(ModuleS2 instance_of module_s),
	      ModuleS2.method_code(RF, A, Args, ArgsTemp, Code),
	      ModuleX = ~module_x_from_module_s(ModuleS2)
	  ),
            trust(ModuleX instance_of module_x),
	    ( ModuleX.in_mod(IM2) -> true ; fail ), % TODO: can this fail?
	    QM = IM2,
	    call((
              m :: any <- Module,
	      qualm :: any <- qual_m(QM),
	      mexpand__spec(RF/A, MRF/_A2) % TODO: I do not like this
            )),
%	    mexpand__trace(added_method_code(MRF, A, ModuleX, Args, ArgsTemp, Code)),
	    module_x.add_method_code(MRF, A, ModuleX, Args, ArgsTemp, Code),
	    fail
	).
gen_inline_code(_).
}.

{
:- extends modcomp_ctx.
:- extends errlog_ctx.
:- fluid m_s :: module_s.
gen_defines(Module) :-
	M0 = ~m_s,
	trust(M0 instance_of module_s),
	enum_nested_star(M0, M), % (enumerate modules)
	trust(M instance_of module_s),
	M.enum_defined_preds(Pred), % (enumerate predicates)
	trust(Pred instance_of predicate_s),
	F = ~Pred.f, A = ~Pred.a,
	Pred.get_pub_props(PubProps),
	  % Include in local database to do module expansion
	  PubProps = pub_props(Def, _, Visibility, _, _),
	  ( Visibility = vs_multifile ->
	      set_mult_pub_props(F, A, PubProps),
	      % Avoid incompatible multifile definitions
	      check_multifile(F, A, Def)
	  ; set_pub_props(F, A, Module, PubProps)
	  ),
	  % Include in module_exp
	  % TODO: use a more direct way and avoid this hack to avoid warnings
	  Qual = ( Visibility = vs_multifile ? qual_none | qual_m(Module) ),
	  call((
            m :: any <- Module,
	    qualm :: any <- Qual,
	    mexpand__spec(F/A, MF2/A2)
	  )),
	  Exp = ~modcompEnv.exp,
	  ( Exp.expanded_decl(MF2, A2, _) ->
	      % TODO: expand names in a way so that there is no name clash between predicates with different context decl?
	      errlog:temperror(['conflicting definitions for ', MF2, '/', A2])
	  ; true
	  ),
	  Exp.add(expanded_decl(MF2, A2, PubProps)),
        fail. %% loop
gen_defines(_Module) :-
	% Generate redefines
	m_s.get_def_redefining(F, A),
	  assertz_fact(redefining(F, A)),
	  fail.
gen_defines(_Module) :-
	% Copy pragma and properties (functional expansion, etc.)
	( modcompEnv.pli.fun_eval(F, A),
	    assertz_fact(frontend:fun_eval(F, A)),
	    fail
	; modcompEnv.pli.eval_arith,
	    assertz_fact(frontend:eval_arith),
	    fail
	; modcompEnv.pli.pragma(functional_expand),
	    assertz_fact(functional_expand),
	    fail
	; modcompEnv.pli.pragma(class_expand),
	    assertz_fact(class_expand),
	    fail
	).
gen_defines(_) :-
	% TODO: It must be done before sht_usermemo is normalized
        % Types for ptoc
	modcompEnv.pli.ptoc_type(Type, Def),
	  modcompEnv.exp.add(ptoc_type(Type, Def)),
	fail. %% loop
gen_defines(_) :-
	% TODO: It must be done before sht_usermemo is normalized
        % Types operations for ptoc
	modcompEnv.pli.ptoc_typeprop(Type, Op, Def),
	  ( typeprop_kind(Op, Kind) ->
	      ( Kind = predname ->
	          % TODO: check that Name is _/_ during compiler/frontend.pl
	          Def = F/A,
		  call((
                    m :: any <- Module,
		    qualm :: any <- qual_m(Module),
		    mexpand__spec(F/A, MF2/A2)
	          )),
		  Def2 = MF2/A2
	      ; Kind = ctype ->
		  Def2 = Def
	      ; Kind = bool ->
		  Def2 = Def
	      )
	  ; % TODO: should be an error in compiler/frontend.pl
            errlog:temperror(['unknown typeprop ', Op])
	  ),
	  modcompEnv.exp.add(ptoc_typeprop(Type, Op, Def2)),
	fail. %% loop
gen_defines(Module) :-
        % Private properties (non visible from outside the module)
	modcompEnv.pli.priv_props(F, A, Props0),
          % TODO: multifile preds are not handled properly
          call((
            m :: any <- Module,
	    qualm :: any <- qual_m(Module),
	    mexpand__spec(F/A, MF2/A2)
	  )),
	  % TODO: check for errors before?
	  exp_priv_props(Props0, Module, Props),
	  modcompEnv.exp.add(priv_props(MF2, A2, Props)),
	  fail.
gen_defines(Module) :-
	% TODO: forced props is not very clean (in theory no one should use it)
	modcompEnv.pli.forced_props(F, A, QM, Props0),
	  % TODO: check for errors before?
	  exp_priv_props(Props0, Module, Props),
	  % TODO: does not work for multifile or context predicates
	  trust(QM instance_of module_x), % (is_top_module)
	  ~pred_x.f(MF2,_) = ~QM.pred_ref(F, A),
	  Exp = ~modcompEnv.exp,
	  ( Exp.priv_props(MF2, A, OldProps) ->
	      Exp.del(priv_props(MF2, A, _)),
	      call((
		exp :: module_exp <- Exp,
	        NewProps = ~prop_merge(Props, OldProps)
	      ))
	  ; NewProps = Props
	  ), % TODO: is NewProps used?
	  Exp.add(priv_props(MF2, A, NewProps)),
%	  Exp.add(priv_props(MF2, A, Props)),
	fail. %% loop
gen_defines(_) :-
        % imptype_c for ptoc
	modcompEnv.pli.ptoc_imptype_c(Name, Def),
	  modcompEnv.exp.add(ptoc_imptype_c(Name, Def)),
	fail. %% loop
gen_defines(Module) :-
        % Entries
	modcompEnv.pli.trust_entry(F, A, AbsIntName, Lambda0),
	  % TODO: Normalize sht types in other place?
	  ( AbsIntName = sht ->
	      Exp = ~modcompEnv.exp,
	      call((
		exp :: module_exp <- Exp,
		Lambda = ~map_type_norm(Lambda0)
	      ))
	  ; Lambda = Lambda0
	  ),
	  AbsInt = ~absint_name_to_analyzer(AbsIntName),
          % TODO: multifile preds are not handled properly
          call((
            m :: any <- Module,
	    qualm :: any <- qual_m(Module),
            mexpand__spec(F/A, MF2/A2)
          )),
	  modcompEnv.exp.add(trust_entry(MF2, A2, AbsInt, Lambda)),
	fail. %% loop
gen_defines(_).
}.

absint_name_to_analyzer(sht) := sht_analyzer :- !.
absint_name_to_analyzer(X)   := _ :-
	% TODO: should be an error in compiler/frontend.pl
	errlog:temperror(['unknown analysis ', X]).

typeprop_kind(box, predname).
typeprop_kind(unbox, predname).
typeprop_kind(cons, predname).
typeprop_kind(imptype, ctype).
typeprop_kind(native, bool).

{
:- extends modcomp_ctx.
:- extends errlog_ctx.
% TODO: this check is not complete... since multifile defined in not related modules can still collide
check_multifile(F, A, Def) :-
	Mods = ~mod_itf__keys,
	maplist((''(OtherModule) :-
	  OtherItf = ~get_mod_itf(OtherModule),
	  ( OtherItf.export(F, A, PubProps1),
	    PubProps1 = pub_props(Def1, _, Visibility, _, _),
	    Visibility = vs_multifile,
            Def \== Def1 ->
	      ft_error(incompatible_multifile(F/A,Def,Def1,OtherModule))
	  ; true
	  )
        ), Mods).
}.

{
:- extends modcomp_ctx.
exp_priv_props(Ps0, Module, Ps) :-
	maplist((''(P0, P) :- 
          ( nonvar(P0), P0 = (specialize = Value0) ->
	      exp_specialize_props(Value0, Module, Value),
	      P = (specialize = Value)
	  ; nonvar(P0), P0 = (Prop = Value0) ->
	      Exp = ~modcompEnv.exp,
	      call((
		exp :: module_exp <- Exp,
	        norm_prop(Prop, Value0, Value)
              )),
	      P = (Prop = Value)
	  ; P = P0
          )
	), Ps0, Ps).
}.

exp_specialize_props(Ps0, Module, Ps) :-
	maplist((''(P0, P) :- 
	  ( P0 = on(CallTypes, rename(Predspec)) ->
	      ( Predspec = _/_ ->
                  % TODO: multifile preds are not handled properly
                  Qual = qual_m(Module),
  	  	  FA = Predspec
	      ; Predspec = QM0:FA0, FA0 = _/_ ->
	          Qual = qual_m(QM0),
	          FA = FA0
	      ; % TODO: check errors before!
	        errlog:bug(['in specialize prop, bad predspec ', Predspec]),
	        fail
	      ),
	      % TODO: multifile preds are not handled properly (right?)
	      call((
                m :: any <- Module,
		qualm :: any <- Qual,
	        mexpand__spec(FA, MFA)
              )),
	      P = on(CallTypes, rename(MFA))
	  ; P = P0
	  )
        ), Ps0, Ps).

{
:- extends modcomp_ctx.
:- extends errlog_ctx.
exp_preds(Module, Preds, Metadatas) :-
	modcompEnv.exp.add(counter(0)),
	% copy assertions 
	call((
	  module :: any <- Module,
	  exp_assertions
        )),
        % module-expand all clauses and group them in predicates
	call((
          bag :: m_dic,
	  call((
	    module :: any <- Module,
	    order :: accum(Order),
	    exp_clauses
          )),
	  % TODO: LonelyPreds: merge with compiler__expand? this code definition is correct for dynamic but not for other predicate kinds
          findall(pred_def(MF/A, []), lonely_decl(MF, A), LonelyPreds),
	  call(( preds :: accum(Preds), add_preds(LonelyPreds), bag_to_list(Order) ))
	)),
	call(( metadatas :: accum(Metadatas), gen_metadatas )).
}.

{
:- extends modcomp_ctx.
:- extends errlog_ctx.
:- fluid module :: any.
exp_assertions :-
	% TODO: do expansion per nested module
	Cs = ~findall(C0, modcompEnv.pli.module__assertion(_, C0)),
	( member(C, Cs),
	    ( module_expansion(C, ~module, C2) ->
	        modcompEnv.exp.add(expanded_assertion(C2))
	    ; true
	    ),
	    fail
	; true
	).
}.

{
:- extends modcomp_ctx.
:- extends errlog_ctx.
:- fluid bag :: m_dic.
:- fluid order :: accum.
:- fluid module :: any.
exp_clauses :-
	% TODO: do expansion per nested module
	Cs = ~findall(C0, modcompEnv.pli.clauses(C0)), % TODO: avoid findall/3
	maplist(([bag, order] -> ''(C) :-
          ( module_expansion(C, ~module, C2) ->
	      ( C2 = c(H, _, _) ->
		  functor(H, MF, A),
		  bag_put(MF/A, C2)
	      ; C2 = a(_,_,_,_) -> % TODO: used?
		  modcompEnv.exp.add(expanded_assertion(C2))
	      )
	  ; true
	  )
        ), Cs).

:- '$ctxprj'(bag_put/2, [bag, order]).
bag_put(Key, X) :-
	( Xs = ~bag.get(Key) ->
	    bag.replace(Key, [X|Xs])
	; bag.lookup(Key, [X]),
	  order.add(Key)
	).

:- '$ctxprj'(lonely_decl/2, [modcompEnv, u(bag)]).
% a pred declaration without code...
lonely_decl(MF, A) :-
	modcompEnv.exp.expanded_decl(MF, A, _),
	\+ bag.get(MF/A, _).
}.

{
:- extends modcomp_ctx.
:- fluid preds :: accum.
add_preds(Xs) :-
	maplist(([preds] -> ''(X) :-
          X = pred_def(NA, Code),
	  Exp = ~modcompEnv.exp, % TODO: exp not fluid
	  call((
	    exp :: module_exp <- Exp,
	    PredId = ~predicate_x.reg_new(NA)
          )),
	  PredId.set_code(Code),
	  preds.add(PredId)
	), Xs).
}.

:- use_module(library(dict)).

{
:- extends modcomp_ctx.
:- fluid bag :: m_dic + u.
:- fluid preds :: accum.
bag_to_list(Keys) :-
	maplist(([preds] -> ''(Key) :-
          Xs0 = ~bag.get(Key),
	  Xs = ~reverse(Xs0),
	  Key = MF/A,
	  Exp = ~modcompEnv.exp, % TODO: exp not fluid
	  call((
            exp :: module_exp <- Exp,
	    PredId = ~predicate_x.reg_new(MF/A)
          )),
	  PredId.set_code(Xs),
	  preds.add(PredId)
        ), Keys).
}.

{
:- extends errlog_ctx.
module_expansion(a(AssrtStatus, AssrtType, AssrtBody0, Loc), Module, A) :- !,
	Errs = ~get_errs,
	Errs.add_loc(Loc),
	( mexpand__assertion(AssrtBody0, Module, AssrtBody) ->
	    Ok = yes
	; Errs.compiler_error(assertion_not_expanded(AssrtBody0)),
	  Ok = no
	),
	Errs.del_loc,
	Ok = yes,
	A = a(AssrtStatus, AssrtType, AssrtBody, Loc).
module_expansion(c(H, B, Loc), Module, C) :- !,
	Errs = ~get_errs,
	Errs.add_loc(Loc),
	errlog:loc_dict(Loc, Dict),
        ( expand_clause(Errs, H, B, Module, Dict, H1, B1),
	  mexpand__clause(H1, B1, Module, H2, B2) ->
	    Ok = yes
	; Errs.compiler_error(clause_not_expanded(H, B)),
	  Ok = no
	),
	Errs.del_loc,
	Ok = yes,
	C = c(H2, B2, Loc).
}.

% ---------------------------------------------------------------------------

% note: to know if the program uses runtime data or not, this predicate must be called after the program clauses are expanded
{
:- extends modcomp_ctx.
:- fluid metadatas :: accum.
gen_metadatas :-
	gen_metadatas__2(u),
	gen_metadatas__2(meta_args),
	gen_metadatas__2(context),
	gen_metadatas__2(exports),
	gen_metadatas__2(instvar), % OO extension
	gen_metadatas__2(instdata), % OO extension
        ( get_uses_runtime_data ->
	    gen_metadatas__2(imports),
	    gen_metadatas__2(multifile),
	    gen_metadatas__2(defines),
	    gen_metadatas__2(imports_all)
	; true
	).

gen_metadatas__2(What) :-
	findall(Head, gen_metadatas__3(What, Head), Cs),
	( Cs == [] ->
	    true
	; metadatas.add(md(What, Cs))
	).
}.

{
:- extends modcomp_ctx.
gen_metadatas__3(u, Head) :-
	modcompEnv.ideps.imported(IM, _),
          Head = u(IM).
gen_metadatas__3(meta_args, Head) :-
        % Info about predicate typing (if no runtime expansion is used for 
        % this module then only exported predicates are included here).
	pdecl(F, A, MF),
	  ( pinfo(MF, A, Meta, _, _) -> true ; fail ),
	  gen_metadatas__required_runtime_data(F, A),
	  Meta \== 0,
	  Meta =.. [_|Types],
	  Meta2 =.. [MF|Types],
	  Head = meta_args(Meta2).
gen_metadatas__3(context, Head) :-
	pdecl(F, A, MF),
	  ( pinfo(MF, A, _, _, Context) -> true ; fail ),
	  gen_metadatas__required_runtime_data(F, A),
	  Context \== none,
	  Head = context(MF, A, Context).
gen_metadatas__3(exports, Head) :-
	modcompEnv.sym.exports(F, N, MF),
	  Head = exports(F, N, MF).
gen_metadatas__3(imports, Head) :-
        imports(F, N, IM, _EM, not_all, MF),
          Head = imports(F, N, IM, MF).
gen_metadatas__3(defines, Head) :-
        ( pdecl(F, N, MF)
        ; reexports(F, N, MF) % TODO: right? problem: dynamic export will not work
	),
	  Head = defines(F, N, MF).
gen_metadatas__3(imports_all, Head) :-
	modcompEnv.pli.get_imports_all(ImportedSpec),
	  spec_defines_module(ImportedSpec, IM),
	  Head = imports_all(IM).
% OO extensions
gen_metadatas__3(instvar, Head) :-
	ModuleS = ~modcompEnv.itf.get_top_module,
	ModuleS.field(N, _),
	  Head = instvar(N).
gen_metadatas__3(instdata, Head) :-
	ModuleS = ~modcompEnv.itf.get_top_module,
	ModuleS.data(_RF, Arity, F),
	  Head = instdata(F, Arity).

% true if runtime data is required or the predicate is exported
gen_metadatas__required_runtime_data(F, N) :-
	( get_uses_runtime_data -> % because of runtime expansions
	    true
	; % because it is a exported predicate
	  modcompEnv.sym.exports(F, N, _) -> true
	).
}.

% ---------------------------------------------------------------------------

:- use_module(compiler(semantic_translation)).

{
:- extends modcomp_ctx.
:- extends errlog_ctx.
activate_translation(Module) :-
	activate_translation__load_transform,
	activate_translation__add_srcdbg_expand(Module),
	activate_translation__add_trans_hook(Module),
        expand_init(~get_errs, Module).
}.

{
:- extends modcomp_ctx.
activate_translation__load_transform :-
	modcompEnv.deps.transform(CompSpec),
	  store:denormalized_spec(CompSpec, CompUspec),
	  translation_module_holder:do_use_module(CompUspec),
	  fail.
activate_translation__load_transform.

activate_translation__add_srcdbg_expand(Module) :-
	% TODO: add package priorities and recode it as any other trans
	( modcompEnv.pli.pragma(insert_debug_info) ->
	    add_srcdbg_expand(Module)
	; true
	).
}.

{
:- extends modcomp_ctx.
:- extends errlog_ctx.
% Add all translation hooks for the semantic translation ('goal' and 'clause') 
activate_translation__add_trans_hook(Module) :-
        Decl = add_trans_hook(Kind, Pred, Prior),
	modcompEnv.pli.pragma(Decl),
	  ( translation_module_holder:do_meta_exp_spec(Pred, Pred2),
	    semantic_translation:add_trans_hook(Module, Kind, Pred2, Prior) ->
	      true
	  ; ft_error(decl_failed(~decl_from_hook(Kind, Pred, Prior)))
	  ),
          fail.
activate_translation__add_trans_hook(_).
}.

deactivate_translation(Module) :-
	expand_end(Module).

% The culprit declaration for a given translation hook
% (just for error reporting)
decl_from_hook(clause, Pred, Prior, add_clause_trans(Pred, Prior)).
decl_from_hook(goal, Pred, Prior, add_goal_trans(Pred, Prior)).

% ---------------------------------------------------------------------------
% note: F is the first argument to improve the indexing! 

% TODO: move elsewhere
% TODO: data + instance = inst_data (currently, it asserts the whole state, which is nonsense)
:- data pdecl/3. % TODO: merge with module_jsexp_:module__pred/4
:- data reexports/3.
:- data imports/6.
:- data pinfo/5.

:- pred redefining(F, A).
:- data redefining/2.

% Extensions (fluids, classes, etc.)
:- data class_expand/0.

% Extensions for functional expansion
:- data fun_eval/2.
:- data eval_arith/0.
:- data functional_expand/0.

delete_module_data :-
        retractall_fact(pdecl(_,_,_)),
        retractall_fact(reexports(_,_,_)),
        retractall_fact(imports(_,_,_,_,_,_)),
        retractall_fact(pinfo(_,_,_,_,_)),
        retractall_fact(redefining(_,_)),	
        retractall_fact(frontend:fun_eval(_,_)),
        retractall_fact(frontend:eval_arith),
        retractall_fact(functional_expand),
        retractall_fact(class_expand),
	% TODO: one for each nested module
	module_x.clean.

:- use_module(engine(rt_exp), ['$user_module_id'/1, '$module_concat'/3]).
:- include(compiler(mexpand)).

mexpand__goal_trans(Module, T) :-
	m :: any <- Module,
	semantic_translation:get_translation_hook(goal, KV), % (nondet)
	semantic_translation:pqueue_values([KV], [T]).
mexpand__fun_eval(X, Z) :-
	frontend:fun_eval(X, Z).
mexpand__eval_arith :-
	frontend:eval_arith.
mexpand__option(functional_expand) :-
	functional_expand.
mexpand__option(class_expand) :-
	class_expand.
mexpand__uses_rt_metacast(Term, KnownMetatype, ExpectedMetatype) :-
	% TODO: implement per-module global variables
	'$global_vars_get'(2, static_data(Memoize, ModcompEnv)),
	memo :: memoize <- Memoize,
	modcompEnv :: modcomp_env <- ModcompEnv,
	uses_runtime_expansion(Term, KnownMetatype, ExpectedMetatype).
mexpand__uses_hiord :-
	% TODO: implement per-module global variables
	'$global_vars_get'(2, static_data(_, ModcompEnv)),
	modcompEnv :: modcomp_env <- ModcompEnv,
	get_uses_hiord.
mexpand__uses_hiord_pred(N, A) :-
	% TODO: implement per-module global variables
	'$global_vars_get'(2, static_data(_, ModcompEnv)),
	modcompEnv :: modcomp_env <- ModcompEnv,
	get_uses_hiord_pred(N, A).

mexpand__error(Error) :-
	static_errs(Errs),
	Errs.compiler_error(Error).

% ---------------------------------------------------------------------------

% TODO: a bit dirty...
mexpand__prop_check(H, NH, _M) :-
	functor(H, F, N),
	functor(NH, MF, _), % ignore expanded arity, however it should be N
	% TODO: does N change after expansion (see: context)? maybe those access to pinfo are not correct...
	( pinfo(MF, N, _, false, _) ->
	    mexpand__error(not_prop(F, N))
	; true
	).

% warn if a non-redefined predicate is imported from anywhere else
% TODO: use redefining when the predicate is imported?
% TODO: precompute??
mexpand__check_dups(F, N) :-
        redefining(F, N), !.
mexpand__check_dups(F, N) :- !,
        ( pdecl(F, N, _) ->
	    ( imports(F, N, IM, _, _, _) ->
	        % locally defined but also imported
	        mexpand__error(imported_needs_qual(F, N, IM))
	    ; true
	    )
        ; imports(F, N, IM, EM, _, _) ->
	    ( imports(F, N, IM0, EM0, _, _),
	      EM0 \== EM -> 
                % imported from two different modules IM and IM0 and not 
                % pointing to the same implementation EM0:F/N and EM:F/N
	        mexpand__error(imported_needs_qual(F, N, IM0, IM))
	    ; true
	    )
	; true
	).

:- use_module(compiler(assertions__common)).

mexpand__assertion(Ass0, Module, Ass) :-
       	assertion_body(PD0,DP0,CP0,AP0,GP0,CO,Ass0),
	% TODO: use MetatypeDic?
        call((
          vEnv :: v_env <- ~v_env.empty,
	  pEnv :: p_env,
	  m :: any <- Module,
	  qualm :: any <- qual_m(Module),
          mexpand__head(PD0, PD, _FinalTbl),
	  mexpand__props(DP0, DP),
	  mexpand__props(CP0, CP),
	  mexpand__props(AP0, AP),
	  mexpand__props(GP0, GP)
        )),
       	assertion_body(PD,DP,CP,AP,GP,CO,Ass).

{
:- fluid vEnv :: v_env.
:- fluid pEnv :: p_env + u.
:- fluid m :: any.
mexpand__props(As, NAs) :-
	maplist((''(A, NA) :-
          % TODO: move to a separate predicate
%	  call((
%            vEnv :: v_env <- ~v_env.no_rt,
            ( var(A) ->
	        NA = A % TODO: why?
	    ; get_qualifier(A, Qual, A0),
	      call((
                ignorefun :: any <- false,
	        residue :: accum(NA0),
		mexpand__goal__1(Qual, A0)
%		trace(a(Qual, A0, NA0))
              )),
	      NA0 = [NA], % TODO: What error emit if NA0 is not a list of one element?
	      mexpand__prop_check(A0, NA, ~m)
	    )
%          ))
        ), As, NAs).
}.

{
:- extends modcomp_ctx.
:- extends errlog_ctx.
% Mark that the module uses runtime 'metacast' (runtime expansion) and
% warn the user, if required.
uses_runtime_expansion(Term, KnownMetatype, ExpectedMetatype) :-
	( modcompEnv.pli.pragma(allow_runtime_expansions) ->
	    true % Allow runtime expansions, no error
	; % Show an error
	  ( \+ get_uses_runtime_data -> % First time, explain the error
	      ft_error(runtime_expansions)
	  ; true
	  ),
	  ft_error(runtime_expansion_culprit(Term, KnownMetatype, ExpectedMetatype))
	),
	% Mark that the module uses runtime data
	mark_uses_runtime_data.
}.

{
:- extends modcomp_ctx.
% Mark that the module does arbitrary hiord calls
get_uses_hiord :-
	Exp = ~modcompEnv.exp,
	( Exp.uses_hiord ->
	    true
	; Exp.add(uses_hiord)
	).
}.

{
:- extends modcomp_ctx.
% Mark that the predicate may be called from an arbritrary location through a hiord call
get_uses_hiord_pred(N, A) :-
	Exp = ~modcompEnv.exp,
	( Exp.uses_hiord_pred(N, A) ->
	    true
	; Exp.add(uses_hiord_pred(N, A))
	).
}.

% ---------------------------------------------------------------------------
:- doc(subsection, "Load the default abstract machine definition").

{
:- fluid memo :: memoize.

:- public load_absmach/0.
load_absmach :-
	absmach_spec(AbsmachSpec),
	module_ipexp:load_def(AbsmachSpec).
}.

absmach_spec([engine, absmach_def]).

% ---------------------------------------------------------------------------
:- doc(subsection, "Analysis and code generation (WAM and low-level)").

:- use_module(.(ptoc__jump_opt)).

{
:- extends modcomp_ctx.
:- extends errlog_ctx.
codegen(Spec, Preds0, Bin) :-
	% Read foreign interface ttr
	% TODO: a bit dirty...
        LoadGluecodeTTR = ( modcompEnv.exp.pragma(load_gluecode_ttr) ? yes | no ),
        ( LoadGluecodeTTR = yes ->
	    foreign__gluecode:load_ttr_from_exp(~modcompEnv.exp)
	; true
	),
	%
	Metadatas = ~modcompEnv.exp.metadatas,
%	add_exported_props(Metadatas),
	%
	Bin = ~module_bin.new,
	%
	Errs = ~memo.errs,
	'$global_vars_get'(3, OldErrs),
	'$global_vars_set'(3, Errs),
	%
	% Normalize predicates
	Exp = ~modcompEnv.exp,
        call((
          exp :: module_exp <- Exp,
	  compiler__expand:norm_preds(Preds0, PredsB0),
	  exp.preddic__register_preds(PredsB0),
	  % Get list of entries
	  AbsInt = sht_analyzer,
	  % TODO: enable warning (and add a pragma to disable it, like
	  % for runtime expansions).
	  % TODO: some predicates created in compilation passes (like
	  % those for disjunctions) may not be called in hiord calls,
	  % improve analysis precision by marking those internal
	  % predicates.
%	  ( exp.uses_hiord ->
%	      errlog:trace(['warning: some predicates in module ', ~exp.defines_module, ' that can be called through high-order calls are unknown (assuming top entries for all predicates in the module, analysis precision may be very low)'])
%	  ; errlog:trace(['warning: all predicates in module ', ~exp.defines_module, ' can be called through high-order calls are known'])
%          ),
          call((
            intr :: absint <- AbsInt,
	    ptoc__analyze:get_entries(PredsB0, EntriesB0)
          )),
          call((
            intr :: absint <- idet_analyzer,
	    ptoc__analyze:get_entries(PredsB0, EntriesIDet)
	  )),
          % Type and sharing analysis + indexer transformation
          % TODO: this is one of the slowest passes
          _ = ~analyze(AbsInt, no, EntriesB0, no), % sht analysis
          _ = ~analyze(trivial_analyzer, yes(sht_analyzer), EntriesB0, no), % indexing transformation
          _ = ~analyze(AbsInt, no, EntriesB0, yes), % sht analysis (over the transformed program) + annotation
          PredsB1b = ~analyze(idet_analyzer, no, EntriesIDet, no), % idet_analyzer analysis
          % Dump analysis results
          analysis_dump(AbsInt, Spec, PredsB1b),
          %
          ptoc__lowcomp:reg_analyze(PredsB1b),
          % Subpredicate analysis
          ptoc__jump_opt:new_analyze_subpreds(PredsB1b, NewSubpredDic),
          call((
            allpreds :: any <- PredsB1b,
	    subpreddic :: any <- NewSubpredDic,
	    PredsB1 = ~(ptoc__jump_opt:new_unfold_subpreds(PredsB1b))
          )),
          % ImProlog compilation
          % TODO: also understands the lowcomp output, split in a separate pass?
          '$absmach'(Absmach),
          ( exp.pragma('$emit_emulator') ->
              UseImpexp = no,
              Absmach2 = Absmach
          ; Exp = ~exp, findall(Ip, Exp.pragma(ip(Ip)), Ips),
            module_ipexp:load_from_list(Ips, Absmach, Impexp),
            UseImpexp = yes,
            Absmach2 = Impexp
          ),
          % Bytecode and annotated bytecode compilation
          % TODO: this is one of the slowest passes
          % TODO: merge the next two phases
          call(( preds :: accum(PredsB5), comp_preds(PredsB1) )),
          ptoc__impcomp:comp(Absmach2, PredsB5, Bin, NativeCode, NativeHeader),
          %
          ( UseImpexp = yes ->
              '$inst_destroy'(Impexp)
          ; true
          )
        )),
        %
        '$global_vars_set'(3, OldErrs),
        %
	% Save --
	Module = ~modcompEnv.exp.defines_module,
	% Bytecode
	InitName = ~atom_concat(~encode_symbol_a(Module), '__init'),
	EndName = ~atom_concat(~encode_symbol_a(Module), '__end'),
 	save_bytecode(Spec, Module, InitName, EndName, Metadatas, PredsB5),
	Bin.add(contains_bytecode),
	% Native external source
	( modcompEnv.exp.native_include_c_source(CSourceSpec),
	  Bin.add(native_include_c_source(CSourceSpec)),
	  fail
	; true
	),
	% Native external headers
	( modcompEnv.exp.native_include_c_header(CHeaderSpec),
	  Bin.add(native_include_c_header(CHeaderSpec)),
	  fail
	; true
	),
	% Write native code (only if necessary)
	( NativeCode = [] ->
	    true
	; write_native(NativeCode, Spec, Module),
	  Bin.add(contains_native),
	  % TODO: can external libraries be useful even if NativeCode = []?
	  ( modcompEnv.exp.pragma(use_foreign_library(Lib)),
	    Bin.add(native_library_use(Lib)),
	    fail
	  ; true
	  ),
	  ( modcompEnv.exp.pragma(foreign_library_path(LibPath)),
	    Bin.add(native_library_path(LibPath)),
	    fail
	  ; true
	  )
	),
	% TODO: merge with previous
	( NativeHeader = [] ->
	    true
	; write_nativeh(NativeHeader, Spec, Module),
	  Bin.add(contains_native_h)
	),
	%
	% clean ttr information
	% TODO: a bit dirty...
        ( LoadGluecodeTTR = yes ->
	    foreign__gluecode:clean_ttr
	; true
	).
}.

:- use_module(compiler(write_c), [encode_symbol_a/2]).

{
:- fluid exp :: module_exp.
:- fluid preds :: accum.
comp_preds([]) :- !.
comp_preds([Pred0|Preds0]) :-
	comp_pred(Pred0),
	comp_preds(Preds0).

comp_pred(PredId) :-
	trust(PredId instance_of predicate_x),
	( CompMode = ~PredId.get_prop(compmode) ->
	    ( CompMode = bytecode ->
		Code = ~PredId.code,
		Code = icode(a, Args, or(Cs)),
		bytecode__compiler:compile_clauses(Cs, Args, Cs2),
		PredId.set_code(bytecode(Cs2)),
		preds.add(PredId)
	    ; CompMode = bytecodehook ->
		Code1 = ~hook_code(PredId),
	        % TODO: index is ignored??!
	        Code1 = icode(a, Args, or(Cs)),
	        bytecode__compiler:compile_clauses(Cs, Args, Cs2),
	        % TODO: emit only one definition?? it is wrong to create a new predicate with the same name...
		PredId2 = ~predicate_x.new(~PredId.name),
		PredId2.set_code(bytecode(Cs2)),
%		errlog:trace([name(Name)]),
%		errlog:trace([code_________(Code)]),
%		errlog:trace([code_bytecode(Code1)]),
	        preds.add(PredId2),
	        preds.add(PredId)
	    ; CompMode = lowcomp ->
		lowcomp_pred(PredId)
	    )
	; % TODO: add a compmode for the rest!
          preds.add(PredId)
	),
	!.
comp_pred(PredId) :-
	trust(PredId instance_of predicate_x),
	errlog:bug(['comp failed for predicate ', ~PredId.name]),
	fail.
}.

hook_code(PredId) := icode(a, Args, or([[Call]])) :-
	Call = ~strgoal.new_f(PredId),
	Args = ~Call.args.

% ---------------------------------------------------------------------------
% Write analysis results

{
:- fluid exp :: module_exp.

analysis_dump(AbsInt, Spec, Preds) :-
        % TODO: create another pragma to enable dump?
	( uses_any_analysis ->
	    store:addr_new(compile__dump(Spec), DumpName),
	    open_and_protect(DumpName, OutputStream, Ref),
	    ( uses_analysis(AbsInt) ->
	        call((
	          intr :: absint <- AbsInt,
		  ptoc__analyze:dump(OutputStream, Preds)
		))
	    ; true
	    ),
	    ( uses_analysis(idet_analyzer) ->
	        call((
	          intr :: absint <- idet_analyzer,
	          ptoc__analyze:dump(OutputStream, Preds)
                ))
	    ; true
	    ),
	    close(OutputStream),
	    end_protect(Ref)
	; true
	).

uses_any_analysis :- 
	( exp.pragma(analyze_all) -> true
	; exp.pragma(analyze_idet) -> true
	; fail
	).

uses_analysis(sht_analyzer) :- !,
	( exp.pragma(analyze_all) -> true
	; fail
	).
uses_analysis(idet_analyzer) :- !,
	( exp.pragma(analyze_all) -> true
	; exp.pragma(analyze_idet) -> true
	; fail
	).
}.

% ---------------------------------------------------------------------------
% Write native code (C code)

:- use_module(compiler(write_c)).

write_native(Code, Spec, Module) :-
	store:addr_new(compile__c(Spec), NativeName),
	w :: c_writer <- ~c_writer.new,
	w.to_file(NativeName, Module, Code).

write_nativeh(Code, Spec, Module) :-
	store:addr_new(compile__h(Spec), NativeName),
	w :: c_writer <- ~c_writer.new,
	w.to_file(NativeName, Module, Code).

% ---------------------------------------------------------------------------
% Assemble bytecode into ql files

:- use_module(library(sort)).
:- use_module(library(ctrlcclean), [ctrlcclean/0]).

:- use_module(engine(ql_inout)).

% TODO: write simplified form of metadata clauses

{
:- extends modcomp_ctx.
save_bytecode(Spec, M, InitName, EndName, Metadatas, Preds) :-
	store:addr_new(compile__emu(Spec), QlName),
	spec_to_key(Spec, SpecKey),
	% TODO: ql_inout is not thread safe
        open_and_protect(QlName, OutputStream, Ref),
	% ql_inout depends on an particular set of absmach definitions
        % (e.g. to know the size of integers, etc.). The first
        % argument of '$qwrite_begin' is set to 1 to instruct it to
        % use absnext instead of the current absmach definitions
        % TODO: improve (passing 1 is not very clean)
	'$qwrite_begin'(1, OutputStream),
	'$qwrite'(M),
	'$qwrite'(SpecKey),
	'$qwrite'(InitName),
	'$qwrite'(EndName),
	emit_metadatas(Metadatas),
	filter_bytecode_preds(Preds, Preds2),
%	length(Preds, CountB),
	length(Preds2, Count),
%	( CountB = Count -> true ; display(user_error, b(CountB, Count, Preds)), nl(user_error) ),
	'$qwrite'(c), % emit predicate definition count
	'$qwrite'(Count),
	emit(Preds2),
	'$qwrite'(e(0)), % mark the end
	'$qwrite_end',
        close(OutputStream),
        end_protect(Ref).
}.

emit_metadatas(Xs) :-
	maplist((''(X) :-
	  X = md(What, Cs),
	  length(Cs, NCs),
	  '$qwrite'(What),
	  '$qwrite'(NCs),
	  maplist((''(C) :- emit_metadata(C)), Cs)
        ), Xs).

emit_metadata(C) :-
	( C = meta_args(Meta2) -> '$qwrite'('multifile:$meta_args'(Meta2))
	; C = context(MF, A, Context) -> '$qwrite'('multifile:$context'(MF, A, Context))
	% TODO: use =.. and specialize?
	; C = u(IM) -> '$qwrite'(IM)
	; C = exports(F, N, MF) -> '$qwrite'(F), '$qwrite'(N), '$qwrite'(MF)
	; C = defines(F, N, MF) -> '$qwrite'(F), '$qwrite'(N), '$qwrite'(MF)
	; C = imports_all(IM) -> '$qwrite'(IM)
	; C = imports(F, N, IM, MF) -> '$qwrite'(F), '$qwrite'(N), '$qwrite'(IM), '$qwrite'(MF)
	% OO extensions
	; C = instvar(N) -> '$qwrite'(N)
	; C = instdata(N, A) -> '$qwrite'(N), '$qwrite'(A)
	).

filter_bytecode_preds(Xs, Ys) :-
	preds :: accum(Ys), 
	maplist(([preds] -> ''(PredId) :-
	  trust(PredId instance_of predicate_x),
	  Code = ~PredId.code,
	  ( ( Code = bytecode(_) ; Code = int(_) ) -> preds.add(PredId) ; true )
        ), Xs).

{
:- extends modcomp_ctx.
emit(Xs) :-
	maplist((''(PredId) :-
	  trust(PredId instance_of predicate_x),
	  Code = ~PredId.code,
	  ( Code = bytecode(Cs) ->
	      emit__d(~PredId.name),
	      maplist((''(C) :- emit__bc(C)), Cs)
	  ; Code = int(Cs) ->
	      emit__d(~PredId.name),
	      maplist((''(C) :- emit__ic(C)), Cs)
	  ; true
  	  )
	), Xs).

emit__d(Name) :-
	Bits = ~modcompEnv.exp.pred__defbits(Name),
	'$qwrite'(d(Name, Bits)).
}.

:- '$ctxprj'(emit__bc/1, []).
emit__bc(clause(Code, Data)) :-
	'$absmach'(Absmach),
	Absmach.asm_insns(Code, Size, Tokens),
	'$qwrite_b'(b([Size|Tokens],Data)).

:- '$ctxprj'(emit__ic/1, []).
emit__ic(c(Head, Body, _)) :-
        ( Body = 'basiccontrol:true' ->
	    '$qwrite'(f(Head))
	; '$qwrite'(i(Head, Body))
	).
:- endif.
