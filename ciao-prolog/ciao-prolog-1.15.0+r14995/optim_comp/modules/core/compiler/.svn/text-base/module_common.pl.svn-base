% (This file is instantiated from module_*.pl)
% ===========================================================================
:- if(( module_projection(deps)
      ; module_projection(itf)
      ; module_projection(pli) )).
:- doc(section, "Definition of modules").
% Three versions of this class are instantiated:
%   - deps: Dependencies of the module
%   - itf:  Interface projection
%   - pli:  All the module contents (includes deps and itf)
%
% The 'interface projection' represents subsets of the module
% properties that is visible to other modules. It stores public
% information (like the module name exported predicates, exported
% properties, etc.). Alternatively, private information can be stored
% inside a @tt{itf} only if this improves the compilation or analysis
% of the module which uses this @tt{itf}.
%
% It is convinient to have short @tt{itf} and @tt{deps} objects:
%  - it requires less resources for compilation
%  - cheaper recompilations (a module only needs to be recompiled if
%    the @tt{itf} of the imported modules has changed)
%
% ---------------------------------------------------------------------------

:- use_module(library(aggregates), [findall/3]).

:- public data defines_module/1.

% ---------------------------------------------------------------------------
:- if(( module_projection(deps) ; module_projection(pli) )).
:- doc(subsection, "Dependencies of the module").
:- public data import/1.
:- public import__list/1.
import__list := ~findall(Spec, import(Spec)).
:- public data transform/1.
:- public data include/1.
:- public include__list/1.
include__list := ~findall(Spec, include(Spec)).
:- public data options/1.
:- endif. % deps ; pli
% ---------------------------------------------------------------------------

% ---------------------------------------------------------------------------
:- if(( module_projection(itf) ; module_projection(pli) )).
:- doc(subsection, "Module exports").

:- public reexport__list/1.
:- public data reexport/1.
:- pred reexport(Spec) # "The module that imports this module_itf also
   needs to include the module_itf of reexported modules".
reexport__list := ~findall(Spec, reexport(Spec)).
:- if(module_projection(itf)).
:- public data export/3.
:- pred export(F, A, PubProps) # "The predicate @var{F}/@var{A} is exported with properties @var{PubProps}".
:- endif.
:- public data '$reexports'/3.
:- pred '$reexports'(Spec, F, A) # "Predicate @var{F}/@var{A} is reexported from @var{Spec}".
:- public data '$reexports_all'/1.
:- pred '$reexports_all'(Spec) # "All predicates in @var{Spec} are reexported".

% TODO: move to a module_s_itf record 
% TODO: poor indexing, move F/A first
:- public get_reexports/3.
get_reexports(Spec, F, A) :- '$reexports'(Spec, F, A).

% TODO: move to a module_s_itf record 
:- public get_reexports_all/1.
get_reexports_all(Spec) :- '$reexports_all'(Spec).
:- endif. % itf ; pli

% ---------------------------------------------------------------------------
:- if(module_projection(pli)).
:- doc(section, "Module contents").
% (similar to the .i files in C, but without the interface information of the modules that it imports)
% ---------------------------------------------------------------------------

:- attr spec :: any.

:- data imports_nocheck/3.
:- pred imports_nocheck(Module, F, A)
        # "The predicate @var{F}/@var{A} is imported from module @var{Module} using
           @decl{import/2}.".

:- data should_be_usermod/1.
:- pred should_be_usermod(ImpSpec)
        # "The imported module @var{ImpSpec} should be a user module.".

% TODO: poor indexing?
:- data '$imports'/3.
:- pred '$imports'(ImpSpec, F, A) # "Imports the predicate @var{F}/@var{A} of @var{ImpSpec}".

% TODO: poor indexing?
:- data '$imports_all'/1.
:- pred '$imports_all'(ImpSpec) # "Imports all predicates of @var{ImpSpec}.".

:- data def_redefining/2.
:- pred def_redefining(F, A).

:- data def_pred/3.
:- pred def_pred(F, A, PubProps).

% ---------------------------------------------------------------------------

% Extensions for functional expansion (valid when pragma fuctional_expand is enabled)
:- public data fun_eval/2. % fun_eval(F, A)
:- public data eval_arith/0. % eval_arith

:- public data clauses/1.

:- public data expansion_check/1.
:- pred expansion_check(Pred) # "Expansion check @var{Pred} that must
   be executed during import/export checks before compilation.".

:- public data native_include_c_source/1.
:- pred native_include_c_source(Spec) # "Includes native C source @var{Spec}".
:- public data native_include_c_header/1.
:- pred native_include_c_header(Spec) # "Includes native C header @var{Spec}".
:- public data native_inline/1.
:- pred native_inline(Decl) # "Native inline code @var{Decl}".
:- public data priv_props/3.
:- pred priv_props(F, A, Props) # "Predicate properties local to the module".
:- public data forced_props/4.
:- pred forced_props(F, A, M, Props) # "Forced properties about imported modules".
:- public data ptoc_type/2.
:- pred ptoc_type(Type, Def) # "ptoc type".
:- public data ptoc_typeprop/3.
:- pred ptoc_typeprop(Type, Op, Name) # "ptoc type operation".
:- public data ptoc_imptype_c/2.
:- pred ptoc_imptype_c(Type, Def) # "ptoc imptype_c".
% TODO: there can be more than one entry per F/A... pass as a list, like it is done with the predicates
:- public data trust_entry/4.
:- pred trust_entry(F, A, AbsInt, Lambda).
:- public data pragma/1.
:- pred pragma(Pragma).

:- public get_imports/3.
get_imports(Spec, F, A) :-
	'$imports'(Spec, F, A).

:- public get_imports_all/1.
get_imports_all(Spec) :-
	'$imports_all'(Spec).

:- use_module(compiler(ptoc__props), [prop_merge_noexp/3]).
:- use_module(compiler(errlog)).

% ---------------------------------------------------------------------------

:- public data exported_binder/2.
:- pred exported_binder(F, A). 
:- public data forbid_def/2.
:- pred forbid_def(F, A).

% TODO: a kludge
:- public data trust_module_statemodel/2.

:- public data translation_hook/2.

% ---------------------------------------------------------------------------

is_fun_head(Head) :- nonvar(Head), Head = ':='(_, _).

% TODO: you should be able to do it later
replace_head_name(yes, (H0 := R), F, (H := R)) :- !,
	H0 =.. [_|As], H =.. [F|As].
replace_head_name(_, H0, F, H) :-
	H0 =.. [_|As], H =.. [F|As].

% ---------------------------------------------------------------------------
% Counter for identifiers of mod_open_anon nested module

%:- pred counter/1.
:- data counter/1.

% TODO: new names for contexts -- use sub_id
:- public new_anon_name/2.
new_anon_name(BaseName) := Atom :-
	( counter(Counter0) -> true ; Counter0 = 0 ),
	Counter1 is Counter0 + 1,
	del(counter(_)),
	add(counter(Counter1)),
	% TODO: do not use module concat
	atom_concat(BaseName, '_:$ctx_', Prefix),
	Atom = ~concat_auxnum(Prefix, Counter0).

:- static concat_auxnum/3.
concat_auxnum(Prefix, N) := ~atom_concat(Prefix, ~atom_concat('#', N2)) :-
	atom_codes(N2, ~number_codes(N)).

% ---------------------------------------------------------------------------
% Clean temporary data (not for element__save)

% TODO: Refine, is it necessary?
:- public clean_temp/0.
clean_temp :-
	del(pred__prop(_,_,_)),
%	del(defined_pred(_,_)),
%	del(has_clauses(_,_)),
	del(exported_pred(_,_)),
	del(module__prop(_,_)),
	del(exported_binder(_,_)),
	del(forbid_def(_,_)),
	del(trust_module_statemodel(_,_)),
	del(translation_hook(_,_)),
	del(counter(_)).

% ---------------------------------------------------------------------------
:- endif. % pli
% ---------------------------------------------------------------------------

:- if(( module_projection(itf)
      ; module_projection(pli) )).

:- if(module_projection(itf)).
% TODO: only useful for lpdoc/ciaopp?
:- public data decl/1.
:- pred decl(Decl) # "The module contains the declaration @var{Decl}
   as an itf-exported new_declaration.".
:- elif(module_projection(pli)).
:- pred decl(Decl, Loc)
        # "We have read from the module (or included files) the
           declaration @var{Decl}, at location @var{Loc}.". 
:- public data decl/2.
:- endif.

:- if(module_projection(pli)).
% TODO: A more complex visibility is needed. It must be a function
% that depends on the declaration (e.g. the declaration should not be
% exported if the declaration says something about a predicate and the
% predicate is private)
:- public data new_decl/2.
:- pred new_decl(Pred, Visibility) # "@var{This} has defined the
   declaration @var{Pred} with a new_declaration directive,
   @var{Visibility} is 'public' if the declaration is to be seen by
   other modules, 'private' otherwise.".
:- endif.

:- if(module_projection(itf)).
% TODO: merge with 'module_s'?
% TODO: add hook to allow 'serialization' of this part without considering all the previous 'data'
:- public class module_s_itf {
    % :- doc(title, "Structure for modules (interface projection)").
    :- attr itf :: module_itf.
    :- attr id :: any.

    :- constructor from_id_/2.
    from_id_(Itf, Id) :- ~itf = Itf, ~id = Id.
    :- public get_id/1.
    get_id := ~id.

    :- public set_name/1. % TODO: make it a constructor
    set_name(Name) :- itf.add(module__name(~id, Name)).
    :- public get_name/1. % TODO: here?
    get_name := Name :-
        itf.module__name(~id, Name0), !, Name = Name0.

    % ---------------------------------------------------------------------------

    :- public get_modif/1.
    get_modif := Modifier :- itf.module__modif(~id, Modifier).
    :- public add_modif/1.
    add_modif(Modifier) :- itf.add(module__modif(~id, Modifier)).

    % ---------------------------------------------------------------------------
    % Base(s) of this module/class

    :- public extendsR/1. % (nondet)
    extendsR := Base :- itf.module__extendsR(~id, Base).
    :- public add_extendsR/1.
    add_extendsR(Base) :- itf.add(module__extendsR(~id, Base)).

    % ---------------------------------------------------------------------------
    % Nested module navigation
    % TODO: duplicated in module_s, share

    % (nondet if N is unbound)
    :- public meta_predicate get_nested_module(?, out(module_s_itf)).
    get_nested_module(N) := Nested :-
	itf.module__nested_module(N, ~id, NestedId),
	Nested = ~module_s_itf.from_id(~itf, NestedId).

    :- public add_nested_module/2.
    add_nested_module(N, Nested) :-
	trust(Nested instance_of module_s_itf),
	itf.add(module__nested_module(N, ~id, ~Nested.get_id)).

    :- public nested_module_list/1.
    % List of modules nested in this module (only at depth 1)
    nested_module_list := Xs :-
        % Note: use small objects (Ids) inside findall for efficiency
%        Xs = ~findall(Nested, get_nested_module(_, Nested)).
        Xs0 = ~findall(NestedId, itf.module__nested_module(_, ~id, NestedId)),
        Xs = ~maplist((''(Id, Nested) :- Nested = ~module_s_itf.from_id(~itf, Id)), Xs0).

    % ---------------------------------------------------------------------------
    % Declared module/class attributes

    :- public field/2. % field(FieldName, Class) (nondet)
    field(FieldName, Class) :- itf.module__field(FieldName, ~id, Class). % module__field(FieldName, Id, FieldClass)
    :- public add_field/2.
    add_field(FieldName, Class) :- itf.add(module__field(FieldName, ~id, Class)). % module__field(FieldName, Id, Class)

    % ---------------------------------------------------------------------------
    % Declared module/class predicates

    :- public data/3. % (nondet)
    data(RF, A, F) :- itf.module__data(RF, A, ~id, F).
    :- public add_data/3.
    add_data(RF, A, F) :- itf.add(module__data(RF, A, ~id, F)).
    :- public method/3. % (nondet)
    method(A, B, C) :- itf.module__method(A, B, ~id, C).
    :- public add_method/3.
    add_method(A, B, C) :- itf.add(module__method(A, B, ~id, C)).
    :- public method_code/5. % (nondet)
    method_code(A, B, C, D, E) :- itf.module__method_code(A, B, ~id, C, D, E).
    :- public add_method_code/5.
    add_method_code(A, B, C, D, E) :- itf.add(module__method_code(A, B, ~id, C, D, E)).
    :- public virtual/5. % (nondet)
    virtual(A, B, C, D, E) :- itf.module__virtual(A, B, ~id, C, D, E).
    :- public add_virtual/5.
    add_virtual(A, B, C, D, E) :- itf.add(module__virtual(A, B, ~id, C, D, E)).

    % ---------------------------------------------------------------------------
    % State model 

    :- public selfname/1.
    selfname := SelfName :- itf.module__selfname(~id, SelfName).
    :- public add_selfname/1.
    add_selfname(SelfName) :- itf.add(module__selfname(~id, SelfName)).

    :- public statemodel/1. % statemodel(StateModel)
    statemodel := X :- itf.module__statemodel(~id, X).
    :- public add_statemodel/1.
    add_statemodel(X) :- itf.add(module__statemodel(~id, X)).

    % ---------------------------------------------------------------------------
    % Environment for implicits

    :- public getctx/1.
    getctx := Def :- itf.module__ctx(~id, Def).
    :- public addctx/1.
    addctx(Def) :- itf.add(module__ctx(~id, Def)).
}.
% --
:- elif(module_projection(pli)).
% --
:- public class module_s {
    % :- doc(title, "Structure for modules").

    % TODO: Ideally, a single 'module' interface would be enough even
    %   for runtime; with different incarnations (e.g., one for
    %   separate compilation, other for runtime, etc.).

    :- public attr pli :: module_pli # "Reference to pli".
    :- attr id :: any.

    :- constructor new_/0.
    new_ :-
        % TODO: ~id is unbound
        ~pli = ~module_pli.new.

    :- public meta_predicate query_nested_module(?, out(module_s)).
    query_nested_module(Name) := Module :-
        % TODO: merge with JS version
        ( pli.module__nested_module(Name, ~id, Id) ->
	    true
        ; Id = ~sub_id(~get_name, Name), /*QC*//*qualify name to get id?*/
	  Module = ~module_s.from_id(~pli, Id),
	  Module.set_enclosing_module(~self),
	  Module.set_name(Name),
	  add_nested_module(Name, Module)
	).

    :- public meta_predicate query_anon_module(out(module_s)).
    query_anon_module := Module :-
        Id = ~pli.new_anon_name(~get_name),
	Id = Name, % TODO: wrong, Id is not Name
	Module = ~module_s.from_id(~pli, Id),
	Module.set_enclosing_module(~self),
	Module.set_name(Id), % TODO: can we omit the name here?
	add_nested_module(Name, Module).

    :- static sub_id/3.
    sub_id(RootId, Name) := Id :-
	atom_concat(RootId, ':', N1),
	atom_concat(N1, Name, Id).

    :- public set_name/1. % TODO: make it a constructor
    set_name(Name) :- pli.add(module__name(~id, Name)).

    :- public set_enclosing_module/1.
    set_enclosing_module(Enclosing) :-
	trust(Enclosing instance_of module_s),
        EnclosingId = ~Enclosing.get_id,
        pli.add(module__enclosing(~id, EnclosingId)).

    :- public set_name_top/1.
    % (Special case for 'top' module)
    set_name_top(Name) :-
        ~id = Name,
        pli.del(defines_module(_)),
        pli.add(defines_module(Name)),
	set_name(Name).

    % ---------------------------------------------------------------------------
    % The name of the module (at source)

    :- public get_name/1.
    get_name := Name :-
        pli.module__name(~id, Name0), !, Name = Name0.

    % TODO: [Merge] missing in JS backend
    :- constructor from_id_/2.
    from_id_(Pli, Id) :- ~pli = Pli, ~id = Id.
    :- public get_id/1.
    get_id := ~id.

    :- public mod_spec/1.
    mod_spec := ~pli.spec.

    % ---------------------------------------------------------------------------    
    % Module modifiers:
    % 
    % * 'mod_static': module with static state (for which there exist a single instance)
    %
    % * 'mod_class': module with dynamic state (for which many instances can be created)
    %                (which can be backtrackable or not)
    %
    % * 'mod_interface': interface module
    %
    % * 'mod_mixin': mixin module.
    %
    % * 'mod_open_anon': open-anonymous nested module.
    %
    %   These are nested modules that are both open (no qualification is
    %   required) and anonymous (no given name). Their only purpose is
    %   simply the association of certain scope definitions to a group of
    %   predicates.

    :- public set_modifier/1.
    set_modifier(Modifier) :-
	add_modif(Modifier),
        set_modifier_(Modifier),
	% (special case, export the module symbol)
	( is_top_module, is_class_or_interface ->
	    Name = ~get_name,
	    add1_exported_sym(Name/*QC*//*Id or Name?*/)
	; true
	).

    set_modifier_(mod_static) :- !.
    set_modifier_(mod_open_anon) :- !.
    set_modifier_(mod_mixin) :- !,
        % The context for mixin is initially set to 'none'. Each
	% 'fluid' declaration will make it grow.
	addctx(none).
    set_modifier_(_Modifier) :- % TODO: what is this?
	add_selfname('self'). % TODO: make it user-customizable?

    % modifier := mod_class | mod_interface | ... . % TODO: add a type
    :- public get_modif/1.
    get_modif := Modifier :- pli.module__modif(~id, Modifier).
    :- public add_modif/1.
    add_modif(Modifier) :- pli.add(module__modif(~id, Modifier)).

    :- public is_static/0.
    is_static :- ~get_modif = mod_static.
    :- public is_class/0.
    is_class :-  ~get_modif = mod_class.
    :- public is_interface/0.
    is_interface :- ~get_modif = mod_interface.
    :- public is_open_anon/0.
    is_open_anon :- ~get_modif = mod_open_anon.
    :- public is_mixin/0.
    is_mixin :- ~get_modif = mod_mixin.
    % TODO: Find better names for them.
    :- public is_class_or_interface/0.
    is_class_or_interface :- (is_class ; is_interface), !.
    :- public is_static_or_class_or_interface/0.
    is_static_or_class_or_interface :-
	(is_static ; is_class ; is_interface), !.
    :- public is_open_anon_or_mixin/0.
    is_open_anon_or_mixin :- (is_open_anon ; is_mixin), !.

    % ---------------------------------------------------------------------------
    % Base(s) of this module/class

    :- public add_extendsR/1.
    add_extendsR(BaseId) :- pli.add(module__extendsR(~id, BaseId)).
    :- public extendsR/1. % extendsR(Base) (nondet)
    extendsR(BaseId) :- pli.module__extendsR(~id, BaseId).

    :- public add_extends/1. % (not resolved)
    add_extends(Base) :- pli.add(module__extends(~id, Base)).

    :- public all_extends/1.
    all_extends := ~findall(Base, pli.module__extends(~id, Base)).

    % ---------------------------------------------------------------------------
    % Enclosing module navigation

    :- public meta_predicate enclosing_module(out(module_s)).
    enclosing_module := Enclosing :-
        pli.module__enclosing(~id, EnclosingId),
	Enclosing = ~module_s.from_id(~pli, EnclosingId).

    % TODO: merge with JS version?
    :- public meta_predicate top_enclosing_module(out(module_s)).
    % Top enclosing module (the highest module that is a top module)
    top_enclosing_module := ~self :- is_top_module, !.
    top_enclosing_module := ~((~enclosing_module).top_enclosing_module).

    % TODO: add to JS version?
    :- public meta_predicate find_def_enclosing(out(module_s)).
    % Find the nearest enclosing module that can hold definitions
    % (i.e., non-anonymous).
    find_def_enclosing := R :- is_top_module, !, R = ~self.
    find_def_enclosing := R :- get_prop(static_scope), !,
	R = ~top_enclosing_module. % TODO: not really?
    find_def_enclosing := R :- is_static_or_class_or_interface, !,
	R = ~self.
    find_def_enclosing := R :-
        Enclosing = ~enclosing_module,
	R = ~Enclosing.find_def_enclosing.

    :- public is_top_module/0.
    % The module is a 'top module' (not a nested module)
    % TODO: use 'enclosing'
    is_top_module :- ~id = ~pli.defines_module.

    % ---------------------------------------------------------------------------
    % Nested module navigation

    % (nondet if N is unbound)
    :- public meta_predicate get_nested_module(?, out(module_s)).
    get_nested_module(N) := Nested :-
	pli.module__nested_module(N, ~id, NestedId),
	Nested = ~module_s.from_id(~pli, NestedId).

    :- public add_nested_module/2.
    add_nested_module(N, Nested) :-
	trust(Nested instance_of module_s),
	pli.add(module__nested_module(N, ~id, ~Nested.get_id)).

    :- public nested_module_list/1.
    % List of modules nested in this module (only at depth 1)
    nested_module_list := Xs :-
        % Note: use small objects (Ids) inside findall for efficiency
%        Xs = ~findall(Nested, get_nested_module(_, Nested)).
        Xs0 = ~findall(NestedId, pli.module__nested_module(_, ~id, NestedId)),
        Xs = ~maplist((''(Id, Nested) :- Nested = ~module_s.from_id(~pli, Id)), Xs0).

    :- public meta_predicate nested_star_find_class(?, out(module_s)).
    % Find @var{Name} in all the modules in ~nested_star
    % TODO: deprecate, do proper module resolution
    nested_star_find_class(Name) := Module :- Name = 'serializable', !, % TODO: special case, fix
	Module = ~module_s.from_id(~pli, Name).
    nested_star_find_class(Name) := Module :-
	pli.module__name(Id0, Name),
	!,
	Module = ~module_s.from_id(~pli, Id0).

    % ---------------------------------------------------------------------------
    % Module dependencies and bindings

    :- public add_options/1.
    % Options are global module parameters (mainly taken from the environment)
    add_options(Opts) :-
	( Opts = [] -> true ; pli.add(options(Opts)) ).

    % TODO: distinguish between 'mod_package' and 'mod_include', avoid
    %       the same 'spec' being included or used in more than one
    %       way?
    :- public get_using/2.
    get_using(Modifier, Spec) :- ( Modifier = mod_include ; Modifier = mod_package ), !,
        pli.include(Spec).
    :- public add1_using/2.
    add1_using(Modifier, Spec) :- ( Modifier = mod_include ; Modifier = mod_package ), !,
        pli.add1(include(Spec)).

    % TODO: necessary? (see get_using with 'mod_include')
    :- public get_include/1.
    get_include(Spec) :-
        pli.include(Spec).
    :- public add1_include/1.
    add1_include(Spec) :-
        pli.add1(include(Spec)).

    :- public get_imports_nocheck/3.
    get_imports_nocheck(Mod, F, A) :-
	pli.imports_nocheck(Mod, F, A).
    :- public add1_imports_nocheck/3.
    add1_imports_nocheck(Mod, F, A) :-
	pli.add1(imports_nocheck(Mod, F, A)).

    :- public add1_import/1.
    add1_import(PackageSpec) :-
        pli.add1(import(PackageSpec)).

    :- public add1_reexport/1.
    add1_reexport(PackageSpec) :-
        pli.add1(reexport(PackageSpec)).

    % TODO: poor indexing, move F/A first
    :- public add_reexports/3. % TODO: Missing ~id
    add_reexports(Spec, _, _) :-
    	pli.'$reexports_all'(Spec), !.
    add_reexports(Spec, F, A) :-
	pli.'$reexports'(Spec, F, A), !.
    add_reexports(Spec, F, A) :-
	pli.add('$reexports'(Spec, F, A)).

    :- public add_reexports_all/1. % TODO: Missing ~id
    % TODO: poor indexing
    add_reexports_all(Spec) :-
	pli.'$reexports_all'(Spec), !.
    add_reexports_all(Spec) :-
	pli.del('$reexports'(Spec, _, _)),
	pli.add('$reexports_all'(Spec)).

    :- public add_imports/3. % TODO: Missing ~id
    add_imports(Spec, _, _) :-
	pli.'$imports_all'(Spec), !.
    add_imports(Spec, F, A) :-
	pli.'$imports'(Spec, F, A), !.
    add_imports(Spec, F, A) :-
	pli.add('$imports'(Spec, F, A)).

    :- public add_imports_all/1. % TODO: Missing ~id
    add_imports_all(Spec) :-
	pli.'$imports_all'(Spec), !.
    add_imports_all(Spec) :-
	pli.del('$imports'(Spec, _, _)),
	pli.add('$imports_all'(Spec)).

    :- public get_should_be_usermod/1.
    get_should_be_usermod(UsedSpec) :-
        pli.should_be_usermod(UsedSpec).
    :- public add1_should_be_usermod/1.
    add1_should_be_usermod(UsedSpec) :-
        pli.add1(should_be_usermod(UsedSpec)).

    :- public compilation_module/1.
    compilation_module(CompSpec) :-
	pli.transform(CompSpec), !.
    :- public add_compilation_module/1.
    add_compilation_module(CompSpec) :-
	pli.add(transform(CompSpec)).

    :- public add1_native_dep/2.
    add1_native_dep(c_source, Spec) :- !,
	pli.add1(native_include_c_source(Spec)).
    add1_native_dep(c_header, Spec) :- !,
	pli.add1(native_include_c_header(Spec)).

    % ---------------------------------------------------------------------------
    % Declared module/class attributes

    % TODO: add a field_s special predicate (or a predicate with a 'is_field' property)
    :- public field/2. % field(FieldName, Class)
    field(FieldName, Class) :- pli.module__field(FieldName, ~id, Class).
    :- public add1_field/2.
    add1_field(FieldName, Class) :- pli.add1(module__field(FieldName, ~id, Class)).

    % ---------------------------------------------------------------------------
    % Declared module/class predicates

    :- public meta_predicate enum_defined_preds(out(predicate_s)).
    enum_defined_preds(Pred) :-
	Pli = ~pli,
	( is_top_module ->
	    % TODO: ugly implementation; add module__method even when is_top_module
	    Pli.defined_pred(F, A),
	    \+ Pli.module__method(_, A, _, F), % make sure that it is not a method of a module
	    F2 = F
	; Pli.module__method(_F, A, ~id, F2)
	),
	Pred = ~predicate_s.from_id(~self, F2, A).

    :- public meta_predicate enum_exported_preds(out(predicate_s)).
    enum_exported_preds(Pred) :-
        enum_defined_preds(Pred),
	trust(Pred instance_of predicate_s),
	Pred.get_prop(exported).

    % (like pred_ref but for assertions -- it does not register the pred)
    % TODO: think again about it
    :- public meta_predicate pred_ref_noreg(?, ?, out(predicate_s)).
    pred_ref_noreg(F0, A) := Pred :-
	( is_top_module ->
	    F = F0
	; F = ~sub_id(~id, F0)
	),
	Pred = ~predicate_s.from_id(~self, F, A).

    :- public meta_predicate pred_ref0(?, ?, out(predicate_s)).
    pred_ref0(F0, A) := Pred :-
	( is_top_module ->
	    F = F0,
	    Pred = ~predicate_s.from_id(~self, F, A)
	; method(F0, A, F) ->
	    % already defined
	    Pred = ~predicate_s.from_id(~self, F, A)
	; ( ~get_modif = mod_interface -> /*QC*/
	      % display(user_error, donotincludeinterfaces), nl(user_error), % TODO: interfaces must be exported; not included [see module_instance_functor]
	      F = ~sub_id(~get_name, F0)
	  ; F = ~sub_id(~id, F0)
	  ),
	  add_method(F0, A, F),
	  Pred = ~predicate_s.from_id(~self, F, A)
	).

    :- public meta_predicate pred_ref(?, ?, out(predicate_s)).
    % Obtain a reference to the predicate F0/A associated to
    % @var{Module0}.  The predicate will be registered in the nearest
    % @em{definition module} (non open-anon) of @var{Module0}.
    %
    % Note: The idea here is that predicates can be grouped within an
    %       anonymous open module (with some associated information like a
    %       fluid environment), and at the same time being registed in a
    %       definition module (i.e., not open-anon).
    %
    pred_ref(F0, A) := Pred :-
	Module = ~find_def_enclosing,
	Pred = ~Module.pred_ref0(F0, A),
	( Pred.get_prop(defined) ->
	    true
	; Pred.set_prop(defined),
	  set_pred_owner_module_ctx(Pred)
	).
    % Associate @var{Pred} to the context of its owner_module
    set_pred_owner_module_ctx(Pred) :-
	trust(Pred instance_of predicate_s),
	( get_prop(static_scope) -> % inside $all_static open-anon
	    true % no context
	; is_static ->
	    true % no context because Module is static
	; Pred.set_prop(owner_module_ctx(~get_id))
	).

    :- public add_clause/2.
    % Add source clause (Head :- Body) to the module
    % TODO: Avoid using the expanded name for BC backend
    add_clause(Head, Body) :-
	functor(Head, N, A),
	Head =.. [_|Args],
	Pred = ~pred_ref(N, A),
	N2 = ~Pred.f, % (expanded name)
	Head2 =.. [N2|Args],
	Loc = ~errlog.empty_loc,
	Pred.add_clause(Head2, Body, Loc, no).

    :- public data/3. % data(ModuleF, A, F)
    data(ModuleF, A, F) :- pli.module__data(ModuleF, A, ~id, F).
    :- public add1_data/3.
    add1_data(ModuleF, A, F) :- pli.add1(module__data(ModuleF, A, ~id, F)).
    :- public virtual/5. % virtual(F, A, FieldName, PredName, Context)
    virtual(F, A, FieldName, PredName, Context) :- pli.module__virtual(F, A, ~id, FieldName, PredName, Context).
    :- public add_virtual/5.
    add_virtual(F, A, FieldName, PredName, Context) :- pli.add(module__virtual(F, A, ~id, FieldName, PredName, Context)).
    :- public method/3. % method(F, A, ModuleF)
    method(F, A, ModuleF) :- pli.module__method(F, A, ~id, ModuleF).
    :- public add_method/3.
    add_method(F, A, ModuleF) :- pli.add(module__method(F, A, ~id, ModuleF)).
    :- public method_code/5. % method_code(ModuleF, A, Args, ArgsTemp, Code)
    method_code(ModuleF, A, Args, ArgsTemp, Code) :- pli.module__method_code(ModuleF, A, ~id, Args, ArgsTemp, Code).
    :- public add_method_code/5.
    add_method_code(ModuleF, A, Args, ArgsTemp, Code) :- pli.add(module__method_code(ModuleF, A, ~id, Args, ArgsTemp, Code)).
    :- public set_method_code/5.
    set_method_code(ModuleF, A, Args, ArgsTemp, Code) :-
	pli.del(module__method_code(ModuleF, A, ~id, _, _, _)),
	pli.add(module__method_code(ModuleF, A, ~id, Args, ArgsTemp, Code)).
    :- public constr/3. % constr(F, A, FunName)
    constr(F, A, FunName) :- pli.module__constr(F, A, ~id, FunName).
    :- public add1_constr/3.
    add1_constr(F, A, FunName) :- pli.add1(module__constr(F, A, ~id, FunName)).

    % unexpanded F 
    % TODO: slow, find a better method
    :- public method_f/3.
    method_f(ModuleF, _A, F) :- is_top_module, !,
	ModuleF = F.
    method_f(ModuleF, A, F) :-
        pli.module__method(F, A, ~id, ModuleF), !.

    :- public add1_binder_def/2.
    add1_binder_def(Head, Def) :-
	pli.add1(binder_def(Head, Def)).

    :- public add1_exported_binder/2.
    add1_exported_binder(F, A) :-
        pli.add1(exported_binder(F, A)).

    % Mark if a functor is definable (e.g., forbid redefinition of ','/2)
    :- public set_definable/3.
    set_definable(F, A, yes) :- !,
	pli.del(forbid_def(F, A)).
    set_definable(F, A, no) :- !,
	pli.add1(forbid_def(F, A)).
    :- public not_definable/2.
    not_definable(F, A) :- !,
	pli.forbid_def(F, A).

    :- public get_def_redefining/2.
    get_def_redefining(F, A) :-
	pli.def_redefining(F, A).
    :- public add1_def_redefining/2.
    add1_def_redefining(F, A) :-
	pli.add1(def_redefining(F, A)).

    % ---------------------------------------------------------------------------
    % Export the symbol (modules, classes, or class attributes)

    :- public exported_sym/1. % exported_sym(Sym)
    exported_sym(Sym) :- pli.module__exported_sym(Sym, ~id).
    :- public add1_exported_sym/1.
    add1_exported_sym(Sym) :- pli.add1(module__exported_sym(Sym, ~id)).

    % ---------------------------------------------------------------------------
    % Native, basal, or ImProlog code

    :- public add_native_inline/1.
    add_native_inline(X) :-
	pli.add(native_inline(X)).

    % TODO: Not a good idea, merge representation
    :- public add_improlog_sentence/1.
    add_improlog_sentence(Pragma) :-
	pli.add(pragma(ip(Pragma))).

    % ---------------------------------------------------------------------------
    % Lookup an specific item (predicates or nested modules) in this module 

    % TODO: [Merge] missing

    % ---------------------------------------------------------------------------
    % Assertions and new declarations

    :- public new_decl/2.
    new_decl(D, DeclVisibility) :-
        pli.new_decl(D, DeclVisibility).
    :- public add1_new_decl/2.
    add1_new_decl(D, DeclVisibility) :-
        pli.add1(new_decl(D, DeclVisibility)).

    :- public pragma/1. % TODO: missing ~id, use ~enclosing_star
    pragma(Pragma) :-
	pli.pragma(Pragma).
    :- public add1_pragma/1.
    add1_pragma(Pragma) :-
	% TODO: poor indexing? - but there is no other solution... right?
	pli.add1(pragma(Pragma)).

    :- public add_decl/2.
    add_decl(Decl, Loc) :- pli.add(decl(Decl, Loc)).

    % Note: assertions may describe imported predicates, so we cannot bind
    %       them to some pred_ref, until we do module resolution.
    :- public get_assertion/1.
    get_assertion(X) :- (~find_def_enclosing).get_assertion_(X).
    :- public add_assertion/1.
    add_assertion(X) :- (~find_def_enclosing).add_assertion_(X).
    :- public del_assertion/1.
    del_assertion(X) :- (~find_def_enclosing).del_assertion_(X).
    :- public get_modedef/2.
    get_modedef(H,X) :- (~find_def_enclosing).get_modedef_(H,X).
    :- public add_modedef/2.
    add_modedef(H,X) :- (~find_def_enclosing).add_modedef_(H,X).
    :- public del_modedef/2.
    del_modedef(H,X) :- (~find_def_enclosing).del_modedef_(H,X).

    % (private)
    get_assertion_(X) :- pli.module__assertion(~id, X).
    add_assertion_(X) :- pli.add(module__assertion(~id, X)).
    del_assertion_(X) :- pli.del(module__assertion(~id, X)).
    get_modedef_(H,X) :- pli.module__modedef(~id, H, X).
    add_modedef_(H,X) :- pli.add(module__modedef(~id, H, X)).
    del_modedef_(H,X) :- pli.del(module__modedef(~id, H, X)).

    :- public do_forced_props/4.
    % Set 'forced' props (M:F/A may be any predicate)
    % TODO: Rename by trust_props?
    % TODO: M must be resolved
    do_forced_props(M, F, A, Props) :-
	( pli.forced_props(F, A, M, OldProps) ->
	    pli.del(forced_props(F, A, M, _)),
	    prop_merge_noexp(Props, OldProps, NewProps)
	; NewProps = Props
	),
	pli.add(forced_props(F, A, M, NewProps)).

    :- public do_trust_entry/4.
    do_trust_entry(F, A, AbsInt, Lambda) :-
	pli.add(trust_entry(F, A, AbsInt, Lambda)).

    :- public add_ptoc_type/2.
    add_ptoc_type(Type, Def) :-
	pli.add(ptoc_type(Type, Def)).

    :- public add_ptoc_typeprop/3.
    add_ptoc_typeprop(Type, Op, Name) :-
	% TODO: check errors (e.g. defining an ilegal type property)
	pli.add(ptoc_typeprop(Type, Op, Name)).

    :- public add_ptoc_imptype_c/2.
    add_ptoc_imptype_c(Name, Def) :-
	pli.add(ptoc_imptype_c(Name, Def)).

    % ---------------------------------------------------------------------------
    % Special module properties

    :- public get_prop/1.
    % Valid properties of the module:
    %   'exports_all': all predicates are exported
    %   ...
    get_prop(def_instance_of) :- !, % TODO: simplify, merge with JS version
        ( get_prop(raw_state) ->
	    fail
	; is_top_module ->
	    fail % TODO: Probably this is not correct.
	; ~get_modif = mod_class % TODO: mod_interface need instance_of, but it must be generated in a different way
	).
        % pli.module__prop(~id, def_instance_of)
%	R = ( get_prop(raw_state) ? 1 | 0 ),
%	display(user_error, di(A, ~get_modif, R, ~id)), nl(user_error),
%	A = yes.
    get_prop(Prop) :- pli.module__prop(~id, Prop), !.

    :- public set_prop/1.
    set_prop(Prop) :- pli.add1(module__prop(~id, Prop)).

    :- public add1_fun_eval/2.
    add1_fun_eval(F, A) :-
        pli.add1(fun_eval(F, A)).

    :- public set_eval_arith/1.
    set_eval_arith(true) :-
        pli.add1(eval_arith).
    set_eval_arith(false) :-
        pli.del(eval_arith).

    % ---------------------------------------------------------------------------
    % Translation hooks

    :- public add1_expansion_check/1.
    add1_expansion_check(Pred) :-
	pli.add1(expansion_check(Pred)).

    :- public get_translation_hook/2.
    get_translation_hook(Kind, Tr) :-
	pli.translation_hook(Kind, Tr).
    :- public set_translation_hook/2.
    set_translation_hook(Kind, Tr) :-
	pli.del(translation_hook(Kind, _)),
	pli.add(translation_hook(Kind, Tr)).
    :- public add_translation_hook/2.
    add_translation_hook(Kind, Tr) :-
	pli.add(translation_hook(Kind, Tr)).
    :- public del_translation_hook/1.
    del_translation_hook(Kind) :-
	pli.del(translation_hook(Kind, _)).

    % ---------------------------------------------------------------------------
    % State model 

    :- public selfname/1. % selfname(SelfName)
    selfname(SelfName) :- pli.module__selfname(~id, SelfName).
    :- public add_selfname/1.
    add_selfname(SelfName) :- pli.add(module__selfname(~id, SelfName)).

    % StateModel = single | pair
    :- public statemodel/1.
    statemodel(StateModel) :- pli.module__statemodel(~id, StateModel).
    :- public add_statemodel/1.
    add_statemodel(StateModel) :- pli.add(module__statemodel(~id, StateModel)).
    :- public set_statemodel/1.
    set_statemodel(StateModel) :-
        pli.del(module__statemodel(~id, _)),
        pli.add(module__statemodel(~id, StateModel)).

    :- public trust_module_statemodel/2.
    trust_module_statemodel(Class, StateModel) :-
	pli.trust_module_statemodel(/*QC*/Class, StateModel).

    :- public add_trust_module_statemodel/2.
    add_trust_module_statemodel(Name, StateModel) :-
        % TODO: we should add qualification to Name, but 'trust_module_statemodel' is used just syntactically now
	pli.add(trust_module_statemodel(/*QC*//*add qualification here*/Name, StateModel)).

    :- public instance_arity/1. % instance_arity(Count) # "Arity of the term that holds an instance of this class"
    instance_arity(Count) :- pli.module__instance_arity(~id, Count).
    :- public add_instance_arity/1.
    add_instance_arity(Count) :- pli.add(module__instance_arity(~id, Count)).

    % ---------------------------------------------------------------------------
    % Environment for implicits

    :- public getctx/1.
    getctx := Def :- pli.module__ctx(~id, Def).
    :- public addctx/1.
    addctx(Def) :- pli.add(module__ctx(~id, Def)).

    :- public incctx/1.
    % Increment the current ctx (for 'fluid')
    % TODO: it accepts ModuleId too, which is not very nice
    % Note: Def may be another scope, a mixin, or a fluid
    incctx(Def) :-
        Id = ~id,
        ( pli.module__ctx(Id, Def0) ->
	    pli.del(module__ctx(Id, _)),
	    Def1 = ( Def0 = none ? Def | (Def0, Def) )
	; Def1 = Def
	),
	pli.add(module__ctx(Id, Def1)).
}.
:- endif. % pli

% TODO: generate automatically
:- public data module__name/2.
:- data module__nested_module/3.
:- data module__modif/2.
:- data module__ctx/2.
% TODO: generate automatically
:- if(module_projection(pli)).
:- data module__enclosing/2.
:- data module__prop/2.
:- endif.
:- data module__statemodel/2.
:- if(module_projection(pli)).
:- data module__extends/2. % (unresolved)
:- endif.
:- data module__extendsR/2. % (resolved)
:- data module__selfname/2.
:- if(module_projection(pli)).
:- data module__exported_sym/2.
:- data module__instance_arity/2.
:- data module__constr/4.
:- endif.
:- data module__field/3.
:- data module__method/4.
:- data module__method_code/6.
:- data module__virtual/6.
:- data module__data/4.
% (assertions)
:- public data module__assertion/2. % TODO: do not export (only for the 'compile' part of compiler/frontend.pl)
% Mode assertions (see assertions__nprops)
:- data module__modedef/3.

% Obtain the top module for this file
:- if(module_projection(itf)).
:- public meta_predicate get_top_module(out(module_s_itf)).
get_top_module := ~module_s_itf.from_id(~self, ~defines_module).
:- elif(module_projection(pli)).
:- public meta_predicate get_top_module(out(module_s)).
get_top_module := ~module_s.from_id(~self, ~defines_module).
:- endif.

% Binders
:- public data binder_def/2. % binder_def(Head, Def)

:- doc(subsection, "Definition of predicates").

:- if(module_projection(pli)).
:- use_module(library(lists), [select/3]).
:- use_module(compiler(ptoc__props), [prop_merge_noexp/3, expand_impnat_prop/2]).

:- public class predicate_s {
    % Functors and predicates
    :- public attr owner_module :: module_s.
    :- attr pli :: module_pli. % TODO: extract from owner_module
    :- public attr f :: any.
    :- public attr a :: any.

    :- constructor from_id_/3.
    from_id_(Module, F, A) :-
        ~owner_module = Module,
	Pli = ~owner_module.pli,
	~pli = Pli,
	~f = F, ~a = A.

    % TODO: similar to module_jsexp_:predicate_s.new_pred_id/3
    :- public get_id/1.
    get_id := Atom :-
        MN = ~f,
        A = ~a,
	number_codes(A, ACodes),
	atom_codes(AAtom, ACodes),
	atom_concat(MN, '/', MN1),
	atom_concat(MN1, AAtom, Atom).

    % name as a predicate specifier (Functor/Arity)
    % (e.g., for warning/error messages)
    :- public name_spec/1.
    name_spec := (~f)/(~a).

    % Obtain a sibling predicate (defined in the same module) with the
    % same name but different arity (which can be itself). The input
    % arity may be uninstantiated.
    :- public meta_predicate arity_family(?, out(predicate_s)).
    arity_family(A) := Pred :-
        F = ~f,
	pli.has_clauses(F, A),
        Pred = ~predicate_s.from_id(~owner_module, F, A).

    % The class containing the predicate, or the open-anon module
    % containing the fluid definitions required by this predicate.
    %
    % Note: it may return 'none' (if none) or 'module' (special case)
    %
    % TODO: Return owner_module instead of 'none'? Think again about it.
    :- public meta_predicate envmod_ctx(out(module_s)).
    envmod_ctx := Module :-
        ( get_prop(owner_module_ctx(Id)) ->
	    ( Id = none -> Module = Id % TODO: not really correct
	    ; Id = module -> Module = Id % TODO: not really correct
	    ; Module = ~module_s.from_id(~pli, Id)
	    )
        ; Module = none
        ).

    % ---------------------------------------------------------------------------
    % Properties of this predicate

    % Valid properties of the predicate:
    %   'def(_)': internal definition (data, dynamic, bytecode, etc.)
    %   'defined': queried by some pred_ref
    %   'exported': the predicate is exported
    %   'is_prop': is a 'prop' (assertion language)
    %   'selfprj(SelfMode)': projection of 'self', where
    %       SelfMode = void|use|def|usedef
    %   'has_clauses': the predicate has clauses
    %   'meta(Meta)': metapredicate declaration
    %   'multifile': multifile predicate
    % TODO: this should be part of the record
    :- public get_prop/1.
    get_prop(defined) :- !, pli.defined_pred(~f, ~a).
    get_prop(exported) :- !, pli.exported_pred(~f, ~a).
    get_prop(has_clauses) :- !, pli.has_clauses(~f, ~a).
    % (general case)
    get_prop(Prop) :- pli.pred__prop(~f, ~a, Prop), !.

    :- public set_prop/1.
    set_prop(defined) :- !, pli.add1(defined_pred(~f, ~a)).
    set_prop(exported) :- !, pli.add1(exported_pred(~f, ~a)).
    set_prop(has_clauses) :- !, pli.add1(has_clauses(~f, ~a)).
    % (general case)
    set_prop(Prop) :- functor(Prop, N, 1), !,
        % TODO: optimize
        functor(Prop0, N, 1),
	pli.del(pred__prop(~f, ~a, Prop0)),
	pli.add1(pred__prop(~f, ~a, Prop)).
    set_prop(Prop) :- pli.add1(pred__prop(~f, ~a, Prop)).

    :- public set_def/1.
    % TODO: document
    % TODO: handle exceptions propertly (from the compiler)
    % Where Def is one of:
    %   builtin(CName, MemUsege) -- also preserves all the registers
    %   func(CName, UsesHeap)      -- also preserves all the registers
    %   funcre(CName, UsesHeap)
    %   bytecode, dynamic, data, concurrent, unknown, ...
    %   ...
    set_def(Def) :-
	% TODO: we should support them
	( Def = data ; Def = dynamic ; Def = concurrent ),
	Module = ~owner_module,
	trust(Module instance_of module_s),
	\+ Module.is_top_module,
	!,
	throw(unsupported_def_in_class(Def, Module, ~name_spec)).
    set_def(Def) :-
        % TODO: merge the way defs of predicates are set
	Def = data,
	Module = ~owner_module,
	trust(Module instance_of module_s),
	Module.is_top_module, Module.is_class_or_interface,
	% Important: not defined inside $all_static open-anon
	get_prop(owner_module_ctx(Ctx)), \+ Ctx = none, % TODO: 'none' is used?
	!,
	% TODO: ugly code, set prop and collect them
	F = ~f, A = ~a,
	Module.method_f(F, A, F0), % TODO: not used now, but keep it
	Module.add1_data(F, A, F0).
    set_def(Def) :-
	( get_prop(def(Def2)) ->
	    ( Def2 == Def -> true
	    ; throw(incompatible_decl(~name_spec,Def,Def2))
	    )
	; set_prop(def(Def))
	).

    :- public set_pred_props/1.
    set_pred_props(Props00) :-
	( select(unfold(ArgsTemp), Props00, Props0) ->
	    set_prop(unfold(ArgsTemp))
	; Props0 = Props00
	),
	( select((impnat=Nat), Props0, Props1) ->
	    call((
              props :: accum <- Props,
	      expand_impnat_prop(Nat, Def0),
	      ~props = Props1
            )),
	    MaybeDef = just(Def0)
	; MaybeDef = no,
	  Props = Props0
	),
	% TODO: use maybe...?
	( MaybeDef = just(Def) -> set_def(Def) ; true ),
	do_priv_props(~f, ~a, Props).

    do_priv_props(F, A, Props) :-
	Pli = ~pli,
	% TODO: check properties, syntactically!! merging and intepreting them can be delayed to the 'compile' part of compiler/frontend.pl
	( Pli.priv_props(F, A, OldProps) ->
	    Pli.del(priv_props(F, A, _)),
	    prop_merge_noexp(Props, OldProps, NewProps)
	; NewProps = Props
	),
	Pli.add(priv_props(F, A, NewProps)).

    :- public get_pub_props/1.
    get_pub_props(PubProps) :-
	F = ~f, A = ~a,
	pli.def_pred(F, A, PubProps).
    :- public set_pub_props/1.
    set_pub_props(PubProps) :-
	F = ~f, A = ~a,
	pli.add(def_pred(F, A, PubProps)).

    :- public is_defined/0.
    is_defined :- get_prop(def(_)), !.
    is_defined :- get_prop(has_clauses), !.
    is_defined :-
	Module = ~owner_module,
	trust(Module instance_of module_s),
	F = ~f, A = ~a, Module.data(F, A, _), !.

    % ---------------------------------------------------------------------------
    % Code associated to this predicate

    :- public add_clause/4.
    add_clause(H0, B, Loc, UseFunExp) :-
	F = ~f,
	% TODO: do 'replace_head_name/4' later
	replace_head_name(UseFunExp, H0, F, H),
        set_prop(has_clauses),
        pli.add(clauses(c(H, B, Loc))).

     :- public get_clause/3.
     % Get a clause of this predicate
     % TODO: head is partially expanded, avoid this?
     get_clause(Head, Body, Loc) :-
	F = ~f, A = ~a,
	functor(Head, F, A),
	pli.clauses(c(Head, Body, Loc)).

     :- public erase_clauses/0.
     % Erase all the clauses of this predicate
     erase_clauses :-
	F = ~f, A = ~a,
	functor(Head, F, A),
	pli.del(clauses(c(Head, _, _))).

%    add_clause(H, B, Loc) :-
%        set_prop(has_clauses),
%        pli.add(clauses(c(H, B, Loc))).

    % ---------------------------------------------------------------------------
    % Flattening

    % TODO: Missing
}.
:- public data pred__prop/3.
:- pred pred__prop(F, A, Prop).
% TODO: Register predicates in the enclosing module
:- public data defined_pred/2.
:- pred defined_pred(F, A).
% TODO: we have clauses for the predicate
:- public data has_clauses/2.
:- pred has_clauses(F, A).
:- public data exported_pred/2.
:- pred exported_pred(F, A). 
:- endif. % pli

:- endif. % itf ; pli

% ===========================================================================
:- elif(module_projection(ideps)).
:- doc(section, "Dependencies of the module (after 'compile' part)").
% ===========================================================================

:- use_module(library(aggregates), [findall/3]).

:- pred imported(Module, Spec).
:- public data imported/2.
:- public imported__keys/1.
imported__keys := ~findall(Mod, imported(Mod, _)).

% TODO: change name?
:- public get1_imported_fast/2.
:- use_module(compiler(store)). % for spec_to_default_module
% like imported(Module, Spec) but faster when Spec is ground
get1_imported_fast(Spec, Module) :-
	( nonvar(Spec),
	  % try with the default module name for that spec
	  spec_to_default_module(Spec, Module),
	  imported(Module, Spec) ->
	    true
	; % not found with the default module name for that spec,
	  % search again
	  imported(Module, Spec), !
	).

% ===========================================================================
:- elif(module_projection(sym)).
:- doc(section, "Symbolic information useful to link 'emu' files").
% ===========================================================================

:- pred exports(F, A, MF) # "Export predicate @var{F}/@var{A} with qualified name @var{MF}".
:- public data exports/3.

% ===========================================================================
:- elif(module_projection(archbin)).
:- doc(section, "Architecture-dependant properties of the compiled module").
% ===========================================================================

:- public data contains_bytecode/0.
:- pred contains_bytecode # "The binary has bytecode".
:- public data contains_native/0.
:- pred contains_native # "The binary has native".

% TODO: insert spec everywhere (other modules)...

% ===========================================================================
:- elif(module_projection(bin)).
:- doc(section, "Architecture-independant properties of the compiled module").
% ===========================================================================

:- public data contains_bytecode/0.
:- pred contains_bytecode # "The binary contains bytecode".
:- public data contains_native/0.
:- pred contains_native # "The binary has native code".
:- public data contains_native_h/0.
:- pred contains_native_h # "The binary has native code header".
:- public data native_include_c_source/1.
:- pred native_include_c_source(Spec) # "The binary has native source code".
:- public data native_include_c_header/1.
:- pred native_include_c_header(Spec) # "The binary has native header".
:- public data native_library_use/1.
:- pred native_library_use(Lib) # "A native (foreign) library needed by the binary".
:- public data native_library_path/1.
:- pred native_library_path(LibPath) # "A path for foreign libraries".

% ===========================================================================
:- endif.
% ===========================================================================

% Allow disk serialization for this object.
% This is necessary to perform separate, incremental compilation.
:- extends serializable.

% (common)
:- constructor new_/0.
new_.

% ---------------------------------------------------------------------------
:- doc(subsection, "Register in the 'memoize' driver").

:- use_module(compiler(memoize)). % TODO: also because 'store' needs memoize
:- '$trust_statemodel'(memoize, single).

{
:- fluid memo :: memoize.
% Load/compute from a spec
:- public static from_spec/2.
:- if(module_projection(deps)).
:- meta_predicate from_spec(?, out(module_deps)).
from_spec(Spec) := ~memo.eval(split(Spec)).
:- elif(module_projection(ideps)).
:- meta_predicate from_spec(?, out(module_ideps)).
from_spec(Spec) := ~memo.eval(expand__ideps(Spec)).
:- elif(module_projection(pli)).
:- meta_predicate from_spec(?, out(module_pli)).
from_spec(Spec) := ~memo.eval(split__src(Spec)).
:- elif(module_projection(itf)).
:- meta_predicate from_spec(?, out(module_itf)).
from_spec(Spec) := ~memo.eval(split__itf(Spec)).
:- elif(module_projection(sym)).
:- meta_predicate from_spec(?, out(module_sym)).
from_spec(Spec) := ~memo.eval(expand__sym(Spec)).
:- elif(module_projection(archbin)).
:- meta_predicate from_spec(?, out(module_archbin)).
from_spec(Spec) := ~memo.eval(archcompile(Spec)).
:- elif(module_projection(bin)).
:- meta_predicate from_spec(?, out(module_bin)).
from_spec(Spec) := ~memo.eval(compile(Spec)).
:- endif.
}.

% TODO: this is a mixing!
:- include(.(memoize__callback)).

:- if(module_projection(deps)).
element__save(split, Deps, Name) :- trust(Deps instance_of module_deps), Deps.write_to_file(Name).
element__restore(split, Name, Deps) :- module_deps:read_from_file(Name, Deps).
:- elif(module_projection(ideps)).
element__save(expand__ideps, IDeps, Name) :- trust(IDeps instance_of module_ideps), IDeps.write_to_file(Name).
element__restore(expand__ideps, Name, IDeps) :- module_ideps:read_from_file(Name, IDeps).
:- elif(module_projection(pli)).
element__save(split__src, Pli, Name) :- trust(Pli instance_of module_pli), Pli.write_to_file(Name).
element__restore(split__src, Name, Pli) :- module_pli:read_from_file(Name, Pli).
:- elif(module_projection(itf)).
element__save(split__itf, Itf, Name) :- trust(Itf instance_of module_itf), Itf.write_to_file(Name).
element__restore(split__itf, Name, Itf) :- module_itf:read_from_file(Name, Itf).
:- elif(module_projection(sym)).
element__save(expand__sym, Sym, Name) :- trust(Sym instance_of module_sym), Sym.write_to_file(Name).
element__restore(expand__sym, Name, Sym) :- module_sym:read_from_file(Name, Sym).
:- elif(module_projection(archbin)).
element__save(archcompile, Bin, Name) :- trust(Bin instance_of module_archbin), Bin.write_to_file(Name).
element__restore(archcompile, Name, Bin) :- module_archbin:read_from_file(Name, Bin).
:- elif(module_projection(bin)).
element__save(compile, Bin, Name) :- trust(Bin instance_of module_bin), Bin.write_to_file(Name).
element__restore(compile, Name, Bin) :- module_bin:read_from_file(Name, Bin).
:- endif.

