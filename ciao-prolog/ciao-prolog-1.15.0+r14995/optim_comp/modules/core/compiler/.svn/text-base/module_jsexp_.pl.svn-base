:- module(module_jsexp_, [], [complang]).

:- doc(title, "Representation of modules, predicates, and callables (JS-backend)").
:- doc(author, "Jose F. Morales").

:- use_module(library(lists)).
:- use_module(library(aggregates)).
:- use_module(compiler(errlog)).
:- use_module(compiler(list_common)).

:- include(compiler(ptojs__errlog)). % (defines error/1)

:- use_module(compiler(cdic_)). :- '$trust_statemodel'(cdic, pair).
:- use_module(compiler(meta_syntax)).

% ---------------------------------------------------------------------------

:- include(compiler(psymbol__interface)).

:- public class module_s {
    % :- doc(title, "Structure for modules").

    % TODO: Ideally, a single 'module' interface would be enough even
    %   for runtime; with different incarnations (e.g., one for
    %   separate compilation, other for runtime, etc.).

    :- '$statemodel'(single).
    :- '$raw_state'.
    % TODO: missing instance_of__/1

    % TODO: [Merge] new_/0?
    :- constructor new_/2.
    new_(Name, EnclosingModuleR) :-
	trust(EnclosingModuleR instance_of module_s),
	ModuleR = ~EnclosingModuleR.sub_id(Name),
        ~self = ModuleR,
	assertz_fact(module__def(ModuleR)), % TODO: not really necessary
	assertz_fact(module__name(ModuleR, Name)), % TODO: do not fix here
	assertz_fact(module__enclosing(ModuleR, EnclosingModuleR)).

    :- public meta_predicate query_nested_module(?, out(module_s)).
    % Ensure that the @var{Name} symbol is bound to a nested module in
    % this module.
    query_nested_module(Name) := ModuleR :-
	( ModuleR0 = ~get_nested_module(Name) ->
	    ModuleR = ModuleR0
	; ModuleR = ~module_s.new(Name, ~self),
	  add_nested_module(Name, ModuleR)
	).

    :- public meta_predicate query_anon_module(out(module_s)).
    query_anon_module := _ :-
        throw(anon_module_not_implemented_in_js_backend).

    :- public sub_id/2.
    % Obtain a unique identifier from the unique identifier of this module
    % TODO: 'sub_id' just need to be unique, it can be arbitrary (e.g., a number)
    sub_id(N) := MN :- ~self = ~lookup_module(root), !, MN = N.
    sub_id(N) := MN :-
        ModuleR = ~self,
	atom_concat(ModuleR, ':', N1),
	atom_concat(N1, N, MN).

    :- public new_atom/1.
    % TODO: see module_exp
    % TODO: it has to be deprecated in favor of subatom, associate with predicate
    new_atom := Atom :-
        ModuleR = ~self,
	( current_fact(module__counter(ModuleR, Counter0)) ->
	    true
	; Counter0 = 0
	),
	Counter1 is Counter0 + 1,
	retractall_fact(module__counter(ModuleR, _)),
	assertz_fact(module__counter(ModuleR, Counter1)),
	% TODO: do not use module concat, user files need a unique identifier for each file
	number_codes(Counter0, Cs),
	atom_codes(Atom0, Cs),
	atom_concat('#', Atom0, Atom).

    :- public set_name_top/1.
    % TODO: [Merge] dummy
    set_name_top(_).

    % ---------------------------------------------------------------------------
    % The name of the module (at source)

    :- public get_name/1.
    get_name(Name) :-
        ModuleR = ~self,
        ( ModuleR = ~module_s.lookup_module(root) ->
	    Name = '\6\root' % TODO: strange name, shouldn't it be 'root'?
	; current_fact(module__name(ModuleR, Name))
	).

    % TODO: change by... spec of the 'top module'?
    :- public mod_spec/1.
    mod_spec := Spec :-
        % Find a file_spec property in this module or upwards in any of
        % the enclosing modules.
        get_prop_in_scope(file_spec(RootSpec)),
	!,
	Spec = RootSpec.

    % ---------------------------------------------------------------------------    
    % TODO: [Merge] See module_common.pl for documentation

    :- public set_modifier/1.
    set_modifier(Modifier) :-
	( Modifier = mod_static ->
	    set_prop(static)
	; Modifier = mod_class ->
	    true
	; throw(unknown_module_modifier(Modifier))
	).

    :- public is_static_or_class_or_interface/0.
    is_static_or_class_or_interface. % TODO: merge with bc

    :- public is_open_anon_or_mixin/0.
    is_open_anon_or_mixin :- fail. % TODO: merge with bc

    % ---------------------------------------------------------------------------
    % Base(s) of this module/class

    :- public add_extends/1.
    add_extends(simple_box(CmpOp)) :- !, % TODO: sure?
	set_prop(simple_box(CmpOp)).
    add_extends(Base) :-
        % TODO: missing resolve; resolve later like in BC backend
        Id = ~self,
	( current_fact(module__extends(Id, Base)) ->
	    true
	; assertz_fact(module__extends(Id, Base))
	).

    :- public meta_predicate get_extends(out(module_s)).
    % TODO: @var{Base} this is not a 'module_s' yet! (not resolved)
    get_extends := Base :-
        ModuleR = ~self,
	( current_fact(module__extends(ModuleR, Base)) -> true
	; Base = ~default_base
	).

    % Base class name when no super class is specified
    :- static default_base/1.
    default_base := nonvar.

    :- public meta_predicate base(out(module_s)).
    % Base class (for classes)
    % TODO: basal_base, nonvar_base, var_base, static_base are not real classes, fix
    % TODO: store in resolved form!
    base := B :-
        ~self = ~module_s.lookup_module(root), !, B = static_base.
    base := B :-
        % TODO: this should be more like resolve_term
        Base = ~get_extends,
	Enclosing = ~enclosing_module,
	( Base = basal_base ->
	    B = Base % special name
	; Base = basalnv_base -> % TODO: deprecate? strange case
	    B = Base % special name
	; Base = nonvar_base ->
	    B = Base % special name
	; Base = var_base ->
	    B = Base % special name
	; Base = nonvar -> % TODO: not right...
	    B = ~module_s.lookup_module(':'('basiccontrol', 'nonvar')) % special name
        ; Up = ~Enclosing.enclosing_star,
	  B = ~Up.lookup_nested_module(Base, any) ->
	    true
	; error(base_class_not_found_in_scope(Base, Enclosing)),
	  fail
	).

    % ---------------------------------------------------------------------------
    % Enclosing module navigation

    :- public meta_predicate enclosing_module(out(module_s)).
    % ~self is a nested module of EnclosingModuleR
    % (EnclosingModuleR encloses ~self).
    enclosing_module := EnclosingModuleR :-
        ModuleR = ~self,
        current_fact(module__enclosing(ModuleR, EnclosingModuleR)).

    :- public meta_predicate top_enclosing_module(out(module_s)).
    % Top enclosing module (the highest module that is a top module)
    % TODO: avoid working with the 'root' module?
    top_enclosing_module := ~self :- is_top_or_root_module, !.
    top_enclosing_module := ~((~enclosing_module).top_enclosing_module).

    :- public meta_predicate find_def_enclosing(out(module_s)).
    find_def_enclosing := ~self.

    :- public meta_predicate enclosing_star(out(module_s)).
    % Enumeration of modules, following the enclosing relation.
    %
    % Formally: the reflexive transitive closure of the
    % 'enclosing_module' relation, ordered by length of compositions.
    enclosing_star :=
	( ~self | ~((~enclosing_module).enclosing_star ) ).

    :- public is_top_module/0.
    % The module is a 'top module' (not a nested module)
    is_top_module :-    
	~enclosing_module = ~module_s.lookup_module(root).

    :- public is_top_or_root_module/0.
    is_top_or_root_module :- ~self = ~module_s.lookup_module(root), !.
    is_top_or_root_module :- is_top_module.

    % ---------------------------------------------------------------------------
    % Nested module navigation

    % (nondet if N is unbound)
    :- public meta_predicate get_nested_module(?, out(module_s)).
    get_nested_module(N) := NestedModuleR :-
        ModuleR = ~self,
	current_fact(module__nested_module(N, ModuleR, NestedModuleR)).

    add_nested_module(N, NestedModuleR) :-
        ModuleR = ~self,
	assertz_fact(module__nested_module(N, ModuleR, NestedModuleR)).

    :- public nested_module_list/1.
    % List of modules nested in this module (only at depth 1)
    nested_module_list := ~findall(Nested, get_nested_module(_, Nested)).

    % ---------------------------------------------------------------------------
    % Module dependencies and bindings

    :- public add_options/1.
    % Options are global module parameters (mainly taken from the environment)
    % TODO: [Merge] from BC
    add_options(_).

    :- public add_using/2.
    add_using(Modifier, Spec) :-
        ModuleR = ~self,
        assertz_fact(module__using(ModuleR, Spec, Modifier)).

    :- public add1_using/2.
    add1_using(Modifier, Spec) :- get_using(Modifier, Spec), !.
    add1_using(Modifier, Spec) :- add_using(Modifier, Spec).

    :- public get_using/2.
    get_using(Modifier, Spec) :-
        ModuleR = ~self,
        current_fact(module__using(ModuleR, Spec, Modifier)).

    :- public add_import/1.
    add_import(ImportedModule) :-
        ModuleR = ~self,
        assertz_fact(module__import(ImportedModule, ModuleR)).

    :- public get_import/1.
    get_import(ImportedModule) :-
        ModuleR = ~self,
        current_fact(module__import(ImportedModule, ModuleR)).

    % List of imported modules
    :- public imported_list/1.
    imported_list := ~findall(C, get_import(C)).

    :- public add1_should_be_usermod/1.
    % TODO: unfinished method
    add1_should_be_usermod(_).

    :- public compilation_module/1.
    compilation_module(CompSpec) :-
        ModuleR = ~self,
	current_fact(module__transform(ModuleR, CompSpec)), !.
    :- public add_compilation_module/1.
    add_compilation_module(CompSpec) :-
        ModuleR = ~self,
	assertz_fact(module__transform(ModuleR, CompSpec)).

    % ---------------------------------------------------------------------------
    % Declared module/class attributes

    % TODO: allow static attr (for modules) or nonstatic (for class instances)

    :- public add1_field/2. % TODO: use this one, add initial value as an extra property
    % Define an attribute with name @var{Name}.
    add1_field(Name, _Class) :- % TODO: Class is ignored at this moment
        do_mod_attr(Name, no).

    % TODO: move flattening to flatmod_expansion__js
    :- public do_mod_attr/2.
    % The initial value of the attribute when the object is
    % constructed is given by @var{AttrInitVal}.
    do_mod_attr(Name, AttrInitVal) :-
	add_attr(Name),
	% TODO: define 'static' property for the attribute itself
        ( get_prop(static_noself) -> 
	    IsStatic = yes
        ; get_prop(static) ->
	    IsStatic = yes
        ; IsStatic = no
	),
	% TODO: are different names really necessary?
	( IsStatic = yes ->
	    AttrMem = sattmem(Name)
        ; AttrMem = attmem(Name)
	),
        % get method
	AttrP = ~pred_ref(Name, 1),
	( IsStatic = yes -> AttrP.set_prop(static) ; true ), % TODO: not necessary if we look at the module static prop
        set_native_pred(AttrP, detfun, [box], [], [proceed_out1(AttrMem)]),
        % init method
        atom_concat(Name, '__init', NameInit),
	AttrInitP = ~pred_ref(NameInit, 0),
	( IsStatic = yes -> AttrInitP.set_prop(static) ; true ), % TODO: not necessary if we look at the module static prop
%	AttrInitP.set_prop(static),
	% TODO: init should be called in different places depending if the attribute is static or not
	% TODO: query 'term_typing.var' class
	T = ~my_var_new(_,AttrMem),
        set_native_pred(AttrInitP, det, [], [], [
	  inith(T)
        ]),
        add_attr_init(NameInit, AttrInitVal).

    :- public add_attr/1.
    add_attr(Name) :-
	assertz_fact(module__attr(~self, Name)).

    :- public all_attrs/1.
    all_attrs := Xs :-
        ModuleR = ~self,
	findall(X, module__attr(ModuleR, X), Xs).

    :- public add_attr_init/2.
    add_attr_init(Init, MaybeExpr) :-
	assertz_fact(module__attr_init(~self, Init, MaybeExpr)).

    :- public all_mod_attr_inits/1.
    all_mod_attr_inits := Inits :-
        findall(attr_init(Init, AttrInitVal), module__attr_init(~self, Init, AttrInitVal), Inits).

    % ---------------------------------------------------------------------------
    % Declared module/class predicates

    :- public get_pred/3.
    get_pred(N, A) := FunctorR :-
        ModuleR = ~self,
	current_fact(module__pred(N, A, ModuleR, FunctorR)).

    :- public add_pred/3.
    add_pred(N, A, FunctorR) :-
        ModuleR = ~self,
	assertz_fact(module__pred(N, A, ModuleR, FunctorR)).

    :- public meta_predicate enum_exported_preds(out(predicate_s)).
    % TODO: Missing.
    enum_exported_preds(_) :- fail.

    :- public meta_predicate pred_ref(?, ?, out(predicate_s)).
    % Obtain the reference of a predicate (or method) for the current
    % module (create it if it does not exist)
    pred_ref(N, A) := FunctorR :-
	( FunctorR0 = ~get_pred(N, A) ->
	    % TODO: emit error if the function-ness is different?
	    FunctorR = FunctorR0
	; FunctorR = ~predicate_s.new(N, A, ~self),
	  add_pred(N, A, FunctorR)
	).

    % (like pred_ref but for assertions -- it does not register the pred)
    % TODO: not really working for js backend -- is it really necessary?
    :- public meta_predicate pred_ref_noreg(?, ?, out(predicate_s)).
    pred_ref_noreg(F0, A) := ~pred_ref(F0, A).

    :- public meta_predicate local_functor_ref(?, ?, out(predicate_s)).
    % Obtain the reference of a functor for the current module
    % (create it if it does not exist)
    local_functor_ref(N, A) := FunctorR :-
        FunctorR = ~pred_ref(N, A),
	FunctorR.set_prop(local_functor).

    :- public meta_predicate trait_functor_ref(?, out(predicate_s)).
    % Special 'functor trait' entry
    % TODO: this should be a basal trait
    trait_functor_ref(A) := FunctorR :-
	N = 'ta\6\functor', % TODO: better way to separate the name spaces
	FunctorR = ~pred_ref(N, A),
	FunctorR.set_prop(local_functor), % TODO: necessary?
        FunctorR.set_prop(static),
	FunctorR.set_prop(is_functor_trait).

    :- public add_clause/2.
    % Add source clause (Head :- Body) to the module
    add_clause(Head, Body) :-
	functor(Head, N, A),
	Pred = ~pred_ref(N, A),
	Loc = ~errlog.empty_loc,
	Pred.add_clause(Head, Body, Loc, no).

    :- public all_preds/1.
    % All predicates (and functors) defined in this module (not in nested modules)
    % TODO: [Merge] do not use?
    all_preds := FunctorRs :-
        ModuleR = ~self,
	findall(FunctorR, module__pred(_, _, ModuleR, FunctorR), FunctorRs).

    % TODO: unfinished methods
    :- public set_definable/3.
    set_definable(_, _, _) :- !.
    :- public not_definable/2.
    not_definable(_, _) :- fail.

    % TODO: Missing.
    :- public add1_def_redefining/2.
    add1_def_redefining(_, _).

    % ---------------------------------------------------------------------------
    % Lookup definitions inside this module

    :- public meta_predicate lookup_pred(?, ?, ?, out(predicate_s)).
    % Lookup a predicate in this module
    lookup_pred(N, A, Visibility) := ~lookup(sympred(N, A), Visibility).

    :- public meta_predicate lookup_nested_module(?, ?, out(module_s)).
    % Lookup a (nested) module in this module
    % TODO: is symmod necessary? it is really like an atom
    lookup_nested_module(N, Visibility) := ~lookup(symmod(N), Visibility).

    :- public lookup/3.
    % Lookup a predicate or module
    lookup(sympred(N, A1), Visibility) := Def :-
	( FunctorR = ~get_pred(N, A1) ->
	    trust(FunctorR instance_of predicate_s),
	    ( Visibility = exported -> FunctorR.get_prop(exported) ; true ),
	    Def = FunctorR
	; fail
	).
    lookup(symmod('\6\root'), _Visibility) := Def :- !,
        % Special case for "':'('\6\root', X)" qualification (users should not use it)
        Def = ~lookup_module(root).
    lookup(symmod(Name), _Visibility) := Def :-
	% TODO: Visibility is ignored at this moment
        ModuleR = ~self,
	( current_fact(module__nested_module(Name, ModuleR, ModuleR2)) ->
	    Def = ModuleR2
	; fail
	).

    :- public static meta_predicate lookup_module(_, out(module_s)).
    % Some notable modules
    % TODO: just 'root' module should be defined, the rest must be consulted
    %       from the root module or resolved in other way.
    % TODO: everything should be a nested module of 'root'
    lookup_module('root') := '\6\root' :- !. % TODO: necessary?
    lookup_module(':'(A,B)) := R :- !,
        Ca = ~lookup_module(A),
	R = ~Ca.sub_id(B). % TODO: use lookup_nested!
    lookup_module(X) := X.

    % TODO: [Temporal]
    :- public static meta_predicate lookup_type(_, out(module_s)).
    % Like lookup_module, but for 'types' (a temporal solution)
    % TODO: use also for compatibility tests
    % TODO: class = a module with a type + operations; we should be
    %       able to use modules defining classes as types
    lookup_type(Type) := ModuleR :-
	type_to_qmod(Type, QMod), % TODO: this should not be necessary
	ModuleR = ~lookup_module(QMod).

    % ---------------------------------------------------------------------------
    % Succeeds if the current module has a constructor of arity @var{Arity}

    :- public has_constructor/1.
    has_constructor(Arity) :-
	Arities = ~find_arities('cons__'),
	( Arities = [] -> fail % no constructor
	; Arities = [Arity0] -> Arity = Arity0
	; % TODO: implement! 'cons__' :- ... 'cons__'(_) :- ...
	  bug(the_compiler_does_not_yet_support_more_than_one_constructor_per_class),
	  fail
	).

    % ---------------------------------------------------------------------------
    % Find arities of symbols with a given name N

    find_arities(N) := Arities :-
        ModuleR = ~self,
	findall(Arity, find_arities__2(N, ModuleR, Arity), Arities).

    :- static find_arities__2/3.
    find_arities__2(N, ModuleR, Arity) :-
	current_fact(module__pred(N, Arity, ModuleR, _FunctorR)).

    % ---------------------------------------------------------------------------
    % Export the symbol (modules, classes, or class attributes)

    :- public exported_sym/1. % exported_sym(Sym)
    exported_sym(_) :- fail. % TODO: Missing.
    :- public add1_exported_sym/1.
    add1_exported_sym(_). % TODO: Missing.

    % ---------------------------------------------------------------------------
    % Native, basal, or ImProlog code

    :- public add_native/1.
    add_native(String) :-
        ModuleR = ~self,
        assertz_fact(module__native(ModuleR, String)).

    :- public get_native/1.
    get_native(String) :-
        ModuleR = ~self,
        current_fact(module__native(ModuleR, String)).

    :- public set_native_pred/5.
    set_native_pred(FunctorR, Det, ArgModes, CArgs, Code) :-
        trust(FunctorR instance_of predicate_s),
        FunctorR.set_det(Det),
	FunctorR.parse_prop(argsbox(ArgModes)),
	FunctorR.set_insns_def(CArgs, Code).

    % TODO: missing
    :- public add_improlog_sentence/1.
    add_improlog_sentence(_).

    % Query basal predicate/method refs

    % Reference to a ref_BKM or ref_BKS of the Enclosing object
    % (note: this refers to the actual definition, not a the virtual one)
    :- public static meta_predicate query_ref_BK(?, ?, ?, out(psymbol)).
    % TODO: duplicated in module_s, fill from source instead!
    % TODO: see also query_basal
    query_ref_BK(Enclosing, N, A) := Ref :-
        % TODO: simplify, do not use msym for ClassId, but the source ref
        ( ( Enclosing instance_of predicate_s ->
	      Enclosing2 = '\6\pred' % TODO: '\6\pred' should be a superclass
	  ; Enclosing2 = Enclosing
	  )
	; Enclosing2 = '\6\any' % TODO: '\6\any' should be a superclass of Enclosing
	),
	binfo(N, A, Enclosing2, MethodOrStatic, WithWorker, Det),
	!,
	( MethodOrStatic = static ->
	    Ref = ~ref_BKS.new(Enclosing, N, A, WithWorker, Det)
	; Ref = ~ref_BKM.new(Enclosing, N, A, WithWorker, Det)
	).
    
    % Reference to a ref_BUM or ref_BUS of the ClassId object
    % (note: this may refer to a virtual definition)
    :- public static query_ref_BU/4.
    query_ref_BU(ClassId, N, A) := Ref :-
        binfo(N, A, ClassId, MethodOrStatic, WithWorker, Det),
	!,
	( MethodOrStatic = static ->
	    Ref = ~ref_BUS.new(N, A, WithWorker, Det)
        ; Ref = ~ref_BUM.new(N, A, WithWorker, Det)
	).

    % ---------------------------------------------------------------------------
    % Queue of auxiliary predicates (for compilation)
    %
    % TODO: This should not be part of the module definition, but of
    %       the particular compilation algorithm.

    :- public get_auxpred/3.
    get_auxpred(N, A) := FunctorR :-
        ModuleR = ~self,
	current_fact(module__auxpred(N, A, ModuleR, FunctorR)).

    :- public add_auxpred/3.
    add_auxpred(N, A, FunctorR) :-
        ModuleR = ~self,
	assertz_fact(module__auxpred(N, A, ModuleR, FunctorR)).

    % Ensure that the auxiliary predicate N/A is in the queue
    :- public ensure_auxpred/1.
    ensure_auxpred(TraitR) :-
        trust(TraitR instance_of predicate_s),
        TraitR.get_name(N, A),
        ( _ = ~get_auxpred(N, A) ->
	    true
	; add_auxpred(N, A, TraitR)
	).

    % Remove all elements from the auxpreds queue
    :- public drain_auxpreds/1.
    drain_auxpreds := FunctorRs :-
        ModuleR = ~self,
	findall(FunctorR, module__auxpred(_, _, ModuleR, FunctorR), FunctorRs),
	retractall_fact(module__auxpred(_, _, ModuleR, _)).

    % ---------------------------------------------------------------------------
    % Assertions and new declarations

    % TODO: Not very efficient if all declarations are added at each
    %   time.  Why not share them as blocks? (i.e., packages without
    %   expansion)

    % TODO: is this temp data?
    % TODO: use just prop?

    %:- public add_new_decl/2.
    add_new_decl(D, DeclVisibility) :-
        ModuleR = ~self,
        assertz_fact(module__new_decl(ModuleR, D, DeclVisibility)).

    :- public add1_new_decl/2.
    add1_new_decl(D, DeclVisibility) :- new_decl(D, DeclVisibility), !.
    add1_new_decl(D, DeclVisibility) :- add_new_decl(D, DeclVisibility).

    :- public get_new_decl/2.
    get_new_decl(D, DeclVisibility) :-
        ModuleR = ~self,
        current_fact(module__new_decl(ModuleR, D, DeclVisibility)).

    :- public new_decl/2. % like get_prop_in_scope, but for new_decl
    new_decl(D, DeclVisibility) :-
        ModuleR = ~enclosing_star, % (nondet)
	ModuleR.get_new_decl(D, DeclVisibility).

    % TODO: duplicated, use only one name
    :- public pragma/1.
%    pragma(X) :- get_prop(X).
    pragma(X) :- get_prop_in_scope(X).
    % TODO: duplicated, use only one name
    :- public add1_pragma/1.
%    pragma(X) :- get_prop(X).
    add1_pragma(X) :- set_prop(X).

    :- public add_decl/2.
    % TODO: [Merge] from BC
    add_decl(_,_).

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
    get_assertion_(X) :- current_fact(module__assertion(~self, X)).
    add_assertion_(X) :- assertz_fact(module__assertion(~self, X)).
    del_assertion_(X) :- retractall_fact(module__assertion(~self, X)).
    get_modedef_(H,X) :- current_fact(module__modedef(H, ~self, X)).
    add_modedef_(H,X) :- assertz_fact(module__modedef(H, ~self, X)).
    del_modedef_(H,X) :- retractall_fact(module__modedef(H, ~self, X)).

    % ---------------------------------------------------------------------------
    % Special module properties

    :- public get_prop/1.
    get_prop(def_instance_of) :- !, % TODO: Simplify, merge with BC version
        ( get_prop(static_noself) ->
	    fail
	; Base = ~get_extends,
	  ( Base = ~module_s.lookup_module(basal_base)
	  ; Base = ~module_s.lookup_module(basalnv_base)
	  ) ->
	    fail
	; true
	).
    get_prop(Prop) :-
        ModuleR = ~self,
	current_fact(module__prop(ModuleR, Prop)).

    % TODO: 'exports_all' property not working in the JS backend
    :- public set_prop/1.
    set_prop(Prop) :-
        ModuleR = ~self,
	( current_fact(module__prop(ModuleR, Prop)) ->
	    true
	; assertz_fact(module__prop(ModuleR, Prop))
	).

    % TODO: use 'modifier' instead
    :- public do_not_materialize/0.
    do_not_materialize :-
        fail.
%	( get_prop(abstract) ->
%	    % module cannot be materialized since it is an abstract class
%	    true
%	; get_prop(interface) ->
%	    % module cannot be materialized since it is an interface
%	    true
%	).

    :- public selfbox/1.
    % Default boxing property for 'self' attr
    selfbox := 
	( get_prop(simple_box(_)) ?
	    unbox
	| box
	).

    :- public get_prop_in_scope/1.
    % Get a module property in this module or any of its enclosing
    % modules.
    get_prop_in_scope(Prop) :-
        ModuleR = ~enclosing_star, % (nondet)
	ModuleR.get_prop(Prop),
	!.

    :- public add1_fun_eval/2.
    % TODO: Do not use pred_ref, since we may add fun_eval to symbols
    %   that are not predicates.
    add1_fun_eval(N, A) :-
	A1 is A + 1,
	FunctorR = ~pred_ref(N, A1),
        FunctorR.set_prop(use_functional).

    % TODO: Missing.
    :- public set_eval_arith/1.
    set_eval_arith(_).

    % ---------------------------------------------------------------------------
    % Translation hooks

    :- public add1_expansion_check/1.
    % TODO: [Merge] from BC
    add1_expansion_check(_).

    :- public get_translation_hook/2.
    get_translation_hook(Kind, Tr) :-
	current_fact(module__translation_hook(~self, Kind, Tr)).
    :- public set_translation_hook/2.
    set_translation_hook(Kind, Tr) :-
	M = ~self,
	retractall_fact(module__translation_hook(M, Kind, _)),
	assertz_fact(module__translation_hook(M, Kind, Tr)).
    :- public add_translation_hook/2.
    add_translation_hook(Kind, Tr) :-
	assertz_fact(module__translation_hook(~self, Kind, Tr)).
    :- public del_translation_hook/1.
    del_translation_hook(Kind) :-
	retractall_fact(module__translation_hook(~self, Kind, _)).

    % ---------------------------------------------------------------------------
    % State model 

    % TODO: missing
    :- public add_trust_module_statemodel/2.
    add_trust_module_statemodel(_, _).

    % TODO: missing
    :- public add_statemodel/1.
    add_statemodel(_).

    % ---------------------------------------------------------------------------
    % Environment for implicits

    % TODO: Missing.

    % ---------------------------------------------------------------------------

    % TODO: [Merge] missing in BC backend
    % Free the memory required for this module definition
    % TODO: check compiler/frontend (this is not cleaning nested modules)
    :- public clean/0.
    clean :-
        AllPreds = ~all_preds,
	( member(P, AllPreds),
	  trust(P instance_of predicate_s),
	  P.clean,
	  fail
	; true
	),
        ModuleR = ~self,
	retractall_fact(module__def(ModuleR)),
	retractall_fact(module__name(ModuleR, _)),
	retractall_fact(module__prop(ModuleR, _)),
	retractall_fact(module__nested_module(_, ModuleR, _)),
	retractall_fact(module__enclosing(ModuleR, _)),
	retractall_fact(module__extends(ModuleR, _)),
	retractall_fact(module__pred(_, _, ModuleR, _)),
	retractall_fact(module__auxpred(_, _, ModuleR, _)),
	retractall_fact(module__attr(ModuleR, _)),
	retractall_fact(module__attr_init(ModuleR, _, _)),
	retractall_fact(module__native(ModuleR, _)),
	%
	retractall_fact(module__using(ModuleR, _, _)),
	retractall_fact(module__import(_, ModuleR)),
	%
	retractall_fact(module__new_decl(ModuleR, _, _)),
	%
	retractall_fact(module__transform(ModuleR, _)),
	retractall_fact(module__translation_hook(ModuleR, _, _)),
	%
	retractall_fact(module__counter(ModuleR, _)),
	%
	retractall_fact(module__assertion(ModuleR, _)).

    % ---------------------------------------------------------------------------

    :- public clean_temp/0.
    % TODO: clean new_decl here?
    clean_temp.

    :- public ret/1.
    ret := ~self.
}.

% module__def(ModuleR)
:- public data module__def/1. % TODO: do not export!
:- pred module__name(ModuleR, Name).
:- data module__name/2.
:- pred module__prop(ModuleR, Prop).
:- data module__prop/2.
:- pred module__nested_module(Name, EnclosingModuleR, ModuleR).
%  (Name is a nested module in EnclosingModuleR, with identifier ModuleR)
:- data module__nested_module/3.
:- pred module__enclosing(ModuleR, EnclosingModuleR).
%  (EnclosingModuleR is the enclosing module of ModuleR)
%  (or ModuleR is nested in EnclosingModuleR)
:- data module__enclosing/2.
:- pred module__extends(ModuleR, BaseR). % TODO: there may be more than one base clase
:- data module__extends/2.
:- pred module__pred(Name, Arity, ModuleR, FunctorR).
:- data module__pred/4.
:- pred module__auxpred(Name, Arity, ModuleR, FunctorR).
:- data module__auxpred/4.
:- pred module__attr(ModuleR, Name).
:- data module__attr/2.
:- pred module__attr_init(ModuleR, Init, AttrInitVal).
:- data module__attr_init/3.
:- pred module__native(ModuleR, Codes).
:- data module__native/2.
:- pred module__using(ModuleR, Spec, Modifier).
:- data module__using/3.
:- pred module__import(ImportedModule, ModuleR).
:- data module__import/2.
:- pred module__new_decl(ModuleR, D, DeclVisibility).
:- data module__new_decl/3.
:- pred module__transform(ModuleR, Spec).
:- data module__transform/2.
:- pred module__translation_hook(ModuleR, Kind, Tr).
:- data module__translation_hook/3.
%
:- pred module__counter(ModuleR, Counter).
:- data module__counter/2.
%
:- pred module__assertion(ModuleR, Assrt).
:- data module__assertion/2.
:- pred module__modedef(Head, ModuleR, Assrt).
:- data module__modedef/3.

all_usermod_preds := Ps :-
	findall(P, usermod_preds(P), Ps).

% Gives on backtracking all usermod functors
usermod_preds(FunctorR) :-
	% TODO: delicate...
	current_fact(pred__is_usermod_functor_(Id)),
	FunctorR = ~predicate_s.from_id(Id).

% Makes sure that the symbol is defined as a 'user' functor
:- public meta_predicate ensure_usermod_functor(?, out(predicate_s)).
ensure_usermod_functor(sympred(N, A)) := FunctorR :-
	M = ~module_s.lookup_module('user'),
        ( FunctorR0 = ~M.lookup_pred(N, A, any) ->
	    FunctorR = FunctorR0
	; FunctorR = ~M.local_functor_ref(N, A),
	  FunctorR.set_prop(static),
	  assertz_fact(pred__is_usermod_functor_(~FunctorR.id))
	).

% ===========================================================================

% TODO: add support for indexing in compilation

:- public class predicate_s {
    % Functors and predicates
    :- extends psymbol.
    :- public attr id :: any. % TODO: use '$raw_state'? (like in 'class')

    :- constructor from_id_/1.
    from_id_(Id) :- ~id = Id.

    :- constructor new_/3.
    new_(N, A, ModuleR) :-
	trust(ModuleR instance_of module_s),
	~id = ~new_pred_id(ModuleR, N, A),
	set_owner_module(ModuleR, N, A),
	% TODO: make it optional (take option from the module, or compiler option)
	set_prop(may_use_functor_trait).

    % ModuleR owns this predicate, with name N/A
    set_owner_module(ModuleR, N, A) :-
        Id = ~id,
        ( current_fact(pred__owner_module_(Id, ModuleR0, N0, A0)) ->
	    ( ModuleR0 = ModuleR, N = N0, A = A0 -> true
	    ; bug(inconsistent_module_in_pred_id),
	      fail
	    )
	; assertz_fact(pred__owner_module_(Id, ModuleR, N, A))
	).

    % Create an atom 'M1:...:Mk:N/A', used as a unique identifier for
    % the predicate or functor. This identifier is arbitrary and will
    % not end in the compiled code.
    :- static new_pred_id/3.
    new_pred_id(ModuleR, N, A) := Atom :-
	trust(ModuleR instance_of module_s),
	MN = ~ModuleR.sub_id(N),
	number_codes(A, ACodes),
	atom_codes(AAtom, ACodes),
	atom_concat(MN, '/', MN1),
	atom_concat(MN1, AAtom, Atom).

    :- public get_id/1.
    get_id := ~id.

    :- public name_spec/1.
    name_spec := (N/A) :- get_name(N, A).

    % The module where the predicate is defined
    :- public meta_predicate owner_module(out(module_s)).
    owner_module := ModuleR :-
        Id = ~id,
        current_fact(pred__owner_module_(Id, ModuleR, _, _)), !.

    % Name and arity
    :- public get_name/2.
    get_name(N, A) :-
        Id = ~id,
        current_fact(pred__owner_module_(Id, _, N, A)), !.

    % TODO: duplicated of get_name/2, merge interface (see module_exp)
    :- public name/1.
    name := N/A :- get_name(N, A).

    % Arity
    :- public get_arity/1.
    get_arity := A :-
        Id = ~id,
        current_fact(pred__owner_module_(Id, _, _, A)), !.

    % TODO: REMOVE (see module_pli:predicate_s)
    :- public a/1.
    a := ~get_arity.

    % Obtain a sibling predicate (defined in the same module) with the
    % same name but different arity (which can be itself). The input
    % arity may be uninstantiated.
    :- public meta_predicate arity_family(?, out(predicate_s)). % (nondet)
    arity_family(A) := Pred :-
        Onwer = ~owner_module,
	get_name(N, _),
        current_fact(pred__owner_module_(Id, Onwer, N, A)),
        Pred = ~predicate_s.from_id(Id).

    % ---------------------------------------------------------------------------
    % Properties of this predicate

    :- public is_defined/0.
    % TODO: Missing.
    is_defined :- fail.

    :- public get_prop/1.
    get_prop(has_clauses) :- !,
        Id = ~id,
	pred__clause_(Id, _, _),
	!.
    get_prop(Prop) :-
        Id = ~id,
        current_fact(pred__prop(Id, Prop)), !.

    :- public set_prop/1.
    set_prop(Prop) :-
        Id = ~id,
	( current_fact(pred__prop(Id, Prop)) ->
	    true
	; assertz_fact(pred__prop(Id, Prop))
	).

    :- public parse_props/1.
    parse_props([]).
    parse_props([P|Ps]) :- parse_prop(P), parse_props(Ps).

    parse_prop(Prop) :- det_prop(Prop), !, set_det(Prop).
    parse_prop(basal) :- !, set_prop(basal).
    parse_prop(unfold) :- !, set_prop(unfold).
    parse_prop(basal_builtin(M:N/A)) :- !, set_prop(basal_builtin(M,N,A)).
    parse_prop(no_worker) :- !, set_prop(use_worker(no_worker)).
    parse_prop(with_worker) :- !, set_prop(use_worker(with_worker)).
    parse_prop(argstype(ArgsType)) :- !,
	set_argstype(ArgsType).
    parse_prop(argsbox(ArgsBox)) :- !,
	set_argsbox(ArgsBox).
    parse_prop(X) :- trace(unknown_predicate_prop(X)), fail.

    :- public really_use_functor_trait/0.
    really_use_functor_trait :-
        \+ get_prop(is_functor_trait),
        get_prop(may_use_functor_trait).
	
    :- public set_def/1.
    % TODO: missing
    set_def(_).

    % Compute if the predicate needs 'self' (and caches the result)
    %
    % TODO: add a 'compute_needs_self' and invoke it when all the
    %       module declarations have been read
    % TODO: is caching really necessary?
    :- public needs_self/0.
    % TODO: negate and rename by 'is_static/0'?
    needs_self :-
        ( get_needs_self(NeedsSelf) ->
	    true
	; NeedsSelf = (defining_method ? yes | no),
	  set_needs_self(NeedsSelf)
	),
	NeedsSelf = yes.

    defining_method :-
        get_prop(static), !, fail.
    defining_method :-
        get_name(N, _), static_pred_name(N), !, fail.
    defining_method :-
        OwnerModule = ~owner_module,
	( OwnerModule.get_prop(static_noself) ->
	    fail
	; OwnerModule.get_prop(static) ->
	    % TODO: add 'static' prop or look like this?
	    fail
	; true
	).

    % the predicate needs 'self' argument
    get_needs_self(NeedsSelf) :-
        Id = ~id,
        current_fact(pred__needs_self_(Id, NeedsSelf)), !.

    % annotate that the predicate needs 'self' argument
    set_needs_self(NeedsSelf) :-
        Id = ~id,
        ( current_fact(pred__needs_self_(Id, NeedsSelf)) ->
	    true
	; assertz_fact(pred__needs_self_(Id, NeedsSelf))
	).

    :- public exported/0.
    exported :- get_prop(exported).

    % ---------------------------------------------------------------------------
    % Code associated to this predicate

    :- public set_code/1.
    set_code(Code) :-
        Id = ~id,
	retractall_fact(pred__code_(Id, _)),
	assertz_fact(pred__code_(Id, Code)).

    :- public code/1.
    code := Code :-
	current_fact(pred__code_(~id, Code)).

    :- public add_clause/4.
    add_clause(H0, B1, _Loc, UseFunExp) :-
        % TODO: in optim_comp, expansion is done later
	( UseFunExp = yes, is_fun_head(H0) ->
	    expand_fun_head(H0, Head, R, Res),
	    add_unif_res(B1, R, Res, Body)
	; Head = H0, Body = B1
	),
	assertz_fact(pred__clause_(~id, Head, Body)).

    clause(Head, Body) :-
	current_fact(pred__clause_(~id, Head, Body)).

    :- public all_clauses/1.
    all_clauses := Cs :-
        Id = ~id,
	findall(tc(Head, Body), pred__clause_(Id, Head, Body), Cs).

    % Define the predicate as insns code
    :- public set_insns_def/2.
    set_insns_def(CArgs, Code) :- !,
        % note: in detfun, CArgs does not contain the return argument
        ( needs_self, \+ get_prop(basal) -> % TODO: why not for 'basal'? FIX
	    % Add extra argument for Self (if we are defining a method)
	    CArgs2 = [self|CArgs]
	; CArgs2 = CArgs
	),
	% TODO: args of specialdef are meaningless (wrong?)
	A = ~get_real_arity,
	set_prop(specialdef(~vars(A), basaldef(CArgs2, Code))),
	As = ~vars(A),
	set_code(icode(a, As, or([['\6\predid_apply'(~self, As)]]))).

    % ---------------------------------------------------------------------------
    % Flattening

    % Real arity (adding argument for 'self' if necessary)
    :- public get_real_arity/1.
    get_real_arity := A :-
        A0 = ~get_arity,
	( needs_self -> A is A0 + 1 ; A = A0 ).

    :- public set_det/1.
    set_det(Det) :-
        Id = ~id,
        assertz_fact(pred__det_(Id, Det)).
    :- public get_det/1.
    get_det := Det :-
        Id = ~id,
        current_fact(pred__det_(Id, Det0)), !, Det = Det0.

    set_argstype(ArgsType) :-
        Id = ~id,
        assertz_fact(pred__argstype_(Id, ArgsType)).
    get_argstype(ArgsType) :-
        Id = ~id,
        current_fact(pred__argstype_(Id, ArgsType0)), !, ArgsType = ArgsType0.

    % argstypes, including self if necessary
    % (uses 'term' as default type)
    real_argstype(ArgsType2) :-
        ( ArgsType = ~get_argstype -> true
	; ArgsType = ~repeat_n(~get_arity, term)
	),
        ( needs_self ->
	    % Add type for Self (if we are defining a method)
	    OwnerModule = ~owner_module,
	    ArgsType2 = [OwnerModule|ArgsType]
	; ArgsType2 = ArgsType
	).

    set_argsbox(ArgsBox) :-
        Id = ~id,
        assertz_fact(pred__argsbox_(Id, ArgsBox)).
    get_argsbox(ArgsBox) :-
        Id = ~id,
        current_fact(pred__argsbox_(Id, ArgsBox0)), !, ArgsBox = ArgsBox0.

    % argsbox, including self if necessary
    % (uses 'box' as default box)
    real_argsbox(ArgsBox2) :-
        ( ArgsBox = ~get_argsbox -> true
	; ArgsBox = ~repeat_n(~get_arity, box)
	),
        ( needs_self ->
	    % Add box property for Self (if we are defining a method)
	    OwnerModule = ~owner_module,
	    OwnerModule.selfbox(SelfBox),
	    ArgsBox2 = [SelfBox|ArgsBox]
	; ArgsBox2 = ArgsBox
	).

    % mix of boxing and type for the arguments
    % TODO: Add 'box/1' and 'unbox/1' operations in js_native syntax,
    %       and optimize the code to avoid excessive translations.
    % TODO: Add Add 'box/1' and 'unbox/1' operations in js_native syntax,
    %       and optimize the code to avoid excessive translations.
    :- public get_boxingtypes/1.
    get_boxingtypes(BoxingTypes) :-
        ArgsBox = ~real_argsbox,
        ArgsType = ~real_argstype,
	combine_box_type(ArgsBox, ArgsType, BoxingTypes).

    :- static combine_box_type/3.
    % combine_box_type(ArgsBox, ArgsType, BoxingTypes)
    combine_box_type([], [], []).
    combine_box_type([X|Xs], [Y|Ys], [Z|Zs]) :-
        combine_box_type_(X, Y, Z), !,
        combine_box_type(Xs, Ys, Zs).

    :- static combine_box_type_/3.
    combine_box_type_(box, _Type, box).
    combine_box_type_(unbox, Type, unbox(Type)).

    % the mode of the predicate
    :- public get_det_mode/1.
    get_det_mode := DetMode :-
        Id = ~id,
        ( is_semidet_(Id) -> DetMode = semidet
	; current_fact(pred__det_(Id, DetMode0)) ->
	    DetMode = DetMode0
	; DetMode = nondet % nondet by default
	).

    :- public get_with_worker/1.
    get_with_worker := unknown.

    % ---------------------------------------------------------------------------
    % Query predicate/method refs

    :- public meta_predicate query_basal(out(psymbol)).
    % Principal basal predicate
    % TODO: rename; what is this? is predicate_s both ref_BK and ref_PK?
    query_basal := FunctorR :-
        ( get_prop(use_worker(UseWorker)) ->
	    true
	; UseWorker = with_worker
	),
        get_name(N, A),
	ModuleR0 = ~owner_module,
	DetMode = ~get_det_mode,
	FunctorR = ~ref_BKM.new(ModuleR0, N, A, UseWorker, DetMode).

    % Reference to a ref_BKM or ref_BKS (of this predicate object)
    :- public meta_predicate query_ref_BK(?, ?, out(psymbol)).
    query_ref_BK(N, A) := ~module_s.query_ref_BK(~self, N, A).

    % Reference to a ref_BUM or ref_BUS (of the predicate object)
    :- public static query_ref_BU/3.
    query_ref_BU(N, A) := ~module_s.query_ref_BU('\6\pred', N, A).

    % ---------------------------------------------------------------------------
    % Foreign (JS)

    {
    % Predicates defined as JS code by the user
    % (see lib/js_lang.pl package for documentation)
    % TODO: separate and generalize for other languages?

    % TODO: only one goal is allowed in those predicates, generalize?
    :- public static clause_defines_js_native/3.
    clause_defines_js_native(Cs, Args, Code) :-
        Cs = [tc(Head, Body)], nonvar(Body),
	match_js_lang(Body, Body1),
        ( Body1 = js_lang_stats(Code0),
	  Head =.. [_|Args0] ->
	    Code = Code0, Args = Args0
	; Body1 = js_lang_stats(Code0, Ret),
	  Head =.. [_|As0],
	  append(Args0, [Ret0], As0),
	  Ret0 == Ret ->
	    % (for functional notation)
	    Code = Code0, Args = Args0
	; fail
	).

    :- static match_js_lang/2.
    match_js_lang(Body0, Code) :-
        % Body0 is some js_lang.Goal or Ret = ~js_lang.Goal
	( Body0 = (Ret = ~funcall(~mcall(JSLang, Goal0))), nonvar(Goal0) ->
	    Goal0 =.. [N|As0],
	    append(As0, [Ret], As),
	    Goal =.. [N|As]
        ; Body0 = ~mcall(JSLang, Goal0) ->
	    Goal = Goal0
	; fail
	),
        JSLang == js_lang, % fake module for JS lang code
	% Valid goals for js_lang
        ( Goal = stats(Code0) ->
	    Code = js_lang_stats(Code0)
        ; Goal = stats(Code0, Ret) ->
	    Code = js_lang_stats(Code0, Ret)
        ; Goal = expr(Code0, Ret) ->
	    Code = js_lang_stats([return(Code0)], Ret)
        ; Goal = test(Code0) ->
	    Code = js_lang_stats([return(Code0)])
	; fail
	).
    }.

    % ---------------------------------------------------------------------------
    % Forget the module/class to get a ref_BUM or ref_BUS
    % (necessary to call or define virtuals)

    :- public get_PU/1.
    get_PU := Ref :- % TODO: missing ref_PUS!
	get_name(N, A),
	Ref = ~ref_PUM.new(N, A).

    % -----------------------------------------------------------------------

    :- public meta_predicate new_sub(?, out(predicate_s)).
    % Create a subpredicate
    new_sub(NewArity) := NewPred :-
        ModuleR = ~owner_module,
	Atom = ~ModuleR.new_atom,
	NewPred = ~ModuleR.pred_ref(Atom,NewArity),
	( get_prop(static) -> NewPred.set_prop(static) ; true ).

    % -----------------------------------------------------------------------

    clean :-
        R = ~id,
	retractall_fact(pred__owner_module_(R,_,_,_)),
	retractall_fact(pred__needs_self_(R, _)),
	retractall_fact(pred__prop(R, _)),
	retractall_fact(pred__clause_(R, _, _)),
	retractall_fact(pred__code_(R, _)),
        retractall_fact(pred__det_(R,_)),
        retractall_fact(pred__argstype_(R,_)),
        retractall_fact(pred__argsbox_(R,_)),
        retractall_fact(pred__is_usermod_functor_(R)).
}.

:- pred pred__owner_module_(Id, ModuleR, N, A).
:- data pred__owner_module_/4.
:- pred pred__needs_self_(Id, NeedsSelf).
:- data pred__needs_self_/2.
:- pred pred__prop(Id, Prop).
:- data pred__prop/2.
:- pred pred__clause_(Id, Args, Body).
:- data pred__clause_/3.
:- pred pred__code_(Id, Code).
:- data pred__code_/2.
:- pred pred__det_(Id, Det).
:- data pred__det_/2.
:- pred pred__argstype_(Id, ArgsType).
:- data pred__argstype_/2.
:- pred pred__argsbox_(Id, BoxArgs).
:- data pred__argsbox_/2.
:- pred pred__is_usermod_functor_(Id).
:- data pred__is_usermod_functor_/1.

% ---------------------------------------------------------------------------
% Determinism properties

% A determinism property.
%
% The determinism property specifies the control mechanism of the
% given predicate. Along with the changes in the worker, the
% predicate, once translated to a procedure, specifies the control
% flow as a returned argument. It can be one of the following:
%
%  - nondet: returns a reference to a closure that implements
%            the next continuation (which can be success or 
%            failure)
%  - semidet: returns a boolean (uses success or 
%             fail continuation, but cannot change the fail
%             continuation).
%  - semidetfun: returns one argument, or special value 'null'
%                (uses fail continuation if 'null', success
%                otherwise)
%  - detfun: returns one argument (uses success continuation).
%  - det: returns nothing (uses success continuation).
det_prop(nondet).
det_prop(semidet).
det_prop(semidetfun).
det_prop(detfun).
det_prop(det).

% Detfun or variant
:- public is_detfun/1.
is_detfun(detfun).
is_detfun(semidetfun).

% ---------------------------------------------------------------------------

% Names of predicates that are always static (not methods)
% TODO: move to the source?
:- public static_pred_name/1.
static_pred_name('$check').
static_pred_name('$alloc').
static_pred_name('new__').
static_pred_name('internal_cons__').

% TODO: builtins
is_semidet_('term_basic:=/2').
is_semidet_('basiccontrol:$caller_choice/1').
is_semidet_('basiccontrol:$get_choice/1').
is_semidet_('basiccontrol:$cut/1').
%is_semidet_('basiccontrol:true/0').
is_semidet_('basiccontrol:fail/0').
is_semidet_('$trail_set_attr/3').

% ---------------------------------------------------------------------------

is_fun_head(Head) :- nonvar(Head), Head = ':='(_, _).

% From N(A1,...,AN) := Value, obtain N(A1,...,AN,ResVar)
expand_fun_head((Head := Value), NewHead, ResVar, Value) :-
        Head =.. [N|As],
        append(As, [ResVar], As2),
        NewHead =.. [N|As2].

% TODO: probably duplicated in fsyntax package
add_unif_res(A, R, Res, Body) :- var(A), !, Body = (A, R = Res).
add_unif_res(true, R, Res, Body) :- !, Body = (R = Res).
add_unif_res(X, R, Res, Body) :- var(Res), X = (A ; B), !,
	% (special case to avoid auxiliary predicates, both R and Res are vars)
	add_unif_res(A, R, Res, BodyA),
	add_unif_res(B, R, Res, BodyB),
	Body = (BodyA ; BodyB).
add_unif_res(X, R, Res, Body) :- X = (A -> B), !,
	% (special case to avoid auxiliary predicates, both R and Res are vars)
	add_unif_res(B, R, Res, BodyB),
	Body = (A -> BodyB).
add_unif_res(A, R, Res, Body) :- Body = (A, R = Res).

% ---------------------------------------------------------------------------

% Mems for arguments of (JS) functions
:- public cargs_mem/2.
cargs_mem(N) := ~cargs_mem__2(0, N).
cargs_mem__2(I, N) := [] :- I >= N, !.
cargs_mem__2(I, N) := [cargmem(I)| ~cargs_mem__2(I1, N)] :-
    	I1 is I + 1.

% Mems for argument registers
:- public args_mem/2.
args_mem(N) := ~args_mem__2(0, N).
args_mem__2(I, N) := [] :- I >= N, !.
args_mem__2(I, N) := [a(I)| ~args_mem__2(I1, N)] :-
    	I1 is I + 1.

% Mems for structure arguments
:- public strargs_mem/3.
% strargs_mem :: aterm_var * int * list(varmem)
strargs_mem(Str, N) := ~strargs_mem__2(Str, 0, N).
strargs_mem__2(_Str, I, N) := [] :- I >= N, !.
strargs_mem__2(Str, I, N) := [strmem(Str, I)| ~strargs_mem__2(Str, I1, N)] :-
    	I1 is I + 1.

% TODO: deprecate?
:- public vars/2.
vars(N) := ~vars__2(0, N).
vars__2(I, N) := [] :- I >= N, !.
vars__2(I, N) := [_| ~vars__2(I1, N)] :- I1 is I + 1.

:- public plain_mem/2.
plain_mem([]) := [].
plain_mem([X|Xs]) := [rawmem(X)| ~plain_mem(Xs)].

% ===========================================================================
% References to predicates and methods (including functors)
%
% The predicate references can be classified in:
%  1) (B) basal definition        / (P) program definition
%  2) (U) unknown module or class / (K) known module or class
%  3) (S) static                  / (M) method
%
% We name each reference as ref_***, where the last three letters
% correspond to the legend above, as follows:
%
%          Basal   Module/Class   Method/Static
%     BKM   yes       known          method
%     BKS   yes       known          static
%     BUM   yes      virtual         method
%     BUS   yes      virtual         static
%

% Reference to an unknown class method (ref_PUM)
% TODO: define ref_PUS
% TODO: ref_PUS and ref_PUM are the 'unknown module' equivalent of predicate_s, rename?
%       (predicate_s is equivalent to ref_PKS and ref_PKM)
:- public class ref_PUM {
    :- extends psymbol.

    :- public attr name :: any.
    :- public attr arity :: any.
    
    :- constructor new_/2.
    new_(Name, Arity) :-
	~name = Name,
	~arity = Arity.

    get_with_worker := with_worker.
    get_det_mode := nondet.

    :- public query_ref_BU_buildstr/1.
    query_ref_BU_buildstr := R :-
	N = ~name,
	A = ~arity,
	% (does the query on the owner module)
	R = ~module_s.query_ref_BU('\6\any', 'buildstr'(N, A), A).
}.

% Reference to a basal, unknown class method (ref_BUM)
:- public class ref_BUM {
    :- extends psymbol.

    :- public attr name :: any.
    :- public attr arity :: any.
    :- public attr with_worker :: any.
    :- public attr det_mode :: any.

    :- constructor new_/4.
    new_(Name, Arity, WithWorker, DetMode) :-
	~name = Name,
	~arity = Arity, % TODO: Arity is not used
	~with_worker = WithWorker,
	~det_mode = DetMode.

    get_with_worker := ~with_worker.
    get_det_mode := ~det_mode.
}.

% Reference to a basal, unknown class predicate (ref_BUS)
:- public class ref_BUS {
    :- extends psymbol.

    :- public attr name :: any.
    :- public attr arity :: any.
    :- public attr with_worker :: any.
    :- public attr det_mode :: any.

    :- constructor new_/4.
    new_(Name, Arity, WithWorker, DetMode) :-
	~name = Name,
	~arity = Arity, % TODO: Arity is not used
	~with_worker = WithWorker,
	~det_mode = DetMode.

    get_with_worker := ~with_worker.
    get_det_mode := ~det_mode.
}.

% Reference to a basal, known class static predicate (ref_BKS)
:- public class ref_BKS {
    :- extends psymbol.

    :- public attr owner_module :: module_s.
    :- public attr name :: any.
    :- public attr arity :: any.
    :- public attr with_worker :: any.
    :- public attr det_mode :: any.

    :- constructor new_/5.
    new_(ModuleR, Name, Arity, WithWorker, DetMode) :-
        ~owner_module = ModuleR,
	~name = Name,
	~arity = Arity, % TODO: Arity is not used
	~with_worker = WithWorker,
	~det_mode = DetMode.

    get_with_worker := ~with_worker.
    get_det_mode := ~det_mode.
}.

% Reference to a basal, known class method (ref_BKM)
:- public class ref_BKM {
    :- extends psymbol.

    :- public attr owner_module :: module_s.
    :- public attr name :: any.
    :- public attr arity :: any.
    :- public attr with_worker :: any.
    :- public attr det_mode :: any.
    
    :- constructor new_/5.
    new_(ModuleR, Name, Arity, WithWorker, DetMode) :-
        ~owner_module = ModuleR,
	~name = Name,
	~arity = Arity, % TODO: Arity is not used
	~with_worker = WithWorker,
	~det_mode = DetMode.

    % TODO: necessary? (for psymbol...)
    get_with_worker := ~with_worker.
    get_det_mode := ~det_mode.
}.

% ---------------------------------------------------------------------------
% Annotated terms

:- compilation_fact(use_backend(js)).
:- include(compiler(annotated_terms)).

% ---------------------------------------------------------------------------

:- public psymbol_arity/2.
psymbol_arity(Ref, Arity) :-
	Ref instance_of predicate_s, !,
	( adhoc_predid(~Ref.id, _, Arity0) -> Arity = Arity0
	; Arity = ~Ref.get_real_arity
	).
psymbol_arity(Ref, Arity) :-
	psymbol_name(Ref, _/Arity).

:- public psymbol_name/2.
psymbol_name(Ref, Name) :-
	Ref instance_of ref_PUM, !,
	N = ~Ref.name,
	A = ~Ref.arity,
	Name = N/A.
psymbol_name(Ref, Name) :-
	trust(Ref instance_of predicate_s),
	( adhoc_predid(~Ref.id, N, A) ->
	    Name = N/A
	; Name = ~Ref.name
	).

% TODO: FIX! add those predicates to their modules
adhoc_predid('$trail_set_attr/3', '$trail_set_attr', 3).
adhoc_predid('$nodef/0', '$nodef', 0).
adhoc_predid('$suspend/0', '$suspend', 0).

% ---------------------------------------------------------------------------
% (Auxiliary)

:- public fsR/2.
% Resolve a symbolic filesystem name
fsR(final_js(FsId)) := R :-
	get_out_dir(OutDir),
	R = ~atom_concat(OutDir,
	      ~atom_concat('/', ~atom_concat(FsId, '.out.js'))).
fsR(dyn(Suffix, FsId)) := R :-
	get_out_dir(OutDir),
	R = ~atom_concat(OutDir,
	      ~atom_concat('/', ~atom_concat(FsId,
	        ~atom_concat('.dyn.', Suffix)))).

:- multifile get_out_dir/1. % TODO: find a better way (comp_js.pl) (use store.pl)
:- '$ctxprj'(get_out_dir/1, []).

% ===========================================================================
:- doc(section, "Internal predicate tables").

% TODO: get this information from assertions in the source code
% binfo(N, A, Mod, MethodOrStatic, WithWorker, Semidet): Info for basal predicates and methods
% (Special constructor predicate)
binfo('ctor', _A, '\6\any', static, no_worker, det). % (a family)
% (Methods of any object)
binfo('sd_unify_nonvar', 1, '\6\any', method, with_worker, semidet).
binfo('unbox', 0, '\6\any', method, no_worker, detfun).
% (Methods of 'term' object)
binfo('sd_unify', 1, 'term', method, with_worker, semidet).
% TODO: those only for 'arithmetic objects' (refine this
%       classification, define as indexing tables embedded into
%       the object? or similar)
binfo('b_eq', 1, 'term', method, no_worker, semidet).
binfo('b_neq', 1, 'term', method, no_worker, semidet).
binfo('b_ge', 1, 'term', method, no_worker, semidet).
binfo('b_gt', 1, 'term', method, no_worker, semidet).
binfo('b_le', 1, 'term', method, no_worker, semidet).
binfo('b_lt', 1, 'term', method, no_worker, semidet).
binfo('b_inc', 1, 'term', method, no_worker, detfun).
binfo('b_dec', 1, 'term', method, no_worker, detfun).
binfo('b_add', 2, 'term', method, no_worker, detfun).
binfo('b_sub', 2, 'term', method, no_worker, detfun).
binfo('b_neg', 1, 'term', method, no_worker, detfun).
binfo('b_mul', 2, 'term', method, no_worker, detfun).
binfo('b_div', 2, 'term', method, no_worker, detfun).
binfo('b_idiv', 2, 'term', method, no_worker, detfun).
binfo('b_bitwise_and', 2, 'term', method, no_worker, detfun).
binfo('b_bitwise_or', 2, 'term', method, no_worker, detfun).
binfo('b_mod', 2, 'term', method, no_worker, detfun).
binfo('b_lsh', 2, 'term', method, no_worker, detfun).
binfo('b_rsh', 2, 'term', method, no_worker, detfun).
binfo('b_to_str', 1, 'term', method, no_worker, detfun).
% (Methods of 'worker' object)
binfo('proceed', 0, 'worker', method, no_worker, nondet).
binfo('dealloc', 0, 'worker', method, no_worker, det).
binfo('solve', 1, 'worker', method, no_worker, det).
binfo('sd_solve', 1, 'worker', method, no_worker, semidet).
binfo('cut', 1, 'worker', method, no_worker, det).
binfo('nodef', 0, 'worker', method, no_worker, nondet).
binfo('suspend', 0, 'worker', method, no_worker, nondet).
binfo('push_choice', 1, 'worker', method, no_worker, det).
% (Methods of 'attrvar')
binfo('trail_set_attr', 2, 'attrvar', method, with_worker, det).
% (Methods of 'choice')
binfo('fail', 0, 'choice', method, with_worker, nondet).
% (Methods of the root module)
% (the 'root' object contains the whole program text)
binfo('__start__', 0, '\6\root', static, no_worker, det).
binfo('__mainblock__', 0, '\6\root', static, no_worker, det). % special, the outmost scope
% (Methods of the module/class object)
binfo('buildstr'(_,_), _, '\6\any', method, no_worker, detfun).
% (Methods of the functor/predicate object)
binfo('execute', 0, '\6\pred', method, with_worker, nondet).
binfo('partial_mec', _A, '\6\pred', method, with_worker, semidet). % (a family)
binfo('partial_mgc', _A, '\6\pred', method, with_worker, nondet). % (a family)
% TODO: note that DetMode is a parameter of the name itself...
binfo('natcode'(DetMode), _, '\6\pred', static, no_worker, DetMode). % (a family)
binfo('copy_fresh', 0, '\6\pred', method, no_worker, nondet).
binfo('new_fresh', 2, '\6\pred', static, with_worker, semidetfun).

:- public static type_to_qmod/2.
% TODO: remove, do name resolution
type_to_qmod('t_string', ':'('string_type_rt', 't_string')) :- !.
type_to_qmod('js_object', ':'('js_foreign', 'js_object')) :- !.
type_to_qmod('t_num', ':'('arithmetic', 't_num')) :- !.
type_to_qmod('var', ':'('term_typing', 'var')) :- !.
type_to_qmod('t_module', ':'('basiccontrol', 't_module')) :- !.
type_to_qmod('nonvar', ':'('basiccontrol', 'nonvar')) :- !.
type_to_qmod('attrvar', ':'('attr_rt', 'attrvar')) :- !.
type_to_qmod('worker', ':'('basiccontrol', 'worker')) :- !.
type_to_qmod(Mod, Qual) :- Qual = Mod.


