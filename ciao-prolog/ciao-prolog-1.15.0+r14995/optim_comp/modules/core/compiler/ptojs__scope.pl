% TODO: merge with mexpand.pl (mexpscope, tbl, mtdic, etc.)
% (see semantics.tex)
%:- include(compiler(ptojs__errlog)). % (defines error/1)

:- use_module(compiler(meta_syntax), [sym_to_spec/2, decomp_goal/3]).

:- class scope {
    % :- doc(title, "A predicate scope (for symbol lookup)").
    %
    % This is the scope for predicates. It is connected with the scope
    % of the owner class of the predicate.
    %
    % TODO: Add imported classes/modules, etc. here too?
    % TODO: Equivalent to module resolution in mexpand.pl (merge at some point)

    % The state of a @class{scope} is defined by:
    :- attr currmodule :: module_s. % The module we are defining
    :- attr selfvar :: m_any.   % The 'self' variable

    :- constructor new_/1.
    new_(ModuleR) :-
        ~currmodule = ModuleR.

    :- constant get_selfvar/1.
    get_selfvar := Self :- ~selfvar = yes(Self).

    set_selfvar(Self) :- selfvar <- yes(Self).

    % :- doc(section, "Preliminaries").
    %
    % Notation:
    %   - flat_term: terms in the flat domain
    %   - symbol: pairs Functor/Arity representing predicates or functions

    % ---------------------------------------------------------------------------
    % :- doc(section, "Symbol lookup algorithm").
    %
    % We distinguish the cases for qualified (Obj.Name/Arity) and
    % non-qualified (Name/Arity) symbol resolution. This resolution is
    % done statically, which means that we may not actually know the
    % concrete object. Thus, we pass an abstraction of the object
    % (typically the class or module, if known) for the qualified
    % case.
    %
    % Qualified lookup (with an abstract qualifier Q):
    % 
    %   1: If you know that the symbol is defined in Q:
    %        return a reference to the symbol.
    %   2: If you know that it is not:
    %        repeat step 1 with the (abstract) ancestor of Q.
    %   3: If know nothing:
    %        return a special reference to the symbol
    %        (which may not actually exist).
    %   
    %   RULE: Qualified lookup will only find nested definitions
    %         (thus, if 'a' is imported in 'b', 'b.a' is not defined,
    %         although 'a' is find in the scope of 'b').
    %
    % Unqualified lookup:
    %
    %   Check if the symbol appears in the lexical scope.
    %
    %   SCOPE_LOOKUP(C, N/A): find if N/A symbol corresponds to some
    %     imported predicate or method in C.
    %
    %   1: Start with C = ~currmodule
    %   2: If SCOPE_LOOKUP(C, N/A), goto 4.
    %   3: Repeat with a new C being the enclosing module of C.
    %      (scopes are chained)
    %
    %   4: If N/A requires self, but the module is not ~currmodule,
    %      emit an error.
    %
    % TODO: Resolve those issues:
    %  - Finish this code to implement correct use_module declaration.
    %  - Emit ambiguity warnings (warn when some local definition overrides
    %    an imported definition, and there was no redefine/1 declaration).
    %  - Take visibility into account.
    %
    % TODO: Abstract lookup may be a good reason to do analysis at the
    %       source level, and not at the 'bytecode' level (the
    %       bytecode could radically change after aggressive
    %       optimizations).
    % 
    % Notes on multiple inheritance:
    %   
    %   Consider C3 MRO (method resolution order) 
    %   About method resolution order (MRO). 
    %   A popular algorithm is C3 MRO
    %   (http://search.cpan.org/~flora/perl-5.14.2/ext/mro/mro.pm)
 
    % ---------------------------------------------------------------------------
    %
    % :- regtype functor_r := _ # "Resolved functor".
    % :- regtype term_r := ^term_r(functor_r, maybe_obj, list(term)) # "Resolved term,
    %    containing a resolved functor, maybe_obj, and a list of arguments".
    %
    % :- doc(on_unknown/1, "Behaviour of symbol resolution when the
    %    term is not in the scope").
    % :- regtype on_unknown :=
    %        unqual_user # "A user functor, if the term was unqualified"
    %      | no          # "Prompt an error"
    %      | silent_fail # "Fail".
    %
    % :- doc(ri_functor/1, "Resolved+instance functor (functor + object if necessary)").
    % :- regtype ri_functor := 
    %      | ri_fnc_o(_,_) # "Resolved functor with object term"
    %      | ri_fnc(_)   # "Resolved functor, no object term".
    % % Note: Functors for class instances contain the self object 
    % %       as a term. This is similar to functors in high-order logic.
    % %       Make sure that we do not have logical paradoxes (e.g. Curry's paradox)
    %
    % ---------------------------------------------------------------------------
    % (export)
    %
    % :- fun resolve_term/2 :: term * on_unknown -> rterm 
    % # "Resolve a @regtype{term} to obtain a @regtype{term_r},
    %    controlled by @var{OnUnknown}".
    %
    resolve_term(X, OnUnknown) := C :- functor(X, call, _), !,
	% transform call(G,A1..An) into G.ho_apply__(A1..An)
	X =.. [_,A|As],
	Method =.. ['ho_apply__'|As],
	C = ~resolve_term(~mcall(A, Method), OnUnknown).
    % Other calls
    resolve_term(X, OnUnknown) := C :-
	% Split the term @var{X} into:
        %  - a qualifier (which can be a term, a module specifier, or none),
        %    the abstract qualifier (pair @var{QualObj0} and @var{QualMod}).
        %  - an unqualified term @var{X0}
        %  - @var{Sym}, the symbol of @var{X0}
        %  - @var{Args0}, the arguments of @var{X0}
	get_qualifier(X, Qual, X0),
	decomp_goal(X0, Sym, Args),
	FnObj0 = ~resolve_symbol(Qual, Sym, OnUnknown),
	FnObj = ~forget_overridable(Sym, FnObj0), % TODO: optional?
        C = ~apply_ri_functor(FnObj, Args).

    % :- fun apply_ri_functor/3 :: ri_functor * list(term) -> term_r
    % # "Apply FnObj to the arguments (to obtain a resolved term,
    %    with extra obj arguments if necessary)".
    %
    apply_ri_functor(FnObj, Args) := C :-
	( FnObj = ri_fnc_o(FunctorR, Obj1) ->
	    Obj = ~ensure_obj(Obj1), MaybeObj = yes(Obj)
	; FnObj = ri_fnc(FunctorR), MaybeObj = no
	),
	C = term_r(FunctorR, MaybeObj, Args).

    % :- fun resolve_symbol/4 :: qual * sym * on_unknown -> ri_functor
    % # "Resolve a symbol (with given optional qualifier)".
    %
    % TODO: Richer abstractions for @var{Qual}
    %       (it could be a richer type domain)
    % TODO: Consider overridable/non-overridable predicates
    % TODO: What about static_noself predicates?
    % TODO: Generalize to resolve module names too
    %
    resolve_symbol(Qual, Sym, OnUnknown) := FnObj :-
	% Resolve @var{Sym} w.r.t. the qualifier
        on_unknown :: any <- OnUnknown,
	( ~resolve_symbol_(Qual, Sym) = FnObj0 ->
	    FnObj = FnObj0
	; OnUnknown = fail ->
	    fail
	; % OnUnknown = error
	  error(pred_not_found_in_scope(~sym_to_spec(Sym), ~self)),
	  fail
	).

    {
    :- fluid on_unknown :: any.

    % :- fun resolve_symbol_/3 :: qual * sym -> ri_functor.
    resolve_symbol_(qual_none, Item) := FnObj :- !,
        FnObj = ~resolve_unqual_name(Item).
    resolve_symbol_(qual_o(Obj0), sympred(N, A)) := FnObj :- !,
        % Lookup in an unknown module
        % TODO: Do a lookup in the 'top object' and invoke .get_PU
        %       (since there is a single initial object, we can have something here everytime)
        % TODO: Missing runtime checks for limiting visibility.
	FunctorR = ~ref_PUM.new(N, A),
	FnObj = ri_fnc_o(FunctorR, Obj0).
    resolve_symbol_(qual_m(M), Item) := FnObj :-
	( var(M) ->
	    throw(not_implemented_var_in_module)
	; M = ':'(A, B) ->
	    % Look for B directly defined as part of A (module navigation)
	    ri_fnc(QualModuleR) = ~resolve_symbol_(qual_m(A), symmod(B))
	; ri_fnc(QualModuleR) = ~resolve_symbol_(qual_none, symmod(M))
	),
	% Search symbol @var{Item} within this module as qualifier
	trust(QualModuleR instance_of module_s),
	( Def = ~QualModuleR.lookup(Item, any) -> % TODO: Visibility is not right
	    % Found in a nested_module (from current module environment)
	    FnObj = ri_fnc(Def)
	; fail % TODO: compilation error?
	).

    % :- pred resolve_unqual_name/2 :: sym -> ri_functor
    % # "Resolve unqualified item. Silently fails if cannot be found.".
    resolve_unqual_name(Item) := FnObj :-
        % TODO: warn about ambiguity problems
	QualModuleR = ~currmodule,
	trust(QualModuleR instance_of module_s),
	% (Look in the scope chain)
	OneModuleR = ~related_module(QualModuleR),
	( % 1: Look for a local definition
	  Def0 = ~OneModuleR.lookup(Item, any) ->
	    % Found in the module or a nested_module (from current module environment)
	    Def = Def0,
	    ( Item = sympred(_, _),
	      FunctorR0 = Def,
	      trust(FunctorR0 instance_of predicate_s),
	      OwnerModuleR = ~FunctorR0.owner_module,
	      ~currmodule = OwnerModuleR ->
	        I = 'self'
	    ; I = ~OneModuleR.get_name
	    )
	; % 2: Look for an imported definition
	  I = ~OneModuleR.get_import,
	  trust(I instance_of module_s),
	  Def0 = ~I.lookup(Item, exported) ->
	    % TODO: object for method may require some navigation in the case of nested scopes
	    Def = Def0
	; fail
	),
	!,
	% Object used to access the item
	trust(Def instance_of predicate_s),
	( Def.needs_self ->
	    % TODO: add the object path
	    FnObj = ri_fnc_o(Def, ~funcall(I))
%	    ( OneModuleR.get_prop(static_noself) ->
%	        QualObj = yes(~funcall(I))
%	    ; QualObj = yes(~mcall_(self, ~funcall(I))) % TODO: necessary?
%	    )
	; FnObj = ri_fnc(Def)
	).
    % Not found in any scope, use a usermod symbol (similar to XSB behaviour)
    resolve_unqual_name(Item) := FnObj :-
	~on_unknown = unqual_user,
	Item = sympred(_,_),
	ModuleR = ~currmodule,
	trust(ModuleR instance_of module_s),
	ModuleR.get_prop_in_scope(user_term_on_undefined),
	% TODO: register usermod functor
	FnObj = ri_fnc(~ensure_usermod_functor(Item)).
    }.

    % Modules consulted during symbol lookup (enumerated in the right
    % order by backtracking).
    % TODO: extend with inheritance?
    % TODO: use enclosing_star?
    :- meta_predicate related_module(module_s, out(module_s)).
    related_module(C) := ( C | ~related_module(~C.enclosing_module) ).

    % :- fun forget_overridable/3 :: sym * ri_functor -> ri_functor.
    %
    % Forget the module (or class) of 'FunctorR' if the predicate can
    % be overriden.
    %
    % Even if we know the method, we will use a reference to a
    % method of an unknown module, since the actual method code
    % may be unknown if overriden.
    %
    % TODO: Make predicates 'non-overridable' by default,
    %       Use this only for 'virtual' predicates.
    %
    % TODO: Refine, if we know the exact module (or class) this is not
    % necessary.
    %
    forget_overridable(Sym, FnObj) := FnObj2 :-
        on_unknown :: any <- fail,
	( may_be_overriden(FnObj) ->
	    FnObj = ri_fnc_o(_, QualObj1),
	    FnObj2 = ~resolve_symbol_(qual_o(QualObj1), Sym)
	; FnObj2 = FnObj
	).

    % :- pred may_be_overriden/1 :: ri_functor
    % # "The resolved functor may be overriden (in a descendant
    %    class)".
    %
    % TODO: make sure that we can still call 'super' predicates,
    %       etc. ; maybe we need a property like
    %       FunctorR0.is_virtual_ref, which is true if we may
    %       follow virtual references or false if we do not (that
    %       would be a good way to specify exact methods)).
    %
    may_be_overriden(FnObj) :-
        FnObj = ri_fnc_o(FunctorR0, _),
	trust(FunctorR0 instance_of predicate_s),
	OwnerModuleR = ~FunctorR0.owner_module,
	~currmodule = OwnerModuleR.
}.
