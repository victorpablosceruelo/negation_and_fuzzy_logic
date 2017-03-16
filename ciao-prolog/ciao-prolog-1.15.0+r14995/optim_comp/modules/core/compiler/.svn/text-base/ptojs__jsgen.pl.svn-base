:- use_module(compiler(meta_syntax)).

% How structure arguments are stored (base[<i>] vs base.arg<i>)
%:- compilation_fact(strarg_array).
% Notes: strargs_array produces code almost twice slower
% TODO: fix '$arg'/2 to make it modular (anyway, it is very slow)

% ---------------------------------------------------------------------------
% From middle-level definitions to JavaScript

:- use_module(compiler(write_js)).

middefs_to_js(Kind, FsId, Defs) :-
        Stats = ~middefs_to_js_(Kind, Defs),
	write_native(Stats, FsId).

middefs_to_js_(Kind, Defs) := Stats :-
	enclosing :: any <- '\6\root', % TODO: just a name for 'nothing', do in other way?
	call(( stats :: accum(Stats), tr_module_def('\6\root', Kind, Defs, _MSyms) )),
	!.
middefs_to_js_(Kind, _) := _ :-
	throw(bug_failed_middefs_to_js_(Kind)).

write_native(Code, Spec) :-
	NativeName = ~fsR(dyn(js, Spec)),
	% store:addr_new(compile__js(Spec), NativeName),
	w :: js_writer <- ~js_writer.new,
	w.to_file(NativeName, Code).

% ---------------------------------------------------------------------------
% Translation of middle-level code into JS-tree code (abstract syntax tree)

{
:- fluid enclosing :: any.
:- fluid stats :: accum. % Statements (output)

% tr_module_def(+Ref, +Kind, +Defs, -MSyms)
tr_module_def(Ref, Kind, Defs, MSyms) :-	
	split_defs(Defs, Extends, Ctor, SAttrDecls, Statics, Methods, Nested, Traits, BootCode),
	call((
	  enclosing :: any <- Ref,
	  symdic :: cdic <- ~cdic.new,
	  call((
            stats :: accum(MDefs),
	    tr_defs(Traits), % TODO: Traits cannot be imported/exported at this moment.
	                     %       Note that this requires changes in the runtime,
	                     %       since the order of module definition can change.
            tr_ctor(Ctor, Extends, UseTraits),
	    tr_defs(Statics),
	    tr_defs(SAttrDecls),
	    ( Ref = '\6\root' ->
	        tr_defs(Nested), % TODO: fix, use a single scheme
	        tr_defs(Methods)
	    ; tr_methods(Methods, UseTraits),
	      tr_defs(Nested)
	    )
          )),
	  call((
            stats :: accum(BootStats),
            tr_defs(BootCode)
          )),
	  MSyms0 = ~symdic.to_list,
%	  display(user_error, msyms0(MSyms0)), nl(user_error),
	  call((
            extern_dep_dic :: cdic <- ~cdic.new,
	    call((
              decl_stats :: accum(SymDeclStats),
              ext_stats :: accum(SymExtStats),
              loc_stats :: accum(SymLocStats),
	      syms_cache(MSyms0, MSyms)
	    )),
	    call((
              stats :: accum(PrepStats),
	      tr_prepare_deps(~extern_dep_dic.to_list)
            ))
	  )),
	  call((
            stats :: accum(MDefs2),
            ( Ref = '\6\root' ->
	        tr_stats(~append(PrepStats, SymExtStats))
	    ; tr_symlink(~append(PrepStats, SymExtStats))
	    )
          ))
	)),
	StatsC = ~append(SymDeclStats, ~append(MDefs, ~append(SymLocStats, MDefs2))),
	( Ref = '\6\root' ->
	    % Emit generated code (note that BootCode is at the bottom)
	    ( Kind = init ->
	        StatsC2 = [call(paren(function(['$m'], StatsC)), ['$r'])]
	    ; StatsC2 = StatsC
	    ),
	    tr_stats(StatsC2),
	    tr_stats(BootStats) % TODO: better way?
	; RelId = ~msym_local_id(Ref),
	  ( Kind = trait(Params) ->
	      stats.add(fundecl(id_trait(RelId), ['$m'|Params], StatsC))
	  ; EnclosingM = ~enclosing_js_id,
	    stats.add(call(mb(EnclosingM, 'def'), [
              id_s(RelId),
	      function(['$m'], StatsC)
            ]))
          )
        ).

tr_stats([]).
tr_stats([X|Xs]) :- stats.add(X), tr_stats(Xs).
}.

{
:- fluid stats :: accum. % Statements (output)
:- fluid enclosing :: any.
:- fluid symdic :: cdic. % Symbol dictionary

% tr_ctor(+Ctor, +Extends, -UseTraits):
% Emit code to register the constructor and base (class)
tr_ctor(Ctor, Extends, UseTraits) :-
	Extends = [functor_trait(BCN, N, A)],
	!,
	UseTraits = yes,
	tr_defs(Ctor),
	jg :: jsgen_st <- ~jsgen_st.new,
	% TODO: remember that I have to create this trait
	stats.add(call(id_trait(id_functor('ta\6\functor',A)), ['$m', ~jg.tr_tree(BCN), N])),
	( Ctor = [] ->
	    % TODO: Add definition so that it can be emitted only if necessary
	    % pick constructor definition from '$m' (i.e., the trait)
	    CtorRef = ~module_s.query_ref_BK(~enclosing, 'ctor', A),
	    % TODO: this is the reverse of tr_msym_reg
	    msym_mark_def(CtorRef),
	    R = ~tr_mod_entry('$m', CtorRef),
	    msym_mark_used(CtorRef),
	    Val = ~msym_js_id(CtorRef),
	    stats.add(vardecl(Val, R))
	; true
	).
tr_ctor(Ctor, Extends, UseTraits) :-
	UseTraits = no,
	tr_defs(Ctor),
	( \+ Ctor = [],
	  Extends = [] ->
	    stats.add(mb('$m', 'base') <- 'null')
	; tr_defs(Extends)
	).

% Emit the 'mlink' section of the module definition (method
% definition)
tr_methods(Methods, UseTraits) :-
	call(( stats :: accum(Methods2), tr_defs(Methods) )),
	( Methods2 = [] ->
	    true
	; ( UseTraits = yes ->
	      % Invoke the previous mlink is we are using traits
	      % TODO: make it native in ciao_runtime.js
	      %       (probably traits should have a pointer to the previous definition or 
              %       mlink should be a list)
	      stats.add(vardecl('$pmlink', mb('$m', 'mlink'))),
	      Methods3 = [call('$pmlink', ['$c'])|Methods2]
	  ; Methods3 = Methods2
	  ),
	  stats.add(mb('$m', 'mlink') <- function(['$c'], Methods3))
	).

% Emit the 'link' section of the module definition (linkage)
tr_symlink(Stats) :-
	( Stats = [] ->
	    true
	; stats.add(mb('$m', 'link') <- function([], Stats))
	).

tr_nested_def(Ref, Kind, Defs) :- % nested definition (module, class, or trait)
	tr_module_def(Ref, Kind, Defs, MSyms),
	syms_merge(MSyms).

% Translate each item in the module definition
tr_defs(Xs) :-
	tr_defs_(Xs).

tr_defs_([]).
tr_defs_([X|Xs]) :-
        ( tr_def(X) ->
	    true
	; throw(bug_failed_tr_def(X))
	),
        tr_defs_(Xs).

tr_def(nested_def(Ref, Kind, Defs)) :- !, % definition of a module/class/trait/etc.
	tr_nested_def(Ref, Kind, Defs).
tr_def(extends(BaseRef)) :- !,
	( BaseRef = 'var_base' -> % TODO: special case! do it in other way
	    Val = ~tr_query_mod_('$r', 'var_base')
	; Val = ~tr_query_mod(BaseRef)
	),
	stats.add(mb('$m', 'base') <- Val).
tr_def(prop(PropId, Value)) :- !,
	jg :: jsgen_st <- ~jsgen_st.new,
	jg.tr_attr(PropId, Value).
tr_def(bcode(Ref, Args, Code)) :- !, % code for some ref_B*
	jsgen_st.tr_bcode(Ref, Args, Code).
tr_def(declvar(VName)) :- % (global) variable declaration
	jg :: jsgen_st <- ~jsgen_st.new,
        stats.add(vardecl(~jg.tr_tree(VName))).
tr_def(module_init(ModuleR)) :- % (global) variable declaration
	% TODO: this should be part of the 'link' method of modules
	tr_prepare_dep(ModuleR).
%
tr_def(js_raw_native(X)) :- !, % raw native
	jg :: jsgen_st <- ~jsgen_st.new,
	stats.add(~jg.tr_tree(X)).
%
tr_def(X) :-
	throw(bug_unknown_tr_def(X, ~enclosing)).
}.

% ---------------------------------------------------------------------------
% Symbol linkage caches the values of symbols before program execution
% takes place, by copying them to other variables accessible from the
% current scope in the target (JS) language.
%
% It is related to partial evaluation of symbol lookup.
% 
% Depending on the relation of the symbol scope w.r.t. the current
% scope:
%
%  - enclosed module: no symbol cache is necessary
%  - nested module: like import (all symbols?) <=== This seems incorrect
%  - imported module: cached

% TODO: Avoid duplication of variables for cached symbols? 
%       (when they are accessed from two sibling scopes)

{
:- fluid enclosing :: any.
:- fluid symdic :: cdic. % Symbol dictionary
:- fluid extern_dep_dic :: cdic. % Dictionary of external deps
%
:- fluid decl_stats :: accum. % Sym decls
:- fluid ext_stats :: accum. % Sym defs (done in 'link' closure)
:- fluid loc_stats :: accum. % Local sym defs (done at definition time)

% Gives JS representation for all used symbols in this scope;
% if necessary, uses cached variables to import their values
% from other scopes.
%
% The preparation of symbols consists on:
%  - instantiating LocalId if necessary 
%  - fill symbol binding code if necessary
%  - obtain a new list of symbols for the outer scope
syms_cache(Xs, Ys) :-
	delayed_sym :: accum(Ys),
	syms_cache_(Xs).

{
:- fluid delayed_sym :: accum.

syms_cache_([]).
syms_cache_([X|Xs]) :-
	syms_cache__(X),
	syms_cache_(Xs).

syms_cache__(X) :-
	X = _-msymlive(_, Used, _),
	( nonvar(Used) ->
	    sym_cache__used(X)
	; % Defined but not used, keep symbol unbound
	  delayed_sym.add(X)
	).

sym_cache__used(X) :-
	X = Ref-msymlive(Defined, _, LocalId),
	( var(Defined) -> % used but not defined
	    ( top_or_root_enclosing -> % in a top module
	        ( root_enclosing ->
		    true % TODO: not working yet?
		; msym_external(Ref) ->
		    true % External, define here now
		; % Internal but not defined. If code reaches this
		  % point, then there is a bug in the translation: the
		  % Id is local to the scope, but it is not defined.
%		  throw(bug__sym_used_but_not_defined(~enclosing, X, ~symdic))
		  throw(bug__sym_used_but_not_defined(~enclosing, X))
		),
		Defined = yes, % Mark as defined
		% Annotate external dependecy to the enclosing msym (we are in a top module)
		annotate_dep(~msym_enclosing(Ref)),
	        % TODO: We are in a top module; emit the external
	        % symbol (to do it, we must do a fake definition first
	        % -- this is where we will need interfaces, to know
	        % the kind of symbol too: symkind_ctor, etc.)
		define_local_id(ext, Ref, LocalId),
		% Keep the symbol for outer scopes, with clean Used and LocalId values
		delayed_sym.add(Ref-msymlive(yes, _, _))
	    ; % Delay definition for the outer scope
	      delayed_sym.add(X)
	    )
	; % used and defined
	  define_local_id(loc, Ref, LocalId),
	  % Keep the symbol for outer scopes, with clean Used and LocalId values
	  delayed_sym.add(Ref-msymlive(yes, _, _))
	).

define_local_id(Mode, Ref, LocalId) :-
	% Fill the relative LocalId 
	msym_relative_id(Ref, ~enclosing, _, LocalId),
	% Declare and initialize (if necessary) a cached var
	declare_cached_var(Ref, Mode, LocalId).

declare_cached_var(Ref, Mode, LocalId) :-
	Enclosing = ~msym_enclosing(Ref),
	( ~enclosing = Enclosing ->
	    true % same scope, cached var is not needed
	; Register = ~msym_register(Ref),
	  sym_init(Register, Ref, Mode, Enclosing, LocalId)
	).
}.

% Initialize a symbol (JS)
sym_init(no, _, _, _, _) :- !. % , display(user_error, wrong), nl(user_error). % not registered, do not fetch
sym_init(_, Ref, Mode, Enclosing, LocalId) :-
	decl_stats.add(vardecl(LocalId)),
	% Initialize LocalId with the entry in the module structure
	Val = ~tr_mod_entry(~tr_query_mod(Enclosing), Ref),
	( Mode = loc ->
	    loc_stats.add(LocalId <- Val)
	; Mode = ext ->
	    ext_stats.add(LocalId <- Val)
	; fail
	).
}.

{
:- fluid enclosing :: any.
top_or_root_enclosing :-
	( root_enclosing -> true
	; ~enclosing = R,
	  trust(R instance_of module_s),
	  R.is_top_module
	).

root_enclosing :- ~enclosing = '\6\root'.

enclosing_js_id := R :-
	( root_enclosing -> R = '$r'
	; R = '$m'
	).
}.

% ---------------------------------------------------------------------------

{
% Annotate a dependency for module initialization
:- fluid extern_dep_dic :: cdic. % Dictionary of external deps
annotate_dep(Enclosing) :-
	( _ = ~extern_dep_dic.get(Enclosing) ->
	    true
	; extern_dep_dic.insert(Enclosing, yes)
	).
}.

{
% Emit code to invoke 'prepare' on the selected modules
:- fluid enclosing :: any.
:- fluid stats :: accum.
tr_prepare_deps([]).
tr_prepare_deps([R-_|Rs]) :-
	tr_prepare_dep(R),
	tr_prepare_deps(Rs).

tr_prepare_dep(R) :- stats.add(call(mb(~tr_query_mod(R), 'prepare'), [])).
}.

% ---------------------------------------------------------------------------

{
:- fluid enclosing :: any.
% :- pred tr_query_mod(+Ref,-R) :: psymbol * js_expr
%    # "Translation of a module query (output is relative to the enclosing module)".
% TODO: a CSE-like optimization to share roots?
%       (may generate more optimal code here, but also more temporals)
tr_query_mod(Ref) := '$r' :- Ref = ~module_s.lookup_module(root), !.
tr_query_mod(Ref) := R :- ~enclosing = Ref, !, % enclosing module found
	R = '$m'.
tr_query_mod(Ref) := R :-
	Enclosing = ~msym_enclosing(Ref),
	Rel = ~msym_local_id(Ref),
	R = ~tr_query_mod_(Parent, Rel),
	Parent = ~tr_query_mod(Enclosing).
}.

% Code that queries the module Id nested in M
tr_query_mod_(M, Id) := R :-
	R = call(mb(M, 'query'), [id_s(Id)]).

% ---------------------------------------------------------------------------

{
:- fluid enclosing :: any.
:- fluid symdic :: cdic.
:- fluid stats :: accum. % Statements (output)
% Emit code to register the symbol in the module structure.
%
% It fills constructor or export entries for Id, bound to Val, in the
% current module (this is runtime information, but also required for
% module initialization).
tr_msym_reg(Ref) :-
	Register = ~msym_register(Ref),
	( Register = yes ->
	    msym_mark_used(Ref),
	    Val = ~msym_js_id(Ref),
	    % Initialize the entry in the module structure with Val
	    R = ~tr_mod_entry('$m', Ref),
	    stats.add(R <- Val)
        ; true
	).
}.

% ---------------------------------------------------------------------------

{
:- fluid enclosing :: any.
:- fluid symdic :: cdic.
% :- pred tr_mod_entry(M, Ref, R) :: js_term * psymbol * js_term
%    # "@var{R} is the expression in @var{M} where @var{Ref} is stored".
tr_mod_entry(M, Ref) := R :-
        ( is_ref_BKS_ctor(Ref) ->
	    % The constructor is recorded in the 'ctor' property of M
	    R = mb(M, 'ctor')
	; Name = ~msym_modentry_id(Ref),
	  R = mb(mb(M, 'exports'), Name)
	).
}.

% ---------------------------------------------------------------------------
	
:- class jsgen_st {
    % Translate to WAM-like instructions to JS-tree statements

    % local temporal variables
    :- attr temps :: m_int.
    :- attr decl_f :: m_any. % 'f$' declared?

    :- attr rarg :: m_any. % relative arg mem
    :- attr detmode :: m_any. % determinism mode

    % Depending on detmode, blocks are compiles to JS functions whose
    % 'protocol' for return value follows the specification in
    % 'module_jsexp_' (see description of det modes in that module for
    % more details).
    %
    % Continuations for 'nondet' are defined as some JS function
    % closure "function(w) {...}".

    :- constructor new_/0.
    new_ :-
        ~rarg = from_worker,
	~temps = 0,
	~decl_f = no.

    % -----------------------------------------------------------------------

    {
    :- fluid stats :: accum.
    :- fluid symdic :: cdic.
    % Translate instructions into JS code
    tr_insns_([]).
    tr_insns_([B|Bs]) :-
        tr_ins(B),
        tr_insns_(Bs).

    % TODO: distinguish between WAM instructions and lower level instructions?
    % Some meta-instructions
    tr_ins(rargs(Where)) :- !,
        rarg <- Where. % set the relative position of arguments mem
    % Direct JS statements
    tr_ins(js_stats(Code)) :- !,
        stats.add(~tr_tree(Code)).
    % Allocation of frame
    tr_ins(alloc(Frame)) :- !,
        trust(Frame instance_of frame_str),
        call(( wcode :: accum(Ins), Frame.tr_alloc )),
	tr_insns_(Ins),
	tr_ins(load_frame_reg).
    % Deallocation of frame
    tr_ins(dealloc) :- !,
        call(( wcode :: accum(Ins), frame_str.tr_dealloc )),
	tr_insns_(Ins).
    % Load frame register
    tr_ins(load_frame_reg) :- !,
	CachedF = ~my_var_new(_, rawmem('f$')),
	WFrame = ~my_var_new(_, wreg('frame')),
        tr_assign(WFrame, CachedF).
    % Test class
    tr_ins(test_class(ClassId, X)) :- !,
	tr_test_class(ClassId, X).
    % JS test
    tr_ins(low_test(Exp)) :- !,
	tr_test(Exp).
    % Initialization of heap variables
    tr_ins(inith(X)) :- !,
        tr_assign(new(ctor_lookup(class, ':'(term_typing, var)), []), X).
    tr_ins(init(X,Y)) :- !,
        tr_assign(new(ctor_lookup(class, ':'(term_typing, var)), []), X),
        tr_assign(X, Y).
    % null initialization (for constructors in JS, it helps hidden class optimizations)
    tr_ins(init_null(X)) :- !,
        tr_assign('null', X).
    % Unification
    tr_ins(ld_cons(A, Cons)) :- !,
        tr_assign(cons(Cons), A).
    tr_ins(u_cons(A, Cons)) :- !,
        tr_unify(A, cons(Cons)).
    tr_ins(un_cons(A, Cons)) :- !,
        % TODO: on write: ld_cons
        tr_unify(A, cons(Cons)).
    tr_ins(unify(A, B)) :- !,
        tr_unify(A, B).
    tr_ins(u_val(A, B)) :- !,
        tr_unify(A, B).
    tr_ins(un_var(A, B)) :- !, % TODO: use if-then-else and mode
        % TODO: on write: init(A, B)
        tr_assign(A, B). % on read
    tr_ins(un_val(A, B)) :- !, % TODO: use if-then-else and mode
        % TODO: on write: movexp
        tr_unify(A, B).
    tr_ins(neck(_)) :- !. % (not implemented)
    % Cut
    tr_ins(cut(A)) :- !,
        tr_wcall(~module_s.query_ref_BU('worker', 'cut', 1), [A]).
    % Put/get a structure (which may be normal, unknown, or special like 'apply')
    % (for postorder traversal)
    tr_ins(ld_str_is(T, FunctorR, As)) :- !,
        ( FunctorR instance_of predicate_s ->
	    tr_assign(build_structure(FunctorR, As), T)
	; % (unk and apply)
	  % TODO: write more optimal code (creates free variable)
	  tr_ins(inith(T)),
	  tr_ins(u_str_is(T, FunctorR, As))
	).
    tr_ins(u_str_is(T, FunctorR0, As)) :- !,
        ( FunctorR0 instance_of predicate_s ->
	    % TODO: write more optimal code (missing read/write mode)
	    tr_unify(build_structure(FunctorR0, As), T)
        ; % (unk and apply)
	  Tr = ~psymbol_to_tr(FunctorR0),
	  call(( wcode :: accum(Ins), Tr.tr_u_str_is(As, T) )),
	  tr_insns_(Ins)
	).
    tr_ins(un_str_is(T, FunctorR0, As)) :- !,
        % TODO: on write: ld_str_is
        ( FunctorR0 instance_of predicate_s ->
	    % TODO: write more optimal code (missing read/write mode)
	    tr_unify(build_structure(FunctorR0, As), T)
        ; % (unk and apply)
	  Tr = ~psymbol_to_tr(FunctorR0),
	  call(( wcode :: accum(Ins), Tr.tr_u_str_is(As, T) )),
	  tr_insns_(Ins)
	).
    % (for preorder traversal)
    tr_ins(ld_str_os(T, FunctorR, As, SReg)) :- !, % TODO: only need arity
        ( FunctorR instance_of predicate_s ->
	    tr_assign(build_structure(FunctorR, ~c_nulls(As)), T),
	    ( As = [] -> true ; tr_ins(move(T, SReg)) )
	; bug(preorder_not_implemented(FunctorR))
	).
    tr_ins(u_str_os(T, FunctorR0, As, SReg)) :- !, % TODO: only need arity
        ( FunctorR0 instance_of predicate_s ->
	    tr_ins(deref(T, SReg)),
	    tr_sw_on_var_str(SReg, FunctorR0, As)
	; bug(preorder_not_implemented(FunctorR0))
	).
    tr_ins(un_str_os(T, FunctorR0, As, SReg)) :- !, % TODO: only need arity
        % TODO: on write: ld_str_os
        ( FunctorR0 instance_of predicate_s ->
	    tr_ins(deref(T, SReg)),
	    tr_sw_on_var_str(SReg, FunctorR0, As)
	; bug(preorder_not_implemented(FunctorR0))
	).
    % Set success and failure continuations
    tr_ins(set_success_cont(BB)) :- !,
	trust(BB instance_of bblock),
        R2 = ~BB.ref,
	msym_mark_used(R2),
    	tr_set_success_cont(~msym_js_id(R2)).
    tr_ins(push_failure_cont(BB)) :- !,
	trust(BB instance_of bblock),
        R2 = ~BB.ref,
	msym_mark_used(R2),
    	tr_push_failure_cont(~msym_js_id(R2)).
    % Builtin calls
    tr_ins(blt1(A, PredId)) :- !, tr_blt(PredId, [A]).
    tr_ins(blt2(A, B, PredId)) :- !, tr_blt(PredId, [A, B]).
    tr_ins(blt3(A, B, C, PredId)) :- !, tr_blt(PredId, [A, B, C]).
    tr_ins(fun1(A, B, PredId, _, _)) :- !, tr_blt(PredId, [B, A]).
    tr_ins(fun2(A, B, C, PredId, _, _)) :- !, tr_blt(PredId, [B, C, A]).
    tr_ins(funre1(A, B, PredId, _, _)) :- !, tr_blt(PredId, [B, A]).
    tr_ins(funre2(A, B, C, PredId, _, _)) :- !, tr_blt(PredId, [B, C, A]).
    % Predicate call
    tr_ins(kall(Ref, _Size, As)) :- !,
        % Pre: Ref instance_of predicate_s, ~Ref.get_det_mode \= nondet
	trust(Ref instance_of psymbol),
	Tr = ~psymbol_to_tr(Ref),
	% TODO: define tr_exec_fun for special predicates
	call(( wcode :: accum(Ins), Tr.tr_exec(As) )),
	tr_insns_(Ins).
    %
    tr_ins(lastcall(Ref, [])) :- Ref instance_of predicate_s, ~Ref.id = '$nodef/0', !, % TODO: not truly semidet
	tr_ins(wcall(~module_s.query_ref_BU('worker', 'nodef', 0), [])).
    tr_ins(lastcall(Ref, [])) :- Ref instance_of predicate_s, ~Ref.id = '$suspend/0', !,
	tr_ins(wcall(~module_s.query_ref_BU('worker', 'suspend', 0), [])).
    tr_ins(lastcall(Ref, [])) :- Ref instance_of predicate_s, ~Ref.id = 'basiccontrol:fail/0', !,
    	tr_ins(failins).
    tr_ins(lastcall(Ref, As)) :- !,
        % Pre: Ref instance_of predicate_s, ~Ref.get_det_mode = nondet
	trust(Ref instance_of psymbol),
	Tr = ~psymbol_to_tr(Ref),
	% TODO: define tr_exec_fun for special predicates
	call(( wcode :: accum(Ins), Tr.tr_exec(As) )),
	tr_insns_(Ins).
    % Basal code call of an operation on the current worker
    tr_ins(wcall(FunctorR0, As)) :- !,
        tr_wcall(FunctorR0, As).
    % Basal code call
    tr_ins(bcall(FunctorR0, As)) :- !,
        tr_bcall(FunctorR0, As).
    % Basal code call (as semidet)
    tr_ins(bcall_as_semidet(FunctorR0, As)) :- !, % TODO: only for basal
        tr_bcall_as_semidet(FunctorR0, As).
    % Call a term (as semidet) -- for bcall_as_semidet
    tr_ins(semidet_solve(A)) :- !, % TODO: wrong!
        tr_wcall(~module_s.query_ref_BU('worker', 'sd_solve', 1), [A]).
    % Set call head
    % TODO: split in put_args
    tr_ins(set_call_head(T)) :- !,
        tr_set_call_head(T).
    % Lastcall (exec)
    % TODO: missing predicate (currently in 'callhead')
    tr_ins(lastcall) :- !,
        % note: this indirection ensures that
        % TODO: this function should be static
        % (similar to the ENTER instruction of the optim_comp engine)
	stats.add(return(mb('w', 'pred_enter'))).
    % Return from a predicate
    tr_ins(proceed_out1(Val)) :- !, stats.add(return(~tr_tree(Val))).
    tr_ins(proceed_new_fresh(SReg, FunctorR)) :- !, tr_new_fresh(SReg, FunctorR). % TODO: hack...
    tr_ins(proceed) :- !, tr_proceed.
    tr_ins(failins) :- !, tr_fail.
    % Deref
    tr_ins(deref(Source, Target)) :- !,
        tr_assign(deref(Source), Target).
    % Copy two variables
    % TODO: limit so that Source is a variable?
    tr_ins(move(Source, Target)) :- !,
        tr_assign(Source, Target).

    tr_blt(Ref, As) :- Ref instance_of predicate_s, ~Ref.id = '$trail_set_attr/3', !,
	ExecR = ~module_s.query_ref_BU('attrvar', 'trail_set_attr', 2),
	tr_ins(bcall(ExecR, As)).
    tr_blt(Ref, [A]) :- Ref instance_of predicate_s, ~Ref.id = 'basiccontrol:$get_choice/1', !, % TODO: make it an instruction?
        Choice = ~my_var_new(_, wreg('choice')),
    	tr_ins(u_val(A, Choice)).
    tr_blt(Ref, [X]) :- Ref instance_of predicate_s, Ref.get_prop(specialdef([_], classtest(ModuleR))), !,
        tr_ins(test_class(msym(~query_ref_BKS_ctor(ModuleR)), deref(X))). % TODO: rewrite as a special bcall?
    tr_blt(Ref, As) :-
        Ref instance_of predicate_s, Ref.get_prop(basal_builtin(M, N, A)), !,
	( ExecR = ~module_s.query_ref_BU(M, N, A) ->
	    true
	; bug(failed_query_ref_BU(M, N, A))
	),
	trust(ExecR instance_of psymbol),
	% TODO: share with call of 'natcode'
	tr_ins(bcall(ExecR, As)).
    tr_blt(Ref, _As) :-
        throw(not_blt(Ref)).
    }.

    {
    :- fluid stats :: accum.
    :- fluid symdic :: cdic.
    % TODO: environment trimming? (at least, nullify unused frame entries)
    tr_set_success_cont(BBId) :-
        Cont = ~my_var_new(_, wreg('cont')),
        tr_assign(BBId, Cont).
    % TODO: define patch failure cont!! -- add option to do inlining or not of those definitions
    tr_push_failure_cont(BBId) :-
	tr_wcall(~module_s.query_ref_BU('worker', 'push_choice', 1), [BBId]).
    }.

    {
    :- fluid stats :: accum.
    :- fluid symdic :: cdic.
    % TODO: only put arguments, use 'head' only for control switch (like in WAM)
    tr_set_call_head(BBId) :-
	G = ~my_var_new(_,wreg('callhead')),
        tr_assign(BBId, G).
    }.

    {
    :- fluid stats :: accum.
    :- fluid symdic :: cdic.
    % TODO: use the instanceof JS builtin to make sure that it works with inherited classes
    % TODO: we would need a different thing for traits and interfaces
    tr_test_class(ClassId, X) :-
        tr_test(instanceof(X, ClassId)).
    tr_unify(A, B) :-
	tr_bcall(~module_s.query_ref_BU('term', 'sd_unify', 1), [A, B]).
    }.

    {
    :- fluid stats :: accum.
    :- fluid symdic :: cdic.
    tr_new_fresh(SReg, FunctorR0) :-
        trust(FunctorR0 instance_of predicate_s),
	A = ~FunctorR0.get_real_arity,
	length(As, A), !, % TODO: improve
        stats.add(if(~tr_tree(instanceof(SReg, 'var_base')), 
                     VarC, 
                     NonvarC)),
	RMode = ~my_var_new(_,wreg('rmode')),
	call((
          stats :: accum(VarC),
	  T = ~my_var_new(_,x(_)), % (was tmpvar)
	  % TODO: if SReg is an attributed variable, we cannot delay argument initialization
	  tr_ins(move(build_structure(FunctorR0, ~c_vars(As)), T)),
	  tr_unify(SReg, T),
	  ( A = 0 -> true ; tr_ins(move('false', RMode)) ), % switch to write mode
          tr_ins(proceed_out1(T))
        )),
        NonvarC = if(~tr_tree(instanceof(SReg, msym(~query_ref_BKS_ctor(FunctorR0)))), Ok, Fail),
	call((
          stats :: accum(Ok),
	  ( A = 0 -> true ; tr_ins(move('true', RMode)) ), % switch to read mode
          tr_ins(proceed_out1(SReg))
        )),
	call(( stats :: accum(Fail), tr_ins(failins) )).
    
    tr_sw_on_var_str(SReg, FunctorR0, As) :- % TODO: Rename 'tr_sw_on_var_str' and 'new_fresh'
        % TODO: include attributed variables here or not?
        % TODO: Define .instance_as(ctor) method (see below)
        %       This 'semidetfun' method returns a pointer (or 'null' if failed).
        %       It stores in the worker the mode.
        %       It can call attributed variable hook here if necessary (every functor must implement instance_as).
        %       Pros: the original term can be anything.
        %       The returned pointer is an object with .arg0, .arg1, etc. arguments.
        %       Then we could have something like instance_as_array, etc. that does other things and
        %       represent terms with a different interface.
        tr_bcall(~module_s.query_ref_BK(FunctorR0, 'new_fresh', 2), [SReg, SReg]).

    tr_test(Expr) :-
    	stats.add(simple_if(not(~tr_tree(Expr)), S)),
	call(( stats :: accum(S), tr_fail )).
    tr_proceed :-
        DetMode = ~detmode,
        ( DetMode = nondet ->
	    Cont = ~my_var_new(_, wreg('cont')),
	    stats.add(return(~tr_tree(Cont)))
	; DetMode = semidet ->
	    stats.add(return('true'))
	; throw(unknown_detmode(proceed, DetMode))
	).
    tr_fail :-
        DetMode = ~detmode,
	( DetMode = nondet ->
	    Choice = ~my_var_new(_, wreg('choice')),
	    tr_bcall(~module_s.query_ref_BU('choice', 'fail', 0), [Choice])
	; DetMode = semidet ->
	    stats.add(return('false'))
	; DetMode = semidetfun ->
	    stats.add(return('null')) % (like detfun, but can fail)
        ; throw(unknown_detmode(fail, DetMode))
	).

    % (basal call)
    tr_bcall(Ref, As) :-
        % Precondition: \+ Ref instance_of predicate_s 
	trust(Ref instance_of psymbol),
	Tr = ~psymbol_to_tr(Ref),
	% TODO: tr_exec_fun is not defined for predicate class, do it
	call((
          wcode :: accum(Ins),
	  ~Ref.get_det_mode = DetMode,
          ( DetMode = detfun ->
	      As = ~append(As0, [T]),
	      tr_assign(~Tr.tr_exec_fun(As0), T)
	  ; DetMode = semidetfun ->
	      As = ~append(As0, [T]),
	      tr_assign(~Tr.tr_exec_fun(As0), T),
	      stats.add(simple_if(op('===', [~tr_tree(T), 'null']), S)),
	      call(( stats :: accum(S), tr_fail ))
	  ; Tr.tr_exec(As)
	  )
        )),
	tr_insns_(Ins).
    tr_bcall_as_semidet(Ref, As) :-
	trust(Ref instance_of psymbol),
	Tr = ~psymbol_to_tr(Ref),
	call((
          wcode :: accum(Ins),
	  Tr.tr_exec_as_semidet(As)
        )),
	tr_insns_(Ins).

    % (basal call on the default worker)
    tr_wcall(Ref, As) :-
        tr_bcall(Ref, ['w'|As]).
    }.

    % ---------------------------------------------------------------------------

    {
    :- fluid enclosing :: any.
    :- fluid stats :: accum.
    :- fluid symdic :: cdic.

    % Generate code for some ref_B*
    :- static tr_bcode/3.
    tr_bcode(Ref, Args, Code) :-
        jg :: jsgen_st <- ~jsgen_st.new,
	jg.tr1(Ref, Args, Code).

    tr1(Ref, Args, Code) :-
        trust(Ref instance_of psymbol),
	WithWorker = ~Ref.get_with_worker,
	DetMode = ~Ref.get_det_mode,
	detmode <- DetMode,
        call(( stats :: accum(Code1), tr_insns_(Code) )),
        tr2(Ref, WithWorker, Args, Code1).

    % Emits the body of the JS function
    tr2(Ref, _WithWorker, _Args, Code) :-
        % Special case for code in the root main scope
	Ref instance_of ref_BKS,
	~Ref.owner_module = '\6\root',
	~Ref.name = '__mainblock__',
	~Ref.arity = 0,
	!,
        stats.add(Code).
    tr2(Ref, WithWorker, Args, Code) :-
	Ref instance_of ref_BKM,
	!,
	Args2 = ~w_args(WithWorker, Args),
	Name = ~msym_modentry_id(Ref),
	def_method(Name, Args2, Code).
    tr2(Ref, WithWorker, Args, Code) :-
	Args2 = ~w_args(WithWorker, Args),
	msym_mark_def(Ref),
	stats.add(fundecl(~msym_js_id(Ref), ~tr_tree(Args2), Code)),
	tr_msym_reg(Ref).

    % TODO: move to a reduced library for modular JS generation?
    % Emit a JS method definition
    def_method(Name, Args2, Code) :-
	stats.add(~tr_proto_method(Name) <- function(~tr_tree(Args2), Code)).
    }.

    {
    :- fluid stats :: accum.
    :- fluid symdic :: cdic.
    tr_attr(AttrName, Value) :-
        stats.add(~tr_proto_method(AttrName) <- ~tr_tree(Value)).
    }.

    :- static tr_proto_method/2.
    tr_proto_method(Method) := mb(mb('$c', 'prototype'), Method).

    :- static w_args/3.
    w_args(WithWorker, As) := ( WithWorker = with_worker ? ['w'|As] | As ).

    % Give a real name to the temporal variable
    alloc_temp(X) :-
        Mem = ~my_var_mem(X),
	( Mem = x(N), var(N) -> % (was tmpvar)
	    N = ~temps, temps.inc(1)
	; Mem = rawmem('f$'), ~decl_f = no ->
	    decl_f <- yes
	; true
	).
    % The temporal variable has not been declared
    undeclared_temp(X) :-
        Mem = ~my_var_mem(X),
	nonvar(Mem),
        ( Mem = x(N), var(N) -> % (was tmpvar)
	    true
	; Mem = rawmem('f$'), ~decl_f = no ->
	    true
	; fail
	).

    {
    :- fluid stats :: accum.
    :- fluid symdic :: cdic.
    % Assign Value to T
    % NOTE: declaring with an initial value seems slightly faster than
    %       declaring and then assigning the value
    tr_assign(Value, T) :-
        ( undeclared_temp(T) ->
	    alloc_temp(T),
	    stats.add(vardecl(~tr_tree(T), ~tr_tree(Value)))
	; stats.add(~tr_tree(T) <- ~tr_tree(Value))
	).
    }.

    {
    % The JS variable that represents ''self'' (bself/1) for basal objects
    :- static js_bself/1.
    js_bself := 'this'.
    }.

    :- if(defined(strarg_array)).
    strmem(Base, I) := mb(Base, elem(I)).
    :- else.
    strmem(Base, I) := mb(Base, id_strarg(I)).
    :- endif.

    % Structure argument
    tr_mem(strmem(Str, I)) := ~strmem(~tr_mem(~my_var_mem(Str)), I). % Str instance_of termvar
    % Argument registers (relative to 'rarg')
    tr_mem(a(I)) := ~strmem(Str, I) :-
        Where = ~rarg,
	( Where = from_frame -> S = 'f$' ; S = 'w' ),
	Str = mb(S, 'callhead').
    % (JS) function argument
    tr_mem(cargmem(N)) := id_cargmem(N).
    % Frame variables
    tr_mem(y(N)) := mb('f$', id_y(N)).
    tr_mem(yreg(Field)) := mb('f$', Field).
    tr_mem(framevar0(N)) := mb(~funcall(bself), id_y(N)).
    % TODO: remove 'mthis'? ('this' indirection may have problems with analysis, or not)
    tr_mem(mthis(X)) := mb(~funcall(bself), X).
    % Special registers of the worker
    tr_mem(wreg(Field)) := mb('w', Field).
    % Temporal registers (note that they may not be global in all backends, see argument registers!)
    tr_mem(x(N)) := wreg('previous_choice') :- nonvar(N), N = -1, !. % TODO: not here!
    tr_mem(x(N)) := id_x(N). % (was tmpvar)
    % Raw names (e.g., for foreign variables)
    tr_mem(rawmem(N)) := N.
    % Variables for class attributes
    tr_mem(attmem(N)) := mb(~funcall(self), id_att(N)).
    % Variables for static class attributes
    % TODO: module attributes/globals?
    tr_mem(sattmem(N)) := id_att(N).

    :- static is_mem/1.
    is_mem(cargmem(_)). % native argument registers
    is_mem(a(_)). % argument registers (in the worker)
    is_mem(x(_)). % temporal registers (local or in the worker)
    is_mem(y(_)). % local registers (in the frame)
    %
    is_mem(strmem(_, _)).
    is_mem(framevar0(_)).
    is_mem(mthis(_)).
    is_mem(yreg(_)). % field of current (cached) frame
    is_mem(wreg(_)). % field of the current worker
    is_mem(rawmem(_)).
    is_mem(attmem(_)).
    is_mem(sattmem(_)).

    {
    :- fluid symdic :: cdic.
    % Apply tr_expr to the whole JS-tree
    tr_tree(X) := Y :- var(X), !, Y = X. % for 'holes' in translation
    tr_tree(X) := Y :- tr_expr(X, Y), !.
    tr_tree(X) := Y :-
	( var(X) ->
	    Y = X % TODO: tr_id may leave holes to be instantiated later
        ; functor(X, N, A),
	  functor(Y, N, A),
	  X =.. [_|As],
	  Y =.. [_|Bs],
	  Bs = ~tr_tree_xs(As)
	).

    tr_tree_xs([]) := [].
    tr_tree_xs([X|Xs]) := [~tr_tree(X) | ~tr_tree_xs(Xs)].
        
    % Extensions to the expression grammar
    % Note: this could be done in a separate pass, but is called from js_expr/1
    tr_expr(X) := R :- X = ~funcall(bself), !, % basal self
        % TODO: we should have just 'self' and distinguish depending on the 'basal' property
        R = ~tr_tree(~js_bself).
    tr_expr(X) := R :- X = ~funcall(self), !, % self
        R = ~tr_tree('self'). % TODO: customize this name, use var(_) or mem?
    tr_expr(X) := R :- X instance_of termvar, !, % variable with an associated mem
        Mem = ~my_var_mem(X),
	R = ~tr_tree(~tr_mem(Mem)).
    tr_expr(X) := R :- is_mem(X), !, % a 'mem' identifier
        R = ~tr_tree(~tr_mem(X)).
    % (building X.f(...) -- only called from attr_rt at this moment)
    tr_expr(X) := R :- X = ~funcall(ref_PUM__buildstr(ObjMem, N, Args)), !,
	( atom(ObjMem) -> true
	; bug(only_atom_in_objmem(ObjMem))
	),
	length(Args, A),
	Obj = ~my_var_new(_, rawmem(ObjMem)),
	Ref = ~ref_PUM.new(N,A),
        R = ~tr_tree(~tr_bcall_fun(~Ref.query_ref_BU_buildstr, ~tr_tree([Obj|Args]))).
    % --
    tr_expr(deref(A)) := R :- !,
        R = call(mb(~tr_tree(A), 'deref'), []).
    tr_expr(cons(A)) := R :-
        R = ~tr_expr(~c_cons(A)).
    tr_expr(box(ModuleR, A)) := R :- !,
	Ref = ~query_ref_BKS_ctor(ModuleR),
        msym_mark_used(Ref), % TODO: MISSING BOX OF MODULE
    	R = new(~msym_js_id(Ref), ~tr_tree([A])).
    tr_expr(unbox(_ModuleR, A)) := R :- !,
        R = call(mb(~tr_tree(deref(A)), 'unbox'), []).
    %
    tr_expr(call(X, with_worker, As)) := R :- !,
        R = call(~tr_tree(X), ~tr_tree(['w'|As])).
    tr_expr(call(X, no_worker, As)) := R :- !,
    	R = call(~tr_tree(X), ~tr_tree(As)).
    tr_expr(build_structure(FunctorR, As)) := R :- !,
	trust(FunctorR instance_of predicate_s), A = ~FunctorR.get_real_arity,
	Ref = ~module_s.query_ref_BK(FunctorR, 'ctor', A), % TODO: include in predicate_s definition?
        msym_mark_used(Ref),
    	R = new(~msym_js_id(Ref), ~tr_tree(As)).
    tr_expr(ctor_lookup(class, X)) := R :- !,
	R = ~tr_tree(msym(~query_ref_BKS_ctor(~module_s.lookup_module(X)))).
    tr_expr(ctor_lookup0(class, Ref)) := R :- !, % Ref is a module_s
	R = ~tr_tree(msym(~query_ref_BKS_ctor(Ref))).
    tr_expr(msym(Ref)) := R :- !,
        msym_mark_used(Ref),
        R = ~msym_js_id(Ref). % note R may be unbound during % TODO: correct?
    }.
}.

tr_bcall_fun(Ref, [M|As]) := R :-
	trust(Ref instance_of psymbol),
	Tr = ~psymbol_to_tr(Ref),
	R = ~Tr.tr_exec_fun([M|As]).

query_ref_BKS_ctor(Ref0, Ref) :-
	A = ~ref_ctor_arity(Ref0),
	Ref = ~module_s.query_ref_BK(Ref0, 'ctor', A).

% Arity of the 'ctor' for the given psymbol
ref_ctor_arity(Ref) := A :- Ref instance_of predicate_s, !,
	A = ~Ref.get_real_arity.
ref_ctor_arity(Ref) := A :- Ref instance_of frame_str, !,
	A = ~Ref.ctor_arity.
ref_ctor_arity(Ref) := A :-
 	trust(Ref instance_of module_s),
	( Ref.has_constructor(A0),
	  CRef = ~Ref.lookup_pred('cons__', A0, any),
	  CRef.get_prop(basal) -> % only if it is basal (Prolog-level constructors are implemented as predicates)
	    A = A0
	; Ref.get_prop(simple_box(_)) -> A = 1 % TODO: factorize definition
	; A = 0
	).

{
:- fluid wcode :: accum.
% Emit execution of a given term (already created)
emit_term_exec(T) :-
	wcode.add(set_call_head(T)),
	wcode.add(lastcall).
}.

% TODO: Use foreign JS library for bignums?
% TODO: Distinguish floats and ints?
% TODO: Add more types?
% TODO: Make it customizable?
c_cons(X0) := box(~module_s.lookup_type(t_string), native_string(X)) :- string_codes(X0, X), !.
c_cons(X) := box(~module_s.lookup_type(t_num), X) :- number(X), !.

c_vars([]) := [].
c_vars([_|Xs]) := [new(ctor_lookup(class, ':'(term_typing, var)), [])| ~c_vars(Xs)].

c_nulls([]) := [].
c_nulls([_|Xs]) := ['null'| ~c_nulls(Xs)].

% ===========================================================================
% Predicate (and/or) binarization.
% TODO: Join with the rest of compilation chain (optim_comp)

{
:- fluid middefs :: accum.

binarize([]).
binarize([X|Xs]) :-
	binarize_(X),
	binarize(Xs).

binarize_(nested_def(Ref, Kind, Defs)) :- !,
	pred_ref :: any <- Ref,
	middefs.add(nested_def(Ref, Kind, Defs2)),
	call(( middefs :: accum(Defs2), binarize__(Defs) )).
binarize_(X) :-
	middefs.add(X).

{
:- fluid pred_ref :: any.

binarize__([]).
binarize__([X|Xs]) :-
	binarize___(X),
	binarize__(Xs).

binarize___(bcode_a(MetR, Cs, _)) :- !,
	b :: bin_st <- ~bin_st.init(~pred_ref),
        b.process(MetR, Cs). % (adds to middefs)
binarize___(X) :-
	binarize_(X).
}.
}.

:- class bin_st {
    % Clause binarization for a single block.
	
    % basic blocks (in this case, sequences of det/semidet code)
    :- attr blocks :: array.
    :- attr frames :: cdic. % set of frames (one for each size)
    :- attr pred_ref :: any. % TODO: psymbol or predicate_s?

    :- constructor init_/1.
    init_(Ref) :-
	~blocks = ~array.new,
	~frames = ~cdic.new,
	~pred_ref = Ref.

    {
    :- fluid middefs :: accum.
    process(MetR, Cs3) :-
	Cs3 = [ma_clause(MetArgsP, _, _)|_],
	PreCode = ~binarize_cs(Cs3),
	BB = ~bblock.new(MetR, MetArgsP, PreCode),
	BB.emit,
	% Emit frame objects
	emit_frames,
	% Emit other blocks
	emit_blocks.
    }.

    {
    binarize_cs(Cs) := Body :-
        code :: accum(Body),
	binarize_or(Cs).
    {
    :- fluid code :: accum.

    binarize_or([ma_clause(_, X, _)]) :- !, binarize_and_(X).
    binarize_or([ma_clause(_, X, _)|Xs]) :- !,
    	Cont = ~blocks.insert(BB), % BB free here
    	code.add(push_failure_cont(BB)),
	binarize_and_(X),
	%
	binarize_or_cont(Xs, Cont, BB). % instantiates BB

    binarize_or_cont(Xs, Cont, BB) :-
	BR = ~query_bb(Cont),
	BB = ~bblock.new(BR, [], ContCode),
	call((
	  code :: accum(ContCode),
          binarize_or(Xs)
        )).

    % note: ;/2 is compiled without auxiliar predicates:
    %       - a single frame is shared
    % note: this code makes use of unification of logic variables to
    %       fill references in continuations
    binarize_and_([X]) :- !,
	code.add(X).
    binarize_and_([X|Xs]) :- ins_use_success_cont(X), !,
	% Using continuations, the rest of the clause is compiled as a new block
	Cont = ~blocks.insert(BB),
	code.add(set_success_cont(BB)),
        ( is_call(X) ->
	    code.add(~mark_lastcall(X))
	; code.add(X)
	),
	%
	BR = ~query_bb(Cont),
	BB = ~bblock.new(BR, [], ContCode),
	call((
          code :: accum(ContCode),
	  code.add(rargs(from_frame)),
	  code.add(load_frame_reg),
	  binarize_and_(Xs)
	)).
    binarize_and_([alloc(Frame0)|Xs]) :- Frame0 = frame0(SaveArgs, Size), !,
        Frame = ~create_frame_object(SaveArgs, Size),
        code.add(alloc(Frame)),
	binarize_and_(Xs).
    binarize_and_([X|Xs]) :- !,
        % X does not use success continuation, emit code sequentially
        code.add(X),
	binarize_and_(Xs).
    }.
    }.

    % Query a ref_BKS for a basic block (for continuation passing style compilation)
    query_bb(Cont) := ~ref_BKS.new(~pred_ref, 'bb'(Cont), 0, with_worker, nondet).

    create_frame_object(SaveArgs, Size) := Frame :-
	% Note: SaveArgs is instantiated later (~save_callhead is filled by addframe_cs)
	FrameKey = (SaveArgs, Size),
	( Frame0 = ~frames.get(FrameKey) ->
	    Frame = Frame0
	; Frame = ~frame_str.new(SaveArgs, Size, ~pred_ref),
	  frames.insert(FrameKey, Frame)
	).

    {
    :- fluid middefs :: accum.

    emit_blocks :-
    	emit_blocks__2(~blocks.keys).

    emit_blocks__2([]).
    emit_blocks__2([K|Ks]) :-
        BB = ~blocks.get(K),
	trust(BB instance_of bblock),
	BB.emit,
	emit_blocks__2(Ks).
    }.

    {
    :- fluid middefs :: accum.

    emit_frames :-
    	emit_frames__2(~frames.to_list).

    emit_frames__2([]).
    emit_frames__2([(_-Val)|Ks]) :-
        Frame = Val,
	trust(Frame instance_of frame_str),
	Frame.emit,
	emit_frames__2(Ks).
    }.
}.

% ---------------------------------------------------------------------------
% Split a list of middle-level definitions in several parts

%:- public split_defs/9.
split_defs(Defs, Extends, Ctor, Statics, SAttrDecls, Methods, Nested, Traits, BootCode) :-
	extends :: accum(Extends),
	ctor :: accum(Ctor),
	statics :: accum(Statics),
	sattr_decls :: accum(SAttrDecls),
	methods :: accum(Methods),
	nested :: accum(Nested),
	traits :: accum(Traits),
	bootcode :: accum(BootCode),
	split_defs_(Defs).

{
:- fluid extends :: accum.
:- fluid ctor :: accum.
:- fluid statics :: accum.
:- fluid sattr_decls :: accum.
:- fluid methods :: accum.
:- fluid nested :: accum.
:- fluid traits :: accum.
:- fluid bootcode :: accum.

split_defs_([]).
split_defs_([X|Xs]) :-
	split_defs__(X),
	split_defs_(Xs).

split_defs__(X) :-
	( X = nested_def(_,_,_) ->
	    traits.add(X)
	; X = extends(_) ->
	    extends.add(X)
	; X = extends_nonvar(BaseRef, BCN, N, A) ->
	    extends.add(extends(BaseRef)),
	    methods.add(prop(owner_module, BCN)),
	    methods.add(prop(name, N)),
	    methods.add(prop(arity, A))
	; X = functor_trait(_,_,_) -> % TODO: improve
	    extends.add(X)
	; X = bcode(Ref,_,_), Ref instance_of ref_BKS ->
	    ( ~Ref.owner_module = '\6\root',
	      ~Ref.name = '__mainblock__',
	      ~Ref.arity = 0 ->
	        bootcode.add(X)
	    ; is_ref_BKS_ctor(Ref) ->
	        ctor.add(X)
	    ; statics.add(X)
	    )
	; X = declvar(_) ->
	    sattr_decls.add(X)
	; methods.add(X)
	).
}.

is_ref_BKS_ctor(Ref) :- % TODO: add a property in ref_BK* instead?
	trust(Ref instance_of ref_BKS),
	~Ref.name = 'ctor'.

% ===========================================================================
% Translators of 'psymbol' (see module_jsexp_)
% ===========================================================================
% TODO: define 'bsymbol' for basal predicates?

% TODO: this should not be necessary (see complang_mini.pl)
:- '$trust_statemodel'(ref_PUM, single).
:- '$trust_statemodel'(ref_BKS, single).
:- '$trust_statemodel'(ref_BUM, single).
:- '$trust_statemodel'(ref_BKM, single).

:- include(compiler(psymbol_tr__interface)).

% Obtain the translator object from the psymbol
%:- meta_predicate psymbol_to_tr(psymbol, out(psymbol_tr)).
:- meta_predicate psymbol_to_tr(?, out(psymbol_tr)).
:- discontiguous psymbol_to_tr/2.

% ---------------------------------------------------------------------------
% (predicates)

% Translator for ref_PKS and ref_PKM (both 'predicate_s')
psymbol_to_tr(Ref, Tr) :- Ref instance_of predicate_s, !, Tr = ~tr_PK.new(Ref).
:- public class tr_PK {
    :- extends psymbol_tr.

    :- attr ref :: predicate_s.

    :- constructor new_/1.
    new_(Ref) :- ~ref = Ref.

    {
    :- fluid wcode :: accum.

    % Middle-level code for 'put term'
    :- public tr_u_str_is/2.
    tr_u_str_is(_As, _R) :- bug(tr_u_str_is_not_allowd_for_pred).

    % Middle-level code for execution
    :- public tr_exec/1.
    tr_exec(As) :- \+ ~ref.get_det_mode = nondet, !,
        semidet_exec(As).
    tr_exec(As) :-
        nondet_exec(As).

    tr_exec_as_semidet(_As) :-
        bug(notimplemented), fail.

    % Middle-level code for nondet execution
    nondet_exec(As) :-
	Ob = new(msym(~query_ref_BKS_ctor(~ref)), As),
	% TODO: not really, I know the predicate
	emit_term_exec(Ob).

    % Middle-level code for semidet execution
    semidet_exec(As) :-
	N = ~length(As),
	NatR = ~ref.query_ref_BK('natcode'(~ref.get_det), N),
        BoxingTypes = ~ref.get_boxingtypes,
	DetMode = ~NatR.get_det_mode,
        ( is_detfun(DetMode) ->
            % TODO: always box?
 	    combine_fun_args(As, BoxingTypes, Xs, Ret, RetBoxingType),
	    T = ~my_var_new(_,x(_)), % (was tmpvar)
	    wcode.add(bcall(NatR, ~append(Xs, [T]))),
	    Val = ( RetBoxingType = unbox(RetClass) ? box(~module_s.lookup_type(RetClass), T)
		  | T
		  ),
	    wcode.add(u_val(Ret, Val))
    	; combine_args(BoxingTypes, As, Xs),
	  wcode.add(bcall(NatR, Xs))
	).
    }.

    tr_exec_fun(_) := _ :- bug(not_valid(tr_PK, tr_exec_fun)).

    :- static combine_args/3.
    combine_args([], [], []).
    combine_args([unbox(Class)|Xs], [Y|Ys], [unbox(ModuleR, Y)|Zs]) :- !,
	ModuleR = ~module_s.lookup_type(Class),
	combine_args(Xs, Ys, Zs).
    combine_args([box|Xs], [Y|Ys], [Y|Zs]) :- !, combine_args(Xs, Ys, Zs).

    :- static combine_fun_args/5.
    combine_fun_args(As, BoxingTypes, Xs, Ret, RetBoxingType) :-
        split_last(As, As0, Ret),
        split_last(BoxingTypes, BoxingTypes0, RetBoxingType),
        combine_args(BoxingTypes0, As0, Xs).

    :- static split_last/3.
    % split_last(Xs, Ys, Y) iff (Xs = ~append(Ys, [Y]))
    split_last([X], [], X) :- !.
    split_last([X|Xs], [X|Ys], Last) :- split_last(Xs, Ys, Last).
}.

% Translator for ref_PUM
psymbol_to_tr(Ref, Tr) :- Ref instance_of ref_PUM, !,
	( ~Ref.name = 'ho_apply__' ->
	    % The special 'ho_apply__' method of terms
	    Tr = ~tr_PUM_apply.new(Ref)
        ; Tr = ~tr_PUM.new(Ref)
	).

:- public class tr_PUM {
    :- extends psymbol_tr.

    :- attr ref :: ref_PUM.

    :- constructor new_/1.
    new_(Ref) :-
	~ref = Ref.

    {
    :- fluid wcode :: accum.

    tr_exec(As0) :-
	prepare_self(As0, As),
	emit_term_exec(~tr_bcall_fun(~ref.query_ref_BU_buildstr, As)).

    tr_exec_as_semidet(As0) :- % (force execution in a semidet context, prunning solutions)
	prepare_self(As0, As),
	wcode.add(semidet_solve(~tr_bcall_fun(~ref.query_ref_BU_buildstr, As))). % TODO: wrong?

    tr_u_str_is(As0, R) :-
	prepare_self(As0, As),
        wcode.add(unify(~tr_bcall_fun(~ref.query_ref_BU_buildstr, As), R)). % TODO: this should be u_str_as

    prepare_self([Self|As], [T|As]) :-
        % TODO: we do a preregister assignment and a postregister assignment for x vars
	T = ~my_var_new(_,x(_)), % (was tmpvar)
	wcode.add(deref(Self, T)).
    }.

    tr_exec_fun(_) := _ :- bug(not_valid(tr_PUM, tr_exec_fun)).
}.

:- public class tr_PUM_apply {
    :- extends psymbol_tr.

    :- attr ref :: ref_PUM.

    :- constructor new_/1.
    new_(Ref) :- ~ref = Ref.

    {
    :- fluid wcode :: accum.

    tr_exec([Self]) :- !,
	emit_term_exec(Self).
    tr_exec([Self|As]) :-
        % TODO: check if last call is working here (i.e., efficient stack usage)
	ExecR = ~predicate_s.query_ref_BU('partial_mgc', ~length(As)),
	wcode.add(set_call_head(deref(Self))),
	G = ~my_var_new(_,wreg('callhead')),
	wcode.add(bcall(ExecR, [G|As])).

    tr_exec_as_semidet([Self]) :- !,
	T = ~my_var_new(_,x(_)), % (was tmpvar)
	wcode.add(deref(Self, T)),
	wcode.add(semidet_solve(T)).
    tr_exec_as_semidet([Self|As]) :-
        % TODO: check if last call is working here (i.e., efficient stack usage)
	ExecR = ~predicate_s.query_ref_BU('partial_mgc', ~length(As)),
	S = ~my_var_new(_,x(_)), % (was tmpvar)
	wcode.add(move(deref(Self), S)),
	wcode.add(bcall_as_semidet(ExecR, [S|As])).

    tr_u_str_is([Self|As], T) :-
        A = ~length(As), A1 is A + 1,
	ExecR = ~predicate_s.query_ref_BU('partial_mec', A1),
        % TODO: check if last call is working here (i.e., efficient stack usage)
	S = ~my_var_new(_,x(_)), % (was tmpvar)
	G = ~my_var_new(_,wreg('callhead')),
	% TODO: ugly implementation
	wcode.add(move(G, S)), % save callhead
	wcode.add(set_call_head(deref(Self))),
	wcode.add(bcall(ExecR, [G|(~append(As, [T]))])),
	wcode.add(move(S, G)). % restore callhead
    }.

    tr_exec_fun(_) := _ :- bug(not_valid(tr_PUM, tr_exec_fun)).
}.

% ===========================================================================
% (translators for basal predicates)
% TODO: define 'control protocol' to share det/semidet/detfun, etc. translations

ref_det(Ref) := Det :- trust(Ref instance_of psymbol), ~Ref.get_det_mode = Det.

% Translator for ref_BKS
psymbol_to_tr(Ref, Tr) :- Ref instance_of ref_BKS, !, Tr = ~tr_BKS.new(Ref).
:- public class tr_BKS {
    :- extends psymbol_tr.

    :- attr ref :: ref_BKS.

    :- constructor new_/1.
    new_(Ref) :- ~ref = Ref.

    {
    :- fluid wcode :: accum.

    tr_exec(As) :- ~ref_det(~ref) = det, !,
        wcode.add(js_stats(~tr_call(As))).
    tr_exec(As) :- ~ref_det(~ref) = semidet, !,
        wcode.add(low_test(~tr_call(As))).
    tr_exec(_) :- bug(not_valid(tr_BKS, tr_exec(~ref))).

    tr_exec_as_semidet(As) :- ~ref_det(~ref) = det, !, tr_exec(As).
    tr_exec_as_semidet(As) :- ~ref_det(~ref) = semidet, !, tr_exec(As).
    tr_exec_as_semidet(_) :- bug(not_valid(tr_BKS, tr_exec_as_semidet)).

    tr_u_str_is(_As, _R) :- bug(not_valid(tr_BKS, tr_u_str_is)). % TODO: define some special predicate ~pinfo:ppoint that can be used for error reporting
    }.

    tr_exec_fun(As) := ~tr_call(As) :-
        is_detfun(~ref_det(~ref)), !.
    tr_exec_fun(_) := _ :- bug(not_valid(tr_BKS, tr_exec_fun)).

    tr_call(As) := call(msym(~ref), ~ref.with_worker, As).
}.

% Translator for ref_BUM
psymbol_to_tr(Ref, Tr) :- Ref instance_of ref_BUM, !, Tr = ~tr_BUM.new(Ref).
:- public class tr_BUM {
    :- extends psymbol_tr.

    :- attr ref :: ref_BUM.

    :- constructor new_/1.
    new_(Ref) :- ~ref = Ref.

    {
    :- fluid wcode :: accum.

    tr_exec(As) :- ~ref_det(~ref) = det, !,
        wcode.add(js_stats(~tr_call(As))).
    tr_exec(As) :- ~ref_det(~ref) = semidet, !,
        wcode.add(low_test(~tr_call(As))).
    tr_exec(As) :- ~ref_det(~ref) = nondet, !,
        wcode.add(js_stats(return(~tr_call(As)))).
    tr_exec(_) :- bug(not_valid(tr_BUM, tr_exec)).

    tr_exec_as_semidet(As) :- ~ref_det(~ref) = det, !, tr_exec(As).
    tr_exec_as_semidet(As) :- ~ref_det(~ref) = semidet, !, tr_exec(As).
    tr_exec_as_semidet(As) :- ~ref_det(~ref) = nondet, !,
        wcode.add(semidet_solve(~tr_call(As))). % TODO: wrong?
    tr_exec_as_semidet(_) :- bug(not_valid(tr_BUM, tr_exec_as_semidet)).

    tr_u_str_is(_As, _R) :- bug(not_valid(tr_BUM, tr_u_str_is)).
    }.

    tr_exec_fun(As) := ~tr_call(As) :-
        is_detfun(~ref_det(~ref)), !.
    tr_exec_fun(_) := _ :- bug(not_valid(tr_BUM, tr_exec_fun)).

    tr_call([Self|As]) := call(mb(Self, Name), ~ref.with_worker, As) :-
        Name = ~msym_modentry_id(~ref).
}.

% Translator for ref_BKM
psymbol_to_tr(Ref, Tr) :- Ref instance_of ref_BKM, !, Tr = ~tr_BKM.new(Ref).
:- public class tr_BKM {
    % TODO: det_mode is not used yet
    % TODO: we can refine this with the 'final' (or virtual, overridable) information to 
    %       generate more efficient code in those cases
    :- extends psymbol_tr.

    :- attr ref :: ref_BKM.

    :- constructor new_/1.
    new_(Ref) :- ~ref = Ref.

    {
    :- fluid wcode :: accum.

    tr_exec(_As) :- bug(tr_exec_not_allowed).

    tr_exec_as_semidet(_As) :- bug(tr_exec_not_allowed).

    tr_u_str_is(_As, _R) :- bug(tr_u_str_is_not_allowed).
    }.

    tr_exec_fun(_) := _ :- bug(not_valid(tr_BKM, tr_exec_fun)).
}.

% ---------------------------------------------------------------------------

% (Last case)
psymbol_to_tr(Ref, _Tr) :- throw(bad_psymbol(Ref)).

% ---------------------------------------------------------------------------

:- class bblock {
    % A basic block

    % TODO: set type of 'ref' to 'psymbol'
    %       (but type of ~ref is not propagated; there is a bug in oc/ciao here)
    :- attr ref :: any. 
    :- attr metargsp :: any.
    :- attr code :: any.

    :- constructor new_/3.
    new_(Ref, MetArgsP, Code) :-
        ~ref = Ref,
	~metargsp = MetArgsP,
	~code = Code.

    {
    :- fluid middefs :: accum.
    :- constant emit/0.
    emit :-
	MetArgsP = ~metargsp,
	Code = ~code,
	middefs.add(bcode(~ref, MetArgsP, Code)).
    }.
}.

% ---------------------------------------------------------------------------

:- class frame_str {
    % Frame structure
    :- attr save_callhead :: any.
    :- attr size :: any.
    :- attr pred :: any. % associated predicate % TODO: predicate_s?

    % TODO: Write alternative implementations: simulate this class
    %       using a real stack (like in the WAM).

    :- constructor new_/3.
    new_(SaveArgs, Size, Pred) :-
        ~save_callhead = SaveArgs,
        ~size = Size,
        ~pred = Pred.

    {
    :- fluid wcode :: accum.

    % Emit allocation code for this frame
    tr_alloc :-
        Cont = ~my_var_new(_, wreg('cont')),
        Frame = ~my_var_new(_, wreg('frame')),
        Args = [Cont, Frame|Args0],
        ( ~save_callhead = yes ->
	    G = ~my_var_new(_,wreg('callhead')),
	    Args0 = [G]
	; Args0 = []
	),
	wcode.add(move(new(msym(~query_ref_BKS_ctor(~self)), Args), Frame)). % TODO: this should not be 'move'

    :- static tr_dealloc/0.
    % Emit deallocation code for this frame
    tr_dealloc :-
        Cont = ~my_var_new(_, wreg('cont')),
        Frame = ~my_var_new(_, wreg('frame')),
        FCont = ~my_var_new(_, yreg('cont')),
        FPrev = ~my_var_new(_, yreg('prev')),
        wcode.add(move(FCont, Cont)),
        wcode.add(move(FPrev, Frame)).
    }.

    % TODO: simplify, really necessary?
    ctor_arity := Arity :-
        Arity0 = 2, % 'cont', 'prev'
        ( ~save_callhead = yes ->
	    Arity is Arity0 + 1
	; Arity = Arity0
	).

    {
    % Emit the frame definition (as a class)
    :- fluid middefs :: accum.

    emit :-
	call((
	  code :: accum(Code),
	  Args = [Cont, Prev|Args0],
	  Cont = ~my_var_new(_, cargmem(0)),
	  Prev = ~my_var_new(_, cargmem(1)),
	  CV = ~my_var_new(_, mthis('cont')),
	  code.add(move(Cont, CV)),
	  PV = ~my_var_new(_, mthis('prev')),
	  code.add(move(Prev, PV)),
          ( ~save_callhead = yes ->
	      Args0 = [G],
	      G = ~my_var_new(_, cargmem(2)),
	      GV = ~my_var_new(_, mthis('callhead')),
	      code.add(move(G, GV))
	  ; Args0 = []
	  ),
	  framevardecls(~size)
	)),
	CtorR = ~module_s.query_ref_BK(~self, 'ctor', ~length(Args)),
	middefs.add(nested_def(~self, normal, [bcode(CtorR, Args, Code)])).
    }.

    {
    :- fluid code :: accum.

    % Initialization of frame variables
    :- static framevardecls/1.
    framevardecls(N) :- framevardecls__2(0, N).

    :- static framevardecls__2/2.
    framevardecls__2(N0, N) :- N0 >= N, !.
    framevardecls__2(N0, N) :-
        % TODO: query 'term_typing.var' class
        F = ~my_var_new(_, framevar0(N0)),
%	code.add(inith(F)),
	code.add(init_null(F)),
	N1 is N0 + 1,
	framevardecls__2(N1, N).
    }.
}.

% ===========================================================================
% Symbols in middle-level code (msym)

% Does the msym need to be registered in code?
% TODO: customize, include as part of the ref_BKS
msym_register(Ref) := Register :-
	Ref instance_of ref_BKS,
	~Ref.name = 'bb'(_),
	!,
	Register = no.
msym_register(Ref) := Register :-
	Ref instance_of ref_BKM,
	~Ref.name = 'buildstr'(_,_),
	!,
	Register = no.
msym_register(_) := yes.

{
:- fluid enclosing :: any.
% @var{Id} is defined outside the current scope
msym_external(Ref) :- Ref instance_of ref_BKS, ~Ref.name = 'new_fresh', !,
	% TODO: allowed here for 'new_fresh', fix it (see term_test example)
	true.
msym_external(Ref) :-
	% TODO: kludge: we do not distinguish root functors (for modules) from user functors (for user-terms)
	msym_relative_id(Ref, ~enclosing, IsLocal, _),
	IsLocal = no.
}.

% :- pred msym_height(+Ref,-Height) :: psymbol * int
%    # "Height of Ref (number of steps from the root module)".
msym_height(R) := L :- l :: m_int <- 0, msym_height_(R), L = ~l.
{
:- fluid l :: m_int.
msym_height_(R) :- R = ~module_s.lookup_module(root), !.
msym_height_(R) :- R0 = ~msym_enclosing(R), !, l.inc(1), msym_height_(R0).
msym_height_(_).
}.

% :- pred msym_enclosing(+Ref,-Enclosing) :: psymbol * psymbol
%    # "The enclosing in which the symbol was defined".
msym_enclosing(R) := _ :- var(R), !, fail. % TODO: Strange
msym_enclosing(R) := R2 :- R instance_of frame_str, !,
	R2 = ~R.pred.
msym_enclosing(R) := R2 :- R instance_of predicate_s, !,
        R2 = ~R.owner_module.
msym_enclosing(R) := R2 :- R instance_of ref_BKS, !,
	R2 = ~R.owner_module.
msym_enclosing(R) := R2 :-
	trust(R instance_of module_s),
        R2 = ~R.enclosing_module.

% ===========================================================================
% Mappings between msym and JS-tree identifiers
% (identifier for module structure, absolute identifiers, relative
% identifiers, etc.)

% Name of the indentifier in the module entry
msym_modentry_id(Ref) := R :-
	Ref instance_of ref_BUM,
	!,
	Name = ~Ref.name,
	( Name = 'buildstr'(N, A) -> % TODO: use a hashtable? make it a special case?
	    R = id_pred_entry(id_functor(N,A))
	; R = Name % TODO: this could be improved
	).
msym_modentry_id(Ref) := R :-
	Ref instance_of ref_BKM,
	!,
	Name = ~Ref.name,
	( Name = 'buildstr'(N, A) -> % TODO: use a hashtable? make it a special case?
	    R = id_pred_entry(id_functor(N,A))
	; R = Name % TODO: this could be improved
	).
msym_modentry_id(Ref) := R :-
	Ref instance_of ref_BKS,
	Name = ~Ref.name,
	( Name = 'natcode'(_) ->
	    R = id_natcode('') % TODO: problems?
	; A = ~Ref.arity,
	  R = id_functor(Name, A) % TODO: problems?
	).

% TODO: simplify
% :- pred msym_relative_id(+Ref, +Enclosing, -IsLocal, -RelId) ::
%    psymbol * psymbol * term * term
% # "@var{RelId} is the relative js_id to @var{Id} from
%    @var{Enclosing}.  @var{IsLocal} is @tt{yes} if @var{Id} was local,
%    otherwise it is @tt{no}".
%
% Example:   a.b.c.d.e under a.b.c is represented as
%                3.d.e
msym_relative_id(Ref, Enclosing, IsLocal, RelId) :-
	enclosing :: any <- Enclosing,
	is_relative :: any <- _,
	RelId0 = ~rel_msym(Ref),
	IsLocal = ( nonvar(~is_relative) ? yes | no ),
	( IsLocal = no ->
	    RelId = RelId0
	; L = ~msym_height(~enclosing),
	  % Note: L (level) is used to avoid conflicts in name
	  % declarations (i.e., access an element of an upper scope)
	  RelId = id_l(L, RelId0)
	).

{
:- fluid enclosing :: any.
:- fluid is_relative :: any.
% TODO: simplify; use msym_enclosing?
rel_msym(X) := '' :- X = ~enclosing, !, ~is_relative = yes.
rel_msym(X) := _ :- var(X), !, fail. % TODO: Strange
rel_msym(Ref) := X :- Ref instance_of ref_BKM, !,
%	OM = ~Ref.owner_module,
	Name = ~Ref.name,
	( Name = 'buildstr'(N, A) ->
	    X = id_pred_entry(id_functor(N,A)) % TODO: missing ~rel_msym(OM)
	; throw(not_supported_msym_modentry_id(Ref))
	).
rel_msym(Ref) := X :- Ref instance_of ref_BKS, !,
	OM = ~Ref.owner_module,
	Name = ~Ref.name,
	( Name = 'bb'(Cont) ->
	    X = id_bb(~rel_msym(OM), Cont)
	; Name = 'natcode'(_) ->
	    X = id_natcode(~rel_msym(OM))
	; Name = 'ctor' ->
	    X = ~rel_msym(OM)
        ; A = ~Ref.arity,
	  R3 = ~rel_mod(OM, Name),
	  % TODO: mark basal predicates in some way?
	  X = id_functor(R3, A)
	).
rel_msym(R) := X :- R instance_of frame_str, !,
	X = id_frame(~rel_msym(~R.pred), ~R.save_callhead, ~R.size).
rel_msym(R) := X :- R instance_of predicate_s, !,
	R.get_name(N, A),
        ModuleR = ~R.owner_module,
	R2 = ~rel_mod(ModuleR, N),
	X = id_functor(R2, A).
rel_msym(R) := X :- !,
	trust(R instance_of module_s),
        ( R = ~module_s.lookup_module(root) ->
	    X = id_mod('') % TODO: sure?
        ; N = ~R.get_name,
	  ModuleR = ~R.enclosing_module,
	  R3 = ~rel_mod(ModuleR, N),
	  X = id_mod(R3)
	).

% relative module access path
rel_mod(ModuleR, N) := R2 :-
	( ModuleR = ~module_s.lookup_module(root) ->
	    R2 = N
	; P2 = ~rel_msym(ModuleR),
	  ( P2 = '' -> R2 = N
	  ; id_mod(P3) = P2 -> R2 = ':'(P3,N)
	  ; id_functor(N2,A2) = P2 -> R2 = ':'(N2/A2,N)
	  )
	).
}.

% Local identifier of the msym
msym_local_id(R) := _ :- var(R), !, fail. % TODO: Strange
msym_local_id(R) := Id :- R instance_of frame_str, !,
	Id = id_frame('', ~R.save_callhead, ~R.size).
msym_local_id(R) := Id :- R instance_of predicate_s, !,
	R.get_name(N, A),
	Id = id_functor(N, A).
% TODO: missing
% msym_local_id(R) := Id :- R instance_of ref_BKS, !, ...
msym_local_id(R) := Id :-
	trust(R instance_of module_s),
	Id = id_mod(~R.get_name).

% ===========================================================================
% Symbol dictionary for JS generation
%
%   For each symbol we store:
%    - symkind:  kind of symbol
%    - used:     'yes' if the symbol is being used in the current scope
%    - local_id: JS identifier for the current scope
%    - register: 'yes' if it needs registration in the module structure
%
% TODO: add a class for msym?

{
:- fluid symdic :: cdic. % Symbol dictionary

% Mark @var{Ref} as used in symdic.
msym_mark_used(Ref) :-
	msym_(Ref, msymlive(_, yes, _)).

% Mark @var{Ref} as defined in symdic
msym_mark_def(Ref) :-
	msym_(Ref, msymlive(yes, _, _)).

% The JS identifier (which may be bound later) for the given @var{Ref}.
msym_js_id(Ref) := LocalId :-
	msym_(Ref, msymlive(_, _, LocalId)).

% Merge symbol definitions into the current scope
syms_merge([]).
syms_merge([Ref-D|Xs]) :-
	( msym_(Ref, D) ->
	    true
	; % TODO: this is a bug
	  msym_(Ref, StoredD), 
	  throw(bug_cannot_merge_msym(Ref, D, StoredD))
	),
	syms_merge(Xs).

% (low-level access to properties in 'symdic')
msym_(Ref, D) :-
	( D0 = ~symdic.get(Ref) -> D = D0
	; symdic.insert(Ref, D)
	).
}.




