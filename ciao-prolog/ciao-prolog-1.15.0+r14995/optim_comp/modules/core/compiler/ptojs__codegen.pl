% ===========================================================================
% Compilation of modules to middle-level (aka WAM) code

% ===========================================================================
% Parameters for code generation

% (avoids copying of A(_) registers to Y(_) registers)
use_prealloc_args :- true.
%use_prealloc_args :- fail.

% Enable to use A(_) registers for arguments
use_argmems :- fail. 
%use_argmems. % TODO: problems with coallesce... (see eqvarmem)

:- compilation_fact(args_in_call). % Store call arguments

% Traversal order for compiling terms:
%  - postorder (subterms first)
%  - preorder (root first)
%
% Note: That means that leafs are ready before the roots.
%       For some data representations that cannot be partially
%       constructed, this is the last resort. This will make
%       unification slower if the term is nonvar.

% TODO: Write heuristics here
c_eq_traversal(FunctorR, Traversal) :-
	% This is not a heuristic, for ref_PUM only 'postorder'
	% is available at this moment. Implementing 'preorder' needs a
	% solution for the 'module/state argument'. I.e.,
	% M:f(A1,...An) may contain n or n+1 actual arguments
	% (depending on whether M is part of the state or not).
	FunctorR instance_of ref_PUM, !,
	Traversal = postorder.
c_eq_traversal(_, preorder). % (default in WAM, fails first)
%c_eq_traversal(_, postorder).

% TODO: Merge with bytecode__compiler.pl

% TODO: use real bc compiler
%:- compilation_fact(trace_bc_comp).
:- if(defined(trace_bc_comp)).
trace_bc_comp.
:- else.
trace_bc_comp :- fail.
:- endif.

% ---------------------------------------------------------------------------

:- include(compiler(ptojs__scope)).
:- include(compiler(ptojs__norm)).

{
:- fluid middefs :: accum.
% Compile a list of modules
midcomp_modules([]).
midcomp_modules([ModuleR|ModuleRs]) :-
	trust(ModuleR instance_of module_s),
	midcomp_module(ModuleR),
	midcomp_modules(ModuleRs).
}.

{
:- fluid middefs :: accum.
:- public midcomp_module/1.
midcomp_module(ModuleR) :-
	m :: module_s <- ModuleR,
        midcomp_module0,
	!.
midcomp_module(ModuleR) :-
	throw(bug_failed_module_to_middefs(ModuleR)).
}.

{
:- fluid m :: module_s.
:- fluid middefs :: accum.

midcomp_module0 :-
	m.do_not_materialize, !.
midcomp_module0 :-
	split_preds(~m.all_preds, ConsRs, FunctorRs),
	%
	% TODO: this native code is emitted outside the module, sometimes we do not want this
	compile_native,
	% 
	call((
	  middefs :: accum(Defs),
	  compile__preds(~norm_preds(ConsRs)),
	  compile__inheritance(ConsRs),
	  compile__mod_attrs,
	  compile__preds(~norm_preds(FunctorRs)),
	  ( ~m = ~module_s.lookup_module(root) ->
	      display(user_error, compilingroot), nl(user_error),
	      true % TODO: kludge
	  ; midcomp_modules(~m.nested_module_list)
	  ),
	  add_auto_traits,
	  compile__preds(~norm_preds(~m.drain_auxpreds)) % TODO: repeat again until it is empty
        )),
	( m.get_prop(static_noself) -> % no class wrapper
	    emit_defs(Defs)
	; middefs.add(nested_def(~m, normal, Defs))
	).
}.

% TODO: move outside the scope
% Split predicate (and functor) definitions in several kinds
%   ConsR: constructors
%   FunctorRs: the rest of predicates
split_preds(FunctorRs0, ConsRs, FunctorRs) :-
	% TODO: kludge? extract the (basal) constructor from the predicates
	( select(ConsR, FunctorRs0, FunctorRs),
	  trust(ConsR instance_of predicate_s),
	  ConsR.get_prop(basal),
	  ConsR.get_name(N, _A),
	  N = 'cons__' ->
	    ConsRs = [ConsR]
	; FunctorRs = FunctorRs0,
	  ConsRs = []
	).

{
:- fluid m :: module_s.
:- fluid middefs :: accum.

% Emit code for inheritance
compile__inheritance(ConsRs) :-
	Base = ~m.base,
        ( m.get_prop(static_noself) ->
	    ( ConsRs = [] -> true ; throw(bad_cons(ConsRs)) )
	; Base = ~module_s.lookup_module(var_base) ->
	    middefs.add(extends('var_base'))
	; Base = ~module_s.lookup_module(basal_base) ->
	    true
	; Base = ~module_s.lookup_module(basalnv_base) ->
	    middefs.add(extends(~module_s.lookup_type(nonvar)))
	; Base = ~module_s.lookup_module(nonvar_base) ->
	    true
	; m.get_prop(simple_box(CompareOperator)) ->
            def__simple_box(CompareOperator),
	    check_no_cons(ConsRs)
	; def__atomic_class,
	  check_no_cons(ConsRs)
        ).
}.

% Make sure that there are not additional constructors
check_no_cons([]) :- !.
check_no_cons(ConsRs) :-
	throw(bad_cons(ConsRs)).

{
:- fluid middefs :: accum.

emit_defs([]).
emit_defs([X|Xs]) :- middefs.add(X), emit_defs(Xs).
}.

{
:- fluid m :: module_s.
:- fluid middefs :: accum.

% Emit code for module/class attributes
compile__mod_attrs :-
        ( ( m.get_prop(static_noself)
	  ; m.get_prop(static)
	  ) ->
	    % TODO: filter 'static' attrs (see do_mod_attr)
	    emit_attrs(~m.all_attrs)
	; true
	).
}.

{
:- fluid middefs :: accum.

emit_attrs([]).
emit_attrs([X|Xs]) :- emit_attr(X), emit_attrs(Xs).

emit_attr(Name) :-
        % Declare the variable (if it is static)
	AttrMem = sattmem(Name),
	middefs.add(declvar(AttrMem)).
}.

{
:- fluid m :: module_s.
:- fluid middefs :: accum.

compile_native :-
	% Write native code
	findall(X, m.get_native(X), Xs),
	emit_native_list(Xs).

emit_native_list([]).
emit_native_list([X|Xs]) :-
%        display(user_error, enl(X)), nl(user_error),
        middefs.add(js_raw_native(X)),
	emit_native_list(Xs).
}.

% ===========================================================================
% Compilation of predicates to middle-level code

{
:- fluid middefs :: accum.

compile__preds([]).
compile__preds([FunctorR|FunctorRs]) :-
	trust(FunctorR instance_of predicate_s),
	( compile__pred(FunctorR) ->
	    true
	; throw(bug_failed_pred_to_middefs(FunctorR))
	),
	compile__preds(FunctorRs).

compile__pred(P) :-
	currpred :: predicate_s <- P,
	compile__pred0.
}.

{
:- fluid currpred :: predicate_s.
:- fluid middefs :: accum.
compile__pred0 :- currpred.get_prop(virtual), !.
compile__pred0 :- currpred.get_prop(basal), !,
        compile_basal.
compile__pred0 :-
        compile_functor.
}.

% Get the scope for the predicate body
% Note: this links 'self' with the first argument, if necessary
get_scope(PredId, As) := Scope :-
	trust(PredId instance_of predicate_s),
	ModuleR0 = ~PredId.owner_module,
	cscope :: scope <- ~scope.new(ModuleR0),
	( PredId.needs_self ->
	    % Links the first argument for Self (if we are defining a method)
	    As = [Self|_],
	    cscope.set_selfvar(Self)
	; true
	),
	Scope = ~cscope.

{
% Emit 'buildstr(P)' method of the class/module object for this
% predicate (for calls to ref_PUM/ref_PUS).
% TODO: implement 'set_callhead', which instead of filling the heap, fills the worker
:- fluid middefs :: accum.
emit_buildstr(FunctorR, NeedsSelf) :-
	trust(FunctorR instance_of predicate_s),
	FunctorR.get_name(N, A),
	As = ~new_vars_m(A),
	Mems = ~cargs_mem(A),
	set_varmems_d(As, Mems),
	% We include 'this' as explicit argument to FunctorR
	This = ~my_var_new(_, rawmem('this')),
	As2 = ( NeedsSelf = yes ? [This|As] | As ),
	% TODO: This should just setup call_head
	Code = [proceed_out1(new(msym(~query_ref_BKS_ctor(FunctorR)), As2))],
	%
	ModuleR0 = ~FunctorR.owner_module,
	SetCallHeadR = ~module_s.query_ref_BK(ModuleR0, 'buildstr'(N, A), A),
	middefs.add(bcode(SetCallHeadR, As, Code)).
}.

{
:- fluid currpred :: predicate_s.
{
:- fluid middefs :: accum.
% Compile a basal predicate
compile_basal :-
        ( currpred.get_name(N, A), N = 'cons__' ->
	    OwnerModuleR = ~currpred.owner_module,
	    Ref = ~module_s.query_ref_BK(OwnerModuleR, 'ctor', A)
	; Ref = ~currpred.query_basal
	),
	currpred.get_prop(specialdef(_, basaldef(Args, ECode))),
	middefs.add(bcode(Ref, Args, ECode)).
}.

% ---------------------------------------------------------------------------
% Definition and methods for 'functor class' (atoms and compound terms)
% TODO: devise a way to share definitions
% TODO: problem: variable number of arguments

{
:- fluid middefs :: accum.

compile_functor :-
	call((
          middefs :: accum(CDefs),
          % common definitions for all functor-like objects
          emit_functor_defs(Kind),
	  % the execution of the body associated to the functor
	  emit_natcode, % TODO: should natcode be at the module level instead? (at least cached as that)
	  emit_execcode,
	  emit_partial_apply
        )),
	middefs.add(nested_def(~currpred, Kind, CDefs)),
	% Emit 'buildstr(P)' method of the class/module object for this predicate
	% TODO: this could be just a relational specification compiled in a special way
	( needs_register ->
	    NeedsSelf = ( currpred.needs_self ? yes | no ),
	    emit_buildstr(~currpred, NeedsSelf)
	; true
	).

% The predicate needs to be registered in the module/class (for dynamic dispatching)
% TODO: define 'register_pred' instead? (then, we could add several tables)
needs_register :-
	( currpred.get_prop(is_functor_trait) -> % TODO: not very nice
	    fail
	; OwnerModuleR = ~currpred.owner_module,
	  OwnerModuleR.get_prop(static_noself) -> % TODO: should I remove 'static_noself'?
	    fail
        ; currpred.get_name(N, _), static_pred_name(N) -> % TODO: declare somewhere; internal predicates
	    fail
	; true
	).

emit_functor_defs(Kind) :-
        % The constructor and some static definitions
	N = ~currpred.get_real_arity,
	% TODO: EnclosingId duplicated in predicate:compile_functor_ctor
	( currpred.get_prop(is_functor_trait) ->
	    Kind = trait([OwnerCtor, Name]),
	    % TODO: This should be the real module object (not the ctor)
	    OwnerCtor = id_param('owner'),
	    Name = id_param('name')
	; Kind = normal,
	  OwnerModuleR = ~currpred.owner_module,
	  % TODO: This should be the real module object (not the ctor)
	  OwnerCtor = ( OwnerModuleR = ~module_s.lookup_module(root) ? 'null'
		      | msym(~query_ref_BKS_ctor(OwnerModuleR))
		      ),
	  currpred.get_name(F, _),
	  Name = native_string(~atom_codes(F))
	),
        ( \+ currpred.really_use_functor_trait ->
	    emit_functor_ctor(~currpred, unroll_args(N)),
	    NonvarR = ~module_s.lookup_type(nonvar),
	    middefs.add(extends_nonvar(NonvarR,
	      OwnerCtor, % owner module ctor
	      Name, % name
	      N)), % (real) arity
	    emit_unify_nonvar,
	    emit_copy_fresh,
	    emit_new_fresh
	; middefs.add(functor_trait(OwnerCtor, Name, N))
	).
}.

{
:- fluid middefs :: accum.
% emit sd_unify_nonvar/1 method
emit_unify_nonvar :-
	This = ~my_var_new(_, rawmem('this')),
	Other = ~my_var_new(_, cargmem(0)),
	A = ~currpred.get_real_arity,
	%
	Vs = ~new_vars_m(A),
	VsMems = ~strargs_mem(This, A),
	set_varmems_d(Vs, VsMems),
	%
	Os = ~new_vars_m(A),
	OsMems = ~strargs_mem(Other, A),
	set_varmems_d(Os, OsMems),
	%
	call((
          xdic :: xreg_cache,
	  wcode :: accum(Code),
	  wcode.add(test_class(msym(~query_ref_BKS_ctor(~currpred)), Other)),
	  unifyargs(Vs, Os),
	  wcode.add(proceed)
        )),
	UnifyR = ~module_s.query_ref_BK(~currpred, 'sd_unify_nonvar', 1),
	middefs.add(bcode(UnifyR, [Other], Code)).
}.

{
:- fluid xdic :: xreg_cache.
:- fluid wcode :: accum.

:- '$ctxprj'(unifyargs/2, [wcode, u(xdic)]).
% TODO: WRONG
unifyargs([], []).
unifyargs([X|Xs], [Y|Ys]) :-
	my_c_eq_unify(X, Y),
 	unifyargs(Xs, Ys).
}.

{
:- fluid middefs :: accum.
% Emit 'copy_fresh' basal method
emit_copy_fresh :- % TODO: see 'binfo'
	A = ~currpred.get_real_arity,
	Code = [proceed_out1(new(msym(~query_ref_BKS_ctor(~currpred)), ~c_new_vars(A)))],
	CopyFreshR = ~module_s.query_ref_BK(~currpred, 'copy_fresh', 0),
	middefs.add(bcode(CopyFreshR, [], Code)).
}.

{
:- fluid middefs :: accum.
% Emit 'new_fresh' basal pred
emit_new_fresh :-
	SReg = ~my_var_new(_, cargmem(0)),
	Code = [proceed_new_fresh(SReg, ~currpred)],
	NewFreshR = ~module_s.query_ref_BK(~currpred, 'new_fresh', 2),
	middefs.add(bcode(NewFreshR, [SReg], Code)).
}.

% ---------------------------------------------------------------------------

{
:- fluid middefs :: accum.
% Native JS code associated to the predicate body (if suitable)
emit_natcode :-
	currpred.get_prop(specialdef(_As, basaldef(CArgs, CodeN))),
	!,
	A0 = ~length(CArgs),
	DetMode = ~currpred.get_det,
	( is_detfun(DetMode) ->
	    A is A0 + 1
	; A = A0
	),
	NatCodeR = ~module_s.query_ref_BK(~currpred, 'natcode'(~currpred.get_det), A),
	middefs.add(bcode(NatCodeR, CArgs, CodeN)).
emit_natcode.
}.

{
:- fluid middefs :: accum.
% Compile the method for 'execute' instruction (execute the body
% associated to the functor)
emit_execcode :-
	InsR = ~currpred.query_ref_BK('execute', 0),
	( currpred.really_use_functor_trait, currpred.get_prop(specialdef(_, functor_def)) ->
	    true % when using functor trait, execute method is not needed
	; icode(a, As, or(Cs0)) = ~currpred.code,
	  Scope = ~get_scope(~currpred, As),
	  compile0(Scope, InsR, As, Cs0)
	).
}.

{
% Emit code for the partial application
:- fluid middefs :: accum.
emit_partial_apply :-
	( currpred.get_prop(specialdef(_, partial_def(_, _))) -> % TODO: ugly
	    compile_partial_apply('partial_mgc'),
	    compile_partial_apply('partial_mec')
	; true
	).

% TODO: I would like to optimize the 'build' case... or avoid it; just define abstract preds/functions
compile_partial_apply(Ins) :-
	currpred.get_prop(specialdef(As, partial_def(AppliedArgs, PartialDef))),
	Scope = ~get_scope(~currpred, As),
	% The predicate is defined as a partial application, define internal method for partial application
	PBody = ( PartialDef = method(PartialSelf, FullHead) ?
		    ~mcall(PartialSelf, FullHead)
		| PartialDef = not_method(FullHead) ?
		    FullHead
		),
	%
	% InsArgs: variables of the partial application
	( Ins = 'partial_mgc' ->
	    % call to partial application
	    InsArgs = AppliedArgs,
	    InsBody1 = [PBody] % TODO: normalized code
	; Ins = 'partial_mec' ->
	    % TODO: 'partial_mec' should be a 'semidet predicate'
	    % unification to a partial application
	    InsArgs = ~append(AppliedArgs, [Res]),
	    InsBody1 = [(Res = PBody)] % TODO: normalized code
	),
	InsR = ~currpred.query_ref_BK(Ins, ~length(InsArgs)),
	insert_metargs(InsBody1, InsArgs, InsBody2), % TODO: Add MetArgs as a special prop of the predicate?
	% TODO: missing norm_preds
	compile0(Scope, InsR, As, [InsBody2]).
}.
}.

{
:- fluid middefs :: accum.
% Emit constructor code for the functor
emit_functor_ctor(EnclosingR, unroll_args(N)) :-
	As = ~new_vars_m(N),
	Mems = ~cargs_mem(N),
	set_varmems_d(As, Mems),
	%
	This = ~my_var_new(_, rawmem('this')),
	InThisAs = ~new_vars_m(N),
	InThisMems = ~strargs_mem(This, N),
	set_varmems_d(InThisAs, InThisMems),
	%
	call((
          code :: accum(Code),
	  emit_argscopy(As, InThisAs)
        )),
	CtorR = ~module_s.query_ref_BK(EnclosingR, 'ctor', N), % TODO: define somewhere?
	middefs.add(bcode(CtorR, As, Code)).
}.

{
:- fluid code :: accum.
emit_argscopy([], []).
emit_argscopy([A|As], [InThisA|InThisAs]) :-
	code.add(move(A, InThisA)),
	emit_argscopy(As, InThisAs).
}.

% N new variables
c_new_vars(N) := ~c_new_vars_(0, N).

c_new_vars_(N0, N) := [] :- N0 >= N, !.
c_new_vars_(N0, N) := [X|Xs] :-
	% TODO: query 'term_typing.var' class
	X = new(ctor_lookup(class, ':'('term_typing', 'var')), []),
	N1 is N0 + 1,
	Xs = ~c_new_vars_(N1, N).

% ===========================================================================

{
:- fluid m :: module_s.

% Create necessary traits and register them as auxiliary predicates in
% the top_enclosing_module
% TODO: include usermods in a similar way (so that
%       truly dynamic modules can be implemented)
% TODO: do this during predicate compilation, not after
add_auto_traits :-
        % Make sure that there exist the necessary functor traits for
        % the functors/predicates defined in this module. Define them
        % in the top enclosing module.
	( member(P, ~m.all_preds),
	    add_auto_trait(P),
	    fail
	; true
	).

add_auto_trait(P) :-
        trust(P instance_of predicate_s),
	( \+ P.get_prop(is_functor_trait) ->
	    TopEnclosing = ~m.top_enclosing_module,
	    A = ~P.get_real_arity,
	    TraitR = ~TopEnclosing.trait_functor_ref(A),
	    % Auxiliary predicates
	    TopEnclosing.ensure_auxpred(TraitR)
	; true
	).
}.

% ---------------------------------------------------------------------------
% Definition and methods for the 'simple_box' class (and derived)
{
:- fluid m :: module_s.
:- fluid middefs :: accum.
def__simple_box(CompareOperator) :-
	Base = ~m.base,
	% TODO: this should be a trait, not a base
	% A nonvar that boxes a datum. Comparing two elements involves unboxing
	% TODO: This is a non-unificable attribute
	% TODO: improve the hierarchy class
	emit_functor_ctor(~m, unroll_args(1)),
	middefs.add(extends(Base)),
	middefs.add(prop('arity', 0)),
	emit__simple_box__unbox,
	emit__simple_box__unify(CompareOperator).

% Unbox operation for the 'simple_box' class
emit__simple_box__unbox :-
	Ref = ~module_s.query_ref_BK(~m, 'unbox', 0),
	This = ~my_var_new(_, rawmem('this')),
	V = ~my_var_new(g,strmem(This,0)),
	Code = [proceed_out1(V)],
	middefs.add(bcode(Ref, [], Code)).

emit__simple_box__unify(CompareOperator) :-
	Ref = ~module_s.query_ref_BK(~m, 'sd_unify_nonvar', 1),
	This = ~my_var_new(_, rawmem('this')),
	Other = ~my_var_new(_, rawmem('other')),
	Arg0 = ~my_var_new(g,strmem(This,0)),
	OArg0 = ~my_var_new(g,strmem(Other,0)),
	Code = 
          [test_class(msym(~query_ref_BKS_ctor(~m)), Other),
	   low_test(op(CompareOperator, [Arg0, OArg0])),
	   proceed],
	middefs.add(bcode(Ref, [Other], Code)).
}.

% ---------------------------------------------------------------------------
% Definition and methods for atomic classes (and derived).
%
% Two instances of 'atomic classes' unify iff they are the same
% instance.

{
:- fluid m :: module_s.
:- fluid middefs :: accum.
def__atomic_class :-
	% TODO: EnclosingId duplicated in predicate:compile_functor_ctor
	Base = ~m.base,
	Enclosing = ~m.enclosing_module,
	EnclosingCtor = ( Enclosing = ~module_s.lookup_module(root) ? 'null'
		        | msym(~query_ref_BKS_ctor(Enclosing))
		        ),
	emit_functor_ctor(~m, unroll_args(0)),
	middefs.add(extends(Base)),
	middefs.add(prop('enclosing_module', EnclosingCtor)), % TODO: This should be the real module object (not the ctor)
	middefs.add(prop('name', native_string(~atom_codes(~m.get_name)))),
	emit__atomic__unify.

% Two elements are equal if they are exactly the same object.
% TODO: this compares 'self' (allow args to be just like normal arguments?)
emit__atomic__unify :-
	Ref = ~module_s.query_ref_BK(~m, 'sd_unify_nonvar', 1),
	This = ~my_var_new(_, rawmem('this')),
	Other = ~my_var_new(_, rawmem('other')),
	Code =
	  [test_class(msym(~query_ref_BKS_ctor(~m)), Other),
	   % Note: In Javascript, both '===' and '==' return true iff the
	   %       objects are the same. However, '==' may try type
	   %       conversions for some types.
	   low_test(op('===', [This, Other])),
	   proceed],
	middefs.add(bcode(Ref, [Other], Code)).
}.

% ===========================================================================
% JSWAM compiler (unoptimal)
% TODO: merge with bytecode__compiler

{
:- fluid cscope :: scope.
%:- public my_compile_clauses/3.
my_compile_clauses([], _, []).
my_compile_clauses([C0|Cs], Args, [C2r|Cs2]) :-
	my_compile_clause(C0, Args, C2r),
	my_compile_clauses(Cs, Args, Cs2).

my_compile_clause(Body0000, Args0, ma_clause(MetArgsP, Code2, Data)) :-
	renamevars_clause(Args0, Body0000, Args1, Body000),
	extract_metargs(Body000, MetArgsP, Body00b),
        % TODO: optimize unflat... or remove it, but ensure that the results are ok (without it the contents of X registers is reused when they contain constants - I do not know what is better)
	unflat_head(Args1, Body00b, Args, Body00),
	( Body00 = [icall(G)|Body0],
	  trust(G instance_of strgoal),
	  ~G.get_id = 'basiccontrol:$caller_choice/1',
	  [Choice] = ~G.args ->
	    true
	; Body00 = Body0,
	  Choice = ~my_var_new(_,_) % choice was not used
	),
	Trivial = ( trivial_head(Args, []) ? true | false ),
	extract_type(Body0, Choice, Trivial, Args, 2'11111, Type0, Body1no), Body1 = Body0, % TODO: IMPLEMENT INDEXING!
	TypeKey0 = type_key(Type0,nohash),
	% begin[specific to ptojs]
	( use_prealloc_args -> prealloc_args(Args) ; true ), % TODO: optional, only when using arg registers (make it parametric)
	MetArgsN = ~length(MetArgsP),
	MetArgsMem = ~cargs_mem(MetArgsN),
	set_varmems_d(MetArgsP, MetArgsMem),
	% end[specific to ptojs]
	%
	call((
          chn :: m_int <- 0,
	  list :: any <- List,
	  mk_occurrences_body(Choice, Args, Body1)
        )),
	my_allocate_vas(List, 0),
	%
	call((
          typekey :: m_any <- TypeKey0, % TODO: not used yet
	  my_trans_clause(Args, Choice, Body1, Code),
	  ~typekey = TypeKey
	)),
	incore_parse_key(TypeKey, Data),
	call((
          bcode :: m_any <- Code,
	  my_postcomp,
	  Code2 = ~bcode
	)).
% TODO: SAME extract_type/7
}.

% TODO: SAME type_ck/5

% TODO: SAME trivial_head/2

% TODO: SAME mk_occurrences_body/3

% Allocate frame variables and annotate temporal variables
% (List is the list variables and occurrences in reverse order)
my_allocate_vas([], _) :- !.
my_allocate_vas([size(N)|List], N) :- !,
	my_allocate_vas(List, N).
my_allocate_vas([Mem-Occs|List], N0) :-
	my_allocate_vas_2(Occs, Mem, N0, N),
	my_allocate_vas(List, N).

%DEBUG my_allocate_vas_2(_, _, N, N) :- !. % do nothing
my_allocate_vas_2([_|Single], Mem, N, N) :- var(Single), !, % single chunk
%	( use_prealloc_args, nonvar(Mem) ->
%	    true % do not allocate pre-allocated variables
%	; Mem = x(_) % (was tmpvar) % TODO: inith not introduced
%	).
	Mem = _. % TODO: necessary for some coalescing, but it should be x(_) % (was tmpvar)
my_allocate_vas_2(_, Mem, N0, N) :-
	( use_prealloc_args, nonvar(Mem) ->
	    N = N0 % do not allocate pre-allocated variables
	; Mem = y(N0),
	  N is N0+1
	).

% TODO: SAME incore_parse_key/2
% TODO: merge
{
:- fluid cscope :: scope.
:- fluid typekey :: m_any.
my_trans_clause(Args, Choice, Body, Code) :-
 %TR 	display(user_error, a(Body)), nl(user_error),
	call(( wcode :: accum(Code0), my_trans_clause__1(Args, Choice, Body, Kind, InArity, OutArity) )),
	extract_index(Code0, _Code1b, Gets, Gets, OutArity),
	Code1 = Code0,
% 	display(user_error, ei(Code)), nl(user_error),
 %TR 	display(user_error, b(Code)), nl(user_error),
	%
	% TODO: allocate frames per clause, not per predicate
	save_callhead :: any <- _, % TODO: avoid or move somewhere else?
	% TODO: write a frame for each alternative, not for all of them
        my_clause_lifetime(Kind, InArity, OutArity, [ensure_space(Kind, InArity, _)|Code1], Code2),
	% TODO: avoid or move somewhere else?
	NF = ( Kind = recursive(_) ? yes | no ),
	( NF = no ->
	    % No frame is required (no frame registers and no need to
	    % save continuations)
	    true
	; % TODO: allocate frame objects per module (like user terms), not per clause
	  % Save a pointer to the goal arguments in the frame
	  ( Kind = recursive(MaxFrameSize) ->
	      true
	  ; MaxFrameSize = 0
	  ),
	  Frame = frame0(SaveArgs, MaxFrameSize),
	  SaveArgs = ( nonvar(~save_callhead) ? yes | no )
        ),
 %TR	display(user_error, c(Code1)), nl(user_error),
	% Note: frame is not yet instantiated
	Code = ~my_peep_clause(Code2, Frame, NF).
 %TR	display(user_error, d(Code2)), nl(user_error).

% TODO: SAME extract_index/5
}.

% temporal register allocation x(_) % (was tmpvar)
{
:- fluid save_callhead :: any. % if instantated, save args in frame
my_clause_lifetime(_Kind, _InArity, _OutArity, Code0, Code) :-
	wcallhead_lost :: m_any <- no,
        wcode :: accum(Code),
	my_clause_lifetime_(Code0).

{
:- fluid wcode :: accum.
:- fluid wcallhead_lost :: m_any.

my_clause_lifetime_([]).
my_clause_lifetime_([A|Xs]) :-
        ( \+ ins_use_success_cont(A) -> % TODO: also 'modifies X registers' (or alters w.callhead)
	    true
	; Xs = [] ->
	    true
	; wcallhead_lost <- yes
	),
	( varalloc_ins(A) -> true
	; bug(varalloc_ins_failed(A)), fail
	),
	wcode.add(A),
	my_clause_lifetime_(Xs).

varalloc_ins(inith(A)) :- varalloc_d(A).
varalloc_ins(init(A,B)) :- varalloc_d(A), varalloc_d(B).
varalloc_ins(cut(A)) :- varalloc_u(A).
varalloc_ins(move(A, B, _)) :- varalloc_u(A), varalloc_d(B).
varalloc_ins(move(A, B)) :- varalloc_u(A), varalloc_d(B).
varalloc_ins(globunsafe(A, B)) :- varalloc_u(A), varalloc_d(B).
varalloc_ins(u_val(A, B)) :- varalloc_u(A), varalloc_u(B).
varalloc_ins(un_val(_, A)) :- varalloc_u(A).
varalloc_ins(un_var(_, A)) :- varalloc_d(A).
varalloc_ins(neck(_)).
varalloc_ins(ensure_space(_, _, _)).
varalloc_ins(kall(_, _, As)) :- varalloc_us(As).
varalloc_ins(blt1(A, _)) :- varalloc_u(A).
varalloc_ins(blt2(A, B, _)) :- varalloc_u(A), varalloc_u(B).
varalloc_ins(blt3(A, B, C, _)) :- varalloc_u(A), varalloc_u(B), varalloc_u(C).
% TODO: really use a temporal for output (and change to varalloc_d)
varalloc_ins(fun1(A, B, _, _, _)) :- varalloc_d(A), varalloc_u(B).
varalloc_ins(fun2(A, B, C, _, _, _)) :- varalloc_d(A), varalloc_u(B), varalloc_u(C).
varalloc_ins(funre1(A, B, _, _, _)) :- varalloc_d(A), varalloc_u(B).
varalloc_ins(funre2(A, B, C, _, _, _)) :- varalloc_d(A), varalloc_u(B), varalloc_u(C).
varalloc_ins(lastcall(_, As)) :- varalloc_us(As).
varalloc_ins(ld_cons(T, _)) :- varalloc_d(T).
varalloc_ins(u_cons(A, _Cons)) :- varalloc_u(A).
varalloc_ins(un_cons(A, _Cons)) :- varalloc_u(A).
varalloc_ins(ld_str_is(T, _, As)) :- varalloc_us(As), varalloc_d(T). % (input arguments)
varalloc_ins(u_str_is(T, _, As)) :- varalloc_us(As), varalloc_u(T). % (input arguments)
varalloc_ins(un_str_is(T, _, As)) :- varalloc_us(As), varalloc_u(T). % (input arguments)
varalloc_ins(ld_str_os(T, _, As, _)) :- varalloc_ds(As), varalloc_d(T). % (output arguments)
varalloc_ins(u_str_os(T, _, As, _)) :- varalloc_ds(As), varalloc_u(T). % (output arguments)
varalloc_ins(un_str_os(T, _, As, _)) :- varalloc_ds(As), varalloc_u(T). % (output arguments)

varalloc_us([]).
varalloc_us([X|Xs]) :- varalloc_u(X), varalloc_us(Xs).

varalloc_u(X) :- X instance_of termvar, !,
	Mem = ~my_var_mem(X),
        ( nonvar(Mem) -> % already assigned
	    ( Mem = a(_), ~wcallhead_lost = yes ->
	        % we found some use of an a(_) register after
	        % 'callhead' has been lost
	        ~save_callhead = yes % TODO: Use something next to xdic or xdic.ref to mark it?
	    ; true
	    )
	; Mem = x(_), % (was tmpvar)
	  wcode.add(inith(X)) % initialize
	).
varalloc_u(_).

varalloc_ds([]).
varalloc_ds([X|Xs]) :- varalloc_d(X), varalloc_us(Xs).

varalloc_d(X) :- X instance_of termvar, !,
	% Assign a temporal variable (not yet named) to the variable X
	Mem = ~my_var_mem(X),
        ( nonvar(Mem) -> % already assigned
	    true
	; Mem = x(_) % (was tmpvar)
	).
varalloc_d(_).
}.
}.

% TODO: MISSING until key_of_constant/2

{
:- fluid cscope :: scope.
:- fluid wcode :: accum.
my_trans_clause__1(Args, Choice, Body00, Kind, InArity, OutArity) :-
	kind_of_clause(Body00, Kind),
        DefaultChoice = ~my_var_new(_, x(-1)), % TODO: give it a name (in all backends)
	movexg(DefaultChoice, Choice),
	%
	call((
          xdic :: xreg_cache,
	  InArity = ~length(Args),
	  my_c_head_args(Args),
	  xdic.add(DefaultChoice, Choice),
	  my_c_guards(Kind, Body00, InArity, OutArity)
	)).

% TODO: SAME kind_of_clause/2 

{
:- fluid xdic :: xreg_cache.
:- '$ctxprj'(my_c_icall/1, [cscope, wcode, u(xdic)]).
my_c_icall(G) :-
 %TR	display(user_error, g(G)), nl(user_error),
	trust(G instance_of strgoal),
	Args = ~G.args,
	my_c_icall__2(~G.get_id, ~G.predid, Args).
 %TR	display(user_error, done(G)), nl(user_error).

:- '$ctxprj'(my_c_icall__2/3, [cscope, wcode, u(xdic)]).
my_c_icall__2('basiccontrol:$caller_choice/1', _, [X]) :- !,
	( X instance_of termvar -> true ; errlog:bug(['nonvar in caller_choice']), fail ),
        DefaultChoice = ~my_var_new(_, x(-1)),
	my_c_get_arg(DefaultChoice, X).
%    	wcode.add(u_val(A, DefaultChoice)).
my_c_icall__2('basiccontrol:$cut/1', _, [X]) :- !,
	( X instance_of termvar -> true ; errlog:bug(['nonvar in cut']), fail ),
	xdic.ref(X, X2),
        wcode.add(cut(X2)).
my_c_icall__2('term_basic:=/2', _, [X,Y]) :- !,
	( X instance_of termvar,
	  Y instance_of termvar ->
	    my_c_eq_unify(X, Y)
	; X instance_of termvar ->
	    my_c_eq_instance(X, Y)
	; % Y instance_of termvar ->
	  my_c_eq_instance(Y, X)
	).
my_c_icall__2(_, Ref, [X]) :- Ref instance_of predicate_s, Ref.get_prop(specialdef([_], classtest(_))), !, % TODO: compile as a normal blt1
	XX = ~my_var_new(_,x(_)),
	my_c_put_arg_builtin(X, XX),
	wcode.add(blt1(XX, Ref)).
my_c_icall__2(_, PredId, As) :- !,
	Imp = ~ipred__imp(PredId),
	my_c_icall__3(Imp, PredId, As).

:- '$ctxprj'(my_c_icall__3/3, [cscope, wcode, u(xdic)]).
my_c_icall__3(det, PredId, Args) :- !,
	XV = ~my_var_new(_,x(_)),
	( Args = [X, Value] ->
	    XX = ~my_var_new(_,x(_)),
	    my_c_put_arg_builtin(X, XX),
	    wcode.add(fun1(XV, XX, PredId, _, _))
	; Args = [X, Y, Value] ->
	    XX = ~my_var_new(_,x(_)),
	    XY = ~my_var_new(_,x(_)),
	    my_c_put_arg_builtin(X, XX),
	    my_c_put_arg_builtin(Y, XY),
	    wcode.add(fun2(XV, XX, XY, PredId, _, _))
	; errlog:bug(['invalid function ', PredId, ' with args ', Args]), fail
	),
	% TODO: use this or create a new var? g = ~my_var_d(XV),
	my_c_eq_unify(~my_var_new(g,~my_var_mem(XV)), Value).
my_c_icall__3(semidet_re, PredId, Args) :- !, % TODO: this name seems to be wrong; this should be 'detfun_re'? (it throws exceptions, it does not fail)
	XV = ~my_var_new(_,x(_)),
	( Args = [X, Value] ->
	    XX = ~my_var_new(_,x(_)),
	    my_c_put_arg_builtin(X, XX),
	    wcode.add(funre1(XV, XX, PredId, _, _))
	; Args = [X, Y, Value] ->
	    XX = ~my_var_new(_,x(_)),
	    XY = ~my_var_new(_,x(_)),
	    my_c_put_arg_builtin(X, XX),
	    my_c_put_arg_builtin(Y, XY),
	    wcode.add(funre2(XV, XX, XY, PredId, _, _))
	; errlog:bug(['invalid function ', PredId, ' with args ', Args]), fail
	),
	% TODO: use this or create a new var? g = ~my_var_d(XV),
	my_c_eq_unify(~my_var_new(g,~my_var_mem(XV)), Value).
my_c_icall__3(semidet, PredId, Args) :-
	( Args = [X] ->
	    XX = ~my_var_new(_,x(_)),
	    my_c_put_arg_builtin(X, XX),
	    wcode.add(blt1(XX, PredId))
	; Args = [X, Y] ->
	    XX = ~my_var_new(_,x(_)),
	    XY = ~my_var_new(_,x(_)),
	    my_c_put_arg_builtin(X, XX),
	    my_c_put_arg_builtin(Y, XY),
	    wcode.add(blt2(XX, XY, PredId))
	; Args = [X, Y, Z] ->
	    XX = ~my_var_new(_,x(_)),
	    XY = ~my_var_new(_,x(_)),
	    XZ = ~my_var_new(_,x(_)),
	    my_c_put_arg_builtin(X, XX),
	    my_c_put_arg_builtin(Y, XY),
	    my_c_put_arg_builtin(Z, XZ),
	    wcode.add(blt3(XX, XY, XZ, PredId))
	; errlog:bug(['invalid builtin ', PredId, ' with args ', Args]), fail
	).

% Note: Depending on FunctorR, the term may be build in different ways
% (i.e., like with kall and lastcall, if FunctorR is some
% ref_PUM, the concrete term representation is given by the
% first argument)

% (A termvar, \+ B termvar)
:- '$ctxprj'(my_c_eq_instance/2, [cscope, wcode, u(xdic)]).
my_c_eq_instance(X, V) :-
	Du = ~my_var_d(X),
	( V instance_of termvar -> errlog:bug(['var in instance']), fail ; true ),
	XT1 = ~my_var_new(_,x(_)),
        % Introduce 'T1' here to avoid a cache clash.
	( var(Du) ->
	    Du = g,
	    wcode.add(init(XT1,X))
	; xdic.ref(X, X2),
	  movexp(X2, XT1)
	),
	my_c_get_arg(V, XT1).

% Emit unification (if possible, coalesce variables to avoid an unification)
% (A termvar, B termvar)
% TODO: rename: value=nonfresh; var=fresh (or use 'init, uninit', etc.)
:- '$ctxprj'(my_c_eq_unify/2, [wcode, u(xdic)]).
my_c_eq_unify(U, U1) :-
	U == U1, !.
my_c_eq_unify(X, Y) :-
	Du = ~my_var_d(X),
	U = ~my_var_mem(X),
	Dv = ~my_var_d(Y),
	V = ~my_var_mem(Y),
	( var(Du), U=V, Du=Dv -> true
	; var(Dv), U=V, Du=Dv -> true
        ; var(Du), var(Dv) ->
            my_c_eq_unify__2(X, Y)
        ; var(Du) -> % nonvar(Dv)
	    xdic.ref(Y, Y2),
            my_c_eq_var_value(X, Y2)
        ; var(Dv) -> % nonvar(Du)
	    xdic.ref(X, X2),
            my_c_eq_var_value(Y, X2)
        ; xdic.ref(X, X2),
          xdic.ref(Y, Y2),
          my_c_eq_value_value(X2, Y2)
        ).

:- '$ctxprj'(my_c_eq_unify__2/2, [wcode]).
my_c_eq_unify__2(X, Y) :-
	J = ~my_var_d(X),
	J = ~my_var_d(Y),
	xvarmem(X), yvarmem2(Y, J),
	!,
	wcode.add(init(X,Y)).
my_c_eq_unify__2(X, Y) :-
	I = ~my_var_d(X),
	I = ~my_var_d(Y),
	yvarmem2(X, I), yvarmem2(Y, J),
	I < J, !,
	XT1 = ~my_var_new(_,x(_)),
	wcode.add(init(XT1,X)),
	wcode.add(move(XT1,Y)).
my_c_eq_unify__2(X, Y) :-
	my_c_eq_unify__2(Y, X).

:- '$ctxprj'(my_c_eq_var_value/2, [wcode]).
my_c_eq_var_value(X, Y) :-
	D = ~my_var_d(X),
	D = ~my_var_d(Y),
	sxvarmem(X), sxvarmem(Y), !, 
	XT1 = ~my_var_new(_,x(_)),
	wcode.add(init(XT1, X)),
	wcode.add(u_val(XT1, Y)).
% TODO: old code... bug in unif_move?
%	wcode.add(move(Y,X,g)).
my_c_eq_var_value(X, Y) :-
	D = ~my_var_d(X),
	D = ~my_var_d(Y),
	sxvarmem(X), yvarmem(Y), !,
	wcode.add(move(Y,X)).
my_c_eq_var_value(X, Y) :-
	g = ~my_var_d(X),
	g = ~my_var_d(Y),
	yvarmem(X), sxvarmem(Y), !,
	wcode.add(move(Y,X)).
my_c_eq_var_value(X, Y) :-
	I = ~my_var_d(X),
	yvarmem2(X, I),
	XT1 = ~my_var_new(_,x(_)),
	wcode.add(init(XT1, X)),
	wcode.add(u_val(XT1, Y)).

:- '$ctxprj'(my_c_eq_value_value/2, [wcode]).
my_c_eq_value_value(X, Y) :-
	sxvarmem(X), !,
	wcode.add(u_val(X,Y)).
my_c_eq_value_value(X, Y) :-
	ayvarmem(X), sxvarmem(Y), !,
	wcode.add(u_val(Y,X)).
my_c_eq_value_value(X, Y) :-
	ayvarmem(X), ayvarmem(Y), !,
	XT1 = ~my_var_new(_,x(_)),
	wcode.add(move(X,XT1)),
	wcode.add(u_val(XT1,Y)).

% Compile the body up to first general call.
:- '$ctxprj'(my_c_guards/4, [cscope, wcode, u(xdic)]).
my_c_guards(recursive(Size), [icall(G)|Gs], N0, N) :-
	% If the clause is recursive and G may use the heap, do a
	% 'neck' and 'cframe' (i.e., make sure that the choice point
	% and the frame are created)
	trust(G instance_of strgoal),
	~G.get_id = NA,
	ipred__uses_heap(NA), !,
	wcode.add(neck(N0)),
	wcode.add(cframe(Size)), % (Size of the frame, like for pcall(_,Size))
	my_c_icall(G),
	my_c_goals(Gs, N).
my_c_guards(Kind, [icall(G)|Gs], N0, N) :- !,
	my_c_icall(G),
	my_c_guards(Kind, Gs, N0, N).
my_c_guards(_, Gs, N0, N) :-
	wcode.add(neck(N0)),
	my_c_goals(Gs, N).

% Compile a tail of the body.
:- '$ctxprj'(my_c_goals/2, [cscope, wcode, u(xdic)]).
my_c_goals([], 0) :- !,
	wcode.add(~my_call(last, ~predicate_s.from_id('basiccontrol:true/0'), _Size, [])).
my_c_goals([icall(G)|Gs], N) :-
	my_c_icall(G),
	my_c_goals(Gs, N).
my_c_goals([G], N) :- !,
	my_c_goal(last, G, N).
my_c_goals([G|Gs], N) :-
	my_c_goal(nonlast, G, N),
	xdic <- _Dic, % clean the dic
	my_c_goals(Gs, _).

% Compile a body goal.
:- '$ctxprj'(my_c_goal/3, [cscope, wcode, u(xdic)]).
my_c_goal(Position, G, N) :-
	goal_args(G, PredId, N, Size, Args),
	trust(PredId instance_of predicate_s),
	As = ~my_headargs(Args),
	call((
          size :: any <- Size,
	  my_c_goal_args(Args, As)
        )),
	wcode.add(~my_call(Position, PredId, Size, As)),
	( Position = nonlast -> wcode.add(ensure_space(cont,0,_)) ; true ).

% TODO: SAME goal_args/5

% Head arguments unification.  Match args that are vas first.
% This seems to need fewer temporaries than the source sequence.
:- '$ctxprj'(my_c_head_args/1, [cscope, wcode, u(xdic)]).
my_c_head_args(Args) :-
	As = ~my_headargs_a(Args),
	my_head_arguments(Args, As, Arga),
	my_c_head_args_c(Arga).

:- '$ctxprj'(my_c_head_args_c/1, [cscope, wcode, u(xdic)]).
my_c_head_args_c([]).
my_c_head_args_c([ha(X,Y)|Xs]) :-
	my_c_get_arg(X, Y),
	my_c_head_args_c(Xs).

% TODO: xreg_cache.add is quadratic here!! because it adds consecutive entries to a binary dictionary
:- '$ctxprj'(my_head_arguments/3, [cscope, wcode, u(xdic)]).
my_head_arguments([], _, []).
my_head_arguments([Arg|Args], [A|As], S) :- D = ~my_var_d(Arg), var(D), !,
	call((
          wcode :: accum <- G0,
	  my_c_get_arg(Arg, A),
	  G = ~wcode
        )),
	my_head_arguments(Args, As, S),
	G0 = ~wcode, wcode <- G.
my_head_arguments([Arg|Args], [A|As], [ha(Arg,A)|S]) :-
	my_head_arguments(Args, As, S).
}.

% Goal arguments unification.  Put args that are vas last.
% This seems to need fewer temporaries than the source sequence.
{
:- fluid xdic :: xreg_cache.
:- fluid size :: any.
my_c_goal_args(Xs, As) :-
	% compile nonvar arguments
	my_goal_arguments_nonvar(Xs, As),
	% then compile var arguments
	my_goal_arguments_var(Xs, As).

my_goal_arguments_nonvar([], []).
my_goal_arguments_nonvar([Arg|Args], [_|As]) :-
	Arg instance_of termvar,
	!,
	my_goal_arguments_nonvar(Args, As).
my_goal_arguments_nonvar([Arg|Args], [A|As]) :-
	my_c_put_arg(Arg, A),
	my_goal_arguments_nonvar(Args, As).

my_goal_arguments_var([], _).
my_goal_arguments_var([Arg|Args], [A|As]) :-
	Arg instance_of termvar,
	!,
	my_c_put(Arg, A),
	my_goal_arguments_var(Args, As).
my_goal_arguments_var([_|Args], [_|As]) :-
	my_goal_arguments_var(Args, As).
}.

}.

% TODO: [MERGE]
% Variables for head/call arguments
% TODO: merge back into bytecode__compiler
% TODO: I want length of Args, not Args
my_headargs_a(Args) := ~my_headargs_a_(Args, 0).

my_headargs_a_([], _) := [].
my_headargs_a_([_|Args], I) := [A|As] :-
	A = ~my_headarg_a(I),
	I1 is I+1,
	As = ~my_headargs_a_(Args, I1).

% Variable for head/call arguments
my_headarg_a(I) := ~my_var_new(_,a(I)).

my_headargs([], []).
my_headargs([_|As], [~my_var_new(_,_) | ~my_headargs(As)]).

tmpargs([], []).
tmpargs([_|As], [~my_var_new(_,_) | ~tmpargs(As)]).

% TODO: [MERGE]
{
% Linearize term before emitting 'get' + 'put' + 'unify'.
:- fluid cscope :: scope.
:- fluid flatcode :: accum.
:- fluid gp :: any.
my_flat(S, V) :-
	c :: m_int <- 0, % TODO: or boolean?
	n :: any <- 1,
	my_flat_2(S, V).
{
:- fluid c :: m_int.
:- fluid n :: any.
my_flat_2(S, S1) :- S instance_of termvar, !, S1 = S.
my_flat_2(S, S1) :-
	trust(S instance_of termstr),
	S.get_functor(K, 0),
	!,
	( ~n = 0,
	  functorcons(K/0, FC),
	  \+ functorcons__single_cell(FC) ->
	    c <- 1,
	    ( ~gp = put -> G = g ; true ),
	    S1 = ~my_var_new(G,_), % S1 = ~my_var_new(G,x(_)),
	    flatcode.add(fla(S1,S))
	; S1 = S
	).
my_flat_2(S, S1) :- ~n = 0, !,
	c <- 1,
	S1 = ~my_var_new(G,_), % S1 = ~my_var_new(G,x(_)),
	( ~gp = get -> flatcode.add(fla(S1,S2)), my_flat(S, S2) % G is left unbound
	; ~gp = put -> G = g, my_flat(S, S2), flatcode.add(fla(S1,S2))
	; fail
	).
my_flat_2(S, S1) :-
	trust(S instance_of termstr),
	L = ~S.args,
	S1 = ~S.set_args(L1),
	C0 = ~c,
	c <- 0,
	my_flat_args(L, L1, C0).

my_flat_args([X|Xs], [X1|Xs1], C1) :-
	( Xs = [] ->
	    Xs1 = [],
	    C0 = ~c,
	    N1 is ~n - C0,
	    C2 is C0\/C1,
	    c <- C2,
	    call((
              n :: any <- N1,
	      my_flat_2(X, X1)
            ))
	; N1 is ~n - 1,
	  call((
            n :: any <- N1,
	    my_flat_2(X, X1)
          )),
	  my_flat_args(Xs, Xs1, C1)
	).
}.
}.

{
:- fluid cscope :: scope.
:- fluid wcode :: accum.
:- fluid xdic :: xreg_cache + u.
my_c_get_arg(X, XV) :- X instance_of termstr, !,
	call((
          gp :: any <- get,
	  flatcode :: accum(S),
	  my_flat(X, S1)
        )),
	my_c_get(S1, XV),
	my_c_get_xs(S).
my_c_get_arg(Arg, A) :-
	my_c_get(Arg, A).

% Using X registers as a cache for Y values and constants.
my_c_get_xs([]).
my_c_get_xs([fla(S,X)|R]) :-
	my_c_get(X, S),
	my_c_get_xs(R).

my_c_get(S, XV) :- D = ~my_var_d(S), !,
	( var(D) ->
	    movexg(XV, S)
	; xdic.ref(S, S2),
	  wcode.add(u_val(XV, S2))
	),
	xdic.add(XV, S).
my_c_get(S, XV) :-
	trust(S instance_of termstr),
	S.get_functor(K, 0),
	functorcons(K/0, FC), \+ FC = atom(_), !, % TODO: remove \+ atom!!!
	!,
	( is_blob(K) ->
	    wcode.add(u_blob(XV, K))
	; wcode.add(u_cons(XV, K))
	),
	xdic.add(XV, S).
my_c_get(S, XV) :-
	trust(S instance_of termstr),
	FunctorR = ~S.predid,
	Args = ~S.args,
	c_eq_traversal(FunctorR, Traversal),
        ( Traversal = postorder -> % TODO: use c_eq_traversal(postorder) (depending on FunctorR)
	    As = ~tmpargs(Args),
	    my_c_put_arg_builtin_xs(Args, As),
	    wcode.add(u_str_is(XV, FunctorR, As))
	; % Traversal = preorder,
	  SReg = ~my_var_new(_,x(_)), % (S register)
	  new_strargs(~length(Args), _, SReg, StrArgs),
	  wcode.add(u_str_os(XV, FunctorR, StrArgs, SReg)),
	  my_c_unify_args(StrArgs, Args, unknown_mode)
	). 

% Variables for the arguments of a structure
% new_strargs :: int * term * aterm_var * list(aterm_var)
new_strargs(N, D, Str, Xs) :-
	new_strargs_(0, N, D, Str, Xs).

new_strargs_(N, N, _D, _Str, []) :- !.
new_strargs_(I, N, D, Str, [StrArg|Xs]) :-
	StrArg = ~my_var_new(D,strmem(Str,I)),
	I1 is I + 1,
        new_strargs_(I1, N, D, Str, Xs).

% TODO: change name, similar to c_goal_args, but used for building
%       terms in postorder
my_c_put_arg_builtin_xs([], []).
my_c_put_arg_builtin_xs([X0|Xs0], [X|Xs]) :-
        my_c_put_arg_builtin(X0, X),
        my_c_put_arg_builtin_xs(Xs0, Xs).

my_c_put_arg_builtin(X, XV) :-
	% TODO: is it necessary to scope xdic?
        XDic = ~xdic,
	call((
          size :: any <- 1000,
	  xdic :: xreg_cache <- XDic,
	  my_c_put_arg(X, XV)
        )).
}.

% Compile loading a goal argument.
% First linearize goal argument.
{
:- fluid cscope :: scope.
:- fluid wcode :: accum.
:- fluid xdic :: xreg_cache.
:- fluid size :: any.
my_c_put_arg(X, XV) :- X instance_of termstr, !,
	call((
          gp :: any <- put,
	  flatcode :: accum(S),
	  my_flat(X, S1)
        )),
	my_c_put_xs(S),
	my_c_put(S1, XV).
my_c_put_arg(X, XV) :-
	my_c_put(X, XV).

my_c_put_xs([]).
my_c_put_xs([fla(S,X)|R]) :- my_c_put(X, S), my_c_put_xs(R).

my_c_put(S, XV) :- S instance_of termvar, !,
	D = ~my_var_d(S),
	( var(D) ->
	    ( xvarmem(S) -> D = g ; yvarmem2(S, D) ),
	    wcode.add(init(XV,S)),
	    D1 = D
	; xdic.ref(S, S2),
	  D1 = ~my_var_d(S2), 
	  c_put_value(S2, XV)
	),
	xdic.store(XV, ~my_var_new(D1,~my_var_mem(S))).
my_c_put(S, XV) :-
	trust(S instance_of termstr),
	S.get_functor(K, 0),
	functorcons(K/0, FC), \+ FC = atom(_), !, % TODO: remove \+ atom!!!
	( xdic.search(S, XN) ->
	    wcode.add(move(~my_var_new(_,XN), XV, p))
	; is_blob(K) ->
	    wcode.add(ld_blob(XV, K))
	; wcode.add(ld_cons(XV, K))
	),
	xdic.store(XV, S). 
my_c_put(S, XV) :-
	trust(S instance_of termstr),
	FunctorR = ~S.predid,
	Args = ~S.args,
	c_eq_traversal(FunctorR, Traversal),
        ( Traversal = postorder -> % TODO: use c_eq_traversal(postorder) (depending on FunctorR)
	    As = ~tmpargs(Args),
	    my_c_put_arg_builtin_xs(Args, As),
	    wcode.add(ld_str_is(XV, FunctorR, As)),
	    xdic.clear(~my_var_mem(XV))
	; % Traversal = preorder,
	  % TODO: bad implementation!
	  SReg = ~my_var_new(_,x(_)), % (S register)
	  new_strargs(~length(Args), _, SReg, StrArgs),
	  wcode.add(ld_str_os(XV, FunctorR, StrArgs, SReg)),
%	  xdic.clear(~my_var_mem(XV)), % TODO: we need the mem for strargs, do not clear
	  my_c_unify_args(StrArgs, Args, write_mode)
	). 
}.

% TODO: use?
% mark_out_arg(X, Y) :-	Y = ~my_var_new(g,~my_var_mem(X)).

{
:- fluid cscope :: scope.
:- fluid xdic :: xreg_cache.
:- fluid wcode :: accum.
:- '$ctxprj'(my_c_unify_args/3, [cscope, wcode, u(xdic)]).
% TODO: using un_* instructions
my_c_unify_args([], [], _).
my_c_unify_args([StrArg|StrArgs], [X|Xs], Mode) :-
	my_c_u_arg(X, StrArg, Mode),
	my_c_unify_args(StrArgs, Xs, Mode).

% TODO: rename: value=nonfresh; var=fresh (or use 'init, uninit', etc.)
my_c_u_arg(X, StrArg0, Mode) :-
	( Mode = write_mode ->
	    StrArg = StrArg0
	; StrArg = ~my_var_new(g,~my_var_mem(StrArg0))
	),
	my_c_u_arg_(X, StrArg, Mode).

my_c_u_arg_(X, StrArg, Mode) :- X instance_of termvar, !,
	D = ~my_var_d(X),
	( var(D) ->
	    D = g,
	    ( Mode = write_mode ->
		wcode.add(init(StrArg,X))
	    ; Mode = read_mode ->
	        U = ~my_var_mem(X),
		V = ~my_var_mem(StrArg),
		( U = V -> true % coalesce variables % TODO: wrong, use liveness
		; wcode.add(move(StrArg,X))
		)
	    ; U = ~my_var_mem(X),
	      V = ~my_var_mem(StrArg),
	      ( U = V -> true % coalesce variables % TODO: wrong, use liveness
	      ; wcode.add(move(StrArg,X)) % TODO: this is un_var(~i, S)
	      )
	    )
	; xdic.ref(X, X2),
	  ( Mode = write_mode ->
	      movexp(X2, StrArg)
	  ; Mode = read_mode ->
	      wcode.add(u_val(StrArg,X2))
	  ; wcode.add(un_val(StrArg,X2)) % TODO: this is c_unify__un_val(X2)
	  )
	).
my_c_u_arg_(S, StrArg, Mode) :-
	trust(S instance_of termstr),
	S.get_functor(K, 0),
	functorcons(K/0, FC), \+ FC = atom(_), % TODO: remove \+ atom!!!
	!,
	( is_blob(K) ->
	    ( Mode = write_mode ->
	        wcode.add(ld_blob(StrArg, K))
	    ; Mode = read_mode ->
	        wcode.add(u_blob(StrArg, K))
	    ; wcode.add(un_blob(StrArg, K)) % TODO: this is un_blob(~i, K)
	    )
	; ( Mode = write_mode ->
	      wcode.add(ld_cons(StrArg, K))
	  ; Mode = read_mode ->
	      wcode.add(u_cons(StrArg, K))
	  ; wcode.add(un_cons(StrArg, K)) % TODO: this is un_cons(~i, K)
	  )
	).
my_c_u_arg_(S, StrArg, Mode) :-
	trust(S instance_of termstr),
	FunctorR = ~S.predid,
	Args = ~S.args,
	c_eq_traversal(FunctorR, Traversal),
        ( Traversal = postorder -> % TODO: use c_eq_traversal(postorder) (depending on FunctorR)
	    As = ~tmpargs(Args),
	    my_c_put_arg_builtin_xs(Args, As),
	    ( Mode = write_mode ->
	        wcode.add(ld_str_is(StrArg, FunctorR, As))
	    ; Mode = read_mode ->
	        wcode.add(u_str_is(StrArg, FunctorR, As))
	    ; wcode.add(un_str_is(StrArg, FunctorR, As)) % TODO: this is un_str_is(~i, FunctorR, As)
	    )
	; % Traversal = preorder,
	  SReg = ~my_var_new(_,x(_)), % (S register)
	  new_strargs(~length(Args), _, SReg, StrArgs),
	  ( Mode = write_mode ->
	      wcode.add(ld_str_os(StrArg, FunctorR, StrArgs, SReg)),
	      NextMode = write_mode
	  ; Mode = read_mode ->
	      wcode.add(u_str_os(StrArg, FunctorR, StrArgs, SReg)),
	      NextMode = unknown_mode % TODO: type analysis is required to infer anything else
	  ; wcode.add(un_str_os(StrArg, FunctorR, StrArgs, SReg)), % TODO: this is un_str_os(~i, FunctorR, As)
	    NextMode = unknown_mode % TODO: type analysis is required to infer anything else
	  ),
	  my_c_unify_args(StrArgs, Args, NextMode)
	). 
}.

% -----
% Pre-allocation of temporal variables for head arguments.
%
% This assigns to each head argument an a(_) register. This is
% implemented to avoid using copies of arguments.
%
% TODO: probably, I can remove this by using the 'xdic'
% TODO: merge back into bytecode__compiler?
prealloc_args(Args) :- prealloc_args_(Args, 0).

prealloc_args_([], _).
prealloc_args_([Arg|Args], I) :-
	( Arg instance_of termvar,
	  Mem = ~my_var_mem(Arg),
	  var(Mem) -> % (it may appear twice)
	    a(I) = Mem
	; true
	),
	I1 is I+1,
	prealloc_args_(Args, I1).
% -----

% TODO: SAME xreg_cache

% TODO: SAME my_c_put_value/2
% TODO: SAME movexp/2
% TODO: SAME movexg/2

% TODO: MISSING guard_lifetime/3
% TODO: MISSING body_lifetime/1

% ---------------------------------------------------------------------------

% TODO: unification of clauses, cuts, etc.? \texttt{p /
%     point :- !, ...} should be translated to \texttt{p(point(...), Z)
%     :- !, Z = point(...), ...}}

% TODO: what is the equivalent of head unification flattening for contexts?
% E.g.
%  move/point :- inc(x)
%  move/P :- point <- P, inc(x).
%  move/P :- ^point(x,y) <- P, inc(x).

% ===========================================================================

extract_metargs(C0, MetArgs, C) :-
	( C0 = [icall(G)|C],
	  trust(G instance_of strgoal),
	  ~G.get_id == '$metargs/1' ->
	    [MetArgs] = ~G.args
	; C0 = C, MetArgs = []
	).

insert_metargs(C0, MetArgs, C) :-
	C = ['$metargs'(MetArgs)|C0].

my_peep_clause(C, Frame, NF) := Cs2_2 :-
	needs_frame :: any <- NF,
	call(( code :: accum(Cs_1), my_peep_clause_(C) )),
	( ~needs_frame = yes ->
	    Cs2_2 = [alloc(Frame)|Cs_1]
	; Cs2_2 = Cs_1
	).

{
:- fluid needs_frame :: any.
:- fluid code :: accum.

% note: ;/2 is compiled without auxiliar predicates:
%       - a single frame is shared
% note: this code makes use of unification of logic variables to
%       fill references in continuations
my_peep_clause_([]) :- !,
	code.add(proceed).
my_peep_clause_([X]) :- !,
	( is_lastcall_true(X) ->
	    maybe_dealloc,
	    code.add(proceed)
        ; ins_use_success_cont(X) ->
	    maybe_dealloc,
	    code.add(X)
	; code.add(X),
	  % Add a last proceed if the instruction does not use continuations
	  maybe_dealloc,
	  code.add(proceed)
	).
my_peep_clause_([ensure_space(_,_,_)|Xs]) :- !,
	my_peep_clause_(Xs).
my_peep_clause_([X|Xs]) :- ins_use_success_cont(X), !,
	code.add(X),
	my_peep_clause_(Xs).
my_peep_clause_([I|Xs]) :-
	( I = move(X,Y)
	; I = move(X,Y,_)
	),
	X instance_of termvar,
	Y instance_of termvar,
	MemX = ~my_var_mem(X),
	MemY = ~my_var_mem(Y),
	MemX == MemY,
	!,
	my_peep_clause_(Xs).
my_peep_clause_([X|Xs]) :- !,
        code.add(X),
	my_peep_clause_(Xs).

maybe_dealloc :-
        ( ~needs_frame = yes -> code.add(dealloc) ; true ).
}.

is_lastcall_true(X) :-
	is_lastcall(X),
	PredId = ~call_predid(X),
	trust(PredId instance_of predicate_s),
	~PredId.get_id = 'basiccontrol:true/0'.

ins_use_success_cont(X) :-
	is_call(X), % TODO: make sure that we do not insert frames if only lastcalls are used
	PredId = ~call_predid(X),
	trust(PredId instance_of psymbol),
	~PredId.get_det_mode = nondet.

% ---------------------------------------------------------------------------
% post processing and collapsing of WAM code

my_postcomp. % TODO: empty

% ===========================================================================

:- include(compiler(bytecode__compiler_common)).

{
:- fluid middefs :: accum.
compile0(Scope, MetR, Args0, Cs0) :-
	cscope :: scope <- Scope,
	%
	Cs1 = ~mexpnorm_clauses(Cs0), % TODO: move outwards
	call(( goals :: accum(_), Args = ~aterm__args(Args0) )), % (goals must be empty)
	%
	Cs2 = ~postprocess_plwam(~my_compile_clauses(Cs1, Args)),
	PLWAMCs2 = ~postprocess_plwam(~compile_clauses(Cs1, Args)),
	debug_compare_index(Cs2, PLWAMCs2),
	% TODO: MetArgsP mem is not synchronized with Cs2 (done in renamevars in compile_clauses)
	middefs.add(bcode_a(MetR, Cs2, PLWAMCs2)).
}.

% ===========================================================================

% (implemented in module_ipexp)
% TODO: define properly

% X is a float
is_float(X) :- float(X).

is_blob(_) :- fail.

%:- public typemask_of_functor/2.
typemask_of_functor(K/0,   2'00010) :- number(K), !.
typemask_of_functor(K/0,   2'00010) :- is_string(K), !. % TODO: bad bits
typemask_of_functor(K/0,   2'00100) :- atom(K), !.
%typemask_of_functor('.'/2, 2'01000) :- get__use_opt(optlst), !.
typemask_of_functor(_,     2'10000) :- !.

% Data operations to move from Prolog to Absmach domains (and viceversa)

%:- public functorcons/2.
functorcons(Constant/0) := Constant1 :- !,
	Constant1 = ( is_float(Constant) ? float(Constant)
%		    | is_bignum(Constant) ? bignum(Constant)
		    | number(Constant) ? smallint(Constant)
		    | is_string(Constant) ? string(Constant)
		    | atom(Constant)
		    ).
%functorcons('.'/2) := list :- get__use_opt(optlst), !.
functorcons(Functor) := str(Functor).

% TODO: for the JS-backend, what to do here?
% :- public static functorcons__single_cell/1.
functorcons__single_cell(smallint(_)).
functorcons__single_cell(atom(_)).

% TODO: complete
ipred__uses_heap(_) :- fail. % which icall use heap?

% TODO: complete
is_ipred(PredId) :-
	PredId instance_of predicate_s,
	( is_ipred_(~PredId.get_id) -> true
	; PredId.get_prop(basal_builtin(_, _, _)) -> true
	; PredId.get_prop(specialdef([_], classtest(_)))
	).

is_ipred_('$metargs/1') :- !. % TODO: keep or remove?
is_ipred_('basiccontrol:$caller_choice/1') :- !.
is_ipred_('basiccontrol:$cut/1') :- !. 
is_ipred_('term_basic:=/2') :- !.
is_ipred_('$trail_set_attr/3') :- !.
is_ipred_('basiccontrol:$get_choice/1') :- !.

% TODO: complete, get from the predicate property
ipred__imp(PredId) := Det :-
	trust(PredId instance_of psymbol),
        ( Det = ~ipred__imp_(~PredId.get_det_mode) -> true
	; throw(bad_det(PredId))
	).

ipred__imp_(detfun) := det.
ipred__imp_(semidet) := semidet.

trust_domain(_, _) :- fail.

% Note: maybe useful for JS compilation...
% use_opt(uninit_y) # "allow frames with uninitialized variables".
use_opt(native_string). % TODO: make it optional
use_opt(_) :- fail. % default case

% ---------------------------------------------------------------------------
% Postprocess plwam output for the JS backend

% DEBUG!
{
:- fluid cscope :: scope.
debug_compare_index([], []).
debug_compare_index([C0|Cs], [C1|Cs2]) :-
	C0 = ma_clause(_,_,Data0),
	C1 = clause(_,Data10), Data1 = ~debug_no_ecm(Data10), % TODO: explicit_cut is added in clause life time (which is still not working)
	( Data0 = Data1 -> true
	; N = ~cscope.currmodule,
	  display(user_error, index_mismatch(N, ~idxdata_term(Data0), ~idxdata_term(Data1))), nl(user_error)
%	  display(user_error, c0(C0)), nl,
%	  display(user_error, c1(C1)), nl
	),
	debug_compare_index(Cs, Cs2).
}.

debug_no_ecm(f(T0,D),f(T,D)) :-
	T is T0 /\ 2'0111111.

postprocess_plwam([], []).
postprocess_plwam([C0|Cs], [C1|Cs2]) :-
	postprocess_c(C0, C1),
	postprocess_plwam(Cs, Cs2).

postprocess_c(ma_clause(MetArgsP, Code, Data), ma_clause(MetArgsP, Code2, Data)) :- % 
	deep_code(Code, Code1),
	call(( goals :: accum(Code2), postprocess_insns(Code1) )).
postprocess_c(clause(Code, Data), clause(Code2, Data)) :- % PL2WAM
	deep_code(Code, Code1),
	call(( goals :: accum(Code2), postprocess_insns(Code1) )).

% Get the code for deep backtracking, discard shallow.
% TODO: This is temporal until we compile shallow backtracking

% From:
%   cjump(shallow,S)+Deep+jump(C)+label(S)+Shallow+label(C)+Common
% obtain:
%   Deep+Common

deep_code([cjump(shallow,S)|Rest0], Xs) :-
	append(Deep, [jump(C),label(S0)|Rest1], Rest0), S == S0,
	append(_Shallow, [label(C0)|Common], Rest1), C == C0,
	!,
	append(Deep, Common, Xs).
deep_code([cjump(shallow,S)|Rest0], Xs) :- % no common part
	append(Deep, [label(S0)|Rest1], Rest0), S == S0,
	append(_Shallow, [label(_)], Rest1),
	!,
	Xs = Deep.
deep_code([cjump(shallow,S)|Rest0], Xs) :- % no shallow part
	append(Deep, [label(S0)|Rest1], Rest0), S == S0,
	!,
	append(Deep, Rest1, Xs).
deep_code(Xs, Xs).

{
:- fluid goals :: accum.

postprocess_insns([]).
postprocess_insns([X|Xs]) :-
	postprocess_ins(X),
	postprocess_insns(Xs).

postprocess_ins(move(X,Y,_)) :- !,
	postprocess_ins(move(X,Y)).
postprocess_ins(init2h(X,Y)) :- !,
	postprocess_ins(inith(X)),
	postprocess_ins(move(X,Y)).
postprocess_ins(init2s(X,Y)) :- !,
	postprocess_ins(inits(X)),
	postprocess_ins(move(X,Y)).
postprocess_ins(inits(X)) :- !,
	postprocess_ins(inith(X)). % no stack variables
postprocess_ins(un_lval(I, X)) :- !,
	postprocess_ins(un_val(I, X)). % no stack variables, thus no local vars
postprocess_ins(globunsafe(X, Y)) :- !,
	postprocess_ins(move(X, Y)). % no stack variables, thus no local vars
postprocess_ins(X) :- goals.add(X).
}.
	
% ---------------------------------------------------------------------------

% (For debugging only)
idxdata_term(f(Type,Key), Term) :-
	idxbits_to_list(Type, List),
	key_to_hash(Key, Hash),
	Term = index0(List, Hash).
	
key_to_hash(Key, nohash) :- var(Key), !.
key_to_hash(Key, hash(Key)).

idxbit_name(2'0000001, var).
idxbit_name(2'0000010, number).
idxbit_name(2'0000100, atom).
idxbit_name(2'0001000, list).
idxbit_name(2'0010000, structure).
idxbit_name(2'0100000, implicit_cut).
idxbit_name(2'1000000, explicit_cut).

idxbits_to_list(Type, List) :-
	List = ~findall(Name, idxbit_enabled(Type, Name)).

idxbit_enabled(Type, Name) :-
	idxbit_name(Bit, Name),
	0 =\= Bit /\ Type.

