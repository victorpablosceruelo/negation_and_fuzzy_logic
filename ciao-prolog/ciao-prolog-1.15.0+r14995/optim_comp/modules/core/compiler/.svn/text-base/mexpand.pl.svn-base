% ---------------------------------------------------------------------------
:- doc(section, "Meta-expansion of predicates").

%   This compilation pass transforms modular programs (predicates and
%   goals) into plain programs (with a flatten name-space).
%
%   Additionally, it treats basic flattening of predicates using
%   functional notation, expansion of meta-arguments, and it supports
%   other user-definable goal transformations.
%
%   Authors: the CLIP group and Jose F. Morales 

% ---------------------------------------------------------------------------

% Note: this code generates code both for the compilation-time
%   ('compile' part of compiler/frontend.pl) and run-time (rt_exp.pl)
%   parts of the compiler. The later is enabled with
%   'mexpand_runtime_db' (using the 'condcode' package). When the
%   compile-time does not have enough information to perform the code
%   expansion, it emits calls to the corresponding entries in the
%   run-time code.

% ---------------------------------------------------------------------------
:- doc(subsection, "Extended symbol resolution rules").

% When no qualifier is specified, predicates are searched in this order:
%
%   - pEnv predicates (at this moment only fluid variables, unary
%     predicates whose state is mapped to implicit arguments)
%
%   - class attributes and methods (accessible from ~'$self')
%
%   - locally defined predicates 
%
%   - imported predicates
%
% See flatten_resolve_one/4 with Where=qual_none for more details.
%
% When a qualifier (q1.q2...qn) is specified, resolution of 'q1' follows
% the same rules.

% TODO: Write the documentation in the right place
% TODO: Write tests for resolution, visibility, hiord, etc.
% TODO: Write visibility rules

% TODO: For the sake of reducing the engine size, can this file be
%       (easily) divided in compile-time/run-time parts?

% TODO: This file should be an abstract class or mixin. Then it could
%       use lpdoc-style documentation easily.
% TODO: Complete the list of input predicates (holes) for this file:
%  mexpand__goal_trans/2 - MODULE defines goal translation G
%  mexpand__error/1 ...
%
%  (see mexpand__lookup for more inputs)

% TODO: Use the rule (x.method ==> (~x).method if x/1 is ANY predicate)? I had some problems with it

% TODO: Clean the syntax and document new features (OUTDATED):
%   e.g.
%   '$:'(X): wrapper for boxed metaterms
%
%   '$ctx_value'(Name, Value) :
%   '$ctx_set'(...) :
%   '$meta_exp'(Metatype, P, E) : expands P w.r.t. type Metatype and unify with E (possibly at compile-time)
%   '$trust_metatype'(Variable, Metatype)
%   :- '$context' sets the context of a predicate 

:- use_module(engine(hiord_rt), ['$meta_call'/1]).
:- use_module(compiler(meta_syntax), [funcall/2, mcall/3]).
:- use_module(compiler(meta_syntax), [sym_to_spec/2, decomp_goal/3]).
:- use_module(compiler(meta_syntax), [get_qualifier/3, apply_qual/3]).
:- use_module(compiler(list_common),
	[conj_to_slist/2, list_to_conj/2,
	 mconj_to_slist/2, list_to_mconj/2,
	 dotpath_to_list/2, list_to_dotpath/2]).

% ---------------------------------------------------------------------------
% Residuate a list of expanded goals (partial evaluation)

{
:- fluid residue :: accum.
residue_concat([]).
residue_concat([G|Gs]) :- residue.add(G), residue_concat(Gs).
}.

% ---------------------------------------------------------------------------
% Expand a clause (EXPORTED)
% +det

mexpand__clause(H0, B0, Module, H2, B3) :-
	( mexpand__option(functional_expand) ->
	    defunc_clause_head(H0, B0, H1, B1)
	; H1 = H0, B1 = B0
	),
        call((
          % TODO: (use some mexpscope constructor)
          vEnv :: v_env <- ~v_env.empty,
	  pEnv :: p_env,
	  m :: any <- Module,
	  %
	  qualm :: any <- qual_m(Module),
	  ignorefun :: any <- false,
	  %
	  mexpand__head(H1, H2, FinalPEnv),
	  PEnv = ~pEnv,
	  mexpand__body(B1, B2),
	  OutPEnv = ~pEnv,
	  p_env__finish(PEnv, OutPEnv, FinalPEnv, B2, B3)
        )).

% ---------------------------------------------------------------------------
% Expand a head
% +det

% TODO: rename scope by 'environment'?
% Scope for symbol lookup (including module resolution and meta-expansion)
:- public mixin mexpscope { % (this is indirectly exported)
    :- fluid vEnv :: v_env. % env. for variables
    :- fluid pEnv :: p_env. % env. for locally defined pred. symbols (fluid)
    :- fluid m :: any.
}.

:- public mixin mexpscope_u { % (this is indirectly exported)
    % Like mexpscope, but pEnv is 'readonly'
    :- fluid vEnv :: v_env.
    :- fluid pEnv :: p_env + u.
    :- fluid m :: any.
}.

% TODO: expansion of head args have been disabled
{
:- extends mexpscope.
:- fluid qualm :: any.
% mexpand__head('$:'(H), H, FinalPEnv) :- ... % TODO: remove, do not seem to be used anywhere
mexpand__head(H, H2, FinalPEnv) :-
	functor(H, F, A),
	H =.. [_|Args0],
	~pEnv = ~p_env.simple,
	call(( residue :: accum(HG), flatten_qual_and_resolve(~qualm, sympred(F, A), FnObj) )),
	HG = [], % TODO: It is a bug if the following unifications fail
	%
	FnObj = ri_fnc_m(FunctorR, PEnvSig, Metatypes, _),
	vEnv.init(Metatypes, Args0),
	mexpand__ctx_begin(PEnvSig),
	PEnv = ~pEnv,
	call((
          pEnv::p_env <- PEnv,
	  mexpand__ctx_connect(PEnvSig, Args0, Args), FinalPEnv = ~pEnv
        )),
	H2 = ~pterm__goal(FunctorR, Args).
}.

% ---------------------------------------------------------------------------

% metacast<_> of ctx_scope and body0, where the residual code is a mconj
{
:- extends mexpscope.
:- fluid ignorefun :: any.

mexpand__body(A, NA) :- % expand a body
	call(( residue :: accum(NA0), mexpand__ctx_scope(A) )),
	list_to_mconj(NA0, NA).

% TODO: use only mexpand__body?
mexpand__body1(A, NA) :- % expand a body
	call(( residue :: accum(NA0), mexpand__body0(A) )),
	list_to_mconj(NA0, NA).
}.

% ---------------------------------------------------------------------------

% (auxiliary)
mexpand__check_list(A) :- var(A), !, fail.
mexpand__check_list([]).
mexpand__check_list([_|Xs]) :- mexpand__check_list(Xs).

% ---------------------------------------------------------------------------

% partial evaluation of metacast<_>
{
:- extends mexpscope_u.
:- fluid residue :: accum.
% :- meta_predicate 'peval metacast<_>'(module_x, ?, ?).
'peval metacast<_>'('?', X, X) :- !.
'peval metacast<_>'(Class, X, NX) :-
	Class = list(Class2), mexpand__check_list(X), !,
	% TODO: remember metatype in vEnv so that we solve the O(n^2) complexity problem on some meta_predicates
	'peval metacast<list(_)>'(X, Class2, NX).
'peval metacast<_>'(Class, X, NX) :- var(X), !, % Maybe runtime
	% TODO: if metatypes are incompatible, there must be an error
	Metacast = ~vEnv.match_any(X, Class),
	'peval metacast2'(Metacast, X, NX).
'peval metacast<_>'(Class0, C0, E) :-
	( Class0 = primitive(Class) ->
	    E = E0
	; Class = Class0,
	  E = '$:'(E0)
	),
	'peval metacast<prim>'(Class, C0, E0).

% prim ::= clause | spec | fact | pred(_,_)
'peval metacast<prim>'(Class, C0, E0) :-
	( Class = clause ->
	    'peval metacast<clause>'(C0, E0)
	; Class = spec ->
	    'peval metacast<spec>'(C0, E0)
	; Class = fact ->
	    'peval metacast<fact>'(C0, E0)
	; Class = pred(_,_) ->
	    'peval metacast<pred(_,_)>'(C0, Class, E0)
	; mexpand__error(bad_metatype(Class))
	).
}.

{
:- fluid vEnv :: v_env.
% Get the class for a variable
% :- meta_predicate get_val_class(_,module_x,out(module_x)).
get_val_class(Val, DefaultClass) := Class :-
	( Class0 = ~vEnv.get(Val) ->
	    Class = Class0
	; % TODO: trace and debug this... allow methods even for ctxvars that have no metatype?
	  Class = DefaultClass
	).
}.

% ---------------------------------------------------------------------------

% partial evaluation of metacast<clause>
% Expand a clause
% TODO: missing treatment of 'blt__m_any' context vars
% TODO: name clash with other mexpand__clause
% TODO: can we unify this with mexpand__clause/? ?
% +det, if precondition holds
{
:- fluid pEnv :: p_env + u.
:- fluid m :: any.
'peval metacast<clause>'((H0:-B), (NH:-NB)) :- !,
	nonvar(H0),
	'peval metacast<fact>'(H0, NH),	
	call((
          vEnv :: v_env <- ~v_env.empty,
	  ignorefun :: any <- false,
	  mexpand__body(B, NB)
        )).
'peval metacast<clause>'(H0, NH) :-
	'peval metacast<fact>'(H0, NH).
}.

% ---------------------------------------------------------------------------

% partial evaluation of metacast<spec>
{
:- fluid pEnv :: p_env + u.
:- fluid m :: any.
'peval metacast<spec>'(C0, E0) :-
	% TODO: missing treatment of 'blt__m_any' context vars
	% TODO: missing module qualification in specs?
	get_qualifier(C0, Qual, C), % TODO: Wrong, this is a spec...
	call((
          qualm :: any <- Qual,
	  mexpand__spec(C, E0)
        )).
}.

{
:- fluid m :: any.
:- fluid qualm :: any.
mexpand__spec(F/A, F2/A2) :-
	atom(F), integer(A),
	% TODO: you can do it better... without creating a structure
	functor(H, F, A),
        call((
          vEnv :: v_env <- ~v_env.empty,
	  pEnv :: p_env,
	  mexpand__head(H, H2, _FinalPEnv)
        )),
	functor(H2, F2, A2).
}.

% ---------------------------------------------------------------------------

% partial evaluation of metacast<pred(_,_)>

% TODO: extend to contain nested modules at the predicate level
%   E.g. X = { a(1). b(2). }, X.a(1).

% +det, if precondition holds
% TODO: the head is not expanded?
% TODO: use OutPEnv, at least to give an error if some var is changed
{
:- extends mexpscope_u.
% a predicate abstraction
% TODO: missing propagation of metatypes of shared variables
'peval metacast<pred(_,_)>'({H :- B}, Metatype, Term) :- !,
%	mexpand__trace(predabs1(H,B)),
	mexpand__uses_hiord,
	mexpand__pa__clause(H, B, Metatype, Term).
'peval metacast<pred(_,_)>'((H :- B), Metatype, Term) :- !,
%	mexpand__trace(predabs2(H,B)),
	mexpand__uses_hiord,
        mexpand__pa__clause(H, B, Metatype, Term).
% a goal with complete argument list
'peval metacast<pred(_,_)>'(P, pred(0, []), NP) :- !,
%	mexpand__trace(predabs3(P)),
	call((
          ignorefun :: any <- false,
	  residue :: accum(NP0),
	  mexpand__body0(P) % TODO: should this be mexpand__body?
        )),
	NP = ~list_to_mconj(NP0),
	mexpand__anot_hiord(NP).
% a goal as a predicate abstraction (with incomplete argument list)
% note: that creates a new predicate (H:-G) stored in a term, that links missing arguments and shared variables with the actual call
% note: for higher-order terms, all variables are shared
% TODO: WRONG! do not set P as shared variables, instead calculate the variables common for P and the rest of the clause
'peval metacast<pred(_,_)>'(P0, pred(N, PEnvSig), NP) :-
	get_qualifier(P0, Qual, P),
%	mexpand__trace(predabs4(P)), 
	mexpand__missing_args(P, N, H, G),
	PEnv0 = ~pEnv,
	call((
	  pEnv :: p_env <- PEnv0,
	  mexpand__ctx_begin(PEnvSig),
	  %
	  ( PEnvSig = [] -> H2 = H, FinalPEnv = ~pEnv
	  ; H =.. [_|Args0],
            PEnv1 = ~pEnv,
	    call((
              pEnv :: p_env <- PEnv1,
	      mexpand__ctx_connect(PEnvSig, Args0, Args),
	      FinalPEnv = ~pEnv
            )),
	    H2 =.. [''|Args]
	  ),
	  % module expand G
          PEnv = ~pEnv,
	  % TODO: it should be like goal0, but only expanding the available arguments, not all of them!
	  call((
            ignorefun :: any <- false,
	    qual :: any <- Qual,
	    residue :: accum(NG00),
	    mexpand__goal0(G)
          )),
	  ( NG00 = [NG0] -> true
	  ; mexpand__error(bug(pa_required_expansion_check_todo))
	  ),
	  OutPEnv = ~pEnv,
	  p_env__finish(PEnv, OutPEnv, FinalPEnv, NG0, NG),
	  % TODO: optimize! add to the shared variables the context variables that are not being expanded
	  NG0 =.. [_|AllArgs],
	  ShCtx = ~p_env__shared(PEnv0, AllArgs),
%	  display(user_error, shared_pEnv(ShCtx)), nl(user_error),
	  ShVs = ( ShCtx = [] ? P | [P|ShCtx] ), % TODO: improve!
	  %
	  NP = 'PA'(ShVs,H2,NG),
	  mexpand__anot_hiord(NP)
        )).

mexpand__pa__clause(Hh, B, pred(N, _), 'PA'(ShVs,H,NB)) :-
	mexpand__head_and_shvs(Hh, H, ShVs),
	% TODO: missing propagation of metatypes of shared variables
	% TODO: missing context expansion of H and B!
	mexpand__check_pred(H, N, {Hh :- B}),
	PEnv = ~pEnv,
	call((
          vEnv :: v_env <- ~v_env.empty,
	  pEnv :: p_env <- PEnv,
	  ignorefun :: any <- false,
	  mexpand__body(B, NB)
        )).
}.

% Annotate goals that can be called from arbitrary places (through hiord calls)
% TODO: improve precision and performance by generating new predicates for complex goals (those that does not contain single goals)
% mexpand__anot_hiord(X) :: expanded_goal
mexpand__anot_hiord(X) :- var(X), !.
mexpand__anot_hiord('PA'(_, _, X)) :- !,
	mexpand__anot_hiord(X).
mexpand__anot_hiord('basiccontrol:fail') :- !.
mexpand__anot_hiord('basiccontrol:false') :- !.
mexpand__anot_hiord('basiccontrol:otherwise') :- !.
mexpand__anot_hiord('basiccontrol:true') :- !.
mexpand__anot_hiord('basiccontrol:!') :- !.
mexpand__anot_hiord('basiccontrol:;'(X, Y)) :- !,
	mexpand__anot_hiord(X),
	mexpand__anot_hiord(Y).
mexpand__anot_hiord('basiccontrol:->'(X, Y)) :- !,
	mexpand__anot_hiord(X),
	mexpand__anot_hiord(Y).
mexpand__anot_hiord('basiccontrol:\\+'(X)) :- !,
	mexpand__anot_hiord(X).
mexpand__anot_hiord('basiccontrol:,'(X, Y)) :- !,
	mexpand__anot_hiord(X),
	mexpand__anot_hiord(Y).
mexpand__anot_hiord('basiccontrol:if'(X, Y, Z)) :- !,
	mexpand__anot_hiord(X),
	mexpand__anot_hiord(Y),
	mexpand__anot_hiord(Z).
mexpand__anot_hiord(NP) :-
	functor(NP, N, A),
	mexpand__uses_hiord_pred(N, A).
%	display(user_error, anothiord(NP)), nl(user_error).

%% % TEMP==============
:- use_module(engine(io_basic)).
%% mexpand__trace(A) :-
%% 	display(user_error, A), nl(user_error).
%% % TEMP==============

% Given P (arity A), create G (arity A+N) and H goal (arity N), where H contains the N missing arguments,
% equivalent to the following pseudocode:
%   P = F(P1,P2,P3,...Pa)
%   H = ''(H1,H2,H3,...Hn)
%   G = F(H1,P1,P2,...,Pa,H2,H3,...,Hn)
%
% If N is 0, then G = P.
mexpand__missing_args(P, 0, H, G) :- !, % (special case)
	H = '',
	G = P.
mexpand__missing_args(P, N, H, G) :-
	% decompose input goal P (of arity A)
        nonvar(P), functor(P, F, A), atom(F),
	% create anonymous goal H (to store variables for missing N arguments)
        functor(H,'',N), % H: ''(H1,H2,H3,...Hn)
	% create new goal G with N+A arity
        T is N+A,
        functor(G, F, T), % G: F(G1,G2,G3,...Gan)
	%
	% Obtain G: F(H1,P1,P2,...,Pa,H2,H3,...,Hn)
	% (unify arguments 1..A of P with 2..A+1 of G)
	% (P1 = G2, P2 = G3, ..., Pa = Ga1)
        mexpand__unify_args(1, A, P, 2, G), % Unify pred args skipping first
	% (unify argument 1 of H with argument 1 of G)
	% (H1 = G1)
        arg(1,H,I),
        arg(1,G,I),
	% (unify arguments 2..N of H with (A+2)..(A+N) of G)
	% (H2 = Ga2, H3 = Ga3, ... Hn = Gan)
        A2 is A+2,
        mexpand__unify_args(2, N, H, A2, G).

mexpand__head_and_shvs((ShVs-> H), H, ShVs) :- !.
mexpand__head_and_shvs(H, H, []).

mexpand__check_pred(H, N, PredAbs) :-
        functor(H, F, A),
        ( F = '', ! ; mexpand__error(bad_pred_abs(PredAbs)) ),
        compare(R,A,N),
        ( R = (=) -> true
        ; R = (<) ->
            mexpand__error(short_pred_abs(PredAbs, N))
        ;%R = (>) ->
            mexpand__error(big_pred_abs(PredAbs, N))
        ).

mexpand__unify_args(I, N, _F, _A, _G) :- I > N, !.
mexpand__unify_args(I, N, F, A, G) :- 
        arg(I, F, X),
        arg(A, G, X),
        I1 is I+1,
        A1 is A+1,
        mexpand__unify_args(I1, N, F, A1, G).

% ---------------------------------------------------------------------------

% partial evaluation of metacast<fact>
{
:- fluid pEnv :: p_env + u.
:- fluid m :: any.
% TODO: missing treatment of 'blt__m_any' context vars
'peval metacast<fact>'(H0, NH) :-
	get_qualifier(H0, Qual, H),
	call((
          vEnv :: v_env <- ~v_env.empty,
	  ignorefun :: any <- false,
	  residue :: accum(NA),
	  mexpand__goal__1(Qual, H)
        )),
	NA = [NH]. % TODO: what kind of error emit if NA length is greater than 1?
}.

% ---------------------------------------------------------------------------
% Expand a goal
% +det, if precondition holds

{
:- extends mexpscope.
:- fluid ignorefun :: any.
:- fluid residue :: accum.
mexpand__body0(A) :- % expand a body
	get_qualifier(A, Qual, A0),
	call((
          qual :: any <- Qual,
	  mexpand__goal0(A0)
        )).
}.

% TODO: special cases should be run after module is resolved...
% TODO: in runtime expansions (rt_exp), a special case should be called when do_goal_trans is known not to have any user defined expansion
{
:- extends mexpscope.
:- fluid qual :: any.
:- fluid ignorefun :: any.
:- fluid residue :: accum.
%mexpand__goal0(Call) :- mexpand__trace(tr(Call)), fail.
mexpand__goal0(A) :- var(A), !,
%	mexpand__trace(verify_that_qual_is_empty(~qual)),
	Metacast = ~vEnv.match_goal(A), % TODO: if metatypes are incompatible, there must be an error
	'peval metacast+call'(Metacast, A).
% ----
% TODO: hmmm, are they imported from basiccontrol? check resolved
%       module? (it was enabled before and it did not seem very
%       costly)
mexpand__goal0(','(A,B)) :- !,
	mexpand__body0(A),
	mexpand__body0(B).
mexpand__goal0(';'(A,B)) :- !,
	PEnv = ~pEnv,
	% TODO: define a binder for that
	call((
	  pEnv :: p_env <- PEnv,
	  mexpand__body1(A, A2),
	  OutPEnvA = ~pEnv
        )),
	call((
	  pEnv :: p_env <- PEnv,
	  mexpand__body1(B, B2),
	  OutPEnvB = ~pEnv
        )),
	pEnv <- OutPEnv,
	p_env__meet(PEnv, OutPEnvA, OutPEnvB, A2, A3, B2, B3, OutPEnv),
	residue.add('basiccontrol:;'(A3, B3)).
mexpand__goal0('->'(A,B)) :- !,
	mexpand__body1(A, A2),
	mexpand__body1(B, B2),
	residue.add('basiccontrol:->'(A2, B2)).
mexpand__goal0('\\+'(A)) :- !,
	% Defined as (A -> fail ; true), the only possible success propagation of PEnv is itself
	PEnv = ~pEnv,
	call((
          pEnv :: p_env <- PEnv,
          mexpand__body1(A, A2)
        )),
	residue.add('basiccontrol:\\+'(A2)).
mexpand__goal0('if'(C,A,B)) :- !,
	PEnv = ~pEnv,
	call((
          pEnv :: p_env <- PEnv,
	  mexpand__body1(C, C2),
	  mexpand__body1(A, A2),
	  OutPEnvA = ~pEnv
        )),
	call((
          pEnv :: p_env <- PEnv,
	  mexpand__body1(B, B2),
	  OutPEnvB = ~pEnv
        )),
	p_env__meet(PEnv, OutPEnvA, OutPEnvB, A2, A3, B2, B3, OutPEnv),
	pEnv <- OutPEnv,
	residue.add('basiccontrol:if'(C2, A3, B3)).
% --
% mexpand__goal0(G) :-
% 	mexpand__option(class_expand),
% 	mexpand__dynpred_op(G),
% 	!,
% 	mexpand__treat_dynpred_op(G).
% --
% TODO: use a flag to define it
mexpand__goal0(G) :-
        G = call(X),
	% TODO: check that it is defined in hiord_rt
        !,
	% Note: the scope of X is limited
	% TODO: [BUG] call(((X=1;X=2),!)) is not compiled correctly (i.e., the scope of cut)
        mexpand__ctx_scope(X).
% ----
% special 'binder' predicates
% TODO: define many versions, use HO notation, e.g.
%   foo :: accum(Ys),
%   maplist({ :- fluid foo :: foo_t. ''(X) :- foo.add(X). }, Xs, Ys).
% TODO: infer Ctx
mexpand__goal0(G) :- % TODO: better, resolve binder; optimize!!
	functor(G, N, A),
	A1 is A - 1,
	functor(Head, N, A1),
	mexpand__binder_def(Head, Def0),
	!,
	G =.. [_|As], Head =.. [_|As0], 
	( mexpand__append(As0, [Goal], As) -> true ),
	mexpand__ctx_scope((Def0, Goal)).
mexpand__goal0(Map) :- mexpand__option(class_expand), % HO version
	functor(Map, maplist, _),
	Map =.. [_, PredN|Lists],
	nonvar(PredN),
	PredN = (Hh :- B),
	% TODO: ShVs used here for fluids; check if it makes sense
	mexpand__head_and_shvs(Hh, H, ShVs),
	H =.. [''|Elems],
	mexpand__length(Elems, N),
	mexpand__length(Lists, N),
	!,
	% TODO: find better name for 'Ctx'
	% TODO: implement \eta-conversion (i.e., allow preds names,
	%       without explicit predicate closures, e.g., 'foo'
	%       instead of (''(X) :- foo(X)) if we need pred(N))
	Ctx = ShVs, Goal = B,
%	display(user_error, newbinder(maplist(Ctx, Lists, Elems, Goal))), nl(user_error),
	mexpand__body0(~mexpand__maplistn(Ctx, Lists, Elems, Goal)).
% ----
mexpand__goal0('\6\ctx_enter'(Decls, Goal)) :- !, % TODO: (internal)
	mexpand__ctx_enter(Decls, Goal).
% ----
mexpand__goal0(A) :-
	do_funexp,
	funexp_goal_trans(A), !.
mexpand__goal0(A) :-
	ctx_goal_trans(A), !.
% ----
mexpand__goal0(A) :-
	% This translation does not know about modules of each goal yet
        mexpand__do_goal_trans(A, ~m, ~qual, NB), !,
	% Call expansion again (it may loop if goal translation rules
	% are wrong)
        mexpand__body0(NB).
mexpand__goal0(A) :-
	mexpand__goal__1(~qual, A).

% TODO: implement family of assert/retract operations, that work on all kind of 'data'
% mexpand__dynpred_op(assertz(_)).

% mexpand__treat_dynpred_op(assertz(G)) :-
}.

%% TODO: I disabled this rule. Enable it again?
% x.method ==> (~x).method if x/1 is a predicate (ctx_var, inst_var, or pred)
%	  % TODO: remove this rule?
%	  QualPath0 = [FX|FRest],
%	  QualK = pmod, % only for _._
%	  atom(FX),
%	  push(recv_mtype, recv_implicit) '$ctx_on' mexpand__resolve(sympred(FX, 1), _) ->
%	    mexpand__trace(q(QualK, QualPath0)),
%	    push(ignorefun, false) '$ctx_on' mexpand__body0(FX2 = ~funcall(FX)),
%	    QualPath1 = [FX2|FRest]

self_name('$self').

{
:- extends mexpscope.
:- fluid residue :: accum.
% Input:
%   QualK: mod (A:G) or pmod (A.G)
%   QualPath0: A1...An
%   Sym: predicate symbol of the goal
% Output:
%   F2: resolved predicate
%   A: arity
%   Context, Metatypes:
%   SetReceiver: information to set the receiver ('self')
flatten_qual_and_resolve(Qual, Sym, FnObj) :- % Application of a predicate abstraction
	Qual = qual_o(Obj),
	[G] = ~dotpath_to_list(Obj), % TODO: simplify, generalize for other Obj different than [_]
	Sym = sympred('ho_apply__', N),
	!,
        % Predicate abstraction: predicate information is retrieved from the
        %   metatype dictionary (if available). If no information is
        %   available, code to perform the lookup at runtime is emitted.
	%
	% Do mexpand__get_module on A because it can be an arbitrary term
	( var(G) -> % TODO: hiord and runtime, more cases?
	    NG = G,
	    vEnv.match_predN(G, primitive(pred(N, PEnvSig)), Metacast)
	; PEnvSig = [],
	  'peval metacast<pred(_,_)>'(G, pred(N, PEnvSig), NG),
	  Metacast = identity
	),
	% Metacast: transform the input term NG into a predicate abstraction
	FunctorR = ~pred_x.f('_:ho_apply__', N),
	Metatypes = deftypes, % TODO: extend pred metatypes to include more argument metatypes
	Metatypes2 = ~resolve_metatypes(Metatypes),
	SetReceiver = set_receiver_pa(Metacast, NG),
	FnObj = ri_fnc_m(FunctorR, PEnvSig, Metatypes2, SetReceiver).
flatten_qual_and_resolve(Qual, Sym, FnObj) :-
	% Actual predicate F/N
	% Search order: context, instance, file (defined and imported)
	% TODO: memory consumption increased when '$get' was introduced for more predicates, why?
	% (with optional prefix) P(X) is P.'$get'(X) is P is a instvar or the field of a ctxvar
	call((
          spath :: accum(SelfPath),
          ( Qual = qual_m(QM) ->
	      FinalWhere = qual_m(QM),
	      mexpand__resolve(FinalWhere, Sym, FunctorR2)
	  ; ( Qual = qual_none -> QualPath2 = []
	    ; Qual = qual_o(Obj) -> QualPath2 = ~dotpath_to_list(Obj)
	    ; fail
	    ),
	    QualPath3 = ~mexpand__append(QualPath2, ['\6\g'(Sym)]),
	    flatten_qual(qual_none, QualPath3, FinalWhere0, FunctorR0),
	    ( ref_field_enter(FunctorR0, FinalWhere) ->
	        % p.f(X), where f/1 is a field, is executed as p.f.'$get'(X)
	        mexpand__resolve(FinalWhere, sympred('$get', 1), FunctorR2)
	    ; FunctorR2 = FunctorR0,
	      FinalWhere = FinalWhere0
	    )
          )
	)),
	!, % Predicate found
	FunctorR2 = ~pred_x.f(F2, N),
	module_x.get_info(F2, N, Context, Metatypes),
	PEnvSig = ~mexpand__resolve_pEnvSig(Context),
	Metatypes2 = ~resolve_metatypes(Metatypes),
	% Does the predicate need self? (use/def/usedef?)
	( SelfPath = [_|_] ->
	    % TODO: check the a 'is_method' property instead (much cheaper)
	    self_name(CSelfName),
	    mexpand__find_ctx(PEnvSig, CSelfName, CSelfClass0, CSelfMode),
	    % TODO: Hack to enrich type of '$self'... it should not be 'instance' anymore!!
	    FinalWhere = qual_c(SelfClass),
	    % TODO: 'blt__any' in '\6\imodbase:$get'/1, 'multifile:absint:*', etc. because '$self' has no metatype info in those cases
%	    mexpand__trace(enriching_self(FunctorR2, SelfPath)),
	    CSelfClass = ( CSelfClass0 = 'blt__any' ? SelfClass
			 | CSelfClass0
			 ),
	    % Emit the code to set the receiver
	    ( SelfPath = [] ->
	        SetReceiver = no
	    ; SelfPath = [pf(Root,_)], nonvar(Root), Root = CSelfName ->
	        SetReceiver = no % the receiver is updated by mexpand__call_or_unfold
	    ; ( ( CSelfMode = use ; CSelfMode = usedef ) ->
		  call(( residue :: accum(LoadG), load_qualpath(SelfPath, ReceiverVal) ))
	      ; ReceiverVal = _, % Value of ReceiverVal is ignored
		LoadG = []
	      ),
	      SetReceiver = set_receiver(SelfPath, CSelfName, ReceiverVal, CSelfClass, CSelfMode, LoadG)
	    )
	; SetReceiver = no
	),
	FnObj = ri_fnc_m(FunctorR2, PEnvSig, Metatypes2, SetReceiver).
flatten_qual_and_resolve(Qual, Sym, FnObj) :-
	% Not found
	% TODO: it should not fail, throw exception?
	Sym = sympred(F, N),
	not_found_error(Qual, F, N, Error),
	mexpand__error(Error),
	% fail? throw?
	FunctorR2 = ~pred_x.f('$undefined$', N),
	Context = none,
	Metatypes = deftypes,
	Metatypes2 = ~resolve_metatypes(Metatypes),
	SetReceiver = no,
	PEnvSig = ~mexpand__resolve_pEnvSig(Context),
	FnObj = ri_fnc_m(FunctorR2, PEnvSig, Metatypes2, SetReceiver).

{
% spath: the trace of the qualifier path (to load/update the value)
:- fluid spath :: accum.
flatten_qual(Where, [X|Rest], FinalWhere, FunctorR) :-
	( var(X) ->
	    Where = qual_none, % TODO: error if not (foo.X... is not valid yet)
	    var_enter(X, Where3),
	    flatten_qual(Where3, Rest, FinalWhere, FunctorR)
	; \+ atom(X), \+ X = '\6\g'(_) ->
	    Where = qual_none, % TODO: error if not (?)
	    % Not an atom, flatten with an unification
	    call(( ignorefun :: any <- false, mexpand__body0(Val = X) )),
	    flatten_qual(qual_none, [Val|Rest], FinalWhere, FunctorR)
	; ( X = '\6\g'(G1) -> true ; G1 = sympred(X, 1) ),
	  flatten_resolve_one(Where, Where2, G1, FI),
	  ( Rest = [] ->
	      FinalWhere = Where2,
	      FunctorR = FI
	  ; ref_field_enter(FI, Where3),
	    flatten_qual(Where3, Rest, FinalWhere, FunctorR)
	  )
	).

flatten_resolve_one(Where, Where2, G1, FunctorR) :-
	% TODO: [MERGE] (look in the scope chain)
	( Where = qual_none ->
	    % Lookup in this order: pEnv, instance, rest of the code
	    ( Where2 = in_pEnv % in pEnv
	    ; % in the instance
	      self_name(SelfName0),
	      mexpand__resolve(in_pEnv, sympred(SelfName0, 1), FunctorRSelf),
%	      mexpand__trace(ri(Where, G1, SelfName0, FunctorRSelf)),
	      ref_field_enter(FunctorRSelf, Where2)
	    ; % TODO: this case currently limited to Rest = [] and First = yes
	      Where2 = qual_none % in the rest of code
	    )
	; Where2 = Where
	),
	mexpand__resolve(Where2, G1, FunctorR),
	% TODO: FIX!!! Methods in the outmost class are not treated correctly if looked up in the class <- ?
	( Where = qual_none,
	  \+ FunctorR = ref_field(_, _),
	  Where2 = qual_c(~m) ->
            fail
	; true
	),
	!. % Cut possible search
}.

}.

% ---------------------------------------------------------------------------

{
:- fluid vEnv :: v_env.
:- fluid spath :: accum.
% Enter a variable
var_enter(X, Where) :-
	Class = ~get_val_class(X, '\6\imodbase'),
	spath.add(pv(X, Class)),
	Where = qual_c(Class).
}.

{
:- fluid spath :: accum.
% Enter a resolved field.
% (Fails if FunctorR is not a field)
ref_field_enter(FunctorR, _) :-
	FunctorR = ~pred_x.f(_,_),
	!,
	fail.
ref_field_enter(FunctorR, Where) :-
	FunctorR = ref_field(X, Class),
	!,
	spath.add(pf(X, Class)),
	Where = qual_c(Class).
ref_field_enter(FunctorR, Where) :-
	trust(FunctorR instance_of module_x),
	% TODO: for calling static methods, unsure...
	Where = qual_c(FunctorR).
}.

{
:- extends mexpscope.
:- fluid ignorefun :: any.
:- fluid residue :: accum.
% TODO: [MERGE] see ptojs__scope:resolve_term/3
mexpand__goal__1(Qual, Goal) :-
	Qual = qual_m(QM), var(QM), !,
	% QM is unbound, emit a runtime expansion
	% TODO: metatypes are unknown! 
	Metacast = defer('?', primitive(pred(0,[]))),
	'peval metacast+call'(Metacast, ~apply_qual(Qual, Goal)).
% TODO: treat before get_qualifier is done
mexpand__goal__1(Qual, X) :- Qual = qual_none, functor(X, call, _), !,
	% transform call(G,A1..An) into G.ho_apply__(A1..An)
	X =.. [_,A|As],
	Method =.. ['ho_apply__'|As],
	mexpand__goal__1(qual_o(A), Method).
mexpand__goal__1(Qual, Goal) :-
	nonvar(Goal),
	% Decompose the goal call in callee and arguments
        decomp_goal(Goal, Sym, Args0),
	% Flatten the qualifier and lookup the predicate (or predicate
	% abstraction), obtaining metatype and context information.
        flatten_qual_and_resolve(Qual, Sym, FnObj),
	% Emit the goal call
	mexpand__goal_args(FnObj, Args0, call_to(Qual,Goal)).

:- '$ctxprj'(mexpand__goal_args/3, [vEnv, pEnv, m, ignorefun, residue]).
mexpand__goal_args(FnObj, Args0, Where) :-
	FnObj = ri_fnc_m(FunctorR, PEnvSig, Metatypes, SetReceiver0),
	trust(FunctorR instance_of pred_x),
	( SetReceiver0 = set_receiver(_,_,_,_,_,LoadG) ->
	    residue_concat(LoadG)
	; true
	),
	% TODO: merge defunc_args and mexpand__args?
	( do_funexp ->
	    % TODO: use Metatypes in defunc_args
	    Metatypes2 = ( Metatypes = deftypes ? ~default_metatypes(~FunctorR.get_arity)
			 | Metatypes
			 ),
	    ArithFlag = ( FunctorR = ~pred_x.f('arithmetic:is', 2) ?
			    false % avoid infinite loop
			| ~arith_flag
			),
	    defunc_args(Args0, Metatypes2, ArithFlag, Args1)
	; Args1 = Args0
	),
	( Metatypes = deftypes ->
	    Args2 = Args1
	; mexpand__args(Args1, Metatypes, Args2)
	),	
	( SetReceiver0 = set_receiver_pa(Metacast, PA0) ->
	    'peval metacast2'(Metacast, PA0, PA),
	    SetReceiver = no,
	    AddSelf = yes(PA) % TODO: this is a hack for ho_apply__, use 'set_receiver' instead
	; SetReceiver = SetReceiver0,
	  AddSelf = no
	),
	mexpand__set_receiver(SetReceiver, UpdateReceiver),
	mexpand__call_or_unfold(PEnvSig, AddSelf, FunctorR, Args2, Where),
	mexpand__update_receiver(UpdateReceiver).
}.

{
:- extends mexpscope_u.
:- fluid residue :: accum.
% apply metacast<_> to the arguments (and propagate metatypes)
% TODO: split in and out correctly
mexpand__args([], [], []) :- !.
mexpand__args([X|Xs], [Metatype|Metatypes], [A|As]) :-
	( Metatype = out(ClassR) ->
	    A = X,
	    ( var(X) ->
	        % TODO: this analysis is a kludge
	        % TODO: use expand__hoterm, but emit rt_exp later
	        ( vEnv.set(X, ClassR) -> true ; true )
	    ; true
	    )
	; 'peval metacast<_>'(Metatype, X, A)
	),
        mexpand__args(Xs, Metatypes, As).
}.

{
:- extends mexpscope.
:- fluid qual :: any.
:- fluid residue :: accum.
:- '$ctxprj'(mexpand__set_receiver/2, [vEnv, pEnv, m, residue]).
mexpand__set_receiver(SetReceiver, UpdateReceiver) :-
	( SetReceiver = set_receiver(SelfPath, CSelfName, ReceiverVal, CSelfClass, CSelfMode, _) ->
	    pEnv.set(CSelfName, ReceiverVal, CSelfClass, MaybePrevious),
	    propagate_class(ReceiverVal, CSelfClass),
	    UpdateReceiver = update_receiver(SelfPath, CSelfName, CSelfMode, MaybePrevious)
	; UpdateReceiver = no
	).

:- '$ctxprj'(mexpand__update_receiver/1, [vEnv, pEnv, m, residue]).
mexpand__update_receiver(UpdateReceiver) :-
	( UpdateReceiver = update_receiver(SelfPath, CSelfName, CSelfMode, MaybePrevious) ->
	    pEnv.get(CSelfName, ReceiverVal2, _),
	    pEnv.unset(CSelfName, MaybePrevious),
	    %
	    ( ( CSelfMode = usedef ; CSelfMode = def ) ->
		update_qualpath(SelfPath, ReceiverVal2)
	    ; true
	    )
	; true
	).

% Loads in Val the value of an absolute qualifier Path
:- '$ctxprj'(load_qualpath/2, [vEnv, pEnv, m, residue]).
% TODO: add extra optional argument so that it can be merged load_qualpath__2?
%       (pf/2 here refers to pEnv, pf/2 in __2 refers to in_class; that is not nice since we would want static in_class too)
load_qualpath(Path, Val) :-
	Path = [P|Path0],
	( P = pv(Root, RootClass) -> RootVal = Root
	; P = pf(Root, RootClass) -> pEnv.get(Root, RootVal, _)
	),
	load_qualpath__2(RootClass, RootVal, Path0, Val).

% Loads in Val the value of a qualifier Path relative to (RootMetatype,RootVal)
:- '$ctxprj'(load_qualpath__2/4, [vEnv, pEnv, m, residue]).
load_qualpath__2(RootClass, RootVal, Path, Val) :-
	( Path = [pf(Field, FieldClass)|Path0] ->
	    load_field(RootClass, RootVal, Field, FieldClass, FieldVal),
	    load_qualpath__2(FieldClass, FieldVal, Path0, Val)
	; Path = [],
	  Val = RootVal
	).

:- '$ctxprj'(load_field/5, [vEnv, pEnv, m, residue]).
% :- meta_predicate load_field(?,?,?,module_x,?).
load_field(RootClass, RootVal, Field, FieldClass, FieldVal) :-
	Load0 = ~mexpand__get_field(RootClass, Field, RootVal, FieldVal),
	call(( ignorefun :: any <- true, mexpand__body0(Load0) )),
	% TODO: var(FieldVar) should always be true
	% TODO: this should be done in mexpand__body0(Load0)
	( var(FieldVal) ->
	    ( vEnv.set(FieldVal, FieldClass) -> true ; true )
	; true
	).

% TODO: optimize!
% SubVal = Name.Field
:- '$ctxprj'(mexpand__get_field/5, []).
mexpand__get_field(ClassR, Field, Val, FieldVal) := R :-
	trust(ClassR instance_of module_x),
	DefaultClass = ~ClassR.defclass2,
	( DefaultClass = '\6\imodbase' ->
	    R = '\6\obj_attr_unify'(Val, Field, FieldVal)
	; % TODO: implement as a query of a special method
          LoadSub = ~atom_concat(ClassR, ~atom_concat(':$get_', Field)),
	  R =.. [LoadSub, Val, FieldVal]
	).

% Name.Field <- SubVal
:- '$ctxprj'(mexpand__set_field/6, []).
mexpand__set_field(ClassR, Field, Module0, Val, Module) := R :-
	% TODO: implement as a query of a special method
	StoreSub = ~atom_concat(ClassR, ~atom_concat(':$set_', Field)),
	R =.. [StoreSub, Module0, Val, Module].

% Store Val in the absolute qualifier Path
% Precondition: all the elements in the qualifier path are updatable
:- '$ctxprj'(update_qualpath/2, [vEnv, pEnv, m, residue]).
% TODO: this is a set on a fluid var, done statically (really similar to assignment in SSA transformation)
update_qualpath(Path, Val) :-
	Path = [pf(Root, RootClass)|Path0], nonvar(Root),
	pEnv.get(Root, RootVal0, RootClass0),
	% TODO: what is the difference between RootClass and RootClass0?
	( Path0 = [] ->
	    RootVal = Val
	; update_qualpath__2(RootClass, RootVal0, Path0, Val, RootVal)
	),
	pEnv.set(Root, RootVal, RootClass0, _),
	propagate_class(RootVal, RootClass0).

:- '$ctxprj'(update_qualpath__2/5, [vEnv, pEnv, m, residue]).
update_qualpath__2(RootClass, RootVal0, [pf(Field, FieldClass)|Path0], Val, RootVal) :-
	% Update Root
	( Path0 = [] ->
	    FieldVal = Val
	; % Get field and update
	  Load = ~mexpand__get_field(RootClass, Field, RootVal0, FieldVal0),
	  call(( ignorefun :: any <- true, mexpand__body0(Load) )),
	  update_qualpath__2(FieldClass, FieldVal0, Path0, Val, FieldVal)
	),
	Store = ~mexpand__set_field(RootClass, Field, RootVal0, FieldVal, RootVal),
	call(( ignorefun :: any <- true, mexpand__body0(Store) )).

:- '$ctxprj'(mexpand__call_or_unfold/5, [vEnv, pEnv, m, residue]).
mexpand__call_or_unfold(PEnvSig, AddSelf, FunctorR, Args1, Where) :-
	trust(FunctorR instance_of pred_x),
        ( FunctorR.method_code(HeadArgs, ArgsTemp, Code) ->
%	    mexpand__trace(found_code_to_unfold(F2, A, HeadArgs, ArgsTemp, Code)),
	    % Unfold a predicate
	    % TODO: preexpand the code to be unfolded?
	    mexpand__unfold_headargs(Args1, ArgsTemp, HeadArgs),
	    call(( ignorefun :: any <- false, mexpand__body0(Code) ))
        ; FunctorR.rdata(F) ->
	    H =.. [F|Args1], % TODO: F2 should be F
	    Code = ~mcall('self', 'get__'(H)),
	    call(( ignorefun :: any <- false, mexpand__body0(Code) ))
	; mexpand__ctx_connect_check(PEnvSig, Args1, Args, Where) ->
	    ( AddSelf = yes(FV) ->
	        Args2 = [FV|Args]
	    ; Args2 = Args
	    ),
	    NA = ~pterm__goal(FunctorR, Args2),
	    residue.add(NA)
        ; true % (error)
	).

:- '$ctxprj'(mexpand__ctx_connect_check/4, [vEnv, pEnv, m, residue]).
mexpand__ctx_connect_check(PEnvSig, Args1, Args, Where) :-
	( mexpand__ctx_connect(PEnvSig, Args1, Args) ->
	    true
	; % TODO: just show the problem, not all the contexts
	  %       (PEnvSig minus ~pEnv should be ok)
	  % TODO: pretty print 'Where' (and enrich with resolved module)
	  mexpand__error(cannot_connect_ctx(Where, PEnvSig, ~pEnv)),
	  % TODO: check again how errors are reported, cannot fail here
	  fail
	).

:- '$ctxprj'(mexpand__unfold_headargs/3, [residue]).
mexpand__unfold_headargs([], [], []).
mexpand__unfold_headargs([A|As], [T|Ts], [B|Bs]) :-
	% if T = m, then unify the argument directly
	% Note: that is useful for small predicates to avoid extra unifications
	% (in the case of .$get, it enables indexing when ctxvars are 
	% checked at the beginning of a clause)
	( ( var(A) ; integer(A) ; T = m ) ->
	    % avoid unification in simple cases
	    B = A
	; residue.add('term_basic:='(B, A))
	),
	mexpand__unfold_headargs(As, Ts, Bs).
}.

{
:- fluid vEnv :: v_env.
:- fluid pEnv :: p_env.
ctxvar_alloc(CtxName, Value, Class, MaybePrevious) :-
	pEnv.set(CtxName, Value, Class, MaybePrevious),
	propagate_class(Value, Class).

ctxvar_update(CtxName, NewValue) :-
	pEnv.get(CtxName, _, Class),
	trust(Class instance_of module_x),
	Class.mutable_statemodel,
	pEnv.set(CtxName, NewValue, Class, _),
	propagate_class(NewValue, Class).
}.

% TODO: Some uses of propagate_class are wrong
% TODO: Some uses of propagate_class are wrong
{
:- fluid vEnv :: v_env.
propagate_class(X, Class) :-
	trust(Class instance_of module_x),
	( var(X), Class2 = ~Class.to_metatype, \+ Class2 = ? ->
	    % TODO: this is not really correct...
	    ( vEnv.set(X, Class2) -> true ; true )
	; true
	).

% :- meta_predicate trust_metatype(?, module_x).
trust_metatype(X, MT) :-
	% TODO: this is only right if the variable has always the same metatype (i.e. there are no alternatives)  ... this analysis is a kludge
        ( vEnv.set(X, MT) -> true ; true ).
}.

% TODO: aterm__* and pterm__* should be merged (in some way)
pterm__goal(~pred_x.f('_:ho_apply__', _), [FV|Args]) := NA :- !,
	% A call to a predicate abstraction. 
	% @var{FV} may be an expanded predicate abstraction or a variable.
	% TODO: Define a dedicated instruction to do it at the bytecode level
	As =.. [''|Args],
	NA = 'hiord_rt:call'(FV,As).
pterm__goal(~pred_x.f(F2, _), Args) := NA :-
	NA =.. [F2|Args].

default_metatypes(0) := [] :- !.
default_metatypes(I) := ['?'|~default_metatypes(I1)] :- I1 is I - 1.

% ---------------------------------------------------------------------------

{
:- fluid vEnv :: v_env.
:- fluid pEnv :: p_env.
% TODO: include ctx_simple here?
mexpand__ctx_begin(PEnvSig) :-
	( mexpand__ctx_begin_(PEnvSig) ->
	    true
	; mexpand__error(bug(ctx_begin_failed(PEnvSig)))
	).

mexpand__ctx_begin_(PEnvSig) :-
	maplist(([vEnv, pEnv] -> ''(PSig) :-
	  mexpand__ctx_begin__2(PSig)
        ), PEnvSig).

mexpand__ctx_begin__2(PSig) :-
	psig_decomp(PSig, CtxName, Class, _CtxMode, _),
	ctxvar_alloc(CtxName, _Value, Class, _MaybePrevious).

% Add implicit arguments (Bs is As expanded with context)
mexpand__ctx_connect([], As0, As) :- !, As = As0. % TODO: redundant clause to make it faster, necessary?
mexpand__ctx_connect(PEnvSig, As, Cs) :-
	% Keep the first argument in its place
	( As = [A|As1] ->
	    Bs = [A|Bs1]
	; As1 = As, Bs = Bs1
	),
	% Add the implicit arguments for the state of fluid vars
	% TODO: only one indexed context variable is really indexed (moved as first argument)
	% note: Cs is placed before Bs
	mexpand__ctx_connect__2b(PEnvSig, As1, Bs1, Bs, Cs).

mexpand__ctx_connect__2b(PEnvSig, As0, As, Cs0, Cs) :- !,
	( PEnvSig = [] -> As = As0, Cs = Cs0
	; PEnvSig = [PSig|PEnvSig0],
	  ( psig_decomp(PSig, _, _, _, yes) ->
	      As1 = As0,
	      mexpand__ctx_connect__3(PSig, Cs0, Cs1)
	  ; Cs1 = Cs0,
	    mexpand__ctx_connect__3(PSig, As0, As1)
	  ),
	  mexpand__ctx_connect__2b(PEnvSig0, As1, As, Cs1, Cs)
	).

% TODO: compatible_classes is not finished.
%       It should be a kind of casting between metatypes.
%       Alternatively, make it explicit? (thus avoiding compatible_classes checks)
mexpand__ctx_connect__3(PSig, As, As2) :- !,
	psig_decomp(PSig, CtxName, Class, CtxMode, _),
	( CtxMode = usedef ->
	    pEnv.get(CtxName, Var, PrevClass),
	    compatible_classes(PrevClass, Class),
	    pEnv.set(CtxName, Var2, Class, _MaybePrevious),
	    propagate_class(Var2, Class),
	    As2 = [Var, Var2|As]
	; CtxMode = use ->
	    pEnv.get(CtxName, Var, PrevClass),
	    compatible_classes(PrevClass, Class),
	    As2 = [Var|As]
	; CtxMode = def ->
	    pEnv.set(CtxName, Var2, Class, _MaybePrevious),
	    propagate_class(Var2, Class),
	    As2 = [Var2|As]
	; CtxMode = void ->
	    As2 = As
	).
}.

% Find the CtxName entry in a context and obtain its Class and CtxMode
mexpand__find_ctx([PSig|PEnvSig], CtxName, Class, CtxMode) :-
	( psig_decomp(PSig, CtxName, Class, CtxMode, _) ->
	    true
	; mexpand__find_ctx(PEnvSig, CtxName, Class, CtxMode)
	).

:- meta_predicate mexpand__ctxdef(module_x, ?).
mexpand__ctxdef(Name) :=
	( Name = 'module' ? fluidsig('$module', 'blt__any', none, no)
	| Name.user_ctxdef(Def0) ? Def0
	).

% ---------------------------------------------------------------------------

% Add unifications in the right place (see later)
% TODO: what is the right place?
mexpand__add_unifs([], G0, G) :- !, G = G0.
mexpand__add_unifs(Unifs, G0, G) :-
	unifs :: any <- Unifs,
	mexpand__add_unifs__1(G0, G).
{
:- fluid unifs :: any.
% Repeat unifs for each branch
mexpand__add_unifs__1('basiccontrol:;'(A, B), G) :- !,
	G = 'basiccontrol:;'(A2, B2),
	mexpand__add_unifs__1(A, A2),
	mexpand__add_unifs__1(B, B2).
mexpand__add_unifs__1('basiccontrol:->'(A, B), G) :- !,
	G = 'basiccontrol:->'(A2, B2),
	A2 = A,
	mexpand__add_unifs__2(B, B2). % TODO: add after cut here? maybe... because we are connecting ctx vars in head and cut affects the clause, not this if-then-else
mexpand__add_unifs__1(G0, G) :-
	mexpand__add_unifs__2(G0, G).

mexpand__add_unifs__2(G0, G) :-
	mconj_to_slist(G0, GList),
	( mexpand__split_at_cut(GList, PreCut, PostCut) ->
            % Add after cut
	    mexpand__add_unifs__and(PostCut, PostCut2),
	    list_to_mconj(PreCut, GPreCut),
	    list_to_mconj(PostCut2, GPostCut2),
	    G = 'basiccontrol:,'(GPreCut, GPostCut2)
	; % No visible cut (may be deeper)
	  mexpand__add_unifs__and(GList, GList2),
	  list_to_mconj(GList2, G)
	).

% Skip user unifications
mexpand__add_unifs__and([X|Xs], Ys) :- X = 'term_basic:='(_,_), !,
	Ys = [X|Ys0], mexpand__add_unifs__and(Xs, Ys0).
mexpand__add_unifs__and(Xs, Ys) :- mexpand__add_unifs__and__2(~unifs, Xs, Ys).
}.

mexpand__add_unifs__and__2([], Xs, Ys) :- !, Ys = Xs.
mexpand__add_unifs__and__2([U|Us], Xs, [U|Ys]) :- mexpand__add_unifs__and__2(Us, Xs, Ys).

mexpand__split_at_cut(['basiccontrol:!'|As], Pre, Post) :- !,
	Pre = ['basiccontrol:!'], Post = As.
mexpand__split_at_cut([A|As], Pre, Post) :- !,
	Pre = [A|Pre0],
	mexpand__split_at_cut(As, Pre0, Post).

% ---------------------------------------------------------------------------

% partial evaluation of metacast(list(T))
{
:- extends mexpscope_u.
:- fluid residue :: accum.
'peval metacast<list(_)>'([], _Metatype, []) :- !.
'peval metacast<list(_)>'([E|Es], Metatype, [NE|NEs]) :- !,
        'peval metacast<_>'(Metatype, E, NE),
        'peval metacast<list(_)>'(Es, Metatype, NEs).
}.

% ---------------------------------------------------------------------------

% partial evaluation of metacast2<(FromType,ToType)>
{
:- fluid pEnv :: p_env + u.
:- fluid m :: any.
:- fluid residue :: accum.
'peval metacast2'(defer(KnownMetatype, ExpectedMetatype), A, NA) :- !,
	% defered metacast
	mexpand__uses_hiord, % TODO: not really... we may not be doing a 'call' later
	% TODO: add variables names to v_env, so that I can show good error messages here?
        mexpand__uses_rt_metacast(A, KnownMetatype, ExpectedMetatype),
	'emit rt_exp_term'(A, ExpectedMetatype, NA).
'peval metacast2'(box, A0, A) :- !, % boxing to obtain a non-primitive metatype
	residue.add('term_basic:='(A, '$:'(A0))).
'peval metacast2'(unbox, A0, A) :- !, % boxing to obtain a primitive metatype
	residue.add('term_basic:='(A0, '$:'(A))).
'peval metacast2'(identity, A, A).

'emit rt_exp_term'(X, Metatype, NX) :- pEnv.is_simple, !,
	% see rt_modexp/4 below (emitted when mexpand_runtime_db is enabled)
        residue.add('rt_exp:rt_modexp'(X, Metatype, ~m, NX)).
'emit rt_exp_term'(X, Metatype, NX) :-
	% see rt_exp/4 below (emitted when mexpand_runtime_db is enabled)
        residue.add('rt_exp:rt_exp'(X, Metatype, ~m, -, ~pEnv, NX)).
}.

:- if(defined(mexpand_runtime_db)).
% Runtime version of 'peval metacast<_>' (rt_exp_term/2)

:- export(rt_modexp/4).
% (specialized rt_exp/6 where 'pEnv.is_simple')
rt_modexp(X0, MType, M, X) :-
	m :: any <- M,
	metatype :: any <- MType,
	pEnv :: p_env <- ~p_env.simple,
	rt_exp_term(X0, X).

:- export(rt_exp/6).
% TODO: remove QM
rt_exp(V, MType, M, _QM, PEnv, NT) :-
	m :: any <- M,
	metatype :: any <- MType,
	pEnv :: p_env <- PEnv,
	rt_exp_term(V, NT).
{
:- fluid m :: any.
:- fluid metatype :: any.
:- fluid pEnv :: p_env + u.
rt_exp_term(X, R) :- var(X), !, R = X.
% already expanded
rt_exp_term(X, R) :- X = '$:'(X0), !, ( ~metatype = primitive(_MType) -> R = X0 ; R = X ).
% any module
rt_exp_term(QM:G, R) :- var(QM), !,
	% TODO: is var(QM) valid? why is this different from the compile-time mode?
	% TODO: if QM is a variable, search the module?
	% TODO: the recursion is not correct (not all cases are applicable...)
	% TODO: anyway, it seems inconsistent!!! 
        rt_exp_term(G, R).
rt_exp_term(X, R) :-
	vEnv :: v_env <- ~v_env.empty,
	residue :: m_any <- no, % TODO: rename 'g' by 'residue' (or a more standard name for peval)
	'peval metacast<_>'(~metatype, X, R),
	~residue = no. % TODO: nothing has been added in 'g' (i.e., the evaluation was not partial but total)
}.
:- endif.

% Specialize 'emit rt_exp_term'(X, MetaType, NX) + 'emit call'(NX)
{
:- fluid pEnv :: p_env.
:- fluid m :: any.
:- fluid residue :: accum.

'peval metacast+call'(Metacast, A) :-
	( Metacast = defer(KnownMetatype, ExpectedMetatype) ->
	    mexpand__uses_hiord,
	    % TODO: add variables names to v_env, so that I can show good error messages here?
	    mexpand__uses_rt_metacast(A, KnownMetatype, ExpectedMetatype),
	    'emit rt_exp_term+call'(A, ExpectedMetatype)
	; 'peval metacast2'(Metacast, A, A2),
	  'emit call'(A2)
	).

'emit rt_exp_term+call'(A, primitive(pred(0,[]))) :- pEnv.is_simple, !,
	% see rt_pgcall/2 and see rt_slowpgcall/2 below (emitted when mexpand_runtime_db is enabled)
	residue.add('rt_exp:rt_pgcall'(A, ~m)).
'emit rt_exp_term+call'(A, MetaType) :-
	'emit rt_exp_term'(A, MetaType, A2),
	'emit call'(A2).
}.

{
:- fluid residue :: accum.
'emit call'(A) :-
	residue.add('hiord_rt:call'(A)).
}.

:- if(defined(mexpand_runtime_db)).
:- use_module(engine(hiord_rt), ['$meta_call'/1, call/1]).
:- export(rt_pgcall/2).
% primitive(pred(0,[])) expand and call:
%
%  - If no goal translations, meta args or context are necessary, do a
%    fast goal expansion (implemented in C), and call the result.
%
%  - If not possible, execute the complete but slower expansion
%    rt_slowpgcall.
%
% note: implemented in engine/dynlink.c
:- '$props'(rt_pgcall/2, [impnat=cinsnp(prolog_xcall)]).

:- export(rt_slowpgcall/2).
% Do a complete body expansion (e.g. as required by arbitrary toplevel queries)
% note: called from 'hiord_rt:call', implemented in engine/dynlink.c
rt_slowpgcall(T, M) :-
	pEnv :: p_env <- ~p_env.simple,
	m :: any <- M,
	( var(T) ->
	    % TODO: throw exception?
	    '$meta_call'(T)
	; call((
            vEnv :: v_env <- ~v_env.empty,
	    % Like 'peval metacast<pred(_,_)>'(T, pred(0, []), NP)
	    ignorefun :: any <- false,
	    residue :: accum(NT0),
	    mexpand__body0(T)
          )),
	  % TODO: this should be faster...
	  '$meta_call'(~list_to_mconj(NT0))
        ).
:- endif.

% ---------------------------------------------------------------------------
% Lookup symbols

% Input predicates for expansions:
%  mexpand__imports/5    - MODULE imports from MODULE2 F/A, which resides in ENDMODULE
%  mexpand__meta_args/2  - MODULE has meta declaration META
%  mexpand__multifile/3  - MODULE defines multifile F/A
%  mexpand__defines/3    - MODULE defines F/A
%  mexpand__redefining/3
%  mexpand__context/4

% TODO: Fix atom expansion in module resolution (broken until low-level nested atoms are possible)
%   E.g. Predicate 'c' in module called 'a:b' (explicitly with quotes)
%        is expanded as 'a:b:c'.
%        Predicate 'b:c' (quoted) in module called 'a'
%        is expanded as 'a:b:c'. 
%   The expanded name of the predicates collide.

% TODO: detect loops in ctxdef
mexpand__resolve_pEnvSig(Context) := PEnvSig :-
	pEnvSig :: accum(PEnvSig),
	mexpand__resolve_pEnvSig__2(Context).
{
:- fluid pEnvSig :: accum.
mexpand__resolve_pEnvSig__2(Def) :-
	( Def = none ->
	    true
	; Def = modif(Context, prj(Prj)) ->
	    mexpand__resolve_pEnvSig(Context, PEnvSig),
	    mexpand__keep_self(PEnvSig),
	    mexpand__project_pEnvSig(Prj, PEnvSig)
	; Def = modif(Context, selfprj(Mode)) ->
	    mexpand__resolve_pEnvSig(Context, PEnvSig),
	    mexpand__selfprj_ctx(PEnvSig, Mode)
	; Def = (C1, C2) ->
	    mexpand__resolve_pEnvSig__2(C1),
	    mexpand__resolve_pEnvSig__2(C2)	    
	; atom(Def),
	  ( is_resolved(Def), Def = Def0 % already resolved (for set_pred_context)
	  ; Def0 = ~resolve_class(Def)
	  ) ->
            trust(Def0 instance_of module_x),
	    ( mexpand__ctxdef(Def0, Def2) ->
	        true
	    ; % TODO: improve error message
	      mexpand__error(undefined_ctx(Def0)),
	      fail
	    ),
	    mexpand__resolve_pEnvSig__2(Def2)
	; pEnvSig.add(~resolve_ctx_def(Def))
	).
}.

% TODO: see where it is used -- avoid that in the future (by using owner class)
:- if(defined(mexpand_runtime_db)).
is_resolved(_) :- fail.
:- else.
is_resolved(Def) :- frontend:module__in_mod(Def, _, _), !.
:- endif.

% TODO: class resolution for fluid predicates should be done elsewhere
resolve_ctx_def(fluidsig(Name, Class0, Opts, Indexed), fluidsig(Name, Class, Opts, Indexed)) :- !,
	Class = ~resolve_class(Class0).
resolve_ctx_def('\6\fluidsig'(Name, ClassR, Opts, Indexed), fluidsig(Name, ClassR, Opts, Indexed)) :- !. % (already resolved, for flatmod_expansion)
resolve_ctx_def(X, X) :-
	mexpand__error(bug(wrong_ctx_def(X))), fail.

% TODO: class resolution for fluid predicates should be done elsewhere
resolve_metatypes(deftypes) := deftypes.
resolve_metatypes([]) := [].
resolve_metatypes([X|Xs]) := [~resolve_metatype(X) | ~resolve_metatypes(Xs)].

resolve_metatype(out(X)) := out(~resolve_class(X)) :- !. % TODO: out(_) is a modifier of the metatype, not a metatype
resolve_metatype(X) := ~resolve_class(X).

% TODO: add proper errors to 'class' resolution (and give it a better name than 'class', 'sort'?)
% (built-in classes for default metatypes)
resolve_class('?', '?') :- !. % TODO: class or metatype? (this is similar to a top object)
resolve_class(fact, fact) :- !.
resolve_class(clause, clause) :- !.
resolve_class(spec, spec) :- !.
resolve_class(list(N0), list(N)) :- !, N = ~resolve_class(N0).
resolve_class(pred(N), pred(N,C)) :- !, C = ~mexpand__resolve_pEnvSig(none).
resolve_class(pred(N,C0), pred(N,C)) :- !, C = ~mexpand__resolve_pEnvSig(C0).
resolve_class(goal, pred(0,C)) :- !, C = ~mexpand__resolve_pEnvSig(none).
resolve_class(goal(C0), pred(0,C)) :- !, C = ~mexpand__resolve_pEnvSig(C0).
resolve_class(primitive(X0), primitive(X)) :- !, X = ~resolve_class(X0).
% (built-in classes)
% TODO: add qualification to built-in classes too
resolve_class('module', 'module') :- !. % TODO: ???
resolve_class('\6\ctxbase', '\6\ctxbase') :- !. % TODO: (for contexts)
resolve_class('\6\imodbase', '\6\imodbase') :- !.
resolve_class('blt__any', 'blt__any') :- !.
resolve_class('blt__m_any', 'blt__m_any') :- !.
:- if(defined(mexpand_runtime_db)).
:- else.
resolve_class(Name, Id) :- frontend:module__in_mod(Id0, Name, _Mod), !, Id = Id0. % OK?
resolve_class(Id, Id) :- frontend:module__in_mod(Id, _, _Mod), !,
	mexpand__error(bug(resolved_twice(Id))), fail.
:- endif.
resolve_class(X, X) :-
	mexpand__error(bug(cannot_resolve_class(X))), fail. % TODO: error, not a bug

{
:- extends mexpscope_u.
% Resolve predicates and fields in a given scope
% TODO: in_pEnv must have a proper unique name (e.g., foo/1.$scope1.$scope2)
% TODO: Where = in_pEnv and Pred = sympred(_, 1) because nonunary predicates in context are not yet allowed
:- if(defined(mexpand_runtime_db)).
:- else.
mexpand__resolve(in_pEnv, sympred(Name0, 1), FunctorR) :- % (was a FIELD)
	% resolving a class name (e.g. for calling static methods)
	% TODO: strange use, add a parameter to 'resolve_class' to fail silently (like in JS backend)
	% Name = ~resolve_class(Name0),
	frontend:module__in_mod(Id, Name0, _), % TODO: see TODO above
	trust(Id instance_of module_x),
%	Name.in_mod(_),
%	mexpand__trace(resolved_a_class_name(Name)),
	!,
	% TODO: 
	FunctorR = Id.
:- endif.
mexpand__resolve(in_pEnv, sympred(Name, 1), FunctorR) :- % (was a FIELD)
	% customizable 'selfname'
	% TODO: this is a hack...
	self_name(SelfName),
	pEnv.get(SelfName, Val, _),
	ClassR = ~get_val_class(Val, '\6\ctxbase'),
	trust(ClassR instance_of module_x), % TODO: resolve ClassName?
	SelfName0 = ~ClassR.selfname, Name == SelfName0,
	!,
	FunctorR = ref_field(SelfName, ClassR).
mexpand__resolve(in_pEnv, sympred(Name, 1), FunctorR) :- % (was a FIELD)
	pEnv.get(Name, Val, _),
	ClassR = ~get_val_class(Val, '\6\ctxbase'),
	!,
	% TODO: ref_filed should contain its scope too (each scope must have a name, even if it is introduced locally in a mexpand__ctx_scope)
	FunctorR = ref_field(Name, ClassR).
%
mexpand__resolve(qual_c(ClassR), Pred, FunctorR) :- !,
	% TODO: qualify class names (to make it unique) and use qual_m here... (so that I can check if a given name is for a class)
	trust(ClassR instance_of module_x),
	CurrClassR = ~ClassR.maybe_parent, % search in parents too
	\+ CurrClassR = 'serializable', % TODO: this is a mixin... remove mixins
	Pred = sympred(F, N),
	( ( N = 1, ClassR.field2(F, FieldClass) ->
	      FunctorR = ref_field(F, ~resolve_class(FieldClass))
	  ; CurrClassR.virtual(F, N, _, RF) ->
	      % note: call the wrapper predicate that calls the virtual entry, not the
	      %       real predicate
	      % TODO: There is no syntax to call the non-virtual definition
	      QM2 = ~CurrClassR.in_mod2,
	      mexpand__resolve_module(qual_m(QM2), RF, N, FunctorR)
	  ; CurrClassR.method(F, N, RF) ->
	      ( module_x.ctx_inline_method_info(RF, N, _, _) ->
		  FunctorR = ~pred_x.f(RF, N)
	      ; QM2 = ~CurrClassR.in_mod2,
		mexpand__resolve_module(qual_m(QM2), RF, N, FunctorR)
	      )
	  ; DefClass = ~CurrClassR.defclass2, DefClass = '\6\imodbase' ->
	      % the outmost class, do not use the method tables
	      % TODO: avoid this special case (e.g. no virtual, etc.)
	      QM2 = ~CurrClassR.in_mod2,
	      mexpand__resolve_module(qual_m(QM2), F, N, FunctorR)
	  )
	),
	!.
%
mexpand__resolve(qual_m(QM), sympred(F, N), FunctorR) :-
	mexpand__resolve_module(qual_m(QM), F, N, FunctorR).
%
mexpand__resolve(qual_none, sympred(F, N), Ref) :-
	mexpand__resolve_module(qual_none, F, N, Ref).
}.

% TODO: [MERGE] ptojs__scope:resolve_unqual_name
% +semidet (fails if predicate not found)
% TODO: is imports when QM = - inefficient? calculate cross product before? (just after modules have been loaded... or include a set of common imports... and a list of overwrites... hmmm this is hard)
% TODO: unfold to improve efficiency
:- if(defined(mexpand_runtime_db)).
% TODO: could I maintain a single Prolog/ImProlog version?
{
:- fluid m :: module_x. % (is_top_module)
mexpand__resolve_module(Qual, F, N, Ref) :-
	mexpand__resolve_module_(~m, ~simple_qual__qm(Qual), F, N, MF),
	Ref = ~pred_x.f(MF, N).
}.

simple_qual__qm(qual_m(QM), QM).
simple_qual__qm(qual_none, -).

% note: implemented in engine/rt_exp.c
:- '$props'(mexpand__resolve_module_/5, [impnat=cbool(prolog_module_resolve)]).
:- else.
{
:- fluid m :: module_x. % (is_top_module)
mexpand__resolve_module(_, F, N, Ref) :- number(F), !,
	% TODO: wrong, why is it necessary?
        Ref = ~pred_x.f(F, N).
mexpand__resolve_module(qual_none, F, N, Ref) :- !,
	mexpand__check_dups(F, N), % not necessary in runtime expansions
        ( pdecl(F, N, MF) -> Ref = ~pred_x.f(MF, N)
        ; imports(F, N, _, _, _, MF) -> Ref = ~pred_x.f(MF, N) % includes reexports
	; '$user_module_id'(~m),
	  Ref = ~m.pred_ref(F, N)
	).
mexpand__resolve_module(qual_m(QM), F, N, Ref) :-
	( QM = ~m ->
	    ( pdecl(F, N, MF) -> Ref = ~pred_x.f(MF, N)
	    ; % here we cannot use imports/6 because there is no way
	      % to distinguish between a reexport and an import
	      reexports(F, N, MF) -> Ref = ~pred_x.f(MF, N)
	    )
	; ( imports(F, N, QM, _, _, MF) -> Ref = ~pred_x.f(MF, N)
	  ; '$user_module_id'(~m), QM = user,
	    Ref = ~m.pred_ref(F, N)
	  )
	).
}.
:- endif.

{
:- fluid m :: any.
not_found_error(Qual, F, N) := Error :-
	M = ~m,
	% TODO: check that error messages are correct
	Error = ( Qual = qual_none ? not_defined(F, N, M)
		| Qual = qual_m(M0) ? not_imported_mod(F, N, M, M0)
		| Qual = qual_o(Obj) ? not_imported_path(F, N, M, ~dotpath_to_list(Obj))
		).
}.

% TODO: add a property is_top_module; that is true when it is an old module_x
% TODO: (i.e., in_mod(X,X) or not in_mod)
% TODO: add module qualification for module_x ids (to avoid ambiguities...)

% TODO: use Name as parameter in self
:- class module_x {
    % :- doc(title, "Module representation in mexpand").

    :- '$statemodel'(single).
    :- '$raw_state'.

    % :- public pred_ref/3.
    % TODO: only when 'is_top_module'
    :- meta_predicate pred_ref(?, ?, out(pred_x)).
    pred_ref(N, A) := Ref :-
        Ref = ~pred_x.new(N, A, ~self). 

    % TODO: missing instance_of__/1

:- if(defined(mexpand_runtime_db)).
    % (runtime version for engine/rt_exp.pl)
    selfname(_) :- fail.
    statemodel(_) :- fail. % TODO: missing
    in_mod(_) :- fail. % TODO: missing
    get_modif(_) :- fail. % TODO: missing
    extendsR(_) :- fail. % TODO: missing
    method_(_F, _A, _RF) :- fail.
    virtual(_F, _A, _FV, _FP) :- fail.
    % TODO: at this moment, empty for run-time expansions! enable it (useful for the top-level and for meta-programming)
    %       That is, calls like X.G where X or G are not known.
    field(_Field, _FieldClass) :- fail.

    % TODO: 'method_code' and 'data' does not use '~self'
    :- static method_code_/5.
    method_code_(_, _, _, _, _) :- fail.
    :- static data/3.
    data(_, _, _) :- fail.
:- else.
    selfname(SelfName) :-
	frontend:module__selfname(~self, SelfName), !.
    statemodel(StateModel) :-
	frontend:module__statemodel(~self, StateModel), !.
    in_mod(Mod) :- % TODO: split as owner_module and get_name
	frontend:module__in_mod(~self, _Name, Mod), !.
    get_modif(Modifier) :-
	frontend:module__modif(~self, Modifier), !.
    extendsR(Base) :-
	frontend:module__extendsR(~self, Base), !.
    method_(F, A, RF) :-
	( atom(F), number(A), frontend:module__method(F, A, ~self, RF) ->
	    true
	; fail
	).
    virtual(F, A, FV, FP) :-
	( atom(F), number(A), frontend:module__virtual(F, A, ~self, FV, FP, _) ->
	    true
	; fail
	).
    % TODO: emit errors when field is not defined
    % TODO: propagate the metatype!! like trust_metatype, define the same
    %       for '\6\obj_attr_unify'/3, allow inst_var decl with metatype,
    %       promote, and remove unnecessary trust_metatypes...
    field(Field, FieldClass) :-
	atom(Field),
	( frontend:module__field(Field, ~self, FieldClass0) -> FieldClass = FieldClass0
	; fail
	).

    % TODO: 'method_code_' and 'data' does not use '~self'
    :- static method_code_/5.
    method_code_(RF, A, Args, ArgsTemp, Code) :-
	( frontend:module__method_code(RF, A, _, Args, ArgsTemp, Code) ->
	    true
	; fail
	).
    :- static add_method_code/6.
    add_method_code(MRF, A, ClassName, Args, ArgsTemp, Code) :-
	assertz_fact(frontend:module__method_code(MRF, A, ClassName, Args, ArgsTemp, Code)).
    :- static data/3.
    data(MRF, A, F) :-
	( frontend:module__data(MRF, A, _, F) ->
	    true
	; fail
	).

    :- static copy_from_module_s_itf/2.
    % Fill this @class{module_x} from the @class{module_s_itf} @var{Module}
    copy_from_module_s_itf(Module, IM) :-
	trust(Module instance_of module_s_itf),
	trust(IM instance_of module_x), % (is_top_module)
	Id = ~Module.get_id,
	( ( Module.get_name(Name),
	      assertz_fact(frontend:module__in_mod(Id, Name, IM))
	  ; Module.get_modif(Modifier),
	      assertz_fact(frontend:module__modif(Id, Modifier))
	  ; Module.statemodel(StateModel),
	      assertz_fact(frontend:module__statemodel(Id, StateModel))
	  ; Module.selfname(A),
	      assertz_fact(frontend:module__selfname(Id, A))
	  ; Module.extendsR(A),
	      assertz_fact(frontend:module__extendsR(Id, A))
	  ; Module.field(FieldName, FieldClass),
	      % TODO: Compose IM and _Id?
	      assertz_fact(frontend:module__field(FieldName, Id, FieldClass))
	  ; Module.data(RF, A, F),
	      ~pred_x.f(MRF,_) = ~IM.pred_ref(RF, A),
	      assertz_fact(frontend:module__data(MRF, A, Id, F))
	  ; Module.method(A, B, C),
	      assertz_fact(frontend:module__method(A, B, Id, C))
%	  ; Module.method_code(A, B, C, D, E),
%	      assertz_fact(frontend:module__method_code(A, B, Id, C, D, E))
	  ; Module.virtual(A, B, C, D, E),
	      assertz_fact(frontend:module__virtual(A, B, Id, C, D, E))
	  ; Module.getctx(Def),
	      assertz_fact(frontend:module__ctx(Id, Def))
	  ),
	  fail % (loop)
        ; true
        ).

    % TODO: like copy_from_module_s_itf/2
    :- static copy_from_module_s/2.
    copy_from_module_s(ModuleS, _Module) :-
	trust(ModuleS instance_of module_s),
        % TODO: this special case should not be necessary, but it fails if missing
	% {Compiling (noarch) library(read)
	% error: (lns 67-68) '$self' is not a ctx_var in scope
	% error: (lns 67-68) Predicate $ctx_value/2 undefined in source
	ModuleS.is_static,
	!.
    copy_from_module_s(ModuleS, Module) :-
	trust(ModuleS instance_of module_s),
	trust(Module instance_of module_x), % (is_top_module)
	Id = ~ModuleS.get_id,
	( ( ModuleS.get_name(Name),
	      assertz_fact(frontend:module__in_mod(Id, Name, Module))
	  ; ModuleS.get_modif(Modifier),
	      assertz_fact(frontend:module__modif(Id, Modifier))
	  ; ModuleS.selfname(SelfName),
	      assertz_fact(frontend:module__selfname(Id, SelfName))
	  ; ModuleS.statemodel(StateModel),
	      assertz_fact(frontend:module__statemodel(Id, StateModel))
	  ; ModuleS.extendsR(Base),
	      assertz_fact(frontend:module__extendsR(Id, Base))
	  ; ModuleS.field(FieldName, FieldClass),
	      assertz_fact(frontend:module__field(FieldName, Id, FieldClass))
	  ; ModuleS.data(RF, A, F),
	      ~pred_x.f(MRF,_) = ~Module.pred_ref(RF, A),
	      assertz_fact(frontend:module__data(MRF, A, Id, F))
	  ; ModuleS.method(F, A, RF),
	      assertz_fact(frontend:module__method(F, A, Id, RF))
	  ; ModuleS.virtual(F, A, FV, FP, Ctx),
	      assertz_fact(frontend:module__virtual(F, A, Id, FV, FP, Ctx))
	  ; ModuleS.getctx(Def),
	      assertz_fact(frontend:module__ctx(Id, Def))
	  ),
	  fail % (loop)
	; true
	).

    % TODO: one for each module class?
    :- static clean/0.
    clean :-
        retractall_fact(frontend:module__statemodel(_,_)),
        retractall_fact(frontend:module__in_mod(_,_,_)),
        retractall_fact(frontend:module__modif(_,_)),
        retractall_fact(frontend:module__selfname(_,_)),
        retractall_fact(frontend:module__extendsR(_,_)),
        retractall_fact(frontend:module__field(_,_,_)),
        retractall_fact(frontend:module__data(_,_,_,_)),
        retractall_fact(frontend:module__method(_,_,_,_)),
        retractall_fact(frontend:module__method_code(_,_,_,_,_,_)),
        retractall_fact(frontend:module__virtual(_,_,_,_,_,_)),
        retractall_fact(frontend:module__ctx(_,_)),
        retractall_fact(frontend:binder_def(_,_)).
:- endif.

    % TODO: this should not be necessary
    to_metatype := MT :-
        Class = ~self,
	MT = ( Class = 'blt__any' ? (?)
	     | Class = 'blt__m_any' ? (?)
	     | Class = '\6\ctxbase' ? (?)
	     | Class = '\6\imodbase' ? (?)
	     | Class
	     ).

    % TODO: Duplicated in compiler/frontend.pl
    statemodel2 := StateModel :-
	Class = ~self,
	( Class = '\6\ctxbase' -> StateModel = single
	; Class = '\6\imodbase' -> StateModel = single
	; Class = 'blt__any' -> StateModel = single
	; Class = 'blt__m_any' -> StateModel = pair
	; statemodel(StateModel0) -> StateModel = StateModel0
	; % TODO: Detect statemodel for exported classes
          mexpand__error(bug(not_a_module_nor_noncalculated_statemodel(Class))),
	  fail
	).

    mutable_statemodel :-
	~statemodel2 = pair.

    field2(Field) := FieldClass2 :-
	field(Field, FieldClass),
	FieldClass2 =
            ( ( FieldClass = 'blt__any' ; FieldClass = 'blt__m_any' ) ?
	        ~defclass2
	    | FieldClass
	    ).

    defclass2 := P :- % version that computes defclass
        Modifier = ~get_modif,
	( Modifier = mod_class ; Modifier = mod_interface ),
	!,
	( Mod = ~in_mod, Mod = ~self -> % is_top_module ->
	    P = '\6\imodbase'
	; P = '\6\ctxbase'
	).
    defclass2 := P :-
        ClassName = ~self,
        P = ( ClassName = '\6\ctxbase' ? ClassName
	    | '\6\imodbase'
	    ).

    in_mod2 := P :-
        ClassName = ~self,
	P = ( Mod = ~in_mod ? Mod
	    | ClassName = '\6\ctxbase' ? ClassName % TODO: is this case necessary?
	    | ClassName = '\6\imodbase' ? ClassName
	    ).

    % Nondeterministically enumerate the class and its ancestors
    :- meta_predicate maybe_parent(out(module_x)).
    maybe_parent := P :-
        ClassName = ~self,
	% Unify with the class or the parent
	P = ( ClassName = '\6\ctxbase' ? ClassName
	    | ClassName = '\6\imodbase' ? ClassName
	    | ( ClassName
	      | ~extendsR % TODO: Incomplete
	      | ~defclass2
	      )
	    ).

    % Some predefined ctx methods
    % TODO: Move to the source
    % TODO: maybe is there a base for 'blt__m_any' objects?
    :- static ctx_inline_method/4.
    :- discontiguous ctx_inline_method/4.
    :- static ctx_inline_method_info/4.
    :- discontiguous ctx_inline_method_info/4.
    :- static ctx_inline_method_code/5.
    :- discontiguous ctx_inline_method_code/5.

    ctx_inline_method('$set', 1, '\6\ctxbase', '\6\ctxbase:$set').
    ctx_inline_method_info('\6\ctxbase:$set', 1, fluidsig('$self', 'blt__m_any', d, no), deftypes).
    %ctx_inline_method_info('\6\ctxbase:$get', 1, fluidsig('$self', pair, u, no), deftypes).
    ctx_inline_method_code('\6\ctxbase:$set', 1, [V], [c], '$ctx_set'('$self', V)). % internal
    %
    ctx_inline_method('$get', 1, '\6\imodbase', '\6\imodbase:$get').
    ctx_inline_method_info('\6\ctxbase:$get', 1, fluidsig('$self', 'blt__m_any', u, no), deftypes).
    ctx_inline_method_code('\6\ctxbase:$get', 1, [V], [m], '$ctx_value'('$self', V)). % internal
    %
    ctx_inline_method('$get', 1, '\6\ctxbase', '\6\ctxbase:$get').
    % TODO: this is wrong... it could be 'blt__m_any'+u or 'blt__any'
    ctx_inline_method_info('\6\imodbase:$get', 1, fluidsig('$self', 'blt__any', u, no), deftypes).
    ctx_inline_method_code('\6\imodbase:$get', 1, [V], [m], '$ctx_value'('$self', V)). % internal

    method(F, A, RF) :-
        ctx_inline_method(F, A, ~self, RF0), !,
	RF = RF0.
    method(F, A, RF) :-
        method_(F, A, RF).

    :- static get_info/4.
    get_info(MF, N, Context, Types) :-
	ctx_inline_method_info(MF, N, Context0, Types0), !,
	Context = Context0, Types = Types0.
    get_info(MF, N, Context, Types) :-
	get_info_(MF, N, Context, Types).
	
    :- static get_info_/4.
    :- if(defined(mexpand_runtime_db)).
    % TODO: speed up! (at least the default case) -> in theory, from 450 to 330ms
    get_info_(MF, N, Context, Types) :-
	( '$context'(MF, N, Context) ->
	    true
	; Context = none
	),
        ( functor(Meta, MF, N),
	  '$meta_args'(Meta) ->
	    Meta =.. [_|Types]
	; Types = deftypes
	).
    :- else.
    get_info_(MF, N, Context, Types) :-
        ( pinfo(MF, N, Meta, _, Context) ->
	    true
	; Meta = 0,
	  Context = none
	),
        ( Meta \== 0 -> Meta =.. [_|Types] ; Types = deftypes ).
    :- endif.

    :- static method_code/5.
    method_code(MF, A, HeadArgs, ArgsTemp, Code) :-
	ctx_inline_method_code(MF, A, HeadArgs0, ArgsTemp0, Code0), !,
	HeadArgs = HeadArgs0, ArgsTemp = ArgsTemp0, Code = Code0.
    method_code(MF, A, HeadArgs, ArgsTemp, Code) :-
        method_code_(MF, A, HeadArgs, ArgsTemp, Code).

    :- if(defined(mexpand_runtime_db)).
    user_ctxdef(_) :- fail. % TODO: missing
    :- else.
    user_ctxdef(Def) :-
	frontend:module__ctx(~self, Def0), !,
	Def = Def0.
    :- endif.
}.

:- if(defined(mexpand_runtime_db)).
:- else.
:- data module__statemodel/2.
:- data module__in_mod/3.
:- data module__modif/2.
:- data module__selfname/2.
:- data module__extendsR/2. % (resolved)
:- data module__field/3.
:- data module__data/4.
:- data module__method/4.
:- data module__method_code/6.
:- data module__virtual/6.
:- data module__ctx/2. % TODO: mixins? default ctx for predicates? base pEnvSig?
:- endif.

class_default_mode(Class) := Mode :-
	trust(Class instance_of module_x),
	StateModel = ~Class.statemodel2,
	Mode = ( StateModel = single ? use
	       | StateModel = pair ? usedef
	       ).

% TODO: This is wrong, check for subtype, supertype, etc. depending on
%       the case
compatible_classes(A, B) :-
	trust(A instance_of module_x),
	trust(B instance_of module_x),
	StateModelA = ~A.statemodel2,
	StateModelB = ~B.statemodel2,
	StateModelA = StateModelB.

%:- public pred_x.
% TODO: use cheaper representation
% TODO: find a solution for ref_field(_,_)
:- class pred_x {
    :- attr name :: any. % (module expanded)
    :- attr arity :: any.

    :- constructor f_/2.
    f_(N, A) :-
	~name = N, ~arity = A.

    :- constructor new_/3.
    new_(N, A, ModuleR) :-
	trust(ModuleR instance_of module_x), % (is_top_module)
	'$module_concat'(N, ModuleR, MN),
	~name = MN,
	~arity = A.

    :- public get_arity/1.
    get_arity := A :-
          A = ~arity.

    :- public method_code/3.
    method_code(HeadArgs, ArgsTemp, Code) :-
          F2 = ~name, A = ~arity,
	  module_x.method_code(F2, A, HeadArgs, ArgsTemp, Code).

    :- public rdata/1.
    rdata(F) :-
          F2 = ~name, A = ~arity,
	  module_x.data(F2, A, F).
}.

% ---------------------------------------------------------------------------

:- if(defined(mexpand_runtime_db)).
:- else.
:- data binder_def/2. % TODO: avoid implicit arg?
:- endif.

:- if(defined(mexpand_runtime_db)).
mexpand__binder_def(_, _) :- fail. % TODO: missing
:- else.
mexpand__binder_def(Head, Def) :-
	frontend:binder_def(Head, Def0), !,
	Def = Def0.
:- endif.

% ---------------------------------------------------------------------------
% Goal translations

% Apply a goal translation (the first that match) on goal G.
% This can be done several times until a fixpoint is reached (or it may
% not end if goal translation rules are wrong).
mexpand__do_goal_trans(G, M, Qual, NG) :-
	\+ mexpand__forbidden_syntax(G),
        mexpand__goal_trans(M, T),
          QG = ~apply_qual(Qual, G), % TODO: do not use it here! apply before qual is extracted
	  % TODO: this should be part of semantic_translation
	  %       (but we should not include the whole library)
          arg(1, T, QG),
          arg(2, T, NG),
          '$meta_call'(T), !.

% Syntactic control structures that the goal_trans will not treat.
% TODO: configure using a declaration? 
mexpand__forbidden_syntax(','(_,_)).
mexpand__forbidden_syntax(';'(_,_)).
mexpand__forbidden_syntax('->'(_,_)).
mexpand__forbidden_syntax('\\+'(_)).
mexpand__forbidden_syntax('if'(_,_,_)).

% ===========================================================================
% Translation of OO and context extensions

{
:- extends mexpscope.
:- fluid qual :: any.
:- fluid ignorefun :: any.
:- fluid residue :: accum.

% **Expansion of OO and ctx notation**

% X is an instance of Class (similar to Ruby)
% TODO: implement kind_of: X is an instance of C and C is Class or C extends Class (similar to Ruby)
% TODO: Improve, add version without trust
% -- (classes as metatypes)
ctx_goal_trans(trust(X instance_of Class)) :- mexpand__option(class_expand), !,
	trust_metatype(X, ~resolve_class(Class)).
ctx_goal_trans('$trust_metatype'(V, Metatype)) :- !,
        trust_metatype(V, ~resolve_class(Metatype)).
ctx_goal_trans(X instance_of Class) :- mexpand__option(class_expand), !,
	% TODO: class could be a variable here...
	mexpand__body0(~mcall(Class, 'instance_of__'(X))).
ctx_goal_trans('$meta_exp'(Metatype, P, E)) :- !,
	% TODO: include as a rt_exp predicate
	% TODO: force output to be a variable and update the metatype
	'peval metacast<_>'(~resolve_class(Metatype), P, NP),
	residue.add('term_basic:='(NP,E)).
% Syntactic sugar for setter
ctx_goal_trans('<-'(X, V)) :- mexpand__option(class_expand), !, 
	mexpand__body0(~mcall(X, '$set'(V))).
% -- (low level access to fluid unary preds)
% TODO: for implementation of interfaces
ctx_goal_trans('\6\fluid_class'(CtxName, ClassR)) :- !, % (low-level, see flatmod_expansion)
	pEnv.get(CtxName, Value, _),
	pEnv.set(CtxName, Value, ClassR, _),
	propagate_class(Value, ClassR).
% -- (low level access to fluid unary preds)
ctx_goal_trans('$ctx_value'(Var, X)) :- nonvar(X), !, % jf-imod
        mexpand__body0('$ctx_value'(Var, X0)),
	mexpand__body0(term_basic:(X0 = X)).
ctx_goal_trans('$ctx_value'(Var, X)) :- !, % jf-imod
	% TODO: include as a rt_exp predicate
	( pEnv.get(Var, Val, Class) ->
	    true % TODO: Check that it can be read?
	; mexpand__error(ctx_var_not_in_scope(Var)),
	  fail
	),
	% note: do not unify at compile time!
        mexpand__body0(term_basic:(X = Val)),
	propagate_class(X, Class). % TODO: why not take the metatype from Val?
ctx_goal_trans('$ctx_set'(CtxName, CtxVal)) :- !, % jf-imod
        mexpand__body0(term_basic:(CtxVal0 = CtxVal)),
	mexpand__body0('$ctx_set0'(CtxName, CtxVal0)).
ctx_goal_trans('$ctx_set0'(CtxName, CtxVal)) :- !, % jf-imod % TODO: CtxVal is a fresh variable, sure?check
	% TODO: include as a rt_exp predicate?
	( atom(CtxName), ctxvar_update(CtxName, CtxVal) ->
	    true
	; mexpand__error(internal(ctx_var_not_updatable(CtxName)))
	).
% -- (subpr)
% TODO: this will not work in the interpreter!
% TODO: how are variables shared w.r.t. the rest of the body?
ctx_goal_trans('\6\predabs'(PEnvSig, Args0, Goal0, Term)) :- !,
	% TODO: missing expansion of arguments, missing metatypes
	call((
          goal :: accum <- Goal0,
	  args :: accum(Args1),
	  mexpand__subpr_ctx__2(PEnvSig),
	  Goal = ~goal
        )),
	mexpand__append(Args0, Args1, Args),
	mexpand__body(Goal, Goal2),
	residue.add('$subpr$'(Term, Args, Goal2)),
	trust_metatype(Term, primitive(pred(~mexpand__length(Args0), PEnvSig))).
% TODO: this will not work in the interpreter!
ctx_goal_trans('\6\predabs_static'(PEnvSig, Args0, Goal0, Term)) :- !,
	% TODO: missing expansion of arguments, missing metatypes
	call((
          goal :: accum <- Goal0,
	  args :: accum(Args1),
	  mexpand__subpr_ctx__2(PEnvSig),
	  Goal = ~goal
        )),
	mexpand__append(Args0, Args1, Args),
	mexpand__body(Goal, Goal2),
	residue.add('$subpr_static$'(Term, Args, Goal2)).
% TODO: this will not work in the interpreter!
ctx_goal_trans('$predabs_call$'(Ctx, PredAbs, Args0)) :- !,
	PEnvSig = ~mexpand__resolve_pEnvSig(Ctx),
	call(( args :: accum(ArgsC), mexpand__predabs_call_ctx__2(PEnvSig) )),
	mexpand__append(Args0, ArgsC, Args),
	% TODO: $subpr_call$ is defined in ~/svn/ciaode/ciao/optim_comp/testsuite/contpass/c1.pl
	% TODO: ... that call does not do any copy_term and it can be optimized at low-level (i.e. placing missing arguments just were they must be)
	% TODO: missing expansion of arguments, missing metatypes
	mexpand__body0('$subpr_call$'(PredAbs, Args)).
% TODO: this will not work in the interpreter!
% TODO: internal, do not call!
ctx_goal_trans('$subpr_static_call$'(Term, Args)) :- !,
	% TODO: missing expansion of arguments, missing metatypes
	residue.add('$subpr_static_call$'(Term, Args)). % expanded at compiler__expand
% TODO: this will not work in the interpreter!
ctx_goal_trans('\6\predabs_static_call'(PEnvSig, PredAbs, Args0)) :- !,
	% TODO: missing expansion of arguments, missing metatypes
	call(( args :: accum(ArgsC), mexpand__predabs_call_ctx__2(PEnvSig) )),
	mexpand__append(Args0, ArgsC, Args),
	% TODO: do not use subpr_call but subpr_static_call
	mexpand__body0('$subpr_static_call$'(PredAbs, Args)).
% -- (continuations as fluids)
% TODO: this will not work in the interpreter!
ctx_goal_trans('$cont$'(ContName)) :- !,
	( pEnv.get(ContName, ContVal, _),
	  vEnv.match_exact(ContVal, primitive(pred(0, PEnvSig))) ->
	    true
	; mexpand__error(bug(continuation_has_wrong_context))
	),
	call(( args :: accum(Args), mexpand__predabs_call_ctx__2(PEnvSig) )),
	mexpand__body0('$ctx_value'(ContName, Cont)),
	mexpand__body0('$subpr_call$'(Cont, Args)).
% TODO: this will not work in the interpreter!
% note: (from a context without continuatinos) executes Goal (that needs continuations)
ctx_goal_trans('$enter_cont$'(PEnvPrj, Goal)) :- !,
	PEnvSig = ~mexpand__extract_pEnvSig(PEnvPrj),
	mexpand__enter_cont_ctx(PEnvSig, Inside, Outside),
	mexpand__body0('\6\predabs'(PEnvSig, [], Inside, Cont)),
	mexpand__body0('\6\ctx_enter'([pushv(conttrue, 'blt__any', Cont)], Goal)),
	mexpand__body0(Outside).
% TODO: this will not work in the interpreter!
% note: continuations are implicit arguments, but they could really be worker registers
% note: (from a context with continuations) pushes a new continuation named ContName and executes Goal
ctx_goal_trans('$catch_cont$'(Goal, Ctx, ContName, ContGoal)) :- !,
	PEnvSig = ~mexpand__resolve_pEnvSig(Ctx),
	mexpand__body0('\6\predabs'(PEnvSig, [], ContGoal, C)),
	mexpand__body0('\6\ctx_enter'([pushv(ContName, 'blt__any', C)], Goal)).
% TODO: this will not work in the interpreter!
% note: (from a context with continuations) ,/2 when GoalA requires continuations
ctx_goal_trans('$,cont$'(Ctx, Goal, ContGoal)) :- !, % jf-imod	
	PEnvSig = ~mexpand__resolve_pEnvSig(Ctx),
	mexpand__body0('\6\predabs'(PEnvSig, [], ContGoal, Cont)),
	mexpand__body0('\6\ctx_enter'([pushv(conttrue, 'blt__any', Cont)], Goal)).
}.

{
:- extends mexpscope.
:- fluid ignorefun :: any.
:- fluid residue :: accum.

% This split a 'scope'd goal in declarations and goals.

mexpand__ctx_scope(Goal) :-
	conj_to_slist(Goal, List),
	mexpand__filter_decls(List, Decls, Nondecls),
	list_to_conj(Nondecls, Goal2),
	mexpand__ctx_enter(Decls, Goal2).

mexpand__filter_decls(Xs, Decls, Nondecls) :-
	decls :: accum(Decls), 
	nondecls :: accum(Nondecls),
	mexpand__filter_decls__2(Xs).
{
% TODO: Emit errors if variables are declared twice
:- fluid decls :: accum.
:- fluid nondecls :: accum.
mexpand__filter_decls__2([]).
mexpand__filter_decls__2([X|Xs]) :-
	( nonvar(X), X = (Name :: accum(Val)) ->
	    % Declaration of a close accum
	    Decl = closed_accum(Name, Val),
	    decls.add(Decl)
	; nonvar(X), X = (Name :: revaccum(Val)) ->
	    % Declaration of a close rev accum
	    Decl = closed_revaccum(Name, Val),
	    decls.add(Decl)
	; nonvar(X), X = (Name :: Class) ->
	    % Declaration
	    Decl = pushv(Name, ~resolve_class(Class), _),
	    decls.add(Decl)
	; nonvar(X), X = (Name :: Class <- Val) ->
	    % TODO: Using <- syntax, but it is indeed unification
	    % Declaration and initialization
	    Decl = pushv(Name, ~resolve_class(Class), _),
	    decls.add(Decl),
	    Init =.. [Name, Val],
	    nondecls.add(Init)
	; nondecls.add(X)
	),
	mexpand__filter_decls__2(Xs).
}.
}.

% ---------------------------------------------------------------------------
% TODO: combination of iterators and scopes (make it simpler)

{
:- extends mexpscope.
:- fluid ignorefun :: any.
:- fluid residue :: accum.

mexpand__ctx_enter([], Goal) :- !,
	mexpand__body0(Goal).
mexpand__ctx_enter([A|As], Goal) :- !,
	mexpand__ctx_enter_(A, '\6\ctx_enter'(As, Goal)).

mexpand__ctx_enter_(pushv(CtxName, Class, CtxVal0), Goal) :- !, % jf-imod
	( nonvar(CtxVal0) ->
            mexpand__body0(term_basic:(CtxVal = CtxVal0))
	; CtxVal = CtxVal0
	),
	% TODO: include as a rt_exp predicate?
	( atom(CtxName) ->
	    ctxvar_alloc(CtxName, CtxVal, Class, MaybePrevious)
	; mexpand__error(bad_ctx_var_name(CtxName))
	),
	mexpand__body0(Goal),
        pEnv.unset(CtxName, MaybePrevious).
% TODO: 'accum' and 'revaccum' should not be hardwired
mexpand__ctx_enter_(closed_accum(CtxName, CtxVal), Goal) :- !, % push accum and close it
	mexpand__ctx_enter([pushv(CtxName, ~resolve_class(accum), CtxVal)], (Goal, '$ctx_value'(CtxName, []))).
mexpand__ctx_enter_(closed_revaccum(CtxName, CtxVal), Goal) :- !, % push empty accum and obtain the final result
	mexpand__ctx_enter([pushv(CtxName, ~resolve_class(revaccum), [])], (Goal, '$ctx_value'(CtxName, CtxVal))).
mexpand__ctx_enter_(Decl, _) :-
	mexpand__error(unknown_decl(Decl)).
}.

% ---------------------------------------------------------------------------

% Built-in binders
% TODO: merge with the rest of binder related code

% TODO: Disabled, enable it with an extra argument and add tests programs.
% mexpand__goal0('$ctx_on'(Binder, Goal)) :- !, % TODO: deprecate this syntax (see the next clause)
% 	mexpand__blt_binder(Binder, Goal).

{
:- extends mexpscope.
:- fluid ignorefun :: any.
:- fluid residue :: accum.

% Loops -- fix, those are built-in binders!
% TODO: infer loop ctx automatically (i.e. the first parameter of for_each, while and do_while)
% TODO: allow meta-pred syntax and use 'binders' optionally (ala "A proposed syntax for binders in Mizar")
mexpand__blt_binder(for_each(Ctx, Iter), Goal) :- !,
	mexpand__body0(~mexpand__for_each(Ctx, Iter, Goal)).
mexpand__blt_binder(do_while(Ctx, Cond), Goal) :- !,
	mexpand__body0(~mexpand__do_while(Ctx, Goal, Cond)).
mexpand__blt_binder(while(Ctx, Cond), Goal) :- !,
	mexpand__body0(~mexpand__while(Ctx, Cond, Goal)).
}.

{
:- extends mexpscope.
:- fluid ignorefun :: any.
% Expansion of loops
%
% TODO: use analysis info to infer maybe_empty and transform the code
%   accordingly if the first condition is true, then the condition
%   predabs is only called once and so it can be expanded after the
%   end of the code (that transforms a while into a do_while)
%
%   loop :- init, cond.
%   cond :- ( c -> ok ; true ).
%   ok :- code, cond.
mexpand__for_each(Ctx, Iter, Goal) := AR :-
	( nonvar(Iter), Iter = iter(Init, Cond, Next, MaybeEmpty) ->
	    true
	; mexpand__error(bad_iterator(Iter))
	),
	AR = ~mexpand__loop(Ctx, MaybeEmpty, Init, Cond, Goal, Next).

% Generate 'while' loop
mexpand__while(Ctx, Cond, Goal) := AR :-
	AR = ~mexpand__loop(Ctx, yes, true, Cond, Goal, true).

% Generate 'do-while' loop
% note: do-while is necessary since it may more optimal in very low level code (i.e. absmach definition)
mexpand__do_while(Ctx, Goal, Cond) := AR :-
	AR = ~mexpand__loop(Ctx, no, true, Cond, Goal, true).

mexpand__loop(PEnvPrj, MaybeEmpty, Init, Cond, Goal, Next) := AR :-
	% Inside unifies Vi with the value of context var vi, Outside sets the context var vi to Vi (for all vi in context)
	PEnvSig = ~mexpand__extract_pEnvSig(PEnvPrj),
	mexpand__enter_cont_ctx(PEnvSig, Inside, Outside),
	%
	CondR = ( Cond ->
	            '\6\predabs_static_call'(PEnvSig, LoopDef, [])
		; % finish loop
		  Inside
		),
	LoopR = ( Goal,
	          Next,
		  '\6\predabs_static_call'(PEnvSig, CondDef, [])
		),
	PD = ( '\6\predabs_static'(PEnvSig, [], CondR, CondDef),
	       '\6\predabs_static'(PEnvSig, [], LoopR, LoopDef)
	     ),
	% TODO: this was a todo in ptoc__impcomp: obtain MaybeEmpty automatically from the previous scheme: the first cond is true and inline after next the second cond: use builtins for iterators and properties for iterators! do not expand them during this analysis
	FirstDef = ( MaybeEmpty = yes ? CondDef % start by the condition check
		   | MaybeEmpty = no ? LoopDef % enter the loop, the initial status of condition is always true
		   ),
	%
	AR = ( PD,
	       Init,
	       '\6\predabs_static_call'(PEnvSig, FirstDef, []),
	       Outside
	     ).

mexpand__maplistn(PEnvPrj, Lists, Elems, Goal) := AR :-
	% Inside unifies Vi with the value of context var vi, Outside
	% sets the context var vi to Vi (for all vi in context)
	PEnvSig = ~mexpand__extract_pEnvSig(PEnvPrj),
	mexpand__enter_cont_ctx(PEnvSig, Inside, Outside),
	%
	( maplistn__unifs(Lists, Elems, Args, Args0, UNil0, UCons0) ->
	    UNil = ~list_to_conj(UNil0),
	    UCons = ~list_to_conj(UCons0)
	; mexpand__error(bad_spec_in_maplistn(Lists, Elems)),
	  fail
	),
	LoopR = ( UNil,
	          Inside
		; UCons,
		  Goal,
		  '\6\predabs_static_call'(PEnvSig, LoopDef, Args0)
		),
	AR = ( '\6\predabs_static'(PEnvSig, Args, LoopR, LoopDef),
	       '\6\predabs_static_call'(PEnvSig, LoopDef, Lists),
	       Outside
	     ).
}.

maplistn__unifs([], [], [], [], [], []).
maplistn__unifs([_|Lists], [X|Elems], [Arg|Args], [Arg0|Args0], [Arg = [] | UNil], [Arg = [X|Arg0] | UCons]) :-
	var(X),
	maplistn__unifs(Lists, Elems, Args, Args0, UNil, UCons).

% ---------------------------------------------------------------------------
% Helper predicates to introduce contexts in continuations

{
:- fluid pEnv :: p_env + u.
:- fluid args :: accum.
:- fluid goal :: accum.
mexpand__subpr_ctx__2(PEnvSig) :-
	maplist(([u(pEnv), args, goal] -> ''(PSig) :-
	  mexpand__subpr_ctx__3(PSig)
        ), PEnvSig).

mexpand__subpr_ctx__3(PSig) :- !,
	psig_decomp(PSig, CtxName, Class, _CtxMode, _),
	% TODO: correct? CtxMode not used?
	% TODO: there should be a better way to do it
	trust(Class instance_of module_x),
	( Class.mutable_statemodel ->
	    args.add(Var),
	    goal <- '\6\ctx_enter'([pushv(CtxName, Class, Var)], ~goal)
	; true
	).
}.

{
:- fluid pEnv :: p_env + u.
mexpand__enter_cont_ctx(PEnvSig, Inside, Outside) :-
	inside :: m_any <- true,
	outside :: m_any <- true,
	mexpand__enter_cont_ctx__2(PEnvSig),
	Inside = ~inside, Outside = ~outside.
{
:- fluid inside :: m_any.
:- fluid outside :: m_any.
mexpand__enter_cont_ctx__2(PEnvSig) :-
	maplist(([u(pEnv), inside, outside] -> ''(PSig) :-
	  mexpand__enter_cont_ctx__3(PSig)
        ), PEnvSig).

mexpand__enter_cont_ctx__3(PSig) :- !,
	psig_decomp(PSig, CtxName, Class, _CtxMode, _),
	trust(Class instance_of module_x),
	% TODO: correct? CtxMode not used?
	( Class.mutable_statemodel ->
	    inside <- (~inside, '$ctx_value'(CtxName, V)),
	    outside <- (~outside, '$ctx_set'(CtxName, V))
	; true
	).
}.
}.

{
:- fluid pEnv :: p_env + u.
:- fluid args :: accum.
mexpand__predabs_call_ctx__2(PEnvSig) :-
	maplist(([u(pEnv), args] -> ''(PSig) :-
	  mexpand__predabs_call_ctx__3(PSig)
        ), PEnvSig).

mexpand__predabs_call_ctx__3(PSig) :- !,
	psig_decomp(PSig, CtxName, Class, _CtxMode, _),
	trust(Class instance_of module_x),
	% TODO: correct? CtxMode not used?
	( Class.mutable_statemodel ->
	    ( pEnv.get(CtxName, Val, PrevClass) ->
	        ( compatible_classes(PrevClass, Class) -> true
		; mexpand__error(pa_ctx_var_not_compatible(Var, PrevClass, Class))
		)
	    ; mexpand__error(pa_ctx_var_not_in_scope(Var))
	    ),
	    args.add(Val)
	; true
	).
}.

% ---------------------------------------------------------------------------

mexpand__append([], L, L).
mexpand__append([E|Es], L, [E|R]) :- mexpand__append(Es, L, R).

% TODO: equivalent version?
%mexpand__length(L, N) :- n :: m_int <- 0, maplist(([u(n)] -> ''(_) :- n.inc(1)), L).
mexpand__length(L, N) :- mexpand__length_(L, 0, N).

mexpand__length_([], I, I).
mexpand__length_([_|L], I0, I) :- I1 is I0+1, mexpand__length_(L, I1, I).

% ===========================================================================
% Functional expansion

%:- module(functional_expand, [], ['functions/ops']).

:- include(library('fsyntax/ops')).

%:- use_module(library(terms), [copy_args/3]).
%:- use_module(library(aggregates)).
%:- use_module(library(lists)).

% TODO: backport missing fsyntax features (ciaode/ciao/library/fsyntax)
% TODO: do not expand exps twice!

% Pre-expansion of functional notation in arguments:
% - The goals added to the body are not expanded.

{
:- fluid ignorefun :: any.
do_funexp :-
	mexpand__option(functional_expand),
	false = ~ignorefun. % do not apply functional expand twice
}.

defunc_clause_head(Head0, FuncBody, Head, Body) :- nonvar(Head0), Head0 = (FuncHead := FuncValue), !,
	call((
          unexp_g :: m_any <- AddBody,
	  defunc_head(FuncHead, NewFuncHead),
	  RestBody = ~unexp_g
        )),
	LastBody = ('\6\unif_ret'(NewFuncValue, FuncValue), true),
	Head = ~add_to_term(NewFuncHead, NewFuncValue),
	( FuncBody = true ->
	    LastBody = RestBody,
	    del_last_true(AddBody, Body)
	; RestBody = FuncBody,
	  concat_bodies(AddBody, LastBody, Body)
	).
defunc_clause_head(Head, Body, NewHead, NewBody) :- !,
	call((
          unexp_g :: m_any <- Body0,
	  defunc_head(Head, NewHead),
	  Body = ~unexp_g
        )),
	( Body = true ->
	    del_last_true(Body0, NewBody)
	; NewBody = Body0
	).

arith_flag := ( mexpand__eval_arith ? true | false ).

{
:- fluid unexp_g :: m_any.
defunc_head(Head, NewHead) :-
        functor(Head, F, A),
        functor(NewHead, F, A),
        defunc_args0(1, A, Head, NewHead).

% TODO: DO NOT DUPLICATE CODE! see defunc_args
defunc_args0(I, N, _, _) :- I > N, !. 
defunc_args0(I, N, T0, T1) :-
        arg(I, T0, A0),
        arg(I, T1, A1),
        I1 is I+1,
        defunc_exp0(A0, A1),
        defunc_args0(I1, N, T0, T1).

% TODO: DO NOT DUPLICATE CODE! see defunc_exp
defunc_exp0(V, V1) :- var(V), !, V1 = V.
defunc_exp0(^T0, T1) :- nonvar(T0), !,
        functor(T0, F, A),
        functor(T1, F, A),
        defunc_args0(1, A, T0, T1).
defunc_exp0(^^(T), ^^(T)) :- !.
%defunc_exp0(^(A.B), _) :-
%	mexpand__trace(found_non_escaped_dot(A,B)), fail.
defunc_exp0(Fun, V) :- ~arith_flag = true, arith_exp(Fun), !,
	unexp_g_add((V = Fun)).
defunc_exp0(Fun, V) :- needs_funexp(Fun, _QM, _Fn), !,
	unexp_g_add((V = Fun)).
defunc_exp0(T0, T1) :-
        functor(T0, F, A),
        functor(T1, F, A),
        defunc_args0(1, A, T0, T1).

unexp_g_add(Goal) :-
	~unexp_g = (Goal, Rest), unexp_g <- Rest.
}.

{
:- extends mexpscope.
:- fluid ignorefun :: any.
:- fluid residue :: accum.

defunc_args([], _, _, []).
defunc_args([A0|As0], [MT|MTs], Arith, [A1|As1]) :-
	% Functional expansion skips arguments with 'goal' metatype,
	% since they are not normal arguments. Functions inside those
	% arguments are expanded later. E.g. findall(X, X = ~foo, Xs)
	% is expanded as findall(X, foo(X), Xs), and not as
        % (foo(T), findall(X, X = T, Xs)).
	( MT = pred(0,[]) -> A1 = A0
	; MT = primitive(pred(0,[])) -> A1 = A0
	; defunc_exp(A0, Arith, A1)
	),
        defunc_args(As0, MTs, Arith, As1).

defunc_exp(V, _Arith, V1) :- var(V), !, V1 = V.
defunc_exp(^T0, Arith, T1) :- nonvar(T0), !, % note: this is the term (^_)
        functor(T0, F, A),
        functor(T1, F, A),
	T0 =.. [_|Args0],
	T1 =.. [_|Args1],
	MTs = ~default_metatypes(A),
        defunc_args(Args0, MTs, Arith, Args1).
defunc_exp(^^(T), _Arith, ^^(T)) :- !.
%defunc_exp(^(A.B), _Arith, _) :- % note: this escapes (_._)
%	mexpand__trace(found_non_escaped_dot(A,B)), fail.
defunc_exp(Fun, true, V) :- arith_exp(Fun), !,
        functor(Fun, F, A),
        functor(NFn, F, A),
	Fun =.. [_|Args0],
	NFn =.. [_|Args1],
	MTs = ~default_metatypes(A),
        defunc_args(Args0, MTs, false, Args1),
	call((
          ignorefun :: any <- true,
	  mexpand__body0((V is NFn))
        )).
defunc_exp(Fun, _Arith, V) :- needs_funexp(Fun, Qual, Fn), !,
	defunc_fun(Qual, Fn, V).
defunc_exp(T0, Arith, T1) :-
        functor(T0, F, A),
        functor(T1, F, A),
	T0 =.. [_|Args0],
	T1 =.. [_|Args1],
	MTs = ~default_metatypes(A),
        defunc_args(Args0, MTs, Arith, Args1).
}.

{
:- extends mexpscope.
:- fluid qual :: any.
:- fluid ignorefun :: any.
:- fluid residue :: accum.
	
% **Expansion of functional notation**

funexp_goal_trans(^^(G)) :- !, % Avoid functional expand in all the goal
        ignorefun :: any <- true,
	mexpand__body0(G).
funexp_goal_trans('\6\unif_ret'(NewFuncValue, FuncValue)) :- !,
%	mexpand__trace(ur(NewFuncValue, FuncValue)),
        ArithFlag = ~arith_flag,
	% TODO: make sure that true/0 is removed if NewGoal = []
	defunc_exp(FuncValue, ArithFlag, NewFuncValue).
funexp_goal_trans((U1 = U2)) :- % Avoid intermediate unification in (X = ~foo => foo(X))
        (V = U1, Fun = U2 ; V = U2, Fun = U1),
        var(V), nonvar(Fun),
        needs_funexp(Fun, Qual, Fn), !,
        defunc_fun(Qual, Fn, V).
}.

{
:- extends mexpscope.
:- fluid ignorefun :: any.
:- fluid residue :: accum.

% note: delay expansion so that metatypes are handled correctly
defunc_fun(Qual, Fn, V) :-
	G = ( var(Fn) ?
                call(Fn)
	    | Fn = ^((CondThen | Else)),
	      nonvar(CondThen), CondThen = ^((Cond ? Then)),
	      Qual = qual_none ?
	        (Cond -> V = Then ; V = Else)
	    | Fn = ^((Cond ? Then)),
	      Qual = qual_none ?
	        (Cond -> V = Then)
	    | Fn = ^((A | B)),
	      Qual = qual_none ?
	        (V = A ; V = B)
	    | ~apply_qual(Qual, ~add_to_term(Fn, V))
	    ),
	call((
          ignorefun :: any <- false,
	  mexpand__body0(G)
        )).
}.

arith_exp(-(_)).
arith_exp(+(_)).
arith_exp(--(_)).
arith_exp(++(_)).
arith_exp(+(_,_)).
arith_exp(-(_,_)).
arith_exp(*(_,_)).
arith_exp(/(_,_)).
arith_exp(//(_,_)).
arith_exp(rem(_,_)).
arith_exp(mod(_,_)).
arith_exp(#(_,_)).
arith_exp(/\(_,_)).
arith_exp(\/(_,_)).
arith_exp(\(_)).
arith_exp(<<(_,_)).
arith_exp(>>(_,_)).
arith_exp(integer(_)).
arith_exp(truncate(_)).
arith_exp(float(_)).
arith_exp(gcd(_,_)).
arith_exp(abs(_)).
arith_exp(sign(_)).
arith_exp(float_integer_part(_)).
arith_exp(float_fractional_part(_)).
arith_exp(floor(_)).
arith_exp(round(_)).
arith_exp(ceiling(_)).
arith_exp(**(_,_)).
arith_exp(exp(_)).
arith_exp(log(_)).
arith_exp(sqrt(_)).
arith_exp(sin(_)).
arith_exp(cos(_)).
arith_exp(atan(_)).

%needs_funexp(QM:Fun, Fun, qual_m(QM)) :- !,
%	nonvar(Fun), mexpand__fun_eval(Fun, QM).
needs_funexp(X, Qual, X1) :- X = ~funcall(X0), !,
	get_qualifier(X0, Qual, X1).
needs_funexp(X, Qual, X0) :- nonvar(X), X = ^((_ | _)), !,
	X0 = X, Qual = qual_none.
needs_funexp(X, Qual, X0) :- nonvar(X), X = ^((_ ? _)), !,
	X0 = X, Qual = qual_none.
% Note: disabled X.M as functional expression, use ~X.M instead
% TODO: Allow X.M if M is defined as a function in X (see mexpand__fun_eval)
% TODO: X.M should expand like in an atom-based module system
%needs_funexp(^(X.M), X0, Qual) :- !,
%	mexpand__trace(fev(X,M)),
%	X0 = M, Qual = qual_o(X).
needs_funexp(X, qual_none, X) :-
	functor(X, F, A),
	mexpand__fun_eval(F, A).

concat_bodies(V, B, NB) :- var(V), !,
        del_last_true_(B, V, NB).
concat_bodies((G, Gs), B, (G, NB)) :- !,
        concat_bodies(Gs, B, NB).
concat_bodies(G, B, NB) :-
        del_last_true_(B, G, NB).

% del_last_true(Goal, NewGoal) :- Goal is a sequence whose last element is
% 'true', NewGoal the same sequence without this element, except if it is
% the only element 

del_last_true(true, true).
del_last_true((G, Gs), NG) :-
        del_last_true_(Gs, G, NG).

del_last_true_(true, G, G).
del_last_true_((G,Gs), G0, (G0,NG)) :-
        del_last_true_(Gs, G, NG).

% Returns a term equal to T but with argument V
% added at the end.
add_to_term(T, V) := NT :-
        functor(T, F, A),
        A1 is A+1,
        functor(NT, F, A1),
        arg(A1, NT, V),
        unify_args(A, T, NT).

unify_args(0, _, _) :- !.
unify_args(I, F1, F2) :-
	arg(I, F1, X),
	arg(I, F2, X),
	I1 is I-1,
	unify_args(I1, F1, F2).

% ===========================================================================
% An environment of variables, relating each one to its metatype.
%
% Note: The metatype of a variable is unaltered in all its scope, by
%   definition. Checks and transformations are performend only at the
%   scope boundaries. Other notion of types must be used (even at the
%   same time) if you want to capture more precise information about
%   the shape of data at each program point (descriptively, rather
%   than prescriptively).

:- class v_env {
    :- '$statemodel'(single).
    :- '$raw_state'. % note: this module manages self directly

    % TODO: missing instance_of__/1

    % A dictionary with the metatype of variables
    
    % note: this is used to implemente a very naive analysis, where
    % only the metatype of head variables is kept.

    :- constructor empty_/0.
    :- pred empty_/0 # "Empty dictionary".
    empty_.

%    :- public init/2.
    :- pred init(+Ts, +As) # "Initialize a dictionary with metatypes
       @var{Ts} for variables @var{As}".
    init(deftypes, _) :- !.
    init(Ts, As) :-
        maplist((''(T, A) :-
          ( T = out(_) -> % TODO: set as '?' (those are input metatypes)
              true
          ; set(A, T)
          )
        ), Ts, As).

%    :- public set/2.
    :- pred set(+A, +T) # "Set the metatype of @var{A} to @var{T}".
    set(A, T) :-
        ( \+ T == '?', var(A) ->
            list_lookup(~self, A, T)
        ; true
        ).

%    :- public match/3.
    :- pred match0(+A, +Metatype, -Metacast) # "@var{Metacast} is the
       map that translates the representation of @var{A} as an element
       of the given @{Metatype} (if possible)".
    match0(A, Metatype) := Metacast :-
        Metatype0 = ~get(A),
	Metacast = ~simple_metacast(Metatype0, Metatype).

    :- static simple_metacast/3.
    simple_metacast(Metatype0, Metatype) := Metacast :-
        Metacast = ( Metatype = Metatype0 ? identity
		   | Metatype = primitive(Metatype0) ? unbox
		   | primitive(Metatype) = Metatype0 ? box
		   ).

    % Equivalence of metatypes:
    %
    %   pred(0) == goal
    %   pred(N, none) == pred(N)

    :- pred match_any(+A, +Metatype, -Metacast) # "Match for
       @var{Metatype)}".
    % TODO: does not accept variants (and it should)
    match_any(A, Metatype) := Metacast :-	
        ( Metatype0 = ~get(A) -> true ; Metatype0 = '?' ),
	( Metacast0 = ~simple_metacast(Metatype0, Metatype) ->
	    % no runtime exp is required
	    Metacast = Metacast0
	; Metacast = defer(Metatype0, Metatype)
	).

    % TODO: improve (missing context)
    :- pred match_goal(+A, -Metacast) # "Match for
       @tt{primitive(pred(0,[]))} (and variants)".
    match_goal(A) := ~match_any(A, primitive(pred(0,[]))).

    % TODO: improve (Context is in/out)
    % TODO: change 'deftypes' to allow metatypes in pred metatype! (e.g. pred(?,goal))
    :- pred match_predN(+A, Metatype, -Metacast) # "Match for
       @tt{primitive(pred(_,_))} @var{Metatype}".
    match_predN(A, primitive(pred(N, PEnvSig)), Metacast) :-
	MT = primitive(pred(N, PEnvSig)),
        ( Metatype0 = ~get(A) -> true ; Metatype0 = '?' ),
	( Metacast0 = ~simple_metacast(Metatype0, MT) ->
	    % no runtime exp is required
	    Metacast = Metacast0
	; Metacast = defer(Metatype0, MT),
	  % TODO: this is unsound:
	  %  - we assume default metatypes (we should not)
          %  - we assume 'none' context (we should not)
          %  Any solution for this? We should at least check at runtime that context is 'none'.
	  PEnvSig = []
	).

    :- pred match_exact(+A, +Metatype) # "@var{A} matches
       @var{Metatype} exactly (with an @em{identity} metatype)".
    match_exact(A, Metatype) :-
        Metatype0 = ~get(A),
	Metacast0 = ~simple_metacast(Metatype0, Metatype),
	Metacast0 = identity.

%    :- public get/2.
    :- pred get(+A, -Metatype) # "@var{A} has metatype @var{Metatype}".
    get(A) := Metatype :-
        list_get(~self, A, Metatype0),
	Metatype = Metatype0.

    % TODO: those are private
    :- static list_lookup/3.
    list_lookup(Dic, K, V) :- var(Dic), !,
        Dic = [K-V|_].
    list_lookup([K0-V0|Dic], K, V) :-
        ( K0 == K ->
            V = V0
        ; list_lookup(Dic, K, V)
        ).

    :- static list_get/3.
    list_get(Dic, _, _) :- var(Dic), !, fail.
    list_get([K0-V0|Dic], K, V) :-
            ( K0 == K ->
                V = V0
            ; list_get(Dic, K, V)
            ).
}.

% ===========================================================================
% Predicate-level environment for predicate symbols
%
% Currently it is used to store the definition of visible 'fluid'
% unary predicates. It could be generalized for other purposes.

:- class p_env {
    :- '$statemodel'(pair).
    :- '$raw_state'.

    % TODO: missing instance_of__/1

    {
    :- fluid m :: any.

    :- constructor simple_/0.
    simple_ :- self <- [v('$module', ~m, 'blt__any')].

    :- constant is_simple/0.
    is_simple :- ~self = [v('$module', ~m, 'blt__any')].
    }.

    % TODO: add annot to place a ctx var as first argument to do indexing properly
    % Get the value of a context var
    :- constant get/3.
    get(Name, Val, Class) :-
        PEnv0 = ~self,
        get__2(PEnv0, Name, Val, Class).

    :- constant get__2/4.
    get__2([X|Xs], Name, Val, Class) :-
        ( X = v(Name0,Val0,Class0), Name0 == Name ->
            Val = Val0,
            Class = Class0
        ; get__2(Xs, Name, Val, Class)
        ).

    % Push a new context var, giving the previous value
    set(Name, Val, Class, MaybePrevious) :-
        PEnv0 = ~self, self <- PEnv,
        set__2(PEnv0, Name, Val, Class, PEnv, MaybePrevious).

    :- static set__2/6.
    set__2([X|Xs], Name, Val, Class, Ys, MaybePrevious) :- !,
        ( X = v(Name0,Val0,Class0), Name0 == Name ->
            Ys = [v(Name,Val,Class)|Xs], MaybePrevious = yes(Val0,Class0)
        ; Ys = [X|Ys0], set__2(Xs, Name, Val, Class, Ys0, MaybePrevious)
        ).
    set__2([], Name, Val, Class, [v(Name,Val,Class)], no).

    % Unpush a context var using a previous value obtained from set/5
    unset(Name, MaybePrevious) :-
        PEnv0 = ~self, self <- PEnv,
        unset__2(PEnv0, Name, MaybePrevious, PEnv).

    :- static unset__2/4.
    unset__2([X|Xs], Name, MaybePrevious, Ys) :- !,
        ( X = v(Name0,_,_), Name0 == Name ->
            ( MaybePrevious = yes(Val,Class) ->
                Ys = [v(Name,Val,Class)|Xs]
            ; Ys = Xs
            )
        ; Ys = [X|Ys0], unset__2(Xs, Name, MaybePrevious, Ys0)
        ).
}.

% ---------------------------------------------------------------------------
% Signature for p_env entries

% Indexed: the argument of the fluid will be mapped as indexable arguments
%          of the predicate

% Decompose an entry from the pEnv table
psig_decomp(fluidsig(Name, Class, Opts, Indexed), Name, Class, Mode, Indexed) :-
	( Opts = none -> Mode = ~class_default_mode(Class)
	; Opts = ud -> Mode = usedef % TODO: check...
	; Opts = u -> Mode = use % TODO: check...
	; Opts = d -> Mode = def % TODO: check...
	; Opts = v -> Mode = void % TODO: check...
	; mexpand__error(bug(wrong_opts_in_pEnv(Name, Class, Opts))),
	  fail
	).

% A new fluidsig
psig_newfluid(CtxName, Class, Mode, Indexed) := R :-
	Opts = ~mode_to_opts(Mode),
	R = fluidsig(CtxName, Class, Opts, Indexed).

mode_to_opts(usedef, ud) :- !.
mode_to_opts(use, u) :- !.
mode_to_opts(def, d) :- !.
mode_to_opts(void, v) :- !.

% ---------------------------------------------------------------------------

% A p_env_sig projection modifier (changes the Mode)
p_env_prj_mod(X, Mode, N) :-
	( X = v(N) -> Mode = void
	; X = d(N) -> Mode = def
	; X = u(N) -> Mode = use
	; X = ud(N) -> Mode = usedef
	; atom(X) -> Mode = auto, N = X
	; mexpand__error(bug(bad_p_env_prj_mod(X))), fail % TODO: error, not a bug
	).
	  
% ---------------------------------------------------------------------------

{
% From pEnv, extract a p_env_sig projected from PEnvPrj
% TODO: bad O(n^2) complexity
% TODO: transform PEnvPrj into a plain list?
% TODO: share with mexpand__project_pEnvSig
:- fluid pEnv :: p_env + u.
mexpand__extract_pEnvSig(PEnvPrj) := PEnvSig :-
	pEnvSig :: accum(PEnvSig),
	maplist(([u(pEnv), pEnvSig] -> ''(X) :-
	  p_env_prj_mod(X, Mode0, N),
	  pEnv.get(N, _, Class),
	  Mode = ( Mode0 = auto ? ~class_default_mode(Class)
		 | Mode0
		 ),
	  pEnvSig.add(~psig_newfluid(N, Class, Mode, no))
	), PEnvPrj).
}.

{
% From PEnvSig, extract a p_env_sig projected from PEnvPrj
:- fluid pEnvSig :: accum.
% TODO: share with mexpand__extract_pEnvSig
% TODO: bad O(n^2) complexity
mexpand__project_pEnvSig(PEnvPrj, PEnvSig) :-
	trust(PEnvSig instance_of p_env_sig),
	maplist(([pEnvSig] -> ''(X) :-
	  p_env_prj_mod(X, Mode0, N),
	  ( self_name(N) ->
	      true % TODO: ignore at this moment, self is added later
	  ; PEnvSig.find_pSig(N, Class0, Indexed),
	    Mode = ( Mode0 = auto ? ~class_default_mode(Class0)
		   | Mode0
		   ),
	    pEnvSig.add(~psig_newfluid(N, Class0, Mode, Indexed))
	  )
	), PEnvPrj).
}.

{
:- fluid pEnvSig :: accum.
mexpand__keep_self(PEnvSig) :-
	trust(PEnvSig instance_of p_env_sig),
	( self_name(N), PEnvSig.find_pSig(N, Class, Indexed) ->
	    Mode = ~class_default_mode(Class),
	    pEnvSig.add(~psig_newfluid(N, Class, Mode, Indexed))
	; true
	).
}.

{
:- fluid pEnvSig :: accum.
% Change the mode of the self var
mexpand__selfprj_ctx([], _).
mexpand__selfprj_ctx([PSig0|PEnvSig], Mode) :-
	( PSig0 = fluidsig(N, Class, _, Indexed),
	  self_name(N) ->
	    % Found, modify using the new mode
	    PSig = ~psig_newfluid(N, Class, Mode, Indexed)
	; PSig = PSig0
	),
	pEnvSig.add(PSig),
	mexpand__selfprj_ctx(PEnvSig, Mode).
}.

% ---------------------------------------------------------------------------

:- class p_env_sig {
    :- '$statemodel'(single).
    :- '$raw_state'. % list of fluidsig

    % Find a definition for N
    find_pSig(N, Class, Indexed) :-
        find_pSig_(~self, N, Class, Indexed).

    :- static find_pSig_/4.
    find_pSig_([X|Xs], N, Class, Indexed) :-
	( X = fluidsig(N, Class0, _, Indexed0) ->
	    Class = Class0, Indexed = Indexed0
	; find_pSig_(Xs, N, Class, Indexed)
	).
}.

% ---------------------------------------------------------------------------

% Obtain the subset of variables in Xs that are used as fluid in the given p_env
% TODO: this predicate is inefficient
p_env__shared([], _) := [] :- !.
p_env__shared([v(_CtxName0,CtxVal0,_CtxClass0)|PEnv0], Xs) := Vs :-
	( member(X, Xs), X == CtxVal0 ->
	    Vs = [X|Vs0]
	; Vs = Vs0
	),
	Vs0 = ~p_env__shared(PEnv0, Xs).

p_env__meet(PEnv0, PEnvA, PEnvB, A0, A, B0, B, NewPEnv) :-
	call((
          unif_a :: accum(UnifA),
	  unif_b :: accum(UnifB),
	  new_pEnv :: accum(NewPEnv),
	  p_env__diff(PEnv0, PEnvA, PEnvB)
        )),
	mexpand__add_unifs(UnifA, A0, A),
	mexpand__add_unifs(UnifB, B0, B).

% ---------------------------------------------------------------------------

% Obtain the difference between PEnv0 (original), PEnvA (the first branch), PEnvB (the second branch)
% TODO: 'blt__m_any'+u here should be closed_accum
{
:- fluid unif_a :: accum.
:- fluid unif_b :: accum.
:- fluid new_pEnv :: accum.
p_env__diff([], [], []) :- !.
p_env__diff([v(CtxName0,CtxVal0,CtxClass0)|PEnv0], [v(CtxNameA,CtxValA,CtxClassA)|PEnvA], [v(CtxNameB,CtxValB,CtxClassB)|PEnvB]) :-
	( CtxName0 == CtxNameA, CtxNameA == CtxNameB -> % same ctx_var
	    ( CtxVal0 == CtxValA, CtxValA == CtxValB -> % unchanged in A and B
	        new_pEnv.add(v(CtxName0,CtxVal0,CtxClass0)),
		p_env__diff(PEnv0, PEnvA, PEnvB)
	    ; CtxVal0 == CtxValA -> % unchanged in A
	        new_pEnv.add(v(CtxName0,CtxValB,CtxClassB)),
		unif_a.add('term_basic:='(CtxValB, CtxVal0)),
		p_env__diff(PEnv0, PEnvA, PEnvB)
	    ; CtxVal0 == CtxValB -> % unchanged in B
	        new_pEnv.add(v(CtxName0,CtxValA,CtxClassA)),
		unif_b.add('term_basic:='(CtxValA, CtxVal0)),
		p_env__diff(PEnv0, PEnvA, PEnvB)
	    ; % changed in A and B
	      new_pEnv.add(v(CtxName0,CtxValA,CtxClassA)),
	      CtxValA = CtxValB, % unify both new values
	      p_env__diff(PEnv0, PEnvA, PEnvB)
	    )
	; % impossible case (no new ctx_var can be introduced in branches)
	  % display(user_error, bad(CtxName0, CtxNameA, CtxNameB)), nl(user_error),
	  fail
	).
}.

% ---------------------------------------------------------------------------

% Connect with the last p_env (for head arguments)
p_env__finish(InitialPEnv, LastPEnv, FinalPEnv, A0, A) :-
	% TODO: fluid 'unifs' behaves as a closed_accum, but it is always closed after p_env__finish_diff... (i.e. a just list!, not an accumulator)
	% TODO: introduce new kind of accumulator?
	call((
          unifs :: accum <- Unifs,
	  p_env__finish_diff(InitialPEnv, LastPEnv, FinalPEnv)
        )),
	mexpand__add_unifs(Unifs, A0, A).

% ---------------------------------------------------------------------------

% Obtain the difference between PEnv0 (initial pEnv in head), PEnv1 (last in body), PEnv (final pEnv in head)
% (note that values in final pEnv does not appear in the body yet)
{
:- fluid unifs :: accum + u.
p_env__finish_diff([], [], []) :- !,
	~unifs = []. % TODO: introduce '$ctx_closed'(unifs)
p_env__finish_diff([v(CtxName0,CtxVal0,_CtxClass0)|PEnv0], [v(CtxName1,CtxVal1,_CtxClass1)|PEnv1], [v(CtxName,CtxVal,_CtxClass)|PEnv]) :-
	( CtxName0 == CtxName1, CtxName1 == CtxName -> % same ctx_var
	    ( CtxVal0 == CtxVal -> % unchanged from initial to final
	        % do nothing, it is not a 'blt__m_any'(...) context var
	        p_env__finish_diff(PEnv0, PEnv1, PEnv)
	    ; CtxVal0 == CtxVal1 -> % unchanged from initial to last
		unifs.add('term_basic:='(CtxVal, CtxVal0)), % emit unification to connect values in head
		p_env__finish_diff(PEnv0, PEnv1, PEnv)
	    ; % changed from initial to last
	      CtxVal1 = CtxVal, % unify value in head with last value
	      p_env__finish_diff(PEnv0, PEnv1, PEnv)
	    )
	; % impossible case (no new ctx_var can be introduced in branches)
	  % display(user_error, bad(CtxName0, CtxName1, CtxName)), nl(user_error),
	  fail
	).
}.

