:- module(_, [], [compiler(complang)]).

% ---------------------------------------------------------------------------
% ptoc__impcomp -- ImProlog compiler
% Author: Jose Morales

% ---------------------------------------------------------------------------
% A simple tracing annotation
%:- op(500, yfx, db).
%:- meta_predicate (? db goal).
%(A db X) :-
%	errlog:trace(['(', A, ') before: ', X]),
%	X,
%	errlog:trace(['(', A, ') after: ', X]).
% ---------------------------------------------------------------------------

:- use_module(library(aggregates), [findall/3]).
:- use_module(library(lists)).
:- use_module(library(sort)).
:- use_module(library(dict)).
:- use_module(compiler(frontend), [compiler_version/1]).
:- use_module(compiler(errlog)).
:- use_module(compiler(memoize)).
:- use_module(compiler(store)).
:- use_module(compiler(module_bin)).
:- use_module(compiler(list_common), [conj_to_list/2, list_to_disj/2]).
:- use_module(compiler(meta_syntax)).

:- use_module(compiler(write_c), [encode_symbol_a/2]).

:- use_module(compiler(module_exp)).
:- use_module(.(module_ipexp)).

:- public mixin absmach_ctx {
    :- fluid ipexp :: module_ipexp.
    :- fluid exp :: module_exp.
}.

{
:- extends absmach_ctx.
absmach_blt_dec(A,B,C) :-
	ipexp.blt_dec(A,B,C).
absmach_def_lowatom(A,B) :-
	ipexp.def_lowatom(A,B).
absmach_def_lowtype(A) :-
	ipexp.def_lowtype(A).
}.

% ---------------------------------------------------------------------------

% The state of the code generator
:- class codegen_s {
    :- attr imp :: m_any # "Determinism mode".
    :- attr g :: m_any # "Type dictionary for global variables".
    :- attr ops :: ftype_stream # "Format of the arguments".
    :- attr c :: contdic # "Continuation stack".
    :- attr t :: m_any # "Type dic".
    :- attr e :: m_any # "Mem table".

    :- constructor new_/1.
    new_(Imp) :-
        ~imp = Imp,
	~g = [],
	~ops = [],
	~c = ~contdic.empty,
	~t = [],
	~e = [].

    % TODO: implement (or simplify... shift is no longer used in user code, but internally here)
    %   the rules are: discard the branch(s) with empty continuation stack
    %                  the state of the other branchs must be the same (same format stream and mode)
    %
    % TODO: at least check that states are compatible!, emit a compile time error if they are not
    {
    :- extends absmach_ctx.
    :- constructor merge_/2.
    % TODO: metatypes checks?
    % :- meta_predicate merge_(codegen_s, codegen_s).
    merge_(Sa, Sb) :-
            % This is the (and-)composition of merge operations on
            % each state component.
            trust(Sa instance_of codegen_s),
            trust(Sb instance_of codegen_s),
	    ~imp = ~Sa.imp, % TODO: Sb.imp is ignored, correct?
	    ~g = ~tdic_merge(~Sa.g, ~Sb.g),
	    ~ops = ~ops_merge(~Sa.ops, ~Sb.ops),
	    ~c = ~c_merge(~Sa.c, ~Sb.c),
	    ~t = ~tdic_merge(~Sa.t, ~Sb.t),
	    ~e = ~edic_merge(~Sa.e, ~Sb.e).
    }.

    :- static ops_merge/3.
    ops_merge(OpsA, OpsB) := ( OpsA == OpsB ? OpsA | unknown ).
    %        ( OpsA == OpsB -> true ; ptoc__impcomp:error(['s.merge: different ops ', OpsA, ' and ', OpsB]) ),

    :- static meta_predicate get_global_type(out(ltype)).
    get_global_type('$emu_globals'). % TODO: store in the state?

    % -----------------------------------------------------------------------
    % Operations on named continuations in the state

    % TODO: list NCList of basic blocks should be outside S
    % TODO: should comparing the labels be enough? should the OldC be stored in the 'bblock(...)'?
    :- static c_merge/3.
    c_merge(Ca, Cb) := Cc :-
        ( Ca == Cb -> Cc = Ca
	; ptoc__impcomp:error(['s.merge: different continuations ', Ca, ' and ', Cb])
	).

    :- constant meta_predicate get_cont(?, out(nc_handler)).
    get_cont(ContName) := ~c.lookup(ContName).

    set_cont(ContName, NCId) :- c.replace(ContName, NCId).

    initial_c(Other, FailOther, FailinsOther, PushBacks) :-
        LT = ~nc_new(contcode_other(Other)),
        LE = ~nc_new(contcode_other(FailOther)),
        LF = ~nc_new(contcode_other(FailinsOther)),
        PushBacks = [LT, LE, LF],
        set_cont(true, LT),
        set_cont(fail, LE),
        set_cont(failinscont, LF).

    % unwind continuations
    % TODO: remove
    unwind_cont :-
        NCId = ~get_cont(true),
        ContDic1 = ~NCId.contdic,
	( ContDic1 = no_contdic -> true
        ; ContDic1 = ~contdic.f(A,_,_), is_nop_cont(A) -> true
        ; c <- ContDic1, unwind_cont
        ).

    :- constant cont_is_void/0.
    cont_is_void :-
        NCId = ~get_cont(true),
        ContCode = ~NCId.contcode,
	ContCode = contcode_other(void).

    :- constant cont_is_emuctx/0.
    cont_is_emuctx :-
        NCId = ~get_cont(true),
        ContCode = ~NCId.contcode,
        ContCode = contcode_other(specialcont(exitemu(_))).

    % -----------------------------------------------------------------------

    :- constant nc_new/2.
    nc_new(ContCode) := ~nc_handler.new(ContCode, ~c).

    :- constant nc_new_conj/2.
    nc_new_conj(Goal) := NCId :-
        NCId = ~nc_new(contcode_conj(Goal)).

    % -----------------------------------------------------------------------
    % Operation for the type dictionary

    set_type(Var, Type) :-
            trust(Var instance_of termvar),
            t <- ~vdic_set(~t, ~Var.name, Type).

    set_mut_type(Var, Type) :-
    %        s__get_t(T0),
    %        errlog:trace([s(Var,Type,T0)]),
            set_type(Var, mut(Type)).

    % TODO: new variables should have 'var' type...
    :- constant meta_predicate get_type(?, out(ltype)).
    get_type(Var, Type) :-
            trust(Var instance_of termvar),
            ( vdic_get(~t, ~Var.name, Type) ->
                true
            ; Type = any % this should be var
            ).

    % ----
    % type dictionary for members of the global structure
    % TODO: merge with previous and add 'structure member' information in the
    %   abstract domain

    set_global_t(Var, MutType, MutValType) :-
            g <- ~vdic_set(~g, Var, mutval(MutType, MutValType)).

    % TODO: new variables should have 'var' type...
    :- constant get_global_t/3.
    get_global_t(Var, MutType, MutValType) :-
            ( vdic_get(~g, Var, mutval(MutType, MutValType)) ->
                true
            ; MutType = any, MutValType = any % this should be var
            ).

    % TODO: do the same for all mutables! (not only global) (i.e. keep track of the top mutable type and the current mutable type)
    set_mutglobal_type(Var, MutType, ValType) :-
            set_global_t(Var, MutType, ValType).

    set_mutglobal_val_type(Var, ValType) :-
            get_global_t(Var, MutType, _),
            set_global_t(Var, MutType, ValType).

    % TODO: new variables should have 'var' type...
    :- constant get_mutglobal_val_type/2.
    get_mutglobal_val_type(Var, ValType) :-
            get_global_t(Var, _, ValType).

    % ----
    % mem dictionary

    set_mem(Var, Mem) :-
            trust(Var instance_of termvar),
            e <- ~vdic_set(~e, ~Var.name, Mem).

    :- constant get_mem/2.
    get_mem(Var, Mem) :-
            trust(Var instance_of termvar),
            ( vdic_get(~e, ~Var.name, Mem) ->
                true
            ; Mem = nomem % TODO: it means that the variable is not initialized - give an error if a variable with this mem is being emitted (but then partial evaluation should be made in a previous pass!)
            ).

}. % class s

% ---------------------------------------------------------------------------
% generic key-value table

% TODO: important note: with this representation variables names (they key of the table) cannot be unified (coalesced) because that breaks the invariant "list is ordered"
vdic_set([KV|KVs], Var, Val, KVs2) :- !,
	KV = K-_,
	compare(R, Var, K),
	( R = (=) ->
	    KVs2 = [Var-Val|KVs]
	; R = (<) ->
	    KVs2 = [Var-Val,KV|KVs]
	; R = (>) ->
	    KVs2 = [KV|KVs1],
	    vdic_set(KVs, Var, Val, KVs1)
	).
vdic_set([], Var, Val, [Var-Val]).

% TODO: doc: generic key-value table
vdic_get([KV|KVs], Var, Val) :- !,
	KV = K-Val0,
	compare(R, Var, K),
	( R = (=) -> Val = Val0
	; R = (<) -> fail
	; R = (>) -> vdic_get(KVs, Var, Val)
	).

% ---------------------------------------------------------------------------

% merge type dictionaries
% TODO: use a parametric vdic procedure
{
:- extends absmach_ctx.
tdic_merge(KTas, []) := KTcs :- !, KTcs = KTas.
tdic_merge([], KTbs) := KTcs :- !, KTcs = KTbs.
tdic_merge(KTas0, KTbs0) := KTcs2 :- !,
	KTas0 = [KTa|KTas],
	KTbs0 = [KTb|KTbs],
	KTa = Ka-Va,
	KTb = Kb-Vb,
	( Ka == Kb ->
	    tdic_type_lub(Va, Vb, Vc),
	    KTcs2 = [Ka-Vc|KTcs1],
	    KTcs1 = ~tdic_merge(KTas, KTbs)
	; Ka @< Kb ->
	    KTcs2 = [KTa|KTcs1],
	    KTcs1 = ~tdic_merge(KTas, KTbs0)
	; Ka @> Kb ->
	    KTcs2 = [KTb|KTcs1],
	    KTcs1 = ~tdic_merge(KTas0, KTbs)
	).

tdic_type_lub(mutval(MutTypeA, Va), mutval(MutTypeB, Vb), C) :- !, % TODO: correct?
	trust(MutTypeA instance_of ltype),
	trust(MutTypeB instance_of ltype),
	trust(Va instance_of ltype),
	trust(Vb instance_of ltype),
	C = mutval(~MutTypeA.lub(MutTypeB), ~Va.lub(Vb)).
tdic_type_lub(Va, Vb, Vc) :-
	trust(Va instance_of ltype),
	trust(Vb instance_of ltype),
	Vc = ~Va.lub(Vb).
}.

% merge mem dictionaries
% TODO: use a parametric vdic procedure
edic_merge(KEas, []) := KEcs :- !, KEcs = KEas.
edic_merge([], KEbs) := KEcs :- !, KEcs = KEbs.
edic_merge(KEas0, KEbs0) := KEcs2 :- !,
	KEas0 = [KEa|KEas],
	KEbs0 = [KEb|KEbs],
	KEa = Ka-Ea,
	KEb = Kb-Eb,
	( Ka == Kb ->
	    mem_lub(Ea, Eb, Ec),
	    KEcs2 = [Ka-Ec|KEcs1],
	    KEcs1 = ~edic_merge(KEas, KEbs)
	; Ka @< Kb ->
	    KEcs2 = [KEa|KEcs1],
	    KEcs1 = ~edic_merge(KEas, KEbs0)
	; Ka @> Kb ->
	    KEcs2 = [KEb|KEcs1],
	    KEcs1 = ~edic_merge(KEas0, KEbs)
	).

% TODO: 'mem_conflict' means that the variable has two different mems
% - give an error if a variable with this mem is being emitted (but
% then partial evaluation should be made in a previous pass!)
mem_lub(Ea, Eb) := ( Ea == Eb ? Ea | mem_conflict ).

% ---------------------------------------------------------------------------
% The table of bb_container (for different sections of code)

:- class bb_tbl {
    :- attr subs :: m_dic # "Dictionary of bb_container".

    :- constructor new_/0.
    new_.

    :- constant meta_predicate sub(?, out(bb_container)).
    sub(SubId) := Sub :-
	subs.lookup(SubId, Sub),
	( var(Sub) ->
	    Sub = ~bb_container.empty
	; true
	).

    clean_sub(SubId) :-
	Sub = ~bb_container.empty,
	subs.replace(SubId, Sub).

    clean(Subs) :-
	( Subs = subs ->
	    clean_sub(sub)
	; Subs = subauto ->
	    clean_sub(subauto)
	; Subs = subs4 ->
	    clean_sub(sub),
	    clean_sub(sub2),
            clean_sub(sub3),
            clean_sub(sub4)
	; true
	).

    :- constant symboltbl/1.
    symboltbl := SS :-
        Sub = ~sub(subss),
	SS = ~Sub.codedic.
}.

% ---------------------------------------------------------------------------
% Container of bblocks that preserves a given relative ordering of bblocks
% TODO: find a better name!

% TODO: This is 'pair' but should work with 'single'.
% TODO: Using 'single' does not work.
:- class bb_container {
    :- attr codedic :: m_dic.
    :- attr ordlist :: m_any.
    :- attr priorlist :: m_any.
    :- attr listdic :: m_dic.

    :- constructor empty_/0.
    empty_.

    % insert a new entry
    :- constant insert/3.
    insert(Key, no, Value) :- !,
        codedic.lookup(Key, Value),
        orderinsert(Key).
    insert(Key, Prior, Value) :- !,
        codedic.lookup(Key, Value),
        priorinsert(Key, Prior).

    % insert a entry key in the sub table order list (if it was not
    % inserted before), with no value assigned yet
    % (private)
    orderinsert(Key) :-
        ( listdic.get(Key, _) ->
            true
        ; % TODO: ilist_insert has O(n) complexity !
          List = ~ordlist,
	  ilist_insert(List, Key),
          listdic.lookup(Key, true)
        ).

    % insert a entry key with priority in the sub table order list (if it
    % was not inserted before), with no value assigned yet
    % (private)
    priorinsert(Key, Prior) :-
        ( listdic.get(Key, _) ->
            true
        ; % TODO: ilist_insert has O(n) complexity !
	  List = ~priorlist,
          ilist_insert(List, (Prior,Key)),
          listdic.lookup(Key, true)
        ).

    % insert a decl entry in the sub table
    :- constant insert_decl/2.
    insert_decl(Key, Decl) :-
        insert(Key, no, subdecl(Decl)).

    % insert a bblock in the sub table
    :- constant insert_bb/3.
    insert_bb(Key, Prior, BBId) :-
        insert(Key, Prior, subcode(BBId)).

    % get the status from a code entry in the sub table
    :- constant get_bb/2.
    get_bb(Key) := BBId :-
        get0(Key, subcode(BBId)).

    % get the declaration from a decl entry in the sub table
    :- constant get_decl/2.
    get_decl(Key, Decl) :-
        get0(Key, subdecl(Decl)).

    % get the value of a entry in the sub table
    % (private)
    :- constant get0/2.
    get0(Key, Value) :-
        codedic.get(Key, Value).

    % get all the keys in the right order:
    %   ordered keys w.r.t. priority + unordered keys
    :- constant allkeys/1.
    allkeys := AllKeys :-
	% TODO: inefficient? try to sort only Prior, not the key!
	dl_to_list(~ordlist, ListA),
	dl_to_list(~priorlist, PriorList1),
	sort(PriorList1, PriorList2),
	only_key(PriorList2, ListB),
	AllKeys = ~append(ListB, ListA).
}.

ilist_insert(List, X) :- var(List), !, List = [X|_].
ilist_insert([_|List], X) :- ilist_insert(List, X).

% ---------------------------------------------------------------------------

{
:- extends absmach_ctx.
:- fluid s :: codegen_s + u.
getdef(PA, Args, Code) :-
	( PA = '$pa'(A), A instance_of termvar ->
	    ( var_predabs_code(A, Args, Code0, Props) ->
	        true
	    ; ptoc__impcomp:error(['inlining code from a variable that is not known to contain a predicate abstraction'])
	    ),
	    ( member(propargs, Props) ->
	        true
	    ; proparg_check_args(Args, p(Props,PA))
	    )
	; PA = '$pr'(GN,GA) ->
	    ( ipexp.get__op__prop(GN, GA, propargs) ->
	        true
	    ; proparg_check_args(Args, PA)
	    ),
	    ( ipexp.get_pred_def(PA, Args, Code0) ->
	        true
	    ; ptoc__impcomp:error(['not found code for ', PA])
	    )
	; ptoc__impcomp:error(['bad predicate descriptor ', PA])
	),
	% Pass the contdic as argument, when a special '$get_cont'/1 goal is in the first body literal
	% TODO: this way to pass contdic is not clean, use a context var in ImProlog?
	( nonvar(Code0), Code0 = (A, B), nonvar(A), A = '$get_cont'(ContDic) ->
	    ContDic = ~s.c,
	    Code = B
	; Code = Code0
	).
}.

% 'propargs' propagates the values in arguments to the body (ala C-macros)
% TODO: that is incorrect if the expression in the arguments have side-effects
% TODO: or the expression is the arguments cannot be evaluated in a different order
proparg_check_args(As, Where) :-
	( member(A, As),
	  \+ proparg_check_arg(A) ->
%	    true
	    errlog:bug(['warning: in ', Where, ', propagation of argument expressions may generate incorrect code in the presence of nonpure predicates (since it may change the number of times each argument is evaluated and the evaluation order)\n  Arguments: ', As])
	; true
	).

proparg_check_arg(A) :- var(A), !.
proparg_check_arg(A) :- A instance_of termvar, !.
proparg_check_arg(A) :- atom(A), !. % TODO: not correct... accept by now...

% ---------------------------------------------------------------------------
% TODO: pass code definition for gluecode to foreign code as a predicate property/code so that the gluecode definition is inserted here
% TODO: register property is a hack... should be based on the export list and if predicates needs to be accessible from bytecode

{
:- fluid exp :: module_exp.
:- public comp/5.
% TODO: clean up, improve control of .h exported declarations
comp(Absmach, Preds0, Bin, NativeCode, NativeHeader) :-
	trust(Absmach instance_of module_ipexp),
	ipexp :: any <- Absmach,
	tbl :: bb_tbl <- ~bb_tbl.new,
	comp__2(Preds0, Bin, NativeCode, NativeHeader).
}.

%% TODO: bug: ensure that an error is emitted if engine(prolog_sys) is loaded instead of library(prolog_sys)
%:- use_module(library(prolog_sys), [garbage_collect/0, statistics/0]).
%:- use_module(engine(internals), ['$gc_trace'/2]).

define_preds([]) := [].
define_preds([pred_def(NA, Code)|Xs]) := [PredId|PredIds] :-
	PredId = ~predicate_x.new(NA),
	PredId.set_code(Code),
	PredIds = ~define_preds(Xs).

{
:- extends absmach_ctx.
:- fluid tbl :: bb_tbl + u.
comp__2(Preds0, Bin, NativeCode, NativeHeader) :-
%	statistics,
%	'$gc_trace'(_, verbose),
	Exp = ~exp,
	findall(NativeInline, Exp.native_inline(NativeInline), NativeInlineList),
        % TODO: add a pass of prepass that also includes the shared versions to the predicate list? (and imported predicates protos?)
        findall(Lowtype, absmach_def_lowtype(Lowtype), PLowtypes),
	an_lowtypes(PLowtypes), % TODO: this may introduce new low macros

	%
	Absmach = ~ipexp,
        findall(pred_def(F/N, lowinst(Type, CName, CProps)), Absmach.entry_lowinst(F, N, Type, CName, CProps), LI),
	findall(pred_def(F/N, lowpred(Imp, Types, Modes, CName, imppred)), Absmach.entry_lowpred(F, N, Imp, Types, Modes, CName), LP),
	findall(symbol(CName, atom(N)), absmach_def_lowatom(N, CName), PSymbols),
	findall(R, get_plowmacros(R), PMacros),
	Preds = ~append_all([~define_preds(LI), ~define_preds(LP), Preds0]),
	%
	call(( cdecls :: accum(PredCode), gn_preds_code(Preds) )),
	SS = ~tbl.symboltbl,
	% Prototypes for external imported definitions
	call(( cdecls :: accum(NativeProtos), symboltbl__nativeprotos(SS) )),
	% Symbol declarations
	SymbolList = ~symboltbl__symbol_list(SS),
	AllSymbols = ~append(PSymbols, SymbolList),
	% Symbol declarations
	call(( cdecls :: accum(SymbolDecls), gn_symbol_decls(SymbolList) )),
	% Symbol registering code
        call(( ccode :: accum(BeginCode), gn_symbol_regs(AllSymbols) )),
        call(( ccode :: accum(EndCode), gn_symbol_unregs(AllSymbols) )),
	% TODO: define and use '$ctx_closed_accum_concat'([lowtypedefs,lowtypes], Lowtypes, gn_lowtypes(PLowtypes))
	call((
          lowtypedefs :: accum <- Lowtypes,
	  call(( lowtypes :: accum(Lowtypes2), gn_lowtypes(PLowtypes) )),
	  ~lowtypedefs = Lowtypes2
	)),
	call(( cdecls :: accum(Macros), gn_lowmacros__2(PMacros) )),
	call(( cdecls :: accum(PSymbolDecls), gn_symbol_decls(PSymbols) )),
	get_var_protos(PSymbolDecls, PublicSymbols),
	% 
        get_code_protos(PredCode, Protos),
        get_var_protos(PredCode, PublicProtos),
	Code = ~append_all([PSymbolDecls, SymbolDecls, NativeProtos, Protos, PredCode]),
	( Code = [], all_weak(NativeInlineList), BeginCode = [], EndCode = [] ->
	    NativeCode = []
	; call((
            cdecls :: accum(NativeCode),
	    emit_inline(NativeInlineList),
	    emit_cdecls(Code),
	    Module = ~exp.defines_module,
	    BeginCImp = det,
	    BeginCName = q(~encode_symbol_a(Module), init),
	    cdecls.add(declare(BeginCName, ~detdeftype(with_worker, BeginCImp, [], [], []), lambda(~add_context_args(with_worker, []), BeginCode))),
	    EndCImp = det,
	    EndCName = q(~encode_symbol_a(Module), end),
	    cdecls.add(declare(EndCName, ~detdeftype(with_worker, EndCImp, [], [], []), lambda(~add_context_args(with_worker, []), EndCode)))
          ))
	),
	ForeignIncludeInHeader = ~findall(include(Spec), Absmach.foreign_include_in_header(Spec)),
	NativeHeader0 = ~append_all([ForeignIncludeInHeader, Lowtypes, Macros, PublicSymbols, PublicProtos]),
        ( NativeHeader0 = [] ->
	    NativeHeader = []
	; Module = ~exp.defines_module,
	  add_headers(Bin),
	  gn_headerdefs(Module, Predefs, Postdefs),
	  NativeHeader = ~append_all([Predefs, NativeHeader0, Postdefs])
	).
%	statistics,
%	garbage_collect,
%	statistics.
}.

{
:- extends absmach_ctx.
an_lowtypes([]).
an_lowtypes([Name|Ls]) :-
	trust(Name instance_of ltype),
	( Name.is_struct_or_union ->
	    Name.analyze
	; true
	),
	an_lowtypes(Ls).
}.

% true if all the declarations are 'weak' 
% TODO: define what is a weak declaration: a declaration that does not add code or data (thus, only it needs to be included if the C file contains code or data)
all_weak(Xs) :-
	\+ some_not_weak(Xs).
some_not_weak(Xs) :-
	member(X, Xs), \+ X = weak(_), !.

append_all([]) := [].
append_all([X|Xs]) := ~append(X, ~append_all(Xs)).

% ---------------------------------------------------------------------------
% Native code to be included in the header

{
:- extends absmach_ctx.
add_headers(Bin) :-
	trust(Bin instance_of module_bin),
	( ipexp.lowinclude(predef_h, spec(PredefSpec)),
	    Bin.add(native_include_c_header(PredefSpec)),
	    fail
	; true
	),
	( ipexp.lowinclude(postdef_h, spec(PostdefSpec)),
	    Bin.add(native_include_c_header(PostdefSpec)),
	    fail
	; true
	).

gn_headerdefs(Module, Predefs, Postdefs) :-
	findall(include(H), lowinclude_c_header(predef_h, H), Predefs0),
	findall(include(H), lowinclude_c_header(postdef_h, H), Postdefs0),
	HeaderMark = ~atom_concat('__CIAO_HEADER__', ~encode_symbol_a(Module)),
	Predefs = [ifndef(HeaderMark), define(HeaderMark, 1)|Predefs0],
	Postdefs = ~append(Postdefs0, [endif]).

lowinclude_c_header(Where, H) :-
	ipexp.lowinclude(Where, Spec0),
	( Spec0 = spec(Spec) ->
	    spec_to_local_c_header(Spec, H)
	; Spec0 = foreign(H0) ->
	    H = H0
	; fail
	).
}.

% relative path of a c_header spec
spec_to_local_c_header(Spec, H) :-
	spec_to_default_module(Spec, M),
	H = ~atom_concat('engine/', ~atom_concat(M, '.h')).

% ---------------------------------------------------------------------------
% Compilation of predicates

{
:- extends absmach_ctx.
:- fluid tbl :: bb_tbl + u.
:- fluid cdecls :: accum.
gn_preds_code([]) :- !.
gn_preds_code([PredId|Xs0]) :-
	gn_pred_code(PredId),
	gn_preds_code(Xs0).

% TODO: iproto is used only to generate prototypes, no pred is registered % TODO: find a better solution?
gn_pred_code(PredId) :-
	trust(PredId instance_of predicate_x),
	Name = ~PredId.name,
	Code = ~PredId.code,
	gn_pred_code__2(Name, Code, PredId),
	!.
gn_pred_code(PredId) :-
	trust(PredId instance_of predicate_x),
	ptoc__impcomp:error(['gn_pred_code failed for: ', ~~(~PredId.name)]), fail.

gn_pred_code__2(_, Code, _PredId) :-
	( Code = bytecode(_) -> true
	; Code = int(_) -> true
	; Code = imacro -> true
	; Code = icode(a, _,_) -> true % TODO: this is wrong!!! this comes from impl_defined but we should not emit this code or analysis (and probably other parts) will be incorrect
	),
	!.
gn_pred_code__2(_Name, Code, _PredId) :-
	Code = pswitchcfun(X), !,
	SS = ~tbl.symboltbl,
	symboltbl__define(dswitchcfun(X), _Identifier, SS),
	% TODO: fix!
	cdecls.add(comment("fixme, declare switchcfun")).
gn_pred_code__2(Name, NCode, _PredId) :- NCode = lowpred(Imp, Types, Modes, CName, Impcode), !,
	Name = F/N,
	gn_lowpred('$pr'(F,N), Imp, Types, Modes, CName, Impcode).
gn_pred_code__2(Name, NCode, _PredId) :- NCode = lowinst(Type, CName, CProps), !,
	Name = F/N,
	gn_lowinst('$pr'(F,N), Type, CName, CProps).
gn_pred_code__2(Name, NCode, PredId) :-
	% Obtain properties
	trust(PredId instance_of predicate_x),
	Imp = ~PredId.get_prop(imp),
	%
	Modes = ~PredId.get_prop(argmodes),
	ImpTypes = ~PredId.get_prop(argimptypes),
	Mems = ~callee_argmems(PredId),
	%
	CvarImpTypes = ~filter_cvar(ImpTypes, Mems),
	CvarModes = ~filter_cvar(Modes, Mems),
	% Get label
	Label = ~pred_label(PredId),
	SS = ~tbl.symboltbl,
	translate_label(Label, Label2),
	% Remember to register it if necessary
	( true = ~PredId.get_prop(register) ->
	    Bits = ~exp.pred__defbits(Name),
	    symboltbl__define_predicate(registered(Imp, Name, Bits, Label2), _, SS)
	; true
	),
	% Register builtin if necessary
	( NCode = icd, Name = MF/A ->
	    % NOTE: some of these predicates are never registered (see
	    % module_exp:def__needs_bytecode_hook/1), since they are
	    % wrapped by a bytecode hook
	    symboltbl__define(dbuiltin(MF, A, Label2), _, SS),
	    NCode1 = NCode
	; NCode1 = NCode
	),
	% Call again compilation
	NCode2 = lowpred(Imp, CvarImpTypes, CvarModes, Label2, NCode1),
	gn_pred_code__2(Name, NCode2, PredId).

% Compile predicates as static data
gn_lowinst(InitPred, Type, CName, CProps) :-
	% TODO: use <- or = depending on the type? (mutable or not?)
	% Compact the initializer code into a single expression
	s :: codegen_s <- ~codegen_s.new(det),
	gn_lowinst__2(InitPred, Type, CName, CProps).
{
:- fluid s :: codegen_s + u.
gn_lowinst__2(InitPred, Type, CName, CProps) :-
	Code = ~getdef(InitPred, [This]),
	goal_to_init(Code, This, Expr, IsMut),
	% 
	gn_term(Expr, Value, Type1),
	Type3 = ( IsMut = yes ? mut(Type1) | Type1 ),
	trust(Type3 instance_of ltype),
	( Type3.consequence(Type) ->
	    true
	; ptoc__impcomp:error(['initializing global variable ', InitPred, ' with a value of type ', Type3, ', but it requires a ', Type])
	),
	CType1 = ~Type1.ctype,
	CType = ( CProps = just(CProps0) ? CType1 + CProps0 | CType1 ),
	( Value = '$uninit' -> % TODO: kludge...
	    cdecls.add(declare(CName, CType))
	; cdecls.add(declare(CName, CType, Value))
	).
}.
}.

% Analyze predicate code and transform it into a static initializer
% TODO: improve!
{
:- extends absmach_ctx.
goal_to_init(Code, This, Expr, IsMut) :-
	call((
          this :: any <- This,
	  type :: any <- Type,
          members :: accum(MembersR),
	  goal_to_init__2(Code)
        )),
	( var(Type) ->
	    ptoc__impcomp:error(['undefined type in lowinst'])
	; Type = '$unknown_mut' ->
	    ( MembersR = [mut_init(Val)] ->
	        Expr = Val, IsMut = yes
	    ; ptoc__impcomp:error(['required a single valid initializer for the mutable'])
	    )
	; trust(Type instance_of ltype), % TODO: trust/1 not colored
	  Kind = ~Type.kind, Kind = struct ->
	    Expr = ~funcall('$struct_init'(Type, MembersR)), IsMut = no
	; Expr = ~funcall(Type), IsMut = no
	).
{
:- fluid this :: any.
:- fluid type :: any.
:- fluid members :: accum.
goal_to_init__2((A,B)) :- !,
	goal_to_init__2(A),
	goal_to_init__2(B).
goal_to_init__2(true) :- !.
goal_to_init__2('$ftype_info'(Array)) :- !,
	ipexp.ftype_info(Array).
goal_to_init__2((This0 = ~funcall(Type0))) :- ~this == This0, !,
	Type = ~type,
	( var(Type) ->
	    Type = Type0
	; ptoc__impcomp:error(['redefining type in lowinst'])
	).
goal_to_init__2('<-'(A, V0)) :- nonvar(A), A = ~mcall(This0, X), !,
	( ~this == This0 ->
	    true
	; ptoc__impcomp:error(['initializer not valid in lowinst'])
	),
	members.add((X, V0)).
goal_to_init__2('<-'(This0, V0)) :- ~this == This0, !, % TODO: improve, this is a kludge
	Type = ~type,
	( var(Type) ->
	    Type = '$unknown_mut'
	; ptoc__impcomp:error(['redefining type in lowinst as a mutable'])
	),
	members.add(mut_init(V0)).
goal_to_init__2(A) :-
	ptoc__impcomp:error(['unsupported goal ', A, ' in lowinst']).
}.
}.

% ---------------------------------------------------------------------------
% size of a lowtype

{
:- extends absmach_ctx.
:- public type_size/2.
type_size(Type0, Size) :-
	trust(Type0 instance_of ltype),
	Size = ~Type0.size.
}.

:- public native_size/2.
native_size(int8, 1).
native_size(uint8, 1).
native_size(int16, 2).
native_size(uint16, 2).
native_size(int32, 4).
native_size(uint32, 4).
native_size(int64, 8).
native_size(uint64, 8).
native_size(flt32, 4).
native_size(flt64, 8).

% ---------------------------------------------------------------------------
% generation of low type definitions

{
:- extends absmach_ctx.
:- fluid tbl :: bb_tbl + u.
:- fluid lowtypedefs :: accum.
:- fluid lowtypes :: accum.
gn_lowtypes([]) :- !.
gn_lowtypes([Name|Ns]) :-
	trust(Name instance_of ltype),
	Name.ctype_decl,
	gn_lowtypes(Ns).
}.

% ---------------------------------------------------------------------------
% code generation as C macros

{
:- extends absmach_ctx.
:- fluid tbl :: bb_tbl + u.
get_plowmacros(R) :-
	( ipexp.entry_lowmacro(F, N, Imp, Types, CName, AMems),
	    R = lowmacro(F, N, Imp, Types, CName, AMems)
	; ipexp.entry_lowmacrocons(N, A, CName),
	    R = lowmacrocons(N, A, CName)
	; ipexp.entry_lowmacrofun(N,A,Types,CName),
	    R = lowmacrofun(N,A,Types,CName)
	).
}.

exp_type_name(Name0, Name) :- atom(Name0), !,
	Name = Name0.
exp_type_name(Name0, Name) :-
	nonvar(Name0), Name0 = m(Type, Name1), atom(Name1), !,
	atom_concat(Type, '_t__', X0),
	atom_concat(X0, Name1, Name).
exp_type_name(Name0, _) :-
	ptoc__impcomp:error(['unknown name in exp_type_name ', Name0]).

% TODO: incomplete (Args handling is not correct)
{
:- extends absmach_ctx.
:- fluid tbl :: bb_tbl + u.
gn_lowmacro__fun(F, N, Types, CName) := Macro :-
	N1 is N - 1, % arity-1 head arguments
	functor(Head, F, N1), Head =.. [_|Args],
	ipexp.get_fun_def(F, N, Args, Value1),
	%
	call((
	  s :: codegen_s <- ~codegen_s.new(det),
	  % lowmacro head
	  lowpred_amems(Types, AMems),
	  lowmacro__args(Args, Hs, AMems, Types),
	  % lowmacro body
	  gn_term(Value1, Value2, _),
	  Value = ( atomic(Value2) ? Value2 | quoted(Value2) ),
	  Macro = define(call(CName, Hs), Value)
        )).
}.

{
:- extends absmach_ctx.
:- fluid tbl :: bb_tbl + u.
:- fluid cdecls :: accum.
gn_lowmacros__2([]) :- !.
gn_lowmacros__2([lowmacro(F, N, Imp, Types, CName, AMems)|Ms]) :- !,
	( Macro = ~gn_lowmacro(F, N, Imp, Types, CName, AMems) ->
	    cdecls.add(Macro)
	; ptoc__impcomp:error(['compilation of lowmacro ', ~~(F/N), ' for imp ', Imp, ' and types ', Types, ' failed'])
	),
	gn_lowmacros__2(Ms).
gn_lowmacros__2([lowmacrocons(N,A,CName)|Ms]) :- !,
	A1 is A - 1, % arity-1 head arguments
	functor(Head, N, A1), Head =.. [_|Args],
	ipexp.get_fun_def(N, A, Args, Value),
	gn_lowmacros__cons__2(Value, CName, Args),
	gn_lowmacros__2(Ms).
gn_lowmacros__2([lowmacrofun(F, N, Types, CName)|Ms]) :- !,
	( Macro = ~gn_lowmacro__fun(F, N, Types, CName) ->
	    cdecls.add(Macro)
	; ptoc__impcomp:error(['compilation of lowmacro fun ', ~~(F/N), ' with types ', Types, ' failed'])
	),
	gn_lowmacros__2(Ms).

gn_lowmacros__cons__2(Value0, CName0, Args) :-
	nonvar(Value0), Value0 = ~funcall(Value1),
	nonvar(Value1), ipexp.exp_unfold(Value1, Value2),
	!,
	gn_lowmacros__cons__2(Value2, CName0, Args).
% TODO: generalize for other ## macro definitions
gn_lowmacros__cons__2(Value0, CName0, Args) :-
	Args = [X], var(X),
	Value0 = ~funcall('$keytable'(X0, Cases)),
	X0 == X,
	!,
	CName = ~atom_concat(CName0, '(KEY)'),
	Value = ~atom_concat(CName0, '__##KEY'),
	cdecls.add(define(CName, Value)),	
	call((
          s :: codegen_s <- ~codegen_s.new(det),
	  name :: any <- CName0,
	  gn_keytable_macro__1(Cases)
        )).
gn_lowmacros__cons__2(Value0, CName0, []) :-
	exp_type_name(CName0, CName),
	call((
          s :: codegen_s <- ~codegen_s.new(det),
	  gn_term(Value0, Value2, _)
        )),
	Value = ( atomic(Value2) ? Value2 | quoted(Value2) ),
	cdecls.add(define(CName, Value)).
}.

% Instruction opcodes
{
:- extends absmach_ctx.
:- fluid tbl :: bb_tbl + u.
:- fluid s :: codegen_s + u.
:- fluid name :: any.
:- fluid cdecls :: accum.
gn_keytable_macro__1([]) :- !.
gn_keytable_macro__1([X|Xs]) :- gn_keytable_macro__2(X), gn_keytable_macro__1(Xs).

gn_keytable_macro__2(case(Key, Value)) :-
	gn_term(Value, Opcode, _),
	Ins2 = ~atom_concat(~atom_concat(~name, '__'), Key),
	cdecls.add(define(Ins2, Opcode)).
}.

% TODO: a lowmacro is like a cached unfolding...
% TODO: replace macros by inline functions when possible? (and later, do all inlining here)
{
:- extends absmach_ctx.
:- fluid tbl :: bb_tbl + u.
gn_lowmacro(Ins, _, Imp, Types0, CName, AMems) := Macro :-
        s :: codegen_s <- ~codegen_s.new(Imp), % note: format is empty (shifts are not allowed) % TODO: correct?
	% TODO: right?
	GlType = mode,
	trust(GlType instance_of ltype),
	GlName = mode,
	MValType = ~GlType.coerce_from_atom(t_atom(r)),
        s.set_mutglobal_type(GlName, GlType, MValType),
	% lowmacro head
	lowmacro__args(As, Hs, AMems, Types0),
	% lowmacro body
	X =.. [Ins|As], Body = '$subctx'('$unfold'(X)),
	( Imp = nondet -> true,
	    MaybeTrueLab = no,
	    s.initial_c(void, ~fail_redmode(Imp), void, PushBacks)
	; Imp = det_with_failins ->
	    MaybeTrueLab = yes(Lab),
	    s.initial_c(macro(Lab), det_with_failins__failc, semidet__failc, PushBacks) % TODO: code for failinscont is incorrect
	; MaybeTrueLab = yes(Lab),
	  s.initial_c(macro(Lab), ~fail_redmode(Imp), void, PushBacks)
	),
	call((
	  cstat :: bb_code <- ~bb_code.new,
	  gn_conj(Body), gn_pushbacks_ncs(PushBacks),
	  ~cstat = FR1
	)),
	% note: quoted is necessary to avoid local label duplication (in gcc < 4) and detect missing ';'
	Macro = define(call(CName, Hs), quoted(~gn_c_block(macro, MaybeTrueLab, CName, FR1))).
}.

{
:- fluid s :: codegen_s.
lowmacro__args(Args, Hs, AMems, Types) :-
	lowmacro__args__2(0, _I, Args, Hs, AMems, Types).

lowmacro__args__2(I, I, [], [], [], []) :- !.
lowmacro__args__2(I0, I, [A3|As], [H|Hs], [AM|AMs], [T|Ts]) :- !,
	I1 is I0 + 1,
	number_codes(I0, ICodes),
	atom_codes(A2, "A"||ICodes),
	atom_concat('(', A2, A2q0),
	atom_concat(A2q0, ')', A2q),
	( AM = m ->
	    H = A2,
	    Mem = cvar(address(A2q))
	; H = A2,
	  Mem = ( T = goto_sentence ? cvar(A2) | cvar(A2q) ) % TODO: this is a kludge!!
	),
	A3 = ~termvar.new,
	var_mem_type(A3, Mem, T),
	lowmacro__args__2(I1, I, As, Hs, AMs, Ts).
}.

% ---------------------------------------------------------------------------
% code generation as C functions

lowpred_amems([],[]).
lowpred_amems([_|Ts],[c|AMs]) :- lowpred_amems(Ts,AMs).

{
:- extends absmach_ctx.
:- fluid tbl :: bb_tbl + u.
:- fluid cdecls :: accum.
gn_lowpred(Pred, Imp, Types, Modes, CName, NCode) :-
	ctypes_from_types(Types, CTypes),
	InCTypes = ~filter_mode(CTypes, Modes, in),
	OutCTypes = ~filter_mode(CTypes, Modes, out),
        ( NCode = iforeign(Desc) ->
	    Pred = '$pr'(F,N), Name = F/N,
	    foreign__gluecode:foreign_prototype(Desc, ForeignProto),
	    define_foreignproto(Name, ForeignProto),
	    foreign__gluecode:interface_function(Name, Desc, GlueCode),
	    CodeType = ~detdeftype(with_worker, Imp, InCTypes, OutCTypes, []),
	    cdecls.add(declare(CName, CodeType, lambda(~add_context_args(with_worker, []), GlueCode)))
	; NCode = icd ->
	    CodeType = ~detdeftype(with_worker, Imp, InCTypes, OutCTypes, []),
	    cdecls.add(declare(CName, CodeType))
	; NCode = iproto ->
	    CodeType = ~detdeftype(with_worker, Imp, InCTypes, OutCTypes, []),
	    cdecls.add(declare(CName, CodeType))
	; call((
            s :: codegen_s <- ~codegen_s.new(Imp), % note: format is empty (shifts are not allowed) % TODO: correct?
            ipexp.lowpred_spec(GlobalSpec),
	    ( GlobalSpec = global(GVar, GMutType, GValType0) ->
	        trust(GValType0 instance_of ltype),
	        GValType = ~GValType0.norm_eval,
	        s.set_mutglobal_type(GVar, GMutType, GValType)
	    ; true
	    ),
	    ( NCode = imppred ->
	        % pred body
	        Arity = ~length(Types),
	        Pred = '$pr'(GN, _),
	        functor(X, GN, Arity),
	        X =.. [GN|As],
	        Bodyb = '$subctx'('$unfold'(X)),
	        ( Imp = semidet, OutCTypes = [] ->
	            s.initial_c(semidet, semidet__failc, void, PushBacks),
		    Body = (Bodyb -> true ; fail) % TODO: kludge to avoid duplicated return statements % TODO: performancewise... does it worth it? (I did not detect any change)
	        ; Imp = det_with_failins, OutCTypes = [] ->
	            s.initial_c(det_with_failins, det_with_failins__failc, semidet__failc, PushBacks), % TODO: code for failinscont is incorrect
		    Body = '$catch_failins'(Bodyb, '$cont'(failinscont)) % TODO: kludge to avoid duplicated return statements % TODO: performancewise... does it worth it? (I did not detect any change)
	        ; Imp = det, OutCTypes = [_] ->
	        	  [OutType] = ~filter_mode(Types, Modes, out),
	        	  [OutArg] = ~filter_mode(As, Modes, out),
	            s.initial_c(det(OutArg,OutType), det__failc, void, PushBacks),
	        	  Body = Bodyb
	        ; Imp = det, OutCTypes = [] ->
	            s.initial_c(det, det__failc, void, PushBacks),
	        	  Body = Bodyb
	        ; ptoc__impcomp:error(['unknown value for \'imp\' property in lowpred generation'])
	        ),
	        UseWorker = ~get_context(GN, Arity),
	        InAs1 = ~add_context_args(UseWorker, InAs2),
	        get_foreign_props(GN, Arity, ForeignProps),
	        CodeType = ~detdeftype(UseWorker, Imp, InCTypes, OutCTypes, ForeignProps)
	    ; NCode = vcode(Comment, As, Body1),
	      cdecls.add(Comment),
	      Body = '$vsubctx'('$subctx4'(Body1)),
	      % TODO: TEMPORAL FOR BACKEND_C INTEGRATION
	      s.initial_c(nop, ~fail_redmode(Imp), void, PushBacks),
	      CodeType = ~detdeftype(with_worker, Imp, InCTypes, OutCTypes, []),
	      InAs1 = ~add_context_args(with_worker, InAs2)
	    ),
	    % pred head
	    ( ipexp.get__op__prop(GN, Arity, foreign_imported) ->
	        % no real code % TODO: a kludge? 
	        cdecls.add(declare(CName, CodeType))
	    ; InAs = ~filter_mode(As, Modes, in),
	      InTypes = ~filter_mode(Types, Modes, in),
	      OutAs = ~filter_mode(As, Modes, out),
	      OutTypes = ~filter_mode(Types, Modes, out),
	      pred__in_args(InAs, InTypes),
	      pred__out_args(OutAs, OutTypes),
	      gn_terms(InAs, InAs2, _),
	      ( call((
                  cstat :: bb_code <- ~bb_code.new,
		  gn_conj(Body),
		  gn_pushbacks_ncs(PushBacks),
		  ~cstat = Code0
                )) ->
	          true
	      ; ptoc__impcomp:error(['code generation failed for: ', Body]), fail
	      ),
	      cdecls.add(declare(CName, CodeType, lambda(InAs1, ~gn_c_block(cfun, no, CName, Code0))))
	    )
          ))
	).
}.

% ---------------------------------------------------------------------------

{
:- extends absmach_ctx.
get_foreign_props(GN, Arity, ForeignProps) :-
	ForeignProps = ~findall(ForeignProp, get_foreign_props__2(GN, Arity, ForeignProp)).

get_foreign_props__2(GN, Arity) :=
	( ipexp.get__op__prop(GN, Arity, foreign__static) ? static
	| ipexp.get__op__prop(GN, Arity, foreign__inline) ? inline
	).
}.

% TODO: merge with lowmacro__args?
{
:- fluid s :: codegen_s.

pred__in_args(Args, Types) :-
	i :: m_int <- 0,
	pred__in_args__2(Args, Types).
{
:- fluid i :: m_int + u.
pred__in_args__2([], []) :- !.
pred__in_args__2([A3|As], [T|Ts]) :- !,
	number_codes(~i, ICodes),
	atom_codes(A2, "A"||ICodes),
	Mem = cvar(A2),
	A3 = ~termvar.new,
	var_mem_type(A3, Mem, T),
	i.inc(1),
	pred__in_args__2(As, Ts).
}.

pred__out_args([], []) :- !.
pred__out_args([A3|As], [T|Ts]) :- !,
	A3 = ~termvar.new,
	var_nomem_type(A3, T),
	pred__out_args(As, Ts).
}.

% ---------------------------------------------------------------------------
% Declarations, code, and 'descendant' basic blocks

:- class bb_code {
    :- attr decls :: m_any # "Local declarations".
    :- attr code :: m_any # "Code sentences".
    :- attr bbs :: m_any # "Links to the used basic blocks".

    :- constructor f_/3.
    f_(A,B,C) :-
        ~decls = A,
	~code = B,
	~bbs = C.

    :- constructor new_/0.
    new_ :-
        ~decls = [],
	~code = '$nop',
	~bbs = [].

    emit_csingle(X) :-
  	code <- seq(~code, X).

    emit_cond_br(R0, ThenId, ElseId) :-
  	emit_csingle(cond_br(R0, ThenId, ElseId)).

    insert_declsub(Sub) :-
  	decls <- ~append(~decls, [decls(Sub)]).

    insert_codesub(Sub) :-
  	bbs <- ~append(~bbs, [code(Sub)]).

    % Push back an associated basic block
    insert_bb(Id) :-
  	bbs <- ~append(~bbs, [bb(Id)]).

    {
    :- fluid tbl :: bb_tbl + u.
    insert_subs(Subs) :-
        ( Subs = subs ->
            insert_codesub(~tbl.sub(sub))
        ; Subs = subauto ->
            insert_declsub(~tbl.sub(subauto))
        ; Subs = subs4 ->
            insert_declsub(~tbl.sub(sub3)),
            insert_codesub(~tbl.sub(sub2)),
            insert_codesub(~tbl.sub(sub)),
            insert_codesub(~tbl.sub(sub4))
        ; true
        ).
    }.
}.

% ---------------------------------------------------------------------------
% Continuation for code generation
%   - contain references to bblocks (supporting different versions)
%   - source code to execute
%   - previous continuation dictionary

% TODO: This class uses $mut__init (attributed variables) to store its
%       state, which makes it possible to rewrite cyclic structures.

:- class nc_handler {
    :- '$statemodel'(single).
    :- '$raw_state'.

    % TODO: missing instance_of__/1

    :- constructor new_/2.
    new_(ContCode, OldC) :-
        ~self = Id,
	Id = ~'$mut__init'(nc_handler(_, ContCode, OldC)).

    :- static nc_lookup/2.
    nc_lookup(NCId, NC) :- is_nop_cont(NCId), !,
        NC = nc_handler(_, contcode_other(nop), no_contdic).
    nc_lookup(NCId, NC) :-
        ( NC = ~'$mut__value'(NCId) ->
            true
        ; errlog:bug(['looking up a nc not in contdic ', NCId]), fail
        ).

    sel := Sel :-
        nc_handler(Sel, _, _) = ~nc_lookup(~self).
    contdic := ContDic :-
        nc_handler(_, _, ContDic) = ~nc_lookup(~self).
    contcode := ContCode :-
        nc_handler(_, ContCode, _) = ~nc_lookup(~self).

    {
    :- extends absmach_ctx.
    :- fluid s :: codegen_s + u.
    % Obtain a new bblock in this continuation (specialized for current 's')
    :- meta_predicate query_bb(out(bblock)).
    query_bb := BBId :-
        Sel = ~sel,
	BBId = ~sel_lookup_bb(Sel),
        ( instance_of_bblock(BBId) ->
            true
        ; call((
            s :: codegen_s <- empty_state,
	    BBId = ~bblock.new(~contcode)
          ))
        ).

    :- static sel_lookup_bb/2.
    % TODO: similar to memodic in predicates
    sel_lookup_bb(Sel) := BBId :-
        % TODO: make mode spec more general...
        ( ipexp.get__use_opt(specmode) ->
	    s.get_mutglobal_val_type(mode, ModeT),
	    ( ModeT = t_bitmask(mode, 1) ->
	        Sel = s(BBId,_)
	    ; ModeT = t_bitmask(mode, 2) ->
	        Sel = s(_,BBId)
	    ; errlog:bug(['bad mode in nc ', ModeT]),
	      fail
	    )
	; Sel = s(BBId)
	).
    }.

    {
    bb_list := List :-
        Sel = ~sel,
        List0 = ( Sel = s(A,B) ? [A,B]
		| Sel = s(A) ? [A]
		),
	List = ~filter_bb(List0).
    }.

    % (private)
    :- static filter_bb/2.
    :- '$ctxprj'(filter_bb/2, []).
    filter_bb([], []).
    filter_bb([X|Xs], Ys) :-
	( instance_of_bblock(X) ->
	    Ys = [X|Ys0]
	; Ys = Ys0
	),
	filter_bb(Xs, Ys0).
}.

nop_cont := '$nop_id'.
is_nop_cont(A) :- A =='$nop_id'.

% ---------------------------------------------------------------------------

fail_redmode(det_with_failins) := det_with_failins__failc :- !.
fail_redmode(semidet) := semidet__failc :- !.
fail_redmode(det) := det__failc :- !.
fail_redmode(nondet) := nondet__failc :- !.

% ---------------------------------------------------------------------------
% Operations on bblocks

:- class bblock {
    :- '$statemodel'(single).
    :- '$raw_state'.

    % TODO: missing instance_of__/1

    {
    :- fluid s :: codegen_s + u.
    :- constructor new_/1.
    new_(ContCode) :-
        Id = ~'$mut__init'(bblock(_, _, _, _, ~s, ContCode)),
        ~self = Id.
    }.
    
    lab := Lab :-
        bblock(Lab, _, _, _, _, _) = ~'$mut__value'(~self).
    cstat := CStat :-
        bblock(_, CStat, _, _, _, _) = ~'$mut__value'(~self).
    emitted := Emitted :-
        bblock(_, _, _, Emitted, _, _) = ~'$mut__value'(~self).

    % Update bblock with a new state (doing s.merge)
    {
    :- extends absmach_ctx.
    :- fluid tbl :: bb_tbl + u.
    :- fluid s :: codegen_s + u.
    update_s :-
        OldS = ~bs,
        NewS = ( OldS = empty_state ? ~s
	       | ~codegen_s.merge(OldS, ~s)
	       ),
        update(NewS).
    }.

    update(NewS) :-
        Id = ~self,
        bblock(A, B, C, D, _, ContCode) = ~'$mut__value'(Id),
	'$mut__assign'(Id, bblock(A, B, C, D, NewS, ContCode)).

    % Block entry state
    bs := S :-
        Id = ~self,
        bblock(_, _, _, _, S, _) = ~'$mut__value'(Id).

    % Compile bblock
    {
    :- extends absmach_ctx.
    :- fluid tbl :: bb_tbl + u.
    :- fluid cstat :: bb_code.
    compile :-
        bblock(_Lab, CStat, Seen, _, S, Redmode) = ~'$mut__value'(~self),
        ( nonvar(Seen) ->
            errlog:bug(['pushing back a bblock twice: ', ~self]), fail
        ; true
        ),
        Seen = yes,
        call((
          s :: codegen_s <- S,
	  cstat :: bb_code <- ~bb_code.new,
          ( Redmode = contcode_conj0(Xs) -> gn_conj0(Xs)
          ; Redmode = contcode_conj(Then) -> gn_conj(Then)
          ; Redmode = contcode_other(Other) -> gn_contcode_other(Other)
          ),
          ~cstat = CStat
        )).
    }.
}.

% TODO: this is should be 'instance_of' and 'static'
instance_of_bblock(BBId) :-
	bblock(_, _, _, _, _, _) = ~'$mut__value'(BBId).

% ---------------------------------------------------------------------------
% Dictionary of named continuations

:- class contdic {
    :- attr truecont :: m_any.
    :- attr failcont :: m_any.
    :- attr failinscont :: m_any.

    :- constructor f_/3.
    f_(A,B,C) :-
	~truecont = A,
	~failcont = B,
	~failinscont = C.

    :- constructor empty_/0.
    empty_ :-
	~truecont = ~nop_cont,
	~failcont = ~nop_cont,
	~failinscont = ~nop_cont.

    :- constant lookup/2.
    lookup(true) := ~truecont.
    lookup(fail) := ~failcont.
    lookup(failinscont) := ~failinscont.

    replace(true, V) :- truecont <- V.
    replace(fail, V) :- failcont <- V.
    replace(failinscont, V) :- failinscont <- V.
}.

% ---------------------------------------------------------------------------

{
:- extends absmach_ctx.
:- fluid tbl :: bb_tbl + u.
:- fluid s :: codegen_s + u.
:- fluid cstat :: bb_code.
% Compile Code with clean sections LocalSubs and insert them
% (other sections not in Subs are not inserted)
gn_subctx(LocalSubs, Code) :-
	% Start with clean tbl sections (depending on LocalSubs)
        tbl.clean(LocalSubs),
	%
	gn_conj(Code),
	%
	cstat.insert_subs(LocalSubs).

gn_conj(Xs0) :-
	conj_to_list(Xs0, Xs),
	gn_conj0(Xs).
}.

{
:- extends absmach_ctx.
:- fluid tbl :: bb_tbl + u.
:- fluid s :: codegen_s + u.
optim_sw(Xs, Xs2) :-
	extract_switcherpre(Xs, SwitcherJoin, JoinIndexExpr, Cases0, Xs0),
	call(( cases :: accum(Cases2), expand_cases(Cases0) )),
	% TODO: IndexExpr MUST be (at least) free of side effects!
	call(( indexed_cases :: accum(Cases3), get_cases(Cases2, IndexExpr) )),
	( \+ SwitcherJoin = none, \+ IndexExpr == JoinIndexExpr ->
	    fail % cannot join switcherpre and switch
	; true
	),
	nonvar(IndexExpr),
	( IndexExpr = index_on_member(Member, MemberType, SwitcherX0) ->
	    CaseKeyType = MemberType, % TODO: this should be an output of get_cases
	    SwitcherExpr = ~mcall(SwitcherX0, Member)
	; IndexExpr = index_on_var(VarType, SwitcherX0) ->
%	    errlog:trace([iov0(IndexExpr)]),
	    CaseKeyType = VarType, % TODO: this should be an output of get_cases
	    SwitcherExpr = SwitcherX0
	; fail
	),
	gn_term(SwitcherExpr, _, SwitcherExprType), % TODO: do not throw output?
	prune_cases(Cases3, CaseKeyType, SwitcherExprType, Cases4),
	( SwitcherJoin = none, Cases4 = [case(_, CaseCode)] -> % a single case, no switch is necessary
	    % switch simplified to a single case
	    Xs2 = ~append(~conj_to_list(CaseCode), Xs0)
	; IndexExpr = index_on_member(_, _, _) ->
	    decomp_cases(Cases4, SwitcherX0, SwitcherKeys, Cases5),
%	  errlog:trace([swr(SwitcherXType, SwitcherKeys, SwitcherJoin)]),
            % search for a swrule
	    ipexp.get__swrule_def(SwitcherKeys, SwitcherXType0, SwitcherJoin, SwitcherName),
%	  errlog:trace([swt0(SwitcherXType0)]),
	    SwitcherExprType.consequence(SwitcherXType0),
%	  errlog:trace([ok]),
	    !,
	    Xs2 = ['$sw'(SwitcherName, SwitcherX0, Cases5)|Xs0]
	; IndexExpr = index_on_var(_, _) ->
%	    errlog:trace([iov(Cases4, IndexExpr)]),
	    gen_switchdef_keys(Cases4, SwitcherX0, SwitchdefKeys, Cases5),
%	    errlog:trace([gswk(SwitchdefKeys)]),
	    % TODO: generate switch schemes with computed gotos
	    ( SwitchdefKeys = [K,_] -> % note: the last key is ignored (the input is covered) % TODO: check that it is correct
	        Xs2 = ['$sw'(swif(K), SwitcherX0, Cases5)|Xs0]
	    ; Xs2 = ['$sw'(switchdef(SwitchdefKeys), SwitcherX0, Cases5)|Xs0]
	    )
%	    Xs2 = ['$sw'(switchdef(SwitchdefKeys), SwitcherX0, Cases5)|Xs0]
	; fail  
	).
}.

decomp_cases([], _SwitcherX, [], []).
decomp_cases([case(K,Code0)|Cs0], SwitcherX, [K|Ks], [case(K,Code)|Cs]) :-
	Code = ('$trust_type'(SwitcherX, def_dep(tagged, K)), Code0), % TODO: do not hardwire 'tagged' here
	decomp_cases(Cs0, SwitcherX, Ks, Cs).

{
:- extends absmach_ctx.
gen_switchdef_keys([], _SwitcherX, [], []).
gen_switchdef_keys([case(K,Code0)|Cs0], SwitcherX, [SK|SKs], [case(K,Code)|Cs]) :-
	Code = ('$trust_type'(SwitcherX, K), Code0),
	% TODO: improve and generalize this code, '$error$' makes sense if it is the last case, but it is not a good solution
	trust(K instance_of ltype),
	( Atom = ~K.concrete_atom ->
%	    errlog:trace([tca(K, Atom)]),
	    BaseType = ~K.basic_def,
%	    errlog:trace([enp(BaseType)]),
	    SK = ~gn_unboxed(BaseType, Atom)
%	    errlog:trace([yesk(SK)])
	; %errlog:trace([nosk(K)]),
	  SK = '$no_unboxed_atom$' % TODO: emit multiple cases (except if it is the default case)
	),
	gen_switchdef_keys(Cs0, SwitcherX, SKs, Cs).
}.

% Prune cases in the switch (seen types are filtered out of the guards and cases that are not possible are removed)
% note: the cases cover all possible values of the indexed variable
{
:- extends absmach_ctx.
:- fluid tbl :: bb_tbl + u.
prune_cases([Case0|Cases0], CaseKeyType, Type0, PrunedCases) :- !,
	trust(CaseKeyType instance_of ltype),
	( Case0 = default_case(Code) ->
	    KeyType0 = ~CaseKeyType.subtop
        ; Case0 = case(KeyType00, Code),
	  KeyType0 = KeyType00
        ),
	% filter type
	trust(KeyType0 instance_of ltype),
	trust(Type0 instance_of ltype),
	KeyType = ~KeyType0.glb(Type0),
        % remove KeyType0 from Type0
        Type1 = ~Type0.remove(KeyType0),
	% add case
	( KeyType = ~CaseKeyType.subbottom -> % the guard is false, remove case
	    PrunedCases = PrunedCases0
	; PrunedCases = [case(KeyType, Code)|PrunedCases0]
	),
	( Type1 = ~CaseKeyType.subbottom -> % all cases are covered, stop
	    PrunedCases0 = []
	; prune_cases(Cases0, CaseKeyType, Type1, PrunedCases0)
	).
prune_cases([], _, _, []).
}.

{
:- extends absmach_ctx.
:- fluid tbl :: bb_tbl + u.
:- fluid s :: codegen_s + u.
:- fluid cstat :: bb_code.
gn_conj0([]) :- !,
	gn_conj0([true]).
gn_conj0(Xs) :-
	optim_sw(Xs, Xs2),
	!,
	gn_conj0(Xs2).
gn_conj0([X|Xs0]) :- !,
	( Xs0 = [] -> PushBacks = []
	; LT = ~s.nc_new(contcode_conj0(Xs0)), PushBacks = [LT],
	  s.set_cont(true, LT)
	),
	( gn_goal(X) -> true
        ; ptoc__impcomp:error(['unable to generate code for\n  goal: ', ~~(X), '\n  with cnt: ', ~s.c])
        ),
	gn_pushbacks_ncs(PushBacks).
%	errlog:trace([goal1(X)]),
% TODO: enable with an option, to emit verbose output
%	AR = (comment_msg([~~(begin(X))]), AR1, comment_msg([~~(end(X))])),

gn_pushbacks_ncs(PushBacks) :-
	call(( bbs :: accum(BBs0), nc_bb_versions(PushBacks) )),
	gn_pushbacks_bb(BBs0).
}.

{
:- fluid bbs :: accum.
nc_bb_versions([]).
nc_bb_versions([NCId|NCIds]) :-
	trust(NCId instance_of nc_handler),
	Xs = ~NCId.bb_list,
	nc_bb_versions__2(Xs),
	nc_bb_versions(NCIds).

nc_bb_versions__2([]).
nc_bb_versions__2([BBId|BBIds]) :-
	bbs.add(BBId),
	nc_bb_versions__2(BBIds).
}.

{
:- extends absmach_ctx.
:- fluid tbl :: bb_tbl + u.
:- fluid cstat :: bb_code.
gn_pushbacks_bb([]).
gn_pushbacks_bb([BBlock|Xs]) :-
	trust(BBlock instance_of bblock),
	BBlock.compile,
	cstat.insert_bb(BBlock),
	gn_pushbacks_bb(Xs).
}.

{
:- extends absmach_ctx.
:- fluid tbl :: bb_tbl + u.
:- fluid s :: codegen_s + u.
:- fluid cstat :: bb_code.
gn_contcode_other(C) :-
	( C = semidet ->
	    cstat.emit_csingle(return('TRUE'))
	; C = det_with_failins ->
	    cstat.emit_csingle(return('TRUE'))
	; C = det ->
	    cstat.emit_csingle(return)
	; C = det(X,OutType) ->
	    gn_term(X, XR, Type1), % TODO: check type
	    ( Type1.consequence(OutType) -> true
	    ; ptoc__impcomp:error(['returning wrong type ', Type1, ' when ', OutType, ' was required'])
	    ),
	    cstat.emit_csingle(return(XR))
	; C = nop -> % TODO: TEMPORAL FOR BACKEND_C INTEGRATION! REMOVE!
	    cstat.emit_csingle('$nop')
	; C = specialcont(exitemu(LT)) -> % TODO: do not hardwire the label!
	    % TODO: pending continuation is lost!
	    trust(LT instance_of nc_handler),
	    cstat.emit_csingle(br(~LT.query_bb))
	; C = macro(ExitLabel) -> % TODO: can it be improved?
	    cstat.emit_csingle(goto(ExitLabel))
	; C = void ->
	    ptoc__impcomp:error(['reaching void continuation'])
	; C = semidet__failc ->
	    cstat.emit_csingle(return('FALSE'))
	; C = nondet__failc ->
	    cstat.emit_csingle(return('FAIL_INSNP'))
	; C = det__failc ->
	    ptoc__impcomp:error(['det__failc is not a valid continuation\ns:', ~s])
	; C = det_with_failins__failc ->
	    ptoc__impcomp:error(['det_with_failins__failc is not a valid continuation\ns:', ~s])
	; % TODO: error or bug?
	  ptoc__impcomp:error(['unknown arg in jump_c ', C])
	).

gn_cont(ContName) :-
	BBId = ~gn_cont0(ContName),
	cstat.emit_csingle(br(BBId)).

% Gets a bblock for the continuation in ContName
% (and updates bblock S abstract state)
% TODO: use a phi-node for abstract states here? (and refine it with variables in the bblock)
% TODO:   to implement it, set a output s in each bblock and a list of input bblocks in each bblock: input S is calculated by merging output of input bblocks
gn_cont0(ContName) := BBId :-
	BBId = ~nc_cont(~s.get_cont(ContName)).

% Gets a bblock for the continuation NCId
% (and updates bblock S abstract state)
% TODO: use a phi-node for abstract states here? (and refine it with variables in the bblock)
% TODO:   to implement it, set a output s in each bblock and a list of input bblocks in each bblock: input S is calculated by merging output of input bblocks
nc_cont(NCId) := BBId :-
	trust(NCId instance_of nc_handler),
	ContCode = ~NCId.contcode,
	OldC = ~NCId.contdic,
	s.c <- OldC,
        ( ContCode = contcode_other(_) ->
	    % Create a new bblock for each continuation call
	    Id = ~bblock.new(ContCode),
	    Id.compile,
	    cstat.insert_bb(Id),
	    BBId = Id
	; BBId = ~NCId.query_bb,
	  BBId.update_s
	).

gn_emit(R0) :-
	cstat.emit_csingle(R0),
	gn_cont(true).

gn_goal(G) :- var(G), !, fail.
gn_goal(G) :- G instance_of termvar, !,
	gn_goal('$call'(G, [])).
gn_goal(G) :- G = call(G0), !,
	gn_goal('$call'(G0, [])).
gn_goal(G) :-
	ipexp.goal_unfold(G, G2),
	!,
	gn_conj(G2).
gn_goal(G0) :-
	% TODO: optimize!
        unfold_type_check(G0, G),
	get_index_expr(G, _, _),
	optim_sw([(G->true;fail)], [G2]),
	G2 = '$sw'(_,_,_), !,
	gn_conj(G2).
% note: declare a variable but do not generate its definition unless it is used elsewhere
% TODO: implement automatically...
% TODO: remove Type (infer from Value, refine from analysis of future uses)
gn_goal('$decl_pa_array'(CProps, L, SortedIns1)) :- !,
	L = ~termvar.new,
	Type = array(ref0(clabel)),
	var_mem_type(L, cvar_pa_array(CProps,_,SortedIns1), Type),
	gn_cont(true).
gn_goal('$call_pa_array'(L, Index)) :- !,
	L instance_of termvar,
	Mem = ~s.get_mem(L),
	%
	Mem = cvar_pa_array(CProps, LR, SortedIns1),
	Sub3 = ~tbl.sub(sub3),
	Key = labarray(LR),
	( Sub3.get_decl(Key, _) ->
	    true
	; Sub3.insert_decl(Key, Labels),
	  gn_pa_array_elems(SortedIns1, SortedIns2),
	  LType2 = ref0(array(ref0(subpa))),
	  trust(LType2 instance_of ltype),
	  CType2 = ~LType2.ctype,
	  length(SortedIns2, Size),
	  V = aggregate(CType2, SortedIns2),
	  LType = ref0(array(ref0(subpa), Size)),
	  trust(LType instance_of ltype),
	  CType = ~LType.ctype,
	  ( CProps = static ->
	      Labels = declare(LR, static+CType, V)
	  ; CProps = staticasm ->
	      % a kludge? (at least the last asm, has a ugly syntax and is kludge... since gcc asms are not fully supported in write_c)
	      % note: the last asm ensures that the label array is emitted by the C compiler
	      Labels = (declare(L, static+asm(string("emulabelarray"))+CType, V),
                        call('__asm__', [call('""::"g"', [LR])]),
                        call('__asm__', [string(".globl emulabelarray")]))
	  ; ptoc__impcomp:error(['unknown cprops in cvar_pa_array: ', CProps])
	  )
	),
	%
	gn_term(Index, IndexR, _),
	AR = element(LR, IndexR),
	cstat.emit_csingle(call('COMPUTED_GOTO', [AR])).
gn_goal('$emuctx'(Goal)) :- !,
	s.imp <- emu,
	ContDic0 = ~s.c,
	LT = ~s.get_cont(true),
	s.set_cont(true, ~s.nc_new(contcode_other(specialcont(exitemu(LT))))),
	gn_conj(Goal),
	% TODO: IMPORTANT! I explicitly call gn_cont here from saved ContDic0 because exitemu pending continuations get lost... use subcall infrastructure? this is a kludge
	s.c <- ContDic0,
	gn_cont(true).
gn_goal('$switch'(Var, CasesCode, DefaultCode)) :- !,
	gn_switch(Var, CasesCode, DefaultCode).
gn_goal('$vsubctx'(Body)) :- !, % TODO: temporal!
        % Translate a block of code and declare autodecl variables
	% mark input arguments as declared
	gn_subctx(subauto, Body).
gn_goal('$inline_c'(Xs1, Xs2, R)) :- !,
	gn_terms(Xs1, Xs2, _),
	gn_emit(R).
%%
gn_goal('$copy_from_choice'(B, G, Args0)) :- !,
	maybe_varg_get(Args0, Args),
	gn_copy_from_choice(Args, 0, B, G, R),
	gn_conj(R).
gn_goal('$copy_to_choice'(B, G, Args0)) :- !,
	maybe_varg_get(Args0, Args),
	gn_copy_to_choice(Args, 0, B, G, R),
	gn_conj(R).
gn_goal('$check_cvarmem'(Var, N, T)) :- !, % TODO: only for cvar and mvar
	( fresh_var(Var) ->
	    var_mem_type(Var, cvar(N), T)
	; true
	),
	gn_cont(true).
gn_goal('$check_mvarmem'(Var, N, T)) :- !, % TODO: only for cvar and mvar
	( fresh_var(Var) ->
	    var_mem_type(Var, cvar(address(N)), T)
	; true
	),
	gn_cont(true).
gn_goal('$check_evalmem'(Var, MemExpr, T)) :- !,
	( fresh_var(Var) ->
	    var_mem_type(Var, evalmem(MemExpr), T)
	; true
	),
	gn_cont(true).
%%
gn_goal('$defnatproto'(Name)) :- !, % TODO: temporal?
	define_nativeproto(Name),
	gn_cont(true).
% special control methods
% TODO: deprecated?
gn_goal('$failins_cont') :-
	Imp = ~s.imp,
	Imp = det_with_failins, !,
	cstat.emit_csingle(return('FALSE')).
% TODO: IMPORTANT! This cannot use (_ -> _ ; _). It should use a user continuation (with no backtracking!).
gn_goal('$vicallr'(semidet, GId, Xs1, [], Fail)) :- !, % TODO: temporal!
	gn_goal(('$vicallr_notcond'(GId, Xs1) -> Fail ; true)).
gn_goal('$vicallr_notcond'(GId, Xs1)) :- !, % TODO: temporal!
	Label = ~pred_label(GId),
	translate_label(Label, Label2),
	gn_terms(Xs1, Xs2, _),
	Cond1 = call(Label2, ~add_context_args(with_worker, Xs2)),
	Cond2 = logical_not(Cond1),
	%
	Cond3 = ~simp_cond(Cond2),
	%
	S0x = ~s,
	S0y = ~s,
	gn_cond(Cond3, S0x, S0y).
gn_goal('$vvivcall'(Label)) :- !,
	% TODO: kludge?
	s.unwind_cont,
        % TODO: uses  ~nop_cont (an atom) to ensure that the same calling key is generated when inserting in the sub call table
	s.set_cont(true, ~nop_cont),
	gn_goal('$sub4call'(Label)).
%%
% TODO: define impclasses??? put together the control of all det, semidet, etc.. and define them as control methods; indeed, they should specify the operational semantics of det, semidet, nondet interpreters
% control methods:
% 'det' control methods
gn_goal('$vifail') :-
	Imp = ~s.imp,
	Imp = det, !,
	ptoc__impcomp:error('fail control method is not defined for det').
gn_goal('$vvproceed'([X])) :-
	Imp = ~s.imp,
	Imp = det, !,
	gn_term(X, X2, _),
	R = return(X2),
	gn_emit(R).
gn_goal('$vvproceed'([])) :-
	Imp = ~s.imp,
	Imp = det, !,
	Z0 = return,
	gn_emit(Z0).
% TODO: not used?
gn_goal('$vilast_call'(det, GId, Xs1, [_])) :-
	Imp = ~s.imp,
	Imp = det, !,
	Label = ~pred_label(GId),
	translate_label(Label, Label2),
	gn_terms(Xs1, Xs2, _),
	R = return(call(Label2, ~add_context_args(with_worker, Xs2))),
	gn_emit(R).
gn_goal('$vilast_call'(det, GId, Xs1, [])) :- !,
	Label = ~pred_label(GId),
	translate_label(Label, Label2),
	gn_terms(Xs1, Xs2, _),
	R = seq(call(Label2, ~add_context_args(with_worker, Xs2)), return),
	gn_emit(R).
% 'semidet' control methods
gn_goal('$vifail') :-
	Imp = ~s.imp,
	Imp = semidet, !,
	Z0 = return('FALSE'),
	gn_emit(Z0).
gn_goal('$vvproceed'([])) :-
	Imp = ~s.imp,
	Imp = semidet, !,
	Z0 = return('TRUE'),
	gn_emit(Z0).
gn_goal('$vilast_call'(semidet, GId, Xs1, [])) :-
	Imp = ~s.imp,
	Imp = semidet, !,
	Label = ~pred_label(GId),
	translate_label(Label, Label2),
	gn_terms(Xs1, Xs2, _),
	R = return(call(Label2, ~add_context_args(with_worker, Xs2))),
	gn_emit(R).
% 'nondet' control methods
gn_goal('$vifail') :-
	Imp = ~s.imp,
	Imp = nondet, !,
	Z0 = return('FAIL_INSNP'),
	gn_emit(Z0).
gn_goal('$vvproceed'([])) :-
	Imp = ~s.imp,
	Imp = nondet, !,
	Z0 = return('SUCCESS_INSNP'),
	gn_emit(Z0).
gn_goal('$vilast_call'(nondet, GId, Xs1, [])) :-
	Imp = ~s.imp,
	Imp = nondet, !,
	Label = ~pred_label(GId),
	( Xs1 = [], Label = global(_) ->
	    gn_term(~funcall('$vargexeccont'(Label)), E, _),
	    R = return(E)
	; Xs1 = [], Label = predtblentry(Entry) ->
	    gn_term(~funcall('$vargpredtblentry'(Entry)), Label2, _),
	    R = return(call('DEF_INSNP', [Label2]))
	; translate_label(Label, Label2),
	  gn_terms(Xs1, Xs2, _),
	  R = return(call(Label2, ~add_context_args(with_worker, Xs2)))
	),
	gn_emit(R).
% universal control methods
% TODO: integrate global, fail, imported, proceed continuations into nondet_cont, etc.
% TODO: FAIL is badly calculated... next version is not accurate: instead nextblock should be used hmmmmmmmmm 
gn_goal('$vicallr'(det, GId, Xs1, [Y], _)) :- !,
	gn_goal('<-'(Y, ~funcall('$vifuncallr'(GId, Xs1)))).
gn_goal('$vicallr'(det, GId, Xs1, [], _)) :- !,
	Label = ~pred_label(GId),
	translate_label(Label, Label2),
	gn_terms(Xs1, Xs2, _),
	R = call(Label2, ~add_context_args(with_worker, Xs2)),
	gn_emit(R).
% TODO: merge with unfolding
gn_goal('$subctx'(Def)) :- !,
	gn_subctx(subs, Def).
gn_goal('$subctx4'(Def)) :- !,
	gn_subctx(subs4, Def).
% Generate code for semideterministic calls
gn_goal('$sw'(SwitcherName, SwitcherArg, Cases2)) :- !,
	gn_term(SwitcherArg, SwitcherArgR, _),
	prepare_cases(Cases2, BBlocks),
	cstat.emit_csingle(sw_macro_code(SwitcherName, SwitcherArgR, BBlocks)),
	gn_pushbacks_bb(BBlocks).
% compile a switch
gn_goal((CThen;Else)) :-
	nonvar(CThen), CThen = (C->Then), nonvar(C),
	!,
	LT = ~s.nc_new_conj(Then),
	LE = ~s.nc_new_conj(Else),
	s.set_cont(true, LT),
	s.set_cont(fail, LE),
	gn_conj(C),
	gn_pushbacks_ncs([LT, LE]).
% execute Goal, with a handler for failinscont continuation
gn_goal('$catch_failins'(Goal, OnFailins)) :- !,
	LF = ~s.nc_new_conj(OnFailins),
	s.set_cont(failinscont, LF),
	gn_conj(Goal),
	gn_pushbacks_ncs([LF]).
% TODO: be careful if X generates bindings (no binding should be propagated)
% TODO: not using negation in C code generates slightly different code
gn_goal((\+ X)) :- !,
	gn_goal((X->fail;true)).
% TODO: return the equiv of type for sentences... i.e. if it fails or not, etc.
gn_goal('$for_each'(V, Range, Goal)) :- !,
	gn_for_each(V, Range, Goal).
gn_goal('$while'(Cond, Goal)) :- !,
	gn_while(Cond, Goal).
gn_goal('$do_while'(Goal, Cond)) :- !,
	gn_do_while(Goal, Cond).
gn_goal(true) :- !,
	gn_cont(true).
gn_goal(fail) :- !,
	gn_cont(fail).
gn_goal('$cont'(ContName)) :- atom(ContName), !,
	gn_cont(ContName).
% TODO: try not to use this!
gn_goal('$ct=_evalmem'(A, Expr)) :- A instance_of termvar, !,
	AMem = ~s.get_mem(A),
	( AMem == evalmem(Expr) ->
	    gn_cont(true)
	; gn_cont(fail)
	).
% TODO: a temporal hack...
gn_goal('$mutname'(A, Name)) :- A instance_of termvar, atom(Name), !,
	Mem = ~s.get_mem(A),
	Mem = cvar(address(Name)),
	gn_cont(true).
gn_goal(Goal) :-
	peval_goal__2(Goal, GoalStatus),
	\+ GoalStatus = unknown,
	!,
	( GoalStatus = true ->
	    gn_cont(true)
	; GoalStatus = fail ->
	    gn_cont(fail)
	; fail
	).
gn_goal('$add_autov'(X, Type)) :- atom(X), !,
	add_autov(X, Type),
	gn_cont(true).
gn_goal('$unfold'(G)) :- !, % TODO: do not use this in user code?
        a_decomp_goal(G, PA, Args),
	Code0 = ~getdef(PA, Args),
	gn_conj(Code0).
gn_goal('$inline'(X)) :- !, % TODO: temporal!
	gn_emit(X).
gn_goal('$trace'(Fmt, As)) :- !, % TODO: temporal!
	gn_terms(As, AsR, _AsT),
	G = call(fprintf,[stderr,string(Fmt)|AsR]),
	gn_emit(G).
gn_goal('$tracecomp'(G)) :- !, % TODO: temporal!
	errlog:trace([tracecomp_g(G)]),
	errlog:trace([tracecomp_s(~s)]),
	gn_cont(true).
% TODO: improve, explicit deallocation (see $alloc)
% TODO: return a maybe_ref1 type!
gn_goal('$dealloc'(Allocator, Expr)) :- !,
	dealloc_method(Allocator, DeallocMethod),
	DeallocatorGoal =.. [DeallocMethod, Expr],
	gn_goal(DeallocatorGoal).
% TODO: fix, it should work for any shift, not only 0 and 1
gn_goal('$shift_ops'(N)) :- !,
	gn_shift_ops(N).
gn_goal('$trust_shift_array') :- !,
	trust_shift_array,
	gn_cont(true).
gn_goal('$trust_ops'(Ops)) :- !,
	s.ops <- Ops,
	gn_cont(true).
gn_goal('$trust_type'(Expr, Type)) :- !,
	do_trust_type(Expr, Type),
	gn_cont(true).
gn_goal('$error'(Msg)) :- !,
	ptoc__impcomp:error(['runtime error check not allowed: ', Msg]).
gn_goal(G) :- functor(G, call, N), N >= 2, !,
	G =.. [_,A|Xs],
	gn_goal('$call'(A, Xs)).
gn_goal('$call'(A, Xs)) :- !,
	gn_term(A, AR, AType),
	% TODO: use Xs types
	gn_terms(Xs, Xs2, _),
	( gn_control__call(AR, AType, Xs2) ->
	    true
	; ptoc__impcomp:error(['failed compilation of $call to ', A, ' with arguments ', Xs])
	).
gn_goal('$subcall'(G)) :- !,
	a_decomp_goal(G, GId, Args),
	gn_subcall(sub, GId, Args, BBId),
	cstat.emit_csingle(br(BBId)).
% TODO: merge with subcall
gn_goal('$sub2call'(G)) :- !,
	a_decomp_goal(G, GId, Args),
	gn_subcall(sub2, GId, Args, BBId),
	cstat.emit_csingle(br(BBId)).
gn_goal('$sub4call'(G)) :- !,
	a_decomp_goal(G, GId, Args),
	gn_subcall(sub4, GId, Args, BBId),
	cstat.emit_csingle(br(BBId)).
gn_goal((A = B)) :- !,
	gn_unify(A, B).
gn_goal('<-'(A, B)) :- !,
	gn_assign(old, A, B).
gn_goal(G) :-
	functor(G, GN, GA),
	G =.. [_|As],
	gn_goal__2(GN, GA, As).

gn_goal__2(GN, GA, As) :- % TODO: if I expand functions, this should be treated in the other way
	ipexp.get__op__foreign(GN, GA, ForeignDef),
	ForeignDef = foreignfun(_, _, _),
	!,
	( append(InAs, [OutA], As) -> true ; fail ), % TODO: avoid spurious choice points
	G2 =.. [GN|InAs],
	gn_goal((OutA = ~funcall(G2))).
gn_goal__2(GN, GA, As) :-
	ipexp.get__op__prop(GN, GA, unfold),
	!,
	Code = ~getdef('$pr'(GN, GA), As),
	gn_conj(Code).
gn_goal__2(GN, GA, As) :- % prcall
	ipexp.get__op__prop(GN, GA, subpr),
	!,
	gn_subcall(sub, '$pr'(GN,GA), As, BBId),
	cstat.emit_csingle(br(BBId)).
gn_goal__2(GN, GA, As) :-
	( gn_terms(As, AsR, AsT) -> true ; ptoc__impcomp:error(['gn_terms failed for ', As]) ),
	gn_goal__3(GN, GA, AsR, AsT).

gn_goal__3(GN, GA, AsR, AsT) :-
	% TODO: generalize with polymorphic type defs?
	% TODO: missing deref
	is_comparison(GN), GA = 2, !,
	AsR = [XR,YR],
	AsT = [XType,YType],
	trust(XType instance_of ltype),
	trust(YType instance_of ltype),
	( XType.comparable(YType) ->
	    true
	; ptoc__impcomp:error(['types \'', GN, '\' \'', XType, '\' and \'', YType, '\' are not comparable'])
	),
	R =.. [GN, XR, YR],
	S0x = ~s,
	S0y = ~s,
	gn_cond(R, S0x, S0y).
gn_goal__3(GN, GA, AsR0, AsT) :- % prcall
	ipexp.get__op__foreign(GN, GA, ForeignDef),
	!,
	( ForeignDef = foreignp(Types, Imp, CName) ->
	    true
	; ForeignDef = foreignmacro(Types, Macromodes, CName) ->
	    Imp = macroimp
	; ptoc__impcomp:error(['unknown compilation mode for foreign ', GN, '/', GA, ' with foreigndef ', ForeignDef])
	),
	( coerce_args(AsT, Types, AsR0, AsR, _NewAsT) ->
	    true
	; ptoc__impcomp:error(['unable to coerce arguments in call to ', GN, '/', GA, '.\n  Inferred types: ', AsT, '.\n  Required types: ', Types])
	),
	( Imp = semidet ->
	    UseWorker = ~get_context(GN, GA),
	    R0 = call(CName, ~add_context_args(UseWorker, AsR)),
	    S0x = ~s,
	    S0y = ~s,
	    gn_cond(R0, S0x, S0y)
	; Imp = det_with_failins ->
	    UseWorker = ~get_context(GN, GA),
	    R0 = call(CName, ~add_context_args(UseWorker, AsR)),
	    S0x = ~s,
	    S0y = ~s,
	    gn_cond_with_failins(R0, S0x, S0y)
	; Imp = nondet ->
	    gn_control__call(CName, st_nondet(GA), AsR)
	; Imp = det ->
	    UseWorker = ~get_context(GN, GA),
	    R0 = call(CName, ~add_context_args(UseWorker, AsR)),
	    gn_emit(R0)
	; Imp = macroimp ->
	    postargs(AsR, Macromodes, Ys2),
	    R0 = call(CName, Ys2),
	    gn_emit(R0)
	; ptoc__impcomp:error(['unknown compilation mode for foreign ', GN, '/', GA, ' with imp ', Imp])
	).
gn_goal__3(GN, GA, _AsR, _AsT) :-
	ptoc__impcomp:error(['unknown compilation mode for ', GN, '/', GA]).
}.

{
:- extends absmach_ctx.
:- fluid tbl :: bb_tbl + u.
:- fluid s :: codegen_s.
do_trust_type(Expr, Type) :-
	trust(Type instance_of ltype),
	( nonvar(Expr), Expr = '@'(Var),
	  Var instance_of termvar ->
	    Type0 = ~s.get_type(Var),
	    ( Type0 = mut(OldType) -> % TODO: WRONG! store the type the mutable is compatible with, and the actual type (that must be inside the compatible type)
	        trust(OldType instance_of ltype),
	        NewType = ~OldType.glb(Type),
	        s.set_mut_type(Var, NewType)
	    ; Type0 = any -> % TODO: WRONG?! used to do trust_type(X, Type) where X is a evalmem
	        s.set_mut_type(Var, Type)
	    ; ptoc__impcomp:error(['cannot trust_type of mutval of ', Var, ' because it is not a mutable'])
	    )
	; Expr instance_of termvar ->
	    OldType = ~s.get_type(Expr),
	    NewType = ~OldType.glb(Type),
	    s.set_type(Expr, NewType)
	; ptoc__impcomp:error(['cannot trust type of expression ', Expr, ' to be ', Type]) % TODO: introduce an intermediate variable
	).
}.

gn_copy_from_choice([], _I, _B, _G, R) :- !, R = true.
gn_copy_from_choice([_|Args], I, B, G, R) :-
	Gi = ~funcall('$element'(~mcall(G,x), I)),
	Bi = ~funcall('$element'(~mcall(B,x), I)),
	R = ('<-'(Gi, '@'(Bi)), R0),
	I1 is I + 1,
	gn_copy_from_choice(Args, I1, B, G, R0).

gn_copy_to_choice([], _I, _B, _G, R) :- !, R = true.
gn_copy_to_choice([_|Args], I, B, G, R) :-
	Gi = ~funcall('$element'(~mcall(G,x), I)),
	Bi = ~funcall('$element'(~mcall(B,x), I)),
	R = ('<-'(Bi, '@'(Gi)), R0),
	I1 is I + 1,
	gn_copy_to_choice(Args, I1, B, G, R0).

% TODO: unfolding when some variables are shared still requires the predicate abstraction!
% note: only (Args=[],Shared=all) and (Args=_,Shared=[]) is supported
{
:- fluid s :: codegen_s.
new_predabs(Props, Args, Shared, Code, Var) :- !,
	Type = predabs,
%	( Args0 = [], Shared = all ->
%	    Args = [],
%	    Code = Code0
%	; copy_term_shattr(predabscode(Props, Args0, Shared, Code0), predabscode(Props, Args, Shared, Code))
%	),
	var_mem_type(Var, predabscode(Props, Args, Shared, Code), Type).

new_cons(Value, Var) :- !, % creates a variable with a constant value (that is not materialized)
	Type = cons, % TODO: wrong
	var_mem_type(Var, cons(Value), Type).
}.

{
:- fluid s :: codegen_s + u.
var_cons(A, Value) :- % get the constant value of a variable
	trust(A instance_of termvar),
	AMem = ~s.get_mem(A),
	nonvar(AMem),
	AMem = cons(Value).
}.

{
:- extends absmach_ctx.
:- fluid tbl :: bb_tbl + u.
:- fluid s :: codegen_s + u.
:- fluid cstat :: bb_code.
gn_cond(R0, Sx, Sy) :-
	gn_cond_common(R0, Sx, fail, Sy).

gn_cond_with_failins(R0, Sx, Sy) :-
	gn_cond_common(R0, Sx, failinscont, Sy).

gn_cond_common(R0, Sx, ElseContName, Sy) :-
	call((
          s :: codegen_s <- Sx,
	  ThenId = ~gn_cont0(true)
        )),
	call((
          s :: codegen_s <- Sy,
          ElseId = ~gn_cont0(ElseContName)
        )),
	cstat.emit_cond_br(R0, ThenId, ElseId).
}.

% TODO: errlog:bug should set a global flag and the next compilation step should stop if it detects it (errlog:trace does not)
% unfold code for goal G and cache it under Key
{
:- extends absmach_ctx.
:- fluid tbl :: bb_tbl + u.
:- fluid s :: codegen_s + u.
gn_subcall(SubId, PA, Args, BBId) :-
	( PA = '$pa'(X), X instance_of termvar ->
	    % TODO: add more elements from the state to this key!
	    GlobalSpec = none,
	    TopGlobal = none,
	    Prior0 = no,
	    Key0 = varkey(~X.name)
	; PA = '$pr'(GN,_GA) ->
%	    Key0 = predkey(GN, GA),
	    Key0 = GN,
	    ipexp.pred_spec(PA, GlobalSpec, TopGlobal, Prior0)
	; ptoc__impcomp:error(['unknown goal in subcall ', PA])
	),
	% add contdic to the key
	% TODO: add conts to Prior? (anything that is part of the key)
	Key1 = k(Key0, ~s.c),
%	errlog:trace([k(Key1)]),
	% add a global value to the key (if necessary)
	( GlobalSpec = global(Global) ->
	    s.get_mutglobal_val_type(Global, ValueType),
	    Key = g(Key1,ValueType),
	    Prior = (ValueType-Prior0)
	; Key = Key1,
	  Prior = Prior0
	),
	% TODO: this assumes that there is only one call pattern... this is incomplete
	Sub = ~tbl.sub(SubId),
	( BBId = ~Sub.get_bb(Key) ->
	    % TODO: wrong! merge s, take into account Args and rebuild bblock
	    true
	; % TODO: output S should not be ignored! it may require a fixpoint...
	  Code0 = ~getdef(PA, Args),
	  %
	  Sub.insert_bb(Key, Prior, BBId), % TODO: this inserts a bottom entry!
	  % move to the top value a global variable?
	  ( TopGlobal = global(TGlobal, TMutType, TValueType0) ->
	      trust(TValueType0 instance_of ltype),
	      TValueType = ~TValueType0.norm_eval,
	      s.set_mutglobal_type(TGlobal, TMutType, TValueType)
	  ; true
	  ),
	  BBId = ~bblock.new(contcode_conj(Code0)),
	  call((
            cstat :: bb_code <- ~bb_code.new,
	    BBId.compile
          ))
	).
}.

{
:- fluid s :: codegen_s + u.
var_predabs_code(A, Args, Code, Props) :-
	trust(A instance_of termvar),
	AMem = ~s.get_mem(A),
	nonvar(AMem),
	AMem = predabscode(Props0, Args0, Shared, Code0),
	( Args0 = [], Shared = all ->
	    Props = Props0,
	    Code = Code0
	; copy_term_shattr(AMem, predabscode(Props, Args, Shared, Code))
	).
}.

{
:- extends absmach_ctx.
:- fluid tbl :: bb_tbl + u.
:- fluid s :: codegen_s + u.
:- fluid cstat :: bb_code.
% TODO: incomplete, not clean...
gn_unify(VarA, VarB) :- fresh_var(VarA), fresh_var(VarB), !,
	ptoc__impcomp:error(['unification of fresh variables is not yet supported']).
gn_unify(Var, Val) :- nonvar(Val), fresh_var(Var), !,
	gn_unify_val(Val, Var).
gn_unify(Val, Var) :- nonvar(Val), fresh_var(Var), !,
	gn_unify_val(Val, Var).
gn_unify(_, B) :- B = ~funcall('$vimut'(_, _)), !, % TODO: temporally ignored
	gn_cont(true).
gn_unify(A, _) :- A = ~funcall('$vimut'(_, _)), !, % TODO: temporally ignored
	gn_cont(true).
gn_unify(A, B) :-
	gn_terms([A,B], [AR,BR], [TypeA,TypeB]),
	trust(TypeA instance_of ltype),
	trust(TypeB instance_of ltype),
	Exit = ~TypeA.compare(TypeB),
	( Exit = true ->
	    gn_cont(true)
	; Exit = false ->
	    gn_cont(fail)
	; Exit = unknown ->
	    ( nonvar(A), A = ~mcall(_, tag),
	      nonvar(B), B = ~mcall(_, tag) ->
	        R = (AR == BR),
		S0x = ~s,
		S0y = ~s,
		gn_cond(R, S0x, S0y)
	    ; atom(B) ->
	        gn_unify_with_atom(A, TypeA, B, AR)
	    ; atom(A) ->
	        gn_unify_with_atom(B, TypeB, A, BR)
	    ; ptoc__impcomp:error(['not supported unification ', A=B])
	    )
	).

gn_unify_with_atom(A, TypeA, B, AR) :-
	% TODO: do not pass A, instead, pass 'variable deps' (e.g. x is a member of a structure y... or x is unknown -- then update alias info acoordingly)
	% TODO: (that is, implement an 'abstract memory location', e.g. A may be any var... A is a new variable that do not share with anything)
	% TODO: implement a specialized data type to store the abstract state efficiently!
	trust(TypeA instance_of ltype),
	( GlName = mode, nonvar(A), A = '@'(A0), nonvar(A0), A0 = ~funcall(A1), A1 == GlName ->
	    GlType = mode,
	    trust(GlType instance_of ltype),
	    gn_unboxed(GlType, B, BR1),
	    R = (AR == BR1),
	    MTypeTrue = ~GlType.coerce_from_atom(t_atom(B)),
	    GlTypeTop = ~GlType.subtop,
	    trust(GlTypeTop instance_of ltype),
	    MTypeFalse = ~GlTypeTop.remove(MTypeTrue),
	    S0 = ~s,
	    call((
              s :: codegen_s <- S0,
	      s.set_mutglobal_val_type(GlName, MTypeTrue),
	      ~s = S0x
            )),
	    call((
              s :: codegen_s <- S0,
	      s.set_mutglobal_val_type(GlName, MTypeFalse),
	      ~s = S0y
            )),
	    gn_cond(R, S0x, S0y)
	; nonvar(A), A = ~mcall(TaggedA, tag) ->
	    BaseTagType = 'tagged_$key',
	    gn_unboxed(BaseTagType, B, BR1),
	    R = (AR == BR1),
	    type_tagged__from_tag(BaseTagType, B, TaggedTypeB),
	    S0 = ~s,
	    call((
              s :: codegen_s <- S0,
	      do_trust_type(TaggedA, TaggedTypeB),
              ~s = S0x
            )),
	    % TODO: implement do_trust_not_type to get S0y
	    S0y = S0,
	    gn_cond(R, S0x, S0y)
	; TypeA.coercible_from_atom ->
	    gn_unboxed(TypeA, B, BR1),
	    R = (AR == BR1),
	    % TODO: obtain good S0x and S0y
	    S0x = ~s,
	    S0y = ~s,
	    gn_cond(R, S0x, S0y) 
	; ptoc__impcomp:error(['not supported unification ', A=B])
	).

gn_unify_val(Val, Var) :- !,
	Var = ~termvar.new,
	( Val = ~funcall(Fun) ->
	    gn_unify_val__eval(Fun, Var)
	; % TODO: check again...
	  gn_term(Val, ValR0, ValType),
	  gn_unify_val__store(Var, ValR0, ValType)
	).

gn_unify_val__eval(newmut(Type0), Var) :- !,
	gn_local_mutable(Type0, Var, AR0, _),
	gn_emit(AR0).
gn_unify_val__eval(initmut(Type0, Value), Var) :- !, % create a new mutable variable, constrained to values of type Type0, and assign it the initial value Value
	gn_assign(new(Type0), Var, Value).
gn_unify_val__eval(clonemut(MutValue), Var) :- !, % like initmut, but uses the same type to which MutValue is compatible
	% TODO: fix! type for muts in gn_assign is broken (it is taken from the value, not from the mutable itself)
	gn_assign(clone, Var, '@'(MutValue)).
gn_unify_val__eval(ref0(Type0), Var) :- % TODO: allow the same without ref0? store somewhere that it is a local def (escape analysis...)?
	gn_local_var(ref0(Type0), Var, AR0, _),
	gn_emit(AR0).
gn_unify_val__eval(array(ref0(array(ref0(mut(Type0)), Size2)), Size), Var) :- % TODO: hardwired
	gn_local_var(ref0(array(ref0(array(ref0(mut(Type0)), Size2)), Size)), Var, AR0, _),
	gn_emit(AR0).
gn_unify_val__eval(array(ref0(mut(Type0)), Size), Var) :- % TODO: hardwired
	gn_local_var(ref0(array(ref0(mut(Type0)), Size)), Var, AR0, _),
	gn_emit(AR0).
gn_unify_val__eval(array(ref0(Type0), Size), Var) :- % TODO: hardwired
	gn_local_var(ref0(array(ref0(Type0), Size)), Var, AR0, _),
	gn_emit(AR0).
gn_unify_val__eval('$vimut'(N, Type0), Var) :-
	gn_local_vimutable(N, Type0, Var, AR0),
	gn_emit(AR0).
gn_unify_val__eval('$predabs'(Args, Shared, Code), Var) :-
	% TODO: pending: checking arguments and shared variables
        new_predabs([], Args, Shared, Code, Var),
	gn_cont(true).
gn_unify_val__eval('$predabs'(Props, Args, Shared, Code), Var) :-
	% TODO: pending: checking arguments and shared variables
	new_predabs(Props, Args, Shared, Code, Var),
	gn_cont(true).
gn_unify_val__eval('$forcedmut'(Var0), Var) :-
        % Gets a mutable version of an existing variable
        % TODO: it is a kludge, do not use it!
	( Var0 instance_of termvar,
	  Mem = ~s.get_mem(Var0),
	  Mem = cvar(C) ->
	    Type = ~s.get_type(Var0),
	    Mem2 = cvar(address(C)),
	    var_mem_type(Var, Mem2, mut(Type)),
	    gn_cont(true)
	; ptoc__impcomp:error(['bad call to forcedmut of ', Var0])
	).
gn_unify_val__eval('$evalmem'(Mem), Var) :-
	% TODO: change name! fix semantics...
        var_mem_type(Var, evalmem(Mem), any),
	gn_cont(true).
gn_unify_val__eval('$consprop'(Val2), Var) :-
	new_cons(Val2, Var),
	gn_cont(true).
% TODO: check again...
gn_unify_val__eval(Fun, Var) :-
	gn_term(~funcall(Fun), ValR0, ValType),
	gn_unify_val__store(Var, ValR0, ValType).

% TODO: add a queue as output of gn_term?
%   e.g. X=f(f(g)) should be compiled as A=g,B=f(A),X=f(B)
%   but it is not... since most of the time we can emit code with nested expressions
%   we need a temporal variable elimination process, it can be done later or it
%   can be done on the fly... e.g. to compile goal G, create output vars of G and mark them as unique,
%   compile unifications of args (and expand funtions here), but store C code as unique_use(XCCode, XRealCCode)
%   then substitute if possible...
gn_unify_val__store(Var, ValR0, ValType) :-
	( ValType = predabs_type(Mem) ->
	    var_mem_type(Var, Mem, predabs),
	    gn_cont(true)
	; ( coerce_local_var(ValType, Var, NewValType, NewValR) ->
	      true
	  ; NewValR = ValR0,
	    NewValType = ValType
	  ),
	  gn_local_var(NewValType, Var, LocalR, VarR),
	  AR0 = seq(LocalR, (VarR = NewValR)),
	  gn_emit(AR0)
	).
}.

{
:- fluid s :: codegen_s + u.
fresh_var(Var) :- var(Var), !.
fresh_var(Var) :-
	Var instance_of termvar,
	Mem = ~s.get_mem(Var),
	Mem = nomem.
}.

{
:- extends absmach_ctx.
:- fluid tbl :: bb_tbl + u.
:- fluid s :: codegen_s + u.
:- fluid cstat :: bb_code.
gn_control__call(AR, AType, Xs2) :-
	trust(AType instance_of ltype),
	% TODO: make it general... put prototype on the fly
	CastType = ( AType.consequence(ho_semidet(0)), Xs2 = [] ? cbool0_t
		   | AType.consequence(ho_semidet(1)), Xs2 = [_] ? cbool1_t
		   | AType.consequence(ho_semidet(2)), Xs2 = [_,_] ? cbool2_t
		   | AType.consequence(ho_semidet(3)), Xs2 = [_,_,_] ? cbool3_t
		   ),
	!,
	% TODO: fix! missing parenthesis in macros makes 'quoted' necessary
	R0 = call(quoted(deref(cast(CastType, AR))), ~add_context_args(with_worker, Xs2)),
	% TODO: missing update abstract state
	S0x = ~s,
	S0y = ~s,
	gn_cond(R0, S0x, S0y).
gn_control__call(AR, AType, Xs2) :-
	trust(AType instance_of ltype),
	% TODO: make it general... put prototype on the fly
	CastType = ( AType.consequence(ho_det_with_failins(0)), Xs2 = [] ? cbool0_t
		   | AType.consequence(ho_det_with_failins(1)), Xs2 = [_] ? cbool1_t
		   | AType.consequence(ho_det_with_failins(2)), Xs2 = [_,_] ? cbool2_t
		   | AType.consequence(ho_det_with_failins(3)), Xs2 = [_,_,_] ? cbool3_t
		   ),
	!,
	% TODO: fix! missing parenthesis in macros makes 'quoted' necessary
	R0 = call(quoted(deref(cast(CastType, AR))), ~add_context_args(with_worker, Xs2)),
	% TODO: missing update abstract state
	S0x = ~s,
	S0y = ~s,
	gn_cond_with_failins(R0, S0x, S0y).
gn_control__call(AR, AType, Xs2) :-
	trust(AType instance_of ltype),
	% TODO: merge with next one!
	Imp = ~s.imp,
	Imp = nondet,
	s.cont_is_void,
	!,
	% TODO: too specific??
	( AType.consequence(st_nondet(_)) ->
	    CName = AR, % TODO: strange?
	    AR1 = return(call(CName, ~add_context_args(with_worker, Xs2)))
	; AType.consequence(ho_nondet(0)), Xs2 = [] ->
	    % TODO: fix! missing parenthesis in macros makes 'quoted' necessary
	    AR1 = return(call(quoted(deref(cast(cinsnp0_t, AR))), ~add_context_args(with_worker, [])))
	; % TODO: incomplete implementation!
	  ptoc__impcomp:error(['do not know how to jump to ', AR, ' with arguments ', Xs2])
	),
	cstat.emit_csingle(AR1).
% Cont= ~call_with_cont_to_bcp_jump(G): if the predicate has the form B,call(Cont2), Cont2 is executed as a bcp_jump to Cont... call B and obtain Cont
gn_control__call(AR, AType, Xs2) :-
	% TODO: merge with previous one!
	Imp = ~s.imp,
	Imp = emu,
	s.cont_is_emuctx,
	!,
	% TODO: too specific??
	( reduce_to_bcp_jump(AR, AType, Xs2, JumpGoal) ->
	    true
	; % TODO: this is a compilation error
	  ptoc__impcomp:error(['goal ', AR, ' with arguments ', Xs2, ' cannot be reduced to a bcp jump'])
	),
	gn_conj(JumpGoal).
% TODO: use a mutable var with types 
% TODO: later, change it so that the var is not mutable (it is not necessary)
gn_control__call(AR, AType, Xs2) :-
	trust(AType instance_of ltype),
	( AType.consequence(ho_det(0)), Xs2 = [] ->
	    % TODO: make it general... put prototype on the fly
	    CastType = cvoid0_t
	; fail
	),
	!,
	% TODO: fix! missing parenthesis in macros makes 'quoted' necessary
	R = call(quoted(deref(cast(CastType, AR))), ~add_context_args(with_worker, Xs2)),
	gn_emit(R).
gn_control__call(_, AType, _) :- !,
	Imp = ~s.imp,
	ptoc__impcomp:error(['not supported $call to type ', AType, ' from imp ', Imp, ' with continuations ', ~s.c]).
}.

{
:- extends absmach_ctx.
:- fluid tbl :: bb_tbl + u.
reduce_to_bcp_jump(AR, AType, Xs2, JumpGoal) :-
	trust(AType instance_of ltype),
	( uses_cont_to_bcp_jump(AType) ->
	    Y = ~funcall('$inline_expr'(~gn_call_with_cont_to_bcp_jump(AR, AType, Xs2), bcp)),
%	    JumpGoal = ~bcp_jump([set_p, Y])
	    JumpGoal = ~bcp_jump([recover_frame__set_p, Y])
	; AType.consequence(ref1(definition)), Xs2 = [] ->
	    Y = ~funcall('$inline_expr'(call('DEF_INSNP', [AR]), bcp)),
	    JumpGoal = ~bcp_jump([set_p, Y])
	; AType.consequence(p0emu), Xs2 = [] ->
	    % like bcp but for code that do not read the value of P (e.g. fail instruction, because it will overwrite the value of P)
	    Y = ~funcall('$inline_expr'(AR, bcp)),
	    JumpGoal = ~bcp_jump([no_set_p, Y])
	; AType.consequence(bcp), Xs2 = [] ->
	    Y = ~funcall('$inline_expr'(AR, bcp)),
	    JumpGoal = ~bcp_jump([set_p, Y])
	).
}.

{
:- extends absmach_ctx.
uses_cont_to_bcp_jump(AType) :-
	trust(AType instance_of ltype),
	( AType.consequence(st_nondet(_)) -> true
	; AType.consequence(ho_nondet(_)) -> true
	; fail
	).
}.

bcp_jump(Ys, Goal) :- Goal =.. [bcp_cont|Ys].

{
:- extends absmach_ctx.
:- fluid tbl :: bb_tbl + u.
gn_call_with_cont_to_bcp_jump(AR, AType, Xs2, R) :-
	trust(AType instance_of ltype),
	( AType.consequence(st_nondet(_)) ->
	    AR1 = AR % TODO: strange?
	; AType.consequence(ho_nondet(0)), Xs2 = [] ->
	    % TODO: put the cast in trust_typed
	    % TODO: fix! missing parenthesis in macros makes 'quoted' necessary
	    AR1 = quoted(deref(cast(cinsnp0_t, AR)))
	),
	R = call('CINSNP__LOOP', [call(AR1, ~add_context_args(with_worker, Xs2))]).
}.

% Apply 'macromodes' to the arguments (those cannot be inferred from types).
% m: takes the variable (e.g. C code 'foo(c)', where 'c' is taken as a variable and not its value)
% c: takes the value (e.g. C code 'foo(c)', where 'c' means the value of 'c')
% TODO: 'm' requires a special imptype (mutables, etc.)
postargs([], [], []).
postargs([Y|Ys], [T|Ts], [Y2|Ys2]) :-
	Y2 = ( T = m ? ~deref_c(Y) | Y ),
	postargs(Ys, Ts, Ys2).

{
:- extends absmach_ctx.
:- fluid tbl :: bb_tbl + u.
:- fluid s :: codegen_s + u.
do_not_materialize(Member) :-
	nonvar(Member),
	s.get_global_type(GlobalType),
	GlobalType.field_prop(Member, do_not_materialize),
	!.
}.

{
:- extends absmach_ctx.
:- fluid tbl :: bb_tbl + u.
:- fluid s :: codegen_s + u.
:- fluid cstat :: bb_code.
% TODO: use ct_global to use only one set_mode, do not use update_mode...
% TODO: merge with a general assign!!
gn_assign(old, A, M) :-
	GlName = mode,
	nonvar(A),
	A = ~funcall(A1),
	A1 == GlName,
	!,
	GlType = mode,
	trust(GlType instance_of ltype),
	gn_term(M, _, M1Type),
	( s.get_mutglobal_val_type(GlName, M0Type),
	  trust(M0Type instance_of ltype),
	  MExit = ~M0Type.compare(M1Type), MExit = true ->
	    MR = '$nop'
	; ( MValType = ~GlType.coerce_from_atom(t_atom(M)) ->
	      true
	  ; ptoc__impcomp:error(['global ', GlName, ' cannot be assigned with ', M])
	  ),
	  ( do_not_materialize(GlName) ->
	      MR = '$nop'
	  ; gn_term(A, MemberR, _AType),
	    gn_unboxed(GlType, M, Mv),
	    MR = (~deref_c(MemberR) = Mv)
	  ),
	  s.set_mutglobal_val_type(GlName, MValType)
	),
	gn_emit(MR).
% TODO: hardwired, fix
gn_assign(old, A, PA) :-
	nonvar(A),
	A = ~funcall(A1),
	spec_global_predabs(GlName),
	A1 == GlName,
	gn_term(PA, PA2, PAType),
	PAType = predabs,
	PA2 = temp_predabs(PAMem),
	!, 
        % TODO: does predabs_type belong to the type or the mem?
	s.set_mutglobal_val_type(GlName, predabs_type(PAMem)),
	gn_cont(true).
gn_assign(Mutinit, A, B) :-
	gn_term(B, BR0, BType),
	% get A and type (once it is declared for sure)
	% declare A if not declared
        ( ( Mutinit = new(_MutType) ; Mutinit = clone ) ->
	    ( fresh_var(A) ->
	        true % TODO: use MutType! This may be incorrect
	    ; ptoc__impcomp:error(['mutable initialization of variable ', A, ' with initial value ', B, ' will fail because the first variable is already initialized'])
	    ),
	    % TODO: delay if enctype of B is not known
	    A = ~termvar.new,
	    gn_local_mutable(BType, A, Mut, _),
	    R = seq(Mut, R0),
	    gn_term(A, AR, _), % TODO: do not use gn_term?
	    AType0 = BType, % TODO: sure? use _MutType?
	    trust(AType0 instance_of ltype),
	    BR = BR0
	; Mutinit = old ->
	    gn_term(A, AR, AType),
	    R = R0,
	    % check that types are ok
	    ( AType = ref0(mut(AType0)) ->
	        true
	    ; AType = mut(AType0) ->
	        true
	    ; % TODO: emit another error if the type cannot be proved to be a mutable
	      ptoc__impcomp:error(['the assignment of the nonmutable ', A, ' (typed as ', AType, ') with value ', B, ' is not valid'])
	    ),
	    trust(AType0 instance_of ltype),
	    % TODO: WRONG! check that B type is inside the type that the mutable can hold
	    ( BType = t_atom(BAtom),
	      _BType2 = ~AType0.coerce_from_atom(t_atom(BAtom)) ->
	        gn_unboxed(AType0, BAtom, BR1),
		BR = BR1
	    ; AType0.can_be_assigned(BType) ->
	        BR = BR0
	    ; ptoc__impcomp:error(['invalid assignment to mutable of type ', AType0, ' with value of type ', BType, ' in assigment: ', A, '<-', B])
	    )
	; errlog:bug(['bad mutinit ', Mutinit]),
	  fail
	),
	% assignment code
	( Kind = ~AType0.kind, Kind = struct ->
	    AType0C = ~AType0.ctype,
	    R0 = (call(memcpy, [AR, address(BR), call(sizeof, [AType0C])]))
	; R0 = ((~deref_c(AR))=BR)
	),
	gn_emit(R).
}.

spec_global_predabs(bcp_jump).

{
:- fluid cexpr :: any.
:- fluid exprtype :: any.
gn_output(Type, CExpr) :-
	~cexpr = CExpr, ~exprtype = Type.
}.

{
:- extends absmach_ctx.
:- fluid tbl :: bb_tbl + u.
:- fluid s :: codegen_s + u.
:- fluid cexpr :: any.
:- fluid exprtype :: any.
% internal function definitions
%gn_eval('$varg'(X), X, Type) :- var(X), !, Type = unknown.
gn_eval('$varg'(X)) :- number(X), !, gn_output(unknown, X).
gn_eval('$varg'(X)) :- atom(X), !, gn_output(unknown, X).
gn_eval('$varg'(X)) :- X = string(_), !, gn_output(unknown, X).
gn_eval('$varg'(X)) :- X = q(_, _), !, gn_output(unknown, X).
gn_eval('$varg'(float(X))) :- !,
	SS = ~tbl.symboltbl,
	symboltbl__define(float(X), Identifier, SS),
	% TODO: binary blob type...
	gn_output(mut(tagged), Identifier).
gn_eval('$varg'(bignum(X))) :- !,
	SS = ~tbl.symboltbl,
	symboltbl__define(bignum(X), Identifier, SS),
	% TODO: binary blob type...
	gn_output(mut(tagged), Identifier).
gn_eval('$varg'(atom(X))) :- !,
	SS = ~tbl.symboltbl,
	symboltbl__define(atom(X), Identifier, SS),
	gn_output(tagged, Identifier).
gn_eval('$varg'(str(X))) :- !,
	SS = ~tbl.symboltbl,
	symboltbl__define(functor(X), Identifier, SS),
	% TODO: a functor
	gn_output(tagged, Identifier).
gn_eval('$vargliveinfo_set'(HeapSize, LiveSet)) :- !,
	Arity = ~liveset_to_arity(LiveSet),
	SS = ~tbl.symboltbl,
	symboltbl__define(liveinfo(HeapSize, Arity), Identifier, SS),
	gn_output(bcp, Identifier).
gn_eval('$vargfailcont'(Next, NextIn)) :- !,
	trust(Next instance_of predicate_x),
	Label = ~pred_label(Next),
	NextImp = ~Next.get_prop(imp),
        % TODO: this is wrong, registers may contain holes
	NextInCount = ~liveset_to_arity(NextIn),
	SS = ~tbl.symboltbl,
	symboltbl__define(failcont(NextImp, Label, NextInCount), Identifier, SS),
	gn_output(ref1(try_node), Identifier).
gn_eval('$vargnullfailcont'(As0)) :- !, % TODO: dirty
	maybe_varg_get(As0, As),
	SS = ~tbl.symboltbl,
	symboltbl__define(failcont(nondet, none, ~liveset_to_arity(As)), Identifier, SS),
	gn_output(ref1(try_node), Identifier).
gn_eval('$vargsuccesscont'(Next, FrameLiveSize)) :- !,
	trust(Next instance_of predicate_x),
	Label = ~pred_label(Next),
	NextImp = ~Next.get_prop(imp),
	SS = ~tbl.symboltbl,
	symboltbl__define(successcont(NextImp, Label, FrameLiveSize), Identifier, SS),
	gn_output(bcp, Identifier).
gn_eval('$vargnullsuccesscont'(FrameLiveSize0)) :- FrameLiveSize0 = ~funcall('$varg'(FrameLiveSize)), !, % TODO: dirty
	SS = ~tbl.symboltbl,
	symboltbl__define(successcont(nondet, none, FrameLiveSize), Identifier, SS),
	gn_output(bcp, Identifier).
gn_eval('$vargexeccont'(Label)) :- !,
	Name = ~ident(Label), Identifier = ~atom_concat(Name, '_l'),
	SS = ~tbl.symboltbl,
	symboltbl__define(execcont(Label), Identifier, SS),
	gn_output(unknown, Identifier).
gn_eval('$vargpredtblentry'(X)) :- !,
	SS = ~tbl.symboltbl,
	symboltbl__define_predicate(predtblentry(X), PredEntry, SS),
	gn_output(unknown, PredEntry).
gn_eval('$vargxarity'(As0)) :- !,
	maybe_varg_get(As0, As),
        % TODO: this is wrong, registers may contain holes?
	gn_output(intmach, ~liveset_to_arity(As)).
gn_eval('$vargv'(X)) :- atom(X), !,
	mark_used_autov(X, R, Type),
	gn_output(Type, R).
gn_eval('$vargtype_size'(T)) :- !,
	R = type_size(T),
	gn_output(intmach, R).
gn_eval('$vargtype_arity'(X0)) :- !,
	maybe_varg_get(X0, X),
	X = str(_/Arity),
	gn_output(intmach, Arity).
gn_eval('$vargaddress'(X)) :- !,
	gn_term(X, X2, _Type2),
	gn_output(unknown, address(X2)).
gn_eval('$vargelement'(X, Y)) :- !,
	gn_term(X, X2, _TypeX),
	gn_term(Y, Y2, _TypeY),
	gn_output(unknown, element(X2, Y2)).
gn_eval('$tagged_hashtab_get'(Xs, X)) :- !,
	gn_term(X, X2, _TypeX),
	gn_hashtab(Xs, Identifier),
	gn_output(intmach, call('tagged_hashtab_get', [Identifier, X2])).
%%%
% TODO: mem values are too complicated!
% TODO:   find a better formalization (e.g. X= ~bcop(1), ins(X) ... where X is a special mut that do not changes (a reference) and change all access to instruction operands to @X or use X=@(~bcop(1)) but store it as a constraint or delayed computation)
gn_eval('$custommem_ins_a'(I, Type0)) :- !,
	( calc_a(I, T, Off) ->
	    gn_pmem__op(T, Off, R),
	    gn_output(Type0, R)
	; % TODO: error or bug?
	  ptoc__impcomp:error(['unable to eval calc_a'])
	).
gn_eval('$custommem_x'(I)) :- !,
	gn_output(mut(tagged), address(call('X', [I]))).
gn_eval('$custommem_y'(I)) :- !,
	gn_output(mut(tagged), address(call('Y', [I]))).
gn_eval('$custommem_xb'(X)) :- !,
	gn_term(X, IR, _),
	gn_output(mut(tagged), address(call('Xb', [IR]))).
gn_eval('$custommem_yb'(X)) :- !,
	gn_term(X, IR, _),
	gn_output(mut(tagged), address(call('Yb', [IR]))).
gn_eval('$custommem_lrg'(X)) :- !,
	LType = mut(tagged),
	trust(LType instance_of ltype),
	T = ~LType.ctype,
	gn_term(X, IR, _),
	% TODO: wrong type?
	gn_output(mut(tagged), cast(T, address(IR))).
gn_eval('$custommem_linf'(X)) :- !,
	LType = bcp,
	trust(LType instance_of ltype),
	T = ~LType.ctype,
	gn_term(X, IR, _),
	% TODO: wrong type?
	gn_output(bcp, cast(T, address(IR))).
gn_eval('$custommem_smbt'([])) :- !, % TODO: too specific
	gn_output(nonreftagged, atom_nil).
gn_eval('$custommem_smbpred'(N/A)) :- !, % for C predicates
	absmach_blt_dec(N, A, R),
	% TODO: wrong type!
	gn_output(any, R).
gn_eval('$custommem_default_choice') :- !, % TODO: a boxed previous_choice (strange: it can be read but not written, it is asymmetrical...)
	gn_term(~funcall(v__default_choice), R, _Type),
	% TODO: right?
	gn_output(mut(tagged), R).
%%%
% Generate code for predicates and built-ins in functional notation
gn_eval(X) :-
	ipexp.exp_unfold(X, X2),
	!,
	gn_term(X2, R, Type),
	gn_output(Type, R).
% TODO: may not be correct if those are the input of goals that are unfolded, check!!
gn_eval('$op_at_p'(P)) :- !,
	% TODO: check types of P and I
	gn_term(P, PR, _TypeP),
        ( ipexp.ftype__dectype(f_o, T) -> true ),
	trust(T instance_of ltype),
	CT = ~T.ctype,
	gn_output(ftype_o, call('BCOp', [PR, CT, 0])).
gn_eval('$inline_expr'(X, Type0)) :- !, % TODO: remove!
	gn_output(Type0, X).
gn_eval('@'(X)) :- !,
	gn_term(X, XR1, Type0),
	Type1 = ~Type0.norm_def,
	( nonvar(X), X = ~funcall(GlName), GlName == mode -> % TODO: too specific!
	    s.get_mutglobal_val_type(GlName, Type),
	    gn_output(Type, ~deref_c(XR1))
        ; nonvar(X), X = ~funcall(GlName), nonvar(GlName), spec_global_predabs(GlName) -> % TODO: too specific!
	    s.get_mutglobal_val_type(GlName, Type),
	    gn_output(Type, none)
% TODO: distinguish between cvar and mvar!!! the gn_term of a cvar is itself, the gn_term of a mvar is address(ofitself) and here deref is always introduced!!! 
	    % TODO: wrong!! put a deref and ensure that there is an address in XR1 (if not it will not work with functions returning mut)
	; Type1 = ref0(mut(Type)) ->
	    gn_output(Type, ~deref_c(XR1))
	; Type1 = mut(Type) ->
	    gn_output(Type, ~deref_c(XR1))
	; ptoc__impcomp:error(['cannot get value of non-mutable ', X, ' with type \'', Type0, '\''])
	).
% TODO: too specific!!
gn_eval(tagged(Tag, Val)) :- atom(Tag), !,
	BaseTagType = 'tagged_$key',
	( type_tagged__from_tag(BaseTagType, Tag, Type0) ->
	    gn_term(Val, Val2, _),
	    gn_unboxed(BaseTagType, Tag, Tag2), % TODO: do unbox automatically
	    gn_output(Type0, call('Tagp', [Tag2,Val2]))
	; ptoc__impcomp:error(['unknown tagged ', Tag, ' ', Val])
	).
gn_eval('$element'(X, I)) :- !,
	% TODO: check _TypeI
	gn_terms([X,I], [XR0,IR], [TypeX0,TypeI]),
	trust(TypeI instance_of ltype),
	( TypeI.can_index_array ->
	    true
	; ptoc__impcomp:error(['type cannot index array: ', TypeI])
	),
	derefmut_array(TypeX0, XR0, TypeX, XR),
	( TypeX.elem_type0(Type0, MemLinks) ->
	    true
	; ptoc__impcomp:error(['accessing element of nonarray type: ', TypeX])
	),
	XRb = element(XR, IR),
	( MemLinks = mmem ->
	    XR1 = address(XRb)
	; MemLinks = cmem ->
	    XR1 = XRb
	; ptoc__impcomp:error(['unknown memlinks ', MemLinks])
	),
	gn_output(Type0, XR1).
% TODO: kludge to implement threaded bytecode
gn_eval(A0) :- A0 = ~mcall(X, A), nonvar(A), A = ~funcall('$elem_inv'(X2, B)), X == X2, !, % TODO: deprecate $element, merge with code bellow
	gn_term(X, _, _), % TODO: necessary? forces generation of X
	gn_term(B, XR1, Type),
	gn_output(Type, XR1).
gn_eval(A0) :- A0 = ~mcall(X, A), !,
	gn_term(X, XR, Type00),
	( atom(A) -> true
	; % TODO: this is a compilation error
	  ptoc__impcomp:error(['member name ', A, ' is not valid'])
	),
	derefmut(Type00, XR, Type1, XRb),
	Type2 = ~Type1.norm_eval,
	( Type2 = t_dep(tagged, TagType), A = tag -> % TODO: make more generic
	    gn_output(TagType, call('TagOf', [XRb]))
	; Type2 = t_dep(tagged, TagType), A = ptr -> % TODO: add other fields and make more generic!
	    trust(TagType instance_of ltype),
	    Type3 = tagged,
	    trust(Type3 instance_of ltype),
	    Type3.field(A, Type, MemLinks),
	    ( MemLinks = cmem ->
	        true
	    ; ptoc__impcomp:error(['unsupported memlinks ', MemLinks, ' for tagged member ptr'])
	    ),
	    ( Tag = ~TagType.concrete_atom ->
	        BaseTagType = 'tagged_$key',
		gn_unboxed(BaseTagType, Tag, Tag2), % TODO: do unbox automatically
		XR1 = call('TagpPtr', [Tag2,XR])
	    ; XR1 = call('TaggedToPointer', [XR]) % TODO: check that ptr is defined for all tags in the precondition
	    ),
	    gn_output(Type, XR1)
	; Type2.field(A, Type, MemLinks),
	  ( MemLinks = mmem ->
	      XR1 = address(member(XRb, A))
	  ; MemLinks = cmem ->
	      XR1 = member(XRb, A)
	  ; ptoc__impcomp:error(['unknown memlinks ', MemLinks])
	  ),
	  gn_output(Type, XR1)
	).
gn_eval(A) :-
	atom(A),
	s.get_global_type(Type1),
	Type1.field_query(A, Type, MemLinks),
	!,
	( MemLinks = stamem(Mem) ->
	    true
	; ptoc__impcomp:error(['unknown memlinks ', MemLinks])
	),
	gn_var__1(Mem, XR1),
	gn_output(Type, XR1).
gn_eval('$mut_move'(X, Y)) :- !, % get the adjacent (Y steps) mutable
	gn_terms([X,Y], [XR,YR], [XType0,YType]),
	trust(XType0 instance_of ltype),
	trust(YType instance_of ltype),
	XType = ~XType0.norm_def,
	( XType = mut(_), YType.consequence(intmach) -> true
	; ptoc__impcomp:error(['unsupported types for $mut_move ', XType, ' ', YType])
	),
	% TODO: the type is wrong! use the mutable type, not the actual type
	gn_output(XType, XR+YR).
gn_eval('$subarray'(X, Y)) :- !, % get a subarray reference (does not copy!)
	% note: for all J in [K..L-1] where L is the length of the array, S = ~'$subarray'(X,K), X[J]==S[J-K]
	gn_terms([X,Y], [XR,YR], [XType0,YType]),
	trust(XType0 instance_of ltype),
	trust(YType instance_of ltype),
	XType = ~XType0.norm_def,
	( XType = ref1(array(_)), YType.consequence(intmach) -> true
	; ptoc__impcomp:error(['unsupported types for $subarray ', XType, ' ', YType])
	),
	% TODO: C automatically moves the pointer accordingly to the type of XR
	% TODO: no bounds check!
	gn_output(XType, XR+YR).
% cast (used for some code definitions)
% TODO: THIS IS A KLUDGE!
gn_eval('$cast'(X, Type0)) :- !, % TODO: check that the cast is valid
	trust(Type0 instance_of ltype),
	gn_term(X, R0, Type1),
	Type0e = ~Type0.norm_eval,
	Type1e = ~Type1.norm_eval,
	Type2 = ( Type0e = t_dep(Base, _), Type1e = t_dep(Base, _) ? ~Type0e.glb(Type1e)
		| Type0
		),
	trust(Type2 instance_of ltype),
	T = ~Type2.ctype,
	gn_output(Type2, cast(T, R0)).
% term X encoded as type Type0 (necessary to obtain the unboxed representation of X)
gn_eval('$ccons'(X, T)) :- !,
	gn_output(T, X).
gn_eval('$typed'(TypeA, X)) :- atom(TypeA), !,
	trust(TypeA instance_of ltype),
	TypeB = ~TypeA.basic_def,
	( number(X), native_number_type_cdef(TypeB, _) ->
	    % TODO: should it be TypeB? (problems with arith domains)
	    gn_output(TypeA, X)
	; TypeB.coercible_from_atom ->
	    gn_output(~TypeB.coerce_from_atom(t_atom(X)), ~gn_unboxed(TypeB, X))
	; ptoc__impcomp:error(['not coercible from term ', X, ' to type ', TypeA])
	).
% TODO: improve, explicit allocation
% TODO: return a maybe_ref1 type!
gn_eval('$alloc'(Allocator, T0)) :- !,
	alloc_method(Allocator, AllocMethod),
	( T0 = array(T1, ExprSize) ->
	    ( nonvar(T1), T1 = ref0(mut(T2)) ->
	        T = array(T1),
	        Size = (ExprSize * ~funcall('$type_size'(T2)))
	    ; ptoc__impcomp:error(['not acceptable type in array allocation ', T1])
	    )
	; T = T0,
	  Size = ~funcall('$type_size'(T))
	),
	AllocatorExpr =.. [AllocMethod, Size],
	TAlloc = ref1(T),
	gn_term(~funcall('$trust_typed'(~funcall(AllocatorExpr), TAlloc)), R, Type),
	gn_output(Type, R).
% TODO: improve, explicit re-allocation of arrays
% TODO: return a maybe_ref1 type!
% TODO: check type!
% TODO: it should also be possible to resize structures that contains arrays in the tail
gn_eval('$resize'(Allocator, Expr, NewSize)) :- !,
	gn_term(Expr, _Re, Te), % TODO: do not throw Re? But I just want the type
	realloc_method(Allocator, ReallocMethod),
	( Te = ref1(array(T1)), nonvar(T1), T1 = ref0(mut(T2)) ->
	    TRealloc = Te, % TODO: change if we include the size in the type
	    Size = (NewSize * ~funcall('$type_size'(T2)))
        ; ptoc__impcomp:error(['$resize_array on non array type ', Te])
        ),
	ReallocatorExpr =.. [ReallocMethod, Expr, Size],
	gn_term(~funcall('$trust_typed'(~funcall(ReallocatorExpr), TRealloc)), R, Type),
	gn_output(Type, R).
% TODO: improve, the 'null' element of ref1 (we should define a maybe_ref1, which contains it, and a ref1 that cannot be null)
gn_eval('$null_ref1'(T)) :- !,
	T2 = ref1(T),
	gn_term(~funcall('$ccons'('NULL', T2)), R, Type),
	gn_output(Type, R).
% TODO: improve
gn_eval('$to_ref1'(T)) :- !,
	gn_term(T, R0, Type0),
	( Type0 = ref0(array(TypeA)) ->
	    gn_output(ref1(array(TypeA)), R0)
	; Type0 = ref0(mut(TypeA)) ->
	    gn_output(ref1(mut(TypeA)), R0)
	; Type0 = ref0(TypeA) -> % TODO: check it!
	    gn_output(ref1(TypeA), address(R0))
	; ptoc__impcomp:error(['$to_ref1 failed, cannot obtain a reference to term ', T, ' of type ', Type0])
	).
% TODO: too specific!!
gn_eval('$trust_typed'(X, Type0)) :- !, % TODO: use intermediate variables and checks!
	trust(Type0 instance_of ltype),
	gn_term(X, R, Type1),
	Type0e = ~Type0.norm_eval,
	Type1e = ~Type1.norm_eval,
	Type = ( Type0e = t_dep(Base, _), Type1e = t_dep(Base, _) ? ~Type0e.glb(Type1e)
	       | Type0
	       ),
	gn_output(Type, R).
% TODO: too specific!!
gn_eval(atom(A)) :- !,
	( absmach_def_lowatom(A, CName) ->
	    true
	; ptoc__impcomp:error(['unknown atom ', A])
	),
	gn_output(tagged, CName).
gn_eval('$funcall'(A, Xs)) :- !,
	% TODO: make it general... put prototype on the fly
	gn_term(A, A2, AType),
	gn_terms(Xs, Xs2, _),
	( AType.consequence(ho_fun(1)), Xs = [_] ->
	    HType = tagged,
	    CastType = ctagged1_t
	; AType.consequence(ho_fun(2)), Xs = [_,_] ->
	    HType = tagged,
	    CastType = ctagged2_t
	; ptoc__impcomp:error(['cast type failed for ', Xs])
	),
	% TODO: fix! missing parenthesis in macros makes 'quoted' necessary
	gn_output(HType, call(quoted(deref(cast(CastType, A2))), ~add_context_args(with_worker, Xs2))).
gn_eval('$vifuncallr'(GId, Xs1)) :- !,
	Label = ~pred_label(GId),
	translate_label(Label, Label2),
	gn_terms(Xs1, Xs2, _),
	% TODO: fix! get correct type!
	gn_output(tagged, call(Label2, ~add_context_args(with_worker, Xs2))).
gn_eval('$get_pnext') :- !,
        % TODO: this is a bcp continuation, use more abstract 'continuation' in user code
	% TODO: check that we have P
	gn_output(bcp, ~gn_pmem__next).
gn_eval('$cstring'(X)) :- !,
	gn_output(mut(char), string(X)).
gn_eval('$ctlength'(Xs0)) :- !,
	maybe_varg_get(Xs0, Xs),
	gn_output(intmach, ~length(Xs)).
% TODO: too specific
gn_eval(xcomp(I)) :- !,
	gn_term(I, IR, _),
	gn_output(mut(tagged), address(call('X', [IR]))).
% TODO: too specific
gn_eval(ycomp(I)) :- !,
	gn_term(I, IR, _),
	gn_output(mut(tagged), address(call('Y', [IR]))).
% TODO: too specific
gn_eval(xbcomp(I)) :- !,
	gn_term(I, IR, _),
	gn_output(mut(tagged), address(call('Xb', [IR]))).
% TODO: too specific
gn_eval(ybcomp(I)) :- !,
	gn_term(I, IR, _),
	gn_output(mut(tagged), address(call('Yb', [IR]))).
gn_eval('$type_size'(AType)) :- !,
	type_size(AType, Size),
	gn_output(intmach, Size).
% TODO: think about this...
gn_eval('$type'(Name)) :- !,
	trust(Name instance_of ltype),
	R = ~Name.ctype,
	% TODO: wrong type
	gn_output(unknown, R).
% TODO: improve (this is a kludge to declare uninitialized global variables)
gn_eval('$uninit'(Type)) :- !,
	gn_output(Type, '$uninit').
% TODO: improve
gn_eval('$array_elems'(~funcall('$array'(Type, Elems0)))) :- !,
	gn_terms(Elems0, Elems, _),
	LType = ref0(array(Type)),
	trust(LType instance_of ltype),
	CType = ~LType.ctype,
	length(Elems, Size),
	% TODO: check type?
	gn_output(ref0(array(Type, Size)), aggregate(CType, Elems)).
% TODO: improve
gn_eval('$array_elems'(~funcall('$array_r0'(Type, Elems0)))) :- !,
	gn_terms(Elems0, Elems, _),
	length(Elems, Size),
	% TODO: check type?
	gn_output(ref0(array(ref0(Type), Size)), aggregate(Elems)).
% TODO: improve
gn_eval('$array_size'(~funcall('$array'(_, Elems)))) :- !,
	length(Elems, V),
	gn_output(intmach, V).
gn_eval('$ftype_str0') :- !,
	% TODO: fill good type
	% TODO: write a special entry for holes?
        gn_output(unktype, call('FTYPE_STR0', [])).
gn_eval('$ftype_array'(IdI, IdElem)) :- !,
	% TODO: fill good type
        gn_output(unktype, call('FTYPE_ARRAY', [IdI, IdElem])).
gn_eval('$ftype_str'(Ids)) :- !,
	Length = ~length(Ids),
	% TODO: fill good type
        gn_output(unktype, call('FTYPE_STR', [Length, call('BRACES', Ids)])).
gn_eval('$ftype_blob') :- !,
	% TODO: fill good type
        gn_output(unktype, call('FTYPE_BLOB', [])).
gn_eval('$ftype_basic'(Size,SMethodId,LMethodId)) :- !,
	% TODO: fill good type
        gn_output(unktype, call('FTYPE_BASIC', [Size, SMethodId, LMethodId])).
gn_eval('$ftype__enc_ctype'(T)) :- !,
	( ipexp.ftype__enctype(T, T1) ->
	  trust(T1 instance_of ltype),
	  ( T2 = ~T1.ctype ->
	      true
	  ; ptoc__impcomp:error(['unknown imptype for type ', T2])
	  )
	; ptoc__impcomp:error(['unknown enctype for ftype ', T])
	),
	% TODO: fill good type
        gn_output(unktype, type_exp(T2)).
gn_eval('$ftype__id'(X0)) :- !,
	atom(X0), !,
	( ipexp.ftype__id(X0, X) -> true ),
        gn_output(intmach, X).
gn_eval('$etype__encode'(BaseType, Atom)) :- !,
	atom(Atom), !,
        gn_output(intmach, ~ipexp.get__enum_domain__encode_atom(BaseType, Atom)).
gn_eval('$ftype__size'(X0)) :- !,
	atom(X0), !,
	( ipexp.ftype__size(X0, X) ->
	    true
	; ptoc__impcomp:error(['unknown size for ftype ', X0])
	),
        gn_output(intmach, X).
gn_eval('$struct_init'(Type, Members)) :- !,
	trust(Type instance_of ltype),
	CType = ~Type.ctype,
	gn_struct_elems(Members, MembersR),
	gn_output(Type, aggregate(CType, MembersR)).
gn_eval(G) :-
	functor(G, GN, GA),
	GA1 is GA + 1,
	( ipexp.get__op__prop(GN, GA1, detfun(HType)) -> true ; fail ),
	!,
	G =.. [_|Xs],
	gn_terms(Xs, Xs2, _),
	gn_output(HType, call(GN, ~add_context_args(with_worker, Xs2))).
gn_eval(X) :-
	functor(X, CN, CA),
	CA1 is CA + 1,
	ipexp.get__op__foreign(CN, CA1, ForeignDef),
	!,
	( ForeignDef = foreignfun(InTypes0, Type0, CName) ->
	    X =.. [_|Xs],
	    gn_terms(Xs, Ys, XTypes),
%	    errlog:trace([xt(CN, XTypes)]),
%	    errlog:trace([ft(CN, InTypes0)]),
	    ( coerce_args(XTypes, InTypes0, Ys, Ys2, _NewXTypes) ->
	        true
	    ; ptoc__impcomp:error(['unable to coerce arguments in (functional) call to ', CN, '/', CA1, '.\n  Inferred types: ', XTypes, '.\n  Required types: ', InTypes0])
	    ),
	    UseWorker = ~get_context(CN, CA1),
	    gn_output(Type0, call(CName, ~add_context_args(UseWorker, Ys2)))
	; ForeignDef = foreigncons(Type0, CName) ->
	    gn_output(Type0, CName)
	; ForeignDef = foreignvar(Type0, CName) ->
	    MemLinks = ( Type0 = ref0(mut(_)) ? mmem
		       | Type0 = mut(_) ? mmem
		       | cmem
		       ),
	    Rb = CName,
	    ( MemLinks = mmem -> R = address(Rb)
	    ; MemLinks = cmem -> R = Rb
	    ; ptoc__impcomp:error(['unknown memlinks ', MemLinks])
	    ),
	    gn_output(Type0, R)
	).
gn_eval(X) :-
	functor(X, F, N1),
	N is N1 + 1,
	ipexp.get__op__prop(F, N, unfold), % TODO: use arity
	!,
	X =.. [_|Args],
	ipexp.get_fun_def(F, N, Args, Def),
	gn_term(Def, Value, Type),
	gn_output(Type, Value).
gn_eval(G) :- !,
	functor(G, GN, GA1),
	GA is GA1 + 1,
	G =.. [_|As],
	gn_eval__2(GN, GA, As).

gn_eval__2(GN, GA, As) :-
	gn_terms(As, AsR, AsT),
	( ipexp.get__op__foreignfun_spec(GN, GA, RuleTypes, RuleRetType, In, RuleR),
	    tps_consequence(AsT, RuleTypes),
	    !,
	    In = AsR,
	    gn_output(RuleRetType, RuleR)
	; ptoc__impcomp:error(['does not know how to compile function ', GN, '/', GA, ' with input types ', AsT])
	).
}.

{
:- extends absmach_ctx.
tps_consequence([], []).
tps_consequence([A|As], [B|Bs]) :-
	trust(A instance_of ltype),
	trust(B instance_of ltype),
	A.consequence(B),
	tps_consequence(As, Bs).
}.

{
:- extends absmach_ctx.
:- fluid tbl :: bb_tbl + u.
:- fluid s :: codegen_s + u.
coerce_args([], [], [], [], []) :- !.
coerce_args([Type0|Types], [RequiredType0|RequiredTypes], [Arg0|Args0], [Arg|Args], [NewType|NewTypes]) :-
	trust(Type0 instance_of ltype),
	trust(RequiredType0 instance_of ltype),
	Type = ~Type0.norm_def,
	RequiredType = ~RequiredType0.norm_def,
	( RequiredType = iany -> % TODO: this is a kludge... define types for ground herbrand terms, types, etc.
	    Arg = Arg0,
	    NewType = Type
	; Type.consequence(RequiredType) ->
	    Arg = Arg0,
	    NewType = Type
	; Type = t_string(String), RequiredType = mut(char) ->
	    Arg = string(String),
	    NewType = mut(char)
	; ptoc__impcomp:error(['inferred type ', Type, ', but it requires a ', RequiredType])
	),
	coerce_args(Types, RequiredTypes, Args0, Args, NewTypes).
}.

alloc_method(malloc, malloc).
alloc_method(alloca, alloca).
realloc_method(malloc, realloc).
dealloc_method(malloc, free).

% C deref until the result is not a mutable
% note: this is used to make A.X and (@A).X equivalent if A is a mutable (to simplify notation)
% (that implements the rule A.X := (@A).X :- mut(A).
:- meta_predicate derefmut(?, ?, out(ltype), ?).
derefmut(Type0, R0, Type, R) :-
	( Type0 = mut(Type1) ->
	    R1 = ~deref_c(R0),
	    derefmut(Type1, R1, Type, R)
	; Type0 = ref1(Type1) ->
	    R1 = ~deref_c(R0),
	    derefmut(Type1, R1, Type, R)
	; Type0 = ref0(Type1) -> % TODO: check
	    Type = Type1,
	    R = R0
	; Type = Type0,
	  R = R0
	).

:- meta_predicate derefmut_array(?, ?, out(ltype), ?).
derefmut_array(Type0, R0, Type, R) :-
	( Type0 = mut(Type1) ->
	    R1 = ~deref_c(R0),
	    derefmut_array(Type1, R1, Type, R)
	; Type0 = ref1(array(Type1)) ->
	    R = R0,
	    Type = Type0
	; Type0 = ref1(Type1) ->
	    R1 = ~deref_c(R0),
	    derefmut_array(Type1, R1, Type, R)
	; Type = Type0,
	  R = R0
	).

{
:- extends absmach_ctx.
:- fluid tbl :: bb_tbl + u.
gn_hashtab(Xs, Identifier) :-
	gn_hashtab_elems(Xs, Ys),
	SS = ~tbl.symboltbl,
	symboltbl__define(taggedhashtab(Ys), Identifier, SS).

gn_hashtab_elems([], []).
gn_hashtab_elems([Key-_|Xs], [R|Rs]) :-
	SS = ~tbl.symboltbl,
	( Key = atom(X) ->
	    symboltbl__define(atom(X), R, SS)
	; Key = str(X) ->
	    symboltbl__define(functor(X), R, SS)
	; Key = smallint(X) ->
	    R = call('MakeSmall', [X])
	; ptoc__impcomp:error(['unsupported key in taggedhashtab: ', Key])
	),
	gn_hashtab_elems(Xs, Rs).
}.

% TODO: low level switch, where keys are numbers, cases do not repeat and there is a default option
% TODO: status is not properly updated
{
:- extends absmach_ctx.
:- fluid tbl :: bb_tbl + u.
:- fluid s :: codegen_s + u.
{
:- fluid cstat :: bb_code.
gn_switch(Var, CasesCode, DefaultCode) :-
	% TODO: check type of Var
	gn_term(Var, _VarR, VarType),
	call((
          var_type :: any <- VarType,
	  ccases :: accum(Cases),
	  gn_switch__2(CasesCode),
	  ccases.add(case('$default$', DefaultCode))
        )),
	getkeys(Cases, SwitchdefKeys),
	gn_goal('$sw'(switchdef(SwitchdefKeys), Var, Cases)).
}.
{
:- fluid var_type :: any.
:- fluid ccases :: accum.
gn_switch__2([]).
gn_switch__2([X|Xs]) :- gn_switch__3(X), gn_switch__2(Xs).

gn_switch__3(case(Key, Code)) :-
	call((
          cexpr :: any <- KeyR,
	  exprtype :: any,
	  gn_eval('$typed'(~var_type, Key))
	)),
	ccases.add(case(KeyR, Code)).
}.
}.

getkeys([], []).
getkeys([case(K,_)|Xs], [K|Ks]) :- getkeys(Xs, Ks).

{
:- extends absmach_ctx.
:- fluid tbl :: bb_tbl + u.
:- fluid s :: codegen_s + u.
gn_struct_elems([], []) :- !.
gn_struct_elems([(Member,X)|Xs], [(member(Member)#V)|Vs]) :- !,
	gn_term(X, V, _), % TODO: do not ignore type
	gn_struct_elems(Xs, Vs).
}.

deref_c(A, B) :- nonvar(A), A = address(A0), !,
	B = A0.
deref_c(A, deref(A)).

is_comparison((\==)) :- !.
is_comparison((==)) :- !.
is_comparison((>=)) :- !.
is_comparison((=<)) :- !.
is_comparison((>)) :- !.
is_comparison((<)) :- !.

{
:- extends absmach_ctx.
:- fluid tbl :: bb_tbl + u.
% TODO: rewrite so that I can use meta_syntax:decomp_goal/3
a_decomp_goal(G, GId, Args) :-
	( G instance_of termvar ->
	    GId = '$pa'(G),
	    Args = []
	; functor(G, call, N), N >= 2, G =.. [_,X|Args0], X instance_of termvar ->
	    GId = '$pa'(X),
	    Args = Args0
	; G = '$call'(X, Args0), X instance_of termvar ->
	    GId = '$pa'(X),
	    Args = Args0
	; G = '$insc'(X) -> % TODO: absmach specific! fix or make it more abstract
	    GId = '$pr'('$insdef'(X),0),
	    Args = []
	; G = '$insc_s'(Insns) -> % TODO: absmach specific! fix or make it more abstract
	    ipexp.decomp_insc_s(Insns, GId, Args)
	; functor(G, GN, GA),
	  G =.. [_|Args],
	  GId = '$pr'(GN, GA)
	).
}.

negcond(logical_not(A), A) :- !.
negcond(A, logical_not(A)).

{
:- fluid s :: codegen_s.
var_mem_type(Var, Mem, Type) :-
	s.set_type(Var, Type),
	s.set_mem(Var, Mem).

% only set the type (but the variable is still unbound)
var_nomem_type(Var, Type) :-
	s.set_type(Var, Type).
}.

{
:- extends absmach_ctx.
:- fluid tbl :: bb_tbl + u.
:- fluid s :: codegen_s + u.
extract_switcherpre([PreSwitch,Cases00|Xs], SwitcherJoin, JoinIndexExpr, Cases0, Xs) :-
	nonvar(PreSwitch),
	PreSwitch =.. [PreSwitchName,U],
	ipexp.get__swjoinable_def(PreSwitchName),
	unfolded_is_switch(Cases00, Cases0),
	!,
	SwitcherJoin = join(PreSwitchName),
	JoinIndexExpr = index_on_member(tag, 'tagged_$key', '@'(U)).
extract_switcherpre([Cases00|Xs], SwitcherJoin, JoinIndexExpr, Cases00, Xs) :-
	is_switch(Cases00), % unfold is not necessary here (it will be done later)
	!,
	SwitcherJoin = none,
	JoinIndexExpr = index_on_nothing.
}.

% TODO: kludge implementation...
is_switch(G) :- var(G), !, fail.
is_switch(G) :- G = (_;_).

{
:- extends absmach_ctx.
:- fluid tbl :: bb_tbl + u.
:- fluid s :: codegen_s + u.
unfolded_is_switch(G, Code) :- is_switch(G), !, Code = G.
unfolded_is_switch(G, Code) :-
	functor(G, GN, GA),
	ipexp.get__op__prop(GN, GA, unfold),
	!,
	G =.. [_|Xs],
	Code0 = ~getdef('$pr'(GN, GA), Xs),
	unfolded_is_switch(Code0, Code).
}.

{
:- extends absmach_ctx.
:- fluid tbl :: bb_tbl + u.
:- fluid s :: codegen_s.
gn_local_vimutable(N, Type0, Var, AR) :- !,
	Type = mut(Type0),
	trust(Type instance_of ltype),
	var_mem_type(Var, cvar(address(N)), Type),
	Type2 = ~Type.applymemlinks(mmem),
	CType = ~Type2.ctype,
	AR = declare(N, CType).
%	AR = call('DECLVAR', [Mem2, CType]).
% TODO: check that Type0 is a type!

gn_local_mutable(Type0, Var, LocalR, VarR) :-
	CVarMem = _,
	Type = mut(Type0),
	trust(Type instance_of ltype),
	var_mem_type(Var, cvar(address(CVarMem)), Type),
	Type2 = ~Type.applymemlinks(mmem),
	CType = ~Type2.ctype,
	LocalR = declare(CVarMem, CType),
	VarR = CVarMem.
}.

{
:- extends absmach_ctx.
:- fluid tbl :: bb_tbl + u.
:- fluid s :: codegen_s + u.
coerce_local_var(Type, Var, NewValType, NewValR) :-
	trust(Var instance_of termvar),
	VarType = ~s.get_type(Var),
	VarType2 = ~VarType.basic_def,
	\+ VarType2 = any,
	\+ is_native_type(VarType2),
	VarType2.coercible_from_atom,
	Type = t_atom(X),
	NewValType0 = ~VarType2.coerce_from_atom(Type),
	!,
	NewValType = NewValType0,
	gn_unboxed(VarType2, X, NewValR).
}.

% TODO: check that Type0 is a type!
{
:- extends absmach_ctx.
:- fluid tbl :: bb_tbl + u.
:- fluid s :: codegen_s.
gn_local_var(Type, Var, LocalR, VarR) :-
	CVarMem = _,
	var_mem_type(Var, cvar(CVarMem), Type),
	trust(Type instance_of ltype),
	CType = ~Type.ctype,
	LocalR = declare(CVarMem, CType),
	VarR = CVarMem.
}.

% ---------------------------------------------------------------------------
% a dummy partial evaluator for constants
% TODO: improve

% partial evaluation of constants
{
:- extends absmach_ctx.
peval_cons(X, Value) :-
	( call(( depth :: m_int <- 10, peval_cons__2(X, Value) )) ->
	    true
	; ptoc__impcomp:error(['peval cons failed for ', X])
	).
{
:- fluid depth :: m_int + u.
peval_cons__2(_, _) :- ~depth = 0, !, % max depth reached
	ptoc__impcomp:error(['max depth reached in peval_cons']).
peval_cons__2(V, Value) :- integer(V), !, Value = V.
peval_cons__2(A+B, Value) :- !,
	peval_cons__2(A, Av),
	peval_cons__2(B, Bv),
	Value is Av+Bv.
peval_cons__2(A-B, Value) :- !,
	peval_cons__2(A, Av),
	peval_cons__2(B, Bv),
	Value is Av-Bv.
peval_cons__2(A*B, Value) :- !,
	peval_cons__2(A, Av),
	peval_cons__2(B, Bv),
	Value is Av*Bv.
peval_cons__2(X, Value) :- X = ~funcall(Name), atom(Name), !,
	depth.dec(1),
	ipexp.get_fun_def(Name, 1, [], V),
	peval_cons__2(V, Value).
}.
}.

% partial evaluation of goals
{
:- extends absmach_ctx.
:- public peval_goal/2.
peval_goal(X, GoalStatus) :-
	% TODO: add Exp? use Tbl from the beginning as input argument? problem: this is used outside the compilation
	tbl :: bb_tbl <- ~bb_tbl.new,
	s :: codegen_s <- ~codegen_s.new(det),
	peval_goal__2(X, GoalStatus).
{
:- fluid tbl :: bb_tbl + u.
:- fluid s :: codegen_s + u.
peval_goal__2(X, GoalStatus) :- var(X), !, GoalStatus = unknown.
peval_goal__2(X, GoalStatus) :- X instance_of termvar, !, GoalStatus = unknown.
peval_goal__2(true, GoalStatus) :- !,
	GoalStatus = true.
peval_goal__2(fail, GoalStatus) :- !,
	GoalStatus = fail.
peval_goal__2('$vcccteq'(X0, Y0), GoalStatus) :- !,
	maybe_varg_get(X0, X),
	maybe_varg_get(Y0, Y),
	GoalStatus = ( X = Y ? true | fail ).
peval_goal__2('$vcccteqvt'(X0, Y0), GoalStatus) :- !,
	maybe_varg_get(X0, X),
	maybe_varg_get(Y0, Y),
	GoalStatus = ( X == Y ? true | fail ).
peval_goal__2('$use_opt'(Opt), GoalStatus) :-
	atom(Opt), !,
	GoalStatus = ( ipexp.get__use_opt(Opt) ? true | fail ).
peval_goal__2((A =< B), GoalStatus) :- var_cons(A, Av), var_cons(B, Bv), !,
	GoalStatus = ( Av =< Bv ? true | fail ).
peval_goal__2('$with_compiler_version'(V), GoalStatus) :- number(V), !,
	frontend:compiler_version(V2),
	GoalStatus = ( V == V2 ? true | fail ).
peval_goal__2(\+ X, GoalStatus) :- !,
	GoalStatus0 = ~peval_goal__2(X),
	GoalStatus = ( GoalStatus0 = true ? fail
		     | GoalStatus0 = fail ? true
		     | GoalStatus0 = unknown ? unknown
		     ).
peval_goal__2((A,B), GoalStatus) :- !, % TODO: remove? not a goal...
	GoalStatusA = ~peval_goal__2(A),
	GoalStatus = ( GoalStatusA = fail ? fail
		     | ~goal_status__and(GoalStatusA, ~peval_goal__2(B))
		     ).
peval_goal__2((A;B), GoalStatus) :- !, % TODO: remove? not a goal...
	GoalStatusA = ~peval_goal__2(A),
	GoalStatus = ( GoalStatusA = true ? true
		     | ~goal_status__or(GoalStatusA, ~peval_goal__2(B))
		     ).
peval_goal__2(_, GoalStatus) :- GoalStatus = unknown.
}.
}.

maybe_varg_get(X0, X) :-
	( X0 = ~funcall('$varg'(X)) -> true ; X = X0 ).

goal_status__and(fail, _, fail) :- !.
goal_status__and(_, fail, fail) :- !.
goal_status__and(true, X, X) :- !.
goal_status__and(X, true, X) :- !.
goal_status__and(X, X, X) :- !.

goal_status__or(fail, X, X) :- !.
goal_status__or(X, fail, X) :- !.
goal_status__or(true, _, true) :- !.
goal_status__or(_, true, true) :- !.
goal_status__or(X, X, X) :- !.

% ---------------------------------------------------------------------------

{
:- fluid s :: codegen_s + u.
% TODO: case key is ignored!
prepare_cases([], []).
prepare_cases([case(_, X0)|Xs], [BBId|BBlocks]) :-
	BBId = ~bblock.new(contcode_conj(X0)),
	prepare_cases(Xs, BBlocks).
}.

% ---------------------------------------------------------------------------

% unfold, expand and remove conditions, generate a list of alternatives
{
:- extends absmach_ctx.
:- fluid tbl :: bb_tbl + u.
:- fluid s :: codegen_s + u.
:- fluid cases :: accum.
expand_cases((Cond0 -> Case)) :- !,
	expand_cond(Cond0, Cond),
	expand_cases__2(Cond, Case).
expand_cases((A ; B)) :- !,
        expand_cases(A),
        expand_cases(B).
expand_cases(Case) :- !,
	cases.add(Case).

:- '$ctxprj'(expand_cases__2/2, [cases]).
expand_cases__2((A;B), Case) :- !,
	expand_cases__2(A, Case),
	expand_cases__2(B, Case).
expand_cases__2(Cond, Case) :- !,
	cases.add((Cond -> Case)).
}.

% obtain the list of cases and the indexed expression (a variable or function) from a list of alternatives
% note: a 'default_case' is always inserted to ensure that all possible values of the indexed expression are covered: prune_cases will remove unnecessary cases later
{
:- extends absmach_ctx.
:- fluid tbl :: bb_tbl + u.
:- fluid s :: codegen_s + u.
:- fluid indexed_cases :: accum.
get_cases([], _IndexExpr) :- !.
get_cases([(Cond -> Case)|Cases], IndexExpr) :-
	get_index_expr(Cond, CondIndexExpr, Key),
	( var(IndexExpr) -> % no index expr yet
	    IndexExpr = CondIndexExpr
	; same_index_expr(CondIndexExpr, IndexExpr)
	),
	!,
	indexed_cases.add(case(Key, Case)),
	get_cases(Cases, IndexExpr).
get_cases(Rest, _IndexExpr) :-
	% move the rest of alternatives as a disjunction in the other case
	list_to_disj(Rest, Other),
	indexed_cases.add(default_case(Other)).
}.

% check that we are indexing on the same expression
same_index_expr(A, B) :-
	A = index_on_member(MemberA, MemberTypeA, VarA),
	B = index_on_member(MemberB, MemberTypeB, VarB),
	!,
	MemberA == MemberB,
	MemberTypeA == MemberTypeB,
	VarA == VarB.
same_index_expr(A, B) :- A == B, !.

% TODO: kludge implementation...
{
:- extends absmach_ctx.
:- fluid tbl :: bb_tbl + u.
:- fluid s :: codegen_s + u.
expand_cond(X, X) :- var(X), !.
expand_cond('$expandcond'(G), Code) :- !,
	expand_cond(G, G2),
	unfold_if_possible(G2, Code).
expand_cond(G, Code) :-
	unfold_type_check(G, Code).

unfold_type_check(G, Code) :-
	G =.. [Name,A0],
	trust(Name instance_of ltype),
	Name.may_have_typekey,
	Name.analyze,
	Field = ~Name.typekey_field, % emit check for a dependent type
	!,
	BaseTypeKey = ~Name.typekey_type,
	G2 =.. [BaseTypeKey, ~mcall(A0, Field)],
%	errlog:trace([tkm(Name,Member,G2)]),
	Code = G2.
unfold_type_check(X, X).

unfold_if_possible(X, X) :- var(X), !.
unfold_if_possible(G, Code) :-
	functor(G, GN, GA),
	ipexp.get__op__prop(GN, GA, unfold),
	!,
	G =.. [_|Xs],
	Code = ~getdef('$pr'(GN, GA), Xs).
% TODO: This was a call inside... true?
%unfold_if_possible(X, X).
}.

% TODO: generalize (make it work with other structure members different than 'tag')
{
:- extends absmach_ctx.
:- fluid tbl :: bb_tbl + u.
:- fluid s :: codegen_s + u.
get_index_expr(G, CondExpr, Key) :-
	G =.. [Name,A0],
	trust(Name instance_of ltype),
	Name.coercible_from_atom,
	A0 = ~mcall(U, tag),
	!,
	Name.analyze,
	Atoms = ~Name.bitmask_atoms,
	list_to_disj(Atoms, TagDisj),
	CondType00 = def_enum('tagged_$key', TagDisj),
	trust(CondType00 instance_of ltype),
	CondExpr = index_on_member(tag, 'tagged_$key', U),
	CondVar = U,
	%
	gn_term(CondVar, _, CondVarType), % TODO: do not throw output
	Key = ~CondType00.glb(~CondVarType.field_type(tag)).
get_index_expr((A0 = T), CondExpr, Key) :-
	A0 = ~mcall(U, tag),
	atom(T),
	!,
	CondType00 = def_enum('tagged_$key', T),
	trust(CondType00 instance_of ltype),
	CondExpr = index_on_member(tag, 'tagged_$key', U),
	CondVar = U,
	%
	gn_term(CondVar, _, CondVarType), % TODO: do not throw output
	Key = ~CondType00.glb(~CondVarType.field_type(tag)).
get_index_expr((A0 = T), CondExpr, Key) :-
	A0 instance_of termvar, \+ fresh_var(A0),
	atom(T),
	!,
%	errlog:trace([gie(A0, T)]),
	gn_term(A0, _, CondVarType), % TODO: do not throw output
%	errlog:trace([cv(CondVarType)]),
	trust(CondVarType instance_of ltype),
	BaseType = ~CondVarType.basic_def,
%	errlog:trace([ep(BaseType)]),
	CondExpr = index_on_var(BaseType, A0),
	%
	CondType00 = ~BaseType.coerce_from_atom(t_atom(T)),
%	errlog:trace([tca(CondType00)]),
	Key = ~CondType00.glb(CondVarType).
%	errlog:trace([tk(Key)]).
}.

% ---------------------------------------------------------------------------

{
:- extends absmach_ctx.
:- fluid tbl :: bb_tbl + u.
:- fluid s :: codegen_s + u.
gn_terms([], [], []).
gn_terms([X|Xs], [Y|Ys], [T|Ts]) :-
	gn_term(X, Y, T),
	gn_terms(Xs, Ys, Ts).

:- meta_predicate gn_term(?, ?, out(ltype)).
gn_term(X, _R, _Type) :- var(X), !,
	ptoc__impcomp:error(['unbound variable ', X, ' not supported by gn_term']).
gn_term(X, R, Type) :- X instance_of termvar, !,
	gn_var(X, R, Type).
gn_term(^'~'(X), R, Type) :- nonvar(X), !,
	cexpr :: any <- R,
	exprtype :: any <- Type,
	gn_eval(X).
gn_term(X, R, Type) :- X = ~mcall(_, _), !,
	cexpr :: any <- R,
	exprtype :: any <- Type,
	gn_eval(X).
gn_term(X0, R, Type) :- square_index(X0, X, A), !,
	% TODO: deprecate $element
	cexpr :: any <- R,
	exprtype :: any <- Type,
	gn_eval('$element'(X, A)).
gn_term(X, R, Type) :-
	functor(X, XN, XA),
	ipexp.use_fun_eval(XN,XA), !,
	call((
          cexpr :: any <- R,
	  exprtype :: any <- Type,
	  gn_eval(X)
        )).
% TODO: store as an number without an encoding type, add encoding type from code constraints
gn_term(X, R, Type) :- integer(X), !,
	R = X,
	Type = intmach.
% TODO: store as an number without an encoding type, add encoding type from code constraints
gn_term(X, R, Type) :- float(X), !,
	R = X,
	Type = flt64.
gn_term(X, R, Type) :- ( X = [] ; X = [_|_] ), is_string(X), !,
	R = temp_string(X),
	Type = t_string(X).
gn_term(X, R, Type) :- atom(X), !,
	R = temp_atom(X),
	Type = t_atom(X).
gn_term(X, _, _) :- !,
	ptoc__impcomp:error(['term ', X, ' not supported by gn_term']).
}.

% check that the term is ground and defines a string
is_string(X) :- var(X), !, fail.
is_string([]) :- !.
is_string([X|Xs]) :- !,
	nonvar(X),
	( X >= 0 ; X =< 255 ), % TODO: will not work with unicode
	is_string(Xs).

% Generate C expression for a variable
{
:- extends absmach_ctx.
:- fluid tbl :: bb_tbl + u.
:- fluid s :: codegen_s + u.
gn_var(X, R, Type) :-
	X instance_of termvar,
	Type0 = ~s.get_type(X),
	Mem = ~s.get_mem(X),
	( Mem = nomem ->
	    ptoc__impcomp:error(['variable ', X, ' does not have an assigned mem'])
	; Mem = mem_conflict ->
	    ptoc__impcomp:error(['variable ', X, ' is assigned different mems'])
	; Mem = evalmem(Expr) ->
	    gn_term(Expr, R, TypeExpr),
	    ( TypeExpr = mut(Ta),
	      Type0 = mut(Tb),
	      trust(Ta instance_of ltype),
	      trust(Tb instance_of ltype),
	      Type = mut(~Ta.glb(Tb)) -> % TODO: right or wrong??
	        true
	    ; Type = ~TypeExpr.glb(Type0) ->
	        true
	    ; errlog:bug(['failed type glb for evalmem: ', Expr, ' with type: ', TypeExpr]),
	      fail
	    )
	; Mem = predabscode(_,_,_,_) ->
	    R = temp_predabs(Mem),
	    Type = Type0
	; gn_var__1(Mem, R),
	  Type = Type0
	).

gn_var__1(cvar(I), R) :- !,
	R = I.
gn_var__1(Mem, _) :-
	ptoc__impcomp:error(['unknown mem ', Mem]).

gn_pa_array_elems([], []).
gn_pa_array_elems([X|Xs], [Y|Ys]) :-
        % note: (only valid with GCC goto* extension)
	a_decomp_goal(X, GId, Args),
	% TODO: add pushbacks just like in $switch...
	gn_subcall(sub2, GId, Args, BBId),
	Y = label_addressbb(BBId),
	gn_pa_array_elems(Xs, Ys).

% mems and offsets in the bytecode stream
gn_pmem__next(AR) :- !,
	calc_e(Off0, Dyn),
	( Dyn = d(DynXT, DynXOff) -> % TODO: incomplete, only accepts one L
	    gn_pmem__op(DynXT, DynXOff, DynX2),
	    ipexp.ftype__size(f_f, FSize),
	    Off is (Off0 + FSize),
	    N = Off+call('BlobFunctorSizeAligned', [DynX2])
	; N = Off0
	),
	gn_pmem__off(N, AR).

% obtain operands size (using the instruction format stored in S)
% TODO: incomplete, does not support ftypes with def array or str, only supports a single blob as dynamic size operand
calc_e(Off, Dyn) :-
	Ops = ~s.ops,
	call((
          off :: m_int <- 0,
	  i :: m_int <- 0,
	  dyn :: accum(Dyn0),
	  size_ops(Ops),
	  ~off = Off0
        )),
	ipexp.ftype__size(f_o, OSize),
	Off is (Off0 + OSize),
	( Dyn0 = [DynI] ->
	    calc_a(DynI, DynXT, DynXOff),
	    Dyn = d(DynXT, DynXOff)
	; Dyn = nd
	).

% obtain operand number I in Lc
calc_a(I, T, Off) :-
	Ops = ~s.ops,
	op_off_ops(I, Ops, Off0, OpF),
	( ftype__head(OpF, OpFH) ->
	    ipexp.ftype__enctype(OpFH, T)
	; ipexp.ftype__enctype(OpF, T)
	),
	ipexp.ftype__size(f_o, OSize),
	Off is (Off0 + OSize).
}.

% offset and operand type of the operand I
{
:- extends absmach_ctx.
:- fluid tbl :: bb_tbl + u.
op_off_ops(0, [OpF|_], 0, OpF) :- !.
op_off_ops(I, [OpF|OpFs], Off, OpF2) :-
	ipexp.ftype__size(OpF, IOpF),
	I2 is I - 1,
	op_off_ops(I2, OpFs, Off0, OpF2),
	Off is Off0 + IOpF.
}.

% size of all the operands 
{
:- extends absmach_ctx.
:- fluid tbl :: bb_tbl + u.
:- fluid i :: m_int + u.
:- fluid dyn :: accum.
:- fluid off :: m_int.
size_ops([]) :- !.
size_ops([OpF|OpFs]) :-
	( ipexp.ftype__def(OpF, blob) ->
	    % TODO: what happens with str and array?
	    dyn.add(~i)
	; ipexp.ftype__size(OpF, IOpF),
	  off.inc(IOpF)
	),
	i.inc(1),
	size_ops(OpFs).
}.

gn_pmem__off(N, call('Poff', [N])) :- !.

{
:- extends absmach_ctx.
:- fluid tbl :: bb_tbl + u.
gn_pmem__op(Type, Off, call('POp', [type_exp(CType), Off])) :- !,
	trust(Type instance_of ltype),
	CType = ~Type.ctype.
}.

% ---------------------------------------------------------------------------

{
:- extends absmach_ctx.
ctypes_from_types([], []).
ctypes_from_types([T|Ts], [CT|CTs]) :-
	trust(T instance_of ltype),
	CT = ~T.ctype,
	ctypes_from_types(Ts, CTs).
}.

{
:- extends absmach_ctx.
:- fluid tbl :: bb_tbl + u.
:- fluid s :: codegen_s.
:- fluid cstat :: bb_code.
% Move the program counter to discard an argument
gn_shift_ops(0) :- !,
	gn_cont(true).
gn_shift_ops(I) :-
	shift_ops(I, Size),
	gn_pmem__off(Size, SR),
	R0 = ('P' = SR),
	gn_emit(R0).
}.

{
:- extends absmach_ctx.
:- fluid tbl :: bb_tbl + u.
:- fluid s :: codegen_s.
% Shift N operands and return in Size the sum of size of the shifted operands
shift_ops(N, Size) :-
	size :: m_int <- 0,
	shift_ops_(N),
	~size = Size.
{
:- fluid size :: m_int.
shift_ops_(0) :- !.
shift_ops_(I) :-
	s.ops.get_head(OpF),  
	s.ops.shift,
	ipexp.ftype__size(OpF, SizeF),
	size.inc(SizeF),
	I1 is I - 1,
	shift_ops_(I1).
}.
}.

% trust shift over a list operand
{
:- fluid s :: codegen_s.
trust_shift_array :-
	s.ops.shift_array.
}.

% A stream of ftype (for the list of opcode ftypes)
:- class ftype_stream {
    :- '$statemodel'(pair).
    :- '$raw_state'.

    % TODO: missing instance_of__/1

    :- constant get_head/1.
    get_head := Y :-
            ~self = [X|_],
            Y = ( ftype__head(X, Y0) ? Y0 | X ).

    % Shift one operand. If the opcode is an array, loop in the type of
    % the array elements.
    shift :-
            ~self = [X|Xs],
            ( ftype__tail(X, Y) ->
                self <- [Y|Xs]
            ; self <- Xs
            ).

    % Shift all elements in an array operand.
    shift_array :-
            ~self = [X|Xs], ftype__is_arrayrest(X), !,
            self <- Xs.
}.

% Generate 'for-each' loop
% TODO: use analysis info to infer maybe_empty and transform the code accordingly
%   if the first condition is true, then the condition subpred is only called once and so it can be expanded after the end of the code (that transforms a while into a do_while)
% TODO: state after loops is not updated
%   loop :- init, cond.
%   cond :- ( c -> ok ; true ).
%   ok :- code, cond.
% TODO: unify unfolding, subpr and subcall (those are equivalent)
% TODO: in unfolding, code can be duplicated but there are not jumps
%       in subcalls there may be jumps inside the same function
%       in subcalls there may be loops
%       if we combine unfolding with subcalls, we can get loop unrolling
{
:- extends absmach_ctx.
:- fluid tbl :: bb_tbl + u.
:- fluid s :: codegen_s + u.
:- fluid cstat :: bb_code.
gn_for_each(I, Iter0, Goal) :-
	Iter0 = ~funcall(Iter),
	( ipexp.get__op__iterdef(Iter, I, Init, Cond, Next, MaybeEmpty) ->
	    true
	; ptoc__impcomp:error(['definition for iterator not found: ', Iter])
	),
	gn_loop(MaybeEmpty, Init, Cond, Goal, Next).

% Generate 'while' loop
gn_while(Cond, Goal) :-
	gn_loop(yes, true, Cond, Goal, true).

% Generate 'do-while' loop
gn_do_while(Goal, Cond) :-
	gn_loop(no, true, Cond, Goal, true).

gn_loop(MaybeEmpty, Init, Cond, Goal, Next) :-
	CondR = (Cond -> '$subcall'(LoopDef) ; true),
	LoopR = (Goal, Next, '$subcall'(CondDef)),
	PD = (CondDef = ~funcall('$predabs'([], all, CondR)), LoopDef = ~funcall('$predabs'([], all, LoopR))),
	% TODO: obtain automatically the case for 'MaybeEmpty = no' from the previous scheme: the first cond is true and inline after next the second cond: use builtins for iterators and properties for iterators! do not expand them during this analysis
	AR1 = ( MaybeEmpty = yes ? (PD, Init, '$subctx'('$subcall'(CondDef)))
	      | MaybeEmpty = no ? (PD, Init, '$subctx'('$subcall'(LoopDef)))
	      ),
        gn_subctx(subs, AR1).
}.

% ---------------------------------------------------------------------------
% Type operations

{
:- extends absmach_ctx.
:- public type_lub/3. % TODO: do not export! for absmach
type_lub(A, B) := C :-
	trust(A instance_of ltype),
	trust(B instance_of ltype),
	C = ~A.lub(B).

% transform a pair of types (TypeA, TypeB) to types (TypeA2, TypeB2), accordingly to automatic unboxing, so that they can be compared
:- meta_predicate type_coerce_pair(?, ?, out(ltype), out(ltype)).
type_coerce_pair(TypeA, TypeB, TypeA2, TypeB2) :-
	trust(TypeA instance_of ltype),
	trust(TypeB instance_of ltype),
	TypeA = t_atom(_), TypeB.coercible_from_atom, !,
	TypeA2 = ~TypeB.coerce_from_atom(TypeA),
	TypeB2 = TypeB.
type_coerce_pair(TypeA, TypeB, TypeA2, TypeB2) :-
	trust(TypeA instance_of ltype),
	trust(TypeB instance_of ltype),
	TypeB = t_atom(_), TypeA.coercible_from_atom, !,
	TypeB2 = ~TypeA.coerce_from_atom(TypeB),
	TypeA2 = TypeA.
type_coerce_pair(TypeA, TypeB, TypeA, TypeB).

:- public type_eval/2. % TODO: do not export
% TODO: use function and quoted notation for types! (e.g. ^atom)
type_eval(T0) := T :-
	tp :: ltype <- T0,
	T = ~tp.eval.

type_tagged__from_tag(BaseTagType, Tag, Type) :-
	trust(BaseTagType instance_of ltype),
	TagType = ~BaseTagType.coerce_from_atom(t_atom(Tag)),
	type_tagged__from_type_tag(TagType, Type).

:- '$ctxprj'(type_tagged__from_type_tag/2, []).
type_tagged__from_type_tag(TagType, t_dep(tagged, TagType)).

% check that T is a predefined or user defined type
% TODO: This is strange, mixes parsing/processing (T is already a class(ltype))
is_type(T) :-
	trust(T instance_of ltype),
	T1 = ~T.basic_def,
	( T1 = mut(_) -> true
	; T1 = ref1(_) -> true
	; T1 = ref0(_) -> true
	; native_number_type_cdef(T1, _) -> true
	; native_other_type_cdef(T1, _) -> true
	; _ = ~T1.kind -> true
	; fail
	).

:- '$ctxprj'(is_native_type/1, []).
is_native_type(T) :-
	( native_number_type_cdef(T, _) -> true
	; native_other_type_cdef(T, _) -> true
	).

% TODO: incomplete
% definition (in C) of predefined native types
:- '$ctxprj'(native_number_type_cdef/2, []).
native_number_type_cdef(int8, 'int8_t').
native_number_type_cdef(uint8, 'uint8_t').
native_number_type_cdef(int16, 'int16_t').
native_number_type_cdef(uint16, 'uint16_t').
native_number_type_cdef(int32, 'int32_t').
native_number_type_cdef(uint32, 'uint32_t').
native_number_type_cdef(int64, 'int64_t').
native_number_type_cdef(uint64, 'uint64_t').
native_number_type_cdef(flt64, 'flt64_t').
native_number_type_cdef(bitfield(N), CDef) :-
	CDef = bitfield(N, unsigned+int).

% TODO: address for a predicate abstraction inlined inside a function (i.e. a local label) FIX: strange... (at least the const part)
:- '$ctxprj'(native_other_type_cdef/2, []).
native_other_type_cdef(p0emu, CDef) :- % TODO: a kludge! define a type (with properties)
	CDef = pointer(void).
native_other_type_cdef(ho_nondet(_), CDef) :- % TODO: a kludge! define a type (with properties)
	CDef = pointer(void).
native_other_type_cdef(subpa, CDef) :-
	CDef = const+pointer(const+void).

gn_unboxed(BaseType, Atom, Encoded) :-
	Encoded = ~ipexp.get__enum_domain__encode_atom(BaseType, Atom).
}.

% ---------------------------------------------------------------------------

% ensure that the type is analyzed (if not execute a type definition, asserting type info)
{
:- extends absmach_ctx.
:- public type_analyze/1. % TODO: do not export? only for export_bitstr
type_analyze(BaseType) :-
	tp :: ltype <- BaseType,
	tp.analyze.
}.

% TODO: The state of the ltype objects is stored in 'absmach'. It is
%       too complex. Do it in other way.

:- class ltype {
    :- '$statemodel'(single).
    :- '$raw_state'.

    % TODO: missing instance_of__/1

    {
    :- extends absmach_ctx.
    eval := Type :-
        Def = ~self,
        ( Def = def_enum(BaseType, Atoms) ->
	    trust(BaseType instance_of ltype),
            ( TagType = ~BaseType.bitmask_from_atoms(Atoms) ->
                Type = TagType
            ; ptoc__impcomp:error(['unknown atoms ', Atoms, ' for enum ', BaseType])
            )
        ; atom(Def), coercible_from_atom_noana ->
            Type = ~subtop
        ; atom(Def), may_have_typekey,
          BaseType = Def,
          trust(BaseType instance_of ltype),
          BaseType.analyze,
          ipexp.ltype__typekey_type(BaseType, BaseTypeKey) ->
            BaseType2 = ~BaseType.encoding_type,
            ( ipexp.ltype__typekey_type(BaseType2, BaseTypeKey2) ->
                true
            ; ptoc__impcomp:error(['parent does not have a typekey ', BaseType2])
            ),
            trust(BaseTypeKey instance_of ltype),
            BaseTypeKey.analyze,
            TagType = ~BaseTypeKey.bitmask_convert(BaseTypeKey2),
            Type = t_dep(BaseType2, TagType)
        ; Def = def_dep(BaseType, TagType0) ->
	    trust(TagType0 instance_of ltype),
            ( TagType = ~TagType0.norm_eval ->
                true
            ; ptoc__impcomp:error(['unknown tag type def ', Atoms])
            ),
            Type = t_dep(BaseType, TagType)
        ; Type = Def
        ).

    :- meta_predicate norm_eval(out(ltype)).
    norm_eval := T :-
        T0 = ~norm_def,
        T = ~T0.eval.
    }.

    {
    :- extends absmach_ctx.
    analyze :-
	BaseType = ~self,
        ( BaseType = mut(BaseType0) -> % sure?
	    trust(BaseType0 instance_of ltype),
            BaseType0.analyze
        ; BaseType = ref1(BaseType0) -> % sure?
	    trust(BaseType0 instance_of ltype),
            BaseType0.analyze
        ; BaseType = ref0(BaseType0) -> % sure?
	    trust(BaseType0 instance_of ltype),
            BaseType0.analyze
        ; is_native_type(BaseType) ->
            true % nothing, it is a native type
        ; ipexp.ltype__analyzed(~self) ->
            true % nothing, already analyzed
        ; Kind = ~kind,
          Kind = foreign(_) ->
            true % nothing, it is foreign
        ; 
          ( call((
              s :: codegen_s <- ~codegen_s.new(det), % TODO: det? sure? even for unions?
              analyze__usertype
            )) ->
              true
          ; ptoc__impcomp:error(['could not analyze type definition for \'', BaseType, '\''])
          )
        ).
    }.

    {
    :- extends absmach_ctx.
    :- fluid s :: codegen_s + u.
    analyze__usertype :-
        ( Kind = ~kind,
          ( Kind = union -> true
          ; Kind = struct -> true
          ; Kind = bitstr -> true
          ; Kind = enum -> true
          ; Kind = predabs -> true
          ; Kind = equiv -> true
          ) ->
            true
        ; ptoc__impcomp:error(['type \'', ~self, '\' cannot be analyzed'])
        ),
        call((
	  this :: any <- This,
	  ( Kind = bitstr, Sentences = ~sentences -> analyze_bitstr('$sentences'(Sentences))
	  ; Kind = bitstr, Def = ~get_def2(This) -> analyze_bitstr(Def)
	  ; Kind = enum -> Def = ~get_def(This), analyze_enum(Def)
	  ; Kind = predabs -> Def = ~get_def(This), analyze_predabs(Def)
	  ; Kind = equiv -> Def = ~get_def(This), analyze_equiv(Def)
	  ; Kind = union, Sentences0 = ~sentences ->
              ipexp.filter_sentences(Sentences0, Sentences),
              analyze__union_sentences(Sentences)
	  ; Sentences0 = ~sentences,
            ipexp.filter_sentences(Sentences0, Sentences),
	    analyze__sentences(Sentences)
	  )
	)),
        ipexp.add(ltype__analyzed(~self)).
    }.

    {
    :- extends absmach_ctx.
    :- fluid s :: codegen_s + u.
    % TODO: get_def/2 should not be used... Def should not return ;/2
    get_def(This) := Def :-
        Def = ~getdef('$pr'(~self, 1), [This]).

    get_def2(This) := Def :-
        Def0 = ~get_def(This),
	Def = ( is_switch(Def0) ? '$or'(~or_to_cases(Def0))
	      | Def0
	      ).
    }.

    {
    :- extends absmach_ctx.
    kind := Kind :-
	ipexp.get__ltype__kind(~self, Kind).

    is_struct_or_union :-
	Kind = ~kind,
	( Kind = union -> true
	; Kind = struct -> true
	).

    % TODO: it should also check that it is a bitstr
    % ignore storage decl for descendant definitions
    ignore_storage :-
	ipexp.get__ltype__parent(~self, _), !.
    % ignore storage decl for misc definitions
    ignore_storage :-
	ipexp.get__ltype__disj(~self), !.

    sentences := Sentences :- ipexp.get__ltype__sentences(~self, Sentences).

    % normalize type definition
    % (expand 'equiv' types)
    :- meta_predicate norm_def(out(ltype)).
    norm_def := NT :-
        basic_def__2(~self, normdef, NT).

    % normalize type definition using top types for t_dep and t_bitmask
    :- meta_predicate basic_def(out(ltype)).
    basic_def := NT :-
        basic_def__2(~self, basicdef, NT).

    % traverse typedef links in a type definition up to a basic definition
    % TODO: it may halt
    :- static basic_def__2/3.
    basic_def__2(T, Mode, NT) :-
        trust(T instance_of ltype),
        Kind = ~T.kind, Kind = equiv, !,
        T.analyze,
        ( ipexp.ltype__equiv(T, DefT) -> true ),
        basic_def__2(DefT, Mode, NT).
    basic_def__2(ref1(T0), Mode, T) :- !,
        T = ref1(T1),
        basic_def__2(T0, Mode, T1).
    basic_def__2(ref0(T0), Mode, T) :- !,
        T = ref0(T1),
        basic_def__2(T0, Mode, T1).
    basic_def__2(array(T0), Mode, T) :- !,
        T = array(T1),
        basic_def__2(T0, Mode, T1).
    basic_def__2(array(T0,E), Mode, T) :- !,
        T = array(T1,E),
        basic_def__2(T0, Mode, T1).
    basic_def__2(T0, Mode, T) :-
        Mode = basicdef,
	trust(T0 instance_of ltype),
        T1 = ~T0.eval,
        ( T1 = t_dep(Base, _) -> true
        ; T1 = t_bitmask(Base, _) -> true
        ; fail
        ),
        !,
        T = Base.
    basic_def__2(T, _Mode, T).
    }.

    % TODO: generalize to structures with bitfield members
    % TODO: detect infinite recursive calls!
    {
    :- extends absmach_ctx.
    :- fluid s :: codegen_s + u.
    :- fluid this :: any.
    analyze_bitstr(Def) :-
            ( call((
                ts :: bitlayout_allocator <- ~bitlayout_allocator.new,
		analyze_bitstr__goal(Def),
		~ts = TS
              )),
              \+ TS = tsnone,
              analyze_bitstr__add_typekey ->
                true
            ; ptoc__impcomp:error(['could not analyze bitstr ', ~self])
            ).

    {
    :- fluid ts :: bitlayout_allocator.

    analyze_bitstr__goal('$sentences'(Sentences0)) :- !,
            ipexp.filter_sentences(Sentences0, Sentences),
            analyze_bitstr__sentences(Sentences).
    analyze_bitstr__goal('$or'(Cases)) :- !,
            analyze_bitstr__cases(Cases).
    analyze_bitstr__goal(Decl) :-
            ptoc__impcomp:error(['unknown bitstr decl ', Decl]).

    analyze_bitstr__sentences([]).
    analyze_bitstr__sentences([S|Ss]) :-
            analyze_bitstr__sentence(S),
            analyze_bitstr__sentences(Ss).

    analyze_bitstr__sentence(Sentence) :-
            Sentence = sentence(Decl, _, _, _, _),
            ( Decl = (:- key(Member, Val)) ->
                ( Member0 = ~typekey_field,
                  \+ Member0 = Member ->
                    ptoc__impcomp:error(['inconsistent typekey member ', Member, ' in type ', ~self, ' (was ', Member0, ')'])
                ; ipexp.add(ltype__typekey_field(~self, Member))
                ),
                ( ipexp.ltype__typekey_value(~self, Val) ->
                    true
                ; ipexp.add(ltype__typekey_value(~self, Val))
                )
            ; Decl = (:- mut(Member :: Val)) ->
                analyze_bitstr__sentence(sentence((:- attr(Member :: mut(Val))), [], [], 0, 0))
            ; Decl = (:- attr(Member :: Val)) ->
                % TODO: use process_type_expr
                ( Val = '$prop'(bitpack(Storage)) ->
                    ( ignore_storage -> true
                    ; ( ipexp.ltype__field(~self, Member, MemberType0, MemLinks) -> true ),
		      trust(MemberType0 instance_of ltype),
		      MemberType = ~MemberType0.applymemlinks(MemLinks),
                      ( Storage = upper ->
                          Size = ~MemberType.packedbitsize,
                          add_bitstr_storage(Member, upper(Size))
                      ; Storage = lower ->
                          Size = ~MemberType.packedbitsize,
                          add_bitstr_storage(Member, lower(Size))
                      ; add_bitstr_storage(Member, Storage)
                      )
                    )
                ; Val = mut(MemberType) ->
                    MemLinks = mmem,
                    MemberType2 = mut(MemberType),
                    add_field(Member, MemberType2, MemLinks)
                ; Val = ref1(mut(MemberType)) ->
                    MemLinks = cmem,
                    MemberType2 = mut(MemberType),
                    add_field(Member, MemberType2, MemLinks)
                ; Val = ref1(MemberType) ->
                    MemLinks = cmem,
                    MemberType2 = ref1(MemberType),
                    add_field(Member, MemberType2, MemLinks)
                ; Val = ref0(mut(MemberType)) ->
                    MemLinks = mmem,
                    MemberType2 = mut(MemberType),
                    add_field(Member, MemberType2, MemLinks)
                ; Val = ref0(MemberType) ->
                    MemLinks = cmem,
                    MemberType2 = MemberType,
                    add_field(Member, MemberType2, MemLinks)
                ; ptoc__impcomp:error(['unrecognized value for type member ', Member, ': ', Val])
                )
            ; Decl = (:- encoding_type(_Type)) ->
                true
    %            % TODO: this is a kludge, search encoding type
    %            ( Type = ~self ->
    %                true
    %            ; errlog:trace(adding_encoding_type(~self, Type)),
    %              ipexp.add(ltype__encoding_type(~self, Type))
    %            )
            ; Decl = (:- base(Type)) ->
                % Fixing the encoding base of a class:
                % - a limited number of derived classes is allowed
                % - derived classes inherit the same encoding base
                ( ignore_storage -> true
                ; ( ipexp.ltype__encoding_base(~self, Type0) ->
                      ( Type == Type0 ->
                          true
                      ; ptoc__impcomp:error(['inconsistent bases for ltype ', ~self, ' : ', Type0, ' and ', Type0])
                      ),
                      ( ipexp.ltype__bitstr__size(~self, Size) -> true ),
                      % reinit storage bits % TODO: kludge?
                      ts.set(Size, 0)
                  ; ipexp.add(ltype__encoding_base(~self, Type)),
                    % calculate size and init storage bits
                    type_size(Type, Size0), % size in bytes
                    Size is Size0 * 8, % size in bits
                    ipexp.add(ltype__bitstr__size(~self, Size)), % TODO: remove!?
                    ts.set(Size, 0)
                  )
                )
            ; Decl = (:- Decl0), nested_decl(Decl0, Member, 'sub', SubSentences0) -> % TODO: kludge? prepare for subdefinitions
                ( ignore_storage -> true
                ; ( ipexp.ltype__bitstr__storage(~self, Member, bitmember(SizeVal, OffVal)) -> true ),
                  Upper is OffVal + SizeVal,
                  Lower is OffVal,
                  ipexp.filter_sentences(SubSentences0, SubSentences),
                  call((
                    ts :: bitlayout_allocator <- ~bitlayout_allocator.region(Upper, Lower),
                    analyze_bitstr__sentences(SubSentences)
                  ))
                )
            ; Decl = (:- smallptrmodel(Member, MemberBaseType)) ->
                ( ignore_storage -> true
                ; type_size(MemberBaseType, MemberBaseSize0), % size in bytes
                  MemberBaseSize is MemberBaseSize0 * 8, % size in bits
                  ( ipexp.ltype__bitstr__storage(~self, Member, bitmember(SizeVal, OffVal)) -> true ),
                  % Number of upper bits in the uintval not available to pointer and value encoding
                  UpperBits is MemberBaseSize - SizeVal - OffVal,
                  % Number of lower bits in the uintval not available to pointer and value encoding
                  LowerBits is OffVal,
                  call((
                    upperbits :: any <- UpperBits,
		    lowerbits :: any <- LowerBits,
		    find_smallptr_model(Model)
                  )),
                  ( ipexp.ltype__bitstr__smallptr_model(~self, Model0) ->
                      ( Model == Model0 ->
                          true
                      ; ptoc__impcomp:error(['inconsistent smallptr model for ltype ', ~self, ' : ', Model, ' and ', Model0])
                      )
                  ; ipexp.add(ltype__bitstr__smallptr_model(~self, Model))
                  )
                )
            ; Decl = (:- mixin_extends(Name)),
              trust(Name instance_of ltype),
              Sentences2 = ~Name.sentences -> % TODO: check that it does not include itself recursively
    %            errlog:trace(me(Name, Decls2)),
                analyze_bitstr__goal('$sentences'(Sentences2))
            ; Decl = (:- mixin_extends(Name)),
              trust(Name instance_of ltype),
              Def = ~Name.get_def2(~this) -> % TODO: check that it does not include itself recursively
    %            errlog:trace(me2(Name, Def)),
                analyze_bitstr__goal(Def)
            ; Decl = (:- extends(Name)),
              Name2 = ~atom_concat(Name, '__invariant'),
              % TODO: do a lookup to get the class
              trust(Name2 instance_of ltype),
              Sentences2 = ~Name2.sentences -> % TODO: check that it does not include itself recursively
                analyze_bitstr__goal('$sentences'(Sentences2))
            ; ptoc__impcomp:error(['unknown bitstr decl ', Decl])
            ).

    analyze_bitstr__cases(Decls) :-
            Decls = [A|Decls0],
            TS0 = ~ts,
            analyze_bitstr__sentence(sentence((:- mixin_extends(A)), [], [], 0, 0)),
            ( Decls0 = [] ->
                true
            ; TSa = ~ts,
              call((
		ts :: bitlayout_allocator <- TS0,
		analyze_bitstr__cases(Decls0),
		~ts = TSb
              )),
              ts <- ~bitlayout_allocator.merge(TSa, TSb)
            ).
    }.

    % TODO: calculate 'tag' automatically from the typekey of 'tagged'... use typekeys of other types to allow checks like strtagged(@U) instead of strtag((@U).tag)
    :- '$ctxprj'(analyze_bitstr__add_typekey/0, [ipexp, exp]).
    analyze_bitstr__add_typekey :-
            ( _Member = ~typekey_field ->
                % build enum type for the typekey
                atom_concat(~self, '_$key', BaseTypeKey),
                ipexp.add(ltype__typekey_type(~self, BaseTypeKey)),
                ipexp.add(op__prop(BaseTypeKey, 1, unfold)),
                ipexp.add(op__prop(BaseTypeKey, 1, propargs)), % TODO: hmmm it should work without it
                ipexp.add(ltype__kind(BaseTypeKey, enum)),
                BaseTypeKeyHead =.. [BaseTypeKey, T],
    %            errlog:trace([itk(~self, Member, Val)]),
                ( ipexp.ltype__typekey_value(~self, Val),
                  BaseTypeKeyBody = (T = Val),
    %              errlog:trace([ac(BaseTypeKeyHead, BaseTypeKeyBody)]),
                  ipexp.add_clause(BaseTypeKeyHead, BaseTypeKeyBody),
                  fail
                ; true
                )
            ; true
            ).

    analyze_equiv(Def) :-
            ( Def = (This0 = Val), ~this == This0, nonvar(Val), Val = ~funcall(Other) ->
                ipexp.add(ltype__equiv(~self, Other))
            ; ptoc__impcomp:error(['could not analyze equiv ', ~self, ' defined as ', Def])
            ).

    analyze_predabs(Def) :-
            ( Def = (This0 = Val), ~this == This0, nonvar(Val), Val = ~funcall(predabs(Imp, InTypes0, OutTypes0)) ->
                ipexp.add(ltype__predabs(~self, Imp, InTypes0, OutTypes0))
            ; ptoc__impcomp:error(['could not analyze predabs ', ~self, ' defined as ', Def])
            ).

    % TODO: may hang on loops... use real program analysis (e.g. ptoc__analyze)
    analyze_enum(Def) :-
            ( call(( enum_items :: accum(List), analyze_enum_goal(Def) )),
              ipexp.process_enum_domain(~self, List) ->
                true
            ; ptoc__impcomp:error(['could not analyze enum ', ~self, ' defined as ', Def])
            ).
    {
    :- fluid enum_items :: accum.
    analyze_enum_goal(Def) :- var(Def), !, fail.
    analyze_enum_goal((This = ~funcall('$base'(Type)),Rest)) :- !,
            ~this == This,
            ( ipexp.get__ltype__encoding_type(~self, _) ->
                true % ignore storage decl
            ; ( ipexp.ltype__encoding_base(~self, Type0) ->
                  ( Type == Type0 ->
                      true
                  ; ptoc__impcomp:error(['inconsistent bases for ltype ', ~self, ' : ', Type0, ' and ', Type0])
                  )
              ; ipexp.add(ltype__encoding_base(~self, Type))
              )
            ),
            analyze_enum_goal(Rest).
    analyze_enum_goal((A;B)) :- !,
            analyze_enum_goal(A),
            analyze_enum_goal(B).
    analyze_enum_goal((This0 = Def2)) :-
            ~this == This0,
            analyze_enum_expr(Def2).

    analyze_enum_expr(A) :- var(A), !, enum_items.add(A). % TODO: sure? check
    analyze_enum_expr(^'|'(A,B)) :- !,
            analyze_enum_expr(A),
            analyze_enum_expr(B).
    analyze_enum_expr(A) :- A = ~funcall(A2), !,
            ( A2 = '$typekey'(B), atom(B) -> % TODO: do not use typekey, but 'member type'?
                trust(B instance_of ltype),
                B.may_have_typekey,
                B.analyze,
                ( ipexp.ltype__typekey_type(B, TypeKey) -> true ),
                Def = (~this = ~funcall(TypeKey)),
                analyze_enum_goal(Def)
            ; trust(A2 instance_of ltype), % TODO: trust/1 not colored
              Def = ~A2.get_def(~this) ->
                analyze_enum_goal(Def)
            ; ptoc__impcomp:error(['unrecognized function in analyze_enum_expr ', A])
            ).
    analyze_enum_expr(A) :- enum_items.add(A).
    }.
    }.

    % TODO: T is ignored...
    {
    :- extends absmach_ctx.
    :- fluid this :: any.
    analyze__union_sentences([]).
    analyze__union_sentences([S0|Ss]) :-
            S0 = sentence(S, _, _, _, _),
            analyze__union_sentence(S),
            analyze__union_sentences(Ss).

    analyze__union_sentence(S) :-
            % TODO: right now, the same; but it should not be
            analyze__sentence(S).

    analyze__sentences([]).
    analyze__sentences([S0|Ss]) :-
            S0 = sentence(S, _, _, _, _),
            analyze__sentence(S),
            analyze__sentences(Ss).

    analyze__sentence((:- mut(Member :: B0))) :- !,
            analyze__sentence((:- attr(Member :: mut(B0)))).
    analyze__sentence((:- attr(Member :: B0))) :- !,
            ( B0 = '$macrocons'(B) ->
                exp_type_name(m(~self, Member), Name),
                ipexp.add(entry_lowmacrocons(Name, 1, Name)),
                ipexp.add(fun_body(Name, 1, [], B))
            ; B0 = '$macrofun'(Args, Types, Value) ->
                exp_type_name(m(~self, Member), Name),
                Head =.. [Name|Args],
                functor(Head, _, N1),
                N is N1 + 1,
                ipexp.add(entry_lowmacrofun(Name, N, Types, Name)),
                ipexp.add(fun_body(Name, N, Args, Value))
            ; B0 = '$macropred'(Args, Imp, Types, AMems, Body) ->
                exp_type_name(m(~self, Member), Name),
                CName = Name,
                Head =.. [Name|Args],
                functor(Head, _, N),
                ipexp.add(entry_lowmacro(Name, N, Imp, Types, CName, AMems)),
                ipexp.add_clause(Head, Body)
            ; B0 = '$gvar'(N, Type) ->
	        % TODO: force a C expression as mem for the variable
	        % TODO: redefine as a property
	        % TODO: Missing conversion from parsed form?
                process_type_expr(Type, Type2, MemLinks),
                N2 = ( MemLinks = mmem ? address(N) | N ),
                add_field(Member, Type2, stamem(cvar(N2)))
            ; B0 = '$prop'(Prop) ->
                ipexp.add(ltype__field_prop(~self, Member, Prop))
            ; process_type_expr(B0, MemberT, MemLinks),
              add_field(Member, MemberT, MemLinks)
            ).
    analyze__sentence(X) :-
            ptoc__impcomp:error(['cannot process type sentence ', X]).
    }.

    {
    :- extends absmach_ctx.
    :- fluid ts :: bitlayout_allocator.
    add_bitstr_storage(Member, Storage) :-
            ts.check_available,
            ( ts.alloc(Storage, StorageBits) ->
                add_unique_bitstr_storage(Member, StorageBits)
            ; ptoc__impcomp:error(['unrecognized storage property ', Storage])
            ).

    :- '$ctxprj'(add_unique_bitstr_storage/2, [exp, ipexp]).
    add_unique_bitstr_storage(Member, StorageBits) :-
            ( ipexp.ltype__bitstr__storage(~self, Member, StorageBits0) ->
                ( StorageBits == StorageBits0 ->
                    true
                ; ptoc__impcomp:error(['incompatible storage bits ', StorageBits, ' and ', StorageBits0, ' for ltype ', ~self, ' and member ', Member])
                )
            ; ipexp.add(ltype__bitstr__storage(~self, Member, StorageBits))
            ).
    }.

    {
    :- extends absmach_ctx.
    add_field(Member, MemberType, MemLinks) :-
            ( ipexp.ltype__field(~self, Member, MemberType0, MemLinks0) ->
                ( MemberType == MemberType, MemLinks0 == MemLinks ->
                    true
                ; ptoc__impcomp:error(['defining multiple incompatible definitions of member ', Member, ' of type ', ~self, ': defined as ', ~~(MemberType, MemLinks), ' (previously was ', ~~(MemberType0, MemLinks0), ')'])
                )
            ; ipexp.add(ltype__field(~self, Member, MemberType, MemLinks))
            ).
    }.

    {
    % For record-like types
    :- extends absmach_ctx.

    field(Member, Type, MemLinks) :-
        ( field_query(Member, Type0, MemLinks) ->
            Type = Type0
        ; % TODO: this is a compilation error
          ptoc__impcomp:error(['type ', ~self, ' does not have a member called ', Member])
        ).

    field_query(Member, Type, MemLinks) :-
        analyze,
        ( ipexp.ltype__field(~self, Member, Type0, MemLinks) ->
            Type = Type0
        ; fail
        ).

    field_prop(Member, Prop) :-
        analyze,
        ( ipexp.ltype__field_prop(~self, Member, Prop) ->
            true
        ; fail
        ).

    % TODO: this is incomplete, only works for tagged type
    :- meta_predicate field_type(?, out(ltype)).
    field_type(Member) := MemberType :-
	TypeA = ~norm_eval,
	( TypeA = t_dep(tagged, B), Member = tag ->
	    MemberType = B
	; fail
	).

    fields := Fields :-
        findall(Field, is_field(Field), Fields).

    % (nondet) 
    is_field(Field) :-
	ipexp.ltype__field(~self, Field, _, _).
    }.

    {
    :- extends absmach_ctx.
    :- meta_predicate encoding_type(out(ltype)).
    encoding_type := BaseType2 :-
        analyze,
        ( ipexp.get__ltype__encoding_type(~self, BaseType2) ->
            trust(BaseType2 instance_of ltype),
            BaseType2.analyze
        ; ipexp.get__ltype__parent(~self, BaseType1) ->
            trust(BaseType1 instance_of ltype),
            BaseType1.analyze,
            ( ipexp.ltype__encoding_base(BaseType1, _) ->
                BaseType2 = BaseType1
            ; BaseType2 = ~BaseType1.encoding_type
            )
        ; BaseType2 = ~self
        ).
    }.

    {
    :- extends absmach_ctx.
    size := Size :-
        Type2 = ~basic_def,
        ( size__2(Type2, Size) ->
            true
        ; ptoc__impcomp:error(['size of type \'', ~self, '\' cannot be calculated'])
        ).

    :- static size__2/2.
    size__2(Type2, Size) :-
        trust(Type2 instance_of ltype),
        ( Type2 = mut(_) -> % TODO: wrong?
            % size of a pointer
            ipexp.get__pointer_size(Size)
        ; Type2 = ref1(mut(_)) ->
            % size of a pointer
            ipexp.get__pointer_size(Size)
        ; Type2 = ref0(mut(X0)) ->
            trust(X0 instance_of ltype),
            X1 = ~X0.basic_def,
            size__2(X1, Size)
        ; Type2 = ref1(array(_)) ->
            % size of a pointer
            ipexp.get__pointer_size(Size)
        ; Type2 = ref1(_) ->
            % size of a pointer
            ipexp.get__pointer_size(Size)
        ; Type2 = ref0(array(_, AElems)) ->
            X1 = ~Type2.elem_type,
            ASize = ~X1.size,
            ( AElems = 'ANY' ->
                % TODO: this is incorrect...
                Size = 0
            ; integer(AElems) ->
                Size is ASize * AElems
            ; ptoc__impcomp:error(['size of array of ', AElems, ' elements is not known'])
            )
        ; Type2 = ref0(X0) -> % TODO: check
            trust(X0 instance_of ltype),
            X1 = ~X0.basic_def,
            size__2(X1, Size)
        ; Type2 = array(_, AElems) -> % TODO: incorrect
            X1 = ~Type2.elem_type,
            ASize = ~X1.size,
            ( AElems = 'ANY' ->
                % TODO: this is incorrect...
                Size = 0
            ; integer(AElems) ->
                Size is ASize * AElems
            ; ptoc__impcomp:error(['size of array of ', AElems, ' elements is not known'])
            )
        % TODO: base the following cases in user definitions!
        ; Type2 = liveinfo -> % TODO: this is a compound ftype!
            ipexp.ftype__size(f_l, Sl),
            ipexp.ftype__size(f_i, Si),
            Size is Sl + Si
        ; Type2 = char -> % TODO: try to remove (some shootout benchmarks require it)
            native_size(uint8, Size)
        ; Type2 = cbool -> % TODO: remove??
            ipexp.get__pointer_size(Size)
        ; Type2 = cinsnp -> % TODO: remove??
            ipexp.get__pointer_size(Size)
        ; native_size(Type2, NSize) ->
            Size = NSize
        ; Kind = ~Type2.kind ->
            ( Kind = struct ->
                Size = ~Type2.struct_size
            ; ( Kind = bitstr ; Kind = enum ) ->
                Type2.analyze,
                ( ipexp.ltype__encoding_base(Type2, BaseType) ->
                    true
                ; ptoc__impcomp:error(['no encoding base for ', Type2])
                ),
                size__2(BaseType, Size)
            ; Kind = predabs ->
                % TODO: check
                ipexp.get__pointer_size(Size)
            ; ptoc__impcomp:error(['does not know how to calculate the size of type ', Type2, ', defined as ', Kind])
            )
        ).

    % TODO: is it a good idea?
    packedbitsize := Size :-
        Type2 = ~basic_def,
        ( Type2 = bitfield(N) ->
            Size = N
        ; size__2(Type2, Size0) ->
            Size is Size0 * 8
        ; ptoc__impcomp:error(['packed bit size of type \'', ~self, '\' cannot be calculated'])
        ).

    % TODO: WRONG! check that we are not in definition loops
    struct_size := Size :-
        ( ipexp.ltype__struct_size(~self, Size0) ->
            Size = Size0
        ; analyze,
          Size = ~sum_sizes,
          ipexp.add(ltype__struct_size(~self, Size))
        ).

    sum_sizes := Size :-
        offset :: m_int <- 0,
	sum_sizes__2(~fields),
	~offset = Size.
    {
    :- fluid offset :: m_int.
    sum_sizes__2([]).
    sum_sizes__2([Field|Ms]) :-
        ipexp.ltype__field(~self, Field, MemberType0, MemLinks),
	trust(MemberType0 instance_of ltype),
        MemberType1 = ~MemberType0.applymemlinks(MemLinks),
        SizeM = ~MemberType1.size,
	check_align(Field, SizeM),
        offset.inc(SizeM),
        sum_sizes__2(Ms).

    :- '$ctxprj'(check_align/2, [u(offset)]).
    % TODO: improve! consider paddings introduced by the C compiler? avoid type_size?
    % Check if the member has to be aligned
    % TODO: align to 8 only when pointer size is 8? (not valid in Sparc, valid in Intel?)
    check_align(Field, SizeM) :-
        ( ( SizeM = 2
          ; SizeM = 4
          ; % ipexp.get__pointer_size(PointerSize)
            %PointerSize = 8,
            SizeM = 8
          ) ->
             ( ModOff is ~offset mod SizeM,
               ModOff = 0 ->
                 true
             ; ptoc__impcomp:error(['unaligned field ', Field, ' in structure ', ~self])
             )
        ; true
        ).
    }.
    }.

    {
    :- extends absmach_ctx.
    glb(B0) := C :-
        A1 = ~norm_eval,
	trust(B0 instance_of ltype),
        B1 = ~B0.norm_eval,
        ( A1 = B1 ->
            C = A1
        ; A1 = t_dep(tagged, A), B1 = t_dep(tagged, B) ->
	    trust(A instance_of ltype),
	    trust(B instance_of ltype),
            C = t_dep(tagged, ~A.glb(B))
        ; A1 = t_bitmask(Base, AMask), B1 = t_bitmask(Base, BMask) ->
            CMask is AMask /\ BMask,
            C = t_bitmask(Base, CMask)
        ; A1 = any ->
            C = B1
        ; B1 = any ->
            C = A1
        ; ptoc__impcomp:error(['unknown glb of types ', ~self, ' and ', B0])
        ).

    lub(B0) := C :-
        A1 = ~norm_eval,
	trust(B0 instance_of ltype),
        B1 = ~B0.norm_eval,
        ( A1 = B1 ->
            C = A1
        ; A1 = any ->
            C = A1
        ; B1 = any ->
            C = B1
        ; A1 = t_dep(tagged, A), B1 = t_dep(tagged, B) ->
            trust(A instance_of ltype),
            C = t_dep(tagged, ~A.lub(B))
        ; A1 = t_bitmask(Base, AMask), B1 = t_bitmask(Base, BMask) ->
            CMask is AMask \/ BMask,
            C = t_bitmask(Base, CMask)
        ; A1 = mut(ValA), B1 = mut(ValB) ->
            trust(ValA instance_of ltype),
            % TODO: correct? note that the argument of the mut is a runtime type invariant
            C = mut(~ValA.lub(ValB))
        ; A1 = ref1(ValA), B1 = ref1(ValB) ->
            trust(ValA instance_of ltype),
            % TODO: correct? note that the argument of the mut is a runtime type invariant
            C = ref1(~ValA.lub(ValB))
        ; ptoc__impcomp:error(['unknown lub of types ', ~self, ' and ', B0])
        ).

    % TODO: check
    equal(B0) :-
	trust(B0 instance_of ltype),
	A = ~norm_eval,
	B = ~B0.norm_eval,
	( A = t_dep(tagged, A1), B = t_dep(tagged, B1) ->
	    trust(A1 instance_of ltype),
	    trust(B1 instance_of ltype),
	    A1.equal(B1)
	; A = B
	).

     % TODO: wrong!! incomplete
     % TypeA is inside TypeB
     consequence(TypeB0) :-
	trust(TypeB0 instance_of ltype),
	TypeA = ~norm_eval,
	TypeB = ~TypeB0.norm_eval,
	( TypeA = t_dep(tagged, A), TypeB = t_dep(tagged, B) ->
	    trust(A instance_of ltype),
	    trust(B instance_of ltype),
	    A.consequence(B)
	; TypeA = t_bitmask(Base, A), TypeB = t_bitmask(Base, B) ->
	    B \/ A =:= B % all bits in A are already in B
	; TypeA = mut(A), TypeB = mut(B) -> % TODO: sure?
	    trust(A instance_of ltype),
	    trust(B instance_of ltype),
	    A.consequence(B)
	; TypeA = ref0(A), TypeB = ref0(B) -> % TODO: sure?
	    trust(A instance_of ltype),
	    trust(B instance_of ltype),
	    A.consequence(B)
	; TypeA = ref0(A), TypeB = B -> % TODO: this removes ref0 (incorrect)
	    trust(A instance_of ltype),
	    trust(B instance_of ltype),
	    A.consequence(B)
	; TypeA = A, TypeB = ref0(B) -> % TODO: this removes ref0 (incorrect)
	    trust(A instance_of ltype),
	    trust(B instance_of ltype),
	    A.consequence(B)
	; TypeA = array(A, _), TypeB = array(B) -> % TODO: sure?
	    trust(A instance_of ltype),
	    trust(B instance_of ltype),
	    A.consequence(B)
	; TypeA = array(A, SizeA), TypeB = array(B, SizeB) -> % TODO: sure?
	    trust(A instance_of ltype),
	    trust(B instance_of ltype),
	    A.consequence(B),
	    SizeA = SizeB
	; TypeA = TypeB
	).

    % ~self and TypeB0 are comparable
    comparable(TypeB0) :-
	trust(TypeB0 instance_of ltype),
	TypeA = ~norm_eval,
	TypeB = ~TypeB0.norm_eval,
	( TypeA = t_dep(BaseA, _), TypeB = t_dep(BaseB, _), BaseA == BaseB ->
	    true
	; TypeA = t_bitmask(BaseA, _), TypeB = t_bitmask(BaseB, _), BaseA == BaseB ->
	    true
	; TypeA.equal(TypeB)
	).

    % TODO: indeed, obtain the most general type that the mutable can hold, and that ValueType is a consequence of MutType
    can_be_assigned(TypeB0) :-
	trust(TypeB0 instance_of ltype),
	TypeA = ~norm_eval,
	TypeB = ~TypeB0.norm_eval,
	( TypeA = t_dep(BaseA, _), TypeB = t_dep(BaseB, _), BaseA == BaseB ->
	    true
	; TypeA = t_bitmask(BaseA, _), TypeB = t_bitmask(BaseB, _), BaseA == BaseB ->
	    true
%	; TypeA.equal(TypeB)
	; TypeB.consequence(TypeA)
	).

    % TypeA and TypeB does not have any common element
    % note: this predicate will fail for types of different encoding!
    disjoint(TypeB0) :-
	trust(TypeB0 instance_of ltype),
	TypeA = ~norm_eval,
	TypeB = ~TypeB0.norm_eval,
	( TypeA = t_dep(tagged, A), TypeB = t_dep(tagged, B) ->
	    trust(A instance_of ltype),
	    trust(B instance_of ltype),
	    A.disjoint(B)
	; TypeA = t_bitmask(Base, A), TypeB = t_bitmask(Base, B) ->
	    A /\ B =:= 0 % none of the bits in A are in B
	; \+ TypeA = TypeB
	).

    % TODO: add support for more types, make it work even if it is not a basic_tag
    compare(TypeB0) := Exit :-
	trust(TypeB0 instance_of ltype),
	TypeA1 = ~norm_eval,
	TypeB1 = ~TypeB0.norm_eval,
	type_coerce_pair(TypeA1, TypeB1, TypeA, TypeB),
	% tagged
	Exit = ( TypeA = t_dep(tagged, _), TypeB = t_dep(tagged, _) ?
	           ~TypeA.tagged__compare(TypeB)
	       % others
	       | AAtom = ~TypeA.concrete_atom, BAtom = ~TypeB.concrete_atom ?
	           ( AAtom = BAtom ? true | false )
	       | TypeA.disjoint(TypeB) ? false
	       | unknown
	       ).
    }.

    {
    :- extends absmach_ctx.
    % If self is an array type, T is the type of its elements
    :- meta_predicate elem_type(out(ltype)).
    elem_type := T :-
	elem_type0(T1, MemLinks),
	T = ~T1.applymemlinks(MemLinks).

    % (like elem_type, but before applying applymemlinks)
    :- meta_predicate elem_type0(out(ltype), ?).
    elem_type0(T, MemLinks) :-
	X2 = ~elem_type_,
	X1 = ~X2.norm_def,
	( X1 = ref0(mut(X0)) -> MemLinks = mmem, T = mut(X0)
	; X1 = ref0(X0) -> MemLinks = cmem, T = ref0(X0)
	; X1 = ref1(mut(X0)) -> MemLinks = cmem, T = mut(X0)
	; X1 = ref1(X0) -> MemLinks = cmem, T = ref1(X0)
	; ptoc__impcomp:error(['type of array elements failed for ', ~self])
	).
    }.

    % TODO: use ref0 and ref1 before! (integrate into the member...)
    % Type of the elements in array types
    :- meta_predicate elem_type_(out(ltype)).
    elem_type_ := T :-
        X = ~self,
	T = ( X = array(T0) ? T0
	    | X = array(T0, _) ? T0
	    | X = ref0(array(T0)) ? T0
	    | X = ref0(array(T0, _)) ? T0
	    | X = ref1(array(T0)) ? T0
	    | X = ref1(array(T0, _)) ? T0
	    ).

    :- meta_predicate applymemlinks(?,out(ltype)).
    applymemlinks(MemLinks) := Type1 :-
        Type0 = ~self,
	( MemLinks = mmem ->
	    ( Type0 = mut(Type1) ->
	        true
	    ; ptoc__impcomp:error(['wrong memlinks ', MemLinks, ' for type ', Type0])
	    )
	; MemLinks = cmem ->
	    Type1 = Type0
	; ptoc__impcomp:error(['wrong memlinks ', MemLinks])
	).

    {
    :- extends absmach_ctx.
    % ~self represents the single atomic value Atom
    concrete_atom := Atom :-
        Type = ~self,
	Atom = ( Type = t_bitmask(Base, Mask),
	         trust(Base instance_of ltype),
		 Atom0 = ~Base.bitmask_concrete_atom(Mask) ? Atom0
	       | Type = t_atom(Atom0) ? Atom0
	       ).

    bitmask_elems := Elems :-
	ipexp.get__enum_domain__elems(~self, Elems).

    bitmask_concrete_atom(Mask) := Atom :-
	ipexp.get__enum_domain__mask_to_atom(~self, Mask, Atom).

    bitmask_from_atom(Atom) := Mask :-
	ipexp.get__enum_domain__atom_to_mask(~self, Atom, Mask).

    bitmask_top_mask := Mask :-
	Elems = ~bitmask_elems,
	Mask is (1<<Elems) - 1.

    % TODO: is this a good idea to have a bottom for each unboxed type?
    bitmask_bottom_mask := 0. 

    bitmask_atoms := Atoms :-
	ipexp.get__enum_domain__atoms(~self, Atoms).

    tagged__compare(TypeB) := Exit :-
	trust(TypeB instance_of ltype),
	TagTypeA = ~field_type(tag),
	TagTypeB = ~TypeB.field_type(tag),
	Exit0 = ~TagTypeA.compare(TagTypeB), % TODO: this was wrong, if the tags are the same, exit is unknown (since the value may be different)
	% TODO: use type_does_not_lose_info... if the comparison of two abstract types is true and it does not lose info, then concrete objects are the same
	Exit = ( Exit0 = false ? false | unknown ).

    % Return the top subtype of ~self
    subtop := Type :-
	analyze,
	( ~self = tagged ->
	    KeyType = 'tagged_$key', trust(KeyType instance_of ltype),
	    Type = t_dep(tagged, ~KeyType.subtop)
	; coercible_from_atom_noana ->
	    BaseType2 = ~encoding_type,
	    ( ~self == BaseType2 ->
	        Type = t_bitmask(BaseType2, ~BaseType2.bitmask_top_mask)
	    ; Type = ~bitmask_convert(BaseType2)
	    )
	).

    % Return the bottom subtype of ~self
    % TODO: is this a good idea to have a bottom for each unboxed type?
    subbottom := Type :-
	analyze,
	( ~self = tagged ->
	    KeyType = 'tagged_$key', trust(KeyType instance_of ltype),
	    Type = t_dep(tagged, ~KeyType.subbottom)
	; coercible_from_atom_noana ->
	    BaseType2 = ~encoding_type,
	    Type = t_bitmask(BaseType2, ~BaseType2.bitmask_bottom_mask)
	).

    % Type1 = Type0 - KeyType0
    :- meta_predicate remove(?, out(ltype)).
    remove(KeyType0) := Type1 :-
	trust(KeyType0 instance_of ltype),
	Type0e = ~norm_eval,
	KeyType0e = ~KeyType0.norm_eval,
	( Type0e = t_dep(tagged, TagType0), KeyType0e = t_dep(tagged, TagKeyType0) ->
	    trust(TagType0 instance_of ltype),
	    Type1 = t_dep(tagged, ~TagType0.remove(TagKeyType0))
	; Type0e = t_bitmask(Base, Mask0), KeyType0e = t_bitmask(Base, KeyMask0) ->
	    trust(Base instance_of ltype),
	    TopMask = ~Base.bitmask_top_mask,
	    Mask1 is Mask0 /\ (TopMask - KeyMask0),
	    Type1 = t_bitmask(Base, Mask1)
	; ptoc__impcomp:error(['type remove failed for ', ~self, ' and ', KeyType0])
	).

    coercible_from_atom :-
	BaseType = ~basic_def,
	is_type(BaseType),
	\+ is_native_type(BaseType), % not a native size
	BaseType.analyze,
	BaseType.coercible_from_atom_noana.

    coercible_from_atom_noana :- Kind = ~kind, Kind = enum.

    may_have_typekey :- Kind = ~kind, Kind = bitstr.

    :- meta_predicate coerce_from_atom(?, out(ltype)).
    coerce_from_atom(t_atom(Atom)) := Type :-
	BaseType = ~basic_def,
	BaseType.coercible_from_atom, !,
	BaseType2 = ~BaseType.encoding_type,
	Type = t_bitmask(BaseType2, ~BaseType2.bitmask_from_atom(Atom)).

    % TODO: optimize
    bitmask_convert(BaseTypeKey2) := TagType :-
	trust(BaseTypeKey2 instance_of ltype),
	ipexp.get__enum_domain__atoms(~self, Atoms2),
	list_to_disj(Atoms2, Atoms3),
	TagType = ~BaseTypeKey2.bitmask_from_atoms(Atoms3).

    bitmask_from_atoms(Def) := Type :-
	analyze,
	Base2 = ~encoding_type,
	Type = t_bitmask(Base2, ~bitmask_from_atoms__2(Def, Base2)).

    bitmask_from_atoms__2(X, _) := _ :- var(X), !, fail.
    bitmask_from_atoms__2(X, Base) := Mask :- atom(X), !,
	trust(Base instance_of ltype),
	Mask = ~Base.bitmask_from_atom(X).
    bitmask_from_atoms__2((X;Y), Base) := Mask :- !,
	XMask = ~bitmask_from_atoms__2(X, Base),
	YMask = ~bitmask_from_atoms__2(Y, Base),
	Mask is XMask \/ YMask.

    can_index_array :-
	( equal(intmach) -> true
	; equal(intval) -> true
	; equal(uintmach) -> true
	; equal(uintval) -> true
	; equal(ftype_o) -> true
	; equal(char) -> true
	; fail
	).

    typekey_field := Field :-
        ( ipexp.ltype__typekey_field(~self, Field) -> true ).

    typekey_type := BaseTypeKey :-
	( ipexp.ltype__typekey_type(~self, BaseTypeKey) -> true ).
    }.

    % -----------------------------------------------------------------------
    % Translation to C types
    {
    :- extends absmach_ctx.
    % translation to C type 
    ctype := CT :-
	Type2 = ~basic_def,
	( Type2 = mut(X0) ->
	    trust(X0 instance_of ltype),
	    CT = pointer(~X0.ctype)
	; Type2 = ref0(array(X0, Size)) ->
	    CT = array(CT0, Size),
	    ElemType2 = ~Type2.elem_type,
	    CT0 = ~ElemType2.ctype
	; Type2 = ref0(array(X0)) ->
	    CT = array(CT0),
	    ElemType2 = ~Type2.elem_type,
	    CT0 = ~ElemType2.ctype
	; Type2 = ref0(mut(X0)) ->
	    trust(X0 instance_of ltype),
	    CT = ~X0.ctype
	; Type2 = ref0(X0) ->
	    trust(X0 instance_of ltype),
	    CT = ~X0.ctype
	; Type2 = array(X0, Size) -> % TODO: incorrect
	    CT = array(CT0, Size),
	    ElemType2 = ~Type2.elem_type,
	    CT0 = ~ElemType2.ctype
	; Type2 = ref1(array(ref1(X0))) -> % TODO: wrong?
	    trust(X0 instance_of ltype),
	    CT = pointer(~X0.ctype)
	; Type2 = ref1(array(ref0(mut(X0)))) ->
	    trust(X0 instance_of ltype),
	    CT = pointer(~X0.ctype)
	; Type2 = ref1(array(ref0(X0))) ->
	    trust(X0 instance_of ltype),
	    CT = pointer(~X0.ctype)
	; Type2 = ref1(mut(X0)) ->
	    trust(X0 instance_of ltype),
	    CT = pointer(~X0.ctype)
	; Type2 = ref1(X0) ->
	    trust(X0 instance_of ltype),
	    CT = pointer(~X0.ctype)
	; native_number_type_cdef(Type2, CT0) ->
	    CT = CT0
	; native_other_type_cdef(Type2, CT0) ->
	    CT = CT0
	; Kind = ~Type2.kind ->
	    ( ( Kind = bitstr ; Kind = enum ) ->
	        Type2.analyze,
		( ipexp.ltype__encoding_base(Type2, BaseType) ->
		    true
		; ptoc__impcomp:error(['no encoding base for ', Type2])
		),
		trust(BaseType instance_of ltype),
		CT = ~BaseType.ctype
	    ; Kind = struct ->
	        atom_concat(Type2, '_t', CT)
	    ; Kind = union ->
	        atom_concat(Type2, '_t', CT)
	    ; Kind = enum ->
	        atom_concat(Type2, '_t', CT)
	    ; Kind = predabs ->
	        atom_concat(Type2, '_t', CT)
	    ; Kind = foreign(CT0) ->
	        CT = CT0
	    ; ptoc__impcomp:error(['unknown C type for \'', Type2, '\''])
	    )
	; ptoc__impcomp:error(['unknown C type for \'', Type2, '\''])
	).
    }.

    {
    :- extends absmach_ctx.
    :- fluid tbl :: bb_tbl + u.
    :- fluid lowtypedefs :: accum.
    :- fluid lowtypes :: accum.
    % Declare the C type (when applicable)
    %   - it may fill both the typedefs and type definitions sections
    ctype_decl :-
        atom(~self), !,
	atom_concat(~self, '_t', Name2),
	Kind = ~kind,
	( Kind = equiv ->
	    Def2 = ~ctype,
	    lowtypedefs.add(declare_type(Name2, Def2))
	; Kind = union ->
	    Decls = ~aggregate_field_decls,
	    MidName = ~self,
	    lowtypedefs.add(declare_type(Name2, union(MidName))),
	    lowtypes.add(declare_union(MidName, Decls))
	; Kind = struct ->
	    Decls = ~aggregate_field_decls,
	    MidName = ~self,
	    lowtypedefs.add(declare_type(Name2, struct(MidName))),
	    lowtypes.add(declare_struct(MidName, Decls))
	; Kind = bitstr ->
	    Def2 = ~ctype,
	    lowtypedefs.add(declare_type(Name2, Def2))
	; Kind = enum ->
	    Def2 = ~ctype,
	    lowtypedefs.add(declare_type(Name2, Def2))
	; Kind = predabs -> % TODO: define externally as typedefs, put properties
	    analyze,
	    ( ipexp.ltype__predabs(~self, Imp, InTypes0, OutTypes0) ->
	        true
	    ; ptoc__impcomp:error(['no predabs type def for ', Name])
	    ),
	    ctypes_from_types(InTypes0, InCTypes),
	    ctypes_from_types(OutTypes0, OutCTypes),
	    % TODO: get also foreign props from ltype.analyze  
	    lowtypedefs.add(declare_type(Name2, pointer(~detdeftype(with_worker, Imp, InCTypes, OutCTypes, []))))
	; ptoc__impcomp:error(['cannot generate typedef for type ', Name, ' defined as ', Kind])
        ).
    ctype_decl.
    }.

    {
    :- extends absmach_ctx.
    aggregate_field_decls := Decls :-
	analyze,
	Fields = ~fields,
	( Fields = [] ->
	    ptoc__impcomp:error(['type definition for ', ~self, ' is empty or incorrect'])
	; true
	),
	aggregate_field_decls__2(Fields, Decls).

    aggregate_field_decls__2([], []).
    aggregate_field_decls__2([Field|Fields], [Decl|Decls]) :-
	Decl = ~cdecl_field(Field),
	aggregate_field_decls__2(Fields, Decls).

    cdecl_field(Member) := Decl :-
	( ipexp.ltype__field(~self, Member, Type0, MemLinks) -> true ),
	trust(Type0 instance_of ltype),
	Type1 = ~Type0.applymemlinks(MemLinks),
	CType = ~Type1.ctype,
%	errlog:trace(['declaring ', c(BaseType, Member, Type0, MemLinks, CType)]),
	Decl = declare(Member, CType).
    }.
}.

% TODO: deprecate
or_to_cases(A) := Xs :-
	cases :: accum(Xs),
	or_to_cases_(A).
{
:- fluid cases :: accum.
or_to_cases_((A ; B)) :- !, or_to_cases_(A), or_to_cases_(B).
or_to_cases_(X) :- functor(X, A, 1), cases.add(A).
}.

% Bit layout allocator for bitstr types
:- class bitlayout_allocator {
    :- attr upper :: m_int.
    :- attr lower :: m_int.

    :- constructor f_/2.
    f_(A,B) :-
        ~upper = A,
	~lower = B.

    :- constructor new_/0.
    new_ :-
        ~upper = none,
	~lower = none.

    :- constructor region_/2.
    region_(Upper, Lower) :-
        ~upper = Upper,
	~lower = Lower.

    set(Upper, Lower) :-
	upper <- Upper,
	lower <- Lower.

    % merge the type status
    % TODO: take the max number of upper and lower bits as glb?
    :- constructor merge_/2.
    merge_(TSa, TSb) :-
            % Here merge is not the merging of each component (see other merge/2 constructors)
            trust(TSa instance_of bitlayout_allocator),
            trust(TSb instance_of bitlayout_allocator),
            Upper1a = ~TSa.upper,
            Lower1a = ~TSa.lower,
            Upper1b = ~TSb.upper,
            Lower1b = ~TSb.lower,
            ( Upper1a = Upper1b, Lower1a = Lower1b ->
	        ~upper = Upper1a,
		~lower = Lower1a
            ; ptoc__impcomp:error(['bitlayout_allocator.merge failed because upper and lower bits differ'])
            ).

    % TODO: finish!
    :- constant check_available/0.
    check_available :-
            ~self = ~bitlayout_allocator.f(none, none), !,
            ptoc__impcomp:error(['no more available bits in bitstr']).
    check_available.

    % Allocate a variable in a bitstr type, returning the assigned storage bits
    {
    :- extends absmach_ctx.
    alloc(Storage, StorageBits) :-
            ( Storage = upper(Size) -> % use Size upper bits
                OffVal is ~upper - Size,
                StorageBits = bitmember(Size, OffVal),
                upper.dec(Size)
            ; Storage = lower(Size) -> %  use Size lower bits
                OffVal = ~lower,
                StorageBits = bitmember(Size, OffVal),
                lower.inc(Size)
            ; Storage = split(Size1Expr,Size2Expr) -> % use Size1Expr upper bits and Size2Expr lower bits
                peval_cons(Size1Expr, Size1),
                peval_cons(Size2Expr, Size2),
                Off1Val is ~upper - Size1,
                Off2Val = ~lower,
                StorageBits = splitbitmember(Size1, Off1Val, Size2, Off2Val),
                upper.dec(Size1),
                lower.inc(Size2)
            ; Storage = rest -> % use the rest of available bits
                OffVal = ~lower,
                SizeVal is ~upper - ~lower,
                StorageBits = bitmember(SizeVal, OffVal),
                upper <- none,
                lower <- none
            ; ptoc__impcomp:error(['bad storage property ', Storage])
            ).
    }.
}.

{
:- extends absmach_ctx.
process_type_expr(X, T, MemLinks) :-
	( X = mut(T0) ->
	    process_type_expr(T0, T1, _MemLinks),
	    T = mut(T1),
	    MemLinks = mmem
	; X = ref1(array(Y)) ->
	    process_elem_type_expr(Y, YT),
	    T = ref1(array(YT)),
	    MemLinks = cmem
	; X = ref1(mut(T0)) ->
	    process_type_expr(T0, T1, _MemLinks),
	    T = mut(T1),
	    MemLinks = cmem
	; X = ref1(T0) ->
	    process_type_expr(T0, T1, _MemLinks),
	    T = ref1(T1),
	    MemLinks = cmem
	; X = ref0(array(Y, Size)) ->
	    process_elem_type_expr(Y, YT2),
	    T = ref0(array(YT2, Size)),
	    MemLinks = cmem
	; X = ref0(mut(T0)) ->
	    process_type_expr(T0, T1, _MemLinks),
	    T = mut(T1),
	    MemLinks = mmem
	; X = ref0(T0) ->
	    process_type_expr(T0, T1, _MemLinks),
	    T = T1,
	    MemLinks = cmem
	; X = array(ref0(Y), Size) ->
	    process_type_expr(Y, YT, _MemLinks),
	    T = ref0(array(ref0(YT), Size)),
	    MemLinks = cmem
	; X = array(Y, Size) ->
	    process_type_expr(Y, YT, _YMemLinks),
	    T = ref0(array(ref0(YT), Size)),
	    MemLinks = cmem
	; is_type(X) ->
	    T = X,
	    MemLinks = unkmem
	; ptoc__impcomp:error(['unknown type expression ', X])
	).

process_elem_type_expr(Y) := YT2 :-
	process_type_expr(Y, YT, YMemLinks),
	YT2 = ( YT = ref0(_) ? YT
	      | YT = ref1(_) ? YT
	      | YT = mut(_), YMemLinks = cmem ? ref1(YT)
	      | YT = mut(_), YMemLinks = mmem ? ref0(YT)
	      | ref0(YT)
	      ).
}.

% Search a good smallptr model for the required upper and lower bits.
% Heuristics:
%   (performance)
%   - Adjust the model lower bits to the valptr lower bits when possible
%     (to avoid shifting)
%   - In 32 bits, it is cheap to reserve up to 2 lower bits.
%   - In 64 bits, it is cheap to reserve up to 3 lower bits.
%   (address space)
%   - In 32 bits, upper bits are very expensive (limits address space)
%   - In 64 bits, upper bits are not expensive (address space is huge)
%     (align bits can be wasted if that makes pointer untagging faster)

{
:- extends absmach_ctx.
:- fluid upperbits :: any.
:- fluid lowerbits :: any.
find_smallptr_model(Model) :- find_smallptr_model__2(Model), !.
find_smallptr_model(_) :-
	ptoc__impcomp:error(['not found a smallptr model for ', ~upperbits, ' upper bits and ', ~lowerbits, ' lower bits']).

find_smallptr_model__2(Model) :-
	ipexp.get__use_opt(pointer64), !,
	find_smallptr64_model(Model).
find_smallptr_model__2(Model) :-
	find_smallptr32_model(Model).
}.

{
:- fluid upperbits :: any.
:- fluid lowerbits :: any.
find_smallptr64_model(Model) :-
	( ~lowerbits =< 3 ->
	    % adjust smallptr upper and lower bits to the required bits
	    Model = model(~upperbits, ~lowerbits)
	; % use 3 align bits and reserve the rest of upper bits
	  AllBits = ~all_bits,
          X is AllBits - 3,
	  Model = model(X, 3)
	).

find_smallptr32_model(Model) :-
	AllBits = ~all_bits,
	( AllBits = 0 ->
            % waste align bits
	    Model = model(0,0)
	; % use 2 align bits and reserve the rest of upper bits
          X is AllBits - 2,
	  Model = model(X, 2)
	).

all_bits := AllBits :-
	AllBits is ~upperbits + ~lowerbits.
}.

% ---------------------------------------------------------------------------
% (Prolog to C compiler) Back-end to C
% This part takes the output of ptoc__lowcomp and generates C code
% TODO: rename lowcomp by annotated bytecode compiler
% TODO: this module does the unfolding of that annotated bytecode w.r.t. the machine definition! (that is not yet shared with emucomp)
% TODO: merge with previous code! (the old emucomp)

:- use_module(.(ptoc__ins)).
:- use_module(.(ptoc__props)).
:- use_module(.(foreign__gluecode)).

:- use_module(.(sht_analyzer)).

:- include(.(absint__interface)).

% TODO: identify if-then-else to compile semidet preds with more than one clause
% TODO: insert missing... wake_and_call(PredicateName, Cont)) --> !,
%	{ symboltbl__define_predicate(predtblentry(PredicateName), PredicateDefinitionIdentifier, Symbols0, Symbols) },
%	c_cont(Cont),   
%	[return(call(q(state, enter_predicate), [state, PredicateDefinitionIdentifier]))].
% TODO: recursive call optimization in opt_low (global(A), ... jump(global(A)) -> global(A), local(A'), ... jump(local(A')))

% ---------------------------------------------------------------------------

% TODO: share with lowcomp definition
get_imptype(A) := T :-
	trust(A instance_of termvar),
	T = ~A.getp(imptype), !.
get_imptype(_) := tagged. % default value

% ---------------------------------------------------------------------------
% Symbols

% Emit declaration of symbols
{
:- extends absmach_ctx.
:- fluid cdecls :: accum.
gn_symbol_decls([]).
gn_symbol_decls([symbol(Identifier, Value)|Xs]) :-
	( symbol__decl(Value, Identifier) ->
	    true
	; ptoc__impcomp:error(['unrecognized symbol: ', Value])
	),
	gn_symbol_decls(Xs).
}.

% Emit code to register symbols
% TODO: rename ccode by cstats???
{
:- fluid ccode :: accum.
gn_symbol_regs([]).
gn_symbol_regs([X|Xs]) :- gn_symbol_reg(X), gn_symbol_regs(Xs).

gn_symbol_reg(symbol(Identifier, Value)) :-
	symbol__reg(Value, Identifier), !.
gn_symbol_reg(symbol(_, Value)) :-
	ptoc__impcomp:error(['cannot register symbol ', Value]).

% Emit code to unregister symbols
gn_symbol_unregs([]).
gn_symbol_unregs([X|Xs]) :- gn_symbol_unreg(X), gn_symbol_unregs(Xs).

gn_symbol_unreg(symbol(Identifier, Value)) :-
	symbol__unreg(Value, Identifier), !.
gn_symbol_unreg(symbol(_, Value)) :-
	ptoc__impcomp:error(['cannot unregister symbol ', Value]).
}.

% ---------------------------------------------------------------------------

% number of alive registers: maximum x(_) register plus 1
% TODO: not correct (registers may have holes!)
% TODO: avoid holes by reimplementing it as a register mask (one bit per register... bit 31 for more registers, etc.)
liveset_to_arity(Xs) := Arity :-
	( Ms0 = ~getxmems(Xs) ->
	    true
	; ptoc__impcomp:error(['unsupported liveset ', Xs, ' (only X registers are supported)'])
	),
	sort(Ms0, Ms),
	max_arity__no_holes(Ms, 0, _Holes, Arity).
% TODO: fix? in some places (when only heap reallocation is allowed) holes are not dangerous
%	( Holes == yes ->
%	    errlog:bug(['warning: liveset with holes ', Xs])
%	; true
%	).

% get the mem of all variables, ensuring that it contains assigned X registers
getxmems([]) := [] :- !.
getxmems([X|Xs]) := Ms :-
	trust(X instance_of termvar),
	M = ~X.getp(mem),
	nonvar(M), M = default_choice, % remove default_choice?
	!,
	Ms = ~getxmems(Xs).
getxmems([X|Xs]) := [M|Ms] :- !,
	trust(X instance_of termvar),
	M = ~X.getp(mem),
	nonvar(M), M = x(I), nonvar(I),
	Ms = ~getxmems(Xs).

% get the max arity of the (sorted) mems, ensuring that they are contiguous
max_arity__no_holes([], _, _, Arity) :- !, Arity = 0.
max_arity__no_holes([x(I)|Ms], J, Holes, Arity) :- !,
	( J == I -> true ; Holes = yes ),
	I1 is I + 1,
	( Ms = [] ->
	    Arity = I1
	; max_arity__no_holes(Ms, I1, Holes, Arity)
	).

{
:- extends absmach_ctx.
:- fluid tbl :: bb_tbl + u.
pred_label(GId) := X :-
	trust(GId instance_of predicate_x),
	( ImpCode = ~GId.get_prop(impcode) ->
	    X = quoted(ImpCode)
	; true = ~GId.get_prop(impreg) ->
	    Name = ~GId.name,
	    X = predtblentry(Name)
	; Name = ~GId.name,
	  X = global(Name)
	).
}.

% ---------------------------------------------------------------------------

{
:- extends absmach_ctx.
:- fluid tbl :: bb_tbl + u.
define_nativeproto(Name) :-
	SS = ~tbl.symboltbl,
	( symboltbl__lookup(nativeproto(Name), _, SS) ->
	    true
	; PredId = ~predicate_x.reg_new(Name),
	  PredId.set_code(iproto),
	  call(( cdecls :: accum(Decls), gn_pred_code(PredId) )),
	  symboltbl__insert(nativeproto(Name), Decls, SS)
	).

define_foreignproto(Name, Decl) :-
	SS = ~tbl.symboltbl,
	( symboltbl__lookup(nativeproto(Name), _, SS) ->
	    true
	; symboltbl__insert(nativeproto(Name), [Decl], SS)
	).
}.

% ---------------------------------------------------------------------------

% TODO: needed?? why??
%need_prealloc(var(_)).
%need_prealloc(float(_)).
%need_prealloc(bignum(_)).
%need_prealloc(list).
%need_prealloc(str(_)).

detdeftype(UseWorker, Det, InTypes, OutTypes, ForeignProps) := Type :-
	OutType = ~detdeftype_out(Det, OutTypes),
	InTypes2 = ~add_context_types(UseWorker, InTypes),
	Type0 = function(InTypes2, OutType),
	add_foreign_props(ForeignProps, Type0, Type).

add_foreign_props([ForeignProp|ForeignProps], Type0, Type) :- !,
	Type1 = ForeignProp+Type0,
	add_foreign_props(ForeignProps, Type1, Type).
add_foreign_props([], Type, Type).

detdeftype_out(nondet, []) := bcp_t :- !.
detdeftype_out(semidet, []) := bool_t :- !.
detdeftype_out(det_with_failins, []) := bool_t :- !.
detdeftype_out(det, []) := void :- !.
detdeftype_out(det, [OutCType]) := OutCType :- !.
detdeftype_out(semidet_re, [OutCType]) := OutCType :- !.

% TODO: last_call should be renamed to something like 'continue'

{
:- extends absmach_ctx.
:- fluid tbl :: bb_tbl + u.
add_autov(X, Type) :-
	SubAuto = ~tbl.sub(subauto),
	Key = autov(X),
	( SubAuto.get_decl(Key, autodecl(_Used, _AutoV, _)) ->
	    true
	; Decl = autodecl(_Used, _AutoV, Type),
	  SubAuto.insert_decl(Key, Decl)
	).

mark_used_autov(X, R, Type) :-
	SubAuto = ~tbl.sub(subauto),
	Key = autov(X),
	( SubAuto.get_decl(Key, autodecl(Used, AutoV, Type)) ->
	    Used = yes,
	    R = address(AutoV)
	; ptoc__impcomp:error(['not an autov ', X])
	).

translate_label(Label, Y) :- Y = ~ident(Label).
}.

simp_cond(X) := X :- var(X), !.
simp_cond(logical_not(logical_not(X))) := X :- !.
simp_cond(X) := X.

% ---------------------------------------------------------------------------

symboltbl__define(X, Id, SS) :-
	( symboltbl__lookup(X, Id, SS) ->
	    true
	; symboltbl__insert(X, Id, SS)
	).

symboltbl__define_predicate(Predicate, PredicateDefinitionIdentifier, SS) :-
	Predicate = registered(_, _, _, _), !,
	symboltbl__insert(Predicate, PredicateDefinitionIdentifier, SS).
symboltbl__define_predicate(Predicate, PredicateDefinitionIdentifier, SS) :-
	Predicate = predtblentry(PredicateName), !,
	( symboltbl__lookup(registered(_, PredicateName, _, _), PredicateDefinitionIdentifier, SS) ->
	    true
	; symboltbl__lookup(Predicate, PredicateDefinitionIdentifier, SS) ->
	    true
	; symboltbl__insert(Predicate, PredicateDefinitionIdentifier, SS)
	).
symboltbl__define_predicate(Predicate, PredicateDefinitionIdentifier, SS) :-
	symboltbl__insert(Predicate, PredicateDefinitionIdentifier, SS).

symboltbl__insert(Defined, Definition, Symbols) :-
	symboltbl__insert__2(Symbols, Defined, Definition).

symboltbl__insert__2(SS, Defined, Definition) :- var(SS), !,
	SS = [Defined-Definition|_].
symboltbl__insert__2([_|SS], Defined, Definition) :-
	symboltbl__insert__2(SS, Defined, Definition).

symboltbl__lookup(Defined, Definition, Symbols) :-
	symboltbl__lookup__2(Symbols, Defined, Definition).

symboltbl__lookup__2(Symbols, _, _) :- var(Symbols), !, fail.
symboltbl__lookup__2([Defined0-Definition|_], Defined, Definition) :-
	Defined0 == Defined, !.
symboltbl__lookup__2([_|Symbols], Defined, Definition) :-
	symboltbl__lookup__2(Symbols, Defined, Definition).

symboltbl__symbol_list(Xs) := [] :- var(Xs), !.
symboltbl__symbol_list([Value-Identifier|Xs]) := [symbol(Identifier, Value)|~symboltbl__symbol_list(Xs)] :-
	% TODO: remove those entries before?
	\+ Value = nativeproto(_),
	!.
symboltbl__symbol_list([_|Xs]) := ~symboltbl__symbol_list(Xs).

{
:- fluid cdecls :: accum.
symboltbl__nativeprotos(Xs) :- var(Xs), !.
symboltbl__nativeprotos([Value-Decls|Xs]) :-
	Value = nativeproto(_), !,
	emit_cdecls(Decls),
	symboltbl__nativeprotos(Xs).
symboltbl__nativeprotos([_|Xs]) :-
	symboltbl__nativeprotos(Xs).

emit_cdecls([]).
emit_cdecls([X|Xs]) :- cdecls.add(X), emit_cdecls(Xs).
}.

% ---------------------------------------------------------------------------
% Declaration and initialization of symbols
% TODO: complete symbol__unreg entries (unregister builtins, atoms, functors, functions, etc.)
% TODO: Define a symbol class

:- discontiguous symbol__decl/2.
{
    :- extends absmach_ctx.
    :- fluid cdecls :: accum.
    :- '$props'(symbol__decl/2, []). % TODO: trick to get the predicate defined in this scope
}.

:- discontiguous symbol__reg/2.
{
    :- fluid ccode :: accum.
    :- '$props'(symbol__reg/2, []). % TODO: trick to get the predicate defined in this scope
}.

:- discontiguous symbol__unreg/2.
{
    :- fluid ccode :: accum.
    :- '$props'(symbol__unreg/2, []). % TODO: trick to get the predicate defined in this scope
}.

% ---------------------------------------------------------------------------

symbol__decl(registered(_, _, _, _), Identifier) :- !,
	cdecls.add(declare(Identifier, pointer(definition_t))).
symbol__reg(registered(Imp, Name, Bits, Label), Identifier) :- !,
	symbol__reg__registered(Imp, Identifier, Name, Label),
	symbol__reg__defbits(Name, Bits).
symbol__unreg(registered(Imp, Name, _, _), _) :- !,
	symbol__unreg__registered(Imp, Name).

{
:- fluid ccode :: accum.

symbol__reg__defbits(_, 0) :- !. % no special bits
symbol__reg__defbits(Name/Arity, Bits) :- !,
	atom_codes(Name, NameString),
	ccode.add(call(set_defbits, [string(NameString), Arity, Bits])).

symbol__reg__registered(det, Identifier, Name/Arity, Label) :- !,
	atom_codes(Name, NameString),
	ccode.add(Identifier = call(register_cvoid, [string(NameString), Arity, Label])).
symbol__reg__registered(semidet, Identifier, Name/Arity, Label) :- !,
	atom_codes(Name, NameString),
	ccode.add(Identifier = call(register_cbool, [string(NameString), Arity, Label])).
symbol__reg__registered(nondet, Identifier, Name/Arity, Label) :- !,
	atom_codes(Name, NameString),
	ccode.add(Identifier = call(register_cinsnp, [string(NameString), Arity, Label])).
symbol__reg__registered(Imp, _, Name/Arity, _) :-
	ptoc__impcomp:error(['predicate ', Name/Arity, ' with imp=', Imp, ' cannot be registered']).

symbol__unreg__registered(det, Name/Arity) :- !,
	atom_codes(Name, NameString),
	ccode.add(call(unregister_cvoid, [string(NameString), Arity])).
symbol__unreg__registered(semidet, Name/Arity) :- !,
	atom_codes(Name, NameString),
	ccode.add(call(unregister_cbool, [string(NameString), Arity])).
symbol__unreg__registered(nondet, Name/Arity) :- !,
	atom_codes(Name, NameString),
	ccode.add(call(unregister_cinsnp, [string(NameString), Arity])).
symbol__unreg__registered(Imp, Name/Arity) :-
	ptoc__impcomp:error(['predicate ', Name/Arity, ' with imp=', Imp, ' cannot be unregistered']).
}.

% ---------------------------------------------------------------------------

symbol__decl(predtblentry(_), Identifier) :- !,
	cdecls.add(declare(Identifier, pointer(definition_t))).
symbol__reg(predtblentry(Name/Arity), Identifier) :- !,
	StringName = ~atom_codes(Name),
	ccode.add(Identifier = call(query_predicate, [string(StringName), Arity])).
symbol__unreg(predtblentry(_), _) :- !. % TODO: implement?

% ---------------------------------------------------------------------------

symbol__decl(functor(_), Identifier) :- !,
	cdecls.add(declare(Identifier, tagged_t)).
symbol__reg(functor(Name/Arity), Identifier) :- !,
	StringName = ~atom_codes(Name),
	ccode.add(Identifier = call(deffunctor, [string(StringName), Arity])).
symbol__unreg(functor(_), _) :- !. % TODO: implement?

% ---------------------------------------------------------------------------

symbol__decl(atom(_), Identifier) :- !,
	cdecls.add(declare(Identifier, tagged_t)).
symbol__reg(atom(Name), Identifier) :- !,
	StringName = ~atom_codes(Name),
	ccode.add(Identifier = call('GET_ATOM', [string(StringName)])).
symbol__unreg(atom(_), _) :- !. % TODO: implement?

% ---------------------------------------------------------------------------

symbol__decl(float(X), Identifier) :- !,
	ipexp.float_size(X, H),
	% note: add the size of the head functor
	ipexp.get__tagged_size(S),
	Size is H + S,
	cdecls.add(declare(Identifier, array(tagged_t, ~size_in_tagged(Size)))).
symbol__reg(float(X), Identifier) :- !,
	XString = ~number_codes(X),
	CName = 'init_number', % TODO: add properties, define code in ImProlog?
	ccode.add(call(CName, ~add_context_args(with_worker, [string(XString), Identifier]))).
symbol__unreg(float(_), _) :- !. % TODO: implement?

{
:- extends absmach_ctx.
size_in_tagged(S0) := S :-
	ipexp.get__tagged_size(Ts),
	S is (S0 + Ts - 1) // Ts.
}.

% ---------------------------------------------------------------------------

symbol__decl(bignum(X), Identifier) :- !,
	ipexp.bignum_aligned_size(X, H),
	% note: add the size of the head functor
	ipexp.get__tagged_size(S),
	AlignedSize is H + S,
	cdecls.add(declare(Identifier, array(tagged_t, ~size_in_tagged(AlignedSize)))).
symbol__reg(bignum(X), Identifier) :- !,
	XString = ~number_codes(X),
	CName = 'init_number', % TODO: add properties, define code in ImProlog?
	ccode.add(call(CName, ~add_context_args(with_worker, [string(XString), Identifier]))).
symbol__unreg(bignum(_), _) :- !. % TODO: implement?

% ---------------------------------------------------------------------------

symbol__decl(liveinfo(_, _), Identifier) :- !,
	cdecls.add(declare(Identifier, liveinfo_t)).
symbol__reg(liveinfo(HeapSize, Arity), Identifier) :- !,
	ccode.add(call('LIVEINFO__INIT', [Identifier, HeapSize, Arity])).
symbol__unreg(liveinfo(_, _), _) :- !. % TODO: implement?

% ---------------------------------------------------------------------------

symbol__decl(failcont(_, _, _), Identifier) :- !,
	cdecls.add(declare(Identifier, pointer(try_node_t))).
symbol__reg(failcont(_, Label, Arity), Identifier) :- !,
	% TODO: define code in ImProlog
	ccode.add(Identifier = call('def_retry_cinsnp', ~add_context_args(with_worker, [~ident(Label), Arity]))).
symbol__unreg(failcont(_, _, _), _) :- !. % TODO: implement?

% ---------------------------------------------------------------------------

symbol__decl(successcont(_, _, _), Identifier) :- !,
	cdecls.add(declare(Identifier, bcp_t)).
symbol__reg(successcont(_, Label, FrameLiveSize), Identifier) :- !,
	% TODO: define code in ImProlog
	ccode.add(Identifier = call('def_success_cinsnp', ~add_context_args(with_worker, [~ident(Label), FrameLiveSize]))).
symbol__unreg(successcont(_, _, _), _) :- !. % TODO: implement?

% ---------------------------------------------------------------------------

symbol__decl(execcont(_), Identifier) :- !,
	cdecls.add(declare(Identifier, bcp_t)).
symbol__reg(execcont(Label), Identifier) :- !,
	% TODO: define code in ImProlog
	ccode.add(Identifier = call('def_exec_cinsnp', ~add_context_args(with_worker, [~ident(Label)]))).
symbol__unreg(execcont(_), _) :- !. % TODO: implement?

% ---------------------------------------------------------------------------

symbol__decl(dbuiltin(_, _, _), _) :- !.
symbol__reg(dbuiltin(EAtom, EArity, CName), _) :- !,
	atom_codes(EAtom, EAtomString),
	ccode.add(call(register_builtin, [string(EAtomString), EArity, CName])).
symbol__unreg(dbuiltin(_, _, _), _) :- !. % TODO: implement?

% ---------------------------------------------------------------------------

symbol__decl(dswitchcfun(_), _) :- !.
symbol__reg(dswitchcfun(Xs), _) :- !,
	% TODO: do not ignore atom and arity?
	% TODO: declare a hash table for this predicate and pass it to register_ctagged
	reg_cswitchcfun(Xs).
symbol__unreg(dswitchcfun(_), _) :- !. % TODO: implement?

{
:- fluid ccode :: accum.
reg_cswitchcfun((A;B)) :- !,
	reg_cswitchcfun(A),
	reg_cswitchcfun(B).
reg_cswitchcfun((EAtom/EArity->CName)) :- !,
	atom_codes(EAtom, EAtomString),
	ccode.add(call(register_ctagged, [string(EAtomString), EArity, CName])).
}.

% ---------------------------------------------------------------------------

symbol__decl(taggedhashtab(_), Identifier) :- !,
	cdecls.add(declare(Identifier, pointer(hashtab_t))).
symbol__reg(taggedhashtab(Xs), Identifier) :- !,
	% TODO: do not ignore atom and arity?
	% TODO: declare a hash table for this predicate and pass it to register_ctagged
	Length = ~length(Xs),
	ccode.add(Identifier = call(register_sw_on_tagged, [aggregate(array(tagged_t), Xs), Length])).
symbol__unreg(taggedhashtab(_), Identifier) :- !,
	ccode.add(call(hashtab_free, [Identifier])).

% ---------------------------------------------------------------------------

%ident(Prefix, Name/Arity, identifier("~w_~w_~d", [Prefix, Name, Arity])).
ident(quoted(X)) := X :- !.
ident(global(X)) := X :- var(X), !.
ident(none) := 'NULL' :- !. % TODO: a hack for push_choice
ident(global(aux(X)/_)) := X :- !.
ident(global(X)) := Y :- X = q(Mod, A), !, Y = q(~encode_symbol_a(Mod), A).
ident(global(X)) := Y :- Y = ~encode_symbol_a(X).

% ---------------------------------------------------------------------------
% Contexts (implicit arguments that are passed to all calls)

{
:- extends absmach_ctx.
get_context(N, A) :=
	( ipexp.get__op__prop(N, A, with_worker) ? with_worker
	| no_worker
	).
}.

add_context_args(UseWorker, Args0, Args) :-
	( UseWorker = no_worker ->
	    Args = Args0
	; UseWorker = with_worker ->
	    Args = ['w'|Args0]
	; ptoc__impcomp:error(['unknown context ', ~~(UseWorker)])
	).
	
add_context_types(UseWorker, Args0, Args) :-
	( UseWorker = no_worker ->
	    Args = Args0
	; UseWorker = with_worker ->
	    Args = [pointer('worker_t')|Args0]
	; ptoc__impcomp:error(['unknown context ', ~~(UseWorker)])
	).
	
% ---------------------------------------------------------------------------
% Emit a C code block from the output generated by the gn_* family of predicates

{
:- extends absmach_ctx.
gn_c_block(Mode, MaybeTrueLab, CName, Code0) := Code :-
	call((
          labset :: multiset01n <- ~multiset01n.empty,
          call((
            ccode :: accum(Code1b),
	    Code0 = ~bb_code.f(DeclsA, CodeA, SubsA),
	    subdecls_flat(DeclsA),
	    flatc(CodeA),
	    subcode_flat(SubsA),
	    ( MaybeTrueLab = yes(TrueLab) -> ccode.add(label(TrueLab)) ; true )
          )),
	  Code1 = ~remove_superfluous_labels(~remove_single_source_labels(Code1b))
        )),
	call((
          seen :: u_dic,
	  scan_decls(Code1, Decls, Code2)
        )),
	( Mode = macro ->
	    LabelPrefix = ~atom_concat(CName, '_lab'),
	    VarPrefix = ~atom_concat(CName, '_var'),
	    scan_labels(Code2, LocalLabels),
	    LocalLabelDecls = ~gn_loc_labs(LocalLabels)
	; Mode = cfun ->
	    LabelPrefix = 'lab',
	    VarPrefix = 'var',
	    LocalLabelDecls = []
	),
	rec_to_loop(CName, Code2, Code2b),
	call((
          labelprefix :: any <- LabelPrefix,
	  n :: m_int <- 0,
	  name_labels(Code2b)
        )),
	call((
          varprefix :: any <- VarPrefix,
	  n :: m_int <- 0,
	  name_vars(Decls)
        )),
	Code3 = ~append(LocalLabelDecls, ~append(Decls, Code2b)),
	Code = block(Code3).
}.

{
:- extends absmach_ctx.
:- fluid labset :: multiset01n.
:- fluid ccode :: accum.

:- '$ctxprj'(flatlab/2, [labset]).
flatlab(BBId) := Lab :-
	trust(BBId instance_of bblock),
	% TODO: for homogeneity, (BBId.lab) or force ~ in every attribute access? (lab/1 is not an attribute)
	Lab = ~BBId.lab,
	labset.insert(Lab).

flatc(seq(A, B)) :- !,
	flatc(A),
	flatc(B).
flatc(cond_br(A, B, C)) :- !,
	ccode.add(if(A, goto(~flatlab(B)), goto(~flatlab(C)))).
flatc(br(A)) :- !,
	ccode.add(goto(~flatlab(A))).
flatc(sw_macro_code(SwitcherName, SwitcherArgR, BBIds)) :- !,
	ccode.add(~sw_macro_code(SwitcherName, SwitcherArgR, BBIds)).
flatc('$nop') :- !.
flatc(A) :- !,
	ccode.add(A).

:- '$ctxprj'(labeled/3, []).
labeled(Label, Code) := (label(Label), Code).

subdecls_flat([]).
subdecls_flat([X|Xs]) :-
	X = decls(Sub),
	sub_flat_decls(Sub),
	subdecls_flat(Xs).

subcode_flat([]).
subcode_flat([X|Xs]) :-
	( X = code(Sub) -> sub_flat_code(Sub)
	; X = bb(Id) -> flat_bb(Id)
	),
	subcode_flat(Xs).

flat_bb(Id) :-
	trust(Id instance_of bblock),
	CStat = ~Id.emitted,
	( var(Emitted) ->
	    Emitted = yes,
	    %
	    Lab = ~Id.lab,
	    ccode.add(label(Lab)),
	    %
	    CStat = ~Id.cstat,
	    CStat = ~bb_code.f(DeclsA, CodeA, SubsA),
	    subdecls_flat(DeclsA),
	    flatc(CodeA),
	    subcode_flat(SubsA)
	; true
	).

% flat the code entries using the order in List, emitting only entries that have a value
sub_flat_code(Sub) :-
	trust(Sub instance_of bb_container),
	codedic :: u_dic <- ~Sub.codedic,
	AllKeys = ~Sub.allkeys,
	sub_flat_code__2(AllKeys).
{
:- fluid codedic :: u_dic.
% note: the key list indicates the order (but some elements may be missing)
sub_flat_code__2([]).
sub_flat_code__2([Key|Xs]) :-
	( codedic.get(Key, subcode(BBId)) -> flat_bb(BBId)
	; true
	),
	sub_flat_code__2(Xs).
}.

% flat the code stored in CodeDic using the order in List, emitting only entries that have a value
sub_flat_decls(Sub) :-
	trust(Sub instance_of bb_container),
	codedic :: u_dic <- ~Sub.codedic,
	AllKeys = ~Sub.allkeys,
	sub_flat_decls__2(AllKeys).
{
:- fluid codedic :: u_dic.
% note: the key list indicates the order (but some elements may be missing)
sub_flat_decls__2([]).
sub_flat_decls__2([Key|Xs]) :-
	( codedic.get(Key, subdecl(Code)) ->
	    sub_flat_decls__3(Code)
	; true
	),
	sub_flat_decls__2(Xs).
}.

sub_flat_decls__3(autodecl(Used, Name, Type)) :- !,
	( Used == yes ->
	    trust(Type instance_of ltype),
	    Type2 = ~Type.applymemlinks(mmem),
	    CType = ~Type2.ctype,
	    D = declare(Name, CType),
	    ccode.add(D)
	; true
	).
% TODO: incomplete
sub_flat_decls__3(declare(L, Type, aggregate(AType, Items))) :- !,
	Items2 = ~flat_exprs(Items),
	ccode.add(declare(L, Type, aggregate(AType, Items2))).
sub_flat_decls__3(X) :- !,
	flatc(X).
	%ccode.add(X).

:- '$ctxprj'(dl_to_list/2, []).
dl_to_list(A, R) :- var(A), !, R = [].
dl_to_list([X|Xs], [X|Ys]) :- dl_to_list(Xs, Ys).

:- '$ctxprj'(only_key/2, []).
only_key([], []).
only_key([(_,Key)|Xs], [Key|Ys]) :- only_key(Xs, Ys).

:- '$ctxprj'(flat_exprs/2, [labset]).
flat_exprs([]) := [].
flat_exprs([X|Xs]) := [~flat_expr(X)|~flat_exprs(Xs)].

:- '$ctxprj'(flat_expr/2, [labset]).
flat_expr(X) := Y :- nonvar(X), X = label_addressbb(Id), !,
	Y = label_address(~flatlab(Id)).
flat_expr(X) := X.

:- '$ctxprj'(sw_macro_code/4, [labset]).
sw_macro_code(swcond(Cond), SwitcherXR, Ys) := R :- !,
	Ys = [ThenId, ElseId],
	R = if(call(Cond, [SwitcherXR]), goto(~flatlab(ThenId)), goto(~flatlab(ElseId))).
sw_macro_code(swif(Value), SwitcherXR, Ys) := R :- !,
	Ys = [ThenId, ElseId],
	R = if((SwitcherXR == Value), goto(~flatlab(ThenId)), goto(~flatlab(ElseId))).
sw_macro_code(switchdef(Cases), SwitcherXR, Ys) := R :- !,
	switchdef_code(Cases, Ys, Cases2),
	R = switch(SwitcherXR, Cases2).
sw_macro_code(Switcher0, SwitcherXR, Ys) := R :-
	labs_to_exprstat_goto(Ys, Ys2),
	R = call(Switcher0, [SwitcherXR|Ys2]).

:- '$ctxprj'(labs_to_exprstat_goto/2, [labset]).
labs_to_exprstat_goto([], []).
labs_to_exprstat_goto([X|Xs], [Y|Ys]) :-
	Y = exprstat(goto(~flatlab(X))),
	labs_to_exprstat_goto(Xs, Ys).

:- '$ctxprj'(switchdef_code/3, [labset]).
switchdef_code([_K], [C], [default(goto(~flatlab(C)))]) :- !.
switchdef_code([K|Ks], [C|Cs], [case(K,goto(~flatlab(C)))|Cases]) :- !, switchdef_code(Ks, Cs, Cases).

:- '$ctxprj'(remove_single_source_labels/2, [labset]).
remove_single_source_labels([]) := [] :- !.
remove_single_source_labels([goto(LabelA), label(LabelB)|Xs]) := ~remove_single_source_labels(Xs) :-
	LabelA == LabelB, labset.single_count(LabelA), !. % coalesce labels
remove_single_source_labels([X|Xs]) := [X|~remove_single_source_labels(Xs)].

:- '$ctxprj'(remove_superfluous_labels/2, []).
remove_superfluous_labels([]) := [] :- !.
remove_superfluous_labels([label(Label), goto(Label)|Xs]) := ~remove_superfluous_labels(Xs) :- !. % coalesce labels
remove_superfluous_labels([X|Xs]) := [X|~remove_superfluous_labels(Xs)].
}.

{
:- fluid seen :: u_dic.
scan_decls([A], [], [A, block([])]) :- A = label(_), !. % note: the empty block avoids a C compiler error (labels are not allowed to end a block)
scan_decls([A|As], Ds0, Bs) :- is_var_decl(A, _), !,
	seen.lookup(A, SeenVal), % TODO: this is necessary because '$vimut' generates duplicated declarations!
	( var(SeenVal) ->
	    SeenVal = yes,
	    Ds0 = [A|Ds]
	; Ds0 = Ds
	),
	scan_decls(As, Ds, Bs).
scan_decls([A|As], Ds, [A|Bs]) :- !,
	scan_decls(As, Ds, Bs).
scan_decls([], [], []).
}.

gn_loc_labs([], []).
gn_loc_labs([A|As], [B|Bs]) :-
	B = declare(A, '__label__'),
	gn_loc_labs(As, Bs).

scan_labels([A|As], [Lab|Bs]) :- A = label(Lab), !,
	scan_labels(As, Bs).
scan_labels([_|As], Bs) :- !,
	scan_labels(As, Bs).
scan_labels([], []).

{
:- fluid labelprefix :: any.
:- fluid n :: m_int.
name_labels([A|As]) :-
	( A = label(Lab), var(Lab) ->
	    Lab = p(~labelprefix, ~n),
	    n.inc(1)
	; true
	),
	name_labels(As).
name_labels([]).
}.

{
:- fluid varprefix :: any.
:- fluid n :: m_int.
name_vars([A|As]) :-
	( is_var_decl(A, Tmp), var(Tmp) ->
	    Tmp = p(~varprefix, ~n),
	    n.inc(1)
	; true
	),
	name_vars(As).
name_vars([]).
}.

is_var_decl(declare(Tmp, _), Tmp).
is_var_decl(declare(Tmp, _, _), Tmp).

% TODO: this is a kludge, use subpr (to do that, create a bblock also for the initial code)
rec_to_loop(CName, Code0, Code) :-
	call((
          cname :: any <- CName,
	  used :: any <- Used,
	  rec_to_loop__2(Code0, Code1)
        )),
	( nonvar(Used), Used = lab(Begin) ->
	    Code = [label(Begin)|Code1]
	; Code = Code0
	).
{
:- fluid cname :: any.
:- fluid used :: any.
rec_to_loop__2([X0|Xs], [X|Code1]) :-
	X0 = return(call(Name, ~add_context_args(with_worker, []))),
	Name == ~cname,
	!,
	~used = lab(Begin),
	X = goto(Begin),
	rec_to_loop__2(Xs, Code1).
rec_to_loop__2([X0,X1|Xs], [X|Code1]) :-
	X0 = call(Name, ~add_context_args(with_worker, [])),
	X1 = return,
	Name == ~cname,
	!,
	~used = lab(Begin),
	X = goto(Begin),
	rec_to_loop__2(Xs, Code1).
rec_to_loop__2([X|Xs], [X|Code1]) :- !,
	rec_to_loop__2(Xs, Code1).
rec_to_loop__2([], []).
}.

% A multiset of with zero-one-more usage count
:- class multiset01n {
    :- '$statemodel'(single).
    :- '$raw_state'.

    % TODO: missing instance_of__/1

    :- constructor empty_/0.
    empty_.

    insert(X) :-    
        dic_lookup(~self, X, Count),
        ( var(Count) -> % count is 0
            Count = s(_) % count is 1
        ; Count = s(Count0), % count is at least 1
          ( var(Count0) -> Count0 = s(z) % count is 2
          ; true % count is 2 (stop counting)
          )
        ).

    single_count(X) :-
        dic_lookup(~self, X, Count),
        % check that count is 1
        nonvar(Count), Count = s(Count0), var(Count0).
}.

% ---------------------------------------------------------------------------
% General helper predicates

% Emit the input list (accept weak entries)
{
:- fluid cdecls :: accum.
emit_inline([]).
emit_inline([weak(X)|Xs]) :- !, cdecls.add(X), emit_inline(Xs).
emit_inline([X|Xs]) :- cdecls.add(X), emit_inline(Xs).
}.

% Obtain the list of function prototypes
% TODO: if a prototype is found in the original code, does it have to be moved upwards?

% TODO: KLUDGE! (allow public and private decls)
get_code_protos([declare(Name, Type, _)|Xs], [declare(Name, Type2)|Ys]) :-
	code_type(Type), !,
	Type2 = Type, % for C functions
	get_code_protos(Xs, Ys).
get_code_protos([_|Xs], Ys) :- !,
	get_code_protos(Xs, Ys).
get_code_protos([], []).

% TODO: KLUDGE! (allow public and private decls)
get_var_protos([X|Xs], Ys) :-
	( X = declare(Name, Type, _) -> true
	; X = declare(Name, Type) -> true
	),
	\+ code_type(Type),
	!,
	( has_static(Type) ->
	    Ys = Ys0
	; Type2 = (extern+Type), % for variables
	  Y = declare(Name, Type2),
	  Ys = [Y|Ys0]
	),
	get_var_protos(Xs, Ys0).
get_var_protos([_|Xs], Ys) :- !,
	get_var_protos(Xs, Ys).
get_var_protos([], []).

code_type(function(_, _)) :- !.
code_type(A + B) :- ( code_type(A) -> true ; code_type(B) -> true ).

has_static(static) :- !.
has_static(A + B) :- ( has_static(A) -> true ; has_static(B) -> true ).

% ---------------------------------------------------------------------------

:- public error/1.
error(Msg) :-
	errlog:bug(['error: '|Msg]),
	fail.


