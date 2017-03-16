:- module(_, [], [compiler(complang)]).

:- use_module(library(dict)).
:- use_module(library(lists), 
        [length/2, dlist/3, last/2, list_lookup/3,
         nocontainsx/2, contains_ro/2,
         intset_insert/3, intset_delete/3, intset_in/2, intset_sequence/3,
	 append/3]).

:- use_module(compiler(module_exp)).
:- use_module(compiler(module_ipexp)).
:- use_module(compiler(errlog)).
:- use_module(compiler(ptoc__props)).
:- use_module(compiler(ptoc__ins)).
:- use_module(compiler(memoize)). % because of module_ipexp

:- use_module(compiler(compiler__expand)). % unflat_head

my_var_new(D, Mem) := V :-
	V0 = ~termvar.new,
	V1 = ~V0.addp(d, D),
	V = ~V1.addp(mem, Mem).
my_var_mem(X) := ~X.getp(mem) :-
	trust(X instance_of termvar).
my_var_d(X) := ~X.getp(d) :-
	trust(X instance_of termvar).
xvarmem(X) :-
	nonvar(X),
	trust(X instance_of termvar),
	~X.getp(mem) = x(_).
yvarmem(X) :-
	nonvar(X),
	trust(X instance_of termvar),
	~X.getp(mem) = y(_).
xvarmem2(X, R) :-
	nonvar(X),
	trust(X instance_of termvar),
	~X.getp(mem) = x(R).
yvarmem2(X, R) :-
	nonvar(X),
	trust(X instance_of termvar),
	~X.getp(mem) = y(R).
eqvarmem(X, Y) :-
	nonvar(X),
	trust(X instance_of termvar),
	Xmem = ~X.getp(mem),
	nonvar(Y),
	trust(Y instance_of termvar),
	Ymem = ~Y.getp(mem),
	Xmem = R, Ymem = R.

% ---------------------------------------------------------------------------
% TODO: update!
% Instructions emitted by the compiler:
%
%	choice
%	switch_on_term((Clause)*, (Clause)*, (Key-Clause)*, (Clause)*)
%		       %Var       %List      %Other         %Default
%
%	cframe(Integer)
%	kall(PredLabel,Integer)
%	lastcall(PredLabel)
%	proceed
%	failins
%	blt1(x(Arg1),Name)
%	blt2(x(Arg1),x(Arg2),Name)
%	blt3(x(Arg1),x(Arg2),x(Arg3),Name)
%	fun1(x(Value),x(Arg1),Name,N,EffAr)
%	fun2(x(Value),x(Arg1),x(Arg2),Name,N,EffAr)
%	funre1(x(Value),x(Arg1),Name,N,EffAr)
%	funre2(x(Value),x(Arg1),x(Arg2),Name,N,EffAr)
%	init(x(Arg),x(Arg))
%	init(x(Arg),y(Arg))	% not after first call
%	move(x(Arg),x(Arg),p)
%	move(y(Arg),x(Arg))
%	globunsafe2(x(Arg),x(Arg))
%	globunsafe(y(Arg),x(Arg))
%	ld_cons(x(Arg),Atomic)
%	ld_blob(x(Arg),Blob)
%	ld_str(x(Arg),Functor)
%	ld_lst(x(Arg))
%
%	move(x(Arg),x(Arg),g)
%	move(x(Arg),y(Arg))	% not after first call
%	u_fval(x(Arg),y(Arg))	% after first call
%	u_val(x(Arg),x(Arg))
%	u_val(x(Arg),y(Arg))
%	u_cons(x(Arg),Atomic)
%	u_blob(x(Arg),Blob)
%	u_str(x(Arg),Functor)
%	u_lst(x(Arg))
%
%	un_void
%	un_var(x(Arg))
%	un_var(y(Arg))		% not after first call
%	un_fval(y(Arg))	% after first call
%	un_val(x(Arg))
%	un_val(y(Arg))
%	un_lval(x(Arg))
%	un_lval(y(Arg))
%	un_cons(Atomic)
%	un_blob(Blob)
%	un_str(Functor)
%	un_lst
%
%	alloc
%	dealloc
%	init(L)
%	neck(N)			% create/update choicepoint
%	getchoice(x(Arg))
%	getchoice(y(Arg))
%	cutb(x(-1))			% before alloc
%	cute(x(-1))			% after alloc before init
%	cutf(x(-1))			% after init
%	cutb(x(Arg))			% before alloc
%	cute(x(Arg))			% after alloc before init
%	cutf(x(Arg))			% after init
%	cut(y(Arg))
%
%	heapmargin_call(N, EffAr)

% How to add a new kernel predicate and its instruction:
%
% 1. Add a unit clause to is_ipred/1 and c_icall/5, 
%    saying how to compile goal.
% 2. Add a clause to x_def_use/2 to tell which temporaries the instruction
%    defines and uses and ins_heap/2 to tell how much heap it requires.

% Emulator Interface:
% A bit mask is computed for each clause.  The bits are:
%	2'0000001	X0 may be var
%	2'0000010	X0 may be number
%	2'0000100	X0 may be atom
%	2'0001000	X0 may be list
%	2'0010000	X0 may be structure
%	2'0100000	this clause does an implicit cut
%	2'1000000	this clause contains an explicit cut

% ---------------------------------------------------------------------------
% Replace all the variables in the code with new variables that
% contain the 'd' and 'mem' field.

{
:- fluid exp :: module_exp.
renamevars_clause(Args0, Body0, Args, Body) :-
	rdic :: u_dic,
	renamevars_clause_(Args0, Body0, Args, Body).
{
    :- fluid rdic :: u_dic.

    renamevars_clause_(Args0, Body0, Args, Body) :-
        renamevars_terms(Args0, Args),
        renamevars_body(Body0, Body).

    renamevars_body(Xs, Ss) :-
        goals :: accum(Ss), 
        maplist(([goals] -> ''(X) :-
          ( _ = ~trust_domain(X) ->
              true
          ; renamevars_goal(X, S),
	    trust(S instance_of strgoal),
	    NA = ~S.name,
	    G = ( exp.is_ipred(NA) ? icall(S) | pcall(S, _) ),
	    goals.add(G)
	  )
        ), Xs).

    :- '$ctxprj'(renamevars_terms/2, [rdic]).
    renamevars_terms([X|Xs], [X2|Xs2]) :-
        renamevars_term(X, X2),
        renamevars_terms(Xs, Xs2).
    renamevars_terms([], []).

    :- '$ctxprj'(renamevars_term/2, [rdic]).
    renamevars_term(V, Tran) :-
        V instance_of termvar, !,
        rdic.lookup(~V.name, Tran),
        Tran = ~my_var_new(_,_).
    renamevars_term(X, S) :-
        trust(X instance_of termstr),
        S = ~X.set_args(~renamevars_terms(~X.args)).

    :- '$ctxprj'(renamevars_goal/2, [rdic]).
    renamevars_goal(V, Tran) :-
        V instance_of termvar, !,
        rdic.lookup(~V.name, Tran),
        Tran = ~my_var_new(_,_).
    renamevars_goal(X, S) :-
        trust(X instance_of strgoal),
        S = ~X.set_args(~renamevars_terms(~X.args)).
}.

% note: it works with trans_clause
% precondition: the clause has been flatted
% TODO: unflat could be more intelligent and support more cases that do not come from flatted clauses
unflat_head(As0, B0, As, B1) :-
	ins :: accum,
	~ins = B0,
	% Skip caller_choice and put in B1
	( ins.add(C), % TODO: This is input!
	  C = icall(G),
	  trust(G instance_of strgoal),
	  ~G.name = 'basiccontrol:$caller_choice'/1 ->
	    B1 = [C|B2]
	; B1 = B2
	),
	% Unflat heads
	unflat_head_unifs(As0, [], As),
	B2 = ~ins.

{
:- fluid ins :: accum.
% TODO: bad complexity (num head vars * sizeof code)
% precondition: first list is a list of variables
unflat_head_unifs([], _, []) :- !.
unflat_head_unifs([A|As], Prev, [A2|As2]) :-
	ins.add(I), % TODO: This is input!
	I = icall(G),
	trust(G instance_of strgoal),
	Args = ~G.args,
	NA = ~G.name,
	( NA = 'term_basic:$unify'/2, Args = [A2, B]
	; NA = 'term_basic:$unify'/2, Args = [B, A2]
	; NA = 'term_basic:$instance'/2, Args = [B, A2]
	),
	% TODO: check also the we are not creating a cyclic term, i.e. see that A2 does not contain B in the instance case
	B instance_of termvar, A == B,
	var_not_in_code(Prev, B),
	Rest = ~ins, % TODO: bad complexity!!
	var_not_in_code(Rest, B),
	!,
	unflat_head_unifs(As, [I|Prev], As2).
unflat_head_unifs([A|As], Prev, [A|As2]) :-
	% try with next argument
	unflat_head_unifs(As, Prev, As2).
}.
}.

var_not_in_code([], _).
var_not_in_code([X0|Xs], V) :-
	( X0 = icall(X) -> true ; X0 = pcall(X, _) ),
	var_not_in_goal(X, V),
	var_not_in_code(Xs, V).

var_not_in_goal(X, V) :-
	X instance_of termvar, !, \+ X == V.
var_not_in_goal(X, V) :-
	trust(X instance_of strgoal),
	var_not_in_list(~X.args, V).

var_not_in_term(X, V) :-
	X instance_of termvar, !, \+ X == V.
var_not_in_term(X, V) :-
	trust(X instance_of termstr),
	var_not_in_list(~X.args, V).

var_not_in_list([], _).
var_not_in_list([X|Xs], V) :-
	var_not_in_term(X, V),
	var_not_in_list(Xs, V).

% --

{
:- fluid exp :: module_exp.
:- public compile_clauses/3.
compile_clauses([], _, []).
compile_clauses([C|Cs], Args, [C2|Cs2]) :-
	compile_clause(C, Args, C2),
	compile_clauses(Cs, Args, Cs2).

% Translating a clause: Emit naive code, then extract  info. and delay
% 'movexgs', then allocate temps for deep and shallow cases,
% finally pho emitted code.
% TODO: Add a class for type_key/2
compile_clause(Body000, Args0, clause(Code2, Data)) :-
	renamevars_clause(Args0, Body000, Args1, Body00b),
        % TODO: optimize unflat... or remove it, but ensure that the results are ok (without it the contents of X registers is reused when they contain constants - I do not know what is better)
	unflat_head(Args1, Body00b, Args, Body00),
	( Body00 = [icall(G)|Body0],
	  trust(G instance_of strgoal),
	  ~G.name = 'basiccontrol:$caller_choice'/1,
	  [Choice] = ~G.args ->
	    true
	; Body00 = Body0,
	  Choice = ~my_var_new(_,_) % choice was not used
	),
	Trivial = ( trivial_head(Args, []) ? true | false ),
	extract_type(Body0, Choice, Trivial, Args, 2'11111, Type0, Body1),
	TypeKey0 = type_key(Type0,nohash),
	call((
          chn :: m_int <- 0,
	  list :: any <- List,
	  mk_occurrences_body(Choice, Args, Body1)
        )),
	allocate_vas(List, 0),
	call((
          typekey :: m_any <- TypeKey0,
	  trans_clause(Args, Choice, Body1, Code),
	  ~typekey = TypeKey
	)),
	incore_parse_key(TypeKey, Data),
	call((
          bcode :: m_any <- Code,
	  postcomp,
	  Code2 = ~bcode
	)).

extract_type(Gs, Choice, Trivial, _, Type0, Type, Gs2) :-
	Gs = [G|Gs0],
	G = icall(S),
	trust(S instance_of strgoal),
	~S.name = 'basiccontrol:$cut'/1,
	[X] = ~S.args,
	Trivial = true,
	!,
	Type is Type0\/2'0100000,
	( X == Choice ->
	    Gs2 = Gs0 % cut will be done in indexing tree
	; Gs2 = Gs
	).
extract_type(Gs, Choice, Trivial, Args, Type0, Type, Gs2) :-
	Gs = [G|Gs0],
	( G = icall(S) -> true ; G = pcall(S, _) ),
	trust(S instance_of strgoal),
	~S.name = Atom/Arity,
	GArgs = ~S.args,
	type_ck(Atom, Arity, GArgs, Args, TypeG, Auto),
	!,
	( Trivial = true, Auto = 1 ->
	    Type1 is Type0/\TypeG,
	    extract_type(Gs0, Choice, Trivial, Args, Type1, Type, Gs2)
	; Type is Type0/\(TypeG\/1),
	  Gs2 = Gs
	).
extract_type(Gs, _, _, _, Type, Type, Gs).
}.

% TODO: use symbolic definitions for the bits
type_ck('term_typing:var', 1, [X], [Y|_], 2'1, 1) :- X==Y.
type_ck('attributes:get_attribute', 2, [X,_], [Y|_], 2'1, 0) :- X==Y. % DMCAI -- ATTRVARS
type_ck('term_typing:nonvar', 1, [X], [Y|_], 2'11110, 1) :- X==Y.
type_ck('term_typing:atom', 1, [X], [Y|_], 2'100, 0) :- X==Y.
type_ck('term_typing:atomic', 1, [X], [Y|_], 2'110, 0) :- X==Y.
type_ck('term_typing:number', 1, [X], [Y|_], 2'10, 0) :- X==Y.

trivial_head([], _).
trivial_head([X|Xs], Seen) :-
	X instance_of termvar,
	nocontainsx(Seen, X), % TODO: rewrite for the var case
	trivial_head(Xs, [X|Seen]).

% Compute environment sizes, allocate perm. variables.
{
:- fluid chn :: m_int + u.
:- fluid list :: any.
mk_occurrences_body(Choice, Args, Gs) :- !,
	mk_occurrences_list(Gs),
	record_occurrences(Choice),
	record_occurrences_args(Args).

mk_occurrences_list([]) :- !.
mk_occurrences_list([icall(G)|Gs]) :- !,
	mk_occurrences_list(Gs),
	record_occurrences_goal(G).
mk_occurrences_list([pcall(G,U)|Gs]) :-
	Chn = ~chn,
	chn.inc(1),
	mk_occurrences_list(Gs),
	last(~list, size(U)),
	chn <- Chn,
	record_occurrences_goal(G).

record_occurrences_goal(X) :-
	X instance_of termvar, !,
	Mem = ~my_var_mem(X),
	list_lookup(~list, Mem, Occs), % TODO: complexity???
	add_chunk(Occs, ~chn).
record_occurrences_goal(X) :-
	trust(X instance_of strgoal),
	Args = ~X.args,
	record_occurrences_args(Args).

record_occurrences(X) :-
	X instance_of termvar, !,
	Mem = ~my_var_mem(X),
	list_lookup(~list, Mem, Occs), % TODO: complexity???
	add_chunk(Occs, ~chn).
record_occurrences(X) :-
	trust(X instance_of termstr),
	record_occurrences_args(~X.args).

record_occurrences_args(Xs) :-
	maplist(([u(chn)] -> ''(X) :-
          record_occurrences(X)
        ), Xs).
}.

add_chunk([Chn|_], Chn) :- !.
add_chunk([_|nonsingle], _). % mark nonsingle (variable appears in more than one chunk)

allocate_vas([], _) :- !.
allocate_vas([size(N)|List], N) :- !,
	allocate_vas(List, N).
allocate_vas([Mem-Occs|List], N0) :-
	allocate_vas_2(Occs, Mem, N0, N),
	allocate_vas(List, N).

allocate_vas_2([_|Single], Mem, N, N) :- var(Single), !, % single chunk
	Mem = x(_).
allocate_vas_2(_, Mem, N0, N) :- Mem = y(N0), N is N0+1.

incore_parse_key(type_key(Type,Key), Data) :- !,
	( Key = hash(N/A) ->
	    functor(F, N, A),
	    Data = f(Type, F)
	; Key = hash(K) ->
	    Data = f(Type, K)
	; Key = nohash ->
	    Data = f(Type, _)
	).

{
:- fluid exp :: module_exp.
:- fluid typekey :: m_any.
trans_clause(Args, Choice, Body, Code) :-
%	errlog:trace([before_trans_clause(Args, Body)]),
	call(( wcode :: accum(Code0), trans_clause__1(Args, Choice, Body, Kind, InArity, OutArity) )),
%	errlog:trace([before_extract_index(Args, Code0)]),
	extract_index(Code0, Code1, Gets, Gets, OutArity),
%	errlog:trace([before_clause_lifetime(Args, Code1)]),
        clause_lifetime(Kind, InArity, OutArity, [ensure_space(Kind, InArity, _)|Code1], Code2),
%	errlog:trace([after__clause_lifetime(Args, Code2)]),
	Code = ~peep_clause(Code2).

:- '$ctxprj'(extract_index/5, [typekey]).
% extract_index(C0, C1, Y0, Y) :-
%	C0 = sequence of WAM insns,
%	C1 = C0, with neck(_) moved and
%    with all elements in Y0 moved to the end,
%	~typekey = type and key info updated by indexing insn,
%	Y0 = Y = all 'movexg' up to first call.
extract_index([I|Code1], Code, Queue, Head, A) :-
	is_movexg(I), !,
	filter_get(I, A, Queue, Queue1),
	extract_index(Code1, Code, Queue1, Head, A).
extract_index([move(X,Y,p)|Code0], Code, Queue, Head, A) :-
	xvarmem(X), xvarmem(Y), eqvarmem(X, Y), !,
	extract_index(Code0, Code, Queue, Head, A).
extract_index([I|Code0], Code, Queue, Head, _) :-
	index_insn(I, Type01, Key1),
	Type1 is Type01 \/ 2'1,
	~typekey = type_key(Type0,nohash),
	Type0 =\= Type0\/2'0100000, !,
	Type is Type0/\Type1,
	typekey <- type_key(Type,hash(Key1)),
	extract_index_2(Code0, Code1, Queue, Head),
	ins_first_unless_neck(Code1, I, Code).
extract_index(Code0, Code, Queue, Head, _) :-
	extract_index_2(Code0, Code, Queue, Head).
%	errlog:trace([code(Code)]).

:- '$ctxprj'(extract_index_2/4, []).
extract_index_2([C|Code1], Code, Queue, Head) :- C = cut(_), !,
	Code = [C,Neck|Head],
	butneck(Code1, Code2, Neck, Queue, Code3),
	Neck = neck(N),
	rest_of_chunk(Code2, Code3, N).
extract_index_2(Code0, [Neck|Head], Queue, Head) :-
	call((
          safe_insn_queue :: m_any <- Queue,
	  safe_insns(Code0, Code2, Neck),
	  ~safe_insn_queue = Code3
	)),
	!,
	Neck = neck(N),
	rest_of_chunk(Code2, Code3, N).
%	errlog:trace([queue(Queue)]),
%	errlog:trace([code0(Code0)]),
%	errlog:trace([code2(Code2)]),
%	errlog:trace([code3(Code3)]).
extract_index_2([I|Code0], [I|Code], Queue, Head) :-
	extract_index_collapse(I),
%	errlog:trace([eic(I)]),
	extract_index_2(Code0, Code, Queue, Head).

clause_lifetime(Kind, InArity, OutArity,
	        [Ensure, Neck|Code],
	        [cjump(shallow,Shallow), Neck, jump(Common), label(Shallow), label(Common), Ensure|Code]) :- Neck = neck(_), !,
        live :: m_any,
	w_in :: accum([Ensure|Code]),
	guard_lifetime(Kind, InArity, OutArity),
	trans_clause__maybe_mark_explicit_cut.
clause_lifetime(Kind, InArity, OutArity, [X,C|Code0], Code) :-
	Kind \== unit,
	guard_and_body(C, 0, Guard1, G2, Body, Code0), !,
	copy_term_shattr(G2, Guard2),
	call((
          live :: m_any,
	  w_in :: accum([X|Guard1]),
	  guard_lifetime(Kind, InArity, OutArity),
	  trans_clause__maybe_mark_explicit_cut
        )),
	% (lifetime of guard copy is ignored)
	call((
          live :: m_any,
	  w_in :: accum([X|Guard2]),
	  guard_lifetime(Kind, InArity, OutArity)
        )),
	call((
          live :: m_any,
          w_in :: accum(Body),
	  body_lifetime(0)
        )),
	compare_streams(Guard1, Guard2, Head1, [R1|Out1h], [R1|R1s], Head2, Out2h, Rest2),
	( R1=neck(_) ->
	    dlist([X|Guard1], Code, Body)
	; merge_insn_tails(R1s, Rest2, Out1h, Out1t, Out2h, Out2t, Out3h, Body, _),
	  merge_insn_streams(Out3h, Code, [X|Head1], Out1t, Head2, Out2t)
	).
clause_lifetime(Kind, InArity, OutArity, Code, Code) :-
	live :: m_any,
	w_in :: accum(Code),
	guard_lifetime(Kind, InArity, OutArity),
	trans_clause__maybe_mark_explicit_cut.

% TODO: use intseq_member(-1, Live) instead of Live=[-1|_]
{
:- fluid live :: m_any.
:- '$ctxprj'(trans_clause__maybe_mark_explicit_cut/0, [u(live), typekey]).
trans_clause__maybe_mark_explicit_cut :-
	~live = [-1|_], !,
	type_key__mark_explicit_cut. % (this clause contains an explicit cut)
trans_clause__maybe_mark_explicit_cut.
}.

:- '$ctxprj'(type_key__mark_explicit_cut/0, [typekey]).
type_key__mark_explicit_cut :-
	type_key(Type0,Key) = ~typekey, typekey <- type_key(Type,Key),
	Type is Type0\/2'1000000. % (this clause contains an explicit cut)

}.

compare_streams([X|Xs], [X|Ys], [X|P0], P1, P, [X|Q0], Q1, Q) :- !,
	compare_streams(Xs, Ys, P0, P1, P, Q0, Q1, Q).
compare_streams(Xs, Ys, P, P, Xs, Q, Q, Ys).


merge_insn_streams([I|Out3h], Out, Out1h, [I|Out1t], Out2h, [I|Out2t]) :-
	duplicatable_insn(I), !,
	merge_insn_streams(Out3h, Out, Out1h, Out1t, Out2h, Out2t).
merge_insn_streams(Out3h,
	           [cjump(shallow,Shallow)|Out1h], Out1h, [jump(Common),label(Shallow)|Out2h], Out2h, [label(Common)|Out3h]).

duplicatable_insn(un_var(_)) :- !.
duplicatable_insn(move(_, _, _)) :- !.
duplicatable_insn(move(_, _)) :- !.
duplicatable_insn(lastcall(_)) :- !.

% merge_insn_tails(In1, In2, Out1h, Out1t, Out2h, Out2t, Out3h, Out3t, X) :
% Split the two lists In1 and In2 into distinct fronts (Out1, Out2)
% and a shared tail (Out3).  length(In1)=length(In2).
merge_insn_tails([], [], O1, O1, O2, O2, O3, O3, +).
merge_insn_tails([I|Is], [J|Js], O1h, O1t, O2h, O2t, O3h, O3t, X) :-
	merge_insn_tails(Is, Js, O1a, O1t, O2a, O2t, O3a, O3t, X0),
	merge_insn_tails_2(X0, X, I, J, O1h, O1a, O2h, O2a, O3h, O3a).

merge_insn_tails_2(+, +, I, I, T1, T1, T2, T2, [I|T3], T3) :- !.
merge_insn_tails_2(_, -, I, J, [I|T1], T1, [J|T2], T2, T3, T3).

guard_and_body(Neck, Seen, [Neck|Ys], Zs, Body, [X|Xs]) :- Neck = neck(_), !,
	Seen=1,
	guard_and_body(X, Seen, Ys, Zs, Body, Xs).
guard_and_body(I, _, [I|Ys], [I|Zs], Body, [X|Xs]) :-
	I = un_var(A), xvarmem(A), !,
	guard_and_body(X, 1, Ys, Zs, Body, Xs).
guard_and_body(I, _, [I], [I], Body, Xs) :-
	xfer_insn(I, _), !,
	Body=Xs.
guard_and_body(I, Seen, [I|Ys], [I|Zs], Body, [X|Xs]) :-
	guard_and_body(X, Seen, Ys, Zs, Body, Xs).

% Extract information from code i.e. from matching first argument.

index_insn(u_cons(X,K), Type, Key) :- xvarmem2(X, 0), !,
	'$absmach'(Absmach),
	Absmach.typemask_of_functor(K/0, Type),
	key_of_constant(K, Key).
index_insn(u_blob(X,K), Type, Key) :- xvarmem2(X, 0), !,
	'$absmach'(Absmach),
	Absmach.typemask_of_functor(K/0, Type),
	key_of_constant(K, Key).
index_insn(u_str(X,S), Type, S) :- xvarmem2(X, 0),
	'$absmach'(Absmach),
	Absmach.typemask_of_functor(S, Type).

% Pre register allocation:
% If 'choice' is in x(-1) it will not be used.
% Arg regs Ai where i >= arity of first goal cannot cause conflicts.
% Collapse right away for these cases.
filter_get(move(X, Y, g), _, Q, Q) :- xvarmem2(X, -1), xvarmem2(Y, -1), !.
filter_get(move(X, Y, g), A, Q, Q) :- xvarmem2(X, R), xvarmem2(Y, R), R >= A, !.
filter_get(I, _, [I|Q], Q).

% Pre register allocation.  The following can always be
% collapsed before 'neck', because they do not involve output arg regs.
extract_index_collapse(move(A,B,p)) :-
	xvarmem(A), xvarmem(B), eqvarmem(A, B), !.
extract_index_collapse(init(A,B)) :-
	xvarmem(A), xvarmem(B), eqvarmem(A, B), !.
extract_index_collapse(_).

ins_first_unless_neck([Neck|Code], I, [Neck, I|Code]) :- Neck = neck(_), !.
ins_first_unless_neck(Code, I, [I|Code]).

butneck([I|Xs], Xs, I, S, S) :- I = neck(_), !.
butneck([I|Xs0], Xs, Neck, [I|S1], S) :-
	extract_index_collapse(I),
	butneck(Xs0, Xs, Neck, S1, S).

{
:- fluid safe_insn_queue :: accum.
safe_insns([I|Xs], Xs, I) :- I = neck(_), !.
safe_insns([I|Xs0], Xs, Neck) :-
	safe_insn(I),
	safe_insn_queue.add(I),
	safe_insns(Xs0, Xs, Neck).
}.

safe_insn(move(A,B,p)) :-
	xvarmem(A), xvarmem(B), eqvarmem(A, B), !.
safe_insn(init(A,B)) :-
	xvarmem(A), xvarmem(B), eqvarmem(A, B), !.
safe_insn(move(A,B,g)) :-
	xvarmem(A), xvarmem(B), !.
safe_insn(move(A,B)) :-
	xvarmem(A), yvarmem(B), !.
safe_insn(un_var(_)) :- !.
safe_insn(fun1(_,_,_,_,_)) :- !.
safe_insn(fun2(_,_,_,_,_,_)) :- !.
safe_insn(funre1(_,_,_,_,_)) :- !.
safe_insn(funre2(_,_,_,_,_,_)) :- !.

% Pre register allocation: move(x(X),x(A),p) can be collapsed after neck
% if A is a temporary or is >= arity of head.
rest_of_chunk(Is, Is, _) :-
	Is = [I|_],
	xfer_insn(I, _),
	!.
rest_of_chunk([I|Is], Is2, N) :-
	( I = move(A,B,p),
	  xvarmem2(A, M), xvarmem2(B, M),
	  ( var(M) ; M >= N ) ->
	    Is2 = Is3
	; Is2 = [I|Is3]
	),
	rest_of_chunk(Is, Is3, N).

key_of_constant(K, K) :- atomic(K), !.
key_of_constant(K, F/L) :- functor(K, F, L).


% Translate a clause: first head, then body.  Note whether will need env.
{
:- fluid exp :: module_exp.
:- fluid wcode :: accum.
trans_clause__1(Args, Choice, Body, Kind, InArity, OutArity) :-
	kind_of_clause(Body, Kind),
	DefaultChoice = ~my_var_new(_, x(-1)),
	movexg(DefaultChoice, Choice),
	call((
          xdic :: xreg_cache,
	  c_head_args(Args, InArity),
	  xdic.add(DefaultChoice, Choice),
	  c_guards(Kind, Body, InArity, OutArity)
        )).

:- '$ctxprj'(kind_of_clause/2, []).
kind_of_clause([], unit).
kind_of_clause([icall(_)|Gs], Kind) :- !,
	kind_of_clause(Gs, Kind).
kind_of_clause([_], iterative) :- !.
kind_of_clause([pcall(_,Size)|_], recursive(Size)).

{
:- fluid xdic :: xreg_cache.
:- '$ctxprj'(c_icall/1, [exp, wcode, u(xdic)]).
c_icall(G) :-
	trust(G instance_of strgoal),
	~G.name = Atom/Arity,
	Args = ~G.args,
	c_icall__2(Atom, Arity, Args).

:- '$ctxprj'(c_icall__2/3, [exp, wcode, u(xdic)]).
c_icall__2('basiccontrol:$caller_choice', 1, [X]) :- !,
	( X instance_of termvar -> true ; errlog:bug(['nonvar in caller_choice']), fail ),
	DefaultChoice = ~my_var_new(_, x(-1)),
	movexg(DefaultChoice, X).
c_icall__2('basiccontrol:$cut', 1, [X]) :- !,
	( X instance_of termvar -> true ; errlog:bug(['nonvar in cut']), fail ),
	% TODO: assumes that D is nonvar?
	xdic.ref(X, X2),
	wcode.add(cut(X2)).
c_icall__2('term_basic:$unify', 2, [X,Y]) :- !,
	c_eq_unify(X, Y).
c_icall__2('term_basic:$instance', 2, [X,Y]) :- !,
	c_eq_instance(X, Y).
c_icall__2(Atom, Arity, Args) :- !,
	Name = Atom/Arity, Imp = ~exp.ipred__imp(Name),
	c_icall__3(Imp, Name, Args).

:- '$ctxprj'(c_icall__3/3, [exp, wcode, u(xdic)]).
c_icall__3(det, Name, Args) :- !,
	XV = ~my_var_new(_,x(_)),
	( Args = [X, Value] ->
	    XX = ~my_var_new(_,x(_)),
	    c_put_arg_builtin(X, XX),
	    wcode.add(fun1(XV, XX, Name, _, _))
	; Args = [X, Y, Value] ->
	    XX = ~my_var_new(_,x(_)),
	    XY = ~my_var_new(_,x(_)),
	    c_put_arg_builtin(X, XX),
	    c_put_arg_builtin(Y, XY),
	    wcode.add(fun2(XV, XX, XY, Name, _, _))
	; errlog:bug(['invalid function ', Name, ' with args ', Args]), fail
	),
	% TODO: use this or create a new var? g = ~my_var_d(XV),
	c_eq_unify(~my_var_new(g,~my_var_mem(XV)), Value).
c_icall__3(semidet_re, Name, Args) :- !,
	XV = ~my_var_new(_,x(_)),
	( Args = [X, Value] ->
	    XX = ~my_var_new(_,x(_)),
	    c_put_arg_builtin(X, XX),
	    wcode.add(funre1(XV, XX, Name, _, _))
	; Args = [X, Y, Value] ->
	    XX = ~my_var_new(_,x(_)),
	    XY = ~my_var_new(_,x(_)),
	    c_put_arg_builtin(X, XX),
	    c_put_arg_builtin(Y, XY),
	    wcode.add(funre2(XV, XX, XY, Name, _, _))
	; errlog:bug(['invalid function ', Name, ' with args ', Args]), fail
	),
	% TODO: use this or create a new var? g = ~my_var_d(XV),
	c_eq_unify(~my_var_new(g,~my_var_mem(XV)), Value).
c_icall__3(semidet, Name, Args) :-
	( Args = [X] ->
	    XX = ~my_var_new(_,x(_)),
	    c_put_arg_builtin(X, XX),
	    wcode.add(blt1(XX, Name))
	; Args = [X, Y] ->
	    XX = ~my_var_new(_,x(_)),
	    XY = ~my_var_new(_,x(_)),
	    c_put_arg_builtin(X, XX),
	    c_put_arg_builtin(Y, XY),
	    wcode.add(blt2(XX, XY, Name))
	; Args = [X, Y, Z] ->
	    XX = ~my_var_new(_,x(_)),
	    XY = ~my_var_new(_,x(_)),
	    XZ = ~my_var_new(_,x(_)),
	    c_put_arg_builtin(X, XX),
	    c_put_arg_builtin(Y, XY),
	    c_put_arg_builtin(Z, XZ),
	    wcode.add(blt3(XX, XY, XZ, Name))
	; errlog:bug(['invalid builtin ', Name, ' with args ', Args]), fail
	).

% Unif. of variable and non-variable.
:- '$ctxprj'(c_eq_instance/2, [exp, wcode, u(xdic)]).
c_eq_instance(X, V) :-
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
	c_get_arg(V, XT1).

% Treat explicit unif. of two variables as a special case, because it's
% faster and because of the dangling pointer problem: 'movexg Yn, Xm'
% may place a dangling pointer in 'Yn'.

:- '$ctxprj'(c_eq_unify/2, [wcode, u(xdic)]).
c_eq_unify(U, U1) :-
	U == U1, !.
c_eq_unify(X, Y) :-
	Du = ~my_var_d(X),
	U = ~my_var_mem(X),
	Dv = ~my_var_d(Y),
	V = ~my_var_mem(Y),
	( var(Du), U=V, Du=Dv -> true
	; var(Dv), U=V, Du=Dv -> true
        ; var(Du), var(Dv) ->
            c_eq_unify__2(X, Y)
        ; var(Du) -> % nonvar(Dv)
	    xdic.ref(Y, Y2),
            c_eq_var_value(X, Y2)
        ; var(Dv) -> % nonvar(Du)
	    xdic.ref(X, X2),
            c_eq_var_value(Y, X2)
        ; xdic.ref(X, X2),
          xdic.ref(Y, Y2),
          c_eq_value_value(X2, Y2)
        ).

:- '$ctxprj'(c_eq_unify__2/2, [wcode]).
c_eq_unify__2(X, Y) :-
	J = ~my_var_d(X),
	J = ~my_var_d(Y),
	xvarmem(X), yvarmem2(Y, J),
	!,
	wcode.add(init(X,Y)).
c_eq_unify__2(X, Y) :-
	I = ~my_var_d(X),
	I = ~my_var_d(Y),
	yvarmem2(X, I), yvarmem2(Y, J),
	I < J, !,
	XT1 = ~my_var_new(_,x(_)),
	wcode.add(init(XT1,X)),
	wcode.add(move(XT1,Y)).
c_eq_unify__2(X, Y) :-
	c_eq_unify__2(Y, X).

:- '$ctxprj'(c_eq_var_value/2, [wcode]).
c_eq_var_value(X, Y) :-
	D = ~my_var_d(X),
	D = ~my_var_d(Y),
	xvarmem(X), xvarmem(Y), !, 
	XT1 = ~my_var_new(_,x(_)),
	wcode.add(init(XT1, X)),
	wcode.add(u_val(XT1, Y)).
% TODO: old code... bug in unif_move?
%	wcode.add(move(Y,X,g)).
c_eq_var_value(X, Y) :-
	D = ~my_var_d(X),
	D = ~my_var_d(Y),
	xvarmem(X), yvarmem(Y), !,
	wcode.add(move(Y,X)).
c_eq_var_value(X, Y) :-
	g = ~my_var_d(X),
	g = ~my_var_d(Y),
	yvarmem(X), xvarmem(Y), !,
	wcode.add(move(Y,X)).
c_eq_var_value(X, Y) :-
	I = ~my_var_d(X),
	yvarmem2(X, I),
	XT1 = ~my_var_new(_,x(_)),
	wcode.add(init(XT1, X)),
	wcode.add(u_val(XT1, Y)).

:- '$ctxprj'(c_eq_value_value/2, [wcode]).
c_eq_value_value(X, Y) :-
	xvarmem(X), !,
	wcode.add(u_val(X,Y)).
c_eq_value_value(X, Y) :-
	yvarmem(X), xvarmem(Y), !,
	wcode.add(u_val(Y,X)).
c_eq_value_value(X, Y) :-
	yvarmem(X), yvarmem(Y), !,
	XT1 = ~my_var_new(_,x(_)),
	wcode.add(move(X,XT1)),
	wcode.add(u_val(XT1,Y)).

% Compile the body up to first general call.
:- '$ctxprj'(c_guards/4, [exp, wcode, u(xdic)]).
c_guards(recursive(Size), [icall(G)|Gs], N0, N) :-
	trust(G instance_of strgoal),
	~G.name = NA,
	exp.ipred__uses_heap(NA), !,
	wcode.add(neck(N0)),
	wcode.add(cframe(Size)),
	c_icall(G),
	c_goals(Gs, N).
c_guards(Kind, [icall(G)|Gs], N0, N) :- !,
	c_icall(G),
	c_guards(Kind, Gs, N0, N).
c_guards(_, Gs, N0, N) :-
	wcode.add(neck(N0)),
	c_goals(Gs, N).

% Compile a tail of the body.
:- '$ctxprj'(c_goals/2, [exp, wcode, u(xdic)]).
c_goals([], 0) :-
	wcode.add(lastcall('basiccontrol:true'/0)).
c_goals([icall(G)|Gs], N) :- !,
	c_icall(G),
	c_goals(Gs, N).
c_goals([G], N) :- !,
	c_last_goal(G, N).
c_goals([G|Gs], N) :-
	c_goal(G, N),
	xdic <- _Dic, % clean the dic
	c_goals(Gs, _).

% Compile a body goal.
:- '$ctxprj'(c_goal/2, [exp, wcode, u(xdic)]).
c_goal(G, N) :-
	goal_args(G, Fu/N, Size, Args),
	call((
          size :: any <- Size,
	  c_goal_args(Args)
        )),
	wcode.add(kall(Fu/N,Size)),
	wcode.add(ensure_space(cont,0,_)).

% Compile last body goal.
:- '$ctxprj'(c_last_goal/2, [exp, wcode, u(xdic)]).
c_last_goal(G, N) :-
	goal_args(G, Fu/N, Size, Args),
	call((
          size :: any <- Size,
	  c_goal_args(Args)
        )),
	wcode.add(lastcall(Fu/N)).

:- '$ctxprj'(goal_args/4, []).
goal_args(pcall(G, Size), NA, Size, Args) :-
	trust(G instance_of strgoal),
	~G.name = NA,
	Args = ~G.args.

% Head arguments unification.  Match args that are vas first.
% This seems to need fewer temporaries than the source sequence.
:- '$ctxprj'(c_head_args/2, [exp, wcode, u(xdic)]).
c_head_args(Args, N) :-
	head_arguments(Args, 0, N, Arga),
	c_head_args_c(Arga).

:- '$ctxprj'(c_head_args_c/1, [exp, wcode, u(xdic)]).
c_head_args_c([]).
c_head_args_c([ha(X,Y)|Xs]) :-
	c_get_arg(X, Y),
	c_head_args_c(Xs).

% TODO: xreg_cache.add is quadratic here!! because it adds consecutive entries to a binary dictionary
:- '$ctxprj'(head_arguments/4, [exp, wcode, u(xdic)]).
head_arguments([], N, N, []).
head_arguments([A|As], I, N, S) :- D = ~my_var_d(A), var(D), !,
	call((
          wcode :: accum <- G0,
	  c_get_arg(A, ~my_var_new(_,x(I))),
	  G = ~wcode
        )),
	I1 is I+1,
	head_arguments(As, I1, N, S),
	G0 = ~wcode, wcode <- G.
head_arguments([A|As], I, N, [ha(A,~my_var_new(_,x(I)))|S]) :-
	I1 is I+1,
	head_arguments(As, I1, N, S).
}.

% Goal arguments unification.  Put args that are vas last.
% This seems to need fewer temporaries than the source sequence.
{
:- fluid xdic :: xreg_cache.
:- fluid size :: any.
c_goal_args(Xs) :-
	% compile nonvar arguments
	goal_arguments_nonvar(Xs, 0),
	% then compile var arguments
	goal_arguments_var(Xs, 0).

goal_arguments_nonvar([], _).
goal_arguments_nonvar([Arg|Args], I) :-
	Arg instance_of termvar,
	!,
	I1 is I+1,
	goal_arguments_nonvar(Args, I1).
goal_arguments_nonvar([Arg|Args], I) :-
	c_put_arg(Arg, ~my_var_new(_,x(I))),
	I1 is I+1,
	goal_arguments_nonvar(Args, I1).

goal_arguments_var([], _).
goal_arguments_var([A|Args], I) :-
	A instance_of termvar,
	!,
	c_put(A, ~my_var_new(_,x(I))),
	I1 is I+1,
	goal_arguments_var(Args, I1).
goal_arguments_var([_|Args], I) :-
	I1 is I+1,
	goal_arguments_var(Args, I1).
}.

}.

{
% Linearize term before emitting 'get' + 'put' + 'unify'.
:- fluid exp :: module_exp.
:- fluid flatcode :: accum.
:- fluid gp :: any.
flat(S, V) :-
	c :: m_int <- 0, % TODO: or boolean?
	n :: any <- 1,
	flat_2(S, V).
{
:- fluid c :: m_int.
:- fluid n :: any.
flat_2(S, S1) :- S instance_of termvar, !, S1 = S.
flat_2(S, S1) :-
	trust(S instance_of termstr),
	~S.arity = 0,
	!,
	( ~n = 0,
	  '$absmach'(Absmach),
	  Absmach.functorcons(~S.name, FC),
	  \+ functorcons__single_cell(FC) ->
	    c <- 1,
	    ( ~gp = put -> G = g ; true ),
	    S1 = ~my_var_new(G,x(_)),
	    flatcode.add(fla(S1,S))
	; S1 = S
	).
flat_2(S, S1) :- ~n = 0, !,
	c <- 1,
	S1 = ~my_var_new(G,x(_)),
	( ~gp = get -> flatcode.add(fla(S1,S2)), flat(S, S2) % G is left unbound
	; ~gp = put -> G = g, flat(S, S2), flatcode.add(fla(S1,S2))
	; fail
	).
flat_2(S, S1) :-
	trust(S instance_of termstr),
	L = ~S.args,
	S1 = ~S.set_args(L1),
	C0 = ~c,
	c <- 0,
	flat_args(L, L1, C0).

flat_args([X|Xs], [X1|Xs1], C1) :-
	( Xs = [] ->
	    Xs1 = [],
	    C0 = ~c,
	    N1 is ~n - C0,
	    C2 is C0\/C1,
	    c <- C2,
	    call((
              n :: any <- N1,
	      flat_2(X, X1)
            ))
	; N1 is ~n - 1,
	  call((
            n :: any <- N1,
	    flat_2(X, X1)
          )),
	  flat_args(Xs, Xs1, C1)
	).
}.
}.

% Compile matching a head argument.
% First linearize goal argument.
{
:- fluid exp :: module_exp.
:- fluid wcode :: accum.
:- fluid xdic :: xreg_cache + u.
c_get_arg(X, XV) :- X instance_of termstr, !,
	call((
          gp :: any <- get,
	  flatcode :: accum(S),
	  flat(X, S1)
        )),
	c_get(S1, XV),
	c_get_xs(S).
c_get_arg(X, XV) :-
	c_get(X, XV).

% Using X registers as a cache for Y values and constants.
c_get_xs([]).
c_get_xs([fla(S,X)|R]) :-
	c_get(X, S),
	c_get_xs(R).

c_get(S, XV) :- D = ~my_var_d(S), !,
	( var(D) ->
	    movexg(XV, S)
	; xdic.ref(S, S2),
	  wcode.add(u_val(XV, S2))
	),
	xdic.add(XV, S).
c_get(S, XV) :-
	trust(S instance_of termstr),
	S.get_functor(K, 0), !,
	( is_blob(K) ->
	    wcode.add(u_blob(XV, K))
	; wcode.add(u_cons(XV, K))
	),
	xdic.add(XV, S).
c_get(S, XV) :-
	trust(S instance_of termstr),
	NA = ~S.name,
	Args = ~S.args,
	wcode.add(u_str(XV, NA)),
	c_unify_args(Args).

c_put_arg_builtin(X, XV) :-
	% TODO: is it necessary to scope xdic?
        XDic = ~xdic,
	call((
          size :: any <- 1000,
	  xdic :: xreg_cache <- XDic,
	  c_put_arg(X, XV)
        )).

% Compile loading a goal argument.
% First linearize goal argument.
{
:- fluid exp :: module_exp.
:- fluid wcode :: accum.
:- fluid xdic :: xreg_cache.
:- fluid size :: any.
c_put_arg(X, XV) :- X instance_of termstr, !,
	call((
          gp :: any <- put,
	  flatcode :: accum(S),
	  flat(X, S1)
        )),
	c_put_xs(S),
	c_put(S1, XV).
c_put_arg(X, XV) :-
	c_put(X, XV).

c_put_xs([]).
c_put_xs([fla(S,X)|R]) :- c_put(X, S), c_put_xs(R).

c_put(S, XV) :- S instance_of termvar, !,
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
c_put(S, XV) :-
	trust(S instance_of termstr),
	S.get_functor(K, 0), !,
	( xdic.search(S, XN) ->
	    wcode.add(move(~my_var_new(_,XN), XV, p))
	; is_blob(K) ->
	    wcode.add(ld_blob(XV, K))
	; wcode.add(ld_cons(XV, K))
	),
	xdic.store(XV, S). 
c_put(S, XV) :-
	trust(S instance_of termstr),
	NA = ~S.name,
	Args = ~S.args,
	wcode.add(ld_str(XV, NA)),
	xdic.clear(~my_var_mem(XV)),
	c_unify_args(Args).
}.
}.

is_blob(K) :-
	'$absmach'(Absmach),
	( module_ipexp:is_float(K) ->
	    true
	; Absmach.is_bignum(K) ->
	    true
	).

{
:- fluid wcode :: accum.
:- fluid xdic :: xreg_cache + u.
c_unify_args([]).
c_unify_args([X|Xs]) :- c_unify(X), c_unify_args(Xs).

c_unify(S) :- S instance_of termvar, !,
	D = ~my_var_d(S),
	( var(D) ->
	    D = g,
	    wcode.add(un_var(S))
	; xdic.ref(S, S2),
	  c_unify__un_val(S2)
	).
c_unify(S) :-
	trust(S instance_of termstr),
	S.get_functor(K, 0), !,
	( is_blob(K) ->
	    wcode.add(un_blob(K))
	; wcode.add(un_cons(K))
	).
c_unify(S) :- !,
	trust(S instance_of termstr),
	NA = ~S.name,
	Args = ~S.args,
	wcode.add(un_str(NA)),
	c_unify_args(Args).
}.

:- class xreg_cache {
    :- '$statemodel'(pair). % note: this module manages self directly
    :- '$raw_state'.

    % A binary tree to map X registers to constants and variables
    %
    % This is used to reuse X registers when possible (that is cheaper than
    % creating new constants and loading variables).
    % TODO: this should be a class

    % TODO: missing instance_of__/1

    :- constant add/2.
    add(X, S) :-
            x(V) = ~my_var_mem(X), integer(V), !, % S may be a var or 0-ary str
            dic_lookup(~self, V, S).
    add(_, _).

    :- constant search/2.
    search(Val, x(K)) :- dic_node(~self, dic(K,Val,_,_)).

    store(U, V) :- xvarmem2(U, K), integer(K), !,
            Cache0 = ~self, self <- Cache,
            dic_replace(Cache0, K, V, Cache).
    store(_, _).

    clear(x(K)) :- integer(K), !,
            Cache0 = ~self, self <- Cache,
            dic_replace(Cache0, K, void, Cache).
    clear(_).

    :- constant ref/2.
    ref(X, X2) :-
            Ref = ~my_var_mem(X),
            search(Val, XK),
            Ref0 = ~my_var_mem(Val),
            Ref==Ref0, !,
            D1 = ~my_var_d(Val),
            X2 = ~my_var_new(D1, XK).
    ref(X, X).
}.

{
:- fluid wcode :: accum.

{
:- fluid size :: any.
c_put_value(X, XV) :-
        ( I = ~my_var_d(X),
          integer(I), I >= ~size ->
            wcode.add(globunsafe(X,XV))
        ; movexp(X, XV)
        ).
}.

movexp(X, Y) :-
	( xvarmem(X) ->
	    wcode.add(move(X,Y,p))
	; wcode.add(move(X,Y))
	).

c_unify__un_val(X) :-
	( g = ~my_var_d(X) ->
	    wcode.add(un_val(X))
	; % local value
	  wcode.add(un_lval(X))
	).

:- '$ctxprj'(is_movexg/1, []).
is_movexg(move(A,B,g)) :- xvarmem(A), xvarmem(B).
is_movexg(move(A,B)) :- xvarmem(A), yvarmem(B).

movexg(X, Y) :-
	r = ~my_var_d(Y),
	( xvarmem(Y) ->
	    wcode.add(move(X,Y,g))
	; wcode.add(move(X,Y))
	).
}.

%-----------------------------------------------------------------------------
% Lifetime analysis and allocation of X registers.  The first chunk does
% steps 1-4.  Subsequent chunks do step 4 only.

% 1. Make a dictionary of all variables involved in moves (Map).
% 2. Fill in Def-LastUse for temporaries and LastInargUse-OutargDef
%    for argument registers, by scanning the chunk backwards.
% 3. Consider one move at a time.  Collapse it if the temporary and
%    arg reg have disjoint lifetimes.  If they have, update the DU info
%    of the arg reg.
% 4. Traverse chunk backward and assign free arg regs for yet unallocated
%    temporaries.  Maintain a list of live arg regs during traversal.
{
:- fluid exp :: module_exp.
:- fluid w_in :: accum.
:- fluid live :: m_any + d.
guard_lifetime(unit, InArity, _) :- !,
	body_lifetime(InArity).
guard_lifetime(_, InArity, OutArity) :-
	w_in.add(ensure_space(_,_,Heap)),
	w_in.add(Insn),
	call((
          map :: u_dic,
	  call(( dulist :: accum(DUreverse), guard_def_use_list(Insn, Moves, Heap) )),
	  lifetime_map(DUreverse)
        )),
	guard_collapse_moves(Moves),
	intset_sequence(OutArity, [], Live0),
	live <- Live0,
	call((
          max :: any <- InArity,
	  guard_allocate(DUreverse)
        )),
        call((
	  live :: any,
	  body_lifetime(0)
        )).
}.

{
:- fluid exp :: module_exp.
:- fluid w_in :: accum.
:- fluid dulist :: accum.
:- fluid map :: u_dic.
guard_def_use_list(I, Moves, Heap) :- xfer_insn(I, _), !,
	Moves=[],
	Heap=0.
guard_def_use_list(I, Moves0, Heap) :-
	w_in.add(I1),
	guard_def_use_list(I1, Moves, Heap0),
	x_def_use(I, DU),
	Heap1 = ~(bytecode__compiler:ins_heap(I)),
	insn_heap_usage(I, Heap0),
	Heap is Heap0+Heap1,
	call((
          moves :: accum <- Moves0,
	  guard_def_use(DU),
	  ~moves = Moves
        )).
}.

{
:- fluid dulist :: accum.
:- fluid map :: u_dic.
:- fluid moves :: accum.
guard_def_use(x_d0u0) :- !.
guard_def_use(DU) :- DU = x_move(T,R), !,
	compare(C, T, R),
	( C = (<), var(T) ->
	    dulist.add(DU),
	    moves.add(alias(T,R,Titem,Ritem)),
	    map.lookup(T, Titem),
	    map.lookup(R, Ritem)
	; C = (>), var(R) ->
	    dulist.add(DU),
	    moves.add(alias(R,T,Ritem,Titem)),
	    map.lookup(R, Ritem),
	    map.lookup(T, Titem)
	; C = (=) ->
	    true
	; dulist.add(DU)
	).
guard_def_use(DU) :-
	dulist.add(DU).
}.

{
:- fluid live :: m_any.
:- fluid max :: any.
guard_allocate([]).
guard_allocate([DU|DUlist]) :-
	simple_x_alloc(DU),
	guard_allocate(DUlist).
}.

guard_collapse_moves([]).
guard_collapse_moves([alias(T,R,Titem,Ritem)|Moves]) :-
	( var(T), check_eq(Titem, Ritem) ->
	    T=R
        ; true
	),
	guard_collapse_moves(Moves).

check_eq(T, _) :- var(T), !.
% check_eq(T1-T1, _) :- !.
check_eq(T1-T2, [Rhead|Rtail]) :-
	check_eq_last(Rtail, Rhead, R1-R2, [S1-S2|_]),
	check_eq_2(T1, T2, R1, R2, S1, S2).

check_eq_2(A, B, void, B, _, A) :- !.
check_eq_2(A, B, A, C, B, C) :- B =< C, !.
check_eq_2(B, C, A, C, A, B) :- A =< B.

check_eq_last(Tail0, Head0, Head, Tail) :- var(Tail0), !,
	Head0=Head,
	Tail0=Tail.
check_eq_last([Head0|Tail0], _, Head, Tail) :-
	check_eq_last(Tail0, Head0, Head, Tail).

{
:- fluid map :: u_dic.
lifetime_map(_) :- map.is_empty, !.
lifetime_map(DUs) :-
	i :: m_int <- 0,
	lifetime_map_2(DUs).
{
:- fluid i :: m_int + u.
lifetime_map_2([]).
lifetime_map_2([x_d0u0|DUs]) :- !, lifetime_map_2(DUs).
lifetime_map_2([DU|DUs]) :-
	lifetime_map_3(DU),
	i.dec(1),
	lifetime_map_2(DUs).

lifetime_map_3(x_d0u1(A)) :- lifetime_map_u(A).
lifetime_map_3(x_d0u2(A,B)) :- lifetime_map_u(A), lifetime_map_u(B).
lifetime_map_3(x_d0u3(A,B,C)) :- lifetime_map_u(A), lifetime_map_u(B), lifetime_map_u(C).
lifetime_map_3(x_d0un(A)) :- lifetime_map_un(0, A).
lifetime_map_3(x_d1u0p(A)) :- lifetime_map_d(A).
lifetime_map_3(x_d1u0(A)) :- lifetime_map_d(A).
lifetime_map_3(x_move(A,B)) :- lifetime_map_move(A, B).
lifetime_map_3(x_d1u1live(A,B,_)) :- lifetime_map_d(A), lifetime_map_u(B).
lifetime_map_3(x_d1u2live(A,B,C,_)) :- lifetime_map_d(A), lifetime_map_u(B), lifetime_map_u(C).
lifetime_map_3(x_d2u0p(A,B)) :- lifetime_map_d(A), lifetime_map_d(B).

lifetime_map_move(Def, _) :-
	var(Def),
	map.get(Def, Dint),
	var(Dint), !.
lifetime_map_move(Def, Use) :- lifetime_map_d(Def), lifetime_map_u(Use).

lifetime_map_d(A) :-
	map.get(A, Int), !,
	lifetime_map_d1(A, ~i, Int).
lifetime_map_d(_).

:- '$ctxprj'(lifetime_map_d1/3, []).
lifetime_map_d1(A, I, I-_) :- var(A), !.
lifetime_map_d1(_, I, [_-I|_]).

lifetime_map_u(A) :-
	map.get(A, Int), !,
	lifetime_map_u1(A, ~i, Int).
lifetime_map_u(_).

:- '$ctxprj'(lifetime_map_u1/3, []).
lifetime_map_u1(A, I, _-I) :- var(A), !.
lifetime_map_u1(A, I, [I-J|_]) :- nonvar(A), nonvar(J), !.
lifetime_map_u1(_, _, _).

lifetime_map_un(A, A) :- !.
lifetime_map_un(A0, A) :-
	map.get(A0, Int), !,
	lifetime_map_u1(A0, ~i, Int),
	A1 is A0+1,
	lifetime_map_un(A1, A).
lifetime_map_un(A0, A) :-
	A1 is A0+1,
	lifetime_map_un(A1, A).
}.
}.

{
:- fluid exp :: module_exp.
:- fluid w_in :: accum.
:- fluid live :: m_any + d.
body_lifetime(InArity) :-
	w_in.add(ensure_space(Kind, _, Heap)),
	w_in.add(I), !,
	body_lifetime_2(I, InArity, Kind, Heap),
	call((
          live :: m_any,
          body_lifetime(0)
        )).
body_lifetime(_) :-
	live <- [].

body_lifetime_2(move(A,B,p), InArity, Kind, Heap) :-
	xvarmem(A), xvarmem(B), eqvarmem(A, B), !,
	w_in.add(I1),
	body_lifetime_2(I1, InArity, Kind, Heap).
body_lifetime_2(lastcall('basiccontrol:true'/0), _, unit, Heap) :- !,
	'$absmach'(Absmach),
	Absmach.heap_pad_size(cont, Heap),
	live <- [].
body_lifetime_2(I, _, _, 0) :- xfer_insn(I, A), !,
	intset_sequence(A, [], Live),
	live <- Live.
body_lifetime_2(I, InArity, Kind, Heap) :-
	w_in.add(I1),
	body_lifetime_2(I1, InArity, Kind, Heap0),
	x_def_use(I, DU),
	insn_heap_usage(I, Heap0),
	Heap1 = ~(bytecode__compiler:ins_heap(I)),
	Heap is Heap0+Heap1,
	call((
          max :: any <- InArity,
	  simple_x_alloc(DU)
        )).
}.

{
:- fluid live :: m_any.
:- fluid max :: any.
simple_x_alloc(x_d0un(0)) :- !.
simple_x_alloc(x_d0un(N)) :- N1 is N-1, x_alloc(N1), simple_x_alloc(x_d0un(N1)).
simple_x_alloc(x_d0u0).
simple_x_alloc(x_d0u1(A)) :- x_alloc(A).
simple_x_alloc(x_d0u2(A,B)) :- x_alloc(A), x_alloc(B).
simple_x_alloc(x_d0u3(A,B,C)) :- x_alloc(A), x_alloc(B), x_alloc(C).
simple_x_alloc(x_d1u0(X)) :- delete_check(X).
simple_x_alloc(x_move(X,A)) :- x_alloc_move(X, A).
simple_x_alloc(x_d1u1live(X,A,Set)) :- delete_check(X), x_alloc(A), Set = ~live.
simple_x_alloc(x_d1u2live(X,A,B,Set)) :- delete_check(X), x_alloc(A), x_alloc(B), Set = ~live.
simple_x_alloc(x_d1u0p(X)) :- delete_check_p(X).
simple_x_alloc(x_d2u0p(X,Y)) :- delete_check_p(X), delete_check_p(Y).

delete_check(X) :-
	nonvar(X),
	Set0 = ~live, live <- Set,
	intset_delete(Set0, X, Set), !.
delete_check(X) :-
	Live = ~live,
	% TODO: why throwing modified live?
	call((
          live :: m_any <- Live,
	  x_alloc(X)
        )).

:- '$ctxprj'(delete_check_p/1, [live]).
delete_check_p(X) :-
	nonvar(X),
	Set0 = ~live, live <- Set,
	intset_delete(Set0, X, Set), !.
delete_check_p(_).

x_alloc_move(X, A) :-
	nonvar(X),
	Set0 = ~live, live <- Set1,
	intset_delete(Set0, X, Set1), !,
	x_alloc(A).
x_alloc_move(_, _).

x_alloc(X) :- 
	var(X), !, x_free_arg(X).
x_alloc(X) :-
	nonvar(X),
	Set0 = ~live, live <- Set,
	intset_insert(Set0, X, Set).

% x_free_arg(I)
%   I is the smallest integer such that ~max =< I and I is not in the liveset
%  Then, adds I to the liveset.
x_free_arg(I) :-
	Set0 = ~live, live <- Set,
	x_free_arg_(Set0, ~max, I, Set).

% TODO: I would need ctx var expanded in is first argument here
:- '$ctxprj'(x_free_arg_/4, []).
x_free_arg_([], I0, I, Set) :-
	!,
	I=I0,
	Set=[I0].
x_free_arg_(Set0, I0, I, Set) :-
	Set0=[X|_],
	X>I0, !,
	I=I0,
	Set=[I0|Set0].
x_free_arg_([X|Xs], I0, I, Set) :-
	X<I0, !,
	Set=[X|Set1],
	x_free_arg_(Xs, I0, I, Set1).
x_free_arg_(Set0, I0, I, Set) :-
	I1 is I0+1,
	x_free_arg_(Set0, I1, I, Set).
}.

% What X registers does an instruction use or ?
x_def_use(neck(N), x_d0un(N)) :- !.
x_def_use(cut(X), x_d0u1(A)) :- xvarmem2(X, A), !.
x_def_use(fun1(A,B,_,_,L), x_d1u1live(V,X,L)) :- xvarmem2(A, V), xvarmem2(B, X), !.
x_def_use(fun2(A,B,C,_,_,L), x_d1u2live(V,X,Y,L)) :- xvarmem2(A, V), xvarmem2(B, X), xvarmem2(C, Y), !.
x_def_use(funre1(A,B,_,_,L), x_d1u1live(V,X,L)) :- xvarmem2(A, V), xvarmem2(B, X), !.
x_def_use(funre2(A,B,C,_,_,L), x_d1u2live(V,X,Y,L)) :- xvarmem2(A, V), xvarmem2(B, X), xvarmem2(C, Y), !.
x_def_use(blt1(A,_), x_d0u1(X)) :- xvarmem2(A, X), !.
x_def_use(blt2(A,B,_), x_d0u2(X,Y)) :- xvarmem2(A, X), xvarmem2(B, Y), !.
x_def_use(blt3(A,B,C,_), x_d0u3(X,Y,Z)) :- xvarmem2(A, X), xvarmem2(B, Y), xvarmem2(C, Z), !.
x_def_use(move(X,Y,_), x_move(V,A)) :- xvarmem2(X, A), xvarmem2(Y, V), !.
x_def_use(move(X,_), x_d0u1(A)) :- xvarmem2(X, A), !.
x_def_use(move(_,Y), x_d1u0(A)) :- xvarmem2(Y, A), !.
x_def_use(u_val(X,Y), x_d0u2(V,A)) :- xvarmem2(X, A), xvarmem2(Y, V), !.
x_def_use(u_val(X,_), x_d0u1(A)) :- xvarmem2(X, A), !.
x_def_use(u_cons(X,_), x_d0u1(A)) :- xvarmem2(X, A), !.
x_def_use(u_blob(X,_), x_d0u1(A)) :- xvarmem2(X, A), !.
x_def_use(u_str(X,_), x_d0u1(A)) :- xvarmem2(X, A), !.
x_def_use(init(X,Y), x_d2u0p(V,A)) :- xvarmem2(X, A), xvarmem2(Y, V), !.
x_def_use(init(X,_), x_d1u0(A)) :- xvarmem2(X, A), !.
x_def_use(globunsafe(X,Y), x_d1u1live(V,A,_)) :- xvarmem2(X, A), xvarmem2(Y, V), !.
x_def_use(globunsafe(_,Y), x_d1u0(A)) :- xvarmem2(Y, A), !.
x_def_use(ld_cons(X,_), x_d1u0(A)) :- xvarmem2(X, A), !.
x_def_use(ld_blob(X,_), x_d1u0(A)) :- xvarmem2(X, A), !.
x_def_use(ld_str(X,_), x_d1u0(A)) :- xvarmem2(X, A), !.
x_def_use(un_var(A), x_d1u0p(V)) :- xvarmem2(A, V), !.
x_def_use(un_val(A), x_d0u1(V)) :- xvarmem2(A, V), !.
x_def_use(un_lval(A), x_d0u1(V)) :- xvarmem2(A, V), !.
x_def_use(_, x_d0u0).

{
:- fluid exp :: module_exp.
ins_heap(blt1(_,Name)) := H :- !,
	builtin_heap_usage(Name, H).
ins_heap(blt2(_,_,Name)) := H :- !,
	builtin_heap_usage(Name, H).
ins_heap(blt3(_,_,_,Name)) := H :- !,
	builtin_heap_usage(Name, H).
ins_heap(init(A,B)) := H :- xvarmem(A), xvarmem(B), !,
	'$absmach'(Absmach),
	Absmach.tagged_size(H).
ins_heap(globunsafe(_,_)) := H :- !,
	'$absmach'(Absmach),
	Absmach.tagged_size(H).
ins_heap(u_blob(_,C)) := H :- !,
	'$absmach'(Absmach),
	Absmach.functorcons(C/0, FC),
	Absmach.functorcons__heap_usage(FC, H).
ins_heap(u_str(_,FA)) := H :- !,
	'$absmach'(Absmach),
	Absmach.functorcons(FA, FC),
	Absmach.functorcons__heap_usage(FC, H).
ins_heap(ld_blob(_,C)) := H :- !,
	'$absmach'(Absmach),
	Absmach.functorcons(C/0, FC),
	Absmach.functorcons__heap_usage(FC, H).
ins_heap(ld_str(_,FA)) := H :- !,
	'$absmach'(Absmach),
	Absmach.functorcons(FA, FC),
	Absmach.functorcons__heap_usage(FC, H).
ins_heap(un_blob(C)) := H :- !,
	'$absmach'(Absmach),
	Absmach.functorcons(C/0, FC),
	Absmach.functorcons__heap_usage(FC, H).
ins_heap(un_str(FA)) := H :- !,
	'$absmach'(Absmach),
	Absmach.functorcons(FA, FC),
	Absmach.functorcons__heap_usage(FC, H).
ins_heap(_) := 0.

builtin_heap_usage(Name, H) :-
	H0 = ~exp.ipred__heap(Name),
	'$absmach'(Absmach),
	Absmach.tagged_size(S),
	H is H0 * S.
}.

% Which instructions have operands encoding heap requirements?
insn_heap_usage(fun1(_,_,_,H,_), H) :- !.
insn_heap_usage(fun2(_,_,_,_,H,_), H) :- !.
insn_heap_usage(funre1(_,_,_,H,_), H) :- !.
insn_heap_usage(funre2(_,_,_,_,H,_), H) :- !.
insn_heap_usage(_, _).

% What instructions jump?
% TODO: look in ptoc__lowcomp for the correct name of this property (there are several props)
xfer_insn(kall(_/A,_), A).
xfer_insn(lastcall(_/A), A).

%-----------------------------------------------------------------------------
% Final pass over entire compiled code to emit 'alloc', 'dealloc',
% and cut related instructions.

peep_clause(Code0) := Code :-
	% TODO: dic seems to be unused!
	xdic :: xreg_cache,
	env :: m_any <- noenv,
	wcode :: accum(Code),
	peep_xs(Code0).
{
    :- fluid wcode :: accum.
    :- fluid xdic :: xreg_cache.
    :- fluid env :: m_any.

    :- '$ctxprj'(peep_xs/1, [wcode, xdic, u(env)]).
    % TODO: u(env) because the final output does not matter... not very clean anyway
    peep_xs([]).
    peep_xs([Insn|Insns]) :- peep_1(Insn), peep_xs(Insns).

    peep_1(jump(Label)) :- !, env <- noenv, wcode.add(jump(Label)).
    peep_1(cut(X)) :- ~env = noenv, xvarmem(X), !, wcode.add(cutb(X)).
    peep_1(cut(X)) :- ~env = env(_,_), xvarmem(X), !, wcode.add(cute(X)).
    peep_1(cut(X)) :- ~env = env2(_,_), !, wcode.add(cutf(X)).
    peep_1(cut(X)) :- ~env = env(_), !, wcode.add(cutf(X)).
    peep_1(kall(U,A)) :- !, ensure_call(A), wcode.add(kall(U,A)).
    peep_1(cframe(A)) :- !, ensure_call(A), wcode.add(cframe(A)).
    % TODO: THIS MAKES THE TEST FAIL!
    % peep_1(lastcall('basiccontrol:true'/0)) :- !, ensure_no_env, wcode.add(proceed).
    % peep_1(lastcall('basiccontrol:fail'/0)) :- !, ensure_no_env, wcode.add(failins).
    peep_1(lastcall('basiccontrol:true'/0)) :- ~env = noenv, !, wcode.add(proceed).
     % TODO: why fail? true/0 is necessary to catch heap overflows (see the tests/heap_proceed test) but fail/0 
    peep_1(lastcall('basiccontrol:fail'/0)) :- ~env = noenv, !, wcode.add(failins).
    peep_1(lastcall(U)) :- !, ensure_no_env, wcode.add(lastcall(U)).
    peep_1(init(A,B)) :- xvarmem2(A, void), xvarmem2(B, void), !.
    peep_1(init(X,Y)) :- xvarmem(X), xvarmem(Y), eqvarmem(X, Y), !, wcode.add(inith(X)).
    peep_1(init(X,Y)) :- ~env = env(_), xvarmem(X), yvarmem(Y), !, wcode.add(move(Y,X)).
    peep_1(init(X,Y)) :- xvarmem(X), yvarmem2(Y, B), !, ensure_env(B), wcode.add(init(X,Y)).
    peep_1(un_var(A)) :- xvarmem2(A, void), !, wcode.add(un_void).
    peep_1(un_var(X)) :- ~env = env(_), yvarmem2(X, B), !, wcode.add(un_fval(y(B))).
    peep_1(un_var(X)) :- yvarmem2(X, B), !, ensure_env(B), wcode.add(un_var(X)).
    peep_1(move(X,Y,_)) :- xvarmem(X), xvarmem(Y), eqvarmem(X, Y), !.
    peep_1(move(X,Y,_)) :- xvarmem2(X, -1), xvarmem(Y), !, wcode.add(getchoice(Y)).
    peep_1(move(X,Y)) :- ~env = env(_), xvarmem2(X, A), yvarmem(Y), A \== -1, !, wcode.add(u_fval(X,Y)).
    peep_1(move(X,Y)) :- xvarmem2(X, -1), yvarmem2(Y, U), !, ensure_env(U), wcode.add(getchoice(Y)).
    peep_1(move(X,Y)) :- xvarmem(X), yvarmem2(Y, B), !, ensure_env(B), wcode.add(move(X,Y)).
    peep_1(ensure_space(Kind,EffAr,H)) :- !, c_ensure_space(Kind, EffAr, H).
    peep_1(Insn) :- peep_common(Insn).

    :- '$ctxprj'(peep_common/1, [wcode]).
    peep_common(fun1(XB,XC,A,H,Set)) :- !,
            live_arity(Set, 0, Arity),
            wcode.add(fun1(XB,XC,A,liveinfo(H,Arity))).
    peep_common(fun2(XB,XC,XD,A,H,Set)) :- !,
            live_arity(Set, 0, Arity),
            wcode.add(fun2(XB,XC,XD,A,liveinfo(H,Arity))).
    peep_common(funre1(XB,XC,A,H,Set)) :- !,
            live_arity(Set, 0, Arity),
            wcode.add(funre1(XB,XC,A,liveinfo(H,Arity))).
    peep_common(funre2(XB,XC,XD,A,H,Set)) :- !,
            live_arity(Set, 0, Arity),
            wcode.add(funre2(XB,XC,XD,A,liveinfo(H,Arity))).
    peep_common(Insn) :- wcode.add(Insn).

    :- '$ctxprj'(live_arity/3, []).
    live_arity([], X, X) :- !.
    live_arity([X|Xs], _, Y) :- X1 is X+1, live_arity(Xs, X1, Y).

    :- '$ctxprj'(c_ensure_space/3, [wcode]).
    c_ensure_space(Kind, EffAr, Size) :-
            '$absmach'(Absmach), 
            ( Absmach.heap_pad_size(Kind, Heap), Size>Heap ->
                wcode.add(heapmargin_call(Size, EffAr))
            ; true
            ).

    % noenv: no environment created
    % env(Size,Ys): environment of size Size created, before the first call, where Ys is the list of initialized Y entries
    % env2(Size,Ys): environment of size Size created, after the first call, where Ys is the list of initialized Y entries
    % env(Size): environment of size Size created, after the first call, where all the Y entries are initialized

    :- '$ctxprj'(ensure_env/1, [wcode, env]).
    ensure_env(Y) :- Env0 = ~env, env <- Env, ensure_env_(Env0, Env, Y).

    :- '$ctxprj'(ensure_env_/3, [wcode]).
    ensure_env_(noenv, env(_,Ys), Y) :-
            wcode.add(alloc),
            Set0=[],
            intset_insert(Set0, Y, Ys).
    ensure_env_(env(Size), env(Size), _).
    ensure_env_(env(Size,Ys), env(Size,Ys1), Y) :-
            intset_insert(Ys, Y, Ys1).
    ensure_env_(env2(Size,Ys), env2(Size,Ys1), Y) :-
            intset_insert(Ys, Y, Ys1).

    ensure_call(Size) :- Env0 = ~env, env <- Env, ensure_call_(Env0, Env, Size).

    :- '$ctxprj'(ensure_call_/3, [wcode, xdic]).
    ensure_call_(noenv, env2(_,_), _Size) :-
            '$absmach'(Absmach),
            Absmach.use_opt(uninit_y), % allow frames with uninitialized Y entries
            !,
            wcode.add(alloc),
            wcode.add(init([])).
    ensure_call_(noenv, env(_), Size) :-
            wcode.add(alloc),
            init_ys(Size, []).
    ensure_call_(env(Size), env(_), Size).
    ensure_call_(env2(Size,Ys), env2(_,Ys), Size).
    ensure_call_(env(Size,Ys), env2(_,Ys), Size) :-
            '$absmach'(Absmach),
            Absmach.use_opt(uninit_y), % allow frames with uninitialized Y entries
            !,
            wcode.add(init([])). % TODO: remove by modifying the instruction set, if it works...
    ensure_call_(env(Size,Ys), env(_), Size) :-
            init_ys(Size, Ys). % initialize 

    :- '$ctxprj'(ensure_no_env/0, [wcode, env]).
    ensure_no_env :- Env0 = ~env, env <- Env, ensure_no_env_(Env0, Env).

    :- '$ctxprj'(ensure_no_env_/2, [wcode]).
    ensure_no_env_(noenv, noenv).
    ensure_no_env_(env(-1), noenv) :- wcode.add(dealloc).
    ensure_no_env_(env2(-1,_), noenv) :- wcode.add(dealloc).

    :- '$ctxprj'(init_ys/2, [wcode, xdic]).
    init_ys(Size, Ys) :-
            init_list(0, Size, Ys, L, ~xdic),
    %        { errlog:trace([initlist(L)]) },
            wcode.add(init(L)).

    :- '$ctxprj'(init_list/5, []).
    init_list(Max, Max, _, [], _) :- !.
    init_list(Y, Max, Perms, Ys, Dic) :-
            intset_in(Y, Perms), !,
            Y1 is Y+1,
            init_list(Y1, Max, Perms, Ys, Dic).
    init_list(Y, Max, Perms, [Y|Ys], Dic) :-
            Y1 is Y+1,
            init_list(Y1, Max, Perms, Ys, Dic).
}.

% ---------------------------------------------------------------------------
% post processing and collapsing of WAM code

{
:- fluid bcode :: m_any.
postcomp :-
	'$absmach'(Absmach),
	( Absmach.use_opt(varops) -> voidcompact ; true ),
	( Absmach.use_opt(varops) -> precollapse ; true ),
	remove_useless_cut,
	precollapse_init,
	( Absmach.use_opt(varops) -> true ; expand_init ),
	move_alloc,
	remove_useless_insargs,
	opt_jump.
}.

% ---------------------------------------------------------------------------
% Compact un_void to un_voidr(Repeat)

{
:- fluid bcode :: m_any.
voidcompact :-
	Insns0 = ~bcode,
	call(( wcode :: accum(Insns), voidcompact__1(Insns0) )),
	bcode <- Insns.
}.

{
:- fluid wcode :: accum.
voidcompact__1([]) :- !.
voidcompact__1([Insn|Insns]) :-
	( Insn = un_void -> % one void
	    voidcompact__void(Insns, 1)
	; wcode.add(Insn),
	  voidcompact__1(Insns)
	).

voidcompact__void(Insns0, X) :-
	( Insns0 = [un_void|Insns] -> % another void
	    X1 is X + 1, voidcompact__void(Insns, X1)
	; % emit voids and start again
	  wcode.add(un_voidr(X)),
	  voidcompact__1(Insns0)
	).
}.

% ---------------------------------------------------------------------------
% Emit zputn instruction 
% TODO: improve documentation and change name

{
:- fluid bcode :: m_any.
precollapse :-
	~bcode = Insns0,
	call(( wcode :: accum(Insns), precollapse__1(Insns0) )),
	bcode <- Insns.
}.

{
:- fluid wcode :: accum.
precollapse__1([]) :- !.
precollapse__1([Insn|Insns0]) :-
	precollapse__ins(Insn), !,
	call((
	  pending :: accum(L),
	  pending.add(Insn),
	  precollapse__z(Insns0, Insns)
	)),
	% emit collected insns
	( precollapse__stop(Insns),
	  call((
            i :: m_int <- 0,
	    zarg :: revaccum(Arg),
	    precollapse_call_1(L, L1)
          )) ->
            % TODO: doc
	    precollapse_call_2(L1),
            % TODO: doc
	    ( Arg = [] -> true ; wcode.add(zputn(Arg)) )
	; % TODO: doc
	  precollapse_call_2(L)
	),
	precollapse__1(Insns).
precollapse__1([Insn|Insns]) :-
	wcode.add(Insn),
	precollapse__1(Insns).
}.

{
:- fluid pending :: accum.
precollapse__z([Insn|Insns], Rest) :-
	precollapse__ins(Insn), !, 
        % collect insn
        pending.add(Insn),
	precollapse__z(Insns, Rest).
precollapse__z(Insns, Insns).
}.

precollapse__ins(inith(A)) :- xvarmem(A).
precollapse__ins(init(A,B)) :- xvarmem(A), xvarmem(B).
precollapse__ins(move(A,B)) :- yvarmem(A), xvarmem(B).
precollapse__ins(globunsafe(A,B)) :- yvarmem(A), xvarmem(B).

% TODO: why?
precollapse__stop([kall(_,_)|_]).
precollapse__stop([dealloc, lastcall(_)|_]).

{
:- fluid i :: m_int.
:- fluid zarg :: revaccum.
% TODO: increment i, but i is readonly... this is because it is tail recursive and i is not used from the caller positions
:- '$ctxprj'(precollapse_call_1/2, [u(i), zarg]).
precollapse_call_1([], []).
precollapse_call_1([Insn|S0], S) :-
	precollapse_call_1_1(Insn, U), !,
	zarg.insert0(U),
	i.inc(1),
	precollapse_call_1(S0, S).
precollapse_call_1([Insn|S0], [Insn|S]) :-
	precollapse_call_1_2(Insn),
	precollapse_call_1(S0, S).

:- '$ctxprj'(precollapse_call_1_1/2, [u(i)]).
precollapse_call_1_1(move(A,B), v(Y)) :-
	I = ~i, xvarmem2(B, I),
	yvarmem2(A, Y).
precollapse_call_1_1(globunsafe(A,B), u(Y)) :-
	I = ~i, xvarmem2(B, I),
	yvarmem2(A, Y).

:- '$ctxprj'(precollapse_call_1_2/1, [u(i)]).
precollapse_call_1_2(move(A,B)) :- yvarmem(A), xvarmem2(B, X), I = ~i, X >= I.
precollapse_call_1_2(globunsafe(A,B)) :- yvarmem(A), xvarmem2(B, X), I = ~i, X >= I.
precollapse_call_1_2(inith(A)) :- xvarmem2(A, X), I = ~i, X >= I.
precollapse_call_1_2(init(A, B)) :- xvarmem2(A, X1), xvarmem2(B, X), I = ~i, X >= I, X1 >= I.
}.

{
:- fluid wcode :: accum.
precollapse_call_2([X|Xs]) :- wcode.add(X), precollapse_call_2(Xs).
precollapse_call_2([]).
}.

% ---------------------------------------------------------------------------
% cutb or cute followed by anything different from neck(_) ===> (removed)
% cutb, neck ===> cutb
% cute, neck ===> cute

{
:- fluid bcode :: m_any.
remove_useless_cut :-
	bcode <- ~remove_useless_cut_(~bcode).
}.

% TODO: change name
remove_useless_cut_([], []).
remove_useless_cut_([Cut, neck(_)|Xs0], [Cut|Xs]) :-
	( Cut = cutb(A), xvarmem(A) ; Cut = cute(A), xvarmem(A) ), !,
	% remove the neck after cut
	% TODO: continue or end?
	remove_useless_cut_(Xs0, Xs).
remove_useless_cut_([Cut|Xs0], Xs) :-
	( Cut = cutb(A), xvarmem2(A, -1) ; Cut = cute(A), xvarmem2(A, -1) ), % TODO: why not cutb(x(_)) and cute(x(_))? why? ... maybe a default cut not followed by neck is useless...
	\+ Xs0 = [neck(_)|_], !,
	% remove the cut
	% TODO: continue or end?
	remove_useless_cut_(Xs0, Xs).
remove_useless_cut_([X|Xs0], [X|Xs]) :- !,
	remove_useless_cut_(Xs0, Xs).

% ---------------------------------------------------------------------------
% special (alloc, init(_)) instructions, mark first fall
% TODO: strange implementation

{
:- fluid bcode :: m_any.
precollapse_init :-
	bcode <- ~precollapse_init_(~bcode).
}.

precollapse_init_([], []).
precollapse_init_([alloc, init(_), cframe(S)|Xs0], [alloc_init_cframe(S)|Xs]) :- !,
	precollapse_init_(Xs0, Xs).
precollapse_init_([alloc, init(_), kall(L,S)|Xs0], [alloc_init_fcall(L,S)|Xs]) :- !,
	precollapse_init_(Xs0, Xs).
precollapse_init_([init([]), kall(L,S)|Xs0], [fcall(L,S)|Xs]) :- !,
	precollapse_init_(Xs0, Xs).
precollapse_init_([init(Ys), kall(L,S)|Xs0], [init(Ys), fcall(L,S)|Xs]) :- !,
	precollapse_init_(Xs0, Xs).
precollapse_init_([X|Xs0], [X|Xs]) :- !,
	precollapse_init_(Xs0, Xs).

{
:- fluid bcode :: m_any.
expand_init :-
	bcode <- ~expand_init_(~bcode).
}.

% for opt(varops)
expand_init_([], Zs) :- !, Zs = [].
expand_init_([init([Y|Ys])|Xs], Zs) :- !,
	B = ~my_var_new(_,y(Y)),
	Zs = [inits(B)|Zs0],
	expand_init_([init(Ys)|Xs], Zs0).
expand_init_([init([])|Xs], Zs) :- !,
	expand_init_(Xs, Zs).
expand_init_([X|Xs], Zs) :- !,
	Zs = [X|Zs0],
	expand_init_(Xs, Zs0).

% ---------------------------------------------------------------------------
% move alloc before some un_? instructions to ease instruction collapse
% remove dealloc before fail

{
:- fluid bcode :: m_any.
move_alloc :-
	bcode <- ~move_alloc_(~bcode).
}.

move_alloc_([X, alloc, Y|Xs0], [alloc, X, Y|Xs]) :-
	Y = un_var(A), yvarmem(A), move_alloc__2(X), !,
	move_alloc_(Xs0, Xs).
move_alloc_([dealloc, failins|Xs0], [failins|Xs]) :- !,
	move_alloc_(Xs0, Xs).
move_alloc_([X|Xs0], [X|Xs]) :- !,
	move_alloc_(Xs0, Xs).
move_alloc_([], []) :- !.

move_alloc__2(un_voidr(_)) :- !.
move_alloc__2(un_var(A)) :- xvarmem(A), !.
move_alloc__2(un_val(A)) :- xvarmem(A), !.
move_alloc__2(un_lval(A)) :- xvarmem(A), !.

% ---------------------------------------------------------------------------
% Remove useless ins args before collapsing
% TODO: change name

{
:- fluid bcode :: m_any.
remove_useless_insargs :-
	bcode <- ~remove_useless_insargs_(~bcode).
}.

remove_useless_insargs_([], []).
remove_useless_insargs_([X0|Xs0], [X|Xs]) :-
	remove_useless_insargs__1(X0, X), !,
	remove_useless_insargs_(Xs0, Xs).
remove_useless_insargs_([X|Xs0], [X|Xs]) :-
	remove_useless_insargs_(Xs0, Xs).

% TODO: move some renaming to previous passes
% TODO: WHY?!?!?!!?!?!? (or why not... similar code was activated but I disabled it; why absmach_def:invalidate_local_top is needed in absmach_def:cutb?)
%   it seems that from the choice point creation to the cutb no new frame is created, thus the cached local top is ok... 
%remove_useless_insargs__1(cutb(x(-1)), cutf(x(-1))) :- !.
remove_useless_insargs__1(neck(_), neck) :- !.
remove_useless_insargs__1(move(X, Y, _), move(X, Y)) :- !.
remove_useless_insargs__1(globunsafe(X, Y), globunsafe2(X, Y)) :- xvarmem(X), xvarmem(Y), !.
remove_useless_insargs__1(init(X, Y), init2h(X, Y)) :- xvarmem(X), xvarmem(Y), !.
remove_useless_insargs__1(init(X, Y), init2s(X, Y)) :- xvarmem(X), yvarmem(Y), !.

% ---------------------------------------------------------------------------
% opt jump

% TODO: collapse labels, remove useless jump
{
:- fluid bcode :: m_any.
opt_jump :-
	~bcode = Code0,
	Code1 = ~collapse_labels(Code0),
	Code2 = ~remove_useless_jump(Code1),
	bcode <- Code2.
}.

collapse_labels([], []) :- !.
collapse_labels([label(Label)|Xs], Ys) :-
	Xs = [label(Label)|_], !, % unify labels
	collapse_labels(Xs, Ys).
collapse_labels([X|Xs], [X|Ys]) :-
	collapse_labels(Xs, Ys).

remove_useless_jump([], []) :- !.
remove_useless_jump([A|Xs], [A|Ys]) :-
	% TODO: this is a naive deadcode elimination
	nocont_ins(A), !,
	skip_until_label(Xs, Xs2),
	remove_useless_jump(Xs2, Ys).
remove_useless_jump([A|Xs], Ys) :-
	% remove useless jump
	A = jump(LabelA), Xs = [label(LabelB)|_],
	LabelA == LabelB, !,
	remove_useless_jump(Xs, Ys).
remove_useless_jump([X|Xs], [X|Ys]) :-
	remove_useless_jump(Xs, Ys).

skip_until_label([], []) :- !.
skip_until_label(Xs, Xs) :- Xs = [label(_)|_], !.
skip_until_label([_|Xs], Ys) :-
	skip_until_label(Xs, Ys).

% TODO: search equivalent predicate...
nocont_ins(lastcall(_)).
nocont_ins(proceed).
nocont_ins(failins).

