:- class(module_ipexp, [], [compiler(complang)]).

% Similar to module_exp, but storing imProlog programs with support for 
% abstract machines.
%
% TODO: separate reader from module_ipexp and merge with action__*
% Author: Jose Morales

:- use_module(compiler(module_exp)).
:- use_module(compiler(list_common),
	[sum_to_list/2, disj_to_list/2, fundisj_to_list/2,
	 conj_to_list/2, list_to_conj/2]).

:- use_module(library(lists)).

:- data tagtest/5.

:- static test_macro_nums/1.
test_macro_nums(8). % TODO: move to source: top of the tagtest

:- static tag_encode_mask_top/1.
tag_encode_mask_top(Mask) :-
	Mask is (1 << ~test_macro_nums) - 1 - (1 << 3).

add_tagtestdefcustom(Head, Body) :- % TODO: a kludge...
	Body2 = ~funcall('$ccons'(Body, intmach)),
	add(entry_lowmacrocons(Head, 1, Head)),
	add(fun_body(Head, 1, [], Body2)).

add_enctagtest(Pos, Post0, Pre0, Cost, Var, Body) :- % TODO: improve
	% Get test name and masks
	TestName0 = ~atom_concat('Tag', Pos),
	( Pre0 = any ->
	    Pre1 = '',
	    PreMask = ~tag_encode_mask_top
	; Pre = ~numdisj_to_atom(Pre0),
	  Pre1 = ~atom_concat('_Pre', Pre),
	  PreMask = ~numdisj_to_mask(Pre0)
	),
	Post = ~numdisj_to_atom(Post0),
	Post1 = ~atom_concat('_Is', Post),
	PostMask = ~numdisj_to_mask(Post0),
	TestName = ~atom_concat(~atom_concat(TestName0, Pre1), Post1),
	% add mask
	add(tagtest(PostMask, PreMask, Pos, Cost, TestName)),
	% add external definition declaration
	treat_decl(pred(TestName/2+foreignfun([tagged], intmach, TestName)+prop(no_worker))),
	% add definition
	Head = ~atom_concat(TestName, ~atom_concat('(', ~atom_concat(Var, ')'))),
	Body2 = ~funcall('$ccons'(Body, intmach)),
	add(entry_lowmacrocons(Head, 1, Head)),
	add(fun_body(Head, 1, [], Body2)).

:- static numdisj_to_mask/2.
numdisj_to_mask(X, _) :- var(X), !, fail.
numdisj_to_mask((X;Y), M) :-
	numdisj_to_mask(X, Xm),
	numdisj_to_mask(Y, Ym),
	M is Xm \/ Ym.
numdisj_to_mask(X, M) :- integer(X), !,
	M is 1 << X.

:- static numdisj_to_atom/2.
numdisj_to_atom(X, Y) :-
	disj_to_list(X, X2),
	numlist_to_atom(X2, Y).

:- static numlist_to_atom/2.
numlist_to_atom([], '') :- !.
numlist_to_atom([X|Xs], T) :- !,
	numlist_to_atom(Xs, T2),
	number_codes(X, 16, Codes),
	atom_codes(X2, Codes),
	atom_concat(X2, T2, T).

% Obtain a mask of the encoding numbers of a tag disjunction
tagdisj_to_encode_mask(Disj) :=
	( Disj = any ? ~tag_encode_mask_top
	| ~tagdisj_to_encode_mask__2(Disj)
	).

tagdisj_to_encode_mask__2(X, _) :- var(X), !, fail.
tagdisj_to_encode_mask__2((X;Y), M) :-
	tagdisj_to_encode_mask__2(X, Xm),
	tagdisj_to_encode_mask__2(Y, Ym),
	M is Xm \/ Ym.
tagdisj_to_encode_mask__2(X0, M) :- atom(X0),
	BaseType = 'tagged_$key',
	( enum_domain__encode_atom(X0, BaseType, X) ->
	    true
	; ptoc__impcomp:error(['unknown tag ', X0])
	),
	M is 1 << X.
	
add_swtest_macro(CName, Pre0, Post0) :-
	PreMask = ~tagdisj_to_encode_mask(Pre0),
	PostMask = ~tagdisj_to_encode_mask(Post0),
	TestName = ~tagtest_macro_id(PreMask, PostMask),
	TaggedVar = _,
	Types = [tagged],
	TestArg = TaggedVar,
	add(entry_lowmacrofun(CName, 2, Types, CName)),
	Body =.. [TestName, TestArg],
	add(fun_body(CName, 2, [TaggedVar], ~funcall(Body))).

tagtest_macro_id(PreMask, PostMask) := TestName :-
	( swtest_mode(Pos) ->
	    true
	; ptoc__impcomp:error(['no swtest mode'])
	),
	( ( Cost = 1 ; Cost = 2 ),
	  tagtest(TestPostMask, TestPreMask, Pos, Cost, TestName0),
	  PostMask is (TestPostMask /\ PreMask), % allow a test that succeeds with the test post filtered with our precondition
	  PreMask is (TestPreMask /\ PreMask) -> % check that the test includes at least our precondition in its precondition
	    TestName = TestName0
	; ptoc__impcomp:error(['not found tagtest for ', PreMask, ' and ', PostMask])
	).
	
add_swrule_macro(N, Args, Mode, Cases) :-
	( tagswcode(Mode, Cases, Reg, Code) ->
	    true
	; ptoc__impcomp:error(['cannot generate code for swrule macro ', Head])
	),
	Head =.. [N,Reg|Args],
	functor(Head, N, A),
	functor(Head2, N, A),
	Head2 =.. [_,Reg2|Args2],
	Imp = det,
	tagswcode_info(Args, Types, MArgs),
	CName = N,
	add(entry_lowmacro(N, A, Imp, [tagged|Types], CName, [c|MArgs])),
	add_clause(Head2, '$inline_c'([Reg2|Args2], [Reg|Args], Code)).

tagswcode_info([_|Args], [goto_sentence|Types], [c|MArgs]) :-
	tagswcode_info(Args, Types, MArgs).
tagswcode_info([], [], []).

tagswcode(Mode0, Cases, Reg, Code) :- !,
	Mode = ( Mode0 = sw(ModeCTree, ModeCSwitch) ?
	           ( get__use_opt(tagswbit) ? ModeCTree
		   | ModeCSwitch
		   )
	       | Mode0
	       ),
	( Mode = ctree ->
	    tagswcode__tagtree_annot(Cases, Cases2, Pre),
	    PreMask = ~tagdisj_to_encode_mask(Pre),
	    tagswcode__tagtree_cases(Cases2, Reg, PreMask, Code)
	; Mode = cswitch ->
	    tagswcode__tagcswitch_annot(Cases, Cases2, Pre),
	    PreMask = ~tagdisj_to_encode_mask(Pre),
	    tagswcode__tagcswitch(Cases2, Reg, PreMask, Code)
	; Mode = cif ->
	    Cases = [case(Tag, True), case(_, False)],
	    tagswcode__tagcif(Reg, Tag, True, False, Code)
	).

% annotate non-leaf branches with the disjunction of tags of their descendants
tagswcode__tagtree_annot(CaseCode, _Code, _DPost) :- var(CaseCode), !, fail.
tagswcode__tagtree_annot([sw(CaseCode0)|Xs0], [case(Post, CaseCode)|Xs], DPost) :- !,
	Post = CPost,
	tagswcode__tagtree_annot(CaseCode0, CaseCode, CPost),
	( Xs0 = [] ->
	    DPost = CPost
	; DPost = (CPost ; DPost1),
	  tagswcode__tagtree_annot(Xs0, Xs, DPost1)
	).
tagswcode__tagtree_annot([case(Post, CaseCode0)|Xs0], [case(Post, CaseCode)|Xs], DPost) :- !,
	CaseCode = CaseCode0,
	( Xs0 = [] ->
	    DPost = Post
	; DPost = (Post ; DPost1),
	  tagswcode__tagtree_annot(Xs0, Xs, DPost1)
	).

% TODO: merge with tagtree_annot
tagswcode__tagcswitch_annot(CaseCode, _Code, _DPost) :- var(CaseCode), !, fail.
tagswcode__tagcswitch_annot([sw(Cases)|Xs0], Xs, DPost) :- !,
	tagswcode__tagcswitch_annot(~append(Cases, Xs0), Xs, DPost).
tagswcode__tagcswitch_annot([case(Post, CaseCode0)|Xs0], [case(Post, CaseCode)|Xs], DPost) :- !,
	CaseCode = CaseCode0,
	( Xs0 = [] ->
	    DPost = Post
	; DPost = (Post ; DPost1),
	  tagswcode__tagcswitch_annot(Xs0, Xs, DPost1)
	).

% generate code for a switch on tag tree, using the cheapest test in each branch (using tag preconditions)
tagswcode__tagtree_cases(CaseCode, _Reg, _Mask0, _Code) :- var(CaseCode), !, fail.
tagswcode__tagtree_cases([case(Post, CaseCode)|Xs], Reg, Mask0, Code) :- !,
	PostMask2 = ~tagdisj_to_encode_mask(Post),
	MaskThen is Mask0 /\ PostMask2, % filter postmask from Mask0 (just in case Mask0 was more restrictive)
	tagswcode__tagtree_case(CaseCode, Reg, MaskThen, TrueCode),
	MaskTop = ~tag_encode_mask_top,
	MaskElse is Mask0 /\ (MaskTop - PostMask2), % remove condition cases for the else mask
	( MaskElse = 0 ->
	    ( Xs = [] -> true
	    ; ptoc__impcomp:error(['bug: uncovered cases in tagtree ', Xs])
	    ),
	    Code = TrueCode
	; TestMacro = ~tagtest_macro_id(Mask0, MaskThen),
	  Code = if(call(TestMacro,[Reg]), block([TrueCode]), block([ElseCode])),
	  tagswcode__tagtree_cases(Xs, Reg, MaskElse, ElseCode)
	).

tagswcode__tagtree_case(CaseCode, _Reg, _Mask0, Code) :- var(CaseCode), !,
	Code = CaseCode.
tagswcode__tagtree_case(CaseCode, Reg, Mask0, Code) :- CaseCode = [_|_], !,
	tagswcode__tagtree_cases(CaseCode, Reg, Mask0, Code).
tagswcode__tagtree_case(CaseCode, _Reg, _Mask0, Code) :- !,
	Code = CaseCode.

tagswcode__tagcswitch(Cases, Reg, Mask0, Code) :- !,
	tagswcode__tagcswitch__2(Cases, Mask0, Cases2),
	Code = switch(call('TagOf', [Reg]), Cases2).

% generate code for a switch on tag tree, using the cheapest test in each branch (using tag preconditions)
tagswcode__tagcswitch__2(CaseCode, _Mask0, _Code) :- var(CaseCode), !, fail.
tagswcode__tagcswitch__2([case(Post, CaseCode)|Xs], Mask0, Code) :- !,
	PostMask2 = ~tagdisj_to_encode_mask(Post),
	% MaskThen is Mask0 /\ PostMask2, % filter postmask from Mask0 (just in case Mask0 was more restrictive)
	MaskTop = ~tag_encode_mask_top,
	MaskElse is Mask0 /\ (MaskTop - PostMask2), % remove condition cases for the else mask
	( MaskElse = 0 ->
	    ( Xs = [] -> true
	    ; ptoc__impcomp:error(['bug: uncovered cases in tagcswitch ', Xs])
	    ),
	    Code = [default(CaseCode)]
	; disj_to_list(Post, KeyList),
	  tagswcode__tagcswitch__keys(KeyList, CaseCode, Code, ElseCode),
	  tagswcode__tagcswitch__2(Xs, MaskElse, ElseCode)
	).

tagswcode__tagcswitch__keys([Key|Keys], CaseCode, Ys, Ys0) :-
	Ys = [case(call('TAG', [Key]), block([]))|Ys1],
	tagswcode__tagcswitch__keys(Keys, CaseCode, Ys1, Ys0).
tagswcode__tagcswitch__keys([Key], CaseCode, Ys, Ys0) :-
	Ys = [case(call('TAG', [Key]), block([CaseCode, break]))|Ys0].

tagswcode__tagcif(Reg, Tag, True, False, Code) :-
	Code = if(call('TagOf', [Reg]) == call('TAG', [Tag]), block([True]), block([False])).

% TODO: in addition to ptoc__impcomp.pl, do:
% - split as a improlog source reader (and merge it with the prolog reader) and a absmach expansor
% - instruction collapsing when mode changes
% - think how to implement and specify threaded bytecode (opcodes become pointer to functions), instruction after ins that switch between read and write mode needs dual opcode

% TODO: err bugs that begin with "error:" (errlog:bug(['error:...'])) should be compilation errors!

:- meta_predicate maybe_parent(out(module_ipexp)).
maybe_parent(Ipexp2) :-
	( Ipexp2 = ~self
	; ~parent_ipexp = ParentIpexp,
	  \+ ParentIpexp = none,
	  Ipexp2 = ParentIpexp
	).

:- public use_fun_eval/2.
use_fun_eval(GN,GA) :-
	maybe_parent(Ipexp2),
	Ipexp2.fun_eval(GN,GA).

:- public get__op__foreign/3.
get__op__foreign(GN,GA,Def) :-
	maybe_parent(Ipexp2),
	Ipexp2.op__foreign(GN,GA,Def), !.
:- public get__op__foreignfun_spec/6.
get__op__foreignfun_spec(GN,GA,InTypes,Ret,CIn,CExpr) :- % TODO: does backtracking
	maybe_parent(Ipexp2),
	Ipexp2.op__foreignfun_spec(GN,GA,InTypes,Ret,CIn,CExpr).
:- public get__op__prop/3.
get__op__prop(GN,GA,B) :-
	maybe_parent(Ipexp2),
	Ipexp2.op__prop(GN,GA,B), !.
:- public get__ltype__kind/2.
get__ltype__kind(Name,TpDef) :-
	maybe_parent(Ipexp2),
	Ipexp2.ltype__kind(Name, TpDef), !.
:- public get__ltype__sentences/2.
get__ltype__sentences(Name,Sentences) :-
	maybe_parent(Ipexp2),
	Ipexp2.ltype__sentences(Name, Sentences), !.
:- public get__ltype__encoding_type/2.
get__ltype__encoding_type(GN,BaseType) :-
	maybe_parent(Ipexp2),
	Ipexp2.ltype__encoding_type(GN,BaseType), !.
:- public get__ltype__parent/2.
get__ltype__parent(GN,Parent) :-
	maybe_parent(Ipexp2),
	Ipexp2.ltype__parent(GN,Parent), !.
:- public get__ltype__disj/1.
get__ltype__disj(GN) :-
	maybe_parent(Ipexp2),
	Ipexp2.ltype__disj(GN), !.
:- public get__op__iterdef/6.
get__op__iterdef(Iter, I, Init, Cond, Next, MaybeEmpty) :-
	maybe_parent(Ipexp2),
	Ipexp2.iterator_def(Iter, I, Init, Cond, Next, MaybeEmpty), !.
:- public get__swrule_def/4.
get__swrule_def(SwitcherKeys, SwitcherXType0, SwitcherJoin, Switcher0) :-
	maybe_parent(Ipexp2),
	Ipexp2.swrule_def(SwitcherKeys, SwitcherXType0, SwitcherJoin, Switcher0), !.
:- public get__swjoinable_def/1.
get__swjoinable_def(X) :-
	maybe_parent(Ipexp2),
	Ipexp2.swjoinable_def(X).
:- public get__use_opt/1.
get__use_opt(A) :-
	maybe_parent(Ipexp2),
	Ipexp2.use_opt(A), !.
:- public get__pointer_size/1.
get__pointer_size(Size) :-
	maybe_parent(Ipexp2),
	Ipexp2.pointer_size(Size), !.
:- public get__tagged_size/1.
get__tagged_size(Size) :-
	maybe_parent(Ipexp2),
	Ipexp2.tagged_size(Size), !.
:- public get__enum_domain__elems/2.
get__enum_domain__elems(Name, Elems) :-
	maybe_parent(Ipexp2),
	Ipexp2.enum_domain__elems(Name, Elems), !.
:- public get__enum_domain__atom_to_mask/3.
get__enum_domain__atom_to_mask(Name, Atom, Mask) :-
	maybe_parent(Ipexp2),
	Ipexp2.enum_domain__atom_to_mask(Atom, Name, Mask), !.
:- public get__enum_domain__mask_to_atom/3.
get__enum_domain__mask_to_atom(Name, Mask, Atom) :-
	maybe_parent(Ipexp2),
	Ipexp2.enum_domain__mask_to_atom(Mask, Name, Atom), !.
:- public get__enum_domain__encode_atom/3.
get__enum_domain__encode_atom(Name, Atom, Encoded) :-
	maybe_parent(Ipexp2),
	Ipexp2.enum_domain__encode_atom(Atom, Name, Encoded), !.
:- public get__enum_domain__atoms/2.
get__enum_domain__atoms(Name, Atoms) :-
	maybe_parent(Ipexp2),
	!,
	% TODO: may be very inefficient (index on Atom, not in Name)
	Ipexp2.enum_domain__atoms(Name, Atoms).

enum_domain__atoms(Name, Atoms) :-
	findall(Atom, enum_domain__atom_to_mask(Atom, Name, _), Atoms).

% ---------------------------------------------------------------------------
% Program parameters

:- public data use_opt/1.

% ---------------------------------------------------------------------------

:- public blob_aligned_size/2.
blob_aligned_size(C) :=
	( is_float(C) ? ~float_size(C)
	| is_bignum(C) ? ~bignum_aligned_size(C)
	).

:- public static is_float/1.
% X is a float
is_float(X) :- float(X).

:- public float_size/2.
% Get the size (in cells) of a float numbers
float_size(_) := ~tp_size(flt64).

% ---------------------------------------------------------------------------
% Wrappers for type and other operations
% TODO: impcomp operations require class(module_exp) but we do not
% have them here.

% TODO: do not use ptoc__impcomp here!
:- use_module(compiler(ptoc__impcomp),
	[type_analyze/1, type_size/2, peval_goal/2, type_eval/2, type_lub/3,
	 error/1]).

tp_analyze(BaseType) :-
	ipexp :: any <- ~self,
	exp :: any <- no_exp,
	ptoc__impcomp:type_analyze(BaseType).

tp_size(Type) := Size :-
	ipexp :: any <- ~self,
	exp :: any <- no_exp,
	ptoc__impcomp:type_size(Type, Size).

tp_eval(Def) := T :-
	ipexp :: any <- ~self,
	exp :: any <- no_exp,
	ptoc__impcomp:type_eval(Def, T).

tp_lub(A, B) := T :-
	ipexp :: any <- ~self,
	exp :: any <- no_exp,
	ptoc__impcomp:type_lub(A, B, T).

process_cond(Cond, S) :-
	ipexp :: any <- ~self,
	exp :: any <- no_exp,
	ptoc__impcomp:peval_goal(Cond, S).

% ---------------------------------------------------------------------------

:- data num_bits/1.

:- public is_bignum/1.
% I is a bignum
is_bignum(I) :- 
	integer(I),
	num_bits(Bits),
	Bits1 is Bits - 1,
	Bits2 is Bits,
	I>>Bits1 =\= I>>Bits2.

:- public bignum_aligned_size/2.
% Get the size (in cells) of a bignum number
% TODO: make it parametic w.r.t. bignum_t (it is assumed here that it is a 32 bits word)
bignum_aligned_size(I) := H :-
	bignum_unit_size(Bytes),
	Bits is Bytes * 8,
	Bits1 is Bits - 1,
	J is I>>Bits1,
	H0 = ~bignum_aligned_size__2(J, Bits, 1),
	H1 is H0 * Bytes,
	( get__tagged_size(8) -> % TODO: use ALIGN_BLOB_64, with tagged64 blobs are aligned to 8 bytes too
	    H is ((H1 + 7) // 8) * 8
	; H = H1
	).

:- data bignum_unit_size/1.

:- static bignum_aligned_size__2/4.
bignum_aligned_size__2(I, Bits, H0) := H :-
	( I =:= I>>1 ->
	    H = H0
	; K is I>>Bits,
	  H1 is H0 + 1,
	  H = ~bignum_aligned_size__2(K, Bits, H1)
	).

:- public typemask_of_functor/2.
typemask_of_functor(K/0,   2'00010) :- number(K), !.
typemask_of_functor(K/0,   2'00100) :- atom(K), !.
typemask_of_functor('.'/2, 2'01000) :- get__use_opt(optlst), !.
typemask_of_functor(_,     2'10000) :- !.

% Data operations to move from Prolog to Absmach domains (and viceversa)

:- public functorcons/2.
functorcons(Constant/0) := Constant1 :- !,
	Constant1 = ( is_float(Constant) ? float(Constant)
		    | is_bignum(Constant) ? bignum(Constant)
		    | number(Constant) ? smallint(Constant)
		    | atom(Constant)
		    ).
functorcons('.'/2) := list :- get__use_opt(optlst), !.
functorcons(Functor) := str(Functor).

:- public static functorcons__is_constant/1.
functorcons__is_constant(float(_)).
functorcons__is_constant(bignum(_)).
functorcons__is_constant(smallint(_)).
functorcons__is_constant(atom(_)).

:- public static functorcons__constant_value/2.
functorcons__constant_value(float(A)) := A.
functorcons__constant_value(bignum(A)) := A.
functorcons__constant_value(smallint(A)) := A.
functorcons__constant_value(atom(A)) := A.

:- public functorcons__functor/2.
functorcons__functor(float(A)) := A/0.
functorcons__functor(bignum(A)) := A/0.
functorcons__functor(smallint(A)) := A/0.
functorcons__functor(atom(A)) := A/0.
functorcons__functor(str(N/A)) := N/A.
functorcons__functor(list) := '.'/2 :- get__use_opt(optlst), !.

:- public static functorcons__single_cell/1.
functorcons__single_cell(smallint(_)).
functorcons__single_cell(atom(_)).

:- public functorcons__heap_usage/2.
functorcons__heap_usage(float(A)) := Size :-
	float_size(A, Size0),
	% note: add the size of the head and mirror functor
	get__tagged_size(S),
	Size is Size0 + 2*S.
functorcons__heap_usage(bignum(A)) := Size :-
	bignum_aligned_size(A, Size0),
	% note: add the size of the head and mirror functor
	get__tagged_size(S),
	Size is Size0 + 2*S.
functorcons__heap_usage(smallint(_)) := 0.
functorcons__heap_usage(atom(_)) := 0.
functorcons__heap_usage(str(NA)) := Size :-
	NA = _/Ar,
	get__tagged_size(S),
	Size is S*(Ar+1).
functorcons__heap_usage(list) := Size :- get__use_opt(optlst), !,
	get__tagged_size(S),
	Size is 2*S.

% ---------------------------------------------------------------------------
% Compute the padding for a instruction 

get_padding(Fs) := Padding :-
	format :: any <- Fs,
	off :: m_int <- 0,
	count :: m_int <- 0,
	Padding = ~get_padding__1.
{
:- fluid format :: any.
:- fluid count :: m_int.
:- fluid off :: m_int.

:- '$ctxprj'(get_padding__1/1, [format, u(count), u(off)]).
get_padding__1 :=
	( check_aligned(0, 2, Padding0) ?
	    Padding0 % no padding is required
	| padding(AlignOff, AlignMod),
	  check_aligned(AlignOff, AlignMod, Padding0) ?
	    [pad(AlignOff, AlignMod)|Padding0]
	| impossible
	).

:- '$ctxprj'(padding/2, []).
:- static padding/2.
padding(0,4). % pad 0202
padding(2,4). % pad 2020
padding(0,8). % pad 0642
padding(2,8). % pad 2064
padding(4,8). % pad 4206
padding(6,8). % pad 6420

:- '$ctxprj'(check_aligned/3, [format, u(count), u(off)]).
check_aligned(AlignOff, AlignMod, Padding) :-
	alignoff :: m_int <- AlignOff,
	alignmod :: m_int <- AlignMod,
	check_aligned__2(~format, Padding).
{
:- fluid alignoff :: m_int.
:- fluid alignmod :: m_int.

:- '$ctxprj'(check_aligned__2/2, [count, off, alignoff, alignmod]).
check_aligned__2([], Padding) :- !,
	Padding = [].
check_aligned__2([F|Fs], Padding) :-
	( ftype__def(F, str(Fs2)) ->
	    call((
              count :: m_int <- 0,
	      check_aligned__2(Fs2, [])
            )),
	    Padding = Padding0,
	    count.inc(1),
	    check_aligned__2(Fs, Padding0)
	; ftype__size(F, Size) ->
	    off_aligned(Size),
	    Padding = Padding0,
	    add_off(Size),
	    off.inc(Size),
	    count.inc(1),
	    check_aligned__2(Fs, Padding0)
	; % an operand whose size is variable
	  ( ftype__def(F, array(IF,EF)) ->
	      ftype__size(IF, IFSize),
	      ftype__size(EF, EFSize),
	      % move the abstract offset
	      off_aligned(IFSize),
	      add_off(IFSize),
	      off_aligned(EFSize),
	      add_moff(EFSize)
	  ; ftype__def(F, blob) ->
	      ( bignum_unit_size(BlobUnitSize) -> true ),
	      get__tagged_size(FunctorSize),
	      % move the abstract offset
	      off_aligned(FunctorSize),
	      add_off(FunctorSize),
	      off_aligned(BlobUnitSize),
	      add_moff(BlobUnitSize)
	  ; ptoc__impcomp:error(['unknown f in sta ', F])
	  ),
	  % the concrete offset is unknown
	  Count0 = ~count,
	  Off0 = ~off,
	  count <- unknown,
	  off <- unknown,
	  Padding1 = ~get_padding(Fs),
	  ( Padding1 = impossible ->
	      fail
	  ; Padding1 = [] ->
	      % ignore if no further alignments are required
	      Padding = []
	  ; % annotate the number of operands to skip (C0) and its size (Off0)
	    Padding = [skip(Count0, Off0)|Padding1]
	  )
	).

% Abstraction of memory offsets
% a0: 0 + 8*i
% a2: 2 + 8*i
% a4: 4 + 8*i
% a6: 6 + 8*i
% a04: (0 or 4) + 8*i
% a26: (2 or 6) + 8*i
% a0246: (0, 2, 4 or 6) + 8*i

% Displace an abstract memory offset

:- '$ctxprj'(add_off/1, [alignoff, u(alignmod)]).
add_off(Size) :-
	AlignOff is (~alignoff + Size) mod ~alignmod,
	alignoff <- AlignOff.

% Displace an abstract memory offset a multiple of the indicated offset
:- '$ctxprj'(add_moff/1, [alignoff, alignmod]).
add_moff(Size) :-
	AlignMod0 = ~alignmod,
	% new align mod is the mininum of {AlignMod0, Size}
	( AlignMod0 < Size -> AlignMod = AlignMod0 ; AlignMod = Size ),
	alignmod <- AlignMod,
        % new align offset is recalculated with new modulo
	AlignOff is ~alignoff mod AlignMod,
	alignoff <- AlignOff.

% Is an abstract memory offset aligned?
:- '$ctxprj'(off_aligned/1, [u(alignoff), u(alignmod)]).
off_aligned(Size) :-
	% it is aligned if Size is not greater than align module
	Size =< ~alignmod,
	% and a word of size Size is aligned to the offset
	0 is ~alignoff mod Size.

}.
}.

% ---------------------------------------------------------------------------

% TODO: not symmetrical??
format_glb([], [], []).
format_glb([F|Fs], [T|Ts], [F2|F2s]) :-
	% TODO: obtain the more specific of F and T (is it a glb?)
	% TODO: improve handling of ftype__array_elem!!
	( F = T -> F2 = T
	; F = a -> F2 = T
	; F = s(N) -> F2 = s(T, N) % TODO: check that it is an ftype__array_elem
	; F = c(N) -> F2 = c(N) % TODO: use f_i(N)??
	; ftype__glb(F, T, F2) -> true
	),
	format_glb(Fs, Ts, F2s).

% ---------------------------------------------------------------------------

% TODO: document
expand_spec(Insns, Patterns, SIns, SArgs, Format) :-
	expand_spec__2(Insns, Patterns, SInsns, SArgs2, Format2),
	id_concat(SInsns, SIns),
	call(( items :: accum(Format), flat_list(Format2) )),
	call(( items :: accum(SArgs), flat_list(SArgs2) )).

:- static id_concat/2.
% Obtain a single identifier for a sequence of instructions
% TODO: incomplete... extend to more specializations
% TODO: incomplete... more checking
id_concat([A]) := A :- !.
id_concat([A|As]) := ~atom_concat(A, ~atom_concat('+', ~id_concat(As))) :- !.

% Obtain a single identifier for a format
% TODO: incomplete... extend to more specializations
% TODO: incomplete... more checking
:- static ground_to_atom/2.
ground_to_atom(A) := _ :- var(A), !, fail.
ground_to_atom(A) := A :- atom(A), !.
ground_to_atom(A) := B :- number(A), !,
	number_codes(A, Codes),
	atom_codes(B, Codes).
ground_to_atom(A) := T :-
	A =.. [N|As],
	T = ~atom_concat(~atom_concat(N, '('), ~ground_to_atom__2(As)).

:- static ground_to_atom__2/2.
ground_to_atom__2([A]) := ~atom_concat(~ground_to_atom(A), ')') :- !.
ground_to_atom__2([A|As]) := ~atom_concat(~ground_to_atom(A), ~atom_concat(',', ~ground_to_atom__2(As))) :- !.

expand_spec__2([], [], [], [], []).
expand_spec__2([Ins|Insns], [Pattern|Patterns], [SIns|SInsns], [SArgs|SArgs2], [Format|Format2]) :-
	expand_spec__3(Ins, Pattern, SIns, _Args, SArgs, Format),
	expand_spec__2(Insns, Patterns, SInsns, SArgs2, Format2).

{
:- fluid items :: accum.
:- static flat_list/1.
flat_list([]).
flat_list([X|Xs]) :-
	flat_list__2(X),
	flat_list(Xs).

:- static flat_list__2/1.
flat_list__2([]).
flat_list__2([X|Xs]) :- items.add(X), flat_list__2(Xs).
}.

% TODO: document
expand_spec__3(I0, Pattern, SIns, Args, SArgs, Format) :-
	ins_name_and_format(I0, InsPattern, Format1),
	spec_pattern(Format1, Args, ArgsPattern, SArgs, Format),
	Pattern =.. [InsPattern|ArgsPattern],
	( InsPattern = cjump ->
	    SIns = InsPattern
	; InsPattern = jump ->
	    SIns = InsPattern
	; SInsTerm =.. [InsPattern|Format1],
	  ground_to_atom(SInsTerm, SIns)
	).

% For the instruction entry (I0), get the name (IN), the format (Format1) and the name of the version (InsPattern)
ins_name_and_format(I0, InsPattern, Format1) :-
	I0 =.. [IN|As], 
	( op__format(IN, Format0) -> true ),
	InsPattern = IN,
	format_glb(As, Format0, Format1).

spec_pattern([s(F, N)|F0s], [FArgs|Args], [FArgs|PArgs], SArgs, SFormat) :- number(N),
	ftype__array_elem(F, Fx), !,
	spec_pattern_list(N, FArgs),
	spec_pattern_flist(N, Fx, Format),
	append(FArgs, SArgs0, SArgs),
	append(Format, SFormat0, SFormat),
	spec_pattern(F0s, Args, PArgs, SArgs0, SFormat0).
spec_pattern([c(N)|F0s], [N|Args], [N|PArgs], SArgs, SFormat) :- number(N), !,
	spec_pattern(F0s, Args, PArgs, SArgs, SFormat).
spec_pattern([F0|F0s], [_|Args], [PArg|PArgs], SArgs, SFormat) :- dec_pat0(F0, PArg), !,
	spec_pattern(F0s, Args, PArgs, SArgs, SFormat).
spec_pattern([F|F0s], [A|Args], [PArg|PArgs], [A|SArgs], [F|SFormat]) :-
	( F = a ->
	    PArg = A
	; dec_pat(F, A, PArg) ->
	    true
	; PArg = A % TODO: add a pattern here for all other cases!
	),
	spec_pattern(F0s, Args, PArgs, SArgs, SFormat).
spec_pattern([], [], [], [], []).

:- static spec_pattern_list/2.
spec_pattern_list(0, []) :- !.
spec_pattern_list(I, [_|Xs]) :- I1 is I - 1, spec_pattern_list(I1, Xs).

:- static spec_pattern_flist/3.
spec_pattern_flist(0, _, []) :- !.
spec_pattern_flist(I, Fx, [Fx|Fs]) :-
	I1 is I - 1, spec_pattern_flist(I1, Fx, Fs).

:- static format_pattern/2.
format_pattern([], []).
format_pattern([_|Xs], [_|Ys]) :- format_pattern(Xs, Ys).

% ===========================================================================

% TODO: merge ftype decls in a single decl for each ftype?

:- discontiguous(ftype__head/2).
:- public static ftype__head/2.
:- discontiguous(ftype__tail/2).
:- public static ftype__tail/2.
:- discontiguous(ftype__is_arrayrest/1).
:- public static ftype__is_arrayrest/1.
:- discontiguous(ftype__array_elem/2).
:- public static ftype__array_elem/2.

% TODO: document: ftype that does not requires encoding, contains all
%   the information
% TODO: add a 'class' 
%:- public class ftype {
%}.
:- public data ftype__id/2.
% note: this is the type of the encoded ftype
:- public data ftype__enctype/2.
:- public data ftype__zero/1.
% note: this is the type of the decoded ftype
:- public data ftype__dectype/2.
:- public data ftype__def/2.

% TODO: document: greatest lower bound of ftypes
%    m is a reference to a variable of type f_t
%    f_x is a kind of m
%    f_y is a kind of m
%    a is anything
% TODO: incomplete
:- public data ftype__glb/3.

% arg o decoding: ftype x token_b -> exp_c
% TODO: check...
:- public data ftype__decfun/2.
% TODO: document... like ftype__decfun but for ftypes that does not require any token_b to be encoded (0-length encodes)
:- public data ftype__decfun0/3.
% decoding pattern (from ftype to La)
% TODO: check... used to generate spec patterns
:- data dec_pat/3.
% TODO: document... like dec_pat but for 0-length encodes
:- data dec_pat0/2.
% encoding: ftype x arg_a -> token_b'
% TODO: check...
:- public data ftype__encfun/2.
%
:- public data ftype__smethod/2.
:- public data ftype__lmethod/2.

ftype__is_arrayrest(arrayrest(_)).

ftype__head(arrayrest(F), F). % A is list of z (unknown length)
ftype__tail(arrayrest(F), arrayrest(F)).

ftype__head(f_Y, f_i). % Y is iB (i is the length of B)
ftype__tail(f_Y, arrayrest(f_y)).
ftype__array_elem(f_Y, f_y).

ftype__head(f_Z, f_i). % Z is iA (i is the length of A)
ftype__tail(f_Z, arrayrest(f_z)).
ftype__array_elem(f_Z, f_z).

:- public ftype__size/2.
ftype__size(A, Size) :-
	ftype__enctype(A, Def),
	Size = ~tp_size(Def).

% size of pointers
:- data pointer_size/1.

% cached value for size of native type tagged
:- public data tagged_size/1.

% Identifiers for each save_method
% TODO: synchronize automatically with tables in absmach_def.pl
:- public static save_method/2.
save_method(integer, 2).
save_method(poffset, 3).
save_method(functor, 5).
save_method(tagged, 6).
save_method(emul_entry, 7).
save_method(builtin_entry, 9).
save_method(small, 8).

% Identifiers for each load_method
% TODO: synchronize automatically with tables in absmach_def.pl
:- public static load_method/2.
load_method(uint16, 8).
load_method(uint32, 6).
load_method(uint64, 9).
load_method(baseptr, 3).

:- public data foreign_include_in_header/1.

:- data cached_pred_body/2. % cached normalization of pred
:- data pred_clause/2.
:- data fun_body/4. % TODO: merge with pred_clause
:- data iterator_def/6.
:- data swrule_def0/3.
:- data swrule_def/4.
:- data swjoinable_def/1.
:- data swtest_mode/1.

% ===========================================================================

:- public data enum_domain__elems/2.
:- public data enum_domain__atom_to_mask/3.
:- public data enum_domain__mask_to_atom/3.
% encoding
:- public data enum_domain__encode_atom/3.

:- public data fun_eval/2.

% ===========================================================================

:- public data op__format/2.
:- public data op__iter/3.
:- public data op__foreign/3.
:- public data op__prop/3.
:- public data op__foreignfun_spec/6.

% ===========================================================================

% name of the instruction set definition to load
:- public data iset/1.
% instruction set definition
:- public data iset_entry/3.
:- public data iset_exported/3.
:- public data iset_illop/1.
:- public data iset_catch_fail/1.

% ===========================================================================

% instruction set definition (after collapsing/spec transformation)
:- public data ic_exported/3.
:- public data ic_catch_fail/1.

% ===========================================================================

% TODO: move to other file?
:- public data entry_lowmacrofact/2. 
:- public data entry_lowmacro/6.
:- public data entry_lowmacrocons/3.
:- public data entry_lowmacrofun/4.
:- public data entry_lowpred/6.
:- public data entry_lowinst/5.
:- public data defmode/2.
:- public data def_lowtype/1.
:- public data def_lowatom/2.
% TODO: temporal?
:- public data lowinclude/2.

% ===========================================================================
% (temporal inst data used in emit_emulator)

% TODO: this is dirty
:- public data ftype_rev_id/2. % for gn_ftype_info/2
% Keep the state of ltype objects
:- public data ltype__kind/2.
:- public data ltype__sentences/2.
:- public data ltype__analyzed/1.
:- public data ltype__field/4.
:- public data ltype__field_prop/3.
:- public data ltype__typekey_field/2. % TODO: experimental
:- public data ltype__typekey_value/2. % TODO: experimental
:- public data ltype__typekey_type/2. % TODO: experimental
:- public data ltype__equiv/2.
:- public data ltype__predabs/4.
:- public data ltype__bitstr__size/2.
:- public data ltype__bitstr__storage/3.
:- public data ltype__bitstr__smallptr_model/2.
:- public data ltype__parent/2.
:- public data ltype__disj/1.
:- public data ltype__encoding_base/2.
:- public data ltype__encoding_type/2.
:- public data ltype__struct_size/2. % for struct_size/2

% ===========================================================================

% for conditional code at the declaration level
% TODO: improve implementation!
:- data cond_status/1.

:- attr spec :: any.

:- attr parent_ipexp :: any.

:- use_module(compiler(errlog)).
:- use_module(compiler(memoize)).
:- use_module(compiler(store)).
:- use_module(compiler(flagcontext)).
:- use_module(compiler(read_source)).

:- use_module(library(system), [getenvstr/2]).
:- use_module(engine(internals), ['$global_vars_set'/2, '$global_vars_get'/2]).

{
:- fluid memo :: memoize.

:- public static load_def/1.
% TODO: only one absmach can be loaded at this time (because of static $absmach)
% TODO: use in errlog set lines instead of add_lines and del_lines!! ... so that we can set the lines in read_sentence and use them forever...? hmmm dunno...
load_def(_) :-
	'$global_vars_get'(6, X), nonvar(X), X = loaded(_),
	!.
load_def(Spec) :-
	'$global_vars_set'(6, loaded(This)),
	This = ~load(Spec).
}.

:- public static meta_predicate '$absmach'(out(module_ipexp)).
'$absmach'(Ipexp) :-
	'$global_vars_get'(6, X), nonvar(X), X = loaded(Ipexp).
	
% TODO: this is a temporary hack to integrate with the default Ciao reader
:- public static load_from_list/3.
load_from_list(List, ParentIpexp) := This :-
	This = ~new,
	( This.load_from_list__1(List, ParentIpexp) ->
            true
%	    errlog:trace(['loaded abstract machine definition ', Spec])
	; '$inst_destroy'(This),
	  fail
	).

:- constructor new_/0.
new_.

load_from_list__1(Xs, ParentIpexp) :-
	ParentIpexp = ~parent_ipexp,
	load_from_list__2(Xs).

load_from_list__2([]).
load_from_list__2([X|Xs]) :-
	treat_sentence__2(X),
	load_from_list__2(Xs).

{
:- fluid memo :: memoize.

:- public static load/2.
load(Spec) := This :-
	This = ~new,
	( This.load__2(Spec) ->
            true
%	    errlog:trace(['loaded abstract machine definition ', Spec])
	; '$inst_destroy'(This),
	  fail
	).

load__2(Spec) :-
	Spec = ~spec,
	none = ~parent_ipexp,
	set_default_opts,
	% TODO: use errlog action protect 
%	display(user_error, '{Reading absmach definition '),
%	display(user_error, Spec),
%	display(user_error, '}'), nl(user_error),
	Errs = ~memo.errs,
	eval_file(prolog_source(Spec), PlName),
	'$open'(PlName, r, Stream),
	flagcontext__new(OldFlags),
 	call((
          errs :: any <- Errs,
	  stream :: any <- Stream,
	  in :: accum <- [],
	  process :: any <- true,
	  process_sentences
	)),
	flagcontext__switch(OldFlags),
%	display(user_error, '{ok}'), nl(user_error),
	close(Stream),
	post_load.
}.

% set the default absmach options
set_default_opts :-
	set_general_opts,
	set_tags_opts.

% Option set with a minimum number of instructions
% TODO: it could be reduced further
opts__minimum_iset :-
	set_opt(joinconts),
	set_opt(op32),
	set_opt(no_padding).
% This seems one of the fastest option set 
opts__fast :-	
	set_opt(joinconts),
	set_opt(omerg),
	set_opt(urules),
	set_opt(op32),
	set_opt(no_padding).
% This seems one of the fastest option set 
opts__fast_specrw :-	
	set_opt(joinconts),
	set_opt(omerg),
	set_opt(urules),
	set_opt(op32),
	set_opt(no_padding),
	set_opt(specmode).
% The DEFAULT option set
opts__default :-
	set_opt(varops),
	set_opt(omerg),
	set_opt(joinconts),
	set_opt(urules),
	set_opt(specmode). % TODO: cannot be disabled with compilation to C (due to r_h)
%	  errlog:trace(['specmode has been temporally removed'])

% TODO: Improve the (ABSMACH_OPTGRP, ABSMACH_OPTS) method to set absmach options
set_general_opts :-
	( get_opts_for_grp(general) ->
	    true
	; %
	  %opts__minimum_iset
	  %opts__fast
	  %opts__fast_specrw
	  opts__default
          % Other option sets
%	  set_opt(varops),
%%	  set_opt(threaded_bytecode),
%%	  set_opt(p_in_reg), % slower!!?
%	  set_opt(omerg),
%	  set_opt(joinconts),
%	  set_opt(urules),
%	  set_opt(specmode), % TODO: cannot be disabled with compilation to C (due to r_h)
%	  errlog:trace(['default absmach options temporally changed'])
	).
set_tags_opts :-
	( get_opts_for_grp(tags) ->
	    true
	; set_opt(tagscheme1) % default
%	; set_opt(tagscheme3)
%	; set_opt(tagscheme22), % basic halfword
%	; set_opt(tagscheme7), % basic extgc
%	; set_opt(tagscheme25), % halfword with extgc
%	; set_opt(tagscheme28),
%	; set_opt(tagscheme37),
%	; set_opt(tagscheme31),
%	; set_opt(tagscheme40),
%	; set_opt(tagscheme46), % split bit
%	  errlog:trace(['default tagscheme has been temporally changed'])
	).

get_opts_for_grp(OptGrp) :-
	getenvstr('ABSMACH_OPTGRP', OptGrpCodes),
	atom_codes(OptGrp, OptGrpCodes),
	getenvstr('ABSMACH_OPTS', OptsCodes),
	get_opts_for_grp__2(OptGrp, OptsCodes).

get_opts_for_grp__2(general, OptsCodes) :-
	( number_codes(X, OptsCodes) ->
            % encoding of some options as numbers (for automated
            % benchmarking via ABSMACH_OPTS environment variable)
	    optbit(varops,1,X),
	    optbit(iblt,2,X),
	    optbit(omerg,4,X),
	    optbit(joinconts,8,X),
	    optbit(urules,16,X),
	    optbit(specmode,32,X)
	; ptoc__impcomp:error(['wrong value for environment variable ABSMACH_OPTS!'])
	).
get_opts_for_grp__2(tags, OptsCodes) :-
	atom_codes(TagScheme, OptsCodes),
	set_opt(TagScheme).

optbit(Opt,Bit,X) :- X /\ Bit =\= 0, !, set_opt(Opt).
optbit(_,_,_).

% ===========================================================================
% The reader of (ImProlog) sentences

% additional state of the reader (empty)
:- mixin modread_ctx {
}.

:- include(.(process_sentences)).

process_sentences__error(X) :- ptoc__impcomp:error(X).

treat_sentence(Sentence) :-
	Sentence = sentence(X, _, _, _, _),
	treat_sentence__2(X).

treat_sentence__2(':-'(A, B)) :- !,
	add_clause(A, B).
treat_sentence__2(':-'(Decl)) :- !,
	treat_decl(Decl).
treat_sentence__2(':='('$use_opt', A0)) :- !, % TODO: transform :=, then extract solutions and assert them with set_opt
	fundisj_to_list(A0, A),
	set_opt_list(A).
treat_sentence__2('$use_opt'(A)) :- !,
	set_opt(A).
treat_sentence__2(':='(Head, B)) :- !,
	functor(Head, N, A0), A is A0 + 1,
	Head =.. [_|Xs],
	add(fun_body(N, A, Xs, B)).
treat_sentence__2(A) :-
	add_clause(A, true).

:- use_module(engine(rt_exp), ['$module_concat'/3]).

% TODO: trying to emulate assertion language... use real assertions!
treat_decl(Decl) :-
	nested_decl(Decl, Name, Keyword, Sentences),
	mod__nested(Keyword, Modifier),
	!,
	treat_nested(Name, Modifier, Sentences).
treat_decl(op(P, F, O)) :- !,
	flagcontext__do(op(P, F, O)).
treat_decl(push_prolog_flag(Flag, Value)) :- !,
	flagcontext__do(push_prolog_flag(Flag, Value)).
treat_decl(function(N/A)) :- !, % TODO: deprecate
	add(fun_eval(N, A)).
treat_decl(fun_eval(N/A)) :- !, % use functional notation for N/(A+1) predicate
	add(fun_eval(N, A)).
treat_decl(pred(Decl0)) :-
	sum_to_list(Decl0, Decl1),
	Decl1 = [Pred|Props],
	( Pred = N/A ->
	    true
	; functor(Pred, N, A),
	  Pred =.. [_|As],
	  add(op__format(N,As))
	),
	process_props(Props, N, A).
treat_decl(foreign_include_in_header(Spec)) :- !,
	add(foreign_include_in_header(Spec)).
treat_decl(iter(A,B,C)) :- !,
	add(op__iter(A,B,C)).
treat_decl(iset(N/_)) :- !,
	add(iset(N)).
	% TODO: redefine as pred props?
treat_decl(lowmacro(F/N,Imp,Types,CName,MArgs)) :- !,
	add(entry_lowmacro(F,N,Imp,Types,CName,MArgs)).
treat_decl(lowtype(Name)) :- !,
	add(def_lowtype(Name)).
treat_decl(lowmacrofact(N/A)) :- !, % export to C the option value, when enabled
	add(entry_lowmacrofact(N,A)).
treat_decl(defmode(Ins, M)) :- !, % TODO: temporal?
	add(defmode(Ins, M)).
treat_decl(bitstr_or(N, EncodingParent, Disj)) :- !,
	add(ltype__kind(N, bitstr)),
	add(ltype__disj(N)),
	add(ltype__encoding_type(N, EncodingParent)),
	N1 =.. [N,_],
	% TODO: do not use predicates
	add_clause(N1, '$or'(Disj)).
treat_decl(type(F/_N) + enum) :- !,
	add(ltype__kind(F, enum)).
treat_decl(type(F/_N) + subenum(Parent)) :- !,
	add(ltype__kind(F, enum)),
	add(ltype__encoding_type(F, Parent)).
treat_decl(type(F/_N) + predabs) :- !,
	add(ltype__kind(F, predabs)).
treat_decl(type(F/_N) + equiv) :- !,
	add(ltype__kind(F, equiv)).
treat_decl(enum_encode(F/_, EncodeList)) :- !,
	add_enum_domain__encode(F, EncodeList). % TODO: kludge implementation...
treat_decl(foreigntype(Name, CName)) :- !,
	add(ltype__kind(Name, foreign(CName))).
treat_decl(lowatom(N, CName)) :- !,
	add(def_lowatom(N, CName)).
treat_decl(lowinclude(Where, Spec0)) :- !,
	find_source(Spec0, Spec),
	add(lowinclude(Where, spec(Spec))).
treat_decl(lowinclude_foreign(Where, Filename)) :- !,
	add(lowinclude(Where, foreign(Filename))).
treat_decl(blt_dec(M:A/B,C)) :- !,
	'$module_concat'(A,M,MA),
	add(blt_dec(MA,B,C)).
treat_decl(iterator_def(Name, I, Init, Cond, Next, MaybeEmpty)) :- !,
	add(iterator_def(Name, I, Init, Cond, Next, MaybeEmpty)).
treat_decl(swrule_def(SwitcherKeys, SwitcherJoin, Switcher0)) :- !,
	( Switcher0 = macro(N, Args, Mode, Cases) ->
	    add_swrule_macro(N, Args, Mode, Cases),
	    Switcher1 = N
	; Switcher1 = Switcher0
	),
	add(swrule_def0(SwitcherKeys, SwitcherJoin, Switcher1)).
treat_decl(swtest_mode(Mode)) :- !,
	del(swtest_mode(_)),
	add(swtest_mode(Mode)).
treat_decl(tagtestdefcustom(Head, Body)) :- !,
	add_tagtestdefcustom(Head, Body).
treat_decl(enctagtest(Pos, Post0, Pre0, Cost, Var, Body)) :- !,
	add_enctagtest(Pos, Post0, Pre0, Cost, Var, Body).
treat_decl(swtest_macro(CName, Pre, Post)) :- !,
	add_swtest_macro(CName, Pre, Post).
treat_decl((decl_opt(_) # _)) :- !,
	true. % TODO: do not ignore
treat_decl(pointer_size(X)) :- !,
	add(pointer_size(X)).
treat_decl(predspec_atom1(Pred, SpecProps)) :- !,
	add_predspec_atom1(Pred, SpecProps).
treat_decl(globalvar(F/N) + lowentry(Type, CName) + prop(foreign__static)) :- !, % TODO: generalize
	add_globalvar(F, N, Type, CName, just(static)).
treat_decl(globalvar(F/N) + lowentry(Type, CName)) :- !,
	add_globalvar(F, N, Type, CName, no).
treat_decl(Decl) :- !,
	ptoc__impcomp:error(['unknown ipexp declaration ', Decl]).

treat_nested(Name, ftype, Sentences) :- !,	
	treat_ftype(Name, Sentences).
treat_nested(Name, Modifier, Sentences0) :-
	( Modifier = mod_mixin ->
	    % note: this definition can be included anywhere with mixin_extends
	    % TODO: a mixin is not a real classes, do something special here
	    DefName = Name
	; Modifier = abstract_class ->
	    DefName = ~atom_concat(Name, '__invariant')
	; Modifier = mod_class ->
	    DefName = Name
	; Modifier = union ->
	    DefName = Name,
	    add(ltype__kind(Name, union))
	; fail
	),
	filter_class_sentences(Sentences0, Name, Sentences1),
	add(ltype__sentences(DefName, Sentences1)).

filter_class_sentences([], _Name, []).
filter_class_sentences([D0|Ds0], Name, Ds) :-
	D0 = sentence(D, _, _, _, _),
	( D = (:- extends(Parent)) ->
	    add(ltype__parent(Name, Parent)),
	    N1 =.. [Name,T],
	    P1 =.. [Parent,T],
	    add_clause(P1, N1),
	    Ds = [D0|Ds1]
	; D = (:- bitstr) ->
	    add(ltype__kind(Name, bitstr)),
	    Ds = Ds1
	; D = (:- struct) ->
	    add(ltype__kind(Name, struct)),
	    Ds = Ds1
	; Ds = [D0|Ds1]
	),
	filter_class_sentences(Ds0, Name, Ds1).

% ---------------------------------------------------------------------------
% TODO: Merge with compiler/frontend_common

:- doc(subsection, "Module Declaration Handlers").
% This is an extensible interface for different kind of nested
% modules.

:- static mod__treatDom/1. % In the domain of mod__treat/3
:- discontiguous mod__treatDom/1.
:- discontiguous mod__treat/3. % Treat module declaration

% :- pred mod__nested(Keyword, Modifier) # "@var{Keyword} declares a
%    nested module with modifier @var{Modifier}".
:- static mod__nested/2.
:- discontiguous mod__nested/2.

% ---------------------------------------------------------------------------

% TODO: make it extensible
mod__nested(ftype, ftype).
mod__nested(mixin, mod_mixin).
mod__nested(abstract_class, abstract_class).
mod__nested(class, mod_class).
mod__nested(union, union).

% (from Ciao)
%mod__nested(module, mod_static).
%mod__nested(class, mod_class).
%mod__nested(interface, mod_interface).
%mod__nested(mixin, mod_mixin).

mod__treatDom(_) :- fail.
mod__nested(_, _) :- fail.
mod__treat(_) :- fail.

% ---------------------------------------------------------------------------
% Reader of ftype blocks

% additional state of the reader (empty)
:- mixin f_modread_ctx {
}.

:- include(.(process_sentences_f)).

f_process_sentences__error(X) :- ptoc__impcomp:error(X).

:- public filter_sentences/2.
filter_sentences(Sentences0, Sentences) :-
        ( call((
	    process :: any <- true,
	    in :: accum <- Sentences0,
	    out :: accum(Sentences),
	    f_process_sentences
	  )) ->
	     true
	; ptoc__impcomp:error(['sentence filtering failed'])
	).

{ 
:- fluid out :: accum.
f_treat_sentence(Sentence) :-
	out.add(Sentence).
}.

treat_ftype(Name, Sentences0) :-
	ftype_name :: any <- Name,
	filter_sentences(Sentences0, Sentences),
	treat_ftype__2(Sentences).
{
:- fluid ftype_name :: any.
treat_ftype__2([]).
treat_ftype__2([Sentence|Ds]) :-
	% TODO: refine the types
	% Note on types:
        %   ftype_instance # "Instance of a particular ftype (e.g., f_x(1))
	Sentence = sentence(Decl, _, _, _, _),
	( Decl = id(B) -> % id(~num)
	    add(ftype__id(~ftype_name,B))
	%
	; Decl = def(B) -> % def(?)
	    add(ftype__def(~ftype_name,B))
	%
	; Decl = dectype(B) -> % dectype(~type)
	    add(ftype__dectype(~ftype_name,B))
	; Decl = enctype(B) -> % enctype(~type)
	    add(ftype__enctype(~ftype_name,B))
	; Decl = smethod(B) -> % smethod(~type)
	    add(ftype__smethod(~ftype_name,B))
	; Decl = lmethod(B) -> % lmethod(~type)
	    add(ftype__lmethod(~ftype_name,B))
	%
	; Decl = zero(A) -> % zero(~ftype_instance)
	    add(ftype__zero(A))
	; Decl = glb(A,B,C) ->
	    % glb(~ftype, ~ftype, ~ftype)
	    % glb(~ftype_instance, ~ftype, ~ftype_instance)
	    add(ftype__glb(A,B,C))
	%
	; Decl = decfun(B) -> % decfun(?)
	    add(ftype__decfun(~ftype_name,B))
	; Decl = decfun0(A,B,C) -> % decfun0(~ftype_instance,?,?)
	    add(ftype__decfun0(A,B,C))
	; Decl = dec_pat(B,C) -> % dec_pat(?,?)
	    add(dec_pat(~ftype_name,B,C))
	; Decl = dec_pat0(A,B) -> % dec_pat0(~ftype_instance,?)
	    add(dec_pat0(A,B))
	; Decl = encfun(B) -> % encfun(?)
	    add(ftype__encfun(~ftype_name,B))
	%
	; ptoc__impcomp:error(['unknown ftype decl ', Decl])
	),
	treat_ftype__2(Ds).
}.

% ---------------------------------------------------------------------------

% TODO: improve! allow any goal in guards? use MOST SPECIFIC ORDERING for goals...
% TODO: similar to C++ template instantiation?
% TODO: add automatically in a different way... (a-la specialization) (however: this declaration is interesting as a way to implement psyco/hotspot-like optimizations: see below)
add_predspec_atom1(Pred, SpecProps) :-
	functor(Pred, PredN, PredA),
	Pred =.. [_|Args],
	Args = [SpecAtom|RestArgs],
	PredA1 is PredA - 1,
	Pred0 =.. [PredN,SpecArg|RestArgs],
	( atom(SpecAtom) ->
	    true
	; ptoc__impcomp:error(['unsupported spec in predspec_atom1: ', Pred])
	),
	atom_concat(PredN, ~atom_concat('__', SpecAtom), SpecPredN),
	atom_concat(PredN, '__', PredDefN),
	PredDef =.. [PredDefN,SpecAtom|RestArgs],
	SpecPred =.. [SpecPredN|RestArgs],
	% always unfold that predicate to
	% TODO: propargs here is probably incorrect too
	treat_decl((pred PredN/PredA + prop(unfold) + prop(propargs))), % TODO: use other thing different than prop(unfold) to implement psyco/hotspot-like optimizations? e.g. if the input is a number, then use a very optimized version, if not, use bytecode and more generic code
	% specialization rule
        treat_sentence__2((Pred0 :- SpecArg = SpecAtom, !, SpecPred)),
	treat_sentence__2((Pred0 :- '$error'(no_spec(Pred)))),
        % (emit specialized version)
        treat_decl((pred SpecPredN/PredA1 + SpecProps)),
        treat_sentence__2((SpecPred :- PredDef)),
        % (generic code)
        treat_decl((pred PredDefN/PredA + prop(unfold) + prop(propargs))).

add_globalvar(F, N, Type, CName, CProps) :-
	atom_concat(F, '__', FInit),
	treat_decl((pred F/N + lowentryvar(FInit, Type, CName, CProps) + prop(no_worker))).

% Add the bitmask type definitions for a enumerable atom domain
:- public process_enum_domain/2.
process_enum_domain(Name, List) :-
	add_enum_domain__2(List, 0, Name).

add_enum_domain__2([Atom|Atoms], Index, Name) :- !,
	Mask is 1<<Index,
	add(enum_domain__atom_to_mask(Atom, Name, Mask)),
	add(enum_domain__mask_to_atom(Mask, Name, Atom)),
	Index2 is Index + 1,
	add_enum_domain__2(Atoms, Index2, Name).
add_enum_domain__2([], Index, Name) :-
	% total number of elements
	add(enum_domain__elems(Name, Index)).

add_enum_domain__encode(Name, List) :-
	add_enum_domain__encode__2(List, Name).

add_enum_domain__encode__2([(Atom,Encoded)|Xs], Name) :- !,
	add(enum_domain__encode_atom(Atom, Name, Encoded)),
	add_enum_domain__encode__2(Xs, Name).
add_enum_domain__encode__2([], _Name).

:- static lowpred_modes/2.
lowpred_modes([],[]).
lowpred_modes([_|Ts],[in|Ms]) :- lowpred_modes(Ts,Ms).

% TODO: temporal...
:- static gen_iany/2.
gen_iany(0, []) :- !.
gen_iany(I, [iany|Xs]) :- I1 is I - 1, gen_iany(I1, Xs).

process_props([], _, _).
process_props([Prop|Props], N, A) :-
	process_prop(Prop, N, A),
	process_props(Props, N, A).

process_prop(Prop, N, A) :-
	( Prop = foreignmacro(Types, Macromodes,CName) ->
%	    gen_iany(A, Types),
	    add(op__foreign(N, A, foreignmacro(Types, Macromodes, CName)))
% TODO: remove
%	; Prop = foreignfun(Type,CName) ->
%	    errlog:trace(['TODO: deprecate process_prop with ', Prop]),
%	    A1 is A - 1,
%	    gen_iany(A1, InTypes),
%	    add(op__foreign(N, A, foreignfun(InTypes, Type, CName)))
	; Prop = foreignfun(InTypes, Type, CName) ->
	    add(op__foreign(N, A, foreignfun(InTypes, Type, CName)))
	; Prop = foreigncons(Type,CName) ->
	    add(op__foreign(N, A, foreigncons(Type, CName)))
	; Prop = foreignvar(Type,CName) ->
	    add(op__foreign(N, A, foreignvar(Type,CName)))
	; Prop = foreign(Types, Imp,CName) ->
%	    gen_iany(A, Types),
	    add(op__foreign(N, A, foreignp(Types, Imp, CName)))
	; Prop = prop(X) ->
	    add(op__prop(N, A, X))
	; Prop = foreignfun_spec(InTypes, Ret, CIn, CExpr) ->
	    add(op__foreignfun_spec(N, A, InTypes, Ret, CIn, CExpr))
	; Prop = lowentry(Imp,Types,CName) ->
	    add(op__foreign(N, A, foreignp(Types, Imp, CName))),
	    lowpred_modes(Types, Modes),
	    add(entry_lowpred(N,A,Imp,Types,Modes,CName))
	; Prop = lowentryvar(FInit,Type,CName,CProps) ->
	    add(entry_lowinst(FInit,A,Type,CName,CProps)),
	    add(op__foreign(N, A, foreignvar(Type,CName)))
	; Prop = lowentryfun(InTypes,OutType,CName) ->
	    add(op__foreign(N, A, foreignfun(InTypes,OutType,CName))),
	    lowpred_modes(InTypes, InModes),
	    Types = ~append(InTypes, [OutType]),
	    Modes = ~append(InModes, [out]),
	    add(entry_lowpred(N,A,det,Types,Modes,CName))
	; Prop = lowentrymacrofuncons(InTypes, Type, CName) -> % a macro fun whose code is generated using macrocons... a kludge for special definitions!
	    add(op__foreign(N, A, foreignfun(InTypes, Type, CName))),
	    add(entry_lowmacrocons(N,A,CName))
% TODO: remove
%	; Prop = lowentrymacrofuncons(Type, CName) -> % a macro fun whose code is generated using macrocons... a kludge for special definitions!
%	    A1 is A - 1,
%	    errlog:trace(['TODO: deprecate process_prop with ', Prop]),
%	    gen_iany(A1, InTypes),
%	    add(op__foreign(N, A, foreignfun(InTypes, Type, CName))),
%	    add(entry_lowmacrocons(N,A,CName))
	; Prop = lowentrymacrocons(Type, CName) ->
	    add(op__foreign(N, A, foreigncons(Type, CName))),
	    add(entry_lowmacrocons(N,A,CName))
	; Prop = lowentrymacrofun(InTypes, OutType, CName) ->
	    add(op__foreign(N, A, foreignfun(InTypes, OutType, CName))),
	    add(entry_lowmacrofun(N,A,InTypes,CName))
	; ptoc__impcomp:error(['unknown property ', Prop, ' of predicate ', (N/A)])
	).  

process_iset_body(Decl) :-
	( Decl = (A, B) ->
	    process_iset_body(A),
	    process_iset_body(B)
	; Decl = (Cond -> DeclTrue ; DeclFalse) ->
	    process_cond(Cond, CondStatus),
	    ( CondStatus = true ->
	        process_iset_body(DeclTrue)
	    ; CondStatus = fail ->
	        process_iset_body(DeclFalse)
	    ; CondStatus = unknown ->
	        ptoc__impcomp:error(['cannot evaluate at compile time: ', Cond])
	    )
	; Decl = entry(Insns0,Def00) ->
	    sum_to_list(Insns0, Insns),
	    add(iset_entry(Insns, Def00, normal))
	; Decl = entry(Insns0, Def00, AlignDef) ->
	    sum_to_list(Insns0, Insns),
	    add(iset_entry(Insns, Def00, AlignDef))
	; Decl = illop(A) ->
	    add(iset_illop(A))
	; Decl = exported_ins(Insns0, CName) -> % exported instruction
	    sum_to_list(Insns0, Insns),
	    add(iset_exported(Insns, CName, none))
	; Decl = exported_ins(Insns0, CName, Prop) -> % exported instruction
	    sum_to_list(Insns0, Insns),
	    add(iset_exported(Insns, CName, Prop))
	; Decl = catch_fail(Insns0) ->
	    sum_to_list(Insns0, Insns),
	    add(iset_catch_fail(Insns))
	; Decl = true ->
	    true
	; ptoc__impcomp:error(['unknown iset decl ', Decl])
	).

export_bitstr(BaseType) :-
	tp_analyze(BaseType),
	  fail.
export_bitstr(BaseType) :-
	ltype__bitstr__storage(BaseType, Member, bitmember(SizeVal, OffVal)),
	  export_bitstr_member_param(BaseType, Member, offset, intmach, OffVal),
	  export_bitstr_member_param(BaseType, Member, size, intmach, SizeVal),
	  fail.
export_bitstr(BaseType) :-
	ltype__bitstr__storage(BaseType, Member, splitbitmember(Size1, Off1Val, Size2, Off2Val)),
	  export_bitstr_member_param(BaseType, Member, is_split, intmach, 1),
	  export_bitstr_member_param(BaseType, Member, size1, intmach, Size1),
	  export_bitstr_member_param(BaseType, Member, offset1, intmach, Off1Val),
	  export_bitstr_member_param(BaseType, Member, size2, intmach, Size2),
	  export_bitstr_member_param(BaseType, Member, offset2, intmach, Off2Val),
	  fail.
export_bitstr(BaseType) :-
	ltype__bitstr__smallptr_model(BaseType, Model),
	  define_smallptr_model(Model),
	  fail.
export_bitstr(BaseType) :-
	ltype__bitstr__size(BaseType, Size),
	  export_bitstr_param(BaseType, size, intmach, Size),
	  fail.
export_bitstr(_).

export_bitstr_param(BaseType, Param, _Type, Val) :-
	CName = ~bitstr_param_cname(BaseType, Param),
	add(entry_lowmacrocons(CName, 1, CName)),
	add(fun_body(CName, 1, [], Val)).

export_bitstr_member_param(BaseType, Member, Param, _Type, Val) :-
	CName = ~bitstr_member_param_cname(BaseType, Member, Param),
	add(entry_lowmacrocons(CName, 1, CName)),
	add(fun_body(CName, 1, [], Val)).

:- static bitstr_param_cname/3.
bitstr_param_cname(BaseType, Param, CName) :-
	CName = ~atom_concat(~atom_concat(BaseType, '__'), Param).

:- static bitstr_member_param_cname/4.
bitstr_member_param_cname(BaseType, Member, Param, CName) :-
	CName = ~atom_concat(~atom_concat(~atom_concat(~atom_concat(BaseType, '__'), Member), '_'), Param).

define_smallptr_model(model(ModelUpper, ModelLower)) :-
	number_codes(ModelUpper, UpperCodes),
	atom_codes(UpperAtom, UpperCodes),
	Base = ~funcall('$ccons'(~atom_concat('MallocBase', UpperAtom), intmach)),
	BlockSize = ~funcall('$ccons'(~atom_concat('MIN_MEM_ALLOC_', UpperAtom), intmach)),
	add(entry_lowmacrocons('SMALLPTR_BASE', 1, 'SMALLPTR_BASE')),
	add(fun_body('SMALLPTR_BASE', 1, [], Base)),
	add(entry_lowmacrocons('OWNMALLOC_BLOCKSIZE', 1, 'OWNMALLOC_BLOCKSIZE')),
	add(fun_body('OWNMALLOC_BLOCKSIZE', 1, [], BlockSize)),
	add(entry_lowmacrocons('OWNMALLOC_MmapAllowed', 1, 'OWNMALLOC_MmapAllowed')),
	add(fun_body('OWNMALLOC_MmapAllowed', 1, [], ~funcall('$ccons'(~atom_concat('MmapAllowed', UpperAtom), intmach)))),
	add(entry_lowmacrocons('OWNMALLOC_MmapSize', 1, 'OWNMALLOC_MmapSize')),
	add(fun_body('OWNMALLOC_MmapSize', 1, [], ~funcall('$ccons'(~atom_concat('MmapSize', UpperAtom), intmach)))),
	add(entry_lowmacrocons('SMALLPTR_UPPERBITS', 1, 'SMALLPTR_UPPERBITS')),
	add(fun_body('SMALLPTR_UPPERBITS', 1, [], ModelUpper)),
	add(entry_lowmacrocons('SMALLPTR_LOWERBITS', 1, 'SMALLPTR_LOWERBITS')),
	add(fun_body('SMALLPTR_LOWERBITS', 1, [], ModelLower)).

% TODO: merge with compiler/frontend
find_source(RelUspec, Spec) :-
	nonvar(RelUspec),
	FromSpec = ~spec,
	store:find_source(RelUspec, relspec(FromSpec), Spec).

% TODO: merge with compiler/frontend (options?)
set_opt(A) :- atom(A), !,
	add(use_opt(A)).

set_opt_list([]) :- !.
set_opt_list([A|As]) :- !,
	set_opt(A),
	set_opt_list(As).

% TODO: do not export!
:- public add_clause/2.
add_clause(Head, Body) :-
	functor(Head, N, A),
	% ensure pred defined
	( op__format(N, _) ->
	    true
	; default_format(A, As),
	  add(op__format(N,As))
	),
	% remove cached info
	functor(CleanHead, N, A),
	del(cached_pred_body(CleanHead, _)),
	% add predicate clause
	add(pred_clause(Head, Body)).

% get the definition of a expression (for unfolding)
:- public exp_unfold/2.
exp_unfold('$ins_info', R) :- !,
	get_sortedins(SortedIns),
	ins_info(SortedIns, R).
exp_unfold('$ins_name_array', R) :- !,
	get_sortedins(SortedIns),
	ins_name_elems(SortedIns, SortedIns1),
	R = ~funcall('$array_elems'(~funcall('$array'(ref1(mut(char)), SortedIns1)))).
exp_unfold('$x_offset_from_worker', R) :- !,
	( x_offset_from_worker(R) -> true ).
exp_unfold('$y_offset_from_frame', R) :- !,
	( y_offset_from_frame(R) -> true ).
exp_unfold('$enabled_opts_cstring'(Opts), R) :- !,
	exp_unfold__enabled_opts_cstring(Opts, R).
% TODO: rename by opmax?
exp_unfold('$ins_opcount', R) :- !,
	get_opcount(R).
exp_unfold('$ins_opcode'(Ins), R) :- !,
	( ins_op(Ins, Opcode) ->
	    R = Opcode
	; ptoc__impcomp:error(['ins opcode failed for ins ', Ins])
	).
exp_unfold('$ins_exported_opcodes'(X), R) :- !,
	get_usedins(UsedIns),
	ins_exported_opcodes__cases(UsedIns, Cases),
	R = ~funcall('$keytable'(X, Cases)).
% TODO: generate c_term code automatically?
% TODO: it is a goal, not an expression! (lowmacro, not a lowmacrocons)
exp_unfold('$ic_exported_emit_op'(X), R) :- !,
	findall(e(Ins, CName, Prop), ic_exported(Ins, CName, Prop), Es),
	ic_exported_emit_op__cases(Es, Cases),
	R = ~funcall('$keytable'(X, Cases)).

ic_exported_emit_op__cases([e(Ins, CName, Prop)|Xs], [R|Rs]) :- !,
	( ins_padding(Ins, Padding) -> true ; Padding = [] ),
	( Padding = [pad(Pad,ModPad)] ->
	    number_codes(Pad, PadCodes), atom_codes(PadAtom, PadCodes),
	    number_codes(ModPad, ModPadCodes), atom_codes(ModPadAtom, ModPadCodes),
	    Emit0 = ~atom_concat('EMIT_opcode_pad', ~atom_concat(PadAtom, ModPadAtom)),
	    Emit = ( Prop = a0 ? % generate code only for the 0-aligned opcode
		       ~atom_concat(Emit0, '_A0')
		   | Emit0 % TODO: do not ignore other props
		   ),
	    R1 = ~funcall('$inline_expr'(call(Emit,[CName]), intmach))
	; Padding = [] ->
	    Emit = 'EMIT_opcode',
	    R1 = ~funcall('$inline_expr'(call(Emit,[CName]), intmach))
	; ptoc__impcomp:error(['$ins_emit_op not supported for instruction with variable size operands: ', Ins])
	),
	R = case(CName, R1),
	ic_exported_emit_op__cases(Xs, Rs).
ic_exported_emit_op__cases([], []).

% cstring showing enabled options filtered from Opts
exp_unfold__enabled_opts_cstring(Opts, R) :- !,
	findall(Opt, enabled_opts_in_set(Opts, Opt), Opts2),
	opts_concat(Opts2, OptsAtom),
	atom_codes(OptsAtom, Codes),
	R = ~funcall('$cstring'(Codes)).

:- static opts_concat/2.
% Obtain a single identifier for a sequence of options
opts_concat([A]) := A :- !.
opts_concat([A|As]) := ~atom_concat(A, ~atom_concat('|', ~opts_concat(As))) :- !.

enabled_opts_in_set(Opts, Opt) :-
	member(Opt, Opts),
	get__use_opt(Opt).

% Instruction name table
% TODO: include ftype names (any other info?)
ins_name_elems([], []).
ins_name_elems([X|Xs], [Y|Ys]) :-
	( iset_illop(X) ->
	    Y = ~funcall('$cstring'("?")) % TODO: write an entry for illop?
	; atom_codes(X, Codes),
	  Y = ~funcall('$cstring'(Codes))
	),
	ins_name_elems(Xs, Ys).

ins_pa_elems([], []).
ins_pa_elems([X|Xs], [X1|Ys]) :-
	( iset_illop(X) ->
	    X1 = X
	; X1 = '$insc'(X)
	),
	ins_pa_elems(Xs, Ys).

ins_exported_opcodes__cases([], []).
ins_exported_opcodes__cases([X|Xs], Ys) :-
	( ins_exported(X, CName) ->
	    Ys = [Y|Ys0],
	    Y = case(CName, ~funcall('$ins_opcode'(X)))
	; Ys = Ys0
	),
	ins_exported_opcodes__cases(Xs, Ys0).

% get the definition of a goal (for unfolding)
:- public goal_unfold/2.
goal_unfold('$insswitch'(Var), R) :- !,
	R = ~ins_switch(Var).
goal_unfold('$decl_ins_pa_array'(CProps, IL), R) :- !,
	get_sortedins(SortedIns),
	ins_pa_elems(SortedIns, SortedIns1),
	R = '$decl_pa_array'(CProps, IL, SortedIns1).

% Switch-based code to jump to the instruction code
ins_switch(Var, R) :-
	get_usedins(UsedIns),
	( iset_illop(IllOp) -> true ),
	call(( cases :: accum(Cases), ins_switch__2(UsedIns) )),
	R = '$switch'(Var, Cases, '$sub2call'(IllOp)).
{
:- fluid cases :: accum.
ins_switch__2([]).
ins_switch__2([X|Xs]) :- ins_switch__3(X), ins_switch__2(Xs).

ins_switch__3(Ins) :-
	( ins_op(Ins, Opcode) -> true ),
	cases.add(case(Opcode, '$sub2call'('$insc'(Ins)))).
}.

:- static default_format/2.
default_format(0, []) :- !.
default_format(I, [m|Fs]) :-
	I1 is I - 1,
	default_format(I1, Fs).

% processed instruction set 
:- data ins_prior/2.
:- public data ins_op/2. % used by apps/ciaodump
:- data ins_def/3.
:- data ins_format/2.
:- data ins_padding/2.
:- data ins_aligned/3.
:- data ins_exported/2.

post_load :-
	% precompute tagged_size
	( S = ~tp_size(tagged) ->
	    true
	; ptoc__impcomp:error(['unknown size of native type tagged'])
	),
	  add(tagged_size(S)),
	  fail.
post_load :-
	% precompute bignum_unit_size
	( S = ~tp_size(bignum) ->
	    true
	; ptoc__impcomp:error(['unknown size of native type bignum'])
	),
	  add(bignum_unit_size(S)),
	  fail.
post_load :-
	% precompute numsize
        ( ltype__bitstr__storage(tagged, num, bitmember(V, _)) ->
	    true
	; ptoc__impcomp:error(['bit size of tagged.num cannot be calculated at compile time'])
	),
	  add(num_bits(V)),
	  fail.
post_load :-
	% TODO: wrong, just compute the relative position up to the X field
	( WorkerSize = ~tp_size(worker) ->
	    add(x_offset_from_worker(WorkerSize))
	; ptoc__impcomp:error(['cannot compute size of worker'])
	),
	% TODO: wrong, just compute the relative position up to the Y field
	( FrameSize = ~tp_size(frame) ->
	    add(y_offset_from_frame(FrameSize))
	; ptoc__impcomp:error(['cannot compute size of frame'])
	),
	fail.
post_load :-
	load_iset,
	fail.
post_load :-
	load_absnext,
	fail.
post_load :-
	% generate macros for bitstr access
	% TODO: export only read, write, etc. methods, in ptoc__impcomp
	ltype__kind(Type, bitstr),
	  % TODO: do not generate macros for subtype definitions
	  \+ ltype__parent(Type, _),
	  \+ ltype__disj(Type),
	  export_bitstr(Type),
	  fail.
post_load :-
	% generate macros to make ipexp options visible to user C code
	% TODO: generalize for other facts
	entry_lowmacrofact('$use_opt', 1), % option exported to C
	  use_opt(Opt),
	    atom_concat('ABSMACH_OPT__', Opt, CName),
	    add(entry_lowmacrocons(CName, 1, CName)),
	    add(fun_body(CName, 1, [], 1)),
	    fail.
post_load :-
	% precompute swrule_def and swjoinable_def
        swrule_def0(SwitcherKeys, SwitcherJoin, Switcher0),
	  ( SwitcherJoin = join(Joinable), \+ swjoinable_def(Joinable) ->
	      add(swjoinable_def(Joinable))
	  ; true
	  ),
	  ( eval_switcher_keys(SwitcherKeys, SwitcherKeys2, SwitcherXType1) ->
	      true
	  ; ptoc__impcomp:error(['cannot evaluate switcher keys', SwitcherKeys])
	  ),
	  ( Switcher0 = swcondtt(Pre, Post) ->
	      PreMask = ~tagdisj_to_encode_mask(Pre),
	      PostMask = ~tagdisj_to_encode_mask(Post),
	      Switcher1 = swcond(~tagtest_macro_id(PreMask, PostMask))
	  ; Switcher1 = Switcher0
	  ),
	  add(swrule_def(SwitcherKeys2, SwitcherXType1, SwitcherJoin, Switcher1)),
	  fail.
post_load.

% TODO: improve, make it work even when the last one is not open? (that depends on how the rule matching is implemented in ptoc__impcomp)
% evaluate keys and obtain the lub of all of them
eval_switcher_keys([X], [_], T) :- !, % leave open the last one
	T = ~tp_eval(def_enum('tagged_$key', X)).
eval_switcher_keys([X|Xs], [T|Ts], Lub) :-
	T = ~tp_eval(def_enum('tagged_$key', X)),
	eval_switcher_keys(Xs, Ts, Lub0),
	Lub = ~tp_lub(T, Lub0).

% generate instruction set definitions from the iset entries
% (add padding, collapsing rules, etc.)
% TODO: anything else?
% TODO: delete unneeded data when no necessary?
load_iset :-
	% reset ins opcode counter
	del(ins_current_op(_)),
	add(ins_current_op(0)),
	% reset ins priority (emit order) counter
	del(ins_current_prior(_)),
	add(ins_current_prior(0)),
	% process the instruction set
	( iset(X) -> true ),
	get_pred_def('$pr'(X, 0), [], Def),
	process_iset_body(Def),
	load_iset__entries.

load_iset__entries :-
	iset_exported(Insns, CName, Prop),
	  expand_spec(Insns, _, Ins, _, _),
	  add(ic_exported(Ins, CName, Prop)),
	  fail.
load_iset__entries :-
	iset_catch_fail(Insns),
	  expand_spec(Insns, _, Ins, _, _),
	  add(ic_catch_fail(Ins)),
	  fail.
load_iset__entries :-
	iset_entry(Insns, Def, AlignDef),
	  add_iset_entry(Insns, Def, AlignDef),
	  fail.
load_iset__entries.

add_iset_entry(Insns, Def00, AlignDef) :-
%	errlog:trace([aie(Insns, Def00, AlignDef)]),
	expand_spec(Insns, Patterns, Ins, SArgs, Format),
	add_speccollapse(Patterns, Ins, SArgs),
%	errlog:trace([iee(Ins, Format, Patterns, SArgs)]),
	% Add instructions (and padded versions)
	Exported = ( ic_exported(Ins, CName, _) ?
		       true(CName) % if the instruction is exported, export all its versions
		   | false
		   ),
	Padding = ( get__use_opt(no_padding) ? []
		  | ~get_padding([f_o|Format])
		  ),
%	errlog:trace([getpad(Padding)]),
	( Padding = impossible ->
	    errlog:bug(['impossible padding of instruction ', Insns, ' with def ', Def00, ' and aligndef ', AlignDef]),
	    fail
	; Padding = [] ->
	    % no padding is necessary
	    treat_urule(Def00, Def0),
	    add_ins(0, Ins, Format, Insns, Def0, Exported)
	; add(ins_padding(Ins, Padding)),
	  ( ( AlignDef = align(PadDef00) ; AlignDef = normal, PadDef00 = 0 ) ->
	      padding_mod(Padding, ModPad),
	      ( ModPad = 4 ->
		  add_ins(2, Ins, Format, Insns, PadDef00, Exported),
		  treat_urule(Def00, Def0),
		  add_ins(0, Ins, Format, Insns, Def0, Exported)
	      ; ModPad = 8 ->
		  add_ins(6, Ins, Format, Insns, PadDef00, Exported),
		  add_ins(4, Ins, Format, Insns, PadDef00, Exported),
		  add_ins(2, Ins, Format, Insns, PadDef00, Exported),
		  treat_urule(Def00, Def0),
		  add_ins(0, Ins, Format, Insns, Def0, Exported)
	      ; ptoc__impcomp:error(['unsupported modpad ', ModPad])
	      )
	  ; AlignDef = align0 ->
	      % generate instruction pad versions only for align 0
	      % (e.g. the instruction is always emitted from C source at 0-aligned memory, thus no more padded versions are required)
	      ( Padding = [Pad] ->
		  true
	      ; ptoc__impcomp:error(['unsupported padding ', Padding, ' for aligned instruction ', Insns])
	      ),
	      reqpad_at(Pad, 0, ReqPad),
	      treat_urule(Def00, Def0),
	      add_ins(ReqPad, Ins, Format, Insns, Def0, Exported)
	  )
	).

:- data ins_current_prior/1.
get_next_prior(Prior) :-
	( ins_current_prior(Prior) -> true ),
	NextPrior is Prior + 1,
	del(ins_current_prior(_)),
	add(ins_current_prior(NextPrior)).

:- data ins_current_op/1.
get_next_op(Op) :-
	( ins_current_op(Op) -> true ),
	NextOp is Op + 1,
	del(ins_current_op(_)),
	add(ins_current_op(NextOp)).

add_ins(PadAmount, Ins, Format, Insns, Def, Exported) :-
	get_next_op(Op),
	pad_ins(PadAmount, Format, Insns, PadFormat, PadPrefix, PadInsns),
	add_prefix(PadPrefix, '|', Ins, PadIns),
	( PadAmount > 0 ->
	    add(ins_aligned(Ins, PadAmount, PadIns)),
	    ( pad_urule(Def, PadDef) ->
	        true
	    ; ptoc__impcomp:error(['unsupported urule ', Def, ' for padded instruction ', Insns])
	    )
	; PadDef = Def
	),
	( Exported = true(CName) ->
	    add_prefix(PadPrefix, '__', CName, PadCName),
	    add(ins_exported(PadIns, PadCName))
	; true
	),
	get_next_prior(Prior),
	add(ins_prior(PadIns, Prior)),
	add(ins_op(PadIns, Op)),
	add(ins_format(PadIns, PadFormat)),
	add(ins_def(PadIns, PadInsns, PadDef)).

:- static add_prefix/4.
add_prefix('', _Sep, X, X) :- !.
add_prefix(Prefix, Sep, X0, X) :-
	X = ~atom_concat(~atom_concat(Prefix, Sep), X0).

% Increment the unfolding count (to unfold the pad)
:- static pad_urule/2.
pad_urule(rw(DefR, DefW), Def) :- !,
	pad_urule(DefR, DefR2),
	pad_urule(DefW, DefW2),
	Def = rw(DefR2, DefW2).
pad_urule(Def0, Def) :- number(Def0), !,
	Def is Def0 + 1. % one unfolding more for the padding
pad_urule(all, Def) :- !,
	Def = all.

% Add pad format and operands to the instruction
:- static pad_ins/6.
pad_ins(6, Format, Insns, [f_Q,f_Q,f_Q|Format], pad6, [pad6(a,a,a)|Insns]).
pad_ins(4, Format, Insns, [f_Q,f_Q|Format], pad4, [pad4(a,a)|Insns]).
pad_ins(2, Format, Insns, [f_Q|Format], pad2, [pad2(a)|Insns]).
pad_ins(0, Format, Insns, Format, '', Insns).
	
treat_urule(Def00) :=
        ( \+ get__use_opt(urules), number(Def00) ?
	    % do not share any instruction code (unfold all)
	    % TODO: make it work also with pad (problem: loop ins)
	    all
	| Def00
	).

% ---------------------------------------------------------------------------

xop(X, Op) :-
	ftype__size(f_t, Size),
	x_offset_from_worker(Off), Op is (X*Size)+Off.
yop(X, Op) :-
	ftype__size(f_t, Size),
	y_offset_from_frame(Off), Op is (X*Size)+Off.
zop(v(X), Op) :- % Y value
	ftype__size(f_t, Size),
	y_offset_from_frame(Off), Op is (X*Size)+Off.
zop(u(X), Op) :- % Y unsafe value
	ftype__size(f_t, Size),
	y_offset_from_frame(Off), Op is (X*Size)+Off-1.
sizeop(X, Op) :-
	ftype__size(f_t, Size),
	y_offset_from_frame(Off), Op is (X*Size)+Off.

% X encoding: as offset in "struct worker"
:- data x_offset_from_worker/1.
% Y encoding: as offset in "struct frame"
:- data y_offset_from_frame/1.

% ---------------------------------------------------------------------------

% TODO: incomplete and hardwired... use information from split__itf about exported predicates!!
:- public data blt_dec/3.

% ---------------------------------------------------------------------------

% default heap margin sizes
% TODO: where does it come from?
% TODO: include more absmach pads here
:- public heap_pad_size/2.
heap_pad_size(Kind, Heap) :-
	( tagged_size(W) -> true ),
	( Kind = cont ->
	    Heap is 128*W % TODO: this is CONTPAD, synchronize
	; Heap is 1152*W % TODO: this is CALLPAD, synchronize
	).

% ---------------------------------------------------------------------------
% Fill absnext table used for qwriter

:- use_module(library(aggregates)).
:- use_module(engine(ql_inout)).
load_absnext :-
	findall([Op|Def], num_ins_format(Op, Def), InsInfo),
	findall([Id|Def], num_ftype_info(Id, Def), FTypeInfo),
	( ftype__id(f_i, IdI) -> true ),
	( ftype__id(f_o, IdO) -> true ),
	( tagged_size(TaggedSize) -> true ),
	% TODO: why CONTPAD and CALLPAD?? 
	ContPad is 128 * TaggedSize,
	CallPad is 1152 * TaggedSize,
	SizeAlign = TaggedSize,
	'$absnext__set'(IdI, IdO, ContPad, CallPad, TaggedSize, SizeAlign, InsInfo, FTypeInfo).

num_ftype_info(Id, DefN) :-
	ftype__id(FType, Id),
	  ( ftype__def(FType, Def) ->
	      ( Def = array(FTypeI, FTypeElem) ->
		  ( ftype__id(FTypeI, IdI) -> true ),
		  ( ftype__id(FTypeElem, IdElem) -> true ),
		  DefN = [2, IdI, IdElem]
	      ; Def = str(FTypes) ->
		  call(( ids :: accum(Ids), ftype_ids(FTypes) )),
		  DefN = [1|Ids]
	      ; Def = blob ->
		  DefN = [3]
	      )
	  ; ftype__size(FType, Size),
	    ( ftype__smethod(FType, SMethod) -> true ),
	    save_method(SMethod, SMethodId),
	    DefN = [0,Size,SMethodId]
	  ).

num_ins_format(Op, Def) :-
	ins_format(Ins, Format),
	  ( ins_op(Ins, Op) -> true ),
	  call(( ids :: accum(Ids), ftype_ids(Format) )),
	  Def = [1|Ids].
{
:- fluid ids :: accum.
ftype_ids([]).
ftype_ids([X|Xs]) :-
	( ftype__id(X, X2) -> true ),
	ids.add(X2),
	ftype_ids(Xs).
}.

% ---------------------------------------------------------------------------
% Generate source for the ipexp runtime (type) information

:- public ftype_info/1.
ftype_info(Array) :-
	% create ftype_rev_id/2 table
	( ftype__id(FType, Id),
	    add(ftype_rev_id(Id, FType)),
	    fail
	; true
	),
	% get id list and max id
	findall(Id, ftype__id(_, Id), Ids),
	listmax(Ids, MaxId),
	% get ftype count and ftype info list (it needs ftype_rev_id/2)
 	FTypeCount is MaxId + 1,
	ftype_info__2(0, FTypeCount, FTypeInfoR0),
	% clean ftype_rev_id/2 table
	del(ftype_rev_id(_,_)),
	Array = ~funcall('$array'(ref1(ftype_base), FTypeInfoR0)).

ftype_info__2(Id, FTypeCount, FTypeInfoR) :- Id >= FTypeCount, !,
	FTypeInfoR = [].
ftype_info__2(Id, FTypeCount, FTypeInfoR) :-
	FTypeInfoR = [FTypeR|FTypeInfoR0],
	( ftype_rev_id(Id, FType) ->
	    ftype_info__3(FType, FTypeR0)
	; FTypeR0 = '$ftype_str0' % TODO: write a special entry for holes?
	),
	FTypeR = ~funcall(FTypeR0),
	Id1 is Id + 1,
	ftype_info__2(Id1, FTypeCount, FTypeInfoR0).

ftype_info__3(FType, DefN) :-
	( ftype__def(FType, Def) ->
	    ( Def = array(FTypeI, FTypeElem) ->
	        ( ftype__id(FTypeI, IdI) -> true ),
	        ( ftype__id(FTypeElem, IdElem) -> true ),
		DefN = '$ftype_array'(IdI, IdElem)
	    ; Def = str(FTypes) ->
	        call(( ids :: accum(Ids), ftype_ids(FTypes) )),
		DefN = '$ftype_str'(Ids)
	    ; Def = blob ->
		DefN = '$ftype_blob'
	    )
	; ftype__size(FType, Size),
	  ( ftype__smethod(FType, SMethod) -> true ),
	  save_method(SMethod, SMethodId),
	  ( ftype__lmethod(FType, LMethod) -> true ),
	  load_method(LMethod, LMethodId),
	  DefN = '$ftype_basic'(Size,SMethodId,LMethodId)
	).

:- public ins_info/2.
% Instruction info table (format)
ins_info(Xs, R) :-
	R = ~funcall('$array_elems'(~funcall('$array'(ref1(ftype_base), R0)))),
	ins_info__2(Xs, R0).

ins_info__2([], []).
ins_info__2([X|Xs], [Y|Ys]) :-
	ins_info__3(X, Y0),
	Y = ~funcall(Y0),
	ins_info__2(Xs, Ys).

ins_info__3(X, '$ftype_str0') :- iset_illop(X), !. % TODO: write a special entry for illop?
ins_info__3(X, '$ftype_str'(Ids)) :-
	( ins_format(X, Format) -> true ),
	call(( ids :: accum(Ids), ftype_ids(Format) )).

% get the maximum of a list of integers
:- static listmax/2.
listmax([X|Xs]) := ~listmax__2(Xs, X).

:- static listmax__2/3.
listmax__2([], Max) := Max.
listmax__2([X|Xs], Max0) := ~listmax__2(Xs, Max1) :-
	Max1 = ( X > Max0 ? X | Max0 ).

% ---------------------------------------------------------------------------
% Unused instruction set, for emulator minimization
% TODO: those are read and provided in apps/comp by means of the --dead parameter (find a better method?)

{
:- '$all_static'.
:- multifile unused_opcode/1.
:- static data unused_opcode/1.
}.

% ---------------------------------------------------------------------------

:- public catch_fail_ins/1.
catch_fail_ins(Ins) :-
	ic_catch_fail(Ins), !.

% TODO: infer from code definition
:- static pred_noargs/1.
pred_noargs(neck).
% TODO: change name, do not use noreturn but 'sets p' or something like that
:- static pred_noreturn/1.
pred_noreturn(proceed). % TODO: this is strange, but makes sense...
pred_noreturn(dynamic_neck__proceed). % TODO: this is strange, but makes sense...

% ---------------------------------------------------------------------------

:- use_module(library(dict)).

% Get the compact list of instructions Ins (in definition order) and the Op ordered list of instructions SortedIns (filling non defined opcodes with illop)
% TODO: split in several inst_var? initialize when the absmach is loaded?
:- attr emudef :: any # "Precomputed emulator definition".

get_emudef(EmuDef) :-
	EmuDef = ~emudef, nonvar(EmuDef), !.
get_emudef(EmuDef) :-
	EmuDef = ~emudef,
	findall(X, ins_used_op(X, _), UsedIns),
	add_op_ins,
	length(UsedIns, Count),
	call((
          opcount :: m_int <- 0,
	  op_list(Count, SortedIns),
	  ~opcount = OpCount
	)),
	del(op_ins(_, _)),
	EmuDef = emudef(UsedIns, SortedIns, OpCount).

:- public get_sortedins/1.
get_sortedins(SortedIns) :-
	get_emudef(EmuDef), EmuDef = emudef(_UsedIns, SortedIns, _OpCount).

:- public get_opcount/1.
get_opcount(OpCount) :-
	get_emudef(EmuDef), EmuDef = emudef(_UsedIns, _SortedIns, OpCount).

:- public get_usedins/1.
get_usedins(UsedIns) :-
	get_emudef(EmuDef), EmuDef = emudef(UsedIns, _SortedIns, _OpCount).

add_op_ins :-
	( ins_used_op(X, Op),
	  add(op_ins(Op, X)),
	  fail
	; true
	).

ins_used_op(X, Op) :-
	ins_op(X, Op),
	( ins_exported(X, _) -> true % necessary even if the instruction is marked as dead
	; unused_opcode(Op) -> fail % marked as dead instruction
	; true
	).

% TODO: hmmm... use a inst_data?
% :- pred op_ins(Opcode, Ins).
:- data op_ins/2.

{
:- fluid opcount :: m_int.
op_list(0, []) :- !. % no more instructions left
op_list(Count, [X|Xs]) :-
	( op_ins(~opcount, X) -> % found
	    Count1 is Count - 1
	; iset_illop(X) -> % not implemented opcode
	    Count1 = Count
	),
	opcount.inc(1),
	op_list(Count1, Xs).
}.

% Generate collapsed and specialized instruction definitions

:- attr defdic :: any # "Cache for already generated instructions".

% Get the definition of a instruction
geninsdef(Ins, InsDef) :-
	geninsdef__2(Ins, ~defdic, InsDef).

geninsdef__2(Ins, DefDic, InsDef) :-
	% already coded
	dic_get(DefDic, Ins, InsDef0), !,
	copy_term_shattr(InsDef0, InsDef). % get a new copy
geninsdef__2(Ins, DefDic, InsDef) :-
	dic_lookup(DefDic, Ins, InsDef),
	( ins_format(Ins, Format) -> true ),
	( ins_def(Ins, Insns, Def0) -> true ),
	auto_spec_def(Ins, Def0, Insns, Def2),
	call((
          uses_contpass_true :: any <- ~insns_uses_contpass_true(Insns),
	  spec_def(Def2, Ins, Insns, InsDef1)
        )),
	InsDef = '$subctx'(~list_to_conj(['$trust_ops'(Format)|InsDef1])).

% TODO: incomplete! what happens if I have some prop(bin) in a middle position?
insns_uses_contpass_true([Ins]) := UsesContpassTrue :- !,
	functor(Ins, IN, IA),
	UsesContpassTrue = ( op__prop(IN, IA, contpass_true) ? true
			   | false
			   ).
insns_uses_contpass_true([_|Insns]) := ~insns_uses_contpass_true(Insns).

% TODO: do not use lists...
:- static modeswitch/3.
modeswitch(R, W) := ~modesw(~list_to_conj(R), ~list_to_conj(W)).

:- static modesw/3.
modesw(R, W) := ( '$readmode' -> R ; W ).

:- static modesw2/4.
modesw2(R, W, U) := ( '$use_opt'(specmode) -> ( '$readmode' -> R ; W ) ; U ).

auto_spec_def(_, Def0, _) := Def :- Def0 = rw(_,_), !, Def = Def0.
auto_spec_def(Ins, Def0, Insns) := Def :-
	Mo = ~auto_spec_def__2(Ins, Def0, Insns),
	Def = ( Mo = u ? Def0
	      | Mo = r ? r(Def0)
	      | Mo = w ? w(Def0)
	      ).

auto_spec_def__2(Ins, Def, Insns) := Mo :-
	( Def = all ->
	    Mo = ( infer_defmode(Insns, M) ? M | u )
	; Def = l(_) ->
	    Insns = [I1F|_],
	    functor(I1F, I1, _),
	    Mo = ( op__iter(I1, IterIns, _),
	           infer_defmode([IterIns], M) ? M | u )
	; Def = e(Sp, Def1) ->
	    sum_to_list(Sp,Sp2),
	    Mo = ~auto_spec_def__2(Ins, Def1, Sp2)
	; number(Def) ->
	    get_prefix(Def, Insns, InsnsPre, _),
	    Mo = ~auto_spec_def__2(Ins, all, InsnsPre)
	; ptoc__impcomp:error(['ignored ', Ins, ' with Def ', Def])
	).

% TODO: the first instruction of a r/1 ins must be a ($.mode <- r), 
%       use that to use the same unfold rule for both modes...
% TODO: get automatic equiv?!?!!!! should be possible if you look at
%       the expanded code (one problem is that code differs depending
%       on unfolding rules... so comparing is difficult -- maybe this
%       heuristic works: start with higher unfolding value,
%       check... it exists its faster and shorter (code is shared), if
%       not, go down until the specified unfolding value <- more
%       problems: it's a global algorithm)

{
:- fluid uses_contpass_true :: any.
spec_def(rw(R, W), Ins, Insns, Def) :- get__use_opt(specmode), !,
	spec_def__2(R, Ins, Insns, RA),
	spec_def__2(W, Ins, Insns, WA),
	Def = [~modeswitch(RA, WA)].
spec_def(w(A0), Ins, Insns, Def) :- get__use_opt(specmode), !,
	RA = [~ins_hook_goal(Ins), '$set_mode'(w), '$sub2call'('$insc'(Ins))],
	spec_def__2(A0, Ins, Insns, WA),
	Def = [~modeswitch(RA, WA)].
spec_def(r(A0), Ins, Insns, Def) :- get__use_opt(specmode), !,
	spec_def__2(A0, Ins, Insns, RA),
	WA = [~ins_hook_goal(Ins), '$set_mode'(r), '$sub2call'('$insc'(Ins))],
	Def = [~modeswitch(RA, WA)].
spec_def(rw(R, W), Ins, Insns, Def) :- !,
	spec_def__2(R, Ins, Insns, RA),
	spec_def__2(W, Ins, Insns, WA),
	Def = [~modeswitch(RA, WA)].
spec_def(w(A0), Ins, Insns, Def) :- !,
	spec_def__2(A0, Ins, Insns, Def).
spec_def(r(A0), Ins, Insns, Def) :- !,
	spec_def__2(A0, Ins, Insns, Def).
spec_def(A0, Ins, Insns, Def) :- !,
	spec_def__2(A0, Ins, Insns, Def).

spec_def__2(Rule0, Ins, Insns1, Es) :- !,
	( Rule0 = e(Insns0, Rule) ->
	    sum_to_list(Insns0, Insns)
	; Rule = Rule0,
	  Insns = Insns1
	),
	call((
          es :: accum <- Es,
	  unfold_def__2(Rule, Insns, A0),
	  ~es = Es0
	)),
	Es0 = [~ins_hook_goal(Ins)|A0].
}.

ins_hook_goal(Ins) := G :-
	( ins_op(Ins, Op) ->
	    R = 'INS_HOOK_R'(~funcall('$typed'(int32, Op))),
	    W = 'INS_HOOK_W'(~funcall('$typed'(int32, Op))),
	    U = 'INS_HOOK_U'(~funcall('$typed'(int32, Op))),
	    G = ~modesw2(R, W, U)
	; % TODO: does opcode-less instructions need a ins_op/2 entry?
	  G = true % no ins hook for entries without an opcode (e.g. unify instruction)
	).

infer_defmode([I0|Is]) := M :-
	% TODO: a kludge?
        %ins_name_and_format(I0, I, _),
	functor(I0, I, _),
	( defmode(I, M0) -> true ),
	% 'none' means that the code does not use the mode
	M = ( M0 = none ? ~infer_defmode(Is)
	    | M0 = r ? M0
	    | M0 = w ? M0
	    ).

% TODO: too complicated... try to use a two step translation
{
:- fluid es :: accum.
:- fluid uses_contpass_true :: any.
unfold_def__2(loopm, Insns, A) :- !,
	% TODO: ad-hoc!!!! (uses the user loop definition which may be optimized depending on the mode...)
	% TODO: try to implement using spec...
	Insns = [B0],
	B0 =.. [Name, c(V)], % TODO: ensure B is loop ins?
	Format0 = [f_i(V)],
	call((
          i :: m_int <- 0,
	  apply_format(Name, Format0, G)
        )),
	A0 = [G],
	add_nextp_call(A0, A).
unfold_def__2(l(LoopRule0), Insns, A) :- !,
	( LoopRule0 = loopall ->
	    LoopRule = until(0),
	    RestRule = all
	; LoopRule0 = until(N) ->
	    LoopRule = until(N),
	    RestRule = 0
	; LoopRule0 = justone ->
	    LoopRule = justone,
	    RestRule = 0
	),
	get_ins_iter(Insns, UsesPos, MaybeIVar, IterI, IterN),
	looprest(LoopRule, UsesPos, Insns, Rest),
	Last = ( Rest = [] ? yes | no ),
	call((
          code :: accum <- A0,
	  iterloop(LoopRule, MaybeIVar, IterI, IterN, UsesPos, Last),
	  ~code = As
	)),
	( Rest = [] ->
	    As = [],
	    add_nextp_call(A0, A)
	; unfold_def__2(RestRule, Rest, As),
	  A = A0
	).
unfold_def__2(Size, Insns, Code) :-
	( Size = all ->
	    Prefix = Insns,
	    Rest = []
	; get_prefix(Size, Insns, Prefix, Rest)
	),
	call((
          i :: m_int <- 0,
	  code :: accum <- Code1,
	  unfold_def__3(Prefix),
	  ~code = Code0,
	  ~i = RI
        )),
	restcont(RI, Rest, RCont1),
	( RCont1 = rcont0(N,RInsns) ->
	    expand_spec(RInsns, _, RIns, _, _),
	    Code0 = ['$shift_ops'(N), '$sub2call'('$insc'(RIns))],
	    Code = Code1
	; Code0 = [],
	  add_nextp_call(Code1, Code)
	).

% TODO: not very clean...
% add a nextp continuation if the instruction code did not call any continuation
:- '$ctxprj'(add_nextp_call/2, [uses_contpass_true]).
add_nextp_call(InsDef0) :=
	( ~uses_contpass_true = true ?
	    InsDef0
	| ~append(InsDef0, [call(~funcall('$get_pnext'))])
	).

% TODO: this is not 'unfold', but something like ins_skel...
{
:- fluid i :: m_int.
:- fluid code :: accum.
:- '$ctxprj'(unfold_def__3/1, [es, i, code]).
unfold_def__3([]) :- !.
unfold_def__3([Ins|Insns]) :- !,
	ins_name_and_format(Ins, Name2, Format0),
	apply_format(Name2, Format0, G),
	code.add(G),
	unfold_def__3(Insns).

:- '$ctxprj'(apply_format/3, [es, i]).
apply_format(Name, Format, G) :-
	arglists(Format, EsL),
	dec_args(Format, EsL, Bs),
	G0 =.. [Name|Bs],
	G = '$unfold'(G0).

:- '$ctxprj'(arglists/2, [i]).
arglists([], []) :- !.
arglists([F|Fs], Es) :- ftype__zero(F), !,
	arglists(Fs, Es).
arglists([F|Fs], [E|Es]) :- !,
	( ftype__dectype(F, T) -> true ),
	E = eq0(_, op_var(evalmem(~funcall('$custommem_ins_a'(~i, T))))),
	i.inc(1),
	arglists(Fs, Es).
}.

{
:- fluid code :: accum.
:- '$ctxprj'(iterloop/6, [es, code]).
iterloop(justone, _, IterI, IterN, _UsesPos, Last) :- !,
	code.add(IterI),
	( Last = yes ->
	    true
	; code.add('$shift_ops'(IterN))
	).
iterloop(until(IterIEnd), MaybeIVar, IterI, IterN, UsesPos, _Last) :- !,
	IterX = (IterI, '$shift_ops'(IterN)),
	( MaybeIVar = just(IVar) -> true ; true ),
	( ftype__dectype(f_i, Ti) -> true ),
	es.add((AVar = ~funcall('$evalmem'(~funcall('$custommem_ins_a'(0, Ti)))))),
	code.add((IVar = ~funcall(initmut(intmach, AVar)))),
	code.add('$shift_ops'(1)),
	code.add('$for_each'(IVar, ~funcall(revrange0_noinit(IterIEnd)), IterX)),
	( UsesPos = zero ->
	     % TODO: incorrect!! should insert IterIEnd times the head of the list
	     true
	; code.add('$trust_shift_array')
	).
}.

}.

% TODO: better name?
:- static iterpos/3.
iterpos(s(LoopSize0), no, IterI) :- !,
	IterI is LoopSize0 - 1.
iterpos(unknown, just(IVar), '@'(IVar) - 1).

:- static get_prefix/4.
get_prefix(0, Xs, [], Xs) :- !.
get_prefix(I, [X|Xs], [X|Ys], Rest) :-
	I1 is I - 1,
	get_prefix(I1, Xs, Ys, Rest).

% shift instruction list using loop rule
:- static looprest/4.
looprest(until(0), _, [_|Rest], Rest) :- !.
looprest(until(IterIEnd), zero, [LoopIns0|Rest], [LoopIns2|Rest]) :- !,
	functor(LoopIns0, LoopIns, _),
	LoopIns2 =.. [LoopIns, c(IterIEnd)]. % TODO: ensure B is loop ins?
looprest(until(IterIEnd), _, [LoopIns0|Rest], [LoopIns2|Rest]) :- !,
	functor(LoopIns0, LoopIns, _),
	LoopIns2 =.. [LoopIns, s(IterIEnd)]. % TODO: ensure B is loop ins?
looprest(justone, _UsesPos, [LoopIns|Rest0], Rest) :-
	LoopIns =.. [Ins, A], % TODO: ensure B is loop ins?
	iter_rest_get(A, I),
	I2 is I - 1,
	( I2 = 0 ->
	    Rest = Rest0
	; iter_rest_set(A, I2, A2),
	  LoopIns2 =.. [Ins, A2], % TODO: ensure B is loop ins?
	  Rest = [LoopIns2|Rest0]
	).

:- static iter_rest_get/2.
iter_rest_get(s(I), I) :- !.
iter_rest_get(c(I), I) :- !.

:- static iter_rest_set/3.
iter_rest_set(s(_), I2, s(I2)) :- !.
iter_rest_set(c(_), I2, c(I2)) :- !.

% 
{
:- fluid es :: accum.
get_ins_iter(Insns0, UsesPos, MaybeIVar, IterI, IterN) :-
	% TODO: only works if first instruction is a loop
	Insns0 = [LoopIns0|_],
	expand_spec__3(LoopIns0, LoopPattern, _, [LoopSpec], _, _),
	functor(LoopPattern, LoopIns, _),
	looparg_size(LoopSpec, LoopSize),
	( op__iter(LoopIns, IterIns, UsesPos) -> true ),
	IterIns =.. [IterInsN|Fmt],
	( UsesPos = yes ->
	    iterpos(LoopSize, MaybeIVar, IterPos),
	    ArgsL = [AVar, IterPosV],
	    ( ftype__dectype(f_i, Ti) -> true ),
	    EsL = [eq0(AVar,op_var(evalmem(~funcall('$custommem_ins_a'(0, Ti))))), eq0(IterPosV, op_val(IterPos))],
	    IterN = 1
	; UsesPos = no ->
	    ArgsL = [AVar],
	    ( ftype__dectype(f_i, Ti) -> true ),
	    EsL = [eq0(AVar,op_var(evalmem(~funcall('$custommem_ins_a'(0, Ti)))))],
	    IterN = 1,
	    MaybeIVar = no
	; UsesPos = zero ->
	    ArgsL = [],
	    EsL = [],
	    IterN = 0,
	    MaybeIVar = no
	),
	( Fmt = [] ->
	    eq0_to_eq(EsL),
	    Bs = ArgsL
	; dec_args(Fmt, EsL, Bs)
	),
	IterI =.. [IterInsN|Bs].

:- '$ctxprj'(eq0_to_eq/1, [es]).
:- static eq0_to_eq/1.
eq0_to_eq([]).
eq0_to_eq([eq0(V,X)|Es0]) :-
	( X = op_var(evalmem(Mem)) ->
	    es.add(E),
	    E = (V = ~funcall('$evalmem'(Mem)))
	; X = op_val(X0) ->
	    V = X0
	),
	eq0_to_eq(Es0).

:- '$ctxprj'(looparg_size/2, []).
:- static looparg_size/2.
looparg_size(X) := s(N) :- list_size(X, N), !.
looparg_size(X) := c(X) :- integer(X), !.
looparg_size(_) := unknown.

:- '$ctxprj'(list_size/2, []).
:- static list_size/2.
list_size(X, _) :- var(X), !, fail.  
list_size([], 0). 
list_size([_|Xs], I2) :- list_size(Xs, I), I2 is I + 1. 

:- '$ctxprj'(dec_args/3, [es]).
dec_args([], [], []).
dec_args([F|Fs], Xs, [Y|Ys]) :- ftype__decfun0(F, Y0, _), !, % the ftype is complete
	( F = f_i(I) -> % TODO: too specific!  use something like this for all? or use new_var and type info?
	    Y = I
	; es.add(E),
	  E = (Y = ~funcall('$evalmem'(Y0)))
	),
	dec_args(Fs, Xs, Ys).
dec_args([F|Fs], [eq0(Y, X)|Xs], [Y|Ys]) :- % the ftype is incomplete
	( X = op_var(A0) ->
	    true
	; X = op_val(A0) ->
	    true
	),
	( ftype__decfun(F, DecMethod) ->
	    ( dec_expr(DecMethod, A0, Y0) ->
	        true
	    ; errlog:bug(['dec_expr failed for: ', DecMethod, ' ', A0, ' ', Y0]),
	      fail
	    )
	; Y0 = A0
	),
	( Y0 = evalmem(Yb) ->
	    true
	; errlog:bug(['y0 is not evalmem: ', Y0]),
	  fail
	),
	es.add(E),
	E = (Y = ~funcall('$evalmem'(Yb))),
	dec_args(Fs, Xs, Ys).
}.

% rcont is a continuation where we specify the number of arguments to shift and the continuation instruction
:- static restcont/3.
% TODO: is the first clause here always correct?
restcont(_, [], defcnt_nextp) :- !. % I is ignored here, since P will be set later
restcont(I, Rest, rcont0(N, Rest)) :-
	N = ( rest_noreturn_noargs(Rest) ? 0 | I ).

:- static rest_noreturn_noargs/1.
rest_noreturn_noargs([X|Xs]) :- pred_noargs(X), !, rest_noreturn_noargs(Xs).
rest_noreturn_noargs([X]) :- pred_noreturn(X), !.

% ---------------------------------------------------------------------------

% TODO: for subcalls, unify with lowpred_spec?
:- public pred_spec/4.
% GN/GA: predicate
% GlobalSpec: specialize w.r.t. that global variable
% TopGlobal: do not specialize w.r.t. that global variable
% TODO: simplify GlobalSpec/TopGlobal
pred_spec('$pr'(GN, GA), GlobalSpec, TopGlobal, Prior0) :-
	( GN = '$insdef'(Ins), GA = 0 ->
	    ( get__use_opt(specmode) ->
	        GlobalSpec = global(mode),
		TopGlobal = none
	    ; GlobalSpec = none,
	      TopGlobal = global(mode, mode, mode)
	    ),
	    % TODO: get the good key based on pred properties (not ins)
	    ( ins_prior(Ins, Prior0) ->
	        true
	    ; Prior0 = no
	    )
	; % TODO: add more elements from the state to this key!
	  GlobalSpec = none,
	  TopGlobal = none,
	  Prior0 = no
	).

% TODO: for lowpred, unify with pred_spec?
:- public lowpred_spec/1.
% TODO: hardwired...
% TODO: GlobalSpec should be part of the lowpred entry! and should be predicate-specific
lowpred_spec :=
	( get__use_opt(specmode) ?
	    global(mode, mode, mode_r)
	| global(mode, mode, mode)
	).

:- public get_pred_def/3.
get_pred_def('$pr'(N, A), Xs) :=
	( N = '$insdef'(Ins), A = 0, Xs = [],
	  geninsdef(Ins, Def0) ?
	    Def0
	| atom(N),
	  C =.. [N|Xs],
	  maybe_parent(Ipexp2),
	  Ipexp2.get_pred_body(C, Def0) ?
	    Def0
	).

% TODO: merge with get_pred_def
:- public get_fun_def/4.
get_fun_def(N, A, Xs) :=
	maybe_parent(Ipexp2),
	Ipexp2.fun_body(N, A, Xs, Def0) ? Def0.

get_pred_body(Head, Body) :-
	functor(Head, N, A),
	functor(H, N, A),
	( op__format(N, _) -> true ), % ensure that it is defined
	( cached_pred_body(H, Body0) -> % already in cache
	    Head = H,
	    Body = Body0
	; % normalize and add to the cache
	  findall(c(H, B), pred_clause(H, B), Cs),
	  norm_pred(Cs, Head2, Body2),
	  add(cached_pred_body(Head2, Body2)),
	  % unify
	  Head = Head2,
	  Body = Body2
	).

% TODO: this is incomplete... unify with compiler__expand:norm_pred/4 (but extract some optimization rules from it and move them to the source code)
norm_pred(Cs, Head, Body) :-
	collect_clauses(Cs, Head, Bodies0),
	cut_to_ifthen(Bodies0, Bodies),
	list_to_disj(Bodies, Body).

:- static list_to_disj/2.
list_to_disj([], true) :- !. % TODO: this is wrong, it should be fail, right?
list_to_disj([A], A) :- !.
list_to_disj([A|As], (A;Bs)) :- !,
	list_to_disj(As, Bs).

collect_clauses([], _, []) :- !.
collect_clauses([c(CHead, CBody)|Cs], Head, Bodies) :-
	Head = CHead,
	Bodies = [CBody|Bodies0],
	collect_clauses(Cs, Head, Bodies0).

cut_to_ifthen([], []).
cut_to_ifthen([B0|Bs0], [B|Bs]) :-
	cut_to_ifthen__2(B0, B),
	cut_to_ifthen(Bs0, Bs).

cut_to_ifthen__2(B0, B) :-
	( conj_to_list(B0, B1),
	  split_guard(B1, Guard, Rest) ->
	    list_to_conj(Guard, Guard2),
	    list_to_conj(Rest, Rest2),
	    B = (Guard2 -> Rest2)
	; B = B0
	).

split_guard([A|As], Gs, Rs) :- A = (!), !,
	Gs = [],
	Rs = As.
split_guard([A|As], Gs, Rs) :- !,
	Gs = [A|Gs0],
	split_guard(As, Gs0, Rs).

:- public decomp_insc_s/3.
decomp_insc_s(Insns0, Pred, Args) :-
	sum_to_list(Insns0, Insns),
	expand_spec(Insns, _, Ins, _, _),
	Pred = '$pr'('$insdef'(Ins), 0),
	Args = [].

% ---------------------------------------------------------------------------

:- use_module(library(lists), [length/2, append/3]).
:- use_module(compiler(errlog)).

% TODO: move merging (collapsing) as a different transformation? only
% if we insert other transformations or simplification between it and encoding

% ---------------------------------------------------------------------------
% Assemble instructions

:- public asm_insns/3.
asm_insns(Code0, Size, Tokens) :-
	speccollapse(Code0, Code1),
	enc_insns(Code1, Size, Tokens),
	!.
asm_insns(Code, _, _) :-
	ptoc__impcomp:error(['asm_insns/3 failed for ', Code]).

% ---------------------------------------------------------------------------
% Specialize and collapse instructions using a set of patterns ordered from more to less specific

:- use_module(library(aggregates), [findall/3]).
:- use_module(library(terms_check), [instance/2]).

:- data speccollapse__rule/4.
% speccollapse__rule(PatternsHead, PatternsTail, CollapsedIns2, CollapsedArgs2)
%    where [PatternsHead|PatternsTail] is an incomplete list (last tail is a var)
% Order relation:
%    A < B iff A is longer than B or A is more specific than B

% TODO: improve pattern reordering by using more general type operations
add_speccollapse([Pattern|PatternTail], InsAlias, ArgsAlias) :-
	more_general_speccollapses(Pattern, PatternTail, As),
	% remove more general speccollapses
	( member(speccollapse(PB, PTailB, AIns, AArgs), As),
	  del(speccollapse__rule(PB, PTailB, AIns, AArgs)),
	  fail
	; true
	),
	% add the new speccollapse
	add(speccollapse__rule(Pattern, PatternTail, InsAlias, ArgsAlias)),
	% re-insert more general speccollapses
	( member(speccollapse(PB, PTailB, AIns, AArgs), As),
	  add(speccollapse__rule(PB, PTailB, AIns, AArgs)),
	  fail
	; true
	).

more_general_speccollapses(Pattern, PatternTail, As) :-
	functor(Pattern, Ins, Arity),
	functor(PatternB, Ins, Arity),
	findall(A, more_general_speccollapse(Pattern, PatternTail, PatternB, A), As).

more_general_speccollapse(Pattern, PatternTail, PatternB, A) :-
	speccollapse__rule(PatternB, PatternTailB, AliasIns, AliasArgs),
	  N = ~length(PatternTail),
	  BN = ~length(PatternTailB),
	  ( N > BN ->
              % [Pattern|PatternTail] is more specific, since it is longer than [PatternB|PatternTailB]
	      true
	  ; N < BN ->
              % [Pattern|PatternTail] is more general, since it is shorter than [PatternB|PatternTailB]
	      fail
	  ; %  [Pattern|PatternTail] is more specific only if it is an instance of [PatternB|PatternTailB]
	    instance([Pattern|PatternTail], [PatternB|PatternTailB])
	  ),
	  A = speccollapse(PatternB, PatternTailB, AliasIns, AliasArgs).

speccollapse([X|Xs], [X|Ops]) :- control_ins(X), !,
	speccollapse(Xs, Ops).
speccollapse([X0|Xs], [ins(Ins, Args)|Ops]) :-
	X0 =.. [XN|XArgs],
	getmems(XArgs, XArgs2),
	X =.. [XN|XArgs2],
	( speccollapse__rule(X, PatternsTail, Ins0, Args0),
	  speccollapse__match(PatternsTail, Xs, Xs0) ->
	    Ins = Ins0,
	    Args = Args0
	; X =.. [Ins|Args],
	  Xs0 = Xs
	),
	speccollapse(Xs0, Ops).
speccollapse([], []).

speccollapse__match([], Xs, Xs) :- !.
speccollapse__match([Pattern|Patterns], [X0|Xs], Xs0) :-
	X0 =.. [XN|XArgs],
	getmems(XArgs, XArgs2),
	X =.. [XN|XArgs2],
	X = Pattern,	
	speccollapse__match(Patterns, Xs, Xs0).

:- static control_ins/1.
control_ins(cjump(_,_)).
control_ins(jump(_)).
control_ins(label(_)).

:- static getmems/2.
getmems([], []).
getmems([X|Xs], [X2|Xs2]) :-
	% TODO: a bit dirty...
	X2 = ( X instance_of termvar ? ~X.getp(mem) | X ),
	getmems(Xs, Xs2).

% ---------------------------------------------------------------------------
% Encode instructions
% note: the output token list is translated to bytes in ql_inout

% TODO: it should not appear in this code any explicit ftype

enc_insns(Code, Size, Tokens) :-
	offset :: m_int <- 0,
	tokens :: accum(Tokens),
	enc_insns__2(Code),
	~offset = Size,
	!.
enc_insns(Code, _, _) :-
	bug(['enc_insns/3 failed for ', Code]), fail.

{
:- fluid offset :: m_int.
:- fluid tokens :: accum.
enc_insns__2(Insns) :-
	( Insns = [cjump(shallow, Shallow)|Insns0] ->
	    enc_arg(f_i, Shallow)
	; enc_arg(f_i, Shallow),
	  Shallow = ~offset,
	  Insns0 = Insns
	),
	enc_cinsns(Insns0).

% TODO: study any assembler and try to avoid list of tokens, call ql_inout directly instead (would that require resolving labels and alignments before?)
enc_cinsns([]).
enc_cinsns([jump(Label)|Is]) :- !,
	enc_cinsns__ins(jump, [Label]),
	enc_cinsns(Is).
enc_cinsns([label(Label)|Is]) :- !,
	Label = ~offset,
	enc_cinsns(Is).
enc_cinsns([ins(Ins, Args)|Is]) :-
	enc_cinsns__ins(Ins, Args),
	enc_cinsns(Is).

enc_cinsns__ins(Ins, Args) :-
	( ins_format(Ins, Format) -> true ),
%	errlog:trace([p(Ins, Format)]),
	align_ins(Ins, Format, Args, ~offset, Ins2, Args2, Format2),
%	errlog:trace([f2(Ins, Format, Args, Off0, Ins2, Args2, Format2)]),
	enc_opcode(Ins2),
%	errlog:trace([eop(Off0, Off1)]),
	enc_args(Format2, Args2),
%	errlog:trace([earg]),
	!.
enc_cinsns__ins(Ins, Args) :-
	bug(['enc_cinsns__ins/6 failed for ', Ins, ' and args ', Args]), fail.

enc_args([], []).
enc_args([F|Fs], [X|Xs]) :-
	enc_arg(F, X),
	enc_args(Fs, Xs).

% TODO: how can I move dynamic size arg cases to absmach?
% TODO: share code with dyn_op_size
enc_arg(F, List) :- ftype__def(F, array(IFType, Fe)), !,
	ftype__size(IFType, ISize),
	ftype__size(Fe, YSize),
	length(List, N), M is ISize + YSize * N, offset.inc(M),
	tokens.add(N),
        enc_list(List, Fe).
enc_arg(F, A) :- ftype__def(F, str(Fs)), !,
	A =.. [_|Xs], % TODO: use the functor name?
	enc_args(Fs, Xs).
enc_arg(F, C) :- ftype__def(F, blob), !,
	% note: add the size of the head functor
	blob_aligned_size(C, H),
	( tagged_size(S) -> true ),
	AlignedSize is H + S,
	offset.inc(AlignedSize),
	% TODO: include Size or recalculate it in '$qwrite_b'?
	tokens.add(blob(C,AlignedSize)).
enc_arg(F, A) :-
	ftype__size(F, Size),
	offset.inc(Size),
	X2 = ( ftype__encfun(F, EncMethod) ?
	         ~enc_method(EncMethod, A)
	     | A
	     ),
	tokens.add(X2).

enc_opcode(Ins) :-
	ftype__size(f_o, OSize),
	offset.inc(OSize),
	( ins_op(Ins, Op) -> true ),
	tokens.add(Op).
}.

align_ins(Ins, Format, Args, Off0, Ins2, Args2, Format2) :-
	( ins_padding(Ins, Padding) -> true ),
        reqpad(Padding, [f_o|Format], [dummy|Args], Off0, ReqPad),
	( ins_aligned(Ins, ReqPad, PadIns) -> true ),
	!,
	Ins2 = PadIns,
	padded_args(ReqPad, Format, Args, Format2, Args2).
align_ins(Ins, Format, Args, _Off0, Ins2, Args2, Format2) :-
	Ins2 = Ins, Format2 = Format, Args2 = Args.

% Add arguments for the padding
:- static padded_args/5.
padded_args(0, Format0, Args0, Format, Args) :- !,
	Format = Format0, Args = Args0.
padded_args(I, Format0, Args0, Format, Args) :- !,
	I1 is I - 2,
	Format = [f_Q|Format1], Args = [0|Args1],
	padded_args(I1, Format0, Args0, Format1, Args1).

% Obtain the padding bytes required to align the word for a given instruction
reqpad([skip(SkipOps, SkipOff)|Padding0], Format, Args, Off0, ReqPad) :-
	skip(SkipOps, Format, [DynF|Format2]),
	skip(SkipOps, Args, [DynA|Args2]),
	dyn_op_size(DynF, DynA, DynSize),
	Off1 is Off0 + SkipOff + DynSize,
	reqpad(Padding0, Format2, Args2, Off1, ReqPad).
reqpad([Pad], _Format, _Args, Off0, ReqPad) :-
	reqpad_at(Pad, Off0, ReqPad).

% ReqPad are the padding bytes required to align at Off0
:- static reqpad_at/3.
reqpad_at(pad(Pad,ModPad), Off0, ReqPad) :-
	OffMod8 is Off0 mod 8,
        ReqPad is (Pad + 8 - OffMod8) mod ModPad.

:- static skip/3.
skip(0, Xs, Xs) :- !.
skip(N, [_|Xs], Ys) :- N1 is N - 1, skip(N1, Xs, Ys).

% Obtain the padding module (4 for 2 padded versions, 8 for 4 padded versions)
:- static padding_mod/2.
padding_mod([skip(_, _)|Padding], Mod) :- !,
	padding_mod(Padding, Mod).
padding_mod([pad(_,ModPad)], ModPad).

% size of dynamic sized operands
dyn_op_size(F, List, Size) :- ftype__def(F, array(IFType, Fe)), !,
	ftype__size(IFType, ISize),
	ftype__size(Fe, YSize),
	length(List, N), Size is (ISize + YSize * N).
dyn_op_size(F, C, Size) :- ftype__def(F, blob), !,
	% note: add the size of the head functor
	blob_aligned_size(C, H),
	( tagged_size(S) -> true ),
	Size is H + S.
dyn_op_size(_, _, 0).

{
:- fluid tokens :: accum.
enc_list([], _).
enc_list([X|Xs], Fe) :-
	X2 = ( ftype__encfun(Fe, EncMethod) ?
	         ~enc_method(EncMethod, X)
	     | X
	     ),
	tokens.add(X2),
	enc_list(Xs, Fe).
}.

enc_method(sizeop, A, A2) :- sizeop(A, A2).
enc_method(xop, A, A2) :- xop(A, A2).
enc_method(yop, A, A2) :- yop(A, A2).
enc_method(zop, A, A2) :- zop(A, A2).

% decoding expression for encoded argument
dec_expr(lrg, evalmem(A), B) :- !, B = evalmem(~funcall('$custommem_lrg'(A))). % TODO: change name?
dec_expr(yb, evalmem(A), B) :- !, B = evalmem(~funcall('$custommem_yb'(A))). % TODO: change name?
dec_expr(xb, evalmem(A), B) :- !, B = evalmem(~funcall('$custommem_xb'(A))). % TODO: change name?
dec_expr(linf, evalmem(A), B) :- !, B = evalmem(~funcall('$custommem_linf'(A))). % TODO: change name?

% ---------------------------------------------------------------------------

:- use_module(compiler(meta_syntax)).

