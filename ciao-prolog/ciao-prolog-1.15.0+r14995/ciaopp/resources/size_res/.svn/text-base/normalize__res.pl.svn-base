:- module(normalize__res,
	    [
		find_recursive_comp/3,
		init_normalize_queue/3,
		literal_output_comp/15,
		normalize/15,
		normalize_time/15, % EMM
		substitute/4,
		substitute_literal_formal/4
	    ], [assertions, resources(inferres_decl), hiord]).

:- doc(author, "Nai-Wei Lin").
:- doc(author, "Edison Mera").

:- doc(module, "This file contains the procedures for performing
	normalization.").

:- use_module(library(hiordlib)).
:- use_module(resources(init_res(builtin_res)),
	    [
		second_order_predicate/1,
		second_order_predicate_pred_arg/2,
		second_order_predicate_pred_num/3
	    ]).
:- use_module(resources(init_res(symtable_res)), [literal_property/10]).
:- use_module(resources(top_res(utility_res)),
	    [
		init_queue/2,
		set_put_queue/3,
		empty_queue/2,
		nonempty_queue/2,
		get_queue/3,
		subterm/2,
		ith_list_element/3,
		member/2,
		compound/1
	    ]).
:- use_module(resources(dependency_res(adg_res)), [find_adg_field/4]).
:- use_module(resources(dependency_res(position_res)),
	    [
		pos_litnum/2,
		pos_argnum/2,
		gen_literal_iopos/5,
		new_pos/3
	    ]).
:- use_module(resources(size_res(term_size_res)), [general_term_size/10]).
:- use_module(resources(size_res(clause_res)),
	    [
		clause_term_measure/8,
		ith_body_literal/3
	    ]).
:- use_module(resources(resources_basic)).

%
%  Initialize the normalization queue.
%
init_normalize_queue(Pos, QHead, QTail) :-
	init_queue(QHead, ITail),
	set_put_queue(ITail, Pos, QTail).

%
%  Insert the predecessors of a position into the normalization queue.
%
insert_normalize_queue(Pos, Adg, QTail, NTail) :-
	find_adg_field(Adg, Pos, pred, EdgeList),
	set_put_queue(QTail, EdgeList, NTail).

% Added by EMM
normalize_time(Exp, QHead, QTail, Approx, BT, ST, Comp, Clause, Key, Adg,
	    Gvars, PosSet, ISize, RSize, NExp) :-
	map(Exp, normalize(QHead, QTail, Approx, BT, ST, Comp, Clause, Key,
		Adg, Gvars, PosSet, ISize, RSize), NExp),
	!.
normalize_time(Exp, QHead, QTail, Approx, BT, ST, Comp, Clause, Key, Adg,
	    Gvars, PosSet, ISize, RSize, NExp) :-
	normalize(Exp, QHead, QTail, Approx, BT, ST, Comp, Clause, Key, Adg,
	    Gvars, PosSet, ISize, RSize, NExp).
% End Added by EMM

:- pred normalize/15 :: term * term * term * approx * list * list * list
	* clause_ppkey_t * atm * list * term * term * term * term * term #
	"Normalize an expression by the sizes in the normalization queue.".
normalize(bot, _QHead, _QTail, _, _, _, _, _, _, _, _, _, _, _, bot) :-
	!.
normalize(Exp, QHead, QTail, _, _, _, _, _, _, _, _, _, _, _, Exp) :-
	Exp \== bot,
	empty_queue(QHead, QTail),
	!.
normalize(Exp, QHead, QTail, Approx, BT, ST, Comp, ClausePPKey, Key, Adg,
	    Gvars, PosSet, ISize, RSize, NExp) :-
	Exp \== bot,
	nonempty_queue(QHead, QTail),
	get_queue(QHead, Pos, NHead),
	(
	    subterm(Pos, Exp) ->
	    normalize_pos(Exp, Pos, NHead, QTail, Approx, BT, ST,
		Comp, ClausePPKey, Key, Adg, Gvars, PosSet, ISize, RSize, NExp)
	;
	    normalize(Exp, NHead, QTail, Approx, BT, ST, Comp,
		ClausePPKey, Key, Adg, Gvars, PosSet, ISize, RSize, NExp)
	).

%
%  Normalize an expression by the size of a position.
%
normalize_pos(Exp, Pos, QHd, QTl, Approx, BT, ST, Comp, ClausePPKey, Key, Adg,
	    Gvars, PosSet, ISize, RSize, NExp) :-
	find_adg_field(Adg, Pos, (mode), +),
	pos_litnum(Pos, 0),
	!,
	pos_argnum(Pos, ArgNum),
	ith_list_element(ArgNum, ISize, Size),
	normalize_size(Exp, Pos, Size, QHd, QTl, Approx, BT, ST,
	    Comp, ClausePPKey, Key, Adg, Gvars, PosSet, ISize, RSize, NExp).
normalize_pos(Exp, Pos, QHd, QTl, Approx, BT, ST, Comp, ClausePPKey, Key, Adg,
	    Gvars, PosSet, ISize, RSize, NExp) :-
	find_adg_field(Adg, Pos, (mode), +),
	pos_litnum(Pos, LitNum),
	LitNum > 0,
	!,
	clause_term_measure(Approx, BT, ST, ClausePPKey, Key, Pos, Term,
	    Measure),
	general_term_size(Measure, Approx, ClausePPKey, Key, BT, ST, Gvars,
	    PosSet, Term, Size),
	normalize_size(Exp, Pos, Size, QHd, QTl, Approx, BT, ST,
	    Comp, ClausePPKey, Key, Adg, Gvars, PosSet, ISize, RSize, NExp).
normalize_pos(Exp, Pos, QHd, QTl, Approx, BT, ST, Comp, ClausePPKey, Key, Adg,
	    Gvars, PosSet, ISize, RSize, NExp) :-
	find_adg_field(Adg, Pos, (mode), -),
	pos_litnum(Pos, LitNum),
	pos_argnum(Pos, ArgNum),
	clause_body(ClausePPKey, Body),
	ith_body_literal(LitNum, Body, LitPPKey),
	lit_ppkey(LitPPKey, Lit, PPKey),
	functor(Lit, F, N),
	( second_order_predicate(F/N) ->
	    (
		second_order_predicate_pred_arg(Lit, Lit1),
		functor(Lit1, F1, N1),
		second_order_predicate_pred_num(Body, LitNum, Num),
		literal_output_comp(F1/N1, Lit, ClausePPKey, Key, PPKey, Num,
		    1, Approx, BT, ST, [], Adg, det, [], Size)
	    )
	;
	    literal_output_comp(F/N, Lit, ClausePPKey, Key, PPKey, LitNum,
		ArgNum, Approx, BT, ST, Comp, Adg, size, RSize, Size)
	),
	normalize_size(Exp, Pos, Size, QHd, QTl, Approx, BT, ST,
	    Comp, ClausePPKey, Key, Adg, Gvars, PosSet, ISize, RSize, NExp).

%
%  Normalize an expression by a size.
%
normalize_size(_Exp, _Pos, bot, _QHead, _QTail, _Approx, _BT, _ST, _Comp,
	    _ClausePPKey, _Key, _Adg, _Gvars, _PosSet, _ISize, _RSize, bot) :-
	!.
normalize_size(Exp, Pos, Size, QHead, QTail, Approx, BT, ST, Comp,
	    ClausePPKey, Key, Adg, Gvars, PosSet, ISize, RSize, NExp) :-
	Size \== bot,
	substitute(Exp, Pos, Size, Exp1),
	insert_normalize_queue(Pos, Adg, QTail, NTail),
	normalize(Exp1, QHead, NTail, Approx, BT, ST, Comp, ClausePPKey, Key,
	    Adg, Gvars, PosSet, ISize, RSize, NExp).

% Added by PLG:

:- pred literal_output_comp(LitName, Lit, ClausePPKey, Key, PPKey, LitNum,
	    ArgNum, Approx, BT, ST, Component, Adg, PropName, RComp, Comp) # "
Get the output complexity function of a literal in the clause, where:
@var{LitName} is the name/arity of the literal predicate.
@var{LitNum} is the ordinal of the literal in the clause.
@var{ArgNum} is the ordinal of the argument.
@var{BT} is the bottom table, i. e., a table that contains the builtins
with bottom or default cost properties, to inform the user the required
missing cost information that could be readed from assertions (EMM).
@var{ST} is the symbol table.
@var{Component}, @var{Adg}, @var{PropName}, @var{RComp}, @var{Comp}: ...
".
literal_output_comp(LitName, Lit, ClausePPKey, Key, PPKey, LitNum, ArgNum,
	    Approx, BT, ST, Component, Adg, PropName, RComp, Comp) :-
	utility_res:member(Component, LitName) ->
	rec_literal_comp(PropName, Adg, LitName, LitNum, ArgNum, RComp, Comp)
    ;
	literal_property(BT, ST, Lit, ClausePPKey, Key, PPKey, LitNum,
	    PropName, Approx, LitComp),
	nonrec_literal_comp(LitComp, LitName, LitNum, ArgNum, Comp).

gen_init_generic_comp(resources(Resources), F, A, ArgNum, Pos, Comp) :-
	!,
	gen_init_comp(F, A, ArgNum, Pos, Comp0),
	build_resource_comp(Resources, Comp0, Comp).
gen_init_generic_comp(_Type, F, A, ArgNum, Pos, Comp) :-
	gen_init_comp(F, A, ArgNum, Pos, Comp).

gen_init_comp(F, A, ArgNum, Pos, Comp) :-
	Comp =.. [F, A, ArgNum|Pos].

% End Modified by EMM

rec_literal_comp(Type, Adg, LitName, LitNum, ArgNum, RComp, Comp) :-
	find_recursive_comp(RComp, LitName, Rcomp),
	LitName = F/A,
	( var(Rcomp) ->
	    ( gen_literal_iopos(Adg, LitName, LitNum, (+), Pos),
		gen_init_generic_comp(Type, F, A, ArgNum, Pos, Comp) ) ;
% EMM
	    ( ith_list_element(ArgNum, Rcomp, Comp1),
		substitute_literal_formal(A, Comp1, LitNum, Comp) ) ).

%
%  Find the mutual recursive complexity when available.
%
find_recursive_comp([],                       _,       _) :- !.
find_recursive_comp([comp(LitName, Rcomp)|_], LitName, Rcomp) :- !.
find_recursive_comp([comp(Pred, _)|RComp],    LitName, Rcomp) :-
	Pred \== LitName,
	find_recursive_comp(RComp, LitName, Rcomp).

%
%  Substitute the actuals for the formals in the output complexity functions 
%  of a nonrecursive literal.
%

:- check comp nonrec_literal_comp/5 + not_fails.
% rtcheck -- EMM
nonrec_literal_comp([C|_], LitName, LitNum, 1, Comp) :-
	LitName = _/A,
	!,
	substitute_literal_formal(A, C, LitNum, Comp).
nonrec_literal_comp([_|CompList], LitName, LitNum, ArgNum, Comp) :-
	ArgNum > 1,
	ArgNum1 is ArgNum-1,
	nonrec_literal_comp(CompList, LitName, LitNum, ArgNum1, Comp).

%
%  Substitute an actual for a formal in the output complexity function 
%  of a nonrecursive literal.
%
substitute_literal_formal(0, Comp, _,      Comp) :- !.
substitute_literal_formal(A, C,    LitNum, Comp) :-
	A > 0,
	new_pos(0,      A, Pos1),
	new_pos(LitNum, A, Pos2),
	substitute(C, Pos1, Pos2, C1),
	A1 is A-1,
	substitute_literal_formal(A1, C1, LitNum, Comp).

%
%  Substitute all occurrences of a subterm by another subterm.
%
substitute(Term, Var, NTerm, NTerm) :-
	Term == Var,
	!.
substitute(Term, Var, _, Term) :-
	Term \== Var,
	var(Term),
	!.
substitute(Term, Var, _, Term) :-
	Term \== Var,
	atomic(Term),
	!.
substitute(Term, Var, NVar, NTerm) :-
	Term \== Var,
	compound(Term),
	functor(Term,  F, N),
	functor(NTerm, F, N),
	substitute(N, Term, Var, NVar, NTerm).

:- push_prolog_flag(multi_arity_warnings, off).

substitute(0, _Term, _Var, _NVar, _NTerm) :-
	!.
substitute(N, Term, Var, NVar, NTerm) :-
	N > 0,
	arg(N, Term, Arg),
	substitute(Arg, Var, NVar, NArg),
	arg(N, NTerm, NArg),
	N1 is N-1,
	substitute(N1, Term, Var, NVar, NTerm).

:- pop_prolog_flag(multi_arity_warnings).

% Added by EMM

build_resource_comp([],            _Comp, []).
build_resource_comp([_|Resources], Comp,  [Comp|ResourceComps]) :-
	build_resource_comp(Resources, Comp, ResourceComps).

% End Added by EMM
