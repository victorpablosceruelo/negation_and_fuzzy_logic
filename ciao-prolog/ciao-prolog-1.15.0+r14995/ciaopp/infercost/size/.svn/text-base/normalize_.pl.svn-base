:- module(normalize_,
	[
	    find_recursive_comp/3,
	    init_normalize_queue/3,
	    literal_output_comp/11,
	    normalize/13,
	    substitute/4,
	    substitute_literal_formal/4
	], [assertions]).

%
%  normalize.pl			Nai-Wei Lin			December, 1991
%
%  This file contains the procedures for performing normalization.
%

:- use_module(infercost(init(builtin)), 
	[
	    second_order_predicate/1,
 	    second_order_predicate_pred_arg/2,
	    second_order_predicate_pred_num/3
	]).
:- use_module(infercost(init(symtable)), [find_symbol_field/4]).
:- use_module(infercost(top(utility)), 
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
:- use_module(infercost(top(error)), [error_message/3]).
:- use_module(infercost(dependency(adg)), [find_adg_field/4]).
:- use_module(infercost(dependency(position)), 
	[
	    pos_litnum/2,
	    pos_argnum/2,
	    gen_literal_iopos/5,
	    new_pos/3
	]).
:- use_module(infercost(size(term_size)), [general_term_size/8]).
:- use_module(infercost(size(clause)), 
	[
	    clause_term_measure/6,
	    ith_body_literal/3
	]).

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

%
%  Normalize an expression by the sizes in the normalization queue.
%
normalize(bot, _QHead, _QTail, _, _, _, _, _, _, _, _, _, bot) :-
	!.
normalize(Exp, QHead, QTail, _, _, _, _, _, _, _, _, _, Exp) :-
	Exp \== bot,
	empty_queue(QHead, QTail),
	!.
normalize(Exp, QHead, QTail, BT, ST, Comp, Clause, Adg, Gvars, PosSet, ISize,
	    RSize, NExp) :-
	Exp \== bot,
	nonempty_queue(QHead, QTail),
	get_queue(QHead, Pos, NHead),
	(
	    subterm(Pos, Exp) ->
	    normalize_pos(Exp, Pos, NHead, QTail, BT, ST, Comp, Clause, Adg,
	        Gvars, PosSet, ISize, RSize, NExp)
	;
	    normalize(Exp, NHead, QTail, BT, ST, Comp, Clause, Adg,
	        Gvars, PosSet, ISize, RSize, NExp)
	).

%
%  Normalize an expression by the size of a position.
%
normalize_pos(Exp, Pos, QHd, QTl, BT, ST, Comp, Clause, Adg, Gvars, PosSet,
	    ISize, RSize, NExp) :-
	find_adg_field(Adg, Pos, (mode), +),
	pos_litnum(Pos, 0),
	!,
	pos_argnum(Pos, ArgNum),
	ith_list_element(ArgNum, ISize, Size),
	normalize_size(Exp, Pos, Size, QHd, QTl, BT, ST, Comp, Clause, Adg,
	    Gvars, PosSet, ISize, RSize, NExp).
normalize_pos(Exp, Pos, QHd, QTl, BT, ST, Comp, Clause, Adg, Gvars, PosSet,
	    ISize, RSize, NExp) :-
	find_adg_field(Adg, Pos, (mode), +),
	pos_litnum(Pos, LitNum),
	LitNum > 0,
	!,
	clause_term_measure(BT, ST, Clause, Pos, Term, Measure),
	general_term_size(Measure, Clause, BT, ST, Gvars, PosSet, Term, Size),
	normalize_size(Exp, Pos, Size, QHd, QTl, BT, ST, Comp, Clause, Adg,
	    Gvars, PosSet, ISize, RSize, NExp).
normalize_pos(Exp, Pos, QHd, QTl, BT, ST, Comp, Clause, Adg, Gvars, PosSet,
	    ISize, RSize, NExp) :-
	find_adg_field(Adg, Pos, (mode), -),
	pos_litnum(Pos, LitNum),
	pos_argnum(Pos, ArgNum),
	arg(2, Clause, Body),
	ith_body_literal(LitNum, Body, Lit),
	functor(Lit, F, N),
	(   second_order_predicate(F/N) ->
	    (
		second_order_predicate_pred_arg(Lit, Lit1),
		functor(Lit1, F1, N1),
		second_order_predicate_pred_num(Body, LitNum, Num),
		literal_output_comp(F1/N1, Lit, Num, 1, BT, ST, [], Adg, det,
		    [], Size)
	    )
	;
	    literal_output_comp(F/N, Lit, LitNum, ArgNum, BT, ST, Comp, Adg,
	        size, RSize, Size)
	),
	normalize_size(Exp, Pos, Size, QHd, QTl, BT, ST, Comp, Clause, Adg,
	    Gvars, PosSet, ISize, RSize, NExp).

%
%  Normalize an expression by a size.
%
normalize_size(_Exp, _Pos, bot, _QHead, _QTail, _BT, _ST, _Comp, _Clause, _Adg,
	    _Gvars, _PosSet, _ISize, _RSize, bot) :-
	!.
normalize_size( Exp,  Pos, Size, QHead,  QTail,  BT,  ST,  Comp,  Clause,  Adg,
	     Gvars,  PosSet,  ISize,  RSize, NExp) :-
	Size \== bot,
	substitute(Exp, Pos, Size, Exp1),
	insert_normalize_queue(Pos, Adg, QTail, NTail),
	normalize(Exp1, QHead, NTail, BT, ST, Comp, Clause, Adg, Gvars, PosSet,
	    ISize, RSize, NExp).

%
%  Get the output complexity function of a literal in the clause.
%

% Added by PLG:
% LitName: name/arity of the literal predicate.
% LitNum: ordinal of the literal in the clause.
% ArgNum: ordinal of the argument.
% BT: builtin table.
% ST: symbol table.
% Component, Adg, Type, RComp, Comp:

literal_output_comp(LitName, Lit, LitNum, ArgNum, BT, ST, Component, Adg,
	    Type, RComp, Comp) :-
	find_symbol_field(BT, LitName, Type, BuildinComp),
	lit_output_comp(BuildinComp, ST, Component, LitName, Lit, LitNum,
	    ArgNum, Adg, Type, RComp, Comp).

% From this point the code needs refactoring:

bottom(_, bot).

lit_output_comp(BuildinComp, ST, Component, LitName, _Lit, LitNum, ArgNum,
	    Adg, Type, RComp, Comp) :-
	var(BuildinComp),
	!,
	(
	    utility:member(Component, LitName) ->
	    rec_literal_comp(Adg, LitName, LitNum, ArgNum, RComp, Comp)
	;
	    (
		find_symbol_field(ST, LitName, Type, LitComp),
		(
		    var(LitComp) ->
		    (
			error_message(dec1, LitName, ''),
			bottom(Type, Comp)
		    )
		;
		    nonrec_literal_comp(LitComp, LitName, LitNum, ArgNum,
				Comp)
		)
	    )
	).

lit_output_comp(BuildinComp, _, _, LitName, _Lit, LitNum, ArgNum, Adg, Type,
	    _, Comp) :-
	nonvar(BuildinComp),
	buildin_output_comp(Type, LitName, LitNum, ArgNum, Adg, Comp).

%
%  Generate the symbolic output complexity function for a recursive literal.
%

gen_init_comp(F, A, ArgNum, Pos, Comp) :-
	Comp =.. [F, A, ArgNum|Pos].

rec_literal_comp(Adg, LitName, LitNum, ArgNum, RComp, Comp) :-
	find_recursive_comp(RComp, LitName, Rcomp),
	LitName = F/A,
	(var(Rcomp) ->
		(gen_literal_iopos(Adg, LitName, LitNum, (+), Pos),
		 gen_init_comp(F, A, ArgNum, Pos, Comp)); % EMM
		(ith_list_element(ArgNum, Rcomp, Comp1),
		 substitute_literal_formal(A, Comp1, LitNum, Comp))).

%
%  Find the mutual recursive complexity when available.
%
find_recursive_comp([], _, _) :- !.
find_recursive_comp([comp(LitName, Rcomp)|_], LitName, Rcomp) :- !.
find_recursive_comp([comp(Pred, _)|RComp], LitName, Rcomp) :-
	Pred \== LitName,
	find_recursive_comp(RComp, LitName, Rcomp).

%
%  Substitute the actuals for the formals in the output complexity functions 
%  of a nonrecursive literal.
%
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
substitute_literal_formal(0, Comp, _, Comp) :- !.
substitute_literal_formal(A, C, LitNum, Comp) :-
	A > 0,
	new_pos(0, A, Pos1),
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
	functor(Term, F, N),
	functor(NTerm, F, N),
	substitute(N, Term, Var, NVar, NTerm).

:- push_prolog_flag(multi_arity_warnings, off).

substitute(0, _Term, _Var, _NVar, _NTerm) :-
	!.
substitute(N,  Term,  Var,  NVar,  NTerm) :-
	N > 0,
	arg(N, Term, Arg),
	substitute(Arg, Var, NVar, NArg),
	arg(N, NTerm, NArg),
	N1 is N-1,
	substitute(N1, Term, Var, NVar, NTerm).

:- pop_prolog_flag(multi_arity_warnings).
	
%
%  Substitute the actuals for the formals in the output complexity function 
%  for an output position of a buildin predicate.
%
buildin_output_comp(size,  LitName,  LitNum,  ArgNum,  Adg, Comp) :-
	buildin_output_size(LitName, LitNum,  ArgNum,  Adg, Comp).
buildin_output_comp(det,  _LitName, _LitNum, _ArgNum, _Adg, 1).
buildin_output_comp(time, _LitName, _LitNum, _ArgNum, _Adg, 0).

%
%  Substitute the actuals for the formals in the output size function 
%  for an output position of a buildin predicate.
%
buildin_output_size(is/2, LitNum, 1, _, Pos) :-
	!,
	new_pos(LitNum, 2, Pos).
buildin_output_size((=)/2, LitNum, 1, Adg, Size) :-
	!,
	new_pos(LitNum, 1, Pos1),
	new_pos(LitNum, 2, Pos2),
	find_adg_field(Adg, Pos1, pred, Pos),
	(
	    var(Pos) ->
	    Size = bot
	;
	    Size = Pos2
	).
buildin_output_size((=)/2, LitNum, 2, Adg, Size) :-
	!,
	new_pos(LitNum, 1, Pos1),
	new_pos(LitNum, 2, Pos2),
	find_adg_field(Adg, Pos2, pred, Pos),
	(
	    var(Pos) ->
	    Size = bot
	;
	    Size = Pos1
	).
buildin_output_size(functor/3, _, 2, _, 1) :-
	!.
buildin_output_size(functor/3, LitNum, 3, _, arity(Pos)) :-
	!,
	new_pos(LitNum, 1, Pos).
buildin_output_size(arg/3, LitNum, 3, _, arg(Pos2, Pos1)) :-
	!,
	new_pos(LitNum, 1, Pos1),
	new_pos(LitNum, 2, Pos2).
buildin_output_size(functor1/3, _, 1, _, 1) :-
	!.
buildin_output_size(arg/4, LitNum, 4, _, Pos2+Pos3) :-
	!,
	new_pos(LitNum, 2, Pos2),
	new_pos(LitNum, 3, Pos3).
%buildin_output_size((=..)/2, LitNum, 2, _, arity(Pos)+1) :-
%	new_pos(LitNum, 1, Pos).
