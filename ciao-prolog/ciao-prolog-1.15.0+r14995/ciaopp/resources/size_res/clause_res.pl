:- module(clause_res,
	    [
		ith_clause_literal/3,
		number_of_literals/3,
		ith_body_literal/3,
		clause_term_measure/8
	    ], [assertions, resources(inferres_decl)]).

%
%  clause.pl			Nai-Wei Lin			December, 1991
%
%  This file contains the procedures for finding information about a clause
%  or a literal in a clause.
%

:- use_module(resources(init_res(symtable_res)), [literal_property/10]
	).
:- use_module(resources(init_res(initsystem_basic_res)), [clause_type/2]).
:- use_module(resources(top_res(utility_res)),
	    [ith_list_element/3, nonsequence/1]).
:- push_prolog_flag(unused_pred_warnings, no).
:- use_module(resources(dependency_res(position_res)),
	    [pos_litnum/2, pos_argnum/2, pos_t/1]).
:- pop_prolog_flag(unused_pred_warnings).

:- use_module(resources(init_res(builtin_res)),
	    [second_order_predicate_pred_arg/2]).
:- use_module(resources(resources_basic)).


:- pred clause_term_measure/8 :: approx * list * list * clause_ppkey_t * atm *
	pos_t * term * measure_t + (not_fails, is_det) #
	"Get the term and its measure at an argument position of a clause.".

clause_term_measure(Approx, BT, ST, ClausePPKey, Key, Pos, Term, Measure) :-
	pos_litnum(Pos, LitNum),
%write(LitNum),nl,
	( LitNum > 0 ->
	    ( clause_body(ClausePPKey, Body),
		number_of_literals(Body, 1, Num),
		( LitNum > Num ->
		    NewNum is LitNum-Num; % assume depth-one single pred
		    NewNum = LitNum ) ) ;
	    NewNum = LitNum ),
	ith_clause_literal(NewNum, ClausePPKey, LitPPKey),
	lit_ppkey(LitPPKey, Lit, PPKey),
%write(Lit),nl,
	( (LitNum > 0, LitNum > Num) ->
	    second_order_predicate_pred_arg(Lit, NewLit) ;
	    NewLit = Lit ),
	pos_argnum(Pos, ArgNum),
	arg(ArgNum, NewLit, Term),
	literal_property(BT, ST, NewLit, ClausePPKey, Key, PPKey, LitNum,
	    measure, Approx, MeasureList),
	ith_list_element(ArgNum, MeasureList, Measure).

%
%  Get the ith literal in the clause.
%
ith_clause_literal(I, Clause, Lit) :-
	clause_type(Clause, Type),
	ith_clause_literal_(Type, I, Clause, Lit).

ith_clause_literal_(rule, 0, Clause, Head:noinfo) :-
	!,
	clause_head(Clause, Head).
ith_clause_literal_(rule, I, Clause, Lit) :-
	I > 0,
	!,
	clause_body(Clause, Body),
	ith_body_literal(I, Body, Lit).
ith_clause_literal_(fact, 0, Clause, Head:noinfo) :-
	clause_head(Clause, Head).

%
%  Get the ith literal in the body.
%
ith_body_literal(1, (Lit, _), Lit) :-
	!.
ith_body_literal(1, Lit, Lit) :-
	!,
	nonsequence(Lit).
ith_body_literal(LitNum, (_, Body), Lit) :-
	LitNum > 1,
	LitNum1 is LitNum -1,
	ith_body_literal(LitNum1, Body, Lit).

%
number_of_literals(Lit, Num, Num) :-
	nonsequence(Lit),
	!.
number_of_literals((_, Body), N1, Num) :-
	N2 is N1 +1,
	number_of_literals(Body, N2, Num).
