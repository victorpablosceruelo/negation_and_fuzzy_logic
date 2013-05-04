:- module(mutual_exclusion_res,
	    [
		mutual_exclusive_classes/3,
		comparison_op/1
	    ], [assertions, resources(inferres_decl)]).

%
%  mutual_exclusion.pl		Nai-Wei Lin			April 1992
%
%  This file contains the procedures for classifying the mutually exclusive
%  classes of clauses.
%

:- use_module(library(lists),                            [append/3, length/2]).
:- use_module(resources(solution_res(relation_res)),     [fail_clause/1]).
:- use_module(resources(init_res(initsystem_basic_res)), [clause_type/2]).
:- use_module(resources(size_res(normalize__res)),       [substitute/4]).
:- use_module(resources(csp_res(constraint_res)),        [arithmetic_op/1]).
:- use_module(resources(top_res(utility_res)),           [member/2]).
:- use_module(resources(resources_basic)).

:- pred mutual_exclusive_classes/3 :: list(clause_key_t) * list(mode_t) * list
	+ (not_fails, is_det) #
	"Find the mutually exclusive classes of clauses.".
mutual_exclusive_classes(ClauseKeys, Mode, Classes) :-
	initial_classes(ClauseKeys, IClasses),
	mutual_exclusive_classes(ClauseKeys, Mode, 1, IClasses, Classes).

%
%  Set up the initial empty classes.
%
initial_classes(ClauseKeys, []) :-
	var(ClauseKeys),
	!.
initial_classes(ClauseKeys, [[]|Classes]) :-
	nonvar(ClauseKeys),
	ClauseKeys = [_|Cs],
	initial_classes(Cs, Classes).

%
%  Find the mutually exclusive classes of clauses based on Prolog ordering.
%

:- push_prolog_flag(multi_arity_warnings, off).

mutual_exclusive_classes(ClauseKeys, _, _, _, []) :-
	var(ClauseKeys),
	!.
mutual_exclusive_classes([ClauseKey|Cs], Mode, I, IClasses, Classes) :-
	clause_key(ClauseKey, ClausePPKey, _Key),
	IClasses = [IC|ICs],
	mutual_exclusive_clauses(Cs, ClausePPKey, ICs, IC, Mode, I, NIClass,
	    Mutex, In),
	( (Mutex == 1 ; In == 1) ->
	    ( append(IC, [I], NIC),
		Classes = [NIC|Classes1] ) ;
	    Classes = Classes1 ),
	I1 is I +1,
	mutual_exclusive_classes(Cs, Mode, I1, NIClass, Classes1).

%
%  Check the mutual exclusion of a clause and its sucessors.
%
mutual_exclusive_clauses(ClauseKeys, _, _, _, _, _, [], 1, 1) :-
	var(ClauseKeys),
	!.
mutual_exclusive_clauses([ClauseKey|Cs], ClausePPKey0, IClasses, IClass, Mode,
	    I, [NIC|NIClasses], Mutex, In) :-
	clause_key(ClauseKey, ClausePPKey, _Key),
	IClasses = [IC|ICs],
	( mutual_exclusive_clause(ClausePPKey0, ClausePPKey, Mode) ->
	    ( Mutex1 = 1, /* Clause & C are mutually exclusive */
		In1 = 1,
		NIC = IC ) ;
	    ( Mutex1 = 0,
		( set_inclusion(IClass, IC) ->
		    In1 = 0 ; /* Class IClass contained in IC */
		    In1 = 1 ),
		append(IC, [I], NIC) ) ), /* Put Clause in the same class as C */
	mutual_exclusive_clauses(Cs, ClausePPKey0, ICs, IClass, Mode, I,
	    NIClasses, Mutex2, In2),
	Mutex is Mutex1*Mutex2,
	In is In1*In2.

:- pred mutual_exclusive_clause/3 :: clause_ppkey_t * clause_ppkey_t *
	list(mode_t) + is_det # "Check if clauses C1 & C2
	are mutually exclusive. It is true if C1 contains a cut (!) or
	the input arguments of C1 and C2 are not unifiable.".
mutual_exclusive_clause(C1, C2, Mode) :-
	mutual_exclusive_clause1(C1, C2, Mode).

:- pred mutual_exclusive_clause1/3 :: clause_ppkey_t * clause_ppkey_t *
	list(mode_t) + is_det.
mutual_exclusive_clause1(C1, C2, Mode) :-
	( (fail_clause(C1) ; fail_clause(C2)) ->
	    true ; /* At least one of the clauses fails */
	    mutual_exclusive_clause2(C1, C2, Mode) ).

:- pred mutual_exclusive_clause2/3 :: clause_ppkey_t * clause_ppkey_t
	* list(mode_t) + is_det.
mutual_exclusive_clause2(C1, C2, Mode) :-
	( cut_clause(C1) ->
	    true ; /* C1 contains a cut */
	    mutual_exclusive_clause3(C1, C2, Mode) ).

:- pred mutual_exclusive_clause3/3 :: clause_ppkey_t * clause_ppkey_t
	* list(mode_t) + is_det.
mutual_exclusive_clause3(C1, C2, Mode) :-
	copy_term(C1, T1),
	clause_head(T1, H1),
	input_term(Mode, H1, 1, I1),
	copy_term(C2, T2),
	clause_head(T2, H2),
	input_term(Mode, H2, 1, I2),
	( non_unifiable(I1, I2) ->
	    true ; /* heads are not unifiable */
	    ( mutual_exclusive_clause4(I1, I2, T1, T2) ->
		true ;
		(mutual_exclusive_clause5(I1, I2, T1, T2)) ) ).

mutual_exclusive_clause4([A1|I1], [A2|I2], C1, C2) :-
	number(A1),
	var(A2), !,
	( mutual_exclusive_clause_number(C2, A2, 'arithmetic:=:='(A2, A1))
	-> true
	; mutual_exclusive_clause4(I1, I2, C1, C2) ).
mutual_exclusive_clause4([A1|I1], [A2|I2], C1, C2) :-
	number(A2),
	var(A1), !,
	( mutual_exclusive_clause_number(C1, A1, 'arithmetic:=:='(A1, A2))
	-> true
	; mutual_exclusive_clause4(I1, I2, C1, C2) ).
mutual_exclusive_clause4([A1|I1], [A2|I2], C1, C2) :-
	atom(A1),
	var(A2), !,
	( mutual_exclusive_clause_number(C2, A2, 'arithmetic:=:='(A2, A1))
	-> true
	; mutual_exclusive_clause4(I1, I2, C1, C2) ).
mutual_exclusive_clause4([A1|I1], [A2|I2], C1, C2) :-
	atom(A2),
	var(A1), !,
	( mutual_exclusive_clause_number(C1, A1, 'arithmetic:=:='(A1, A2))
	-> true
	; mutual_exclusive_clause4(I1, I2, C1, C2) ).
mutual_exclusive_clause4([_|I1], [_|I2], C1, C2) :-
	mutual_exclusive_clause4(I1, I2, C1, C2).

mutual_exclusive_clause5(I1, I2, C1, C2) :-
	variable_set(I1, [], V1),
	length(V1, L1),
	variable_set(I2, [], V2),
	length(V2, L2),
	L1 == L2,
	list_substitute(V2, V1, I2, I3),
	I1 == I3, !,
	list_substitute(V2, V1, C2, C3),
	mutual_exclusive_clauses5(V1, C1, C3).

mutual_exclusive_clauses5([V|_], C1, C2) :-
	get_comparison_literal(C1, V, L1),
	mutual_exclusive_clause_number(C2, V, L1),
	!.
mutual_exclusive_clauses5([_|V1], C1, C2) :-
	mutual_exclusive_clauses5(V1, C1, C2).

variable_set([],     V, V).
variable_set([I|Is], V, NV) :-
	variable_set1(I, V, V1),
	variable_set(Is, V1, NV).

variable_set1(I, V, V1) :-
	var(I), !,
	( utility_res:member(V, I) ->
	    V1 = V;
	    V1 = [I|V] ).
variable_set1(I, V, V) :-
	atomic(I), !.
variable_set1(I, V, NV) :-
	functor(I, _, A),
	variable_set1(A, I, V, NV).

variable_set1(0, _, V, V).
variable_set1(A, I, V, NV) :-
	A > 0,
	arg(A, I, Arg),
	variable_set1(Arg, V, V1),
	A1 is A -1,
	variable_set1(A1, I, V1, NV).

:- pop_prolog_flag(multi_arity_warnings).

list_substitute([],       [],       T,  T).
list_substitute([V1|V1s], [V2|V2s], T1, T2) :-
	substitute(T1, V1, V2, T12),
	list_substitute(V1s, V2s, T12, T2).

get_comparison_literal(C, V, L) :-
	clause_type(C, rule),
	C = (_ :- Body),
	get_comparison_literal1(Body, V, L).

get_comparison_literal1((LitPPKey, Body), V, L) :-
	!,
	lit_ppkey(LitPPKey, Lit, _PPKey),
	(
	    functor(Lit, F, A),
	    comparison_op(F/A),
	    convert_op(Lit, V, L) ->
	    true
	;
	    get_comparison_literal1(Body, V, L)
	).
get_comparison_literal1(LitPPKey, V, L) :-
	lit_ppkey(LitPPKey, Lit, _PPKey),
	functor(Lit, F, A),
	comparison_op(F/A),
	convert_op(Lit, V, L).

%
mutual_exclusive_clause_number(Clause, Var, Expr) :-
	clause_type(Clause, rule),
	Clause = (_ :- Body),
	mutual_exclusive_body_number(Body, Var, Expr).

mutual_exclusive_body_number((LitPPKey, Body), Var, Expr) :-
	!,
	lit_ppkey(LitPPKey, Lit, _PPKey),
	functor(Lit, F, A),
	mutual_allowable_op((F/A)),
	( mutual_exclusive_literal_number(Lit, Var, Expr) ->
	    true ;
	    mutual_exclusive_body_number(Body, Var, Expr) ).
mutual_exclusive_body_number(LitPPKey, Var, Expr) :-
	lit_ppkey(LitPPKey, Lit, _PPKey),
	functor(Lit, F, A),
	mutual_allowable_op((F/A)),
	mutual_exclusive_literal_number(Lit, Var, Expr).

mutual_exclusive_literal_number(Lit, Var, Expr) :-
	functor(Lit,  F1, A1),
	functor(Expr, F2, A2),
	( (comparison_op((F1/A1)), convert_op(Lit, Var, NLit)) ->
	    ( functor(NLit, F3, A3),
		comparable_op((F3/A3), (F2/A2), Sign),
		( Sign > 0 ->
		    mutual_exclusive_number(NLit, Expr) ;
		    mutual_exclusive_number(Expr, NLit) ) ) ).

convert_op('term_compare:=='(E1, E2), Var, 'arithmetic:=:='(E1, E2)) :-
	E1 == Var, !.
convert_op('term_compare:=='(E1, E2), Var, 'arithmetic:=:='(E2, E1)) :-
	E2 == Var, !.
convert_op('term_compare:\\=='(E1, E2), Var, 'arithmetic:=\\='(E1, E2))
:- E1 == Var, !.
convert_op('term_compare:\\=='(E1, E2), Var, 'arithmetic:=\\='(E2, E1))
:- E2 == Var, !.
convert_op(Lit, Var, Lit) :-
	arg(1, Lit, Arg),
	Arg == Var, !.
convert_op(Lit, Var, NLit) :-
	arg(2, Lit, Arg),
	Arg == Var,
	functor(Lit,  F, A),
	functor(NLit, F, A),
	arg(1, NLit, Arg),
	arg(1, Lit,  Arg1),
	arg(2, NLit, Arg1).

mutual_allowable_op(Op) :-
	comparison_op(Op).
mutual_allowable_op((is) /2).
mutual_allowable_op(('term_basic:functor') /3).
mutual_allowable_op(('term_basic:arg') /3).
mutual_allowable_op(('term_typing:atomic') /1).
mutual_allowable_op(('term_typing:atom') /1).
mutual_allowable_op(('term_typing:number') /1).
mutual_allowable_op(('term_typing:integer') /1).
mutual_allowable_op(('term_typing:float') /1).
mutual_allowable_op((true) /0).
mutual_allowable_op((!) /0).

comparison_op(Op) :-
	arithmetic_op(Op).
comparison_op(('term_compare:==') /2).
comparison_op(('term_compare:\\==') /2).

comparable_op(('arithmetic:>') /2,    ('arithmetic:=:=') /2,  1).
comparable_op(('arithmetic:=:=') /2,  ('arithmetic:>') /2,    -1).
comparable_op(('arithmetic:>=') /2,   ('arithmetic:=:=') /2,  1).
comparable_op(('arithmetic:=:=') /2,  ('arithmetic:>=') /2,   -1).
comparable_op(('arithmetic:<') /2,    ('arithmetic:=:=') /2,  1).
comparable_op(('arithmetic:=:=') /2,  ('arithmetic:<') /2,    -1).
comparable_op(('arithmetic:=<') /2,   ('arithmetic:=:=') /2,  1).
comparable_op(('arithmetic:=:=') /2,  ('arithmetic:=<') /2,   -1).
comparable_op(('arithmetic:=\\=') /2, ('arithmetic:=:=') /2,  1).
comparable_op(('arithmetic:=:=') /2,  ('arithmetic:=\\=') /2, -1).
comparable_op(('arithmetic:=:=') /2,  ('arithmetic:=:=') /2,  1).
comparable_op(('arithmetic:=:=') /2,  ('arithmetic:=:=') /2,  -1).
comparable_op(('arithmetic:>') /2,    ('arithmetic:<') /2,    1).
comparable_op(('arithmetic:<') /2,    ('arithmetic:>') /2,    -1).
comparable_op(('arithmetic:>=') /2,   ('arithmetic:<') /2,    1).
comparable_op(('arithmetic:<') /2,    ('arithmetic:>=') /2,   -1).
comparable_op(('arithmetic:>') /2,    ('arithmetic:=<') /2,   1).
comparable_op(('arithmetic:=<') /2,   ('arithmetic:>') /2,    -1).
comparable_op(('arithmetic:>=') /2,   ('arithmetic:=<') /2,   1).
comparable_op(('arithmetic:=<') /2,   ('arithmetic:>=') /2,   -1).

mutual_exclusive_number(
	    'arithmetic:>'(E1, E2),
	    'arithmetic:=:='(E3, E4)) :-
	E1 == E3,
	number(E2),
	number(E4),
	!,
	E2 >= E4.
mutual_exclusive_number(
	    'arithmetic:>='(E1, E2),
	    'arithmetic:=:='(E3, E4)) :-
	E1 == E3,
	number(E2),
	number(E4),
	!,
	E2 > E4.
mutual_exclusive_number(
	    'arithmetic:<'(E1, E2),
	    'arithmetic:=:='(E3, E4)) :-
	E1 == E3,
	number(E2),
	number(E4),
	!,
	E2 =< E4.
mutual_exclusive_number(
	    'arithmetic:=<'(E1, E2),
	    'arithmetic:=:='(E3, E4)) :-
	E1 == E3,
	number(E2),
	number(E4),
	!,
	E2 < E4.
mutual_exclusive_number(
	    'arithmetic:=\\='(E1, E2),
	    'arithmetic:=:='(E3, E4)) :-
	E1 == E3,
	number(E2),
	number(E4),
	!,
	E2 =:= E4.
mutual_exclusive_number(
	    'arithmetic:=:='(E1, E2),
	    'arithmetic:=:='(E3, E4)) :-
	E1 == E3,
	number(E2),
	number(E4),
	!,
	E2 =\= E4.
mutual_exclusive_number(
	    'arithmetic:>'(E1, E2),
	    'arithmetic:<'(E3, E4)) :-
	E1 == E3,
	number(E2),
	number(E4),
	!,
	E2 >= E4.
mutual_exclusive_number(
	    'arithmetic:>='(E1, E2),
	    'arithmetic:<'(E3, E4)) :-
	E1 == E3,
	number(E2),
	number(E4),
	!,
	E2 >= E4.
mutual_exclusive_number(
	    'arithmetic:>'(E1, E2),
	    'arithmetic:=<'(E3, E4)) :-
	E1 == E3,
	number(E2),
	number(E4),
	!,
	E2 >= E4.
mutual_exclusive_number(
	    'arithmetic:>='(E1, E2),
	    'arithmetic:=<'(E3=<E4)) :-
	E1 == E3,
	number(E2),
	number(E4),
	!,
	E2 > E4.
mutual_exclusive_number(
	    'arithmetic:=\\='(E1, E2),
	    'arithmetic:=:='(E3, E4)) :-
	E1 == E3,
	atom(E2),
	atom(E4),
	!,
	E2 == E4.
mutual_exclusive_number(
	    'arithmetic:=:='(E1, E2),
	    'arithmetic:=:='(E3, E4)) :-
	E1 == E3,
	atom(E2),
	atom(E4),
	!,
	E2 \== E4.
mutual_exclusive_number(
	    'arithmetic:>'(E1, E2),
	    'arithmetic:=:='(E3, E4)) :-
	E1 == E3,
	var(E2),
	var(E4),
	!,
	E2 == E4.
mutual_exclusive_number(
	    'arithmetic:<'(E1, E2),
	    'arithmetic:=:='(E3, E4)) :-
	E1 == E3,
	var(E2),
	var(E4),
	!,
	E2 == E4.
mutual_exclusive_number(
	    'arithmetic:=\\='(E1, E2),
	    'arithmetic:=:='(E3, E4)) :-
	E1 == E3,
	var(E2),
	var(E4),
	!,
	E2 == E4.
mutual_exclusive_number(
	    'arithmetic:>'(E1, E2),
	    'arithmetic:<'(E3, E4)) :-
	E1 == E3,
	var(E2),
	var(E4),
	!,
	E2 == E4.
mutual_exclusive_number(
	    'arithmetic:>='(E1, E2),
	    'arithmetic:<'(E3, E4)) :-
	E1 == E3,
	var(E2),
	var(E4),
	!,
	E2 == E4.
mutual_exclusive_number(
	    'arithmetic:>'(E1, E2),
	    'arithmetic:=<'(E3, E4)) :-
	E1 == E3,
	var(E2),
	var(E4),
	!,
	E2 == E4.

%
%  Test if a clause contains a cut.
%
cut_clause(C) :-
	clause_type(C, rule),
	C = (_ :- Body),
	cut_body(Body).

cut_body((LitPPKey, Body)) :-
	!,
	( lit_ppkey(LitPPKey, (!), _) ->
	    true ;
	    cut_body(Body) ).
cut_body(LitPPKey) :-
	lit_ppkey(LitPPKey, (!), _).

%  Check if two terms are not unifiable.
%
non_unifiable(C, C) :- !, fail.
non_unifiable(_, _).

%
%  Get the input arguments of an atom based on its mode.
% 
input_term([],       _,    _, []).
input_term([+|Mode], Term, I, [Arg|Is]) :-
	arg(I, Term, Arg),
	I1 is I +1,
	input_term(Mode, Term, I1, Is).
input_term([-|Mode], Term, I, Is) :-
	I1 is I +1,
	input_term(Mode, Term, I1, Is).

%
%  Test if a set is included in another set.
%
set_inclusion([],     _).
set_inclusion([E|S1], S2) :-
	( utility_res:member(S2, E) ->
	    set_inclusion(S1, S2) ).
