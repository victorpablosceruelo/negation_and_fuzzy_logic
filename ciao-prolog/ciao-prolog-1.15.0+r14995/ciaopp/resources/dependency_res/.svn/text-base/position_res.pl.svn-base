:- module(position_res,
	    [
		gen_clause_pos/2,
		gen_literal_iopos/5,
		gen_literal_pos/3,
		new_pos/3,
		pos_argnum/2,
		pos_litnum/2,
%		Types
		pos_t/1
	    ], [assertions, regtypes]).

%
%  position.pl			Nai-Wei Lin			December, 1991
%
%  This file contains the procedures for handling the positions.
%

:- use_module(resources(dependency_res(adg_res)), [find_adg_field/4]).

%
:- op(200, fx, [$]).

:- regtype pos_t/1 # "Structure that represent a position.".
pos_t('$'(LitNum, ArgNum)) :-
	nnegint(LitNum),
	nnegint(ArgNum).

:- pred new_pos/3 :: nnegint * nnegint * pos_t
# "Construct a new position structure.".
new_pos(LitNum, ArgNum, '$'(LitNum, ArgNum)).

:- pred pos_litnum/2 :: pos_t * nnegint
# "Get the literal number of a position.".
pos_litnum('$'(LitNum, _), LitNum).

:- pred pos_argnum/2 :: pos_t * nnegint
# "Get the argument number of a position.".
pos_argnum('$'(_, ArgNum), ArgNum).

:- pred gen_clause_pos(Adg, Poss) :: term * list #
	"Generate the set of argument positions in a clause.".
gen_clause_pos(Adg, []) :-
	var(Adg),
	!.
gen_clause_pos(Adg, [Pos|PList]) :-
	nonvar(Adg),
	Adg = [adg(Pos, _, _, _)|AList],
	gen_clause_pos(AList, PList).

%
%  Generate the set of argument positions in a literal.
%
gen_literal_pos(_/N, LitNum, Pos) :-
	gen_lit_pos(0, N, LitNum, Pos).

gen_lit_pos(N, N, _,      []) :- !.
gen_lit_pos(M, N, LitNum, [Pos|PList]) :-
	M < N,
	M1 is M +1,
	new_pos(LitNum, M1, Pos),
	gen_lit_pos(M1, N, LitNum, PList).

%
%  Generate the set of input (or output) argument positions in a literal.
%
gen_literal_iopos(Adg, Lit, LitNum, Mode, Pos) :-
	gen_literal_pos(Lit, LitNum, Pos1),
	gen_lit_iopos(Pos1, Adg, Mode, Pos).

gen_lit_iopos([],         _,   _,    []).
gen_lit_iopos([P|PList1], Adg, Mode, Pos) :-
	find_adg_field(Adg, P, (mode), Mode1),
	( Mode1 == Mode ->
	    Pos = [P|PList] ;
	    PList = Pos ),
	gen_lit_iopos(PList1, Adg, Mode, PList).
