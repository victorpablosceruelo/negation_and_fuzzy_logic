:- module(term_diff_res, [general_term_diff/9, term_diff/6, term_diff/5,
		term_diff_int/3, term_diff_length/3, term_diff_depth/4,
		term_diff_depth/5, term_diff_size/4, term_diff_size/5],
	    [assertions, resources(inferres_decl)]).

%
%  term_diff.pl			Nai-Wei Lin			November, 1991
%
%  This file contains the procedures for performing the diff functions.
%

% Modified by Edison Mera 2006-07-06
% - all predicates tested

:- use_module(resources(top_res(utility_res)),
	    [
		minimum/3,
		addition/3,
		noncompound/1,
		sub/3,
		member/2,
		compound/1
	    ]).

:- use_module(resources(size_res(clause_res)), [clause_term_measure/8]).

%
%  Compute the size difference between a term and its predecessors.
%

general_term_diff(Approx, BT, ST, Measure, ClausePPKey, Key, PList, Term, Size) :-
	general_term_diff_(PList, Approx, BT, ST, Measure, ClausePPKey, Key, Term,
	    Size).

general_term_diff_([], _, _, _, _, _, _, _, bot).
general_term_diff_([Pos|PList], Approx, BT, ST, Measure, ClausePPKey, Key, Term,
	    Size) :-
	clause_term_measure(Approx, BT, ST, ClausePPKey, Key, Pos, PosTerm,
	    PosMeasure),
	term_diff(PosMeasure, Measure, Pos, PosTerm, Term, Size1),
	general_term_diff_(PList, Approx, BT, ST, Measure, ClausePPKey, Key, Term,
	    Size2),
	minimum(Size1, Size2, Size).

%
%  Compute the size difference between two terms.
%

:- test term_diff(A, B, C, D, E, F) : ( A= length, B= length, C=0, D=[G|H],
	    E=H ) => (F= -1).

term_diff(M1, M2, _Pos, _Term1, _Term2, bot) :-
	M1 \== (?),
	M2 \== (?),
	M1 \== M2,
	!.
term_diff((?), (?), _Pos, _Term1, _Term2, bot) :-
	!.
term_diff((?), M2, Pos, Term1, Term2, Size) :-
	M2 \== (?),
	!,
	term_diff(M2, Pos, Term1, Term2, Size).
term_diff(M1, (?), Pos, Term1, Term2, Size) :-
	M1 \== (?),
	!,
	term_diff(M1, Pos, Term1, Term2, Size).
term_diff(M1, M2, Pos, Term1, Term2, Size) :-
	M1 == M2,
	M1 \== (?),
	!,
	term_diff(M1, Pos, Term1, Term2, Size).

:- push_prolog_flag(multi_arity_warnings, off).

:- test term_diff(A, B, C, D, E):(A= length, B=5, C=[F|G], D=G)=>(E=4).

term_diff(int, Pos, Term1, Term2, Size) :-
	term_diff_int(Term1, Term2, Size1),
	addition(Pos, Size1, Size).
term_diff(length, Pos, Term1, Term2, Size) :-
	term_diff_length(Term1, Term2, Size1),
	addition(Pos, Size1, Size).
term_diff(depth(ChildList), Pos, Term1, Term2, Size) :-
	term_diff_depth(ChildList, Term1, Term2, Size1),
	addition(Pos, Size1, Size).
term_diff(size, Pos, Term1, Term2, Size) :-
	term_diff_size(Pos, Term1, Term2, Size).

%
%  Compute the size difference between two terms under the measure int.
%

:- test term_diff_int(A, B, C) => (C = bot).
:- test term_diff_int(A, B, C) : (A = B) => (C = 0).

term_diff_int(Term1, Term2, 0) :-
	Term1 == Term2,
	!.
term_diff_int(Term1, Term2, bot) :-
	Term1 \== Term2.

%
%  Compute the size difference between two terms under the measure length.
%

:- test term_diff_length(A, B, C):(A=[_|E], B=E)=>(C= -1).

term_diff_length(Term1, Term2, 0) :-
	Term1 == Term2,
	!.
term_diff_length(Term1, Term2, bot) :-
	Term1 \== Term2,
	noncompound(Term1),
	!.
term_diff_length(Term1, Term2, Size) :-
	Term1 \== Term2,
	Term1 = [_|TList],
	!,
	term_diff_length(TList, Term2, Size1),
	sub(Size1, 1, Size).

%
%  Compute the size difference between two terms under the measure depth.
%

:- test term_diff_depth(A, B, C, D) : (A = [1, 2], B = f(E, F), C = E)
	=> (D = -1).

term_diff_depth(_, Term1, Term2, 0) :-
	Term1 == Term2,
	!.
term_diff_depth(_, Term1, Term2, bot) :-
	Term1 \== Term2,
	noncompound(Term1),
	!.
term_diff_depth(ChildList, Term1, Term2, Size) :-
	Term1 \== Term2,
	compound(Term1),
	functor(Term1, _, N),
	term_diff_depth(N, ChildList, Term1, Term2, Size1),
	sub(Size1, 1, Size).

:- test term_diff_depth(A, B, C, D, E)
	: (A = 2, B = [1, 2], C = f(F, G), D = F) => (E = 0).

term_diff_depth(0, _, _, _, bot) :-
	!.
term_diff_depth(N, ChildList, Term1, Term2, Size) :-
	N > 0,
	N1 is N -1,
	(
	    utility_res:member(ChildList, N) ->
	    (
		arg(N, Term1, Arg),
		term_diff_depth(ChildList, Arg, Term2, SizeN),
		term_diff_depth(N1, ChildList, Term1, Term2, SizeN1),
		minimum(SizeN, SizeN1, Size)
	    )
	;
	    term_diff_depth(N1, ChildList, Term1, Term2, Size)
	).

%
%  Compute the size difference between two terms under the measure size.
%

:- test term_diff_size(A, B, C, D) : (A = 0, B = s(E), C = E)
	=> (D = arg(0, 1)).

term_diff_size(Pos, Term1, Term2, Pos) :-
	Term1 == Term2,
	!.
term_diff_size(Pos, [Head|_], Term, head(Pos)) :-
	Head == Term,
	!.
term_diff_size(Pos, [_|Tail], Term, tail(Pos)) :-
	Tail == Term,
	!.
term_diff_size(_, Term1, Term2, bot) :-
	Term1 \== Term2,
	noncompound(Term1),
	!.
term_diff_size(Pos, Term1, Term2, Size) :-
	Term1 \== Term2,
	compound(Term1),
	functor(Term1, _, N),
	term_diff_size(N, Pos, Term1, Term2, Size).

:- test term_diff_size(A, B, C, D, E) : (A = 1, B = 1, C = s(F), D = F)
	=> (E = arg(1, 1)).

term_diff_size(0, _, _, _, bot) :-
	!.
term_diff_size(N, Pos, Term1, Term2, Size) :-
	N > 0,
	N1 is N -1,
	arg(N, Term1, Arg),
	(
	    Arg == Term2 ->
	    Size = arg(Pos, N)
	;
	    term_diff_size(N1, Pos, Term1, Term2, Size)
	).

:- pop_prolog_flag(multi_arity_warnings).
