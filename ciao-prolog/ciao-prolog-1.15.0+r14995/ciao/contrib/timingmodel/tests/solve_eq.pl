:- module(_, [main/0], [fsyntax, clpr]).

:- use_module(library(apply)).
:- use_module(library(aggregates)).
:- use_module(library(hiordlib)).
:- use_module(library(lists)).
:- use_module(library(sort)).
:- use_module(library(write)).
:- use_module(library(clpr(eval_r))).

% additional equations:
eq(0) := fail_1_0.
% eq(0) := trust_me_0_0.
% eq(0) := retry_me_else_0_0.

:- include('../estimatecommon/_all/calib2eq').

% seq( A, B ) :- eq( B, A ).
% seq( A, C1 - C2 ) :- seq( A + K, C1 ), seq( K, C2 ).
% seq( A, C1 + C2 ) :- seq( A - K, C1 ), seq( K, C2 ).
% seq( A, C1 + C2 ) :- seq( A + K, C1 ), seq( - K, C2 ).
% seq( A, C1 - C2 ) :- seq( A - K, C1 ), seq( - K, C2 ).
% seq( A, C1 - C2 ) :- seq( K + A, C1 ), seq( K, C2 ).
% seq( A, C2 - C1 ) :- seq( K - A, C1 ), seq( K, C2 ).
% seq( A, C1 + C2 ) :- seq( K + A, C1 ), seq( - K, C2 ).
% seq( A, - C1 - C2 ) :- seq( K - A, C1 ), seq( - K, C2 ).

bytecode(BC) :-
	eq(_A, B),
	bytecode_eq(B, BC).

% bytecode_eq( B, B ) :-
% 	B =.. [ F, N ],
% 	atom( F ),
% 	number( N ),
% 	!.
bytecode_eq(B, B) :-
	atom(B),
	!.
bytecode_eq(A + B, BC) :-
	bytecode_eq(A, BC)
    ; bytecode_eq(B, BC).
bytecode_eq(A - B, BC) :-
	bytecode_eq(A, BC)
    ; bytecode_eq(B, BC).
bytecode_eq(- A, BC) :-
	bytecode_eq(A, BC).
bytecode_eq(A * B, BC) :-
	num(A),
	bytecode_eq(B, BC).

:- use_module(library(math(matrix))).
:- use_module(library(llists)).

solution(Solution) :-
	matrix(Matrix, ByteCodes, Calibrators),
	length(Matrix, N),
	matrix_identity(N, I),
	transpose(Matrix, M1),
	matrix_multiply_transpose(M1, Inverse, I),
	!,
	matrix_multiply_transpose_c([Calibrators], Inverse, [Solution0]),
	maplist((''(B, S, D) :- D = (B=S)), ByteCodes, Solution0, Solution).

matrix_multiply_transpose_c([],           _,         []).
matrix_multiply_transpose_c([Row|Matrix], Transpose, [Element|Result]) :-
	matrix_vector_multiply_c(Transpose, Row, Element),
	matrix_multiply_transpose_c(Matrix, Transpose, Result).

matrix_vector_multiply_c([],           _,      []).
matrix_vector_multiply_c([Row|Matrix], Vector, [Element|Result]) :-
	vector_multiply_c(Row, Vector, Element),
	matrix_vector_multiply_c(Matrix, Vector, Result).

vector_multiply_c(V1, V2, S) :-
	map(V1, V2, scalar_multiply_c, 0, S).

scalar_multiply_c(A, B, S0, S) :-
	A .=. 0.0 -> S = S0
    ;
	as_number(A, F),
	( A .=. 1 -> S1 = B
	; A .=. -1 -> S1 = B
	; A .<. 0 -> NF is -F, S1 = NF * B
	; S1 = F * B
	),
	( S0 = 0 -> (A .<. 0 -> S = -S1; S = S1)
	; A .<. 0 -> S = S0 - S1
	; S = S0 + S1
	).

:- use_module(engine(attributes)).
as_number(A, N) :-
	as_float(A, F),
	N0 is round(F),
	(A .=. N0 -> N = N0 ; N = F).

matrix(Matrix, ByteCodes, Calibrators) :-
	findall(A = B,    eq(A, B),           Equations0),
	findall(ByteCode, bytecode(ByteCode), ByteCodes0),
	sort(ByteCodes0, ByteCodes),
	sort(Equations0, Equations),
	map(Equations, (''(X, Y) :- X=(Y=_)), Calibrators),
	define_matrix(Equations, ByteCodes, Matrix).

define_matrix([],     _, []).
define_matrix([E|Es], B, [V|M]) :-
	define_vector(B, E, V),
	define_matrix(Es, B, M).

define_vector(B, E, V) :-
	normalize_eq(E, NE),
	define_vector_normalized(B, NE, V).

define_vector_normalized([],     _,  []).
define_vector_normalized([B|Bs], NE, [X|V]) :-
	(member(C * B, NE) -> X = C ; X = 0),
	define_vector_normalized(Bs, NE, V).

normalize_eq(_ = E, N) :-
	normalize_eq_(E, [], N).

normalize_eq_(E + T, NE0, NE) :-
	!,
	normalize_term(T, TN),
	normalize_eq_(E, [TN|NE0], NE).
normalize_eq_(T, NE0, [TN|NE0]) :-
	normalize_term(T, TN).

normalize_term(C * B, C * B) :- !.
normalize_term(B,     1 * B).

% :- use_module(library(format)).
% maximacmd(solve(EL, BCL)) :-
% 	findall(A = B, eq(A, B),     EL),
% 	findall(BC,    bytecode(BC), BCL0),
% 	sort(BCL0, BCL),
% 	length(EL,  NE),
% 	length(BCL, NBC),
% 	% additionaleqs(AE),
% 	% length(AE, NAE),
% 	(
% 	    NE =:= NBC -> true
% 	;
% 	    format("{WARNING: Number of equations and bytecodes differs:",
% 		[]),
% 	    nl,
% 	    format("Bytecodes: ~w\nEquations: ~w}\n", [NBC, NE])
% 	).

% rankcmd :-
% 	matrix(Matrix0, _, _),
% 	Matrix =.. [matrix|Matrix0],
% 	display(rank(Matrix)),
% 	nl.

% main :-
% 	maximacmd(A),
% 	write(A),
% 	display(';').

main :-
	solution(S),
	write(solution([S])),
	write('.\n').
