:- module(minizinc_rt, _, [assertions, resdefs, regtypes, nativeprops,
	isomodes]).

:- use_package(ciaopp(examples(resources(minizinc(constraints_res))))).
:- use_module(library(llists)).

% a test, only for debugging
:- use_package(ciaopp(examples(resources(minizinc(minizinc_assrts))))).

% a) analysis
% b) run clpfd
% :- include('../ciao-lib/minizinc_clpfd').
% c) run fd
:- include(ciaopp(examples(resources(minizinc(minizinc_fd))))).
% b+c) always when run (just in case)
% :- include('../ciao-lib/minizinc_globals').

% common predicates:

% TYPE DECLARATIONS
% int: var;

% Type definitions.

:- regtype array/1.

% array/1 must not be defined as term/1 or any other built-in type,
% just to avoid undesirable substitutions made by the analyzer.

array(a(A)) :- gnd(A).

% array(A):- list(A).

:- regtype dint/1.
dint(DL .. DU):- int(DL), int(DU).

type_decl_int(_).

% ARRAY MANIPULATION
element(I,a(A),A_i):- arg(I,A,A_i).

build_array(a(S), Ss) :-
	functor(S, array, Ss).

create_var(_V).

eq_int(X, Y) :- eq(X, Y).

eq_dint(X, Y) :- eq(X, Y).

leq_dint_int(X, Y):- leq(X, Y).

plus_dint_int_var(X, Y, Z):- plus(X, Y, Z).

plus_int_dint_var(X, Y, Z):- plus(X, Y, Z).

plus_dint_dint_var(X, Y, Z):- plus(X, Y, Z).

plus_dint_dint_dint(X, Y, Z):- plus(X, Y, Z).

plus_int(X, Y, Z) :- plus(X, Y, Z).

mult_int_dint_var(X, Y, Z) :- mult(X, Y, Z).

xor_dint_dint_var(X, Y, Z) :- xor(X, Y, Z).

minus_dint_int_var(X, Y, Z):- minus(X, Y, Z).

minus_dint_dint_var(X, Y, Z):- minus(X, Y, Z).

minus_int(X, Y, Z) :- minus(X, Y, Z).

mult_int(X, Y, Z):- mult(X, Y, Z).

reif_eq_dint_int_var(A,B,C):- reif_eq(A,B,C).

reif_leq_dint_dint_var(A,B,C):- reif_leq(A,B,C).

or_dint_dint_int(A, B, C) :- or(A, B, C).

neq_dint_dint(A, B) :- neq(A, B).

leq_int_dint(A, B) :- leq(A, B).

element_array(I, A, A_i) :- element(I, A, A_i).
element_int(I, A, A_i)   :- element(I, A, A_i).
element_dint(I, A, A_i)  :- element(I, A, A_i).

element2d_int(I,J,A,A_ij):-
	element_array(I, A, A_i),
	element_int(J, A_i, A_ij).

element2d_dint(I,J,A,A_ij):-
	element_array(I, A, A_i),
	element_dint(J, A_i, A_ij).

is(A, B) :- arithmetic:is(A, B).

% GLOBAL CONSTRAINTS
all_different_(Array,_Size):- flatten(Array,List), all_different(List).
