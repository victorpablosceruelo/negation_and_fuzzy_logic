:- package(minizinc_assrts).

:- use_package(basicmodes).
% :- module(_, _, [assertions, regtypes, nativeprops]).
%
% :- comment(module, "This module contains assertions for basic constraint related
%                     predicates.").

:- op(500, yfx, [..]).

% Resource assertions.

% TYPE DECLARATIONS
% We can have a predicate and trust assertion for each type: type_decl_int/1, type_decl_dint/1, etc.

% MZN: int: var;
% MGB what is this for? is it a type declaration or is it actually a variable declaration
%     i.e., is it creating the solver variable?

% Resource "constraints" counts the number of primitive (non-global) constrains which
% need to be established as propagators, i.e. that are not known to  fail/succeed immediately

% Resource numvars counts the number of variables created by the constraint
% i.e., they go from var to dint

:- trust pred type_decl_int(Var):
        (var(Var)) =>
        (int(Var)).

:- trust comp type_decl_int/1 + (is_det, not_fails).

% reif_eq(X, Y, R)

:- trust pred reif_eq(X, Y, R):
        (dint(X), int(Y), var(R)) =>
        (dint(X), int(Y), dint(R))
       +(cost(lb,constraints,1),
         cost(lb,numvars,1),
         cost(ub,constraints,1), %% it is posting a constraint
         cost(ub,numvars,1)).    %% variable R is created as a result (same for next two)

:- trust pred reif_eq(X, Y, R):
        (int(X), dint(Y), var(R)) =>
        (int(X), dint(Y), dint(R))
       +(cost(lb,constraints,1),
         cost(lb,numvars,1),
         cost(ub,constraints,1),
         cost(ub,numvars,1)).

:- trust pred reif_eq(X, Y, R):
        (dint(X), dint(Y), var(R)) =>
        (dint(X), dint(Y), dint(R))
       +(cost(lb,constraints,1),
         cost(lb,numvars,1),
         cost(ub,constraints,1),
         cost(ub,numvars,1)).

:- trust pred reif_eq(X, Y, R):
        (int(X), int(Y), var(R)) =>
        (int(X), int(Y), int(R))
       +(cost(lb,constraints,0),
         cost(lb,numvars,0),
         cost(ub,constraints,0), % this is not really a constraint, the solver just does a test
         cost(ub,numvars,0)).    % should not create a variable, if it does, is a fixed one

:- trust pred reif_eq(X, Y, R):
        (dint(X), int(Y), dint(R)) =>
        (dint(X), int(Y), dint(R))
       +(cost(lb,constraints,1),
         cost(lb,numvars,0),
         cost(ub,constraints,1), %% it is posting a constraint
         cost(ub,numvars,0)).    %% variable R was already created (same for next three)

:- trust pred reif_eq(X, Y, R):
        (int(X), dint(Y), dint(R)) =>
        (int(X), dint(Y), dint(R))
       +(cost(lb,constraints,1),
         cost(ub,constraints,1)).

:- trust pred reif_eq(X, Y, R):
        (dint(X), dint(Y), dint(R)) =>
        (dint(X), dint(Y), dint(R))
       +(cost(lb,constraints,1),
         cost(ub,constraints,1)).

:- trust pred reif_eq(X, Y, R):
        (int(X), int(Y), dint(R)) =>
        (int(X), int(Y), dint(R))
       +(cost(lb,constraints,0),
         cost(lb,numvars,0),
         cost(ub,constraints,0), % again not a constraint since it is resolved immediately
         cost(ub,numvars,0)).

:- trust pred reif_eq_dint_int_var(X, Y, R):
        (dint(X), int(Y), var(R)) =>
        (dint(X), int(Y), dint(R))
       +(cost(lb,constraints,1),
         cost(lb,numvars,1),
         cost(ub,constraints,1),
         cost(ub,numvars,1)).

:- trust comp reif_eq/3 + (is_det, not_fails).
:- trust comp reif_eq_dint_int_var/3 + (is_det, not_fails).

% eq(X, Y)  (X = Y, X == Y)

:- trust pred eq(X, Y):
        (dint(X), dint(Y)) =>
        (dint(X), dint(Y))
       +(cost(lb,constraints,1),
         cost(ub,constraints,1)).

:- trust pred eq(X, Y):
        (dint(X), int(Y)) =>
        (dint(X), int(Y))
       +(cost(lb,constraints,0),
         cost(ub,constraints,0)). %resolved immediately

:- trust pred eq(X, Y):
        (int(X), dint(Y)) =>
        (int(X), dint(Y)).

:- trust pred eq(X, Y):
        (int(X), int(Y)) =>
        (int(X), int(Y)).

:- trust comp eq(A, B) + (is_det, not_fails, size_metric(B, void)).

:- trust pred eq_int(X, Y):
        (int(X), term(Y)) =>
        (int(X), int(Y)).

:- trust comp eq_int(A, B) + (is_det, not_fails, size_metric(B, void)).

:- trust pred eq_dint(X, Y):
        (dint(X), term(Y)) =>
        (dint(X), dint(Y)).

:- trust comp eq_dint(A, B) + (is_det, not_fails, size_metric(B, void)).

% neq(X, Y)  (X \= Y)

:- trust pred neq(X, Y):
        (dint(X), dint(Y)) =>
        (dint(X), dint(Y))
       +(cost(lb,constraints,1),
         cost(ub,constraints,1)).

:- trust pred neq(X, Y):
        (dint(X), int(Y)) =>
        (dint(X), int(Y))
       +(cost(lb,constraints,0),
         cost(ub,constraints,0)). % assuming domain consistency

:- trust pred neq(X, Y):
        (int(X), dint(Y)) =>
        (int(X), dint(Y))
       +(cost(lb,constraints,0),
         cost(ub,constraints,0)). % assuming domain consistency

:- trust pred neq(X, Y):
         (int(X), int(Y)) =>
         (int(X), int(Y)).

:- trust pred neq_dint_dint(X, Y):
        (dint(X), dint(Y)) =>
        (dint(X), dint(Y))
       +(cost(lb,constraints,1),
         cost(ub,constraints,1)).

:- trust comp neq/2 + (is_det, not_fails).
:- trust comp neq_dint_dint/2 + (is_det, not_fails).

% xor(X, Y, R)

:- trust pred xor(X, Y, R):
        (dint(X), dint(Y), dint(R)) =>
        (dint(X), dint(Y), dint(R))
       +(cost(lb,constraints,1),
         cost(ub,constraints,1)).

:- trust pred xor(X, Y, R):
        (dint(X), dint(Y), var(R)) =>
        (dint(X), dint(Y), dint(R))
       +(cost(lb,constraints,1),
         cost(lb,numvars,1),
	 cost(ub,constraints,1),
         cost(ub,numvars,1)).

:- trust pred xor(X, Y, R):
        (int(X), dint(Y), var(R)) =>
        (int(X), dint(Y), dint(R))
       +(cost(lb,constraints,1),
         cost(lb,numvars,1),
	 cost(ub,constraints,1),
         cost(ub,numvars,1)).

:- trust pred xor(X, Y, R):
        (int(X), dint(Y), dint(R)) =>
        (int(X), dint(Y), dint(R))
       +(cost(lb,constraints,1),
         cost(ub,constraints,1)).

:- trust pred xor(X, Y, R):
        (dint(X), int(Y), var(R)) =>
        (dint(X), int(Y), dint(R))
       +(cost(lb,constraints,1),
         cost(lb,numvars,1),
	 cost(ub,constraints,1),
         cost(ub,numvars,1)).

:- trust pred xor(X, Y, R):
        (dint(X), int(Y), dint(R)) =>
        (dint(X), int(Y), dint(R))
       +(cost(lb,constraints,1),
         cost(ub,constraints,1)).

:- trust pred xor(X, Y, R):
        (int(X), int(Y), var(R)) =>
        (int(X), int(Y), int(R)).

:- trust pred xor(X, Y, R):
        (int(X), int(Y), dint(R)) =>
        (int(X), int(Y), dint(R)).

:- trust comp xor/3 + (is_det, not_fails).

:- trust pred xor_dint_dint_var(X, Y, R):
        (dint(X), dint(Y), var(R)) =>
        (dint(X), dint(Y), dint(R))
       +(cost(lb,constraints,1),
         cost(lb,numvars,1),
	 cost(ub,constraints,1),
         cost(ub,numvars,1)).

:- trust comp xor_dint_dint_var/3 + (is_det, not_fails).

% or(X, Y, R)

:- trust pred or(X, Y, R):
        (dint(X), dint(Y), int(R)) =>
        (dint(X), dint(Y), int(R))
       +(cost(lb,constraints,1),
         cost(ub,constraints,1)).

:- trust pred or_dint_dint_int(X, Y, R):
        (dint(X), dint(Y), int(R)) =>
        (dint(X), dint(Y), int(R))
       +(cost(lb,constraints,1),
         cost(ub,constraints,1)).

:- trust pred or(X, Y, R):
        (dint(X), dint(Y), dint(R)) =>
        (dint(X), dint(Y), dint(R))
       +(cost(lb,constraints,1),
         cost(ub,constraints,1)).

:- trust pred or(X, Y, R):
        (dint(X), dint(Y), var(R)) =>
        (dint(X), dint(Y), dint(R))
       +(cost(lb,constraints,1),
         cost(lb,numvars,1),
         cost(ub,constraints,1),
         cost(ub,numvars,1)).

:- trust pred or(X, Y, R):
        (int(X), dint(Y), var(R)) =>
        (int(X), dint(Y), dint(R))
       +(cost(lb,constraints,1),
         cost(lb,numvars,1),
         cost(ub,constraints,1),
         cost(ub,numvars,1)).

:- trust pred or(X, Y, R):
        (int(X), dint(Y), dint(R)) =>
        (int(X), dint(Y), dint(R))
       +(cost(lb,constraints,1),
         cost(ub,constraints,1)).

:- trust pred or(X, Y, R):
        (dint(X), int(Y), var(R)) =>
        (dint(X), int(Y), dint(R))
       +(cost(lb,constraints,1),
         cost(lb,numvars,1),
         cost(ub,constraints,1),
         cost(ub,numvars,1)).

:- trust pred or(X, Y, R):
        (dint(X), int(Y), dint(R)) =>
        (dint(X), int(Y), dint(R))
       +(cost(lb,constraints,1),
         cost(ub,constraints,1)).

:- trust pred or(X, Y, R):
        (int(X), int(Y), var(R)) =>
        (int(X), int(Y), int(R)).

:- trust pred or(X, Y, R):
        (int(X), int(Y), dint(R)) =>
        (int(X), int(Y), dint(R)).

:- trust comp or/3               + (is_det, not_fails).
:- trust comp or_dint_dint_int/3 + (is_det, not_fails).


% equiv(X, Y, R)

:- trust pred equiv(X, Y, R):
        (dint(X), dint(Y), int(R)) =>
        (dint(X), dint(Y), int(R))
       +(cost(lb,constraints,1),
         cost(ub,constraints,1)).

:- trust pred equiv(X, Y, R):
        (dint(X), dint(Y), dint(R)) =>
        (dint(X), dint(Y), dint(R))
       +(cost(lb,constraints,1),
         cost(ub,constraints,1)).

:- trust pred equiv(X, Y, R):
        (dint(X), dint(Y), var(R)) =>
        (dint(X), dint(Y), dint(R))
       +(cost(lb,constraints,1),
         cost(lb,numvars,1),
         cost(ub,constraints,1),
         cost(ub,numvars,1)).

:- trust pred equiv(X, Y, R):
        (int(X), dint(Y), var(R)) =>
        (int(X), dint(Y), dint(R))
       +(cost(lb,constraints,1),
         cost(lb,numvars,1),
         cost(ub,constraints,1),
         cost(ub,numvars,1)).

:- trust pred equiv(X, Y, R):
        (int(X), dint(Y), dint(R)) =>
        (int(X), dint(Y), dint(R))
       +(cost(lb,constraints,1),
         cost(ub,constraints,1)).

:- trust pred equiv(X, Y, R):
        (dint(X), int(Y), var(R)) =>
        (dint(X), int(Y), dint(R))
       +(cost(lb,constraints,1),
         cost(lb,numvars,1),
         cost(ub,constraints,1),
         cost(ub,numvars,1)).

:- trust pred equiv(X, Y, R):
        (dint(X), int(Y), dint(R)) =>
        (dint(X), int(Y), dint(R))
       +(cost(lb,constraints,1),
         cost(ub,constraints,1)).

:- trust pred equiv(X, Y, R):
        (int(X), int(Y), var(R)) =>
        (int(X), int(Y), int(R)).

:- trust pred equiv(X, Y, R):
        (int(X), int(Y), dint(R)) =>
        (int(X), int(Y), dint(R))
       +(cost(lb,constraints,1),
         cost(ub,constraints,1)).

:- trust comp equiv/3 + (is_det, not_fails).

% reif_leq(X, Y, R)

:- trust pred reif_leq(X, Y, R):
        (dint(X), int(Y), var(R)) =>
        (dint(X), int(Y), dint(R))
       +(cost(lb,constraints,1),
         cost(lb,numvars,1),
         cost(ub,constraints,1),
         cost(ub,numvars,1)).

:- trust pred reif_leq(X, Y, R):
        (int(X), dint(Y), var(R)) =>
        (int(X), dint(Y), dint(R))
       +(cost(lb,constraints,1),
         cost(lb,numvars,1),
         cost(ub,constraints,1),
         cost(ub,numvars,1)).

:- trust pred reif_leq(X, Y, R):
        (dint(X), dint(Y), var(R)) =>
        (dint(X), dint(Y), dint(R))
       +(cost(lb,constraints,1),
         cost(lb,numvars,1),
	 cost(ub,constraints,1),
         cost(ub,numvars,1)).

:- trust pred reif_leq_dint_dint_var(X, Y, R):
        (dint(X), dint(Y), var(R)) =>
        (dint(X), dint(Y), dint(R))
       +(cost(lb,constraints,1),
         cost(lb,numvars,1),
	 cost(ub,constraints,1),
         cost(ub,numvars,1)).


:- trust pred reif_leq(X, Y, R):
        (int(X), int(Y), var(R)) =>
        (int(X), int(Y), int(R)).

:- trust pred reif_leq(X, Y, R):
        (dint(X), int(Y), dint(R)) =>
        (dint(X), int(Y), dint(R))
       +(cost(lb,constraints,1),
         cost(ub,constraints,1)).

:- trust pred reif_leq(X, Y, R):
        (int(X), dint(Y), dint(R)) =>
        (int(X), dint(Y), dint(R))
       +(cost(lb,constraints,1),
         cost(ub,constraints,1)).

:- trust pred reif_leq(X, Y, R):
        (dint(X), dint(Y), dint(R)) =>
        (dint(X), dint(Y), dint(R))
       +(cost(lb,constraints,1),
         cost(ub,constraints,1)).

:- trust pred reif_leq(X, Y, R):
        (int(X), int(Y), dint(R)) =>
        (int(X), int(Y), dint(R)).

:- trust comp reif_leq/3 + (is_det).
:- trust comp reif_leq_dint_dint_var/3 + (is_det, not_fails).

% leq(X, Y) (X =< Y)

:- trust pred leq(X, Y):
        (dint(X), dint(Y)) =>
        (dint(X), dint(Y))
       +(cost(lb,constraints,1),
         cost(ub,constraints,1)).

:- trust pred leq_dint_int(X, Y):
        (dint(X), int(Y)) =>
        (dint(X), int(Y))
       +(cost(lb,constraints,0),
         cost(lb,numvars,0),
	 cost(ub,constraints,0), % resolves immediately (change upperbound)
         cost(ub,numvars,0)).

:- trust pred leq(X, Y):
        (dint(X), int(Y)) =>
        (dint(X), int(Y))
       +(cost(lb,constraints,0),
         cost(lb,numvars,0),
	 cost(ub,constraints,0), % resolves immediately (change upperbound)
         cost(ub,numvars,0)).

:- trust pred leq(X, Y):
        (int(X), dint(Y)) =>
        (int(X), dint(Y))
       +(cost(lb,constraints,0),
         cost(lb,numvars,0),
         cost(ub,constraints,0), % same
         cost(ub,numvars,0)).

:- trust pred leq_int_dint(X, Y):
        (int(X), dint(Y)) =>
        (int(X), dint(Y))
       +(cost(lb,constraints,0),
         cost(lb,numvars,0),
         cost(ub,constraints,0), % same
         cost(ub,numvars,0)).

:- trust comp leq/2          + (is_det, not_fails).
:- trust comp leq_dint_int/2 + (is_det, not_fails).
:- trust comp leq_int_dint/2 + (is_det, not_fails).

% less(X, Y) (X < Y)

:- trust pred less(X, Y):
        (dint(X), dint(Y)) =>
        (dint(X), dint(Y))
       +(cost(lb,constraints,1),
         cost(ub,constraints,1)).

:- trust pred less(X, Y):
        (dint(X), int(Y)) =>
        (dint(X), int(Y)).

:- trust pred less(X, Y):
        (int(X), dint(Y)) =>
        (int(X), dint(Y)).

:- trust comp less/2 + (is_det, not_fails).

% plus(X,Y,Z) (X + Y = Z)

:- trust pred plus_dint_int_var(X,Y,Z):
        (dint(X), int(Y), var(Z)) =>
        (dint(X), int(Y), dint(Z))
       +(cost(lb,constraints,1),
         cost(lb,numvars,1),
         cost(ub,constraints,1),
         cost(ub,numvars,1)).

:- trust pred plus_int_dint_var(X,Y,Z):
        (int(X), dint(Y), var(Z)) =>
        (int(X), dint(Y), dint(Z))
       +(cost(lb,constraints,1),
         cost(lb,numvars,1),
         cost(ub,constraints,1),
         cost(ub,numvars,1)).

:- trust pred plus(X,Y,Z):
        (dint(X), int(Y), var(Z)) =>
        (dint(X), int(Y), dint(Z))
       +(cost(lb,constraints,1),
         cost(lb,numvars,1),
         cost(ub,constraints,1),
         cost(ub,numvars,1)).

:- trust pred plus(X,Y,Z):
        (int(X), dint(Y), var(Z)) =>
        (int(X), dint(Y), dint(Z))
       +(cost(lb,constraints,1),
         cost(lb,numvars,1),
         cost(ub,constraints,1),
         cost(ub,numvars,1)).

:- trust pred plus(X,Y,Z):
        (dint(X), dint(Y), var(Z)) =>
        (dint(X), dint(Y), dint(Z))
       +(cost(lb,constraints,1),
         cost(ub,constraints,1)).

:- trust pred plus_dint_dint_var(X,Y,Z):
        (dint(X), dint(Y), var(Z)) =>
        (dint(X), dint(Y), dint(Z))
       +(cost(lb,constraints,1),
         cost(ub,constraints,1)).

:- trust pred plus(X,Y,Z):
        (int(X), int(Y), var(Z)) =>
        (int(X), int(Y), int(Z)).

:- trust pred plus_int(X,Y,Z):
        (int(X), int(Y), var(Z)) =>
        (int(X), int(Y), int(Z),
	 size(Z, int(X)+int(Y)))
        + (size_metric(X, int),
	   size_metric(Y, int),
	   size_metric(Z, int),
	   is_det, not_fails, relations(inf)).


:- trust pred plus_dint_dint_dint(X,Y,Z):
        (dint(X), dint(Y), dint(Z)) => 
        (dint(X), dint(Y), dint(Z)) 
       +(cost(lb,constraints,1),
         cost(ub,constraints,1)).
 
:- trust pred plus(X,Y,Z):
        (dint(X), dint(Y), dint(Z)) =>
        (dint(X), dint(Y), dint(Z))
       +(cost(lb,constraints,1),
         cost(ub,constraints,1)).

:- trust pred plus(X,Y,Z):
        (int(X), dint(Y), dint(Z)) =>
        (int(X), dint(Y), dint(Z))
       +(cost(lb,constraints,1),
         cost(ub,constraints,1)).

:- trust pred plus(X,Y,Z):
        (dint(X), dint(Y), dint(Z)) =>
        (dint(X), dint(Y), dint(Z))
       +(cost(lb,constraints,1),
         cost(ub,constraints,1)).

:- trust pred plus(X,Y,Z):
        (int(X), int(Y), dint(Z)) =>
        (int(X), int(Y), dint(Z)).

:- trust comp plus/3                + (is_det, not_fails, relations(inf)).
:- trust comp plus_dint_int_var/3   + (is_det, not_fails, relations(inf)).
:- trust comp plus_int_dint_var/3   + (is_det, not_fails, relations(inf)).
:- trust comp plus_dint_dint_var/3  + (is_det, not_fails, relations(inf)).
:- trust comp plus_dint_dint_dint/3 + (is_det, not_fails, relations(inf)).

% minus(X,Y,Z) (X - Y = Z)

:- trust pred minus_dint_int_var(X,Y,Z):
        (dint(X), int(Y), var(Z)) =>
        (dint(X), int(Y), dint(Z))
       +(cost(lb,constraints,1),
         cost(lb,numvars,1),
         cost(ub,constraints,1),
         cost(ub,numvars,1)).

:- trust pred minus(X,Y,Z):
        (dint(X), int(Y), var(Z)) =>
        (dint(X), int(Y), dint(Z))
       +(cost(lb,constraints,1),
         cost(lb,numvars,1),
         cost(ub,constraints,1),
         cost(ub,numvars,1)).

:- trust pred minus(X,Y,Z):
        (int(X), dint(Y), var(Z)) =>
        (int(X), dint(Y), dint(Z))
       +(cost(lb,constraints,1),
         cost(lb,numvars,1),
         cost(ub,constraints,1),
         cost(ub,numvars,1)).

:- trust pred minus(X,Y,Z):
        (dint(X), dint(Y), var(Z)) =>
        (dint(X), dint(Y), dint(Z))
       +(cost(lb,constraints,1),
         cost(lb,numvars,1),
         cost(ub,constraints,1),
         cost(ub,numvars,1)).

:- trust pred minus_dint_dint_var(X,Y,Z):
        (dint(X), dint(Y), var(Z)) =>
        (dint(X), dint(Y), dint(Z))
       +(cost(lb,constraints,1),
         cost(lb,numvars,1),
         cost(ub,constraints,1),
         cost(ub,numvars,1)).

:- trust pred minus(X,Y,Z):
        (int(X), int(Y), var(Z)) =>
        (int(X), int(Y), int(Z)).

:- trust pred minus(X,Y,Z):
        (dint(X), int(Y), dint(Z)) =>
        (dint(X), int(Y), dint(Z))
       +(cost(lb,constraints,1),
         cost(ub,constraints,1)).

:- trust pred minus(X,Y,Z):
        (int(X), dint(Y), dint(Z)) =>
        (int(X), dint(Y), dint(Z))
       +(cost(lb,constraints,1),
         cost(ub,constraints,1)).

:- trust pred minus(X,Y,Z):
        (dint(X), dint(Y), dint(Z)) =>
        (dint(X), dint(Y), dint(Z))
       +(cost(lb,constraints,1),
         cost(ub,constraints,1)).

:- trust pred minus(X,Y,Z):
        (int(X), int(Y), dint(Z)) =>
        (int(X), int(Y), dint(Z)).

:- trust comp minus/3               + (is_det, not_fails, relations(inf)).
:- trust comp minus_dint_int_var/3  + (is_det, not_fails, relations(inf)).
:- trust comp minus_dint_dint_var/3 + (is_det, not_fails, relations(inf)).

% mult(X,Y,Z) (X * Y = Z)

:- trust pred mult(X,Y,Z):
        (dint(X), int(Y), var(Z)) =>
        (dint(X), int(Y), dint(Z))
       +(cost(lb,constraints,1),
         cost(lb,numvars,1),
         cost(ub,constraints,1),
         cost(ub,numvars,1)).

:- trust pred mult_int_dint_var(X,Y,Z):
        (int(X), dint(Y), var(Z)) => 
        (int(X), dint(Y), dint(Z)) 
       +(cost(lb,constraints,1),
         cost(lb,numvars,1),
	 cost(ub,constraints,1),
         cost(ub,numvars,1)).

:- trust pred mult(X,Y,Z):
        (int(X), dint(Y), var(Z)) =>
        (int(X), dint(Y), dint(Z))
       +(cost(lb,constraints,1),
         cost(lb,numvars,1),
         cost(ub,constraints,1),
         cost(ub,numvars,1)).

:- trust pred mult(X,Y,Z):
        (dint(X), dint(Y), var(Z)) =>
        (dint(X), dint(Y), dint(Z))
       +(cost(lb,constraints,1),
         cost(lb,numvars,1),
         cost(ub,constraints,1),
         cost(ub,numvars,1)).

:- trust pred mult(X,Y,Z):
        (int(X), int(Y), var(Z)) =>
        (int(X), int(Y), int(Z)).

:- trust pred mult_int(X,Y,Z):
        (int(X), int(Y), var(Z)) =>
        (int(X), int(Y), int(Z),
	 size(Z, int(X)*int(Y)))
        + (size_metric(X, int),
	   size_metric(Y, int),
	   size_metric(Z, int),
	   is_det, not_fails, relations(inf)).

:- trust pred mult(X,Y,Z):
        (dint(X), int(Y), dint(Z)) =>
        (dint(X), int(Y), dint(Z))
       +(cost(lb,constraints,1),
         cost(ub,constraints,1)).

:- trust pred mult(X,Y,Z):
        (int(X), dint(Y), dint(Z)) =>
        (int(X), dint(Y), dint(Z))
       +(cost(lb,constraints,1),
         cost(ub,constraints,1)).

:- trust pred mult(X,Y,Z):
        (dint(X), dint(Y), dint(Z)) =>
        (dint(X), dint(Y), dint(Z))
       +(cost(lb,constraints,1),
         cost(ub,constraints,1)).

:- trust pred mult(X,Y,Z):
        (int(X), int(Y), dint(Z)) =>
        (int(X), int(Y), int(Z)).

:- trust comp mult/3 + (is_det, not_fails).
:- trust comp mult_int_dint_var(A, B, C)
       +(is_det, not_fails,
         size_metric(A, int),
         size_metric(B, size),
         size_metric(C, size)).
% in(E,DL,DU) (E in DL..DU)

:- trust pred in(E,DL,DU):
        (var(E), int(DL), int(DU)) =>
        (dint(E), int(DL), int(DU))
       +(cost(lb,numvars,1),
         cost(ub,numvars,1)).

:- trust pred in(E,DL,DU):
        (dint(E), int(DL), int(DU)) =>
        (dint(E), int(DL), int(DU)).

:- trust comp in/3 + (is_det, not_fails).

% ARRAY MANIPULATION

:- trust pred element_array(I,A,A_i):
        (int(I), array(A), var(A_i)) =>
        (int(I), array(A), array(A_i)).

:- trust comp element_array/3 + (is_det, not_fails).

:- trust pred element(I,A,A_i):
        (int(I), array(A), var(A_i)) =>
        (int(I), array(A), dint(A_i)).

:- trust comp element/3 + (is_det, not_fails).

:- trust pred element_dint(I,A,A_i):
        (int(I), array(A), var(A_i)) =>
        (int(I), array(A), dint(A_i)).

:- trust comp element_dint/3 + (is_det, not_fails).

:- trust pred element_int(I,A,A_ij):
        (int(I), array(A), var(A_ij)) =>
        (int(I), array(A), int(A_ij))
        + (size_metric(A_ij, int)).

:- trust comp element_int/3 + (is_det, not_fails).

:- trust pred element2d_int(I, J, A, A_ij):
        (int(I), int(J), array(A), var(A_ij)) =>
        (int(I), int(J), array(A), int(A_ij))
        + size_metric(A_ij, size).

:- trust comp element2d_int/4 + (is_det, not_fails).

:- trust pred element2d_dint(I, J, A, A_ij):
        (int(I), int(J), array(A), var(A_ij)) =>
        (int(I), int(J), array(A), dint(A_ij))
        + size_metric(A_ij, size).

:- trust comp element2d_dint/4 + (is_det, not_fails).

:- trust pred build_array(X, Z):
        (var(X), int(Z)) =>
        (array(X), int(Z)).

:- trust comp build_array(X, Z) + (is_det, not_fails).

:- trust pred create_var(X): var(X) => dint(X).
:- trust comp create_var/1 + (is_det, not_fails).

% GLOBAL CONSTRAINTS

% all_different_(A, ASize)

:- trust pred all_different_(A, ASize):
        (array(A), int(ASize)) =>
        (array(A), int(ASize))
       +(% cost(ub,constraints, (int(ASize) * (int(ASize) + 1) / 2)),
         % cost(ub,constraints, 2 * int(ASize)),
	 cost(lb,alldiff,1),
         cost(ub,alldiff,1)).

:- trust comp all_different_/2 + (is_det, not_fails).

% is/2 kludge, to avoid bug: analyzer infer num instead of int if we
% use is from arithmetic:

:- trust pred is(-A, +B) : var * intexpression
	=> (int(A), intexpression(B), size(A, int(B))) + (not_fails, eval).
:- trust pred is(+A, +B) : int * intexpression + (test_type(arithmetic)).
:- trust comp is(A, B) + (size_metric(A, int), size_metric(B, int)).
:- true comp is/2 + (sideff(free), bind_ins, is_det, relations(inf)).

:- redefining(is/2).
