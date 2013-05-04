% Implementation of mutables variables with contexts
% Author: Jose F. Morales
% Sat Oct 25 14:58:58 CEST 2008

% NOTE: THIS TEST IS OBSOLETE!!! ctx_store has been removed

:- module(_, [main/0], [compiler(complang)]).

% ---------------------------------------------------------------------------
% Mutable variables based on contexts
%
% This context defines a mutable store.
% Two kind of contexts are avaiable:
%  - mutstore: full access to mutables
%  - mutstore_ro: read-only access to mutables
%
% * Operations on mutables:
%
%  (mutstore_ro context)
%  - function @A: returns the value of a mutable
%
%  (mutstore context)
%  - ~newmut(V): creates a new mutable variable with initial value V
%  - X <- V: assigns V to the mutable variable X
%
% To preserve the logic meaning, each user predicate must define the
% context it belongs to. The mutable store is conceptually carried on
% the extra arguments included by context definitions.
%
% * Implementation details:
%
%   The mutstore context defines a pair of hidden variables.  Those
%   variables a never visible from user code, but indirectly
%   manipulated through the '$ctx_store__*' family of predicates
%   (which are very time and memory efficient).
%
%   The resulting code is still logical! The mutstore could be
%   implemented, e.g. with lists, without any change in the semantics.
%
% * Related work:
%   - mutables in ImProlog (same semantics, but also contains type constraints)
%   - mutables in Sicstus? Mercury? functional languages?

:- '$begin_context'(mutstore).
:- '$usectx'(hipair(mutstore)).
:- '$end'.
:- '$begin_context'(mutstore_ro).
:- '$usectx'(hipair_ro(mutstore)).
:- '$end'.
:- '$def_binder'(empty_mutstore, push_hipair(mutstore)).

% TODO: untyped version
/*
:- op(50, fx, [(@)]).
:- fun_eval (@)/1.
:- '$context'((@)/2, mutstore_ro).
@X := ~'$ctx_store__value'(mutstore, X).

:- op(700, xfx, [(<-)]).
:- '$context'((<-)/2, mutstore).
X <- V :- '$ctx_store__assign'(mutstore, X, V).

:- '$context'(newmut/2, mutstore).
newmut(V) := ~'$ctx_store__init'(mutstore, V).
*/

:- op(50, fx, [(@)]).
:- fun_eval (@)/1.
:- '$context'((@)/2, mutstore_ro).
@X := V :-
	nonvar(X), X = '$v'(_Type, ValueId),
	V = ~'$ctx_store__value'(mutstore, ValueId).

:- op(700, xfx, [(<-)]).
:- '$context'((<-)/2, mutstore).
X <- V :-
	nonvar(X), X = '$v'(Type, ValueId),
	'$trust_metatype'(Type, pred(1)),
	check_type(Type, V),
	'$ctx_store__assign'(mutstore, ValueId, V).

:- '$context'(newmut/3, mutstore).
:- meta_predicate newmut(pred(1), ?, ?).
newmut(Type, V) := '$v'(Type, ValueId) :-
	check_type(Type, V),
	ValueId = ~'$ctx_store__init'(mutstore, V).

:- meta_predicate check_type(pred(1), ?).
check_type(Type, V) :-
	( Type(V) -> true ; throw(mutable_type_error(Type, V)) ).

% ---------------------------------------------------------------------------

:- include(.(test_code)).

/*
main :-
	empty_mutstore(test).

:- '$context'(test/0, mutstore).
test :-
	X = ~newmut(10),
	Y = ~newmut(10),
	( X == Y -> % TODO: BUG! X = Y does not work yet
	    display('wrong: X and Y are not the same mutvar'), nl
	; display('ok: X and Y are different mutvars'), nl
	),
	X <- 20,
	X1 is @X + 1,
	X <- X1,
	myread(X, Xv), display(should_be_the_same(Xv, 21)), nl,
	mywrongread(X).

:- '$context'(myread/2, mutstore_ro).
myread(X, V) :-
	V = @X.

:- '$context'(mywrongread/1, mutstore_ro).
% TODO: there is a bug: this predicate compiles!
mywrongread(X) :-
	X <- 3. % compilation of this predicate should fail!
*/