:- module(test2, [a/1,b/1,c/1,test/0,testany/0], [hiord, assertions, hmtypes_check]).

:- use_module(library(aggregates)).

:- runtime_hm_type_check(on).

:- prop intlist/1 + hmtype.
intlist(X) :- list(X, integer).

% OK
:- pred a/1 :: intlist + hmtyped.
a([1,2,3]).

% Type checker says 'ok', although the result is more general than a
% list of integers. Is that right?
% --jfmc
:- pred b/1 :: list(integer) + hmtyped.
b([1,_,3]).

% Type checker finds error.
:- pred c/1 :: list(integer) + hmtyped.
% wrong version:
%c([1,X,3]) :- ( X = true ; X = 1 ).
% good version:
c([1,X,3]) :- ( X = 2 ; X = 1 ).

%:- trust_hm_pred atom_codes/1 :: atm * list(integer) + hmtyped.
:- trust_hm_pred my_atom_codes(atm, list(integer)).
my_atom_codes(A, B) :- atom_codes(A, B).

:- pred test0 + hmtyped.
%test0 :- 3 = a.
test0 :- 3 = 3.
test0 :- 3 == 3.
test0 :- 3 \== 3.
test0 :- 3 @< 3.
test0 :- 3 @> 3.
test0 :- 3 @=< 3.
test0 :- 3 @>= 3.
test0 :- copy_term(true, true).
test0 :- compare(<,3,5).

:- pred test + hmtyped.
test :-
	findall(X, (X=0'a;X=0'b;X=0'c), Xs), % OK
%	findall(X, (X=0'a;X=foo;X=0'c), Xs), % wrong
	my_atom_codes(abc, Xs), % OK
%	my_atom_codes(Xs, _), % wrong
%	3 = a, % wrong
	my_atom_codes(hello, "hello"), % OK
%	my_atom_codes("hello", hello), % wrong
	true.

:- pred testany/0 + hmtyped.
testany :-
	type_to_any(3, X),
	any_to_type(X, Y, atm), % wrong at runtime
%	any_to_type(Y, Z, atm), % wrong at compile time
	display(y(Y)), nl.

:- pred testcls/0 + hmtyped.
testcls :-
	A = 1,
	A > 3,
%	4.3 > A, % wrong at compile time
	B = 6.6,
%	7 > B, % wrong at compile time
	B > 1.1.

:- pred testarith/0 + hmtyped.
testarith :-
	B is integer(3.4),
	A is B /\ 1,
%	_ is A \/ 1.3, % compile time error
	_ is A \/ 1.
