:- module(_, [], []).

:- export(main/0).
main :-
	test1,
	test2,
	test3,
	test4,
	test5,
	test6.

test1 :-
	% create a bignum from a float
	X is integer(40000000000000.0),
	% unify a fresh variable with a float
	A = 0.0,
	% unify a float with other float
	A = 0.0,
	% create a bignum and unify with a bignum created from a float
	Y = 40000000000000,
	X = Y,
	% create a float from a bignum
	Za is float(40000000000000000),
	% create a float
	Zb = 4.0e16,
	% unify two floats
	Za = Zb,
	% create a small int from a float and unify with a small int
	Sa is integer(1.0),
	Sb = 1,
	Sa = Sb,
	!.
test1 :-
	display('arith test1 failed!'), nl.

test2 :-
	X is 1,
	Y is X<<32,
	Z is Y>>32,
	Z = X,
	!.
test2 :-
	display('arith test2 failed!'), nl.

test3 :-
	X is 1,
	Y is X<<100,
	Z is Y>>100,
	Z = X,
	!.
test3 :-
	display('arith test3 failed!'), nl.

test4 :-
	X is 100000000000,
	Y is -X,
	Z is -Y,
	Z = X,
	!.
test4 :-
	display('arith test4 failed!'), nl.

test5 :-
	X is 10000,
	Y is X*30000000,
	Z is Y//30000000,
	Z = X,
	!.
test5 :-
	display('arith test5 failed!'), nl.

test6 :-
	X is 30000000,
	Y is X*10000,
	Z is Y//10000,
	Z = X,
	!.
test6 :-
	display('arith test6 failed!'), nl.
	
	
 
