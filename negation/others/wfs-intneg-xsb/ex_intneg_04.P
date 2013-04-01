
:- module('ex_intneg_04', _).

% Member function
mbr2(X, [ X|_L]).
mbr2(X, [_Y| L]) :- mbr2(X, L).

%-------------------------------

test_mbr_01(X) :- mbr2(X, []).
test_mbr_02(X) :- mbr2(X, [1]).
test_mbr_03(X) :- mbr2(X, [X]).

test_mbr_04(X) :- mbr2(X, [1,2]).
test_mbr_05(X) :- mbr2(X, [1,Y]).
test_mbr_06(X) :- mbr2(X, [1,X]).

test_mbr_07(X) :- mbr2(X, [1,2,3]).
test_mbr_08(X) :- mbr2(X, [1,Y,3]).

test_mbr_09(Y) :- mbr2(1, [1,Y,3]).
test_mbr_10(Y) :- mbr2(1, [Y,Z,T]).

test_mbr_11(X) :- mbr2(X, [Y,Z,T]).
test_mbr_12(X) :- mbr2(X, [Y,Z,T]).

%------------------------------

f1(X) :- mbr2(X, [1,2,3]).
f1(X) :- intneg(mbr2(X, [1,2,3])).

f2(X) :- mbr2(X, [1,2,3]).
f2(X) :- intneg(mbr2(X, [4,5,6])).

f3(X).

g1(X, Y) :- f1(X), f1(Y).
g2(X, Y) :- f2(X), f2(Y).

%------------------------------

tests_mbr_ok :-
	intneg(test_mbr_01(X01)), 
	test_mbr_02(X02),
	test_mbr_03(X03),
	test_mbr_04(X04),
	test_mbr_05(X05),
	test_mbr_06(X06),
	test_mbr_07(X07),
	test_mbr_08(X08),
	test_mbr_09(X09),
	test_mbr_10(X10),
	test_mbr_11(X11),
	test_mbr_12(X12).
	
%------------------------------

test_use_forall_when_negated_1_ok :- intneg(f1(X)).
test_use_forall_when_negated_2_ok :- intneg(g1(X, Y)).
test_use_forall_when_negated_3_ok :- intneg(f3(X)).
test_use_forall_when_negated_4_fail :- intneg(f2(X)).
test_use_forall_when_negated_5_fail :- intneg(g2(X, Y)).

test_forall_ok_1 :- intneg_forall([X], f1(X)).
test_forall_ok_2 :- intneg_forall([X, Y], g1(X, Y)).
test_forall_ok_3 :- intneg_forall([X], f3(X)).

test_forall_fail_1 :- intneg_forall([X], f2(X)).
test_forall_fail_2 :- intneg_forall([X, Y], g2(X, Y)).

%------------------------------


