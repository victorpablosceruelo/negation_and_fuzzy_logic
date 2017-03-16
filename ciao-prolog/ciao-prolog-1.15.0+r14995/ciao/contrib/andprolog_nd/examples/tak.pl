:- module(tak,
	[
	    seq/2,
	    par/2,
	    par_nondet/2,
	    data/1
	],
	[andprolog_nd]).

:- use_module(library(lists), [append/3, length/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

x(14).
y(10).
z(3).

data([X,Y,Z]) :- x(X), y(Y), z(Z).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

seq([X,Y,Z],true) :- tak_seq(X,Y,Z,R), var(R).
par([X,Y,Z],true) :- tak_par_gc(X,Y,Z,R), var(R).
par_nondet([X,Y,Z],true) :- tak_par_nondet_gc(X,Y,Z,R), var(R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tak_seq(X,Y,Z,_1) :-
        X1 is X-1,
        Y1 is Y-1,
        Z1 is Z-1,
        tak_seq_(Z1,X,Y,A3),
        tak_seq_(Y1,Z,X,A2),
        tak_seq_(X1,Y,Z,A1),
        tak_seq_(A1,A2,A3,_A),
        X2 is X-1,
        Y2 is Y-1,
        Z2 is Z-1,
        tak_seq_(Z2,X,Y,A6),
        tak_seq_(Y2,Z,X,A5),
        tak_seq_(X2,Y,Z,A4),
        tak_seq_(A4,A5,A6,_B),
        X3 is X-1,
        Y3 is Y-1,
        Z3 is Z-1,
        tak_seq_(Z3,X,Y,A9),
        tak_seq_(Y3,Z,X,A8),
        tak_seq_(X3,Y,Z,A7),
        tak_seq_(A7,A8,A9,_C),
        X4 is X-1,
        Y4 is Y-1,
        Z4 is Z-1,
        tak_seq_(Z4,X,Y,A12),
        tak_seq_(Y4,Z,X,A11),
        tak_seq_(X4,Y,Z,A10),
        tak_seq_(A10,A11,A12,_D),
	!.

tak_seq_(X,Y,Z,A) :-
        X=<Y,
        !,
        Z=A .
tak_seq_(X,Y,Z,A) :-
        X1 is X-1,
        Y1 is Y-1,
        Z1 is Z-1,
	tak_seq_(Z1,X,Y,A3),
	tak_seq_(Y1,Z,X,A2),
	tak_seq_(X1,Y,Z,A1),
	tak_seq_(A1,A2,A3,A).

tak_par_gc(X,Y,Z,_1) :-
        X1 is X-1,
        Y1 is Y-1,
        Z1 is Z-1,
        tak_par_gc_(Z1,X,Y,A3) '&!>' H4,
        tak_par_gc_(Y1,Z,X,A2) '&!>' H5,
        tak_par_gc_(X1,Y,Z,A1) '&!>' H6,
        X2 is X-1,
        Y2 is Y-1,
        Z2 is Z-1,
        X3 is X-1,
        Y3 is Y-1,
        Z3 is Z-1,
        X4 is X-1,
        Y4 is Y-1,
        Z4 is Z-1,
        tak_par_gc_(Z2,X,Y,A6) '&!>' H11,
        tak_par_gc_(Y2,Z,X,A5) '&!>' H12,
        tak_par_gc_(X2,Y,Z,A4) '&!>' H13,
        tak_par_gc_(Z3,X,Y,A9) '&!>' H18,
        tak_par_gc_(Y3,Z,X,A8) '&!>' H19,
        tak_par_gc_(X3,Y,Z,A7) '&!>' H20,
        tak_par_gc_(Z4,X,Y,A12) '&!>' H25,
        tak_par_gc_(Y4,Z,X,A11) '&!>' H26,
        tak_par_gc_(X4,Y,Z,A10) '&!>' H27,
        H4 '<&!' ,
        H5 '<&!' ,
        H6 '<&!' ,
        tak_par_gc_(A1,A2,A3,_A) '&!>' H7,
        H11 '<&!' ,
        H12 '<&!' ,
        H13 '<&!' ,
        tak_par_gc_(A4,A5,A6,_B) '&!>' H14,
        H18 '<&!' ,
        H19 '<&!' ,
        H20 '<&!' ,
        tak_par_gc_(A7,A8,A9,_C) '&!>' H21,
        H25 '<&!' ,
        H26 '<&!' ,
        H27 '<&!' ,
        tak_par_gc_(A10,A11,A12,_D),
        H7 '<&!' ,
        H14 '<&!' ,
        H21 '<&!' .

tak_par_gc_(X,Y,Z,A) :-
        X=<Y,
        !,
        Z=A .
tak_par_gc_(X,Y,Z,A) :-
        X1 is X-1,
        Y1 is Y-1,
        Z1 is Z-1,
	(
	    (Y1 > 8) ->
	     tak_par_gc_(Z1,X,Y,A3) '&!'
             tak_par_gc_(Y1,Z,X,A2) '&!'
             tak_par_gc_(X1,Y,Z,A1),
	     tak_par_gc_(A1,A2,A3,A)
	;
	     tak_par_gc_(Z1,X,Y,A3),
             tak_par_gc_(Y1,Z,X,A2),
             tak_par_gc_(X1,Y,Z,A1),
	     tak_par_gc_(A1,A2,A3,A)
	).

tak_par_nondet_gc(X,Y,Z,_1) :-
        X1 is X-1,
        Y1 is Y-1,
        Z1 is Z-1,
        tak_par_nondet_gc_(Z1,X,Y,A3) &> H4,
        tak_par_nondet_gc_(Y1,Z,X,A2) &> H5,
        tak_par_nondet_gc_(X1,Y,Z,A1) &> H6,
        X2 is X-1,
        Y2 is Y-1,
        Z2 is Z-1,
        X3 is X-1,
        Y3 is Y-1,
        Z3 is Z-1,
        X4 is X-1,
        Y4 is Y-1,
        Z4 is Z-1,
        tak_par_nondet_gc_(Z2,X,Y,A6) &> H11,
        tak_par_nondet_gc_(Y2,Z,X,A5) &> H12,
        tak_par_nondet_gc_(X2,Y,Z,A4) &> H13,
        tak_par_nondet_gc_(Z3,X,Y,A9) &> H18,
        tak_par_nondet_gc_(Y3,Z,X,A8) &> H19,
        tak_par_nondet_gc_(X3,Y,Z,A7) &> H20,
        tak_par_nondet_gc_(Z4,X,Y,A12) &> H25,
        tak_par_nondet_gc_(Y4,Z,X,A11) &> H26,
        tak_par_nondet_gc_(X4,Y,Z,A10) &> H27,
        H4 <& ,
        H5 <& ,
        H6 <& ,
        tak_par_nondet_gc_(A1,A2,A3,_A) &> H7,
        H11 <& ,
        H12 <& ,
        H13 <& ,
        tak_par_nondet_gc_(A4,A5,A6,_B) &> H14,
        H18 <& ,
        H19 <& ,
        H20 <& ,
        tak_par_nondet_gc_(A7,A8,A9,_C) &> H21,
        H25 <& ,
        H26 <& ,
        H27 <& ,
        tak_par_nondet_gc_(A10,A11,A12,_D),
        H7 <& ,
        H14 <& ,
        H21 <& .

tak_par_nondet_gc_(X,Y,Z,A) :-
        X=<Y,
        !,
        Z=A .
tak_par_nondet_gc_(X,Y,Z,A) :-
        X1 is X-1,
        Y1 is Y-1,
        Z1 is Z-1,
	(
	    (Y1 > 8) ->
	     tak_par_nondet_gc_(Z1,X,Y,A3) &
             tak_par_nondet_gc_(Y1,Z,X,A2) &
             tak_par_nondet_gc_(X1,Y,Z,A1),
	     tak_par_nondet_gc_(A1,A2,A3,A)
	;
	     tak_par_nondet_gc_(Z1,X,Y,A3),
             tak_par_nondet_gc_(Y1,Z,X,A2),
             tak_par_nondet_gc_(X1,Y,Z,A1),
	     tak_par_nondet_gc_(A1,A2,A3,A)
	).

