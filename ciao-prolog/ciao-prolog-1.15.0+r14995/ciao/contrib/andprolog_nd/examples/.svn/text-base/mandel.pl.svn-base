:- module(mandel,
	[
	    seq/2,
	    par_nondet/2,
	    data/1
	],
	[andprolog_nd]).

:- use_module(library(lists)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

data(0).

seq(0,true) :- demo_seq.
par_nondet(0,true) :- demo_par.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

data_2( 1, data_7(-1100,10,20,-300,25,15,5) ).
data_2( 2, data_7(-1100,10,30,-300,25,30,5) ).
data_2( 3, data_7(-500,10,20,-200,25,15,5) ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

demo_seq :- demo_seq_(1).

demo_seq_(X):- data_2( X, Data ), go_demo_seq( Data ).

go_demo_seq( data_7(X, XI, XR, Y, YI, YR, Iterations) ) :-
	generate_seq( xrange(X,XI,XR), yrange(Y,YI,YR), Iterations, _Res ),
	fail.
go_demo_seq(_).

demo_par :- demo_par_(1).

demo_par_(X):- data_2( X, Data ), go_demo_par( Data ).

go_demo_par( data_7(X, XI, XR, Y, YI, YR, Iterations) ) :-
	generate_par( xrange(X,XI,XR), yrange(Y,YI,YR), Iterations, _Res ),
	fail.
go_demo_par(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_seq( xrange(XS,XI,XR), yrange(YS,YI,YR), Iterations,
          s(XPixelAdd,YPixelAdd,PChar) ) :-
        generate_val( XS, XI, XR, X, XPixelAdd ),
        generate_val( YS, YI, YR, Y, YPixelAdd ),
        fp_mult( X, X, XSQ ),
        fp_mult( Y, Y, YSQ ),
        ModSq is XSQ + YSQ,
        test_membership( c(X,Y), z0(X,Y), ModSq, Iterations, PChar ).

generate_par( xrange(XS,XI,XR), yrange(YS,YI,YR), Iterations,
          s(XPixelAdd,YPixelAdd,PChar) ) :-
        (
	    generate_val( XS, XI, XR, X, XPixelAdd ),
	    fp_mult( X, X, XSQ )
	)  &
        (
	    generate_val( YS, YI, YR, Y, YPixelAdd ),
	    fp_mult( Y, Y, YSQ )
	),
        ModSq is XSQ + YSQ,
	test_membership( c(X,Y), z0(X,Y), ModSq, Iterations, PChar ).

generate_val( Start, _Inc, Count, Start, Count ) :-
        Count > 0.
generate_val( Start, Inc, Count, Val, Address ) :-
        Count > 0,
        NStart is Start + Inc,
        NCount is Count - 1,
        generate_val( NStart, Inc, NCount, Val, Address ).

/* This predicate tests whether a point is inside the mandelbrot */
/* set or not.  The remainder returned is always a character     */
/* code to be used for display.                                  */

test_membership( c(R,I), z0(R0,I0), ModSq, Count, Char ) :-
	ModSq =< 4000,
	Count > 0  ,
        newr( NewR, R, I, R0 ),
        newi( NewI, R, I, I0 ),
        newmodsq( NewModSq, NewI, NewR ),
        NCount is Count - 1,
	test_membership( c(NewR,NewI), z0(R0,I0), NewModSq, NCount, Char ).
test_membership( _, _, ModSq, 0, h('.', ModSq) ) :-
	ModSq =< 4000.
test_membership( _, _, ModSq, Count, h('X', ModSq) ) :-
	Count > 0,
	ModSq > 4000.

newr( NewR, R, I, R0 ) :-
        fp_mult( R, R, RSQ ),
        fp_mult( I, I, ISQ ),
        NewR is RSQ - ISQ + R0.

newi( NewI, R, I, I0 ) :-
        fp_mult( R, I, RI ),
        NewI is (2*RI) + I0.

newmodsq( NewModSq, NewI, NewR ) :-
        fp_mult( NewR, NewR, NewRSQ ),
        fp_mult( NewI, NewI, NewISQ ),
        NewModSq is NewRSQ + NewISQ.


/* This routine implements fixed point multiplication with */
/* 3 decimal places.                                       */

fp_mult( A, B, C ) :-
        WholeNo is B//1000,
        Remainder is B-(WholeNo*1000),
        Tenths is Remainder//100,
        fp_mult_part2( A, Remainder, Tenths, PartResult ),
        C is (A*WholeNo) + ((A*Tenths)//10) + PartResult.

fp_mult_part2( A, Remainder, Tenths, PartResult ) :-
        Remainder2 is Remainder-(Tenths*100),
        Hundredths is Remainder2//10,
        Thousandths is Remainder2-(Hundredths*10),
        PartResult is ((A*Hundredths)//100) + ((A*Thousandths)//1000).
