:- module(_, [], ['$purest']).
:- include(.(include(common))).
:- include(.(include(c_stdio))).
:- include(.(include(c_stdlib))).

% TODO: without -O3 option, this benchmark was much slower...

:- '$native_weak_inline'(include('engine/recursive.pl-2.native.h')).

:- '$improlog_begin'.
:- pred ack/3 + lowentryfun([intmach,intmach], intmach, 'ack').
ack(X, Y, R) :-
	( X == 0 -> % TODO: use =/2
	    R = Y + 1
	; X1 = X - 1,
	  Y2 = ~newmut(intmach),
	  ( Y \== 0 -> % TODO: use =/2
	      Y1 = Y - 1,
	      ack(X, Y1, Y2b),
	      Y2 <- Y2b
	  ; Y2 <- 1
	  ),
	  ack(X1, @Y2, R)
	).

:- pred fib/2 + lowentryfun([intmach], intmach, 'fib').
fib(N, R) :-
	( N < 2 ->
	    R = 1
	; fib(N - 2, Fa),
	  fib(N - 1, Fb),
	  R = Fa + Fb
	).

:- pred fibFP/2 + lowentryfun([flt64], flt64, 'fibFP').
fibFP(N, R) :-
	( N < 2.0 ->
	    R = 1.0
	; fibFP(N - 2.0, Fa),
	  fibFP(N - 1.0, Fb),
	  R = Fa + Fb
	).

:- pred tak/4 + lowentryfun([intmach,intmach,intmach], intmach, 'tak').
tak(X, Y, Z, R) :-
	( Y < X ->
	    tak(X - 1, Y, Z, A),
	    tak(Y - 1, Z, X, B),
	    tak(Z - 1, X, Y, C),
	    tak(A, B, C, R)
	; R = Z
	).

:- pred takFP/4 + lowentryfun([flt64,flt64,flt64], flt64, 'takFP').
takFP(X, Y, Z, R) :-
	( Y < X ->
	    takFP(X - 1.0, Y, Z, A),
	    takFP(Y - 1.0, Z, X, B),
	    takFP(Z - 1.0, X, Y, C),
	    takFP(A, B, C, R)
	; R = Z
	).

:- pred begin/1 + lowentry(det, [intmach], 'begin').
begin(Arg) :-
	N = Arg - 1,
        %
	I1 = N + 1,
	ack(3, I1, R1),
	printf3("Ack(3,%d): %d\n", I1, R1),
        %
	I2 = 28.0 + ~'$trust_typed'(N, flt64),
	fibFP(I2, R2),
	printf3("Fib(%.1f): %.1f\n", I2, R2),
        %
	I3a = 3 * N,
	I3b = 2 * N,
	I3c = N,
	tak(I3a, I3b, I3c, R3),
	printf5("Tak(%d,%d,%d): %d\n", I3a, I3b, I3c, R3),
        %
	fib(3, R4),
        printf2("Fib(3): %d\n", R4),
        %
	takFP(3.0, 2.0, 1.0, R5),
	printf2("Tak(3.0,2.0,1.0): %.1f\n", R5).
:- '$improlog_end'.
