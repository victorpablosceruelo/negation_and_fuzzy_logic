:- module(_, [], ['$purest']).
:- include(.(include(common))).
:- include(.(include(c_stdio))).
:- include(.(include(c_stdlib))).
:- include(.(include(c_math))).
:- include(.(include(c_string))).

:- '$native_weak_inline'(include('engine/partialsums.native.h')).

:- '$improlog_begin'.

:- pred d/2 + foreignfun([intmach], flt64, d).
:- tagtestdefcustom('d(K)', '((flt64_t)(K))').

% mypow(Q,K) := Q ** K.
:- pred mypow/3 + prop(unfold).
mypow(Q, K, X) :-
	J = ~initmut(flt64, @K),
	mypow__2(Q, J, X).

:- pred mypow__2/3 + prop(subpr).
mypow__2(Q, J, X) :-
	( @J /\ 1 \== 0 ->
	    X <- @X * @Q
	; true
	),
	J <- @J >> 1,
	( @J == 0 ->
	    true % (finish)
	; Q <- @Q * @Q,
	  mypow__2(Q, J, X)
	).

:- pred begin/1 + lowentry(det, [intmach], 'begin').
begin(Arg) :-
	N = Arg,
	Sum = ~newmut(flt64),
	%
	Sum <- 0.0,
	'$for_each'(K, ~intrangeclosed(0, N), ( % pow(2.0/3.0, D(k)) inlined
	  X = ~initmut(flt64, 1.0),
	  Q = ~initmut(flt64, 2.0 / 3.0),
	  mypow(Q, K, X),
	  Sum <- @Sum + @X
        )),
	printf2("%.9f\t(2/3)^k\n", @Sum),
	%
        Sum <- 0.0,
	% TODO: use K when local scope work
	'$for_each'(K2, ~intrangeclosed(1, N), (Sum <- @Sum + (1.0 / ~sqrt(~d(@K2))))), /* aka pow(D(k2), -0.5) */
        printf2("%.9f\tk^-0.5\n", @Sum),
	%
	Sum <- 0.0,
	% TODO: use K when local scope work
	'$for_each'(K3, ~intrangeclosed(1, N), (Sum <- @Sum + (1.0/(~d(@K3)*(~d(@K3)+ 1.0))))),
	printf2("%.9f\t1/k(k+1)\n", @Sum),
	%
	Sum <- 0.0,
	% TODO: use K when local scope work
	'$for_each'(K4, ~intrangeclosed(1, N), (
          Sk = ~sin(~d(@K4)),
	  Sum <- @Sum + 1.0/(~d(@K4) * ~d(@K4) * ~d(@K4) * Sk * Sk)
	)),				       
	printf2("%.9f\tFlint Hills\n", @Sum),
	%
	Sum <- 0.0,
	% TODO: use K when local scope work
	'$for_each'(K5, ~intrangeclosed(1, N), (
          Ck = ~cos(~d(@K5)),
	  Sum <- @Sum + 1.0/((~d(@K5) * ~d(@K5)) * ~d(@K5) * Ck * Ck)
        )),
	printf2("%.9f\tCookson Hills\n", @Sum),
	%
	Sum <- 0.0,
	% TODO: use K when local scope work
	'$for_each'(K6, ~intrangeclosed(1, N), Sum <- @Sum + 1.0 / ~d(@K6)),
	printf2("%.9f\tHarmonic\n", @Sum),
	%
	Sum <- 0.0,
	% TODO: use K when local scope work
	'$for_each'(K7, ~intrangeclosed(1, N), Sum <- @Sum + 1.0 / (~d(@K7) * ~d(@K7))),
	printf2("%.9f\tRiemann Zeta\n", @Sum),
	%
	Sum <- 0.0,
	% TODO: use K when local scope work
	'$for_each'(K8, ~intrangeclosedstep(1, N - 1, 2), Sum <- @Sum + 1.0 / ~d(@K8)),
	% TODO: use K when local scope work
	'$for_each'(K9, ~intrangeclosedstep(2, N, 2), Sum <- @Sum - 1.0 / ~d(@K9)),
	printf2("%.9f\tAlternating Harmonic\n", @Sum),
	%
	Sum <- 0.0,
	% TODO: use K when local scope work
	'$for_each'(K10, ~intrangeclosedstep(1, 2 * N - 1, 4), Sum <- @Sum + 1.0 / ~d(@K10)),
	% TODO: use K when local scope work
	'$for_each'(K11, ~intrangeclosedstep(3, 2 * N, 4), Sum <- @Sum - 1.0 / ~d(@K11)),
	printf2("%.9f\tGregory\n", @Sum).
:- '$improlog_end'.
