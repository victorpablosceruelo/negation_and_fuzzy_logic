:- module(_, [], ['$purest']).
:- include(.(include(common))).
:- include(.(include(c_stdio))).
:- include(.(include(c_stdlib))).
:- include(.(include(c_gmp))).

:- '$native_weak_inline'(include('engine/pidigits.native.h')).

:- '$improlog_begin'.
:- lowtype(ctx).
:- class ctx {
  :- struct.
  % Transformation matrix components.
  :- attr q :: ref0(mpz).
  :- attr r :: ref0(mpz).
  :- attr s :: ref0(mpz).
  :- attr t :: ref0(mpz).
  % Temporary numbers.
  :- attr u :: ref0(mpz).
  :- attr v :: ref0(mpz).
  :- attr w :: ref0(mpz).
  % Counters.
  :- mut d :: intmach.
  :- mut i :: intmach.
  :- mut n :: intmach.
% Accumulated digits for one line.
  :- attr digits :: array(ref0(mut(char)), 11).
}.

% Compose matrix with numbers on the right.
:- pred compose_r/5 + lowentry(det, [ref1(ctx), intmach, intmach, intmach, intmach], 'compose_r') + prop(foreign__static).
compose_r(C, BQ, BR, BS, BT) :-
	mpz_mul_si(C.u, C.r, BS),
	mpz_mul_si(C.r, C.r, BQ),
	mpz_mul_si(C.v, C.t, BR),
	mpz_add(C.r, C.r, C.v),
	mpz_mul_si(C.t, C.t, BT),
	mpz_add(C.t, C.t, C.u),
	mpz_mul_si(C.s, C.s, BT),
	mpz_mul_si(C.u, C.q, BS),
	mpz_add(C.s, C.s, C.u),
	mpz_mul_si(C.q, C.q, BQ).

% Compose matrix with numbers on the left.
:- pred compose_l/5 + lowentry(det, [ref1(ctx), intmach, intmach, intmach, intmach], 'compose_l') + prop(foreign__static).
compose_l(C, BQ, BR, BS, BT) :-
	mpz_mul_si(C.r, C.r, BT),
	mpz_mul_si(C.u, C.q, BR),
	mpz_add(C.r, C.r, C.u),
	mpz_mul_si(C.u, C.t, BS),
	mpz_mul_si(C.t, C.t, BT),
	mpz_mul_si(C.v, C.s, BR),
	mpz_add(C.t, C.t, C.v),
	mpz_mul_si(C.s, C.s, BQ),
	mpz_add(C.s, C.s, C.u),
	mpz_mul_si(C.q, C.q, BQ).

% Extract one digit.
:- pred extract/3 + lowentryfun([ref1(ctx), uintmach], intmach, 'extract') + prop(foreign__static).
extract(C, J, R) :-
	mpz_mul_ui(C.u, C.q, J),
	mpz_add(C.u, C.u, C.r),
	mpz_mul_ui(C.v, C.s, J),
	mpz_add(C.v, C.v, C.t),
	mpz_tdiv_q(C.w, C.u, C.v),
	R = ~'$trust_typed'(~mpz_get_ui(C.w), intmach).

% Print one digit. Returns 1 for the last digit.
:- pred prdigit/2 + lowentry(semidet, [ref1(ctx), intmach], 'prdigit') + prop(foreign__static).
prdigit(C, Y) :-
	% TODO: this predicate is semidet, but impure... since it uses IO
	C.digits[@C.d] <- ~'$trust_typed'(0'0 + Y, char),
	C.d <- @C.d + 1,
	C.i <- @C.i + 1,
	% TODO: allow nondet code here (since alternatives are cut at compile time)
	( ( @C.i mod 10 == 0 -> true
	  ; @C.i == @C.n )
	->
	    C.digits[@C.d] <- ~'$trust_typed'(0, char), % '\0'
	    printf3("%-10s\t:%d\n", C.digits, @C.i),
            C.d <- 0
	; true
	),
	@C.i == @C.n.

% Generate successive digits of PI.
:- pred pidigits/1 + lowentry(det, [ref1(ctx)], 'pidigits') + prop(foreign__static).
pidigits(C) :-
	K = ~initmut(intmach, 1),
	C.d <- 0,
	C.i <- 0,
	mpz_init_set_ui(C.q, ~'$trust_typed'(1, uintmach)),
	mpz_init_set_ui(C.r, ~'$trust_typed'(0, uintmach)),
	mpz_init_set_ui(C.s, ~'$trust_typed'(0, uintmach)),
	mpz_init_set_ui(C.t, ~'$trust_typed'(1, uintmach)),
	mpz_init(C.u),
	mpz_init(C.v),
	mpz_init(C.w),
	pidigits__2(C, K).

:- pred pidigits__2/2 + prop(subpr).
pidigits__2(C, K) :-
	Y = ~extract(C, ~'$trust_typed'(3, uintmach)),
	( Y == ~extract(C, ~'$trust_typed'(4, uintmach)) ->
	    ( prdigit(C, Y) ->
	        true % (finish loop)
	    ; compose_r(C, 10, -10 * Y, 0, 1),
	      pidigits__2(C, K)
	    )
	; compose_l(C, @K, 4 * @K + 2, 0, 2 * @K + 1),
	  K <- @K + 1,
	  pidigits__2(C, K)
	).

:- pred begin/1 + lowentry(det, [intmach], 'begin') + prop(foreign__static).
begin(Arg) :-
	C = ~ref0(ctx),
	C.n <- Arg,
	pidigits(~'$to_ref1'(C)).

:- '$improlog_end'.
