:- module(_, [], ['$purest']).
:- include(.(include(common))).
:- include(.(include(c_stdio))).
:- include(.(include(c_stdlib))).
:- include(.(include(c_string))).

:- '$native_weak_inline'(include('engine/nsievebits.pl-2.native.h')).

:- '$improlog_begin'.

:- lowtype(bits).
:- type(bits/1) + equiv.
bits(T) :- T = ~uint32.
:- tagtestdefcustom('BBITS', '(sizeof(bits_t) * 8)').
:- pred 'BSIZE'/2 + foreignfun([intmach], intmach, 'BSIZE').
:- tagtestdefcustom('BSIZE(x)', '(((x) / 8) + sizeof(bits_t))').
:- pred 'BMASK'/2 + foreignfun([uint32], uint32, 'BMASK').
:- tagtestdefcustom('BMASK(x)', '(1 << ((x) % BBITS))').
:- pred 'BTEST'/3 + foreignfun([mut(bits), intmach], uint32, 'BTEST'). % TODO: wrong input type
:- tagtestdefcustom('BTEST(p, x)', '((p)[(x) / BBITS] & BMASK(x))').
:- pred 'BFLIP'/2 + foreign([mut(bits), intmach], det, 'BFLIP').
:- tagtestdefcustom('BFLIP(p, x)', '(p)[(x) / BBITS] ^= BMASK(x)').

:- pred begin/1 + lowentry(det, [intmach], 'begin').
begin(Arg) :-
	SZ = ~'$typed'(intmach, 10000) << Arg,
	% TODO: type for arrays here is wrong...
	Primes = ~'$trust_typed'(~'$alloc'(malloc, array(ref0(mut(uint8)), ~'BSIZE'(SZ))), mut(bits)), % bits *Primes
	% TODO: exit if malloc failed (returned a NULL)
        '$for_each'(M, ~intrangeclosed(~'$typed'(intmach, 0), ~'$typed'(intmach, 2)), (
	  Count = ~initmut(intmach, ~'$typed'(intmach, 0)),
	  N = SZ >> @M,
	  memset(~'$trust_typed'(Primes, mut(char)), ~'$trust_typed'(0xff, uint8), ~'BSIZE'(N)),
	  '$for_each'(I, ~intrangeclosed(~'$typed'(intmach, 2), N), (
	    ( ~'BTEST'(Primes, @I) \== ~'$typed'(uint32, 0) ->
	        Count <- @Count + 1,
		'$for_each'(J, ~intrangeclosedstep(@I + @I, N, @I), (
	          ( ~'BTEST'(Primes, @J) \== ~'$typed'(uint32, 0) -> 'BFLIP'(Primes, @J) ; true )
		))
	    ; true
	    )
	  )),
	  printf3("Primes up to %8d %8d\n", N, @Count)
	)),
	'$dealloc'(malloc, Primes).

:- '$improlog_end'.

