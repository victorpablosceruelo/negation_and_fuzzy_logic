:- module(_, [], ['$purest']).
:- include(.(include(common))).
:- include(.(include(c_stdio))).
:- include(.(include(c_stdlib))).
:- include(.(include(c_string))).

% TODO: do something to avoid absolute paths here
%:- '$native_weak_inline'(include('__root__.home.jfran.svn.ciaode.ciao.optim_comp.testsuite.tests.shootout_nsieve_ip.native.h')).
:- '$native_weak_inline'(include('engine/nsieve.native.h')).

:- '$improlog_begin'.

:- lowtype(boolean).
:- type(boolean/1) + enum.
boolean(T) :- T = ~'$base'(uint8), T = (false|true).
:- enum_encode(boolean/1, [
	(false, 0),
	(true, 1)]).

:- pred begin/1 + lowentry(det, [intmach], 'begin').
begin(M) :-
	'$for_each'(I, ~intrange(3), nsieve(~'$typed'(intmach, 10000) << (M - @I))).

% TODO: use uintmach! (check the C implementation)
:- pred nsieve/1 + lowentry(det, [intmach], 'nsieve') + prop(foreign__static).
nsieve(M) :-
	Count = ~initmut(intmach, ~'$typed'(intmach, 0)),
	% TODO: type for arrays here is wrong...
	Flags = ~'$alloc'(malloc, array(ref0(mut(boolean)), M)), % boolean *flags
	% TODO: define an array_set builtin (or array_do(Range, X, Op)...)
	% TODO: this only works if the array elements are 1-byte sized
	memset(~'$trust_typed'(Flags, mut(char)), ~'$trust_typed'(~'$typed'(boolean, true), uint8), M),
	'$for_each'(I, ~intrange2(~'$typed'(intmach, 2), M),
	  ( @Flags[@I] = true ->
	      Count <- @Count + 1,
	      '$for_each'(J, ~intrange2step(@I << 1, M, @I), Flags[@J] <- false)
	  ; true
	  )),
	'$dealloc'(malloc, Flags),
	printf3("Primes up to %8u %8u\n", M, @Count).
:- '$improlog_end'.
