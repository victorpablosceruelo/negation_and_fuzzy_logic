% Note: the output is a PNM image

:- module(_, [], ['$purest']).
:- include(.(include(common))).
:- include(.(include(c_stdio))).
:- include(.(include(c_stdlib))).

:- '$native_weak_inline'(include('engine/mandelbrot.native.h')).
:- '$native_weak_inline'(define('_ISOC99_SOURCE', 1)). % TODO: this global option may also be read in included files!

:- '$improlog_begin'.

% TODO: missing foreign property __attribute__((pure, const, nothrow, sseregparm))
% TODO: int also contained the 'hot' C attribute, but it is not recognized by my GCC version... (i.e. __attribute__((pure, const, nothrow, sseregparm, hot)))
% TODO: missing __attribute__((pure, const, nothrow, sseregparm))
% TODO: place attribute at the beginning or at the end of the prototype head?
:- pred iterate/8 + lowentryfun([flt64, flt64, 
                                 flt64, flt64, 
                                 flt64, flt64, 
                                 intmach], intmach, 'iterate').
iterate(Zr, Zi, Tr, Ti, Cr, Ci, Iter, R) :-
	( Iter == 0 ->
	    R = 1
	; Zi2 = 2.0 * Zr * Zi + Ci,
	  Zr2 = Tr - Ti + Cr,
	  Tr2 = Zr2 * Zr2,
	  Ti2 = Zi2 * Zi2,
	  ( Tr2 + Ti2 > 4.0 ->
	      R = 0
	  ; iterate(Zr2, Zi2, Tr2, Ti2, Cr, Ci, Iter - 1, R)
	  )
	).

:- pred begin/1 + lowentry(det, [intmach], 'begin').
% TODO: generalize bit IO operations!
begin(N) :-
	W = N,
	H = N,
	Wrat = 2.0 / ~'$trust_typed'(W, flt64),
	Hrat = 2.0 / ~'$trust_typed'(H, flt64),

	Bit_num = ~initmut(intmach, 0),
	Byte_acc = ~initmut(char, ~'$ccons'(0, char)),

	% TODO: improve
	Buflen0 = ~newmut(intmach),
	( W mod 8 \== 0 ->
	    Buflen0 <- 1
	; Buflen0 <- 0
	),
	Buflen = (W / 8 + @Buflen0) * H,
	Buf = ~'$alloc'(malloc, array(ref0(mut(char)), Buflen)),
	Pbuf = ~initmut(ref1(array(ref0(mut(char)))), Buf),

	printf3("P4\n%d %d\n", W, H),

	Cr = ~newmut(flt64),
	Ci = ~newmut(flt64),
	'$for_each'(Y, ~flt64range(~'$trust_typed'(H, flt64)), (
	  Ci <- @Y * Hrat - 1.0,
	  '$for_each'(X, ~flt64range(~'$trust_typed'(W, flt64)), (
	    Cr <- @X * Wrat - 1.5,
	    %
	    % TODO: add richer type rules to obtain it automatically
	    Byte_acc <- ~'$trust_typed'(~'$trust_typed'(@Byte_acc, intmach) << 1, char),
	    IR = ~iterate(0.0, 0.0, 0.0, 0.0, @Cr, @Ci, 50),
	    ( IR \== 0 ->
	        Byte_acc <- ~'$trust_typed'(~'$trust_typed'(@Byte_acc, intmach) \/ 0x01, char)
	    ; true
	    ),
	    %
	    Bit_num <- @Bit_num + 1,
	    ( @Bit_num == 8 ->
	        Pbuf[0] <- @Byte_acc,
		Pbuf <- ~'$subarray'(@Pbuf, 1),
		Bit_num <- 0,
		Byte_acc <- ~'$ccons'(0, char)
	    ; true
	    )
	  )),			       
	  ( @Bit_num \== 0 ->
	      Byte_acc <- ~'$trust_typed'(~'$trust_typed'(@Byte_acc, intmach) << (8 - W mod 8), char),
	      Pbuf[0] <- @Byte_acc,
	      Pbuf <- ~'$subarray'(@Pbuf, 1),
	      Bit_num <- 0,
	      Byte_acc <- ~'$ccons'(0, char)
	  ; true
	  )
        )),
	fwrite(Buf, Buflen, 1, ~stdout).

:- '$improlog_end'.
