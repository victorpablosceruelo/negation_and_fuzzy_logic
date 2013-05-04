:- module(mandelbrot_mini, [], [assertions, contextcore, fsyntax, regtypes, expander]).
:- fun_eval arith(true).

% (Version of mandelbrot for the paper)
% --Jose F. Morales

:- use_module(library(format)).
:- include(.(loops)).

:- export(main/0).
main :- begin(256). % lowered from 3000, which took too long to finish

% tell('/tmp/mandel.pnm'), main, told, system('xv /tmp/mandel.pnm'). 

:- export(begin/1).
begin(N) :-
	W = N,
	H = N,
	Wrat = 2.0 / W,
	Hrat = 2.0 / H,
	%
	pnm_writer:init[-Writer],
	pnm_writer:header(W, H),
	%
	range:for_each(Y, 0, H) do (
	  Ci = Y * Hrat - 1.0,
	  range:for_each(X, 0, W) do (
	    Cr = X * Wrat - 1.5,
	    Iter <- 50,
	    ^c(Zr, Zi) <- c(0.0, 0.0),
	    iterate(Cr, Ci)[+Zr, +Zi, Iter],
	    ( Iter = 0 -> C = 1 ; C = 0 ),
	    pnm_writer:pixel(C)[Writer]
	  )
        ),
	pnm_writer:flush[Writer].

iterate(Cr, Ci)[+Zr, +Zi, Iter] :-
	( ( Zr*Zr + Zi*Zi > 4.0 ; Iter = 0 ) ->
	    true
	; ^c(Zr, Zi) <- c(Zr*Zr - Zi*Zi + Cr, 2.0*Zr*Zi + Ci),
	  Iter <- Iter - 1,
	  iterate(Cr, Ci)[+Zr, +Zi, Iter]
	).

:- sub_module pnm_writer {
  :- def_context self = ^s(bit_num, byte_acc).
  % Writer for PNM formatted images
  % todo: use implicit context to carry the state
  header(W, H) :-
	format("P4~n~d ~d~n", [W, H]).
  init[-self] :-
	bit_num <- 0,
	byte_acc <- 0.
  pixel(C)[self] :-
	byte_acc <- byte_acc@ << 1,
        ( C \== 0 ->
	    byte_acc <- byte_acc@ \/ 0x01
	; true
	),
	bit_num <- bit_num@ + 1,
	( bit_num@ == 8 ->
	    put_code(byte_acc@),
	    bit_num <- 0,
	    byte_acc <- 0
	; true
	).
  flush[self] :-
	( bit_num@ \== 0 ->
	    byte_acc <- byte_acc@ << (8 - W mod 8),
	    put_code(byte_acc@),
	    bit_num <- 0,
	    byte_acc <- 0
	; true
        ).
}.
	  
