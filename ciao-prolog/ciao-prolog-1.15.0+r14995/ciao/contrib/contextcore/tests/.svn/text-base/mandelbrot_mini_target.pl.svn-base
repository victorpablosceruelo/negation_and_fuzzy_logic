:- module(mandelbrot_mini, [], [assertions, contextcore, fsyntax, regtypes]).
:- fun_eval arith(true).

(Adding WHILE -- not working yet)
% (Version of mandelbrot for the paper)
% --Jose F. Morales

:- use_module(library(format)).
:- include(.(loops)).

:- export(main/0).
main :- begin(256). % lowered from 3000, which took too long to finish

% tell('/tmp/mandel.pnm'), main, told, system('xv /tmp/mandel.pnm'). 

:- export(begin/1).
begin(N) :-
	mandelbrot(N, N).

mandelbrot(W, H) :-
	pnm:init(W, H)[-Writer],
	Wrat = 2.0 / W,
	Hrat = 2.0 / H,
	range:for_each(Y, 0, H) do (
	  Ci = Y * Hrat - 1.0,
	  range:for_each(X, 0, W) do (
	    Cr = X * Wrat - 1.5,
	    ^c(Zr, Zi) <- c(0.0, 0.0),
	    Iter <- 50,
	    while (Iter > 0, Zr*Zr + Zi*Zi <= 4.0) do (
              ^c(Zr, Zi) <- c(Zr*Zr - Zi*Zi + Cr, 
                              2.0*Zr*Zi + Ci),
	      Iter <- Iter - 1
	    ),
	    ( Iter = 0 -> C = 1 ; C = 0 ),
	    pnm:put(C)[Writer]
	  )
        ),
	pnm:finish[Writer].

:- state pnm {
  :- var bit_num, byte_acc.
  -init(W, H) :-
	format("P4~n~d ~d~n", [W, H]),
	bit_num <- 0,
	byte_acc <- 0.
  put(C) :-
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
  finish :-
	( bit_num@ \== 0 ->
	    byte_acc <- byte_acc@ << (8 - W mod 8),
	    put_code(byte_acc@),
	    bit_num <- 0,
	    byte_acc <- 0
	; true
        ).
}.
	  
