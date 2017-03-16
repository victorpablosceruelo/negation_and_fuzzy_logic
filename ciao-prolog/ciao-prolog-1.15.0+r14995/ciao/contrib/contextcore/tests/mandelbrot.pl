:- module(mandelbrot, [], [assertions, contextcore, fsyntax, regtypes]).
:- fun_eval arith(true).

% Test for the contextcore package, based on the ImProlog version for
% the mandelbrot test, based itself on the C version from the Computer
% Language Benchmarks Game.
%
% Usage: ./mandelbrot > out.pnm; # generates a 1-bit B/W PNM image
%
%        (then, view the image with your favourite image viewer, like
%        "eog out.pnm")
%
% NOTE ABOUT PERFORMANCE: 
%
%   The current performance in this Prolog version is not optimal, but
%   the code is not very far from the ImProlog version (which runs at
%   C speed). For example, the generation of a 3000x3000 pixel image
%   took:
%
%     - 280 seconds in the Prolog version
%     - 2 seconds in (the nastily) optimized ImProlog one (x140
%       speedup!)
%   
%   The speed-up came from optimized control, arithmetic, and no need
%   for GC collection. Removing IO operations, the test took 270
%   seconds, which means that IO operations are not a bottle-neck at
%   this point, but it will affect performance later (as C speed is
%   reached in arithmetic part). Faster IO operations could be
%   encapsulated in the pnm_writer module).
%
%   Both versions expands loops as predicates during the compilation.
%
%   Possible ways to optimize this version:
%
%   - connect/merge with the ImProlog/compilation-to-C compiler
%     (native compilation)
%
%   - write a faster version in Prolog to get hints about how to
%     improve the program expansion.
%
%   - design new bytecodes (emulated compilation)
%
%   - improve the PMN writer submodule (e.g. buffered IO, which
%     although it does not seem feasible to introduce automatically,
%     can be encapsulated)
%
% --Jose F. Morales

:- use_module(library(format)).
:- include(.(loops)).

:- export(main/0).
main :- mandelbrot(256). % lowered from 3000, which took too long to finish

% tell('/tmp/mandel.pnm'), main, told, system('xv /tmp/mandel.pnm'). 

:- export(mandelbrot/1).
mandelbrot(N) :-
	W = N,
	H = N,
	Wrat = 2.0 / W,
	Hrat = 2.0 / H,
	%
	pnm_writer:init[-Writer],
	pnm_writer:header(W, H),
	%
	range:for_each(Y, 0, H) do (
	  range:for_each(X, 0, W) do (
	    Ci = Y * Hrat - 1.0, % todo: future optimization? take out of the loop
	    Cr = X * Wrat - 1.5,
	    IR = ~iterate(0.0, 0.0, 0.0, 0.0, Cr, Ci, 50),
	    ( IR \== 0 -> C = 1 ; C = 0 ),
	    pnm_writer:pixel(C)[Writer]
	  )
        ),
	pnm_writer:flush[Writer].

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
	  
