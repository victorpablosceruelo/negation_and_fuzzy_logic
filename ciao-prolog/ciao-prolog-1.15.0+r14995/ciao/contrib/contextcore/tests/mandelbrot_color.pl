:- module(mandelbrot_color, [], [assertions, contextcore, fsyntax, regtypes]).
:- fun_eval arith(true).

% Another version of mandelbrot with colors.
%
% This version generates a .PPM image.
% It defines several submodules:
%   complex: complex numbers
%   image:   images 
%   rgb:     RGB colors

% --Jose F. Morales

:- use_module(library(format)).
:- use_module(library(lists), [length/2]).
:- include(.(loops)).

:- export(main/0).
main :- mandelbrot_color(256). % lowered from 3000, which took too long to finish

% tell('/tmp/mandel.pnm'), main, told, system('xv /tmp/mandel.pnm'). 

:- sub_module background {
  :- extends(image).
  data(["......................x.................x..x...x........ccccc..ii...................",
        "......................x...................xxxx.x.......cc...........................",
	"..xxxx.xxx...xx.x..xxxx..xxx....x..x..x.x..x...xxx....cc.......ii...cccc.c..iiiii...",
	"..x...x...x.x..xx.x...x.x...x...x..x..x.x..x...x..x...cc.......ii..cc...cc.ii...ii..",
	"..x...x...x.x...x.x...x.xxxxx...x..x..x.x..x...x..x...cc.......ii..cc...cc.ii...ii..",
	"..x...x...x.x...x.x...x.x.......x..x..x.x..x...x..x....cc......ii..cc...cc.ii...ii..",
	"..x...x...x..xxxx..xxxx..xxx.....xx.xx..x...xx.x..x.....ccccc...ii..cccccc..iiiii...",
	"...................................................................................."]).
  color(0'., C) :- rgb:init__(0,0,0)[-(^C)].
  color(0'x, C) :- rgb:init__(0.8,0.8,0.8)[-(^C)].
  color(0'c, C) :- rgb:init__(0.5,0,1.0)[-(^C)].
  color(0'i, C) :- rgb:init__(1.0,0.5,0)[-(^C)].
}.

:- export(mandelbrot_color/1).
mandelbrot_color(N) :-
	W = N, H = N,
	Wrat = 2.0 / W,
	Hrat = 2.0 / H,
	%
	ppm_writer:init[-Writer],
	ppm_writer:header(W, H),
	%
	background:init,
	%
	range:for_each(Y, 0, H) do (
	  range:for_each(X, 0, W) do (
	    C = complex(X * Wrat - 1.5, Y * Hrat - 1.0),
	    IR = ~iterate(complex(0.0, 0.0), C, 50),
	    color(X, Y, IR, Color),
	    ppm_writer:pixel(Color)[Writer]
	  )
        ),
	ppm_writer:flush[Writer].

% todo: implement 'while'
iterate(Z, C, Iter, R) :-
	( Iter == 0 ->
	    R = Iter
	; complex:mul(Z, Z, Zmul), Z <- Zmul,
	  complex:add(Z, C, Zadd), Z <- Zadd,
	  ( complex:abs(Z, Zabs), Zabs > 2.0 ->
	      R = Iter
	  ; iterate(Z, C, Iter - 1, R)
	  )
	).

color(X0, Y0, Iter, C) :-
	Iter = 0,
	!,
	background:width(W),
	background:height(H),
	X = (X0 // 2) mod W,
	Y = (Y0 // 2) mod H,
	background:get(X, Y, C).
color(_X, _Y, Iter, C) :-
	I = Iter / 50,
	rgb:init__(0.8 * I - 0.4, 0.5 * I, 0.7 * I)[-(^C)].

% ---------------------------------------------------------------------------

:- sub_module complex {
  % todo: add functions; use hindley-milner typing for modules?
  % todo: overwrite + *, etc.? (doing that efficiently requires types)
  mul(complex(Ar, Ai), complex(Br, Bi), R) :- R = complex(Ar * Br - Ai * Bi, Ar * Bi + Ai * Br).
  add(complex(Ar, Ai), complex(Br, Bi), R) :- R = complex(Ar + Br, Ai + Bi).
  abs(complex(Ar, Ai), R) :- R = sqrt(Ar * Ar + Ai * Ai).
}.

% ---------------------------------------------------------------------------

:- sub_module rgb {
  % todo: omit self everywhere
  :- def_context self = ^rgb(r, g, b).
  % todo: use ~rgb(R,G,B) in a term position as syntactic sugar for init__ method?
  % todo: allow rgb(R,G,B) in atom-based module system?
  init__(R, G, B)[-self] :-
	r <- ~to255(R),
	g <- ~to255(G),
	b <- ~to255(B).
  get_r(R)[+self] :- R = r@ .
  get_g(G)[+self] :- G = g@ .
  get_b(B)[+self] :- B = b@ .
}.

to255(C0) := C :-
	C1 = 255 * C0,
	( C1 < 0 -> C = 0
	; C1 > 255 -> C = 255
	; C = integer(C1)
	).

% ---------------------------------------------------------------------------

:- sub_module image {
  % todo: implement instances of modules
  :- abstract.
  % todo: allow multi-indexed predicates?
  % todo: specialize data definitions
  :- data pixel_/2.
  :- data width/1.
  :- data height/1.
  % Initialize the image data
  init :-
        data(Data),
	Data = [Row0|_], length(Row0, Width), assertz(width(Width)),
	length(Data, Height), assertz(height(Height)),
	P <- 0,
        list:for_each(Row, Data) do (
          list:for_each(B, Row) do (
             assertz(pixel_(P, B)),
	     P <- P + 1
          )
        ).
  % The color at point (X,Y)
  get(X, Y, C) :-
        width(W), P = X + Y * W,
	pixel_(P, B),
	color(B, C).
  % todo: implement
  % :- virtual(data/1).
  % :- virtual(color/2).
}.

% ---------------------------------------------------------------------------

:- sub_module ppm_writer {
  % Writer for PPM formatted images
  header(W, H) :-
	format("P6~n~d ~d~n", [W, H]),
	format("255~n", []).
  init[-self] :- true.
  pixel(C)[self] :-
        % todo: syntax is not nice in that case
	rgb:get_r(R)[+(^C)], put_code(R),
	rgb:get_g(G)[+(^C)], put_code(G),
	rgb:get_b(B)[+(^C)], put_code(B).
  flush[self] :- true.
}.
	  
