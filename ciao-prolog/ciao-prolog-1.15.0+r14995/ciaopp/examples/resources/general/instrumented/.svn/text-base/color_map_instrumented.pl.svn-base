%
%  color.pl			Nai-Wei Lin			April 1992
%
%  This program performs map coloring.
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(color_map_instrumented, [color_map/5],
	    [assertions, regtypes, library(resdefs(resources_decl))]).

:- use_module('../color_map_res').

:- load_resource_module(color_map_res).
:- resource max_number_of_unifs.
:- head_cost(ub, max_number_of_unifs, delta_max_number_of_unifs).
:- literal_cost(ub, max_number_of_unifs, 0).

:- entry color_map/5 : var * var * var * var * var.

% :- mode( color_map/5, [ -, -, -, -, - ] ).
% :- measure( color_map/5, [ size, size, size, size, size ] ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
color_map(A, B, C, D, E) :-
	term_size(color_map(_A, _B, _C, _D, _E), _N),
	display(_N),
	display('+'),
	color(A),
	color(B),
	color(C),
	color(D),
	color(E),
	legal_coloring(A, B, C, D, E).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% :- mode( legal_coloring/5, [ +, +, +, +, + ] ).
% :- measure( legal_coloring/5, [ size, size, size, size, size ] ).
% :- domain( legal_coloring/5, [
% 		[ blue, green, orange, red, yellow ],
% 		[ blue, green, orange, red, yellow ],
% 		[ blue, green, orange, red, yellow ],
% 		[ blue, green, orange, red, yellow ],
% 		[ blue, green, orange, red, yellow ] ] ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
legal_coloring(A, B, C, D, E) :-
	term_size(legal_coloring(_A, _B, _C, _D, _E), _N), display(_N),
	display( '+'),
	A \== B,
	A \== C,
	A \== D,
	A \== E,
	c(B, C, D),
	C \== E.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% :- mode( c/3, [ +, +, + ] ).
% :- measure( c/3, [ size, size, size ] ).
% :- domain( c/3, [
% 		[ blue, green, orange, red, yellow ],
% 		[ blue, green, orange, red, yellow ],
% 		[ blue, green, orange, red, yellow ] ] ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c(X, Y, Z) :-
	term_size(c(_X, _Y, _Z), _N), display(_N), display('+'),
	X \== Y,
	X \== Z.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% :- mode( color/1, [ - ] ).
% :- measure( color/1, [ size ] ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- regtype color/1.

color(blue) :- term_size(color(blue), _N), display(_N), display('+').
color(green) :- term_size(color(green), _N), display(_N), display('+').
color(orange) :- term_size(color(orange), _N), display(_N), display('+').
color(red) :- term_size(color(red), _N), display(_N), display('+').
color(yellow) :- term_size(color(yellow), _N), display(_N), display('+').
