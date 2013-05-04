:- module(ciao_icon, [], [assertions, contextcore, fsyntax, regtypes]).
:- fun_eval arith(true).

:- include(.(loops)).

% ---------------------------------------------------------------------------

%:- sub_module default {
%  :- method(init, init/0).
%  :- method(destroy, destroy/0).
%}.

:- sub_module color_cycle {
%  :- extends(default).
  :- abstract.
  :- data color/2.
  :- data length/1.
  % todo: fsyntax is not working on sub_modules
  cycle(C)[S] :-
        length(N),
	color(S mod N, C),
	S <- S + 1.
  init :-
        % From string/1 build color/2 and len/1
        string(L),
	I <- 0,
        list:for_each(X, L) do (
          atom_codes(Xa, [X]),
          assertz(color(I, Xa)),
          I <- I + 1
        ),
        assertz(length(I)).
}.

% todo: extends is almost as a use_package!

:- sub_module ciao_color {
  :- extends(color_cycle).
  string("ciao").
}.

:- sub_module prolog_color {
  :- extends(color_cycle).
  string("PROLOG").
}.

:- export(main/1).
main([Arg]) :-
	atom_codes(Arg, Codes),
	number_codes(Radius, Codes),
	!,
	%
	prolog_color:init,
	ciao_color:init,
	draw_icon(Radius).
main(_) :-
	display('Usage: ciao_icon NUMBER (usually, from 1 to 30)'),
	nl.

% Draw the Ciao icon in text
draw_icon(Radius) :-
	Radius > 0,
	InnerRadius = Radius / 2,
	R <- 0,
	S <- 0,
        closed_range:for_each(Y, -Radius, Radius) do (
          Blank <- ' ',
  	  closed_range_step:for_each(X, -Radius, Radius, 0.5) do (
            ( X > Y -> Blank <- '.' ; true ),
	    R2 = sqrt(X*X + Y*Y),
	    ( R2 < InnerRadius ->
	        ciao_color:cycle(C)[R]
	    ; R2 < Radius ->
	        ( X > 0, X > Y, X > -Y ->
		    C = Blank
		; prolog_color:cycle(C)[S]
		)
	    ; C = Blank
	    ),
	    display(C)
          ),
	  nl
        ).


