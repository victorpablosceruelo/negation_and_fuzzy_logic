%%---------------------------------------------------------------------
%%
%% ARC CLASS
%%
%%---------------------------------------------------------------------

:- class(arc_class).

:- use_package(assertions).

:- doc(author, "Montserrat Urraca").

:- inherit_class(library('tcltk_obj/shape_class')).
%:- implements(library('class/examples/geometry/mobile')).

%%---------------------------------------------------------------------
%% COORDINATES
%%---------------------------------------------------------------------

:- data        coord/4.
:- inheritable coord/4.

coord(0,0,0,0).

:- export([width/1]).

width(W) :-
	nonvar(W),
	!,
	number(W),
	W >= 0,
	retract_fact(coord(X,Y,_,H)),
	asserta_fact(coord(X,Y,W,H)),
	notify_changes.

width(W) :-
	coord(_,_,W,_).

:- export([height/1]).

height(H) :-
	nonvar(H),
	!,
	number(H),
	H >= 0,
	retract_fact(coord(X,Y,W,_)),
	asserta_fact(coord(X,Y,W,H)),
	notify_changes.

height(H) :-
	coord(_,_,_,H).

:- export([center/2]).

center(X,Y) :-
	nonvar(X),
	!,
	nonvar(Y),
	!,
	number(X),
	number(Y),
	retract_fact(coord(_,_,W,H)),
	asserta_fact(coord(X,Y,W,H)),
	notify_changes.

center(X,Y) :-
	coord(X,Y,_,_).


:- data        angle/1.
:- inheritable angle/1.

angle(0).

:- export(angle_start/1).

angle_start(Angle) :-
	nonvar(Angle),
	!,
	number(Angle),
	Angle > 0,
	set_fact(angle(Angle)),
	notify_changes.

angle_start(Angle) :-
	angle(Angle).

:- data        style/1.
:- inheritable style/1.

style('pieslice').

:- export(style_type/1).

style_type(Style) :-
	nonvar(Style),
	!,
	atom(Style),
	set_fact(style(Style)),
	notify_changes.

style_type(Style) :-
	style(Style).

%%---------------------------------------------------------------------
%% COLOR 
%%---------------------------------------------------------------------

:- data         outline/1.
:- inheritable  outline/1.

outline(black).  % default color.

:- export(outline_color/1).

outline_color(Outline) :-
	nonvar(Outline),
	!,
	atom(Outline),
	retract_fact(outline(_)),
	asserta_fact(outline(Outline)),
	notify_changes.

outline_color(Outline) :-
%	!,
	outline(Outline).

%%---------------------------------------------------------------------
%% IMPLEMENTATION
%%---------------------------------------------------------------------

:- export([tcl_name/1,creation_options/1]).

tcl_name(arc).

%creation_options([" ",X1," ",Y1," ",X2," ",Y2," ",'-start',A," "|Other]) :-
%creation_options([" ",X1," ",Y1," ",X2," ",Y2,' -start',A," "|Other]) :-
creation_options([X1,Y1,X2,Y2,min(start),A,min(style),S,min(outline),O|Other]) :-
	coord(X,Y,W,H),
	W2 is W / 2,
	H2 is H / 2,
	X1 is X-W2,
	X2 is X+W2,
	Y1 is Y-H2,
	Y2 is Y+H2,
	angle(A),
	style(S),
	outline(O),
	inherited creation_options(Other).

%%---------------------------------------------------------------------
%% CONSTRUCTOR
%%---------------------------------------------------------------------

:- set_prolog_flag(multi_arity_warnings,off).

arc_class.
arc_class(Owner) :-
	shape_class(Owner).

arc_class((X,Y),W,H,Owner) :-
	shape_class(Owner),
	width(W),
	height(H),
	center(X,Y).
%	set_angle(A).

arc_class((X,Y),W,H) :-
	width(W),
	height(H),
	center(X,Y).
%	set_angle(A).

:- set_prolog_flag(multi_arity_warnings,on).
