%%---------------------------------------------------------------------
%%
%% OVAL CLASS
%%
%%---------------------------------------------------------------------

:- class(oval_class).

:- use_package(assertions).

:- doc(author, "Montserrat Urraca").


:- inherit_class(library('tcltk_obj/shape_class')).
%:- implements(library('class/examples/geometry/mobile')).
%:- implements(library('class/examples/class/mobile')).

%%---------------------------------------------------------------------
%% COORDINATES
%%---------------------------------------------------------------------

:- data        coord/4.
:- inheritable coord/4.

coord(0,0,0,0).

:- export([width/1,height/1,center/2]).

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

tcl_name(oval).

creation_options([X1,Y1,X2,Y2,min(outline),O|Other]) :-
	coord(X,Y,W,H),
	W2 is W / 2,
	H2 is H / 2,
	X1 is X-W2,
	X2 is X+W2,
	Y1 is Y-H2,
	Y2 is Y+H2,
	outline(O),
	inherited creation_options(Other).

%%---------------------------------------------------------------------
%% CONSTRUCTOR
%%---------------------------------------------------------------------

:- set_prolog_flag(multi_arity_warnings,off).

oval_class.
oval_class(Owner) :-
	shape_class(Owner).

oval_class((X,Y),W,H,Owner) :-
	shape_class(Owner),
	width(W),
	height(H),
	center(X,Y).

oval_class((X,Y),W,H) :-
	width(W),
	height(H),
	center(X,Y).

:- set_prolog_flag(multi_arity_warnings,on).
