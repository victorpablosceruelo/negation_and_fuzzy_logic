%%---------------------------------------------------------------------
%%
%% POLYGON CLASS
%%
%%---------------------------------------------------------------------

:- class(poly_class).

:- use_package(assertions).

:- doc(author, "Montserrat Urraca").


:- inherit_class(library('tcltk_obj/shape_class')).

%:- implements(library('class/examples/geometry/mobile')).
:- use_module(library(lists), [append/3]).

%%---------------------------------------------------------------------
%% POINT LIST
%%---------------------------------------------------------------------

:- data        point_list/1.
:- inheritable point_list/1.

validate_points([]).

validate_points([(X,Y)|N]) :-
	number(X),
	number(Y),
	!,
	validate_points(N).

:- export(vertices/1).

vertices(L) :-
	nonvar(L),
	!,
	validate_points(L),
	set_fact(point_list(L)),
	notify_changes.

vertices(L) :-
	point_list(L).

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

points2options([],[]).

points2options([(X,Y)|Np],[X,Y|No]) :-
	points2options(Np,No).

:- export([tcl_name/1,creation_options/1]).

creation_options(Options) :-
	point_list(Points),
	points2options(Points,Opts),
	inherited creation_options(Other),
	append(Opts,Other,Options).


tcl_name(polygon).

%%---------------------------------------------------------------------
%% CONSTRUCTOR
%%---------------------------------------------------------------------

:- set_prolog_flag(multi_arity_warnings,off).

poly_class.

poly_class(PointList) :-
	validate_points(PointList),
	!,
	vertices(PointList).

poly_class(Owner) :-
	shape_class(Owner).

poly_class(PointList,Owner) :-
	shape_class(Owner),
	vertices(PointList).

:- set_prolog_flag(multi_arity_warnings,on).
