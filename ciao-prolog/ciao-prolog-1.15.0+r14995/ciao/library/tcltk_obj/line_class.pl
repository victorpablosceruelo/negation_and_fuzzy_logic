%%---------------------------------------------------------------------
%%
%% LINE CLASS
%%
%%---------------------------------------------------------------------

:- class(line_class).

:- use_package(assertions).

:- doc(author, "Montserrat Urraca").


:- inherit_class(library('tcltk_obj/shape_class')).

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
%% ARROW 
%%---------------------------------------------------------------------

:- data         arrow/1.
:- inheritable  arrow/1.

arrow(none).  % default color.

:- export(arrowheads/1).

arrowheads(Arrow) :-
	nonvar(Arrow),
	!,
	atom(Arrow),
	retract_fact(arrow(_)),
	asserta_fact(arrow(Arrow)),
	notify_changes.

arrowheads(Arrow) :-
%	!,
	arrow(Arrow).

%%---------------------------------------------------------------------
%% ARROW SHAPE 
%%---------------------------------------------------------------------

:- data         arrowshape/1.
:- inheritable  arrowshape/1.

arrowshape(none).  % default color.

:- export(arrow_shape/1).

arrow_shape(Arrowshape) :-
	nonvar(Arrowshape),
	!,
	atom(Arrowshape),
	retract_fact(arrowshape(_)),
	asserta_fact(arrowshape(Arrowshape)),
	notify_changes.

arrow_shape(Arrowshape) :-
%	!,
	arrowshape(Arrowshape).

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
	arrow(A),
	append([min(arrow)],[A],Opts1),
	append(Opts,Opts1,Opts3),
	inherited creation_options(Other),
	append(Opts3,Other,Options).


tcl_name(line).

%%---------------------------------------------------------------------
%% CONSTRUCTOR
%%---------------------------------------------------------------------

:- set_prolog_flag(multi_arity_warnings,off).

line_class.

line_class(PointList) :-
	validate_points(PointList),
	!,
	vertices(PointList).

line_class(Owner) :-
	shape_class(Owner).

line_class(PointList,Owner) :-
	shape_class(Owner),
	vertices(PointList).

:- set_prolog_flag(multi_arity_warnings,on).
