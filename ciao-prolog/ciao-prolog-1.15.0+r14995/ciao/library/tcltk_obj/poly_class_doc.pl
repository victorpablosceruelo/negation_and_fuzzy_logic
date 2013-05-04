%%---------------------------------------------------------------------
%%
%% POLYGON CLASS
%%
%%---------------------------------------------------------------------

:- module(poly_class_doc,[],[class,objects,assertions,isomodes,regtypes]).

:- doc(author, "Montserrat Urraca").


%:- inherit_class(library('tcltk_obj/shape_class')).

%:- implements(library('class/examples/geometry/mobile')).
:- use_module(library(lists), [append/3]).

:- doc(hide,'class$call'/3).
:- doc(hide,'class$used'/2).
%%---------------------------------------------------------------------
%% POINT LIST
%%---------------------------------------------------------------------

:- data        point_list/1.
:- inheritable(point_list/1).


:- data set_vertices/1.
:- data shape_class/1.

validate_points([]).

validate_points([(X,Y)|N]) :-
	number(X),
	number(Y),
	!,
	validate_points(N).

:- export(vertices/1).

%%---------------------------------------------------------------------
:- pred vertices(+ListofPoints) :: list
	# "The arguments of the list specify  the  coordinates  for
       three  or  more  points that define a closed polygon.  The
       first and last points may be the same.
       After  the  coordinates  there  may  be  any   number   of
       option-value pairs, each of which sets one of the configu-
       ration options for  the  item.".
%%---------------------------------------------------------------------
%%---------------------------------------------------------------------
:- pred vertices(-ListofPoints) :: list
	# "Gets the list of vertices of the polygon.".
%%---------------------------------------------------------------------
vertices(_).

%%---------------------------------------------------------------------
%% COLOR 
%%---------------------------------------------------------------------

%:- data         outline/1.
%:- inheritable  outline/1.

outline(black).  % default color.

:- export(outline_color/1).

%%---------------------------------------------------------------------
:- pred outline_color(+Color) :: atom
	# "@var{Color} specifies the color to be used for drawing the polygon's outline. This option defaults to black.".
%%---------------------------------------------------------------------
%%---------------------------------------------------------------------
:- pred outline_color(-Color) :: atom
	# "Gets poly's outline @var{Color}.".
%%---------------------------------------------------------------------
outline_color(_).

%%---------------------------------------------------------------------
%% IMPLEMENTATION
%%---------------------------------------------------------------------

points2options([],[]).

points2options([(X,Y)|Np],[X,Y|No]) :-
	points2options(Np,No).

:- export([tcl_name/1,creation_options/1]).

:- doc(hide,tcl_name/1).
:- doc(hide,creation_options/1).
%%---------------------------------------------------------------------
:- pred creation_options(-OptionsList) :: list
	# "Creates a list with the options supported by the polygon.".
%%---------------------------------------------------------------------
creation_options(Options) :-
	point_list(Points),
	points2options(Points,Opts),
%	inherited creation_options(Other),
 % MCL: was
 %	append(Opts,Other,Options).
 % and since "Other" appeared in a commented call above, a singleton
 % warning was issued.
        Opts = Options.


%%---------------------------------------------------------------------
:- pred tcl_name(+Shape) :: atom 
	# "Specifies the name of the @var{Shape}. In this case is polygon.".
%%---------------------------------------------------------------------
tcl_name(polygon).

%%---------------------------------------------------------------------
%% CONSTRUCTOR
%%---------------------------------------------------------------------

:- set_prolog_flag(multi_arity_warnings,off).

poly_class.

poly_class(PointList) :-
	validate_points(PointList),
	!,
	set_vertices(PointList).

poly_class(Owner) :-
	shape_class(Owner).

poly_class(PointList,Owner) :-
	shape_class(Owner),
	set_vertices(PointList).

:- set_prolog_flag(multi_arity_warnings,on).
