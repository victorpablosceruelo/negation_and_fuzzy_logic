%%---------------------------------------------------------------------
%%
%% LINE CLASS
%%
%%---------------------------------------------------------------------

:- module(line_class_doc,[],[class,objects,assertions,isomodes,regtypes]).

:- doc(author, "Montserrat Urraca").


%:- inherit_class(library('tcltk_obj/shape_class')).

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
       two or more points that describe a serie of connected line segments.".
%%---------------------------------------------------------------------
%%---------------------------------------------------------------------
:- pred vertices(-ListofPoints) :: list
	# "Gets the list of points of the line.".
%%---------------------------------------------------------------------
vertices(_).

%%---------------------------------------------------------------------
%% ARROW 
%%---------------------------------------------------------------------

%:- data         arrow/1.
%:- inheritable  arrow/1.

arrow(none).  % default color.

:- export(arrowheads/1).

%%---------------------------------------------------------------------
:- pred arrowheads(+Where) :: atom
	# "@var{Where} indicates whether or not arrowheads are to be drawn at one or both ends of the line. @var{Where} must have one of the next values: none ( for no arrowheads ), first (for an arrowhead at the first point of the line), last (for an arrowhead at the last point of the line), or both (for arrowheads at both ends). This option defaults to none.".
%%---------------------------------------------------------------------
%%---------------------------------------------------------------------
:- pred arrowheads(-Where) :: atom
	# "Gets position of the arrowheads.".
%%---------------------------------------------------------------------
arrowheads(_).

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
	# "Creates a list with the options supported by the line.".
%%---------------------------------------------------------------------
creation_options(Options) :-
	point_list(Points),
	points2options(Points,Opts),
	arrow(A),
	append([min(arrow)],[A],Opts1),
	append(Opts,Opts1,Opts3),
%	inherited creation_options(Other),
	append(Opts3,_Other,Options).


%%---------------------------------------------------------------------
:- pred tcl_name(+Shape) :: atom 
	# "Specifies the name of the @var{Shape}. In this case is line.".
%%---------------------------------------------------------------------
tcl_name(line).

%%---------------------------------------------------------------------
%% CONSTRUCTOR
%%---------------------------------------------------------------------

:- set_prolog_flag(multi_arity_warnings,off).

line_class.

line_class(PointList) :-
	validate_points(PointList),
	!,
	set_vertices(PointList).

line_class(Owner) :-
	shape_class(Owner).

line_class(PointList,Owner) :-
	shape_class(Owner),
	set_vertices(PointList).

:- set_prolog_flag(multi_arity_warnings,on).
