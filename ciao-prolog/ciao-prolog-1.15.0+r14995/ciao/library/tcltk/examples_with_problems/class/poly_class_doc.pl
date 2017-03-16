%%---------------------------------------------------------------------
%%
%% POLYGON CLASS
%%
%%---------------------------------------------------------------------
:- module(poly_class_doc,[],[objects,assertions,isomodes,regtypes]).


:- inherit_class(library('tcltk/examples/class/shape_class')).

:- implements(library('class/examples/geometry/mobile')).
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

:- export(set_vertices/1).
%%---------------------------------------------------------------------
:- pred set_vertices(+ListofPoints) :: list
        # "The arguments of the list specify  the  coordinates  for
       three  or  more  points that define a closed polygon.  The
       first and last points may be the same.
       After  the  coordinates  there  may  be  any   number   of
       option-value pairs, each of which sets one of the configu-
       ration options for  the  item.".
%%---------------------------------------------------------------------
       
set_vertices(L) :-
        validate_points(L),
        set_fact(point_list(L)),
        notify_changes.

:- export(get_vertices/1).
%%---------------------------------------------------------------------
:- pred get_vertices(-ListofPoints) :: list
        # "Gets the list of vertices of the polygon.".
%%---------------------------------------------------------------------

get_vertices(L) :-
        point_list(L).

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
%%---------------------------------------------------------------------
:- pred tcl_name(+Shape) :: atom 
        # "Specifies the name of the Shape. In this case is polygon.".
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
