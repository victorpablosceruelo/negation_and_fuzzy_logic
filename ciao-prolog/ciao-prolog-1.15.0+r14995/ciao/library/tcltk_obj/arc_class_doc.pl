%%---------------------------------------------------------------------
%%
%% ARC CLASS
%%
%%---------------------------------------------------------------------

:- module(arc_class_doc,[],[objects,assertions,isomodes,regtypes]).

:- doc(author, "Montserrat Urraca").


%:- inherit_class(library('tcltk/examples/interface/shape_class')).
%:- implements(library('class/examples/geometry/mobile')).

:- doc(hide,'class$call'/3).
:- doc(hide,'class$used'/2).
%%---------------------------------------------------------------------
%% COORDINATES
%%---------------------------------------------------------------------

coord(0,0,0,0).

%%---------------------------------------------------------------------
:- export(coord/4).

:- pred coord(+X1,+Y1,+X2,+Y2) :: int * int * int *int
	# "@var{X1}, @var{Y1}, @var{X2}, and @var{Y2} give the coordinates of
       two diagonally opposite corners of  a  rectangular  region
       enclosing  the oval that defines the arc.".
%%---------------------------------------------------------------------
:- export([width/1]).

%%---------------------------------------------------------------------
:- pred width(+Width) :: int
	# "Specifies shape's @var{Width}.".
%%---------------------------------------------------------------------
%%---------------------------------------------------------------------
:- pred width(-Width) :: int
	# "Gets shape's @var{Width}.".
%%---------------------------------------------------------------------
width(_).

:- export([height/1]).

%%---------------------------------------------------------------------
:- pred height(+Height) :: int
	# "Specifies shape's @var{Height}.".
%%---------------------------------------------------------------------
%%---------------------------------------------------------------------
:- pred height(-Height) :: int
	# "Gets shape's @var{Height}.".
%%---------------------------------------------------------------------

height(_).

:- export([center/2]).

%%---------------------------------------------------------------------
:- pred center(+X,+Y) :: int * int
	# "Specifies shape's center with @var{X} and @var{Y}.".
%%---------------------------------------------------------------------
%%---------------------------------------------------------------------
:- pred center(-X,-Y) :: int * int
	# "Gets shape's center with @var{X} and @var{Y}.".
%%---------------------------------------------------------------------
center(_,_).


angle(0).

:- export(angle_start/1).

%%---------------------------------------------------------------------
:- pred angle_start(+Angle) :: int
	# "@var{Angle} specifies  the beginning of the angular range occupied by 
           the arc.  
           Degrees are given in  units  of
           degrees   measured   counter-clockwise   from   the
           3-o'clock position;  it may be either  positive  or
           negative.".
%%---------------------------------------------------------------------
%%---------------------------------------------------------------------
:- pred angle_start(-Angle) :: int 
	# "Gets the value of the @var{Angle}.".
%%---------------------------------------------------------------------
angle_start(_).

style('pieslice').

:- export(style_type/1).

%%---------------------------------------------------------------------
:- pred style_type(+Style) :: atom 
	# "@var{Style} specifies how to draw the arc. If type is pieslice
           (the default) then the arc's region is defined by a
           section  of the oval's perimeter plus two line segments, 
	   one between the center of the oval and  each
           end  of  the  perimeter  section.  If type is chord
           then the arc's region is defined by  a  section  of
           the  oval's  perimeter  plus  a single line segment
           connecting the two end points of the perimeter section.
           If  type  is arc then the arc's region consists of a section 
           of the perimeter alone.  In this
           last case the -fill option is ignored.".
%%---------------------------------------------------------------------
%%---------------------------------------------------------------------
:- pred style_type(-Style) :: atom 
	# "Gets the @var{Style} of the arc.".
%%---------------------------------------------------------------------
style_type(_).

%%---------------------------------------------------------------------
%% COLOR 
%%---------------------------------------------------------------------

outline(black).  % default color.

:- export(outline_color/1).

%%---------------------------------------------------------------------
:- pred outline_color(+Color) :: atom
	# "@var{Color} specifies the color used for drawing the arc's outline. This option defaults to black.".
%%---------------------------------------------------------------------
%%---------------------------------------------------------------------
:- pred outline_color(-Color) :: atom
	# "It gets arc's outline @var{Color}.".
%%---------------------------------------------------------------------
outline_color(_).

%%---------------------------------------------------------------------
%% IMPLEMENTATION
%%---------------------------------------------------------------------

:- export([tcl_name/1,creation_options/1]).

:- doc(hide,tcl_name/1).
:- doc(hide,creation_options/1).
%%---------------------------------------------------------------------
:- pred tcl_name(+Shape) :: atom 
	# "Specifies the name of the @var{Shape}. In this case is arc.".
%%---------------------------------------------------------------------
tcl_name(arc).

%%---------------------------------------------------------------------
:- pred creation_options(-OptionsList) :: list
	# "Creates a list with the options supported by the arc.".
%%---------------------------------------------------------------------
%creation_options([" ",X1," ",Y1," ",X2," ",Y2," ",'-start',A," "|Other]) :-
%creation_options([" ",X1," ",Y1," ",X2," ",Y2,' -start',A," "|Other]) :-
creation_options([]).

%%---------------------------------------------------------------------
%% CONSTRUCTOR
%%---------------------------------------------------------------------

:- set_prolog_flag(multi_arity_warnings,off).

arc_class.
arc_class(Owner) :-
	shape_class(Owner).

arc_class((X,Y),W,H,Owner) :-
	shape_class(Owner),
	set_width(W),
	set_height(H),
	set_center(X,Y).
%	set_angle(A).

arc_class((X,Y),W,H) :-
	set_width(W),
	set_height(H),
	set_center(X,Y).
%	set_angle(A).

:- set_prolog_flag(multi_arity_warnings,on).
