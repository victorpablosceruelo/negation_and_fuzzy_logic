%%---------------------------------------------------------------------
%%
%% OVAL CLASS
%%
%%---------------------------------------------------------------------

:- module(oval_class_doc,[],[objects,assertions,isomodes,regtypes]).

:- doc(author, "Montserrat Urraca").


%:- inherit_class(library('tcltk_obj/shape_class')).
%:- implements(library('class/examples/geometry/mobile')).
%:- implements(library('class/examples/class/mobile')).

:- doc(hide,'class$call'/3).
:- doc(hide,'class$used'/2).
%%---------------------------------------------------------------------
%% COORDINATES
%%---------------------------------------------------------------------

%:- data        coord/4.
%:- inheritable coord/4.

%%---------------------------------------------------------------------
:- export(coord/4).
:- pred coord(+X1,+Y1,+X2,+Y2) :: int * int * int *int
	# "@var{X1}, @var{Y1}, @var{X2}, and @var{Y2} give the  coordinates  of
       two  diagonally  opposite  corners of a rectangular region
       enclosing the oval.".
%%---------------------------------------------------------------------
coord(0,0,0,0).

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
	# "Specifies shape's @var{Heigh}.".
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

%%---------------------------------------------------------------------
%% COLOR 
%%---------------------------------------------------------------------

%:- data         outline/1.
%:- inheritable  outline/1.

outline(black).  % default color.

:- export(outline_color/1).

%%---------------------------------------------------------------------
:- pred outline_color(+Color) :: atom
	# "@var{Color} specifies the color to be used for drawing the oval's outline. This option defaults to black.".
%%---------------------------------------------------------------------
%%---------------------------------------------------------------------
:- pred outline_color(-Color) :: atom
	# "Gets oval's outline @var{Color}.".
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
	# "Specifies the name of the @var{Shape}. In this case is oval.".
%%---------------------------------------------------------------------
tcl_name(oval).

%%---------------------------------------------------------------------
:- pred creation_options(-OptionsList) :: list
	# "Creates a list with the options supported by the oval.".
%%---------------------------------------------------------------------
creation_options([]).

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
