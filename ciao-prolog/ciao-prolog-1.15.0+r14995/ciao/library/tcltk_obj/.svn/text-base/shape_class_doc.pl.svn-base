%%---------------------------------------------------------------------
%%
%% SHAPE CLASS
%%
%%---------------------------------------------------------------------

:- module(shape_class_doc,[],[class,objects,assertions,isomodes,regtypes]).

:- doc(author, "Montserrat Urraca").

:- use_class(library('tcltk_obj/canvas_class')).

:- doc(hide,'class$call'/3).
:- doc(hide,'class$used'/2).
%%---------------------------------------------------------------------
%% COLOR 
%%---------------------------------------------------------------------

%:- data         tcl_color/1.
%:- inheritable  tcl_color/1.

tcl_color(black).  % default color.

:- export(bg_color/1).
%%---------------------------------------------------------------------
:- pred bg_color(+BackgroundColor) :: atom 
	# "@var{Background Color} specifies the color  to  use  for  drawing  the
             shape's  outline.  This option  defaults  to
          black.".
%%---------------------------------------------------------------------
%%---------------------------------------------------------------------
:- pred bg_color(-BackgroundColor) :: atom 
	# "Gets the shape @var{Background Color}.".
%%---------------------------------------------------------------------

bg_color(_).

%%---------------------------------------------------------------------
%% BORDER WIDTH
%%---------------------------------------------------------------------

%:- data        border/1.
%:- inheritable border/1.

border(1).

:- export(border_width/1).

%%---------------------------------------------------------------------
:- pred border_width(+Width) :: num 
	# "Specifies the @var{Width} that the canvas widget should request from its geometry manager.".
%%---------------------------------------------------------------------
%%---------------------------------------------------------------------
:- pred border_width(-Width) :: num 
	# "Gets the @var{Width} of the canvas widget.".
%%---------------------------------------------------------------------
border_width(_).

%%---------------------------------------------------------------------
%% IMPLEMENTATION
%%---------------------------------------------------------------------

:- export([tcl_name/1,creation_options/1]).

:- doc(hide,'tcl_name'/1).

tcl_name(_) :- fail.

:- doc(hide,tcl_name/1).
:- doc(hide,creation_options/1).
%%---------------------------------------------------------------------
:- pred creation_options(-OptionsList) :: list
	# "Creates a list with the options supported by the shapes.".
%%---------------------------------------------------------------------
%creation_options([min(fill),BG,min(outline),FG,min(width),B]) :-
creation_options([min(fill),BG,min(width),B]) :-
	tcl_color(BG),
	border(B).

%:- inheritable(notify_changes/0).

notify_changes:-
	self(Shape),
	owner(AnOwner),
	AnOwner instance_of canvas_class,
	AnOwner:shape_changed(Shape),
	fail.
notify_changes.

:- export([add_owner/1,remove_owner/1]).

:- doc(hide,'add_owner'/1).
:- doc(hide,'remove_owner'/1).

add_owner(Owner) :-
	\+ owner(Owner),
	Owner instance_of canvas_class,
	assertz_fact(owner(Owner)),
	self(Shape),
	Owner:add_shape(Shape),
	!.
add_owner(_).


remove_owner(Owner) :-
	retract_fact(owner(Owner)),
	Owner instance_of canvas_class,
	self(Shape),
	Owner:remove_shape(Shape),
	!.

remove_owner(_).

%%---------------------------------------------------------------------
%% CONSTRUCTOR/DESTRUCTOR
%%---------------------------------------------------------------------

:- data        owner/1.
:- inheritable(owner/1).

:- set_prolog_flag(multi_arity_warnings,off).
%%------------------------------------------------------------------------
:- export(shape_class/0).

:- pred shape_class
	# " Creates a new shape object.".

shape_class.  % Not owned

:- export(shape_class/1).

:- pred shape_class(+ShapeList) :: list
	# "Adds shapes of the list to the canvas object.".
%%------------------------------------------------------------------------

shape_class.  % Not owned

shape_class([]) :- !.

shape_class([AnOwner|Next]) :-
	add_owner(AnOwner),
	!,
	shape_class(Next).

shape_class(AnOwner) :-
	!,
	add_owner(AnOwner).

:- set_prolog_flag(multi_arity_warnings,on).

destructor :-
	self(Shape),
	retract_fact(owner(AnOwner)),
	AnOwner instance_of canvas_class,     % Owner is still alive
	AnOwner:remove_shape(Shape),
	fail.
