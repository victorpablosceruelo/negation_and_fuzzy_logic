%%---------------------------------------------------------------------
%%
%% TEXT CLASS
%%
%%---------------------------------------------------------------------

:- module(text_class_doc,[],[objects,assertions,isomodes,regtypes]).

:- doc(author, "Montserrat Urraca").


%:- inherit_class(library('tcltk_obj/shape_class')).
%:- implements(library('class/examples/geometry/mobile')).
%:- implements(library('class/examples/class/mobile')).

:- doc(hide,'class$call'/3).
:- doc(hide,'class$used'/2).
%%---------------------------------------------------------------------
%% COORDINATES
%%---------------------------------------------------------------------

%:- data        coord/2.
%:- inheritable coord/2.

%%---------------------------------------------------------------------
:- export(coord/2).
:- pred coord(+X,+Y) :: int * int
	# "@var{X} and @var{Y} specify the coordinates of a point used to position the text on the display.".
%%---------------------------------------------------------------------
coord(0,0).

:- export([point/2]).

%%---------------------------------------------------------------------
:- pred point(+X,+Y) :: int * int
	# "@var{X} and @var{Y} change the coordinates of a point used to position the text on the display.".
%%---------------------------------------------------------------------
point(_,_).

%%---------------------------------------------------------------------
%%  TEXT 
%%---------------------------------------------------------------------

%:- data        text/1.
%:- inheritable text/1.

text('').

:- export(text_characters/1).

%%---------------------------------------------------------------------
:- pred text_characters(+Text) :: atom
	# "@var{Text} specifies the characters to be displayed in the text item. This option defaults to an empty string.".
%%---------------------------------------------------------------------
%%---------------------------------------------------------------------
:- pred text_characters(-Text) :: atom
	# "Gets the text displayed in the text item.".
%%---------------------------------------------------------------------
text_characters(_).

%%---------------------------------------------------------------------
%%  ANCHOR 
%%---------------------------------------------------------------------

%:- data        anchorpos/1.
%:- inheritable anchorpos/1.

anchor('center').

:- export(anchor/1).

%%---------------------------------------------------------------------
:- pred anchor(+AnchorPos) :: atom
	# "@var{AnchorPos} tells how to position the text relative to the positioning point for the text. This option defaluts to center.".
%%---------------------------------------------------------------------
%%---------------------------------------------------------------------
:- pred anchor(-AnchorPos) :: atom
	# "Gets the position of the text relative to the positioning point.".
%%---------------------------------------------------------------------
anchor(_).

%%---------------------------------------------------------------------
%%  FONT 
%%---------------------------------------------------------------------

%:- data        font/1.
%:- inheritable font/1.

font('arial').

:- export(font_type/1).

%%---------------------------------------------------------------------
:- pred font_type(+Font) :: atom
	# "@var{Font} specifies the font to use for the text item. This option defaluts to arial.".
%%---------------------------------------------------------------------
%%---------------------------------------------------------------------
:- pred font_type(-Font) :: atom
	# "Gets the value of the @var{Font} used for the text item.".
%%---------------------------------------------------------------------
font_type(_).

%%---------------------------------------------------------------------
%%  JUSTIFY 
%%---------------------------------------------------------------------

%:- data        justify/1.
%:- inheritable justify/1.

justify('left').

:- export(justify_text/1).

%%---------------------------------------------------------------------
:- pred justify_text(+How) :: atom
	# "@var{How} specifies how to justify the text within its bounding region. @var{How} must be one of the values left, right or center.  This option defaluts to left.".
%%---------------------------------------------------------------------
%%---------------------------------------------------------------------
:- pred justify_text(-How) :: atom
	# "Gets @var{How} is justified the text.".
%%---------------------------------------------------------------------
justify_text(_).

%%---------------------------------------------------------------------
%% IMPLEMENTATION
%%---------------------------------------------------------------------

:- export([tcl_name/1,creation_options/1]).

:- doc(hide,tcl_name/1).
:- doc(hide,creation_options/1).
%%---------------------------------------------------------------------
:- pred tcl_name(-Shape) :: atom 
	# "Specifies the name of the @var{Shape}. In this case is text.".
%%---------------------------------------------------------------------
tcl_name(text).

%%---------------------------------------------------------------------
:- pred creation_options(-OptionsList) :: list
	# "Creates a list with the options supported by the text.".
%%---------------------------------------------------------------------
creation_options([]).

%%---------------------------------------------------------------------
%% CONSTRUCTOR
%%---------------------------------------------------------------------

:- set_prolog_flag(multi_arity_warnings,off).

text_class.
text_class(Owner) :-
	shape_class(Owner).

text_class(X,Y,Owner) :-
	shape_class(Owner),
	point(X,Y).

text_class(X,Y) :-
	point(X,Y).

:- set_prolog_flag(multi_arity_warnings,on).
