%%---------------------------------------------------------------------
%%
%% WIDGET CLASS
%%
%%---------------------------------------------------------------------

:- module(widget_class_doc,[],[objects,assertions,isomodes,regtypes]).

:- doc(author, "Montserrat Urraca").


%% :- use_class(library('tcltk_obj/window_class')).



%%---------------------------------------------------------------------
%% TEXT
%%---------------------------------------------------------------------

%:- data        text/1.
%%:- inheritable text/1.

text('').

:- export(text_characters/1).

%%------------------------------------------------------------------------
:- pred text_characters(+Text) :: atom
	# "Indicates the @var{Text} to be displayed in the widget.".
%%------------------------------------------------------------------------
%%------------------------------------------------------------------------
:- pred text_characters(-Text) :: atom
	# "@var{Text} which is displayed in the widget.".
%%------------------------------------------------------------------------
text_characters(_).

%%---------------------------------------------------------------------
%% FONT
%%---------------------------------------------------------------------

%:- data        font/1.
%:- inheritable font/1.

font('normal').

:- export(font_type/1).

%%------------------------------------------------------------------------
:- pred font_type(+Font) :: atom
	# "Indicates the @var{Font} of the widget's text.".
%%------------------------------------------------------------------------
%%------------------------------------------------------------------------
:- pred font_type(-Font) :: atom
	# "Gets the @var{Font} of the widget's text.".
%%------------------------------------------------------------------------
font_type(_).

%%---------------------------------------------------------------------
%% BACKGROUND
%%---------------------------------------------------------------------

%:- data        background/1.
%:- inheritable background/1.

background('gray').

:- export(background_color/1).

%%------------------------------------------------------------------------
:- pred background_color(+Background) :: atom
	# "Indicates the @var{Background} color. Default to gray.".
%%------------------------------------------------------------------------
%%------------------------------------------------------------------------
:- pred background_color(-Background) :: atom
	# "Returns the @var{Background} color.".
%%------------------------------------------------------------------------
background_color(_).

%%---------------------------------------------------------------------
%% BORDERWIDTH
%%---------------------------------------------------------------------

%:- data        borderwidth/1.
%:- inheritable borderwidth/1.

borderwidth(2).

:- export(borderwidth_value/1).

%%------------------------------------------------------------------------
:- pred borderwidth_value(+BorderWidth) :: num
	# "Indicates the width's border. Default to 2.".
%%------------------------------------------------------------------------
%%------------------------------------------------------------------------
:- pred borderwidth_value(-BorderWidth) :: atom
	# "Gets the width's border.".
%%------------------------------------------------------------------------
borderwidth_value(_).

%%---------------------------------------------------------------------
%% FOREGROUND
%%---------------------------------------------------------------------

%:- data        foreground/1.
%:- inheritable foreground/1.

foreground('black').

:- export(foreground_color/1).

%%------------------------------------------------------------------------
:- pred foreground_color(+Foreground) :: atom
	# "Indicates the @var{Foreground} color. Default to black".
%%------------------------------------------------------------------------
%%------------------------------------------------------------------------
:- pred foreground_color(-Foreground) :: atom
	# "Gets the @var{Foreground} color.".
%%------------------------------------------------------------------------
foreground_color(_).

%%---------------------------------------------------------------------
%% HIGHLIGHTBACKGROUND
%%---------------------------------------------------------------------

%:- data        highlightbackground/1.
%:- inheritable highlightbackground/1.

highlightbackground('white').

:- export(highlightbackground_color/1).

%%------------------------------------------------------------------------
:- pred highlightbackground_color(+Color) :: atom
	# "@var{Color} specifies the highlight background color. Default to white".
%%------------------------------------------------------------------------
%%------------------------------------------------------------------------
:- pred highlightbackground_color(-Color) :: atom
	# "Gets the @var{Color} of the highlight background.".
%%------------------------------------------------------------------------
highlightbackground_color(_).

%%---------------------------------------------------------------------
%% HIGHLIGHTBACKGROUND
%%---------------------------------------------------------------------

%:- data        highlightcolor/1.
%:- inheritable highlightcolor/1.

highlightcolor('white').

:- export(highlight_color/1).

%%------------------------------------------------------------------------
:- pred highlight_color(+Color) :: atom
	# "@var{Color} specifies the highlight color. Default to white".
%%------------------------------------------------------------------------
%%------------------------------------------------------------------------
:- pred highlight_color(-Color) :: atom
	# "Gets the @var{Color} of the highlight.".
%%------------------------------------------------------------------------
highlight_color(_).

%%---------------------------------------------------------------------
%% WIDTH
%%---------------------------------------------------------------------

%:- data        width/1.
%:- inheritable width/1.

width(0).

:- export(width_value/1).

%%------------------------------------------------------------------------
:- pred width_value(+Width) :: int
	# "Specifies the @var{Width} for the widget. Default to 0".
%%------------------------------------------------------------------------
%%------------------------------------------------------------------------
:- pred width_value(+Width) :: int
	# "Gets the @var{Width} specified for the widget.".
%%------------------------------------------------------------------------
width_value(_).

%%---------------------------------------------------------------------
%% RELIEF
%%---------------------------------------------------------------------

%:- data        relief/1.
%:- inheritable relief/1.

relief('sunken').

:- export(relief_type/1).

%%------------------------------------------------------------------------
:- pred relief_type(+Relief) :: atom
	# "Specifies a desired @var{Relief} for the widget. Default to sunken".
%%------------------------------------------------------------------------
%%------------------------------------------------------------------------
:- pred relief_type(-Relief) :: atom
	# "Gets the @var{Relief} of the widget.".
%%------------------------------------------------------------------------
relief_type(_).

%%---------------------------------------------------------------------
%% SIDE
%%---------------------------------------------------------------------

%:- data        side/1.
%:- inheritable side/1.

side('top').

:- export(side_type/1).

%%---------------------------------------------------------------------
:- pred side_type(+Side) :: atom
	# "Specifies which @var{Side} of the master, the slave(s) will be packed against. Must be left, right, top or bottom. Defaults to top".
%%---------------------------------------------------------------------
%%---------------------------------------------------------------------
:- pred side_type(-Side) :: atom
	# "Gets the position of the canvas.".
%%---------------------------------------------------------------------
side_type(_).

%%---------------------------------------------------------------------
%% EXPAND
%%---------------------------------------------------------------------

%:- data        expand/1.
%:- inheritable expand/1.

expand(0).

:- export(expand_value/1).

%%---------------------------------------------------------------------
:- pred expand_value(+Value) :: int
	# "Specifies whether the slaves should be expanded to consume extra space in their master. @var{Value} may have any proper boolean value, such as 1 or 0. Defaults to 0".
%%---------------------------------------------------------------------
%%---------------------------------------------------------------------
:- pred expand_value(-Value) :: int
	# "Gets the boolean value which indicates if the slaves should be expanded or no.".
%%---------------------------------------------------------------------
expand_value(_).

%%---------------------------------------------------------------------
%% FILL
%%---------------------------------------------------------------------

%:- data        fill/1.
%:- inheritable fill/1.

fill('none').

:- export(fill_type/1).

%%---------------------------------------------------------------------
:- pred fill_type(+Option) :: atom
	# "If a slave's parcel is larger than its requested dimensions, this option may be used to stretch the slave. @var{Option} must have one of the following values: none ( this is the default), x, y, both".
%%---------------------------------------------------------------------
%%---------------------------------------------------------------------
:- pred fill_type(-Option) :: atom
	# "Gets the fill value of the canvas".
%%---------------------------------------------------------------------
fill_type(_).

%%---------------------------------------------------------------------
%% PADX
%%---------------------------------------------------------------------

%:- data        padx/1.
%:- inheritable padx/1.

padx(0).

:- export(padx_value/1).

%%---------------------------------------------------------------------
:- pred padx_value(+Amount) :: int
	# "@var{Amount} specifies how much horizontal external padding to leave on each side of the slave(s). Amount defaults to 0".
%%---------------------------------------------------------------------
%%---------------------------------------------------------------------
:- pred padx_value(-Amount) :: int
	# "Gets the @var{Amount} which specifies how much horizontal external padding to leave on each side of the slaves.".
%%---------------------------------------------------------------------
padx_value(_).

%%---------------------------------------------------------------------
%% PADY
%%---------------------------------------------------------------------

%:- data        pady/1.
%:- inheritable pady/1.

pady(2).

:- export(pady_value/1).

%%---------------------------------------------------------------------
:- pred pady_value(+Amount) :: int
	# "@var{Amount} specifies how much vertical external padding to leave on each side of the slave(s). Amount defaults to 0".
%%---------------------------------------------------------------------
%%---------------------------------------------------------------------
:- pred pady_value(-Amount) :: int
	# "Gets the @var{Amount} which specifies how much vertical external padding to leave on each side of the slaves.".
%%---------------------------------------------------------------------
pady_value(_).

%%---------------------------------------------------------------------
%% ROW
%%---------------------------------------------------------------------

%:- data        row/1.
%:- inheritable row/1.

row('0').

:- export(row_value/1).

%%---------------------------------------------------------------------
:- pred row_value(+Row) :: int
	# "Indicates the @var{Row} in which the widget should be allocated.".
%%---------------------------------------------------------------------
%%---------------------------------------------------------------------
:- pred row_value(-Row) :: int
	# "Gets the @var{Row} in which the widget is allocated.".
%%---------------------------------------------------------------------
row_value(_).

%:- data        rowspan/1.
%:- inheritable rowspan/1.

rowspan(1).

:- export(rowspan_value/1).

%%---------------------------------------------------------------------
:- pred rowspan_value(+Row) :: int
	# "Indicates the number of @var{Row} which are going to be occupied in the grid.".
%%---------------------------------------------------------------------
%%---------------------------------------------------------------------
:- pred rowspan_value(-Row) :: int
	# "Gets the number of @var{Row} which are occupied by the widget in the grid.".
%%---------------------------------------------------------------------
rowspan_value(_).

%%---------------------------------------------------------------------
%% COLUMN
%%---------------------------------------------------------------------

%:- data        column/1.
%:- inheritable column/1.

column('0').

:- export(column_value/1).

%%---------------------------------------------------------------------
:- pred column_value(+Column) :: int
	# "Indicates the @var{Column} in which the widget should be allocated.".
%%---------------------------------------------------------------------
%%---------------------------------------------------------------------
:- pred column_value(-Column) :: int
	# "Gets the @var{Column} in which the widget is allocated.".
%%---------------------------------------------------------------------
column_value(_).

%:- data        columnspan/1.
%:- inheritable columnspan/1.

columnspan(1).

:- export(columnspan_value/1).

%%---------------------------------------------------------------------
:- pred columnspan_value(+Column) :: int
	# "Indicates the number of @var{Column} which are going to be occupied in the grid.".
%%---------------------------------------------------------------------
%%---------------------------------------------------------------------
:- pred columnspan_value(-Column) :: int
	# "Gets the number of @var{Column} which are occupied by the widget in the grid.".
%%---------------------------------------------------------------------
columnspan_value(_).

%%---------------------------------------------------------------------
%% EVENT TYPE
%%---------------------------------------------------------------------

%:- data        event_type/1.
%:- inheritable event_type/1.

event_type('').

:- export(event_type_widget/1).

%%---------------------------------------------------------------------
:- pred event_type_widget(+EventType) :: atom
	# "The event @var{EventType} is going to be manage by the interface.".
%%---------------------------------------------------------------------
%%---------------------------------------------------------------------
:- pred event_type_widget(-EventType) :: atom
	# "Gets the event @var{EventType} which is going to be manage by the interface.".
%%---------------------------------------------------------------------
event_type_widget(_).

%%---------------------------------------------------------------------
%% ACTION
%%---------------------------------------------------------------------

%:- data        action/1.
%:- inheritable action/1.

action('').

:- export(action_widget/3).
:- export(action_widget/1).

%%---------------------------------------------------------------------
:- pred action_widget(+Term) :: atom
	# "@var{Term} is going to be associated to the action of the object indicated with the operacion @em{event_type_widget}.".
%%---------------------------------------------------------------------
%%---------------------------------------------------------------------
:- pred action_widget(-Term) :: atom
	# "@var{Term} is associated to the action of the object indicated with the operacion @em{event_type_widget}.".
%%---------------------------------------------------------------------
action_widget(_).
%%---------------------------------------------------------------------
:- pred action_widget(+Input, +Output, +Term) :: atom * atom * atom
	# "Executes @var{Term} with @var{Input} value and @var{Output} variable.".
%%---------------------------------------------------------------------
%%---------------------------------------------------------------------
:- pred action_widget(+Input,+Output,-Term) :: atom * atom * atom
	# "@var{Term} is associated to the action of the object indicated with the operacion @em{event_type_widget}.".
%%---------------------------------------------------------------------
action_widget(_,_,_).

%%---------------------------------------------------------------------
%% VARIABLES
%%---------------------------------------------------------------------

%:- data        variables/1.
%:- inheritable variables/1.

variables('').
:- doc(hide,set_variables/1).

:- export(set_variables/1).

set_variables(Variable) :-
%	display('Variables'),nl,
	atom(Variable),
	atom_concat('$',Variable,Variable1),
%	display(Variable1),nl,
	set_fact(variables([set,Variable,Variable1])),
	notify_changes.


%%---------------------------------------------------------------------
%% IMPLEMENTATION
%%---------------------------------------------------------------------


:- export([tcl_name/1,creation_options/1,creation_position/1,creation_position_grid/1,creation_bind/1]).

:- doc(hide,tcl_name/1).
%%---------------------------------------------------------------------
:- pred tcl_name(-Name) :: atom
	# "@var{Name} is the command to create widget.".
%%---------------------------------------------------------------------
tcl_name(_) :- fail.

%%---------------------------------------------------------------------
:- pred creation_options(-OptionsList) :: list
	# "Creates a list with the options supported by the widget.".
%%---------------------------------------------------------------------
creation_options([min(text),dq(T),min(font),dq(F),min(background),B,min(borderwidth),O,min(foreground),G,min(width),W,min(highlightbackground),HB,min(highlightcolor),HC,min(relief),R]) :-
	%self(ID), 
	%display(ID),nl,
	text(T),
	font(F),
	background(B),
	borderwidth(O),
	foreground(G),
	width(W),
	highlightbackground(HB),
	highlightcolor(HC),
	relief(R).


%%---------------------------------------------------------------------
:- pred creation_position(-OptionsList) :: list
	# "Creates a list with the options supported by the pack command.".
%%---------------------------------------------------------------------
creation_position([' ',min(side),S,min(expand),E,min(fill),F,min(padx),X,min(pady),Y]) :-
	side(S),
	expand(E),
	fill(F),
	padx(X),
	pady(Y).

%%---------------------------------------------------------------------
:- pred creation_position_grid(-OptionsList) :: list
	# "Creates a list with the options supported by the grid command.".
%%---------------------------------------------------------------------
creation_position_grid([' ',min(row),R,min(rowspan),S,min(column),C,min(columnspan),P]) :-
	row(R),
	rowspan(S),
	column(C),
	columnspan(P).


%%---------------------------------------------------------------------
:- pred creation_bind(-BindList) :: list
	# "Creates a list with the event to be manage and the action associated to this event.".
%%---------------------------------------------------------------------
creation_bind([' ',Eventtype,br(C)]) :-
	event_type(Eventtype),
%display('Antes variables'),nl,
%	variables(V),
%display(V),
	action(C).

%:- inheritable(notify_changes/0).

notify_changes:-
	display('En el notify_changes widget'),nl,
	self(Widget),
	owner(AnOwner),
	AnOwner instance_of window_class,
	AnOwner:item_changed(Widget),
	fail.
notify_changes.

:- export([add_owner/1,remove_owner/1]).
:- doc(hide,add_owner/1).
:- doc(hide,remove_owner/1).

add_owner(Owner) :-
	\+ owner(Owner),
	Owner instance_of window_class,
	assertz_fact(owner(Owner)),
	self(Widget),
	Owner:add_item(Widget),
	!.
add_owner(_).


remove_owner(Owner) :-
	retract_fact(owner(Owner)),
	Owner instance_of window_class,
	self(Widget),
	Owner:remove_item(Widget),
	!.

remove_owner(_).

%%---------------------------------------------------------------------
%% CONSTRUCTOR/DESTRUCTOR
%%---------------------------------------------------------------------

%:- data        owner/1.
%:- inheritable owner/1.

:- doc(hide,'class$call'/3).
:- doc(hide,'class$used'/2).

:- set_prolog_flag(multi_arity_warnings,off).

widget_class.  % Not owned

widget_class([]) :- !.

widget_class([AnOwner|Next]) :-
	add_owner(AnOwner),
	!,
	widget_class(Next).

widget_class(AnOwner) :-
	!,
	add_owner(AnOwner).

:- set_prolog_flag(multi_arity_warnings,on).

destructor :-
	self(Widget),
	retract_fact(owner(AnOwner)),
	AnOwner instance_of window_class,     % Owner is still alive
	AnOwner:remove_item(Widget),
	fail.
