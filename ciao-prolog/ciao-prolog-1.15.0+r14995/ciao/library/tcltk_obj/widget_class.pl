%%---------------------------------------------------------------------
%%
%% WIDGET CLASS
%%
%%---------------------------------------------------------------------

:- class(widget_class,[],[objects,assertions]).

:- doc(author, "Montserrat Urraca").


:- use_class(library('tcltk_obj/window_class')).


%%---------------------------------------------------------------------
%% TEXT
%%---------------------------------------------------------------------

:- data        text/1.
:- inheritable text/1.

text('').

:- export(text_characters/1).

text_characters(Text) :-
	nonvar(Text),
	!,
	atom(Text),
	set_fact(text(Text)),
	notify_changes.

text_characters(Text) :-
	text(Text).

%%---------------------------------------------------------------------
%% FONT
%%---------------------------------------------------------------------

:- data        font/1.
:- inheritable font/1.

font('normal').

:- export(font_type/1).

font_type(Font) :-
	nonvar(Font),
	!,
	atom(Font),
	set_fact(font(Font)),
	notify_changes.

font_type(Font) :-
	font(Font).

%%---------------------------------------------------------------------
%% BACKGROUND
%%---------------------------------------------------------------------

:- data        background/1.
:- inheritable background/1.

background('gray').

:- export(background_color/1).

background_color(Bg) :-
	nonvar(Bg),
	!,
	atom(Bg),
	set_fact(background(Bg)),
	notify_changes.

background_color(Bg) :-
	background(Bg).

%%---------------------------------------------------------------------
%% BORDERWIDTH
%%---------------------------------------------------------------------

:- data        borderwidth/1.
:- inheritable borderwidth/1.

borderwidth(2).

:- export(borderwidth_value/1).

borderwidth_value(Bw) :-
	nonvar(Bw),
	!,
	num(Bw),
	set_fact(borderwidth(Bw)),
	notify_changes.

borderwidth_value(Bw) :-
	borderwidth(Bw).

%%---------------------------------------------------------------------
%% FOREGROUND
%%---------------------------------------------------------------------

:- data        foreground/1.
:- inheritable foreground/1.

foreground('black').

:- export(foreground_color/1).

foreground_color(Fg) :-
	nonvar(Fg),
	!,
	atom(Fg),
	set_fact(foreground(Fg)),
	notify_changes.

foreground_color(Fg) :-
	foreground(Fg).

%%---------------------------------------------------------------------
%% HIGHLIGHTBACKGROUND
%%---------------------------------------------------------------------

:- data        highlightbackground/1.
:- inheritable highlightbackground/1.

highlightbackground('white').

:- export(highlightbackground_color/1).

highlightbackground_color(Highlightb) :-
	nonvar(Highlightb),
	!,
	atom(Highlightb),
	set_fact(highlightbackground(Highlightb)),
	notify_changes.

highlightbackground_color(Highlightb) :-
	highlightbackground(Highlightb).

%%---------------------------------------------------------------------
%% HIGHLIGHTBACKGROUND
%%---------------------------------------------------------------------

:- data        highlightcolor/1.
:- inheritable highlightcolor/1.

highlightcolor('white').

:- export(highlight_color/1).

highlight_color(Highlightb) :-
	nonvar(Highlightb),
	!,
	atom(Highlightb),
	set_fact(highlightcolor(Highlightb)),
	notify_changes.

highlight_color(Highlightb) :-
	highlightcolor(Highlightb).

%%---------------------------------------------------------------------
%% WIDTH
%%---------------------------------------------------------------------

:- data        width/1.
:- inheritable width/1.

width(0).

:- export(width_value/1).

width_value(Width) :-
	nonvar(Width),
	!,
	num(Width),
	set_fact(width(Width)),
	notify_changes.

width_value(Width) :-
	width(Width).

%%---------------------------------------------------------------------
%% RELIEF
%%---------------------------------------------------------------------

:- data        relief/1.
:- inheritable relief/1.

relief('sunken').

:- export(relief_type/1).

relief_type(Relief) :-
	nonvar(Relief),
	!,
	atom(Relief),
	set_fact(relief(Relief)),
	notify_changes.

relief_type(Relief) :-
	relief(Relief).

%%---------------------------------------------------------------------
%% SIDE
%%---------------------------------------------------------------------

:- data        side/1.
:- inheritable side/1.

side('top').

:- export(side_type/1).

side_type(Side) :-
	nonvar(Side),
	!,
	atom(Side),
	set_fact(side(Side)),
	notify_changes.

side_type(Side) :-
	side(Side).

%%---------------------------------------------------------------------
%% EXPAND
%%---------------------------------------------------------------------

:- data        expand/1.
:- inheritable expand/1.

expand(0).

:- export(expand_value/1).

expand_value(Expand) :-
	nonvar(Expand),
	!,
	num(Expand),
	set_fact(expand(Expand)),
	notify_changes.

expand_value(Expand) :-
	expand(Expand).

%%---------------------------------------------------------------------
%% FILL
%%---------------------------------------------------------------------

:- data        fill/1.
:- inheritable fill/1.

fill('none').

:- export(fill_type/1).

fill_type(Fill) :-
	nonvar(Fill),
	!,
	atom(Fill),
	set_fact(fill(Fill)),
	notify_changes.

fill_type(Fill) :-
	fill(Fill).

%%---------------------------------------------------------------------
%% PADX
%%---------------------------------------------------------------------

:- data        padx/1.
:- inheritable padx/1.

padx(0).

:- export(padx_value/1).

padx_value(Padx) :-
	nonvar(Padx),
	!,
	num(Padx),
	set_fact(padx(Padx)),
	notify_changes.

padx_value(Padx) :-
	padx(Padx).

%%---------------------------------------------------------------------
%% PADY
%%---------------------------------------------------------------------

:- data        pady/1.
:- inheritable pady/1.

pady(2).

:- export(pady_value/1).

pady_value(Pady) :-
	nonvar(Pady),
	!,
	num(Pady),
	set_fact(pady(Pady)),
	notify_changes.

pady_value(Pady) :-
	pady(Pady).

%%---------------------------------------------------------------------
%% ROW
%%---------------------------------------------------------------------

:- data        row/1.
:- inheritable row/1.

row(0).

:- export(row_value/1).

row_value(Row) :-
	nonvar(Row),
	!,
	num(Row),
	set_fact(row(Row)),
	notify_changes.

row_value(Row) :-
	row(Row).

:- data        rowspan/1.
:- inheritable rowspan/1.

rowspan(1).

:- export(rowspan_value/1).

rowspan_value(Rowspan) :-
	nonvar(Rowspan),
	!,
	num(Rowspan),
	set_fact(rowspan(Rowspan)),
	notify_changes.

rowspan_value(Rowspan) :-
	rowspan(Rowspan).

%%---------------------------------------------------------------------
%% COLUMN
%%---------------------------------------------------------------------

:- data        column/1.
:- inheritable column/1.

column(0).

:- export(column_value/1).

column_value(Column) :-
	nonvar(Column),
	!,
	num(Column),
	set_fact(column(Column)),
	notify_changes.

column_value(Column) :-
	column(Column).

:- data        columnspan/1.
:- inheritable columnspan/1.

columnspan(1).

:- export(columnspan_value/1).

columnspan_widget(Columnspan) :-
	nonvar(Columnspan),
	!,
	num(Columnspan),
	set_fact(columnspan(Columnspan)),
	notify_changes.

columnspan_value(Columnspan) :-
	columnspan(Columnspan).

%%---------------------------------------------------------------------
%% EVENT TYPE
%%---------------------------------------------------------------------

:- data        event_type/1.
:- inheritable event_type/1.

event_type('').

:- export(event_type_widget/1).

event_type_widget(EventT) :-
	nonvar(EventT),
	!,
	atom(EventT),
	set_fact(event_type(EventT)),
	notify_changes.

event_type_widget(EventT) :-
	event_type(EventT).

%%---------------------------------------------------------------------
%% ACTION
%%---------------------------------------------------------------------

:- data        action/1.
:- inheritable action/1.

action('').

:- export(action_widget/3).
:- export(action_widget/1).

action_widget(Predicate) :-
	nonvar(Predicate),
	!,
	atom(Predicate),
	set_fact(action([prolog1,dq(write(execute(Predicate)))])),
	notify_changes.

action_widget(Predicate) :-
	action(Predicate).

action_widget(Input,Output,Predicate) :-
	nonvar(Predicate),
	!,
	atom(Predicate),
	atom_concat('$',Input,Input1),
	atom_concat('$prolog_variables(',Output,Output1),
	atom_concat(Output1,')',Output2),
	set_fact(action([set,Input,Input1,'\n',
	                    prolog_one_event,dq(write(execute(Predicate))),'\n',
                            set,Output,Output2])),
%	set_fact(predicate([prolog_one_event,dq(write(execute(Predicate)))])),
	notify_changes.

action_widget(_,_,Predicate) :-
	action(Predicate).

%%---------------------------------------------------------------------
%% VARIABLES
%%---------------------------------------------------------------------

:- data        variables/1.
:- inheritable variables/1.

variables('').

:- export(set_variables/1).

set_variables(Variable) :-
	atom(Variable),
	atom_concat('$',Variable,Variable1),
	set_fact(variables([set,Variable,Variable1])),
	notify_changes.


%%---------------------------------------------------------------------
%% IMPLEMENTATION
%%---------------------------------------------------------------------


:- export([tcl_name/1,creation_options/1,creation_position/1,creation_position_grid/1,creation_bind/1]).

tcl_name(_) :- fail.

creation_options([min(text),dq(T),min(font),dq(F),min(background),B,min(borderwidth),O,min(foreground),G,min(width),W,min(highlightbackground),HB,min(highlightcolor),HC,min(relief),R]) :-
	%self(ID), 
	text(T),
	font(F),
	background(B),
	borderwidth(O),
	foreground(G),
	width(W),
	highlightbackground(HB),
	highlightcolor(HC),
	relief(R).
	


creation_position([' ',min(side),S,min(expand),E,min(fill),F,min(padx),X,min(pady),Y]) :-
	side(S),
	expand(E),
	fill(F),
	padx(X),
	pady(Y).

creation_position_grid([' ',min(row),R,min(rowspan),S,min(column),C,min(columnspan),P]) :-
	row(R),
	rowspan(S),
	column(C),
	columnspan(P).


creation_bind([' ',Eventtype,br(C)]) :-
	event_type(Eventtype),
%	variables(V),
	action(C).

:- inheritable(notify_changes/0).

notify_changes:-
	self(Widget),
	owner(AnOwner),
	AnOwner instance_of window_class,
	AnOwner:item_changed(Widget),
	fail.
notify_changes.

:- export([add_owner/1,remove_owner/1]).

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

:- data        owner/1.
:- inheritable owner/1.


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
