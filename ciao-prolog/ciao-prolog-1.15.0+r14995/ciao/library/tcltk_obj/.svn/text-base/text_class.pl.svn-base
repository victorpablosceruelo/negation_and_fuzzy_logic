%%---------------------------------------------------------------------
%%
%% TEXT CLASS
%%
%%---------------------------------------------------------------------

:- class(text_class).

:- use_package(assertions).

:- doc(author, "Montserrat Urraca").


:- inherit_class(library('tcltk/examples/interface/shape_class')).
%:- implements(library('class/examples/geometry/mobile')).
%:- implements(library('class/examples/class/mobile')).

%%---------------------------------------------------------------------
%% COORDINATES
%%---------------------------------------------------------------------

:- data        coord/2.
:- inheritable coord/2.

coord(0,0).

:- export([point/2]).

point(X,Y) :-
	nonvar(X),
	!,
	nonvar(Y),
	!,
 	number(X),
 	number(Y),
	retract_fact(coord(_,_)),
	asserta_fact(coord(X,Y)),
	notify_changes.

point(X,Y) :-
	coord(X,Y).
%%---------------------------------------------------------------------
%%  TEXT 
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
%%  ANCHOR 
%%---------------------------------------------------------------------

:- data        anchorpos/1.
:- inheritable anchorpos/1.

anchorpos('center').

:- export(anchor/1).

anchor(Anchor) :-
	nonvar(Anchor),
	!,
	atom(Anchor),
	set_fact(anchorpos(Anchor)),
	notify_changes.

anchor(Anchor) :-
	anchorpos(Anchor).

%%---------------------------------------------------------------------
%%  FONT 
%%---------------------------------------------------------------------

:- data        font/1.
:- inheritable font/1.

font('arial').

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
%%  JUSTIFY 
%%---------------------------------------------------------------------

:- data        justify/1.
:- inheritable justify/1.

justify('left').

:- export(justify_text/1).

justify_text(Justify) :-
	nonvar(Justify),
	!,
	atom(Justify),
	set_fact(justify(Justify)),
	notify_changes.

justify_text(Justify) :-
	justify(Justify).

%%---------------------------------------------------------------------
%% IMPLEMENTATION
%%---------------------------------------------------------------------

:- export([tcl_name/1,creation_options/1]).

tcl_name(text).

creation_options([X,Y,min(text),T,min(anchor),A,min(font),F,min(justify),J|Other]) :-
	coord(X,Y),
	text(T),
	anchorpos(A),
	font(F),
	justify(J),
	inherited creation_options(Other).

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
