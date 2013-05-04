%%---------------------------------------------------------------------
%%
%% MENU BUTTON CLASS
%%
%%---------------------------------------------------------------------

:- class(menubutton_class).
:- inherit_class(library('tcltk/examples/interface/widget_class')).

:- use_package(assertions).

:- doc(author, "Montserrat Urraca").


:- use_module(library(lists), [append/3]).

:- data        menu/1.
:- inheritable menu/1.

menu('default').

:- export(menu_name/1).

menu_name(Menu) :-
	nonvar(Menu),
	!,
	atom(Menu),
	set_fact(menu(Menu)),
	notify_changes.

menu_name(Menu) :-
	self(ID),
	owner(OW),
	append([OW],[ID],PP),
%	menu(Menu).
	menu(Menu_name),
	append(PP,[Menu_name],Menu).

%%---------------------------------------------------------------------
%% IMPLEMENTATION
%%---------------------------------------------------------------------

:- export([tcl_name/1,creation_options/1]).

tcl_name(menubutton).

creation_options([' ',min(menu),write(X),write(OW),write(X),write(ID),write(X),C|Other]) :-
	menu(C),
	X='.',
	self(ID),
	owner(OW),
	inherited creation_options(Other).

%%---------------------------------------------------------------------
%% CONSTRUCTOR
%%---------------------------------------------------------------------

:- set_prolog_flag(multi_arity_warnings,off).

menubutton_class.
menubutton_class(Owner) :-
	menubutton_class(Owner).

:- set_prolog_flag(multi_arity_warnings,on).
