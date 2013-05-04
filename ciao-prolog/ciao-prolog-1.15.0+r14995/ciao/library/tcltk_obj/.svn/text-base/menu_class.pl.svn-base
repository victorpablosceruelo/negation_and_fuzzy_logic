%%---------------------------------------------------------------------
%%
%% MENU CLASS
%%
%%---------------------------------------------------------------------

:- class(menu_class,[],[objects]).

:- use_package(assertions).

:- doc(author, "Montserrat Urraca").


:- use_class(library('tcltk_obj/window_class')).
:- use_class(library('tcltk_obj/menu_entry_class')).

:- use_module(library(tcltk), [tcl_eval/3, tcl_new/1, tcl_delete/1, tk_event_loop/1]).
%:- use_module(library(tcltk(tcltk_low_level))).
:- use_module(library(lists), [append/3]).

:- data menuentry/2.

:- data        name/1.
:- inheritable name/1.

name('').

:- export(name_menu/1).

name_menu(Name) :-
%	atom(Name),
	set_fact(name(Name)),
	notify_changes.

:- data        menu/1.
:- inheritable menu/1.

menu('').

:- export(menu_data/1).

menu_data(Menu) :-
	nonvar(Menu),
	!,
	atom(Menu),
	set_fact(menu(Menu)),
	notify_changes.

menu_data(Menu) :-
	menu(Menu).

:- data        label/1.
:- inheritable label/1.

label('').

:- export(label_value/1).

label_value(Label) :-
	nonvar(Label),
	!,
	atom(Label),
	set_fact(label(Label)),
	notify_changes.

label_value(Label) :-
	label(Label).

:- data        tearoff/1.
:- inheritable tearoff/1.

tearoff('1').

:- export(tearoff_value/1).

tearoff_value(Tearoff) :-
	nonvar(Tearoff),
	!,
	atom(Tearoff),
	set_fact(tearoff(Tearoff)),
	notify_changes.

tearoff_value(Tearoff) :-
	tearoff(Tearoff).

list_name([],[]).

list_name([Des|Next],[write(X),write(Des)|Next1]) :-
	X='.',
	list_name(Next,Next1).

%%---------------------------------------------------------------------
%% IMPLEMENTATION
%%---------------------------------------------------------------------

:- export([tcl_name/1,creation_options/1,creation_options_entry/1,creation_menu_name/1]).

tcl_name('menu').

%creation_options([write(X),write(W),write(X),write(MB),write(X),write(N),' ',min(tearoff),T]) :-
creation_options(Opts) :-
	display('En el creation optitons'),nl,
%	name([W,MB,N]),
	name(P),
	display(P),nl,
	list_name(P,C),
	display(C),nl,
%	X='.',
%	name(N),
	tearoff(T),
	append(C,[' '],Opts0),
	append(Opts0,[min(tearoff)],Opts1),
	append(Opts1,[T],Opts),
	display('Opts'),nl.

creation_menu_name([write(X),write(W),write(X),write(MB),write(X),write(N)]) :-
	name([W,MB,N]),
	X='.'.


%creation_options([write('.'),write(Widget),write('.'),write(C),' add command',min(label),label(L)|Other]) :-
creation_options_entry([write(X),write(W),write(X),write(MB),write(X),write(N),' add command',min(label),label(L)]) :-
	name([W,MB,N]),
	X='.',
	label(L).
%	inherited creation_options(Other).

:- inheritable(notify_changes/0).

notify_changes:-
%	display('En el notify changes menu'),nl,
	self(Menu),
	owner(AnOwner),
	AnOwner instance_of window_class,
	AnOwner:menu_changed(Menu),
	fail.
notify_changes.


:- export([add_owner/1,remove_owner/1]).

add_owner(Owner) :-
	\+ owner(Owner),
	Owner instance_of window_class,
	assertz_fact(owner(Owner)),
	self(Menu),
	Owner:add_menu(Menu),
	!.
add_owner(_).


remove_owner(Owner) :-
	retract_fact(owner(Owner)),
	Owner instance_of window_class,
	self(Menu),
	Owner:remove_menu(Menu),
	!.

remove_owner(_).



%%---------------------------------------------------------------------
%% CONSTRUCTOR
%%---------------------------------------------------------------------
:- data        owner/1.
:- inheritable owner/1.

%D:- doc(hide,'class$call'/3).
%D:- doc(hide,'class$used'/2).
:- set_prolog_flag(multi_arity_warnings,off).

menu_class.  % Not owned

menu_class([]):- !.  

menu_class([Menuentry|Next]) :-
	( add_menu_entry(Menuentry) ; true ),
	!,
	menu_class(Next).

%menu_class([]) :- !.

%menu_class([AnOwner|Next]) :-
%	add_owner(AnOwner),
%	!,
%	menu_class(Next).

%menu_class(AnOwner) :-
%	!,
%	add_owner(AnOwner).

:- set_prolog_flag(multi_arity_warnings,on).
%%---------------------------------------------------------------------
%% SHOW / HIDE ENTIRE WIDGET
%%---------------------------------------------------------------------

:- export(show/0).

show :-
	display('En el show'),nl,
	menuentry(Menuentry,hidden),
%	display('__________w'),display(Menuentry),nl,
	show_menu_entry(Menuentry),
	fail.
show.

:- export(hide/0).

hide :-
	menuentry(Menuentry,shown),
	hide_menu_entry(Menuentry),
	fail.
hide.
%%---------------------------------------------------------------------
%% SHOW / HIDE SPECIFIC ITEMS
%%---------------------------------------------------------------------

:- export(show_menu_entry/1).

show_menu_entry(Menuentry) :-
	display('En el show_menu_entry 1'),nl,
%	menuentry(Menu,hidden),
	Menuentry instance_of menu_entry_class,
%	display('despues intace'),nl,
	owner(Ow),
	Menuentry:creation_options(Opts),
%	display(Opts),nl,
	Ow:interp(I),
%	display(I),nl,
	tcl_eval(I,Opts,_),
	retract_fact(menuentry(Menuentry,hidden)),
	asserta_fact(menuentry(Menuentry,shown)).

:- export(hide_menu_entry/1).

hide_menu_entry(Menuentry) :-
	retract_fact(menuentry(Menuentry,shown)),
	!,
	Menuentry instance_of menu_entry_class,
	owner(Ow),
	Ow:interp(I),
	Menuentry:creation_options_delete(Opts),
	tcl_eval(I,Opts,_),
	asserta_fact(menuentry(Menuentry,hidden)).

%%---------------------------------------------------------------------
%% ADD/REMOVE ITEMS
%%---------------------------------------------------------------------

:- export(add_menu_entry/1).
:- export(remove_menu_entry/1).
:- export(menu_entry_changed/1).

add_menu_entry(Menuentry) :-
	\+ menuentry(Menuentry,_),
	Menuentry instance_of menu_entry_class,
	assertz_fact(menuentry(Menuentry,hidden)),
	self(Principal),
	Menuentry:add_owner(Principal),
	!.
add_menu_entry(_).

remove_menu_entry(Menuentry) :-
	hide_menu_entry(Menuentry),
	retract_fact(menuentry(Menuentry,_)),
	Menuentry instance_of menu_entry_class,
	self(Principal),
	Menuentry:remove_owner(Principal),
	!.
remove_menu_entry(_).


menu_entry_changed(Menuentry) :-
	display('En el item_changed'),nl,
	hide_menu_entry(Menuentry),
	show_menu_entry(Menuentry).
