%%---------------------------------------------------------------------
%%
%% MENU ENTRY CLASS
%%
%%---------------------------------------------------------------------

:- module(menu_entry_class_doc,[],[objects,assertions,isomodes,regtypes]).

:- doc(author, "Montserrat Urraca").



:- use_class(library('tcltk_obj/menu_class')).
:- use_module(library(lists), [append/3]).

%:- data        name/1.
%:- inheritable name/1.

name('').

:- export(set_name/1).

%%------------------------------------------------------------------------
:- pred set_name(+Name) :: atom
	# "@var{Name} of the menubutton associated.".
%%------------------------------------------------------------------------
set_name(_).

%:- data        action/1.
%:- inheritable action/1.

action('').

:- export(set_action/1).

%%------------------------------------------------------------------------
:- pred set_action(+Predicate) :: atom
	# "Specifies @var{Predicate} asociated to the menu entry.".
%%------------------------------------------------------------------------
set_action(_).

%:- data        label/1.
%:- inheritable label/1.

label('default').

:- export(label_value/1).

%%------------------------------------------------------------------------
:- pred label_value(+Value) :: atom
	# "@var{Value} specifies a value to be displayed as an identifying label in the menu entry.".
%%------------------------------------------------------------------------
%%------------------------------------------------------------------------
:- pred label_value(-Value) :: atom
	# "Gets the string which identify label in the menu entry.".
%%------------------------------------------------------------------------
label_value(_).

%:- data        menu/1.
%:- inheritable menu/1.

menu('default').

:- export(menu_name/1).

%%------------------------------------------------------------------------
:- pred menu_name(+Menu) :: atom
	# "@var{Menu} posted when cascade entry is invoked.".
%%------------------------------------------------------------------------
%%------------------------------------------------------------------------
:- pred menu_name(-Menu) :: atom
	# "Gets the @var{Menu} asociated to the cascade entry.".
%%------------------------------------------------------------------------
menu_name(_).

list_name([],[]).

list_name([Des|Next],[write(X),write(Des)|Next1]) :-
	X='.',
	list_name(Next,Next1).


%%---------------------------------------------------------------------
%% IMPLEMENTATION
%%---------------------------------------------------------------------

:- export([tcl_name/1,creation_options/1,creation_options_delete/1]).

:- doc(hide,'tcl_name'/1).
tcl_name('').

:- doc(hide,creation_options/1).
%%---------------------------------------------------------------------
:- pred creation_options(-OptionsList) :: list
	# "Creates a list with the options supported by the menu entry.".
%%---------------------------------------------------------------------
%creation_options([write(X),write(W),write(X),write(MB),write(X),write(N),' add cascade',min(label),L,min(command),br(C),min(menu),write(X),write(W),write(X),write(MB),write(X),write(N),write(X),M]) :-
creation_options(Opts) :-
	name(N),
	list_name(N,NL),
	append(NL,[' add command'],Opts0),
%	X='.',
	label(L),
	append(Opts0,[min(label)],Opts1),
	append(Opts1,[L],Opts2),
	action(C),
	append(Opts2,[min(command)],Opts3),
	append(Opts3,[br(C)],Opts).
%	menu(M),
%	display(M),nl,
%	append(N,[M],NM),
%	list_name(NM,LN),
%	append(Opts4,[min(menu)],Opts5),
%	append(Opts5,LN,Opts),
%	display(Opts),nl.


:- doc(hide,creation_options_delete/1).
%%---------------------------------------------------------------------
:- pred creation_options_delete(-OptionsList) :: list
	# "Creates a list with the options to delete a menu entry.".
%%---------------------------------------------------------------------
creation_options_delete([write(X),write(W),write(X),write(MB),write(X),write(N),' delete ','1']) :-
	name([W,MB,N]),
	X='.'.

%:- inheritable(notify_changes/0).

notify_changes:-
	display('En el notify changes menu'),nl,
	self(Menuentry),
	owner(AnOwner),
	AnOwner instance_of menu_class,
	AnOwner:menu_entry_changed(Menuentry),
	fail.

notify_changes.

:- export([add_owner/1,remove_owner/1]).

:- doc(hide,'add_owner'/1).
:- doc(hide,'remove_owner'/1).

add_owner(Owner) :-
	\+ owner(Owner),
	Owner instance_of menu_class,
	assertz_fact(owner(Owner)),
	self(Menuentry),
	Owner:add_menu_entry(Menuentry),
	!.
add_owner(_).


remove_owner(Owner) :-
	retract_fact(owner(Owner)),
	Owner instance_of menu_class,
	self(Menuentry),
	Owner:remove_menu_entry(Menuentry),
	!.

remove_owner(_).



%%---------------------------------------------------------------------
%% CONSTRUCTOR
%%---------------------------------------------------------------------
%:- data        owner/1.
%:- inheritable owner/1.

:- doc(hide,'class$call'/3).
:- doc(hide,'class$used'/2).

:- set_prolog_flag(multi_arity_warnings,off).

menu_entry_class.  % Not owned

menu_entry_class([]) :- !.

:- set_prolog_flag(multi_arity_warnings,on).
