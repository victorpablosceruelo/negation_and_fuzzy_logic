%%---------------------------------------------------------------------
%%
%% MENU BUTTON CLASS
%%
%%---------------------------------------------------------------------

:- module(menubutton_class_doc,[],[objects,assertions,isomodes,regtypes]).

%:- inherit_class(library('tcltk_obj/widget_class')).

:- doc(author, "Montserrat Urraca").



:- use_module(library(lists), [append/3]).

%:- data        menu/1.
%:- inheritable menu/1.

menu('default').

:- export(menu_name/1).

%%------------------------------------------------------------------------
:- pred menu_name(+Menu) :: atom
	# "@var{Menu} posted when menubutton is clicked.".
%%------------------------------------------------------------------------
%%------------------------------------------------------------------------
:- pred menu_name(-Menu) :: atom
	# "Gets the name of the @var{Menu} asociated to the menubutton.".
%%------------------------------------------------------------------------
menu_name(_).

%%---------------------------------------------------------------------
%% IMPLEMENTATION
%%---------------------------------------------------------------------

:- export([tcl_name/1,creation_options/1]).

:- doc(hide,tcl_name/1).
:- doc(hide,creation_options/1).
%%---------------------------------------------------------------------
:- pred tcl_name(-Widget) :: atom 
	# "Specifies the name of the @var{Widget}. In this case is menubutton.".
%%---------------------------------------------------------------------
tcl_name(menubutton).

%%---------------------------------------------------------------------
:- pred creation_options(-OptionsList) :: list
	# "It creates a list with the options supported by the menubutton.".
%%---------------------------------------------------------------------
creation_options([]).

%%---------------------------------------------------------------------
%% CONSTRUCTOR
%%---------------------------------------------------------------------

:- doc(hide,'class$call'/3).
:- doc(hide,'class$used'/2).

:- set_prolog_flag(multi_arity_warnings,off).

menubutton_class.
menubutton_class(Owner) :-
	menubutton_class(Owner).

:- set_prolog_flag(multi_arity_warnings,on).
