%%---------------------------------------------------------------------
%%
%% BUTTON CLASS
%%
%%---------------------------------------------------------------------

:- module(button_class_doc,[],[objects,assertions,isomodes,regtypes]).

:- doc(author, "Montserrat Urraca").

%:- inherit_class(library('tcltk_obj/widget_class')).

%:- data        command/1.
%:- inheritable command/1.

command('').

:- export(command_button/1).

%%------------------------------------------------------------------------
:- pred command_button(+Command) :: atom
	# "Sets a Tcl @var{Command} to be associated with the button. This @var{Command} is typically invoked when mouse button 1 is released over the button window.".
%%------------------------------------------------------------------------
%%------------------------------------------------------------------------
:- pred command_button(-Command) :: atom
	# "Gets the Tcl @var{Command} associated with the button.".
%%------------------------------------------------------------------------
command_button(_).

%%---------------------------------------------------------------------
%% IMPLEMENTATION
%%---------------------------------------------------------------------

:- export([tcl_name/1,creation_options/1]).

:- doc(hide,tcl_name/1).
:- doc(hide,creation_options/1).
%%---------------------------------------------------------------------
:- pred tcl_name(-Widget) :: atom 
	# "Specifies the name of the @var{Widget}. In this case is button.".
%%---------------------------------------------------------------------
tcl_name(button).

%%---------------------------------------------------------------------
:- pred creation_options(-OptionsList) :: list
	# "Creates a list with the options supported by the button.".
%%---------------------------------------------------------------------
%creation_options([' -command ',sqb(C)," "|Other]) :-
%creation_options([' -command ',C," "|Other]) :-
creation_options([]).

%%---------------------------------------------------------------------
%% CONSTRUCTOR
%%---------------------------------------------------------------------
:- doc(hide,'class$call'/3).
:- doc(hide,'class$used'/2).

:- set_prolog_flag(multi_arity_warnings,off).

button_class.
button_class(Owner) :-
	button_class(Owner).

:- set_prolog_flag(multi_arity_warnings,on).
