%%---------------------------------------------------------------------
%%
%% CHECKBUTTON CLASS
%%
%%---------------------------------------------------------------------

:- module(checkbutton_class_doc,[],[objects,assertions,isomodes,regtypes]).

:- doc(author, "Montserrat Urraca").


%:- inherit_class(library('tcltk_obj/widget_class')).


%:- data        variable/1.
%:- inheritable variable/1.

variable('').

:- export(variable_value/1).

%%------------------------------------------------------------------------
:- pred variable_value(+Variable) :: atom
	# "Sets the value of global @var{Variable} to indicate whether or not this button is selected.".
%%------------------------------------------------------------------------
%%------------------------------------------------------------------------
:- pred variable_value(-Variable) :: atom
	# "Gets the value of global @var{Variable} which indicates if the button is selected.".
%%------------------------------------------------------------------------
variable_value(_).

%%---------------------------------------------------------------------
%% IMPLEMENTATION
%%---------------------------------------------------------------------

:- export([tcl_name/1,creation_options/1]).

:- doc(hide,tcl_name/1).
:- doc(hide,creation_options/1).
%%---------------------------------------------------------------------
:- pred tcl_name(-Widget) :: atom 
	# "Specifies the name of the @var{Widget}. In this case is checkbutton.".
%%---------------------------------------------------------------------
tcl_name(checkbutton).

%%---------------------------------------------------------------------
:- pred creation_options(-OptionsList) :: list
	# "Creates a list with the options supported by the checkbutton.".
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

checkbutton_class.
checkbutton_class(Owner) :-
	checkbutton_class(Owner).

:- set_prolog_flag(multi_arity_warnings,on).
