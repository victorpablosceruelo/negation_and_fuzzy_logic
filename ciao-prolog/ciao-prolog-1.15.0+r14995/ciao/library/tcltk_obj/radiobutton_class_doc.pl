%%---------------------------------------------------------------------
%%
%% RADIOBUTTON CLASS
%%
%%---------------------------------------------------------------------

:- module(radiobutton_class_doc,[],[objects,assertions,isomodes,regtypes]).

:- doc(author, "Montserrat Urraca").

%:- data        variable/1.
%:- inheritable variable/1.

variable('').

:- export(variable_value/1).

%%------------------------------------------------------------------------
:- pred variable_value(+Variable) :: atom
	# "Specifies the value of global @var{Variable} to set whenever this button is selected.".
%%------------------------------------------------------------------------
%%------------------------------------------------------------------------
:- pred variable_value(-Variable) :: atom
	# "Gets the value of global @var{Variable} which indicates if this button is selected.".
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
	# "Specifies the name of the @var{Widget}. In this case is radiobutton.".
%%---------------------------------------------------------------------
tcl_name(radiobutton).

%%---------------------------------------------------------------------
:- pred creation_options(-OptionsList) :: list
	# "Creates a list with the options supported by the radiobutton.".
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

radiobutton_class.
radiobutton_class(Owner) :-
	radiobutton_class(Owner).

:- set_prolog_flag(multi_arity_warnings,on).
