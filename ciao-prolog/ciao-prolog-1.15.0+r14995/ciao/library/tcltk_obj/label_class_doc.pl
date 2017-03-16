%%---------------------------------------------------------------------
%%
%% LABEL CLASS
%%
%%---------------------------------------------------------------------

:- module(label_class_doc,[],[objects,assertions,isomodes,regtypes]).

:- doc(author, "Montserrat Urraca").


%:- inherit_class(library('tcltk_obj/widget_class')).

%:- data        textvariable/1.
%:- inheritable textvariable/1.

textvariable('').

:- export(textvariable_label/1).

%%---------------------------------------------------------------------
:- pred set_textvariable_label(+Variable) :: atom
	# "@var{Variable} specifies name of the Tcl variable".
%%---------------------------------------------------------------------
%%---------------------------------------------------------------------
:- pred get_textvariable(-Value) :: num
	# "@var{Value} is the value of the Tcl variable associated to the label.".
%%---------------------------------------------------------------------
textvariable_label(_).

%%---------------------------------------------------------------------
%% IMPLEMENTATION
%%---------------------------------------------------------------------

:- export([tcl_name/1,creation_options/1]).

:- doc(hide,tcl_name/1).
:- doc(hide,creation_options/1).
%%---------------------------------------------------------------------
:- pred tcl_name(-Widget) :: atom 
	# "Specifies the name of the @var{Widget}. In this case is label.".
%%---------------------------------------------------------------------
tcl_name(label).

%%---------------------------------------------------------------------
:- pred creation_options(-OptionsList) :: list
	# "Creates a list with the options supported by the label.".
%%---------------------------------------------------------------------
creation_options([]).

%%---------------------------------------------------------------------
%% CONSTRUCTOR
%%---------------------------------------------------------------------
:- doc(hide,'class$call'/3).
:- doc(hide,'class$used'/2).

:- set_prolog_flag(multi_arity_warnings,off).

label_class.
label_class(Owner) :-
	label_class(Owner).


:- set_prolog_flag(multi_arity_warnings,on).
