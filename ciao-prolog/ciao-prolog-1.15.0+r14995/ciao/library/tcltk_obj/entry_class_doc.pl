%%---------------------------------------------------------------------
%%
%% ENTRY CLASS
%%
%%---------------------------------------------------------------------

:- module(entry_class_doc,[],[objects,assertions,isomodes,regtypes]).

:- doc(author, "Montserrat Urraca").

:- use_module(library(lists), [append/3]).

%:- inherit_class(library('tcltk_obj/widget_class')).

:- use_module(library(tcltk(examples(tk_test_aux)))).
:- use_module(library(tcltk)).

%:- data        textvariable/1.
%:- inheritable textvariable/1.

textvariable('aux').

:- export(textvariable_entry/1).

%%---------------------------------------------------------------------
:- pred textvariable_entry(+Variable) :: atom
	# "@var{Variable} specifies the name of the Tcl variable".
%%---------------------------------------------------------------------
%%---------------------------------------------------------------------
:- pred textvariable_entry(-Variable) :: atom
	# "Gets the name of the Tcl @var{Variable} associated to the entry".
%%---------------------------------------------------------------------
textvariable_entry(_).
:- export([textvariablevalue_string/1]).

%%---------------------------------------------------------------------
:- pred textvariablevalue_string(+Value) :: num
	# "Specifies the @var{Value} of the Tcl variable associated to the entry.".
%%---------------------------------------------------------------------
%%---------------------------------------------------------------------
:- pred textvariablevalue_string(-Value) :: num
	# "@var{Value} is the value of the Tcl variable associated to the entry.".
%%---------------------------------------------------------------------
textvariablevalue_string(_).

:- export([textvariablevalue_number/1]).

%%---------------------------------------------------------------------
:- pred textvariablevalue_number(+Value) :: num
	# "Specifies the @var{Value} of the Tcl variable associated to the entry.".
%%---------------------------------------------------------------------
%%---------------------------------------------------------------------
:- pred textvariablevalue_number(-Value) :: num
	# "@var{Value} is the value of the Tcl variable associated to the entry.".
%%---------------------------------------------------------------------
textvariablevalue_number(_).

%:- data        justify/1.
%:- inheritable justify/1.

justify('left').

:- export(justify_entry/1).

%%---------------------------------------------------------------------
:- pred justify_entry(+How) :: atom
	# "@var{How} specifies how to justify the text in the entry. @var{How} must be one of the values left, right or center.  This option defaluts to left.".
%%---------------------------------------------------------------------
%%---------------------------------------------------------------------
:- pred justify_entry(-How) :: atom
	# "Gets @var{How} is justified the text.".
%%---------------------------------------------------------------------
justify_entry(_).

%%---------------------------------------------------------------------
%% IMPLEMENTATION
%%---------------------------------------------------------------------

%:- export([tcl_name/1,creation_options/1,creation_bind/1]).
:- export([tcl_name/1,creation_options/1]).

:- doc(hide,tcl_name/1).
:- doc(hide,creation_options/1).
%%---------------------------------------------------------------------
:- pred tcl_name(-Widget) :: atom 
	# "Specifies the name of the @var{Widget}. In this case is entry.".
%%---------------------------------------------------------------------
tcl_name(entry).

%%---------------------------------------------------------------------
:- pred creation_options(-OptionsList) :: list
	# "Creates a list with the options supported by the entry.".
%%---------------------------------------------------------------------
creation_options([]).

%%---------------------------------------------------------------------
%% CONSTRUCTOR
%%---------------------------------------------------------------------
:- doc(hide,'class$call'/3).
:- doc(hide,'class$used'/2).

:- set_prolog_flag(multi_arity_warnings,off).

entry_class.
entry_class(Owner) :-
	entry_class(Owner).


:- set_prolog_flag(multi_arity_warnings,on).
