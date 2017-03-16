%%---------------------------------------------------------------------
%%
%% BUTTON CLASS
%%
%%---------------------------------------------------------------------

:- class(button_class).
:- inherit_class(library('tcltk_obj/widget_class')).

:- use_package(assertions).

:- doc(author, "Montserrat Urraca").


:- data        command/1.
:- inheritable command/1.

command('').

:- export(command_button/1).

command_button(Command) :-
	nonvar(Command),
	!,
	atom(Command),
	set_fact(command([prolog_one_event,dq(write(execute(Command)))])),
	notify_changes.

command_button(Command) :-
	command(Command).

%%---------------------------------------------------------------------
%% IMPLEMENTATION
%%---------------------------------------------------------------------

:- export([tcl_name/1,creation_options/1]).

tcl_name(button).

%creation_options([' -command ',sqb(C)," "|Other]) :-
%creation_options([' -command ',C," "|Other]) :-
creation_options([' ',min(command),br(C)|Other]) :-
	self(ID), display(ID),nl,
	command(C),
	inherited creation_options(Other).

%%---------------------------------------------------------------------
%% CONSTRUCTOR
%%---------------------------------------------------------------------

:- set_prolog_flag(multi_arity_warnings,off).

button_class.
button_class(Owner) :-
	button_class(Owner).

:- set_prolog_flag(multi_arity_warnings,on).
