%%---------------------------------------------------------------------
%%
%% RADIOBUTTON CLASS
%%
%%---------------------------------------------------------------------

:- class(radiobutton_class).

:- use_package(assertions).

:- doc(author, "Montserrat Urraca").


:- inherit_class(library('tcltk_obj/widget_class')).


:- data        variable/1.
:- inheritable variable/1.

variable('').

:- export(variable_value/1).

variable_value(Variable) :-
	atom(Variable),
        set_fact(variable(Variable)),
	notify_changes.

variable_value(Variable) :-
	variable(Variable).

%%---------------------------------------------------------------------
%% IMPLEMENTATION
%%---------------------------------------------------------------------

:- export([tcl_name/1,creation_options/1]).

tcl_name(radiobutton).

%creation_options([' -command ',sqb(C)," "|Other]) :-
%creation_options([' -command ',C," "|Other]) :-
creation_options([' ',min(variable),V|Other]) :-
	variable(V),
	inherited creation_options(Other).

%%---------------------------------------------------------------------
%% CONSTRUCTOR
%%---------------------------------------------------------------------

:- set_prolog_flag(multi_arity_warnings,off).

radiobutton_class.
radiobutton_class(Owner) :-
	radiobutton_class(Owner).

:- set_prolog_flag(multi_arity_warnings,on).
