%%---------------------------------------------------------------------
%%
%% CHECKBUTTON CLASS
%%
%%---------------------------------------------------------------------

:- class(checkbutton_class).
:- inherit_class(library('tcltk_obj/widget_class')).

:- use_package(assertions).

:- doc(author, "Montserrat Urraca").


:- data        variable/1.
:- inheritable variable/1.

variable('').

:- export(variable_value/1).

variable_value(Variable) :-
	nonvar(Variable),
	!,
	atom(Variable),
        set_fact(variable(Variable)),
	notify_changes.

variable_value(Variable) :-
	variable(Variable).

%%---------------------------------------------------------------------
%% IMPLEMENTATION
%%---------------------------------------------------------------------

:- export([tcl_name/1,creation_options/1]).

tcl_name(checkbutton).

%creation_options([' -command ',sqb(C)," "|Other]) :-
%creation_options([' -command ',C," "|Other]) :-
creation_options([' ',min(variable),V|Other]) :-
	variable(V),
	inherited creation_options(Other).

%%---------------------------------------------------------------------
%% CONSTRUCTOR
%%---------------------------------------------------------------------

:- set_prolog_flag(multi_arity_warnings,off).

checkbutton_class.
checkbutton_class(Owner) :-
	checkbutton_class(Owner).

:- set_prolog_flag(multi_arity_warnings,on).
