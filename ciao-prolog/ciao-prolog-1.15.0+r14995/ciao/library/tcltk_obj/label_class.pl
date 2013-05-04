%%---------------------------------------------------------------------
%%
%% LABEL CLASS
%%
%%---------------------------------------------------------------------

:- class(label_class).

:- use_package(assertions).

:- doc(author, "Montserrat Urraca").

:- inherit_class(library('tcltk_obj/widget_class')).

:- data        textvariable/1.
:- inheritable textvariable/1.

textvariable('').

:- export(textvariable_label/1).

textvariable_label(Textvariable) :-
	nonvar(Textvariable),
	!,
	atom(Textvariable),
	set_fact(textvariable(Textvariable)),
	notify_changes.

textvariable_label(Textvariable) :-
	textvariable(Textvariable).

%%---------------------------------------------------------------------
%% IMPLEMENTATION
%%---------------------------------------------------------------------

:- export([tcl_name/1,creation_options/1]).

tcl_name(label).

creation_options([''|Other]) :-
%	textvariable(T),
	inherited creation_options(Other).

%%---------------------------------------------------------------------
%% CONSTRUCTOR
%%---------------------------------------------------------------------

:- set_prolog_flag(multi_arity_warnings,off).

label_class.
label_class(Owner) :-
	label_class(Owner).


:- set_prolog_flag(multi_arity_warnings,on).
