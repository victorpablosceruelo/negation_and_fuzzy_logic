%%---------------------------------------------------------------------
%%
%% ENTRY CLASS
%%
%%---------------------------------------------------------------------

:- class(entry_class).

:- use_package(assertions).

:- doc(author, "Montserrat Urraca").


:- use_module(library(lists), [append/3]).

:- inherit_class(library('tcltk_obj/widget_class')).

:- use_module(library(tcltk(examples(tk_test_aux)))).
:- use_module(library(tcltk)).

:- data        textvariable/1.
:- inheritable textvariable/1.

textvariable('aux').

:- export(textvariable_entry/1).

textvariable_entry(Textvariable) :-
	nonvar(Textvariable),
	!,
	atom(Textvariable),
	set_fact(textvariable(Textvariable)),
	notify_changes.

textvariable_entry(Textvariable) :-
	textvariable(Textvariable).

:- export([textvariablevalue_string/1]).

%set_textvariablevalue(Textvariable,Y) :-
textvariablevalue_string(Y) :-
	nonvar(Y),
	!,
	textvariable(Textvariable),
	owner(OW),
	OW:interp(I),
	tcl_eval(I,[set,write(Textvariable),' ',write(Y)],_).

textvariablevalue_string(Y) :-
	textvariable(Textvariable_aux),
	atom_concat('$',Textvariable_aux,Textvariable),
	owner(OW),
	OW:interp(I),
	tcl_eval(I,[set,'aux',write(Textvariable)],Z),
	atom_codes(Y,Z).	

:- export([textvariablevalue_number/1]).

textvariablevalue_number(Y) :-
	nonvar(Y),
	!,
	textvariable(Textvariable),
	number_codes(Y,Y1),
	atom_codes(Y2,Y1),
	owner(OW),
	OW:interp(I),
	tcl_eval(I,[set,write(Textvariable),' ',write(Y2)],_).

%get_textvariablevalue(Textvariable,Y) :-
textvariablevalue_number(Y) :-
	textvariable(Textvariable_aux),
	atom_concat('$',Textvariable_aux,Textvariable),
	owner(OW),
	OW:interp(I),
	tcl_eval(I,[set,'aux',write(Textvariable)],Z),
	number_codes(Y,Z).	

:- data        justify/1.
:- inheritable justify/1.

justify('left').

:- export(justify_entry/1).

justify_entry(Side) :-
	nonvar(Side),
	!,
	atom(Side),
	set_fact(justify(Side)),
	notify_changes.

justify_entry(Side) :-
	justify(Side).

%%---------------------------------------------------------------------
%% IMPLEMENTATION
%%---------------------------------------------------------------------

%:- export([tcl_name/1,creation_options/1,creation_bind/1]).
:- export([tcl_name/1,creation_options/1]).

tcl_name(entry).

%creation_options([' ',min(justify),S,min(textvariable),T,''|Other]) :-
creation_options([' ',min(justify),S,''|Other1]) :-
	justify(S),
	textvariable(T),
	inherited creation_options(Other),
	append(Other,[min(textvariable)|write(T)],Other1).

%creation_bind([' ','<Any-Key>',br([prolog_one_event,dq(write(execute(widget3:anadir(inputval(7)))))])]) .


%%---------------------------------------------------------------------
%% CONSTRUCTOR
%%---------------------------------------------------------------------

:- set_prolog_flag(multi_arity_warnings,off).

entry_class.
entry_class(Owner) :-
	entry_class(Owner).


:- set_prolog_flag(multi_arity_warnings,on).
