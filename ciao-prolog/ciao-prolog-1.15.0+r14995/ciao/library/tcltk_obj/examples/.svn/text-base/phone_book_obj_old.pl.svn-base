:- module(phone_book_obj,[main/0,test/0],[objects]).

:- use_package(persdb).

:- use_module(library(tcltk(examples(phone_book)))).
:- use_module(library(lists)).

:- use_class(library('tcltk_obj/window_class')).
:- use_class(library('tcltk_obj/button_class')).
:- use_class(library('tcltk_obj/label_class')).
:- use_class(library('tcltk_obj/entry_class')).

:- data position/1.
:- data entry_name/1.
:- data entry_tele/1.

main:- test.

test :-
	Name new button_class,
	Store new button_class,
	Quit new button_class,

	Name:set_text('name recall'),
	Name:set_relief('flat'),
	Name:set_font('arial'),
	Name:set_event_type('<ButtonPress-1>'),
	Name:set_action('exit_tk_event_loop(phone_book_obj:name_recall)'),
	Store:set_text('store'),
	Store:set_relief('flat'),
	Store:set_font('arial'),
	Store:set_event_type('<ButtonPress-1>'),
	Store:set_action('exit_tk_event_loop(phone_book_obj:store)'),
	Quit:set_text('quit'),
	Quit:set_relief('flat'),
	Quit:set_font('arial'),
	Quit:set_event_type('<ButtonPress-1>'),
	Quit:set_action('exit_tk_event_loop'),

	Window1 new window_class([Name,Store,Quit],[],[]),
	Window1:show,

	Window1:event_loop. %,
%	display('++++++++++++En el destroy W1'),nl,
%	destroy Window1.


name_recall :-
	Lname new label_class,
	Ename new entry_class,
	Ltele new label_class,
	Etele new entry_class,
	Next new button_class,
	Back new button_class,
	Menu new button_class,
	Quit new button_class,

	Lname:set_text('Name: '),
	Lname:set_relief('flat'),
	Lname:set_font('arial'),
	Ltele:set_text('Telephone: '),
	Ltele:set_relief('flat'),
	Ltele:set_font('arial'),
	Ename:set_textvariable('ename'),
	Ename:set_font('arial'),
	Etele:set_textvariable('entele'),
	Etele:set_font('arial'),
	Next:set_text('next'),
	Next:set_relief('flat'),
	Next:set_font('arial'),
	Next:set_event_type('<ButtonPress-1>'),
%	Next:set_action('exit_tk_event_loop(phone_book_obj:next)'),
	Next:set_action('phone_book_obj:next'),
	Back:set_text('back'),
	Back:set_relief('flat'),
	Back:set_font('arial'),
	Back:set_event_type('<ButtonPress-1>'),
	Back:set_action('phone_book_obj:back'),
	Menu:set_text('menu'),
	Menu:set_relief('flat'),
	Menu:set_font('arial'),
	Menu:set_event_type('<ButtonPress-1>'),
	Menu:set_action('exit_tk_event_loop(phone_book_obj:test)'),
	Quit:set_text('quit'),
	Quit:set_relief('flat'),
	Quit:set_font('arial'),
	Quit:set_event_type('<ButtonPress-1>'),
	Quit:set_action('exit_tk_event_loop'),

	asserta_fact(position(0)),

	Window2 new window_class([Lname,Ename,Ltele,Etele,Next,Back,Menu,Quit],[],[]),
	asserta_fact(entry_name(Ename)),
	asserta_fact(entry_tele(Etele)),

	Window2:show,

	Window2:event_loop,

	retract_fact(entry_name(Ename)),
	retract_fact(entry_tele(Etele)).%,

%	destroy Window2.

phone_book_search_obj(N,T,P):-
	phone_book_search(Ln,Lt),
%	length(Ln,Q),
	nth(P,Ln,N),
	nth(P,Lt,T).
 
next :- 
	position(X),
	X1 is X + 1,
	retract_fact(position(_)),
	asserta_fact(position(X1)),
	show(X1).

back :- 
	position(X),
	X > 0,
	X1 is X - 1,
	retract_fact(position(_)),
	asserta_fact(position(X1)),
	show(X1).

show(X) :-
	phone_book_search_obj(N,T,X),
	entry_name(En),
	entry_tele(Et),
	En:set_textvariablevalue_string(N),
	Et:set_textvariablevalue_number(T).

store :-
	Lname new label_class,
	Ename new entry_class,
	Ltele new label_class,
	Etele new entry_class,
	Save new button_class,
	Menu new button_class,
	Quit new button_class,

	Lname:set_text('Name: '),
	Lname:set_relief('flat'),
	Lname:set_font('arial'),
	Ltele:set_text('Telephone: '),
	Ltele:set_relief('flat'),
	Ltele:set_font('arial'),
	Ename:set_textvariable('ename'),
	Ename:set_font('arial'),
	Etele:set_textvariable('entele'),
	Etele:set_font('arial'),
	Save:set_text('save'),
	Save:set_relief('flat'),
	Save:set_font('arial'),
	Save:set_event_type('<ButtonPress-1>'),
	Save:set_action('phone_book_obj:save'),
	Menu:set_text('menu'),
	Menu:set_relief('flat'),
	Menu:set_font('arial'),
	Menu:set_event_type('<ButtonPress-1>'),
	Menu:set_action('exit_tk_event_loop(phone_book_obj:test)'),
	Quit:set_text('quit'),
	Quit:set_relief('flat'),
	Quit:set_font('arial'),
	Quit:set_event_type('<ButtonPress-1>'),
	Quit:set_action('exit_tk_event_loop'),

	Window2 new window_class([Lname,Ename,Ltele,Etele,Save,Menu,Quit],[],[]),
	Window2:show,

	asserta_fact(entry_name(Ename)),
	asserta_fact(entry_tele(Etele)),

	Window2:event_loop.
%,
%	destroy Window2.

quit.

save :- 
	entry_name(En),
	entry_tele(Et),
	En:get_textvariablevalue_string(N),
	Et:get_textvariablevalue_number(T),
	phone_book_store(N,T).
