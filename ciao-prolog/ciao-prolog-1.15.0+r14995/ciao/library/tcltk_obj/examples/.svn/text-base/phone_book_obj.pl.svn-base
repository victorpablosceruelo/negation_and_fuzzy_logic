:- module(phone_book_obj,[main/0,test/0],[objects]).

:- use_module(library(tcltk(examples(phone_book)))).
:- use_module(library(lists)).

:- use_class(library('tcltk_obj/window_class')).
:- use_class(library('tcltk_obj/button_class')).
:- use_class(library('tcltk_obj/label_class')).
:- use_class(library('tcltk_obj/entry_class')).

:- data position/1.
:- data entry_name/1.
:- data entry_tele/1.

main :- test.

test :-
	Name new button_class,
	Store new button_class,
	Quit new button_class,

	Name:text_characters('name recall'),
	Name:relief_type('flat'),
	Name:font_type('arial'),
	Name:event_type_widget('<ButtonPress-1>'),
	Name:action_widget('exit_tk_event_loop(phone_book_obj:name_recall)'),
	Store:text_characters('store'),
	Store:relief_type('flat'),
	Store:font_type('arial'),
	Store:event_type_widget('<ButtonPress-1>'),
	Store:action_widget('exit_tk_event_loop(phone_book_obj:store)'),
	Quit:text_characters('quit'),
	Quit:relief_type('flat'),
	Quit:font_type('arial'),
	Quit:event_type_widget('<ButtonPress-1>'),
	Quit:action_widget('exit_tk_event_loop'),

	Window1 new window_class([Name,Store,Quit],[],[]),
	Window1:show,
%	display('++++++++++++En el destroy W1'),nl,
	Window1:title('Phone-Book'),

	Window1:event_loop.%,
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

	Lname:text_characters('Name: '),
	Lname:relief_type('flat'),
	Lname:font_type('arial'),
	Ltele:text_characters('Telephone: '),
	Ltele:relief_type('flat'),
	Ltele:font_type('arial'),
	Ename:textvariable_entry('ename'),
	Ename:font_type('arial'),
	Etele:textvariable_entry('entele'),
	Etele:font_type('arial'),
	Next:text_characters('next'),
	Next:relief_type('flat'),
	Next:font_type('arial'),
	Next:event_type_widget('<ButtonPress-1>'),
%	Next:set_action_widget('exit_tk_event_loop(phone_book_obj:next)'),
	Next:action_widget('phone_book_obj:next'),
	Back:text_characters('back'),
	Back:relief_type('flat'),
	Back:font_type('arial'),
	Back:event_type_widget('<ButtonPress-1>'),
	Back:action_widget('phone_book_obj:back'),
	Menu:text_characters('menu'),
	Menu:relief_type('flat'),
	Menu:font_type('arial'),
	Menu:event_type_widget('<ButtonPress-1>'),
	Menu:action_widget('exit_tk_event_loop(phone_book_obj:test)'),
	Quit:text_characters('quit'),
	Quit:relief_type('flat'),
	Quit:font_type('arial'),
	Quit:event_type_widget('<ButtonPress-1>'),
	Quit:action_widget('exit_tk_event_loop'),

	asserta_fact(position(0)),

	Window2 new window_class([Lname,Ename,Ltele,Etele,Next,Back,Menu,Quit],[],[]),
	asserta_fact(entry_name(Ename)),
	asserta_fact(entry_tele(Etele)),

	Window2:show,
	Window2:title('Name-Recall'),

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
	En:textvariablevalue_string(N),
	Et:textvariablevalue_number(T).

store :-
	Lname new label_class,
	Ename new entry_class,
	Ltele new label_class,
	Etele new entry_class,
	Save new button_class,
	Menu new button_class,
	Quit new button_class,

	Lname:text_characters('Name: '),
	Lname:relief_type('flat'),
	Lname:font_type('arial'),
	Ltele:text_characters('Telephone: '),
	Ltele:relief_type('flat'),
	Ltele:font_type('arial'),
	Ename:textvariable_entry('ename'),
	Ename:font_type('arial'),
	Etele:textvariable_entry('entele'),
	Etele:font_type('arial'),
	Save:text_characters('save'),
	Save:relief_type('flat'),
	Save:font_type('arial'),
	Save:event_type_widget('<ButtonPress-1>'),
	Save:action_widget('phone_book_obj:save'),
	Menu:text_characters('menu'),
	Menu:relief_type('flat'),
	Menu:font_type('arial'),
	Menu:event_type_widget('<ButtonPress-1>'),
	Menu:action_widget('exit_tk_event_loop(phone_book_obj:test)'),
	Quit:text_characters('quit'),
	Quit:relief_type('flat'),
	Quit:font_type('arial'),
	Quit:event_type_widget('<ButtonPress-1>'),
	Quit:action_widget('exit_tk_event_loop'),

	Window2 new window_class([Lname,Ename,Ltele,Etele,Save,Menu,Quit],[],[]),
	Window2:show,
	Window2:title('Store'),

	asserta_fact(entry_name(Ename)),
	asserta_fact(entry_tele(Etele)),

	Window2:event_loop.
%,
%	destroy Window2.

quit.

save :- 
	entry_name(En),
	entry_tele(Et),
	En:textvariablevalue_string(N),
	Et:textvariablevalue_number(T),
	phone_book_store(N,T).
