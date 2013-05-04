:- module(example2,[main/0,test/0],[objects]).

:- use_module(library(tcltk(examples(tk_test_aux)))).


:- use_class(library('tcltk_obj/window_class')).
:- use_class(library('tcltk_obj/widget_class')).
:- use_class(library('tcltk_obj/button_class')).
:- use_class(library('tcltk_obj/label_class')).
:- use_class(library('tcltk_obj/entry_class')).


:- e1 instance_of entry_class.
:- e2 instance_of entry_class.
:- b1 instance_of button_class.
:- b2 instance_of button_class.
:- l1 instance_of label_class.
:- l2 instance_of label_class.
:- w1 instance_of window_class(
	[
 	label_class(l1),
	entry_class(e1),
	label_class(l2),
	entry_class(e2),
	button_class(b1),
	button_class(b2)
	],[],[]).


main:- test.

test :- 
	button_class(b1):set_text('Factorial'),
	button_class(b1):set_font('arial'),
	button_class(b1):set_relief('raised'),
	button_class(b1):set_side('left'),
	button_class(b1):set_event_type('<ButtonPress-1>'),
	button_class(b1):set_action('example2:factorial'),
	button_class(b1):set_variables('inputval'),


	entry_class(e1):set_justify('right'),
	entry_class(e1):set_background('white'),
	entry_class(e1):set_borderwidth('2'),
	entry_class(e1):set_foreground('black'),
%	entry_class(e1):set_side('left'),
	entry_class(e1):set_expand('1'),
	entry_class(e1):set_fill('both'),
%	entry_class(e1):set_padx('8'),
%	entry_class(e1):set_pady('8'),
	entry_class(e1):set_textvariable('inputval'),
	entry_class(e1):set_font('arial'),

	entry_class(e2):set_justify('right'),
	entry_class(e2):set_background('white'),
	entry_class(e2):set_borderwidth('2'),
	entry_class(e2):set_foreground('black'),
	entry_class(e2):set_expand('1'),
	entry_class(e2):set_fill('both'),
	entry_class(e2):set_textvariable('Outputval'),
	entry_class(e2):set_font('arial'),
 
	button_class(b2):set_text('Quit'),
	button_class(b2):set_font('arial'),
	button_class(b2):set_event_type('<ButtonPress-1>'),
	button_class(b2):set_action('tk_test_aux:quit'),
	button_class(b2):set_relief('raised'),
	button_class(b2):set_side('right'),

	label_class(l1):set_text('El factorial de : '),
	label_class(l1):set_font('arial'),
 	label_class(l1):set_relief('flat'),
	label_class(l1):set_background('gray'),

	label_class(l2):set_text('es : '),
	label_class(l2):set_font('arial'),
	label_class(l2):set_relief('flat'),

%        window_class(w1):show,
	window_class(w1):set_title("Factorial"),
	window_class(w1):set_maxsize(300,300),
	window_class(w1):set_minsize(200,200),
        window_class(w1):show,
	window_class(w1):event_loop,

	destroy window_class(w1).


factorial :- 
        entry_class(e1):get_textvariablevalue_number(X),
	tk_test_aux:factorial(X,Z),
	entry_class(e2):set_textvariablevalue_number(Z).
