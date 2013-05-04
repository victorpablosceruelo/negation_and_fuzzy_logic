:- module(e1,[main/0],[objects]).

:- use_module(library(tcltk(examples(tk_test_aux)))).


:- use_class(library('tcltk/examples/interface/window_class')).
:- use_class(library('tcltk/examples/interface/widget_class')).
:- use_class(library('tcltk/examples/interface/button_class')).
:- use_class(library('tcltk/examples/interface/label_class')).
:- use_class(library('tcltk/examples/interface/entry_class')).
:- use_class(library('tcltk/examples/interface/menubutton_class')).
:- use_class(library('tcltk/examples/interface/menu_class')).
:- use_class(library('tcltk/examples/interface/canvas_class')).
:- use_class(library('tcltk/examples/interface/menu_entry_class')).
:- use_class(library('tcltk/examples/interface/oval_class')).
:- use_class(library('tcltk/examples/interface/canvas_class')).
:- use_class(library('tcltk/examples/interface/poly_class')).
:- use_class(library('tcltk/examples/interface/arc_class')).
:- use_class(library('tcltk/examples/interface/text_class')).


:- e1 instance_of entry_class.
:- e2 instance_of entry_class.
:- b1 instance_of button_class.
:- b2 instance_of button_class.
:- l1 instance_of label_class.
:- l2 instance_of label_class.
:- b3 instance_of button_class.
:- b4 instance_of button_class.
:- l3 instance_of label_class.
:- l4 instance_of label_class.
:- m1 instance_of menubutton_class.
:- m2 instance_of menubutton_class.
:- me1 instance_of menu_entry_class.
:- me2 instance_of menu_entry_class.
:- mc1 instance_of menu_class(
	[
	menu_entry_class(me1),
	menu_entry_class(me2)]).
:- text1 instance_of text_class(50,25).
:- line instance_of canvas_class(
	[
	    text_class(text1)
	]).
:- c1 instance_of canvas_class(
	[
	    text_class(t1)
	]).
:- w2 instance_of window_class(
	[
	label_class(l3),
	button_class(b3),
	button_class(b4)
	],[],[]).
:- w1 instance_of window_class(
	[
 	label_class(l1),
	entry_class(e1),
	label_class(l2),
	entry_class(e2),
	button_class(b1),
	button_class(b2)
	],[],[]).

:- w0 instance_of window_class(
	[
	menubutton_class(m1)
	],[menu_class(mc1)],[canvas_class(line)]).

main :-
	window_class(w1):set_withdraw,
	window_class(w2):set_withdraw,

	menubutton_class(m1):set_menu('m1'),
	menubutton_class(m1):set_text('Menu'),
	menubutton_class(m1):set_relief('raised'),
	menubutton_class(m1):set_side('top'),
	menubutton_class(m1):set_font('Times'),
	menubutton_class(m1):get_menu(Y),

	menu_class(mc1):set_tearoff('1'),
	menu_class(mc1):set_name(Y),

	menu_entry_class(me1):set_name(Y),
	menu_entry_class(me1):set_label('Factorial'),
	menu_entry_class(me1):set_action('example1:presentation'),

	menu_entry_class(me2):set_name(Y),
	menu_entry_class(me2):set_label('Quit'),
	menu_entry_class(me2):set_action('example1:quit'),

	text1:set_text('Factorial'),
	text1:set_border_width(100),

	line:set_width(100),
	line:set_height(50),

        window_class(w0):show,
	window_class(w0):set_title("Factorial"),
	line:show,
	menu_class(mc1):show,
	window_class(w0):event_loop,
	destroy window_class(w0).

presentation :- 
	window_class(w0):set_withdraw,
	button_class(b1):set_text('Factorial'),
	button_class(b1):set_font('arial'),
	button_class(b1):set_relief('raised'),
	button_class(b1):set_side('left'),
	button_class(b1):set_event_type('<ButtonPress-1>'),
	button_class(b1):set_action('example1:factorial'),
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
	button_class(b2):set_action('example1:quit'),
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
        entry_class(e1):get_textvariablevalue(X),
	tk_test_aux:factorial(X,Z),
	entry_class(e2):set_textvariablevalue(Z).
	
quit :- 
	window_class(w1):set_withdraw,
	window_class(w0):set_withdraw,
	label_class(l3):set_text('Do you want to quit? '),
	label_class(l3):set_font('arial'),
	label_class(l3):set_relief('flat'),
	label_class(l3):set_background('gray'),

	button_class(b3):set_text('Ok'),
	button_class(b3):set_font('arial'),
	button_class(b3):set_event_type('<ButtonPress-1>'),
	button_class(b3):set_action('tk_test_aux:quit'),
	button_class(b3):set_relief('raised'),
	button_class(b3):set_side('left'),

	button_class(b4):set_text('Cancel'),
	button_class(b4):set_font('arial'),
	button_class(b4):set_event_type('<ButtonPress-1>'),
	button_class(b4):set_action('example1:cancel'),
	button_class(b4):set_relief('raised'),

	entry_class(e2):set_background('green'),

        window_class(w2):show,
	window_class(w2):set_title("Confirm"),
	window_class(w2):event_loop,
	destroy window_class(w2).

cancel :- 
	window_class(w1):set_withdraw,
	window_class(w2):set_withdraw,
	window_class(w0):show,
	window_class(w0):event_loop.
