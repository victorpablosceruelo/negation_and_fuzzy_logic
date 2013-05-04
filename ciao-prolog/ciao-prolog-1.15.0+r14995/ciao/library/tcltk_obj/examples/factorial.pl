:- module(factorial,[main/0,test/0],[assertions,objects]).

:- use_module(library(tcltk(examples(tk_test_aux)))).


:- use_class(library('tcltk_obj/window_class')).
:- use_class(library('tcltk_obj/widget_class')).
:- use_class(library('tcltk_obj/button_class')).
:- use_class(library('tcltk_obj/label_class')).
:- use_class(library('tcltk_obj/entry_class')).
:- use_class(library('tcltk_obj/menubutton_class')).
:- use_class(library('tcltk_obj/menu_class')).
:- use_class(library('tcltk_obj/canvas_class')).
:- use_class(library('tcltk_obj/menu_entry_class')).
:- use_class(library('tcltk_obj/oval_class')).
:- use_class(library('tcltk_obj/canvas_class')).
:- use_class(library('tcltk_obj/poly_class')).
:- use_class(library('tcltk_obj/arc_class')).
:- use_class(library('tcltk_obj/text_class')).


 %% :- e1 instance_of entry_class.
 %% :- e2 instance_of entry_class.
 %% :- b1 instance_of button_class.
 %% :- b2 instance_of button_class.
 %% :- l1 instance_of label_class.
 %% :- l2 instance_of label_class.
 %% :- b3 instance_of button_class.
 %% :- b4 instance_of button_class.
 %% :- l3 instance_of label_class.
 %% :- l4 instance_of label_class.
 %% :- m1 instance_of menubutton_class.
 %% :- m2 instance_of menubutton_class.
 %% :- me1 instance_of menu_entry_class.
 %% :- me2 instance_of menu_entry_class.
 %% :- mc1 instance_of menu_class(
 %% 	[
 %%             menu_entry_class(me1),
 %%             menu_entry_class(me2)
 %%         ]).
 %% :- text1 instance_of text_class(50,25).
 %% :- line instance_of canvas_class(
 %% 	[
 %% 	    text_class(text1)
 %% 	]).
 %% :- c1 instance_of canvas_class(
 %% 	[
 %% 	    text_class(t1)
 %% 	]).
 %% :- w2 instance_of window_class(
 %% 	[
 %%             label_class(l3),
 %%             button_class(b3),
 %%             button_class(b4)
 %% 	],[],[]).
 %% :- w1 instance_of window_class(
 %% 	[
 %%             label_class(l1),
 %%             entry_class(e1),
 %%             label_class(l2),
 %%             entry_class(e2),
 %%             button_class(b1),
 %%             button_class(b2)
 %% 	],[],[]).
 %% 
 %% :- w0 instance_of window_class(
 %% 	[
 %%             menubutton_class(m1)
 %% 	],[menu_class(mc1)],[canvas_class(line)]).


main:- test.

test :- 
        create_objects(E1, E2, B1, B2, L1, L2, B3, B4, L3, L4, M1, M2,
Me1, Me2, Mc1, Text1, Line, C1, W2, W1, W0),
        initialize_objects(W1, W2, M1, MC1, ME1, ME2, Text1, Line, W0).

initialize_objects(W1, W2, M1, MC1, ME1, ME2, Text1, Line, W0):-
	W1:set_withdraw,
	W2:set_withdraw,

	M1:set_menu('m1'),
	M1:set_text('Menu'),
	M1:set_relief('raised'),
	M1:set_side('top'),
	M1:set_font('Times'),
	M1:get_menu(Y),

	MC1:set_tearoff('1'),
	MC1:set_name(Y),

	ME1:set_name(Y),
	ME1:set_label('Factorial'),
	ME1:set_action('example1:presentation'),

	ME2:set_name(Y),
	ME2:set_label('Quit'),
	ME2:set_action('example1:quit'),

	Text1:set_text('Factorial'),
	Text1:set_border_width(100),

	Line:set_width(100),
	Line:set_height(50),

        W0:show,
	W0:set_title("Factorial"),
	Line:show,
	MC1:show,
	W0:event_loop,
	destroy W0.

presentation :- 
	W0:set_withdraw,
	B1:set_text('Factorial'),
	B1:set_font('arial'),
	B1:set_relief('raised'),
	B1:set_side('left'),
	B1:set_event_type('<ButtonPress-1>'),
	B1:set_action('example1:factorial'),
	B1:set_variables('inputval'),


	E1:set_justify('right'),
	E1:set_background('white'),
	E1:set_borderwidth('2'),
	E1:set_foreground('black'),
%	E1:set_side('left'),
	E1:set_expand('1'),
	E1:set_fill('both'),
%	E1:set_padx('8'),
%	E1:set_pady('8'),
	E1:set_textvariable('inputval'),
	E1:set_font('arial'),

	E2:set_justify('right'),
	E2:set_background('white'),
	E2:set_borderwidth('2'),
	E2:set_foreground('black'),
	E2:set_expand('1'),
	E2:set_fill('both'),
	E2:set_textvariable('Outputval'),
	E2:set_font('arial'),
 
	B2:set_text('Quit'),
	B2:set_font('arial'),
	B2:set_event_type('<ButtonPress-1>'),
	B2:set_action('example1:quit'),
	B2:set_relief('raised'),
	B2:set_side('right'),

	L1:set_text('El factorial de : '),
	L1:set_font('arial'),
 	L1:set_relief('flat'),
	L1:set_background('gray'),

	L2:set_text('es : '),
	L2:set_font('arial'),
	L2:set_relief('flat'),

%        W1:show,
	W1:set_title("Factorial"),
	W1:set_maxsize(300,300),
	W1:set_minsize(200,200),
        W1:show,
	W1:event_loop,

	destroy W1.


factorial :- 
        E1:get_textvariablevalue(X),
	tk_test_aux:factorial(X,Z),
	E2:set_textvariablevalue(Z).
	
quit :- 
	W1:set_withdraw,
	W0:set_withdraw,
	L3:set_text('Do you want to quit? '),
	L3:set_font('arial'),
	L3:set_relief('flat'),
	L3:set_background('gray'),

	B3:set_text('Ok'),
	B3:set_font('arial'),
	B3:set_event_type('<ButtonPress-1>'),
	B3:set_action('tk_test_aux:quit'),
	B3:set_relief('raised'),
	B3:set_side('left'),

	B4:set_text('Cancel'),
	B4:set_font('arial'),
	B4:set_event_type('<ButtonPress-1>'),
	B4:set_action('example1:cancel'),
	B4:set_relief('raised'),

	E2:set_background('green'),

        W2:show,
	W2:set_title("Confirm"),
	W2:event_loop,
	destroy W2.

cancel :- 
	W1:set_withdraw,
	W2:set_withdraw,
	W0:show,
	W0:event_loop.
