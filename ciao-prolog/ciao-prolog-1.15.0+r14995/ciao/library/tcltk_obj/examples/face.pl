:- module(face,[test/0,main/0],[objects]).

:- use_module(library(system), [pause/1]).


:- use_class(library('tcltk_obj/window_class')).
:- use_class(library('tcltk_obj/oval_class')).
:- use_class(library('tcltk_obj/canvas_class')).
:- use_class(library('tcltk_obj/poly_class')).
:- use_class(library('tcltk_obj/arc_class')).
	
main:- test.

test :-
%	Delay = 10,

	Left_eye new oval_class((47,49),13,23),
	Right_eye new oval_class((125,49),13,23),

        Border = 3,
	Left_eye:border_width(Border),
	Right_eye:border_width(Border),
	Backgcolor = blue,
	Left_eye:bg_color(Backgcolor),
	Right_eye:bg_color(Backgcolor),

	Nose new poly_class([(77,81),(79,77),(84,81),(80,67)]),

	Face new oval_class((85,85),160,160),

	Face:border_width(2),
	Face:bg_color(gray),

	Mouth new arc_class((85,85),119,119),

	Mouth:border_width(Border),
	Mouth:angle_start(225),
	Mouth:style_type('arc'),

	Face1 new canvas_class([Face,Left_eye,Right_eye,Nose,Mouth]),

	Window1 new window_class([],[],[Face1]),
	Window1:title('Face'),
	Window1:show,
	Face1:width_value(200),
	Face1:height_value(200),
	Face1:show,

%	Face2 new canvas_class([Face,Left_eye,Right_eye,Nose,Mouth]),
%	Face2:show,

%	eng_call(move(25,Left_eye, down,2,Delay),create,create),
%	eng_call(move(25,Right_eye,down,2,Delay),create,create),

	hit_enter_delete('Eyes'),

	destroy Left_eye,
	destroy Right_eye,
	hit_enter_delete('Nose and Mouth'),
	destroy Nose,
	destroy Mouth,
	hit_enter_delete('Face'),
	destroy Face,
	hit_enter,

	destroy Face1,
	destroy Window1.
%	destroy Face2.

hit_enter :-
	display('Hit ENTER to continue...'),
	nl,
	get_code(_).

hit_enter_delete(X) :-
	display('Hit ENTER to delete '),
	display(X),
	nl,
	get_code(_).

move(0,_O,_Direction,_Increment,_Delay).
move(Total,O,Direction,Increment,Delay) :-
	Total > 0,
	MyDelay is Delay * 10000,
	my_pause(MyDelay),
	O:center(X,Y),
	compute_move(Direction,Increment,X,Y,NX,NY),
	O:center(NX,NY),
	NTotal = Total-Increment,
	move(NTotal,O,Direction,Increment,Delay).
	
compute_move(right,Amount,X,Y,NX,Y) :- NX is X+Amount.
compute_move(left, Amount,X,Y,NX,Y) :- NX is X-Amount.
compute_move(up,   Amount,X,Y,X,NY) :- NY is Y-Amount.
compute_move(down, Amount,X,Y,X,NY) :- NY is Y+Amount.

my_pause(0).
my_pause(Delay) :- 
	Delay > 0,
	NDelay is Delay - 1,
	my_pause(NDelay).
