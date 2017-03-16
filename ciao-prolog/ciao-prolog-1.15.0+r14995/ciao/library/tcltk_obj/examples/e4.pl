:- module(_,[main/0],[objects]).

:- use_module(library(tcltk(examples(tk_test_aux)))).


:- use_class(library('tcltk/examples/interface/window_class')).
:- use_class(library('tcltk/examples/interface/canvas_class')).
:- use_class(library('tcltk/examples/interface/oval_class')).
:- use_class(library('tcltk/examples/interface/poly_class')).
:- use_class(library('tcltk/examples/interface/arc_class')).


main :-

        Left_eye new oval_class((47,49),13,23),
        Right_eye new oval_class((125,49),13,23),

        Left_eye:set_border_width(3),
        Right_eye:set_border_width(3),
        Left_eye:set_bg_color(blue),
        Right_eye:set_bg_color(blue),

        Nose new poly_class([(77,81),(79,77),(84,81),(80,67)]),

        Face new oval_class((85,85),160,160),

        Face:set_border_width(2),
        Face:set_bg_color(gray),

        Mouth new arc_class((85,85),119,119),

        Mouth:set_border_width(3),
        Mouth:set_angle_start(225),
        Mouth:set_style('arc'),

        Face1 new canvas_class([Face,Left_eye,Right_eye,Nose,Mouth]),

	Win1 new window_class([],[],[Face1]),
	Win1:set_title('FACE'),
	Win1:show,

        Face1:set_width(200),
        Face1:set_height(200),
        Face1:show,

%	get_code(_),
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
	destroy Win1.
%        destroy Face2.

hit_enter :-
        display('Hit ENTER to continue...'),
        nl,
        get_code(_).

hit_enter_delete(X) :-
        display('Hit ENTER to delete '),
        display(X),
        nl,
        get_code(_).
