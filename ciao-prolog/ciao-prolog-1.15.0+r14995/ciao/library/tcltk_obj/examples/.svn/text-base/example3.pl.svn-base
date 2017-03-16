:- module(example3,[main/0,test/0],[objects]).

:- use_module(library(tcltk(examples(tk_test_aux)))).


:- use_class(library('tcltk_obj/window_class')).
:- use_class(library('tcltk_obj/canvas_class')).
:- use_class(library('tcltk_obj/oval_class')).
:- use_class(library('tcltk_obj/poly_class')).
:- use_class(library('tcltk_obj/arc_class')).

:- left_eye instance_of oval_class((47,49),13,23).
:- right_eye instance_of oval_class((125,49),13,23).
:- nose instance_of poly_class([(77,81),(79,77),(84,81),(80,67)]).
:- mouth instance_of arc_class((85,85),119,119).
:- face instance_of oval_class((85,85),160,160).
:- face1 instance_of canvas_class([
	oval_class(face),
	oval_class(left_eye),
	oval_class(right_eye),
	poly_class(nose),
	arc_class(mouth)]).
:- w1 instance_of window_class([],[],[canvas_class(face1)]).

main:- test.

test :-

        left_eye:set_border_width(3),
        right_eye:set_border_width(3),
        left_eye:set_bg_color(blue),
        right_eye:set_bg_color(blue),

        face:set_border_width(2),
        face:set_bg_color(gray),

        mouth:set_border_width(3),
        mouth:set_angle_start(225),
        mouth:set_style('arc'),

  	window_class(w1):show,
        face1:set_width(200),
        face1:set_height(200),
        face1:show,


%	get_code(_),
        hit_enter_delete('Eyes'),
        destroy oval_class(left_eye),
        destroy oval_class(right_eye),
        hit_enter_delete('Nose and Mouth'),
        destroy poly_class(nose),
        destroy arc_class(mouth),
        hit_enter_delete('Face'),
        destroy oval_class(face),
        hit_enter,

        destroy canvas_class(face1),
	destroy window_class(w1).
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
