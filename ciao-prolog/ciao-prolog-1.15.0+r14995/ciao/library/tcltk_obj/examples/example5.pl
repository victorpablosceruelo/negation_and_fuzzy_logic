:- module(example5,[main/0],[objects]).

:- use_module(library(tcltk(examples(tk_test_aux)))).


:- use_class(library('tcltk_obj/window_class')).
:- use_class(library('tcltk_obj/canvas_class')).
:- use_class(library('tcltk_obj/oval_class')).
:- use_class(library('tcltk_obj/poly_class')).
:- use_class(library('tcltk_obj/arc_class')).
:- use_class(library('tcltk_obj/widget_class')).
:- use_class(library('tcltk_obj/button_class')).

 %% main:- 
 %%     Oval new oval_class((200,200), 20, 50),
 %%     Oval:set_border_width(3),
 %%     Oval:set_bg_color(blue),
 %%     
 %%     Face1 new canvas_class([Oval]),
 %%     Face1:set_width(200),
 %%     Face1:set_height(200),
 %%     
 %%     Window new window_class([],[],[Face1]),
 %%     Window:set_title('Example'),
 %%     Window:show,
 %%     Face1:show,
 %%     move(Oval, 200, 200),
 %%     destroy Oval,
 %%     destroy Face1,
 %%     destroy Window.

main:- 
    Oval new oval_class((200,200), 20, 50),
    
    Face1 new canvas_class([Oval]),
    Face1:set_width(200),
    Face1:set_height(200),
    
    Window new window_class([],[],[Face1]),
    Window:show,
    Face1:show,

    move(Oval, 200, 200),
    destroy Oval,
    destroy Face1,
    destroy Window.

move(_Oval, 0, 0).
move(Oval, X, Y):-
    X > 0, Y > 0,
    X1 is X - 1, Y1 is Y - 1,
    Oval:set_center(X1, Y1),
    move(Oval, X1, Y1).

hit_enter :-
        display('Hit ENTER to continue...'),
        nl,
        get_code(_).
