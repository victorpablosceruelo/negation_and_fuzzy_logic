:- module(inherit1_test, [], [oo_syntax, hiord, assertions, fsyntax, mutables, string_type]).

:- doc(title, "Test for inheritance (figures)").

:- use_module(library(arithpreds)).
:- use_module(engine(io_basic)).

:- export(main/0).
main :-
	figures.main.

% Inheritance problem
%
% TODO: take into account LSP (Liskov Substitution Principle)
%   a immutable square can inherit from a immutable rectangle
%   but a mutable square cannot inherit from a mutable rectangle

:- class flat_figure {
    % :- export(thichness/1). % TODO: not working
    :- attr thickness.

    % :- export(density/1). % TODO: not working
    :- attr density.

    :- export(area/1).
    area := S :- S = undefined.

    :- export(mass/1).
    mass := M :- M = ~density * ~area * ~thickness.
}.

:- class rect {
    :- extends flat_figure.

    % :- export(width/1). % TODO: not working
    :- attr width.

    % :- export(height/1). % TODO: not working
    :- attr height.

    :- export(cons__/2).
    cons__(W, H) :- ~width = W, ~height = H.

    :- export(area/1).
    area := S :- S = ~width * ~height.
}.

:- class circle {
    :- extends flat_figure.

    % :- export(radius/1). % TODO: not working
    :- attr radius.

    :- export(cons__/1).
    cons__(R) :- ~radius = R.

    :- export(area/1).
    area := S :- S = ~radius * ~radius * 3.1415.
}.

:- module figures {
    main :-
        R = ~rect(100, 100), ~R.density = 1, ~R.thickness = 2,
        C = ~circle(50), ~C.density = 1, ~C.thickness = 10,
        display("Rectangle area: " + ~R.area), nl,
        display("Circle area: " + ~C.area), nl,
        display("Rectangle mass: " + ~R.mass), nl,
        display("Circle mass: " + ~C.mass), nl.
  
    % TODO: Formulate problems like:
    %   A = ~rect(10, 10), C = ~circle(5), ~A.mass = ~C.mass, label(~A.thickness).
}.
