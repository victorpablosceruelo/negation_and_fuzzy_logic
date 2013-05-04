:- module(console, [], [oo_syntax, hiord, assertions, fsyntax, mutables, string_type]).

% TODO: implement with inheritance or interfaces?
:- attr out.

:- export(display/1).
display(X) :- out.display(X).

:- export(clear/0).
clear :- out.clear.

:- export(nl/0).
nl :- out.nl.

:- export(attach/1).
attach(Container) :-
	Div = ~js_dom.createElement("div"),
        Div.className <- "console",
        Div.id <- "output",
	Container.append_child(Div),
        ~out = ~streams_div("output").

