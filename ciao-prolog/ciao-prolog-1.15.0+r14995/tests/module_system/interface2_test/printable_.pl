% TODO: avoid module wrapper
:- module(printable_, [], [compiler(complang)]).

:- public printable.
:- interface printable {
    :- '$statemodel'(single). % TODO: This should not be necessary
    :- multifile(show/0). % TODO: Use different syntax
}.
