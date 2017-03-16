:- module(basic_props, [], [oo_syntax, hiord, assertions, fsyntax, mutables, string_type]).

% TODO: this should be defined in lists.pl
:- export(list/1).
list([]).
list([_|L]) :- list(L).

% TODO: this should be defined in lists.pl
:- export(member/2).
member(X, [X|_]).
member(X, [_Y|Xs]):- member(X, Xs).
