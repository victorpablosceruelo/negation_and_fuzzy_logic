:- module(test_username, _, [rfuzzy, clpr, pkgs_output_debug]).

is_ascii_list(X) :- var(X), !, fail.
is_ascii_list([]).
is_ascii_list([X|Xs]) :-
	integer(X),
	is_ascii_list(Xs).

sameUserNameAtom(X, Y) :- var(X), var(Y), !.
sameUserNameAtom(X, Y) :- (var(X) ; var(Y)), !, fail.
sameUserNameAtom(X, Y) :-
	functor(X, Name, Arity),
	functor(Y, Name, Arity), !.

test('Ok') :- localUserName('victorpablosceruelo_at_gmail_com').
test('Ok atom') :- localUserName(Atom), sameUserNameAtom(Atom, 'victorpablosceruelo_at_gmail_com').
test('Ascii in Prolog') :- X = 'victorpablosceruelo_at_gmail_com', is_ascii_list(X).
test('Ascii from outside') :- localUserName(X), is_ascii_list(X).
test('Fail').

