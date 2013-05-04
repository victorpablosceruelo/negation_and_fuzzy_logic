:- module(prau, [a/1,b/3], []).

a(1).
a(2).
a(3).

b((H :- B), H, B) :- !.
b(H, H, true).
