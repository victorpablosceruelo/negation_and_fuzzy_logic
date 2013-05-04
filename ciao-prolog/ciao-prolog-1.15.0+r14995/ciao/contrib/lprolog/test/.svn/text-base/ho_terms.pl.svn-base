:- module(ho_terms,[main/0],[]).
:- use_package(lprolog).
:- use_module(library(write)).

example1 :-
	nl, display(example1), nl,
	display('unify  \\x.x  with  \\x.(N x)'), nl,
	display('result should be  N -> \\x.x'), nl,
	T1 = (x \ x),
	T2 = (x \ (N @ x)),
	show_terms_before(T1,T2),
	T1 = T2,
	show_terms_after(T1,T2),
	display('N = '), display_ref(N), nl.


example2 :-
	nl, display(example2), nl,
	display('unify  \\x.(c x)  with \\x.(c (N x))'), nl,
	display('result should be  N -> \\x.x'), nl,
	T1 = (x \ (c @ x)),
	T2 = (x \ (c @ (N @ x))),
	show_terms_before(T1,T2),
	T1 = T2,
	show_terms_after(T1,T2),
	display('N = '), display_ref(N), nl.


example3 :-
	nl, display(example3), nl,
	display('unify  \\x\\y.(c y x)  with N'), nl,
	display('result should be  N -> \\x\\y.(c y x)'), nl,
	T1 = (x \ y \ c(x,y)),
	show_terms_before(T1,T2),
	T1 = T2,
	show_terms_after(T1,T2).


example4 :-
	nl, display(example4), nl,
	display('unify  \\x\\y.(c y x)  with  \\x.(c (N x))'), nl,
	display('result should be  N -> \\x.x'), nl,
	T1 = (x \ y \ c(x,y)),
	T2 = (x \ (c @ (N @ x))),
	show_terms_before(T1,T2),
	T1 = T2,
	show_terms_after(T1,T2),
	display('N = '), display_ref(N), nl.


example5 :-
	nl, display(example5), nl,
	display('unify  (X a b)  with  (Y b c)'), nl,
	display('result should be  X -> \\x.\\y.(Z x y)  and  Y -> \\x\\y.(Z a x)'), nl,
	pi a \ sigma Y \ pi b \ sigma q2 \ pi c \ (
%     T1 = X(a,b),
%     T2 = Y(b,c),
%     show_terms_before(T1,T2),
%     T1 = T2,
%     show_terms_after(T1,T2),
	X(a,b) = Y(b,c),
	display('X = '), display_ref(X), nl,
	display('Y = '), display_ref(Y), nl ).


example6 :-
	nl, display(example6), nl,
	display('unify  (X a b)  with  (c (Y b c))'), nl,
	display('result should be  X -> \\x\\y.(c (Z x y))  and  Y -> \\x\\y.(Z a x)'), nl,
	pi a \ sigma Y \ pi b \ pi c3 \ (
%     T1 = X(a,b),
%     T2 = c(Y(b,c)),
%     show_terms_before(T1,T2),
%     T1 = T2,
%     show_terms_after(T1,T2),
	X(a,b) = c(Y(b,c3)),
	display('X = '), display_ref(X), nl,
	display('Y = '), display_ref(Y), nl
	).


example7 :-
	nl, display(example7), nl,
	display('unify  (c (X a b) (X b d))  with  (c (Y b c) (b d))'), nl,
	display('result should be X -> \\x\\y.(x y)  and  Y -> \\x\\y.(a x)'), nl,
	pi a \ pi d \ sigma Y \ pi b \ pi c3 \ (
    display_ref(c(X(a,b), X(b,d))), nl,
    display_ref(c(Y(b,c3), b(d))), nl,
	c(X(a,b), X(b,d)) = c(Y(b,c3), b(d)),
%     T1 = c(X(a,b), X(b,d)),
%     T2 = c(Y(b,c), b(d)),
%     show_terms_before(T1,T2),
%     T1 = T2,
%     show_terms_after(T1,T2),
	display('X = '), display_ref(X), nl,
	display('Y = '), display_ref(Y), nl
%     hnorm(X,X1), hnorm(Y,Y1),
%     display('X1 = '), display_ref(X1), nl,
%     display('Y1 = '), display_ref(Y1), nl
	).





main :-
	example1,
	example2,
	example3,
	example4,
	example5,
	example6,
	example7.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

show_terms_before(T1, T2) :-
	display('Before unification:'), nl,
	display('     T1: '), display_ref(T1), nl,
	display('     T2: '), display_ref(T2), nl.

show_terms_after(T1, T2) :-
	display('After unification:'), nl,
	display('     T1: '), display_ref(T1), nl,
	display('     T2: '), display_ref(T2), nl.

% show_terms_after_norm(T1, T2) :-
%     display('After unification and normalization:'), nl,
%     display('     T1: '), display_ref(T1), nl,
%     display('     T2: '), display_ref(T2), nl.



