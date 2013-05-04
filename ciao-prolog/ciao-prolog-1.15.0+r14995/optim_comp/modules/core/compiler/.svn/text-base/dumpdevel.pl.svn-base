% TODO: remove or redefine
% 
% This file defines some predicates useful for comparing terms and
% intermediate bytecode representations during compiler debugging 
%
% Include in the module where you want to use it.
%
% --jfran

displayv(A) :-
 	\+ \+ displayv__2(A).

displayv__2(A) :-
	name_variables(A, '$', 0, _),
	display(A).

comparev(A, B) :-
 	\+ \+ comparev__2(A, B).

comparev__2(A, B) :-
	name_variables(A, '$', 0, _),
 	name_variables(B, '$', 0, _),
 	A = B.

:- data dif/0.
comparec(A, B) :-
	( \+ \+ comparec__2(A, B) -> true ; true ),
	( retract_fact(dif) -> fail ; true ).

comparec__2(A, B) :-
	name_variables(A, '$', 0, _),
	name_variables(B, '$', 0, _),
	comparec__3(A, B, Dif),
	( Dif == yes -> asserta_fact(dif) ; true ).

comparec__3([], [], _).
comparec__3([C|Cs], [B|Bs], Dif) :-
	comparec__4(B, C, Dif),
	comparec__3(Cs, Bs, Dif).

comparec__4(clause(A,B), clause(A2,B2), Dif) :-
	( B = B2, comparec__5(A, A2) -> true
	; display(o(B, A)), nl,
	  display(n(B2, A2)), nl,
	  Dif = yes
	).

comparec__5([], []).
comparec__5([A|As], [B|Bs]) :-
	( A = B -> true
	; A = u_val(X, Y), B = u_val(Y, X) -> true
	),
	comparec__5(As, Bs).

% TODO: remove name_variables!! 
name_variables(X, Module, N1, N2) :-
	var(X), !,
 	N2 is N1 + 1,
	X = q(Module, N1).
name_variables(X, Module, N1, N2) :-
	functor(X, _, A), !,
	name_variables_2(1, A, X, Module, N1, N2). 
name_variables(_, _, N, N).

name_variables_2(I, N, X, Module, N1, N3) :-
	I =< N, !,
	I1 is I + 1,
	arg(I, X, A),
	name_variables(A, Module, N1, N2),
	name_variables_2(I1, N, X, Module, N2, N3).
name_variables_2(_, _, _, _, N, N). 

number_variables(N1, N1, N2) :- !,
 	N2 is N1 + 1.
number_variables(X, N1, N2) :-
	functor(X, _, A), !,
	number_variables_2(1, A, X, N1, N2). 
number_variables(_, N, N).

number_variables_2(I, N, X, N1, N3) :-
	I =< N, !,
	I1 is I + 1,
	arg(I, X, A),
	number_variables(A, N1, N2),
	number_variables_2(I1, N, X, N2, N3).
number_variables_2(_, _, _, N, N). 

