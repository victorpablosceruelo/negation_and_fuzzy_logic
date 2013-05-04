:- module(_, [substitute/3], [assertions, regtypes,
		ciaopp(examples(resources(exectimell)))]).

:- entry substitute(X, Y, Z) : arithexpression * list(replacement) * var.

:- export(replacement/1).
:- regtype replacement/1.

replacement('='(A, B)) :- arithexpression(A), arithexpression(B).


substitute(A + B, Subs, NewA + NewB) :-
	!,
	substitute(A, Subs, NewA),
	substitute(B, Subs, NewB).
substitute(A * B, Subs, NewA * NewB) :-
	!,
	substitute(A, Subs, NewA),
	substitute(B, Subs, NewB).
substitute(A - B, Subs, NewA - NewB) :-
	!,
	substitute(A, Subs, NewA),
	substitute(B, Subs, NewB).
substitute(A = B, Subs, NewA = NewB) :-
	!,
	substitute(A, Subs, NewA),
	substitute(B, Subs, NewB).
substitute((A ** B), Subs, (NewA ** B)) :-
%	integer( B ),
	!,
	substitute(A, Subs, NewA).
substitute(A, Subs, B) :-
	find_replacement(A, Subs, B),
	!.
substitute(A, _, A).

find_replacement(A, [A = B|_], B).
find_replacement(A, [_|Ys],    B) :- find_replacement(A, Ys, B).

% substitute( A, Subs, B ) :-
% 	find_replacement( Subs, A, B ),
% 	!.

% find_replacement( [],            A, A ).
% find_replacement( [ A = B | _ ], A, B ).
% find_replacement( [ _ | Ys ],    A, B ) :- find_replacement( Ys, A, B ).
