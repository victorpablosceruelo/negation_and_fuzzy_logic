:- module(clpfd_bool, 
	[and/3,
	 equiv/3,
	 negate/2,
	 or/3,
	 bool_eq/3,
	 bool_leq/3,
	 xor/3
	],
	[assertions, clpfd]).

:- doc(author, "Peter Stuckey").
% TODO: Document this module

:- use_module(clpfd_store, [ fd_store_get_range/2 ]).
:- use_module(clpfd_range, [ fd_range_get_min/2,
			     fd_range_get_max/2 ]).
:- use_module(clpfd_idx, [ 'a-b=c'/3 ]).


% B <=> X <= 0 is encoded as
% X <= (1-B) * max(X)
% that is      X <= max(X) - max(X) * B
% min(X) + (1-B) * (1 - min(X)) <= X
% that is      (min(X) - 1) * B + 1 <= X
reify_leq0(X,B) :- 
	B in  0..1,
	fd_store_get_range(X, RX),
	fd_range_get_max(RX,MaxX),
	X #=< MaxX - MaxX * B,
	fd_range_get_min(RX, MinX),
	MinX1 is MinX - 1,
	MinX1 * B + 1 #=< X.

% reified less than or equal to B <=> X <= Y 
% X - Y <= (1-B) * (max(X) - min(Y))
% (min(X) - max(Y)) * B <=  X - Y 
bool_leq(X, Y, B) :-
	'a-b=c'(X,Y,C),
	reify_leq0(C,B).
	
% reified equality B <=> X = Y
bool_eq(X,Y,B) :-
	'a-b=c'(X,Y,C),
	reify_leq0(C,B1),
	reify_leq0(-C,B2),
	and(B1,B2,B).

% Boolean Constraints
or(B1,B2,B) :-
	B1 in 0..1,
	B2 in 0..1,
	B in 0..1,
	B1 #=< B,
	B2 #=< B,
%	B1 + B2 - 1 #>= B.
	B1 + B2 #>= B.

and(B1,B2,B) :-
	B1 in 0..1,
	B2 in 0..1,
	B in 0..1,
	B #=< B1,
 	B #=< B2,
	B1 + B2 #=< B + 1.

negate(B1,B) :-
	B1 in 0..1,
	B in 0..1,
	B #= 1 - B1.

equiv(B1,B2,B) :-
	B1 in 0..1,
	B2 in 0..1,
	B in 0..1,
	B1 + B2 #=< 1 + B,
	1 - B #=< B1 + B2,
        B1 - B2 #=< 1 - B,
	B2 - B1 #=< 1 - B.
	
xor(B1,B2,B) :-
	B1 in 0..1,
	B2 in 0..1,
	B in 0..1,
	B1 + B2 #=< 2 - B,
	B #=< B1 + B2,
        B1 - B2 #=< B,
	B2 - B1 #=< B.
