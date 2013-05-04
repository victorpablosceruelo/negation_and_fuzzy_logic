%% ---------------------------------------------------------------------------
%% This file is part of the clpfd package for Ciao
%%
%% Copyright (C) 2006-2012 CLIP Group
%%
%% Authors
%%   * Rémy Haemmerlé
%%   * Emilio Jesús Gallego Arias
%%   * Jose F. Morales
%%
%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version 2
%% of the License, or (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with this program; if not, write to the Free Software
%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
%% ---------------------------------------------------------------------------

:- module(fd_range_finite_intervals,
	[
	    fd_range_type/1,
	    fd_range_bound_t/1,
	    fd_range_t/1,

	    new/3,
	    default/1,

	    is_singleton/1,
	    singleton_to_bound/2,

	    enum/2,
	    next_in_dom/3,

	    get_domain/2,
	    get_domain_term/2,

	    min/2,
	    max/2,
	    size/2,

	    bound_const/2,
	    bound_add/3,
	    bound_sub/3,
	    bound_mul/3,
	    bound_div/3,

	    range_add/3,
	    range_sub/3,
	    range_mul/3,

	    intersect/3,
	    union/3,
	    complement/2,

	    remove/3,

	    in_range/2
	],
	[assertions, regtypes,  fsyntax, dcg]).

:- doc(title, "Sparse Integer Ranges Implemented as List of Intervals").

:- use_module(library(clpfd(fd_utils)), [min/3, max/3, clpfd_error/2]).

:- use_module(library(lists),   [last/2, append/3]).
:- use_module(library(between), [between/3]).

fd_range_type(prolog_finite_interval_list).

%% Ranges are a very important factor for the solver's good
%% performance. This may be implemented in C and get some gain.

%% We may use a vector of bits for the ranges, which could be
%% optimized using SIMD operations.

%% The format is a variation of the proposed in Diaz's paper, but
%% fully written in Prolog:

%% Range = (MaxRange, OrderedIntervalList)
%% The Min value can be inferred from the first element of the list.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Types, Creators and Accessors
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Our range bounds are closed integers

fd_range_bound_t := ~int.

:- regtype fd_interval_t/1 # "Integer Interval List".

fd_interval_t([]).
fd_interval_t([(X,Y)|L]) :-
	fd_range_bound_t(X),
	fd_range_bound_t(Y),
	fd_interval_t(L).

% :- regtype fd_range_t/1 #
% 	"Data Structure for Sparse Ranges with Singleton".

% TODO: Specify well the type.
fd_range_t(X) :-
	int(X).
fd_range_t(range(Max,IntervalList)) :-
	fd_range_bound_t(Max),
	fd_interval_t(IntervalList).

range_max(9999).
range_min(-9999).

default(range(~range_max, [(~range_min, ~range_max)])).

% Creates a singleton range.
new(X, X, X):- !.

% Fail on empty ranges.
new(Min, Max, range(Max, [(Min, Max)])) :-
	Min < Max.

% A singleton range is just an integer.
is_singleton(X) :-
	int(X).

singleton_to_bound(X, Y) :-
	int(X), !, X = Y.

min(X, Y) :-
	int(X),
	!,
	X = Y.

min(range(_Max, [(Min,_)|_]), Min).

max(X, Y) :-
	int(X),
	!,
	X = Y.
max(range(Max, _), Max).

% Fails here, TODO: Check this is right as we don't want to support
% labelling with infitine ranges.

size(I, 1) :- int(I), !.
size(range(_, I), Size) :-
	get_sizes(I, Size).

get_sizes([], 0).
get_sizes([I|Il], Size) :-
	get_sizes(Il, RSize),
	I = (Ia, Ib),
	ISize is Ib - Ia + 1,
	Size is ISize + RSize, !.

%% Get all the integers on a range.
get_domain(Int, [Int]):-
	int(Int), !.

get_domain(range(_Max, I), Dom) :- !,
	get_domain_list(I, Dom).

get_domain_list([], []).
get_domain_list([(Min, Max)|I], Dom):-
	get_domain_list_(Min, Max, Dom, DomTail),
	get_domain_list(I, DomTail).

get_domain_list_(Min, Min, [Min|DomTail], DomTail):-!.
get_domain_list_(Min, Max, [Min|Tail], DomTail):-
	NMin is Min + 1,
	get_domain_list_(NMin, Max, Tail, DomTail).

enum(X, Y) :-
	integer(X), !, X = Y.

enum(range(_, I), Res) :- !,
	enum_(I, Res).

enum_([(Min, Max)|_], Val) :-
	between(Min, Max, Val).
enum_([_|Il], Val) :-
	enum_(Il, Val).

% TODO: EJGA, I don't understand how this works.
% fail for singleton
next_in_dom(up, X, _) :-
	integer(X), !,
	fail.
next_in_dom(down, X, _) :-
	integer(X), !,
	fail.

next_in_dom(up, range(_, [(Min, _)|_]), Min):- !.
next_in_dom(down, range(Max, _), Max) :- !.

next_in_dom(R, _Dom, _Next) :-
        clpfd_error(domain_error(_, R), fd_range:next_in_dom/3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Operations on Ranges
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Operations with bounds.
bound_const(inf, R) :- !, range_min(R).
bound_const(sup, R) :- !, range_max(R).
bound_const(R, R) :- !.

bound_add(P1, P2, Res) :- !,
	Res is P1 + P2.

bound_sub(P1, P2, Res) :- !,
	Res is P1 - P2.

%% Fix for maximum values
bound_mul(B1, B2, Res) :- !,
	Res is B1 * B2.

%% For division we emit an error on undefined cases.
bound_div(B, 0, _) :- !,
	clpfd_error(divison_by_0(B), bound_div).
bound_div(B1, B2, Res) :- !,
	Res is B1 // B2.

%% Pointwise operations.

:- pred range_add(+fd_range_t, +fd_range_bound_t, -fd_range_t) # "Pointwise addition to a range".
range_add(Int, Bound, Res) :-
	int(Int), !,
	bound_add(Int, Bound, Res).
range_add(range(Max, L), B, range(Max1, L1)) :-
	bound_add(Max, B, Max1),
	add_list(L, B, L1).

add_list([], _, []).
add_list([(Min, Max)|Rl], Int, [(Min1, Max1)|Rl1]) :-
	bound_add(Min, Int, Min1),
	bound_add(Max, Int, Max1),
	add_list(Rl, Int, Rl1).

bound_negate(I, -I).

range_sub(R, Int, NR) :-
	bound_negate(Int, MInt),
	range_add(R, MInt, NR).

range_mul(Int1, Int2, Int3) :-
	int(Int1), !,
	bound_mul(Int1, Int2, Int3).
range_mul(range(Max, L), Int, range(Max1, L1)) :-
	bound_mul(Max, Int, Max1),
	mul_list(L, Int, L1).

mul_list([], _, []).
mul_list([(Min, Max)|Rl], Int, [(Min1, Max1)|Rl1]) :-
	bound_mul(Min, Int, Min1),
	bound_mul(Max, Int, Max1),
	mul_list(Rl, Int, Rl1).


% TODO: Document this.
new_internal([], _, _):- !, fail.
new_internal([(Min, Min)], _, Min) :-!.
new_internal(I, GMax, range(GMax, I)).

%% Pointfree operations.

% TODO: Rename to intersection
intersect(R1, R2, R1) :-
	int(R1), !,
	in_range(R1, R2).
intersect(R1, R2, R2) :-
	int(R2), !,
	in_range(R2, R1).
intersect(range(_, I1), range(_, I2), R) :-
	intersect_(I1, I2, NI, ~range_min, GMax),
	new_internal(NI, GMax, R).

intersect_([], _, [], GMax, GMax) :-!.
intersect_(_, [], [], GMax, GMax) :-!.
intersect_([(Min1, Max1)|I1], [(Min2, Max2)|I2], I, GMax_, GMax):-
	(
	    Max1 < Min2 ->
	    intersect_(I1, [(Min2, Max2)|I2], I, GMax_, GMax)
	;
	    Max2 < Min1 -> 
	    intersect_([(Min1, Max1)|I1], I2, I, GMax_, GMax)
	;
	    I = [(Min, Max)|Il], 
	    (
		'=<'(Min1, Min2) ->
		Min = Min2, 
		(
		    '=<'(Max1, Max2) ->
		    Max = Max1,
		    intersect_(I1, [(Max1, Max2)|I2], Il, Max, GMax)
		;
		    Max = Max2, 
		    intersect_([(Max2, Max1)|I1], I2, Il, Max, GMax)
		)
	    ;
		Min = Min1,
		(
		    '=<'(Max1, Max2) ->
		    Max = Max1,
		    intersect_(I1, [(Max1, Max2)|I2], Il, Max, GMax)
		;
		    Max = Max2,
		    intersect_([(Max2, Max1)|I1], I2, Il, Max, GMax)
		)
	    )
	).

union(R1, R2, R3):-
	integer(R1), !,
	union(R2, range(R1, [(R1,R1)]), R3).
union(R1, R2, R3):- 
	integer(R2), !,
	union(R1, range(R2, [(R2,R2)]), R3).
union(range(Max1, I1), range(Max2 , I2), R3):-
	union_(I1, I2, I3),
	new_internal(I3, ~max(Max1, Max2), R3).

union_([], I, I):-!.
union_(I, [], I):-!.
union_([(Min1, Max1)|Tail1], [(Min2, Max2)|Tail2], Tail):-
	(
	    Max1 < Min2-1 ->
	    Tail = [(Min1, Max1)|Tail_],
	    union_(Tail1, [(Min2, Max2)|Tail2], Tail_)
	;
	    Max2 < Min1-1 ->
	    Tail = [(Min2, Max2)|Tail_],
	    union_([(Min1, Max1)|Tail1], Tail2, Tail_)
	;
	    '=<'(Max1, Max2) ->
	    union_(Tail1, [(~min(Min1,Min2), Max2)|Tail2], Tail)
	;
	    union_([(~min(Min1,Min2), Max1)|Tail1], Tail2, Tail)
	).

complement(Int, R) :-
	int(Int), !,
	I1 is Int - 1,
	I2 is Int + 1,
	R = range(~range_max, [(~range_min, I1), (I2, ~range_max)]).

complement(range(_Max, I), R) :-
	complement_(I, R).

complement_(I, R):-
	complement_list(I, ~range_min, ~range_max, ~range_min, GMax, NI),
	new_internal(NI, GMax, R).

complement_list([], Min, Max, _, Max, [(Min,Max)]) :-
	'<'(Min, Max), !.
complement_list([], Min, Min, _, Min, [(Min, Min)]) :- !.
complement_list([], _Min, _Max, GMax, GMax, []).
complement_list([(Min_, Max_)|Il], Min, Max, GMax_, GMax, NI):-
	(
	    '>='(Min, Max) ->
	    NI = []
	;
	    '>'(Min_, Max) ->
	    GMax = Max, NI = [(Min, GMax)]
	;
	    Min_ = Max ->
	    GMax is Max - 1, NI = [(Min, GMax)]
	;
	    '=<'(Min_, Min) ->
	    NMin is Max_ + 1,
	    complement_list(Il, NMin, Max, GMax_, GMax, NI)
	;
	    NMin is Max_ + 1,
	    Max__ is Min_ - 1,
	    NI = [(Min, Max__)|NIl],
	    complement_list(Il, NMin, Max, GMax_, GMax, NIl)
	).

:- pred remove(+fd_range_t, +int, -fd_range_t) # "Remove an integer from a range".
remove(Int1, Int2, Int1):-
        int(Int1), !,
        Int1 \= Int2.

remove(range(Max, I), N, R):-
        remove_(I, N, NI, ~range_min, GMax),
        (
            GMax = Max ->
            true
        ;
            true
        ),
        new_internal(NI, GMax, R).

remove_([], _, [], _, _).
remove_([(Min, Max)| I], N, NI, GMax_, GMax):-
        (
            '<'(N, Min) ->
            NI = [(Min, Max)| I]
        ;
            N = Min ->
            (
                N = Max ->
                NI = I
            ;
                Min_ is Min + 1,
                NI = [(Min_, Max)| I]
            )
        ;
            '<'(N, Max) ->
            Max_ is N - 1, Min_ is N + 1,
            NI = [(Min, Max_), (Min_, Max)| NIt],
            remove_(I, N, NIt, GMax_, GMax)
        ;
            N = Max ->
            Max_ is Max - 1, 
            NI = [(Min, Max_)| I],
            (
                I = [] ->
                GMax = Max_
            ;
                true
            )
        ;
            NI = [(Min, Max)| NIt],
            remove_(I, N, NIt, GMax_, GMax)
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Ranges Membership
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

in_range(N, Int) :-
	int(Int), !,
	N = Int.
in_range(N, range(_, I)) :-
	in_interval(N, I).

in_interval(N, [(Min,Max)|_]) :-
	'>='(N, Min),
	'=<'(N, Max),
	!.
in_interval(N, [(_, Max)|R]) :-
	'>='(N, Max),
	in_interval(N, R).

% TODO: This is semiformal and semi pretty-print, finish.
get_domain_term(Int, Int):- integer(Int), !.
get_domain_term(range(_, Intervals), Domain) :-
	get_domain_term_form_list(Intervals, Domain).


get_domain_term_form_list([(Min, Max)|T], Dom):-
	(
	    Min = Max ->
	    Dom1 = Min
	;
	    Dom1 = '..'(Min, Max)
	),
	(
	    T = []  ->
	    Dom = Dom1
	;
	    Dom = '\\/'(Dom1, Dom2), 
	    get_domain_term_form_list(T, Dom2)
	).

