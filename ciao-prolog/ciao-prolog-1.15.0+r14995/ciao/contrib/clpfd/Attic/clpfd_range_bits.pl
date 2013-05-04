%% ---------------------------------------------------------------------------
%% This file is part of the clpfd package for Ciao
%%
%% Copyright (C) 2006-2012 CLIP Group
%%
%% Originally written by:
%%   * Emilio Jesús Gallego Arias
%%
%% Modified by:
%%   * Rémy Haemmerlé
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

:- module(clpfd_range_bits, 
	[fd_range_default/1,
	 fd_range_new/3,
	 fd_range_get/2,
	 fd_range_get_min/2,
	 fd_range_get_max/2,
	 fd_range_get_size/2,
	 fd_range_get_one/2,
	 fd_range_set/2,
	 fd_range_intersect/3,
	 fd_range_union/3,
	 fd_range_complement/2
	], [assertions]).

:- doc(title, "Rangle handling with bit vectors support").

:- use_package(library(clpfd(clpfd_debug))).

:- use_module(clpfd_util, [min/3, max/3]).

%% Range handling is one of the keys for the solver's good
%% performance.  Ideally, all this should be implemented in C.

%% The format I'm going to use is a variation of the proposed in
%% Diaz's paper, but fully written in Prolog:
%%
%% Range = (Min, Max, IntervalList)
%%
%% If IntervalList is empty, we can quicky operate on Range, in any
%% other case,

%% Going to C I believe the faster would be changing IntervalList to
%% be a vector of bits, which could be optimized for using FP aided
%% calculations.

%% The motivation behind using range_get, operating and then updating
%% the range againts an API using FdVars directly is that I guess C
%% recoding of this module will be much more easier.

%% Interesting problems arise when using (inf,sup) as possible values
%% for the ranges.
fd_range_default(R) :-
	R is \ 0.

fd_range_new(Min, Max, R) :-
	fd_mask(Min,Max, R),
	!.

fd_mask(Min,Max, R) :-
	Min > Max,
	R = 0,
	!.

fd_mask(Min,Max, R) :-
	Min1 is Min + 1,
	fd_mask(Min1, Max, M),
	CMask is 1 << Min,
	R is CMask \/ M.
%% 
fd_range_get(FdVar, Range) :-
	(
	    integer(FdVar) ->
	    Range is 1 << FdVar
	;
	    get_attribute(FdVar, '$fd_store'(_, Range, _))
	).

fd_range_get_min(R, Min) :- 
	fd_range_first_1(R, Min),
	!.

fd_range_get_size(0, 0) :- !.
fd_range_get_size(R, Size) :-
	R1 is R >> 1,
	fd_range_get_size(R1, S1),
	Rm is R /\ 1,
	Size is Rm + S1.

%% Opsss.
fd_range_get_max(-1, 1000) :- !.
fd_range_get_max(R, Max) :- 
	fd_range_last_1(R, Max),
	!.

fd_range_first_1(R, 0) :-
	P is R /\ 1,
	P = 1,
	!.

fd_range_first_1(R, N) :-
	P is R >> 1,
	fd_range_first_1(P, N1),
	N is N1 + 1.

fd_range_last_1(R, 0) :-
	R = 1,
	!.

fd_range_last_1(R, N) :-
	P is R >> 1,
	fd_range_last_1(P, N1),
	N is N1 + 1.

fd_range_next_1(0,  0, _) :- !, fail.
fd_range_next_1(R1, 0, R) :-
	P is R1 /\ 1,
	P = 1,
	R is R1 >> 1,
	!.

fd_range_next_1(R1, N, R) :-
	P is R1 >> 1,
	fd_range_next_1(P, N1, R),
	N is N1 + 1,
	!.

%% This is used mainly for labeling, selects all the values of a range.
fd_range_get_one(0, _) :- fail, !.
fd_range_get_one(R, Res) :-
	(
	    fd_range_next_1(R, Res, _)
	;
	    fd_range_next_1(R, Res1, P),
	    fd_range_get_one(P, Res2),
	    Res is Res1 + Res2 + 1
	).

fd_range_set(FdVar, Range) :-
	get_attribute(FdVar, '$fd_store'(V, _, Chain)),
	update_attribute(FdVar, '$fd_store'(V, Range, Chain)).

%% fd_range_intersect
fd_range_intersect(R1, R2, R) :-
	!,
	R is R1 /\ R2,
	R \= 0.

fd_range_union(R1, R2, R) :-
	!,
	R is R1 \/ R2,
	R \= 0.

%% 
fd_range_complement(R1, R) :-
	!,
	R is \ R1.
	
