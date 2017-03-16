% Some predicates that are not used anymore
% ===========================================================================

to_lower_case([],[]).
to_lower_case([H|T],[NH|NT]) :-
	is_upper_case(H),
	!,
	NH is H+32,
	to_lower_case(T,NT).
to_lower_case([H|T],[H|NT]) :-
	to_lower_case(T,NT).

% Just plain ascii for now
is_upper_case(Ch) :-
	Ch >= 65, Ch =< 90.

:- pred split_at_bangs(A, B) :: string * string 
# "Split string @var{A} at the places where the character @term{!}
   appears as the first character in the line.".

split_at_bangs(Xs, Ys) :-
	split_at_bangs_(Xs, Chunk, Chunk, Ys).

split_at_bangs_([], Chunk, Chunk0, Ys) :- !,
	split_at_bangs__accum([], Chunk, Chunk0, Ys).
split_at_bangs_([0'\n, 0'!|Xs], Chunk, Chunk1, Ys) :- !,
	Chunk1 = [0'\n|Chunk0],
	split_at_bangs__accum(Xs, Chunk, Chunk0, Ys).
split_at_bangs_([X|Xs], Chunk, [X|Chunk0], Ys) :- !,
	split_at_bangs_(Xs, Chunk, Chunk0, Ys).

split_at_bangs__accum(Xs, Chunk, Chunk0, Ys) :-
	Chunk0 = [], % close the list
	Ys = [Chunk|Ys0], % accumulate the chunk
	( Xs = [] ->
	    Ys0 = [] % nothing more
	; split_at_bangs(Xs, Ys0)
	).

