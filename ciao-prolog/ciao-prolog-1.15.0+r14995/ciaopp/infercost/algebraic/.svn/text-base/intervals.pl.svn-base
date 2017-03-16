:- module(_,
	[
	    is_elmt_intersect/2,
	    qsort/2,
	    greater_than/2,
	    intersect/3,
	    build_space/3,
	    shift_boundary/4,
	    batch_shift_boundary/3,
	    is_adjacent/2,
	    remove/3,
	    projection/3,
	    elmt_split/3,
	    batch_remove/3,
	    batch_remove_1F/3
	],
	[assertions]).
%:- use_module(library(lists), [append/3, reverse/2]).

:- doc(filetype, documentation).

:- doc(title,"Intervals Operations").
:- doc(author,"Luthfi Darmawan").

:- doc(summary,"This module provides procedures for intervals operation").

:- doc(module,"This module provides procedures for intervals operation.

An intervals:
- is n-dimensional space interval.
- explicitly states whether it is close or open interval

Representation:
- A 1-dimensional interval is [i(Border1, Close1, Border2, Close2)]
  Close = 1, close end
  Close = 0, open end
- An n-dimensional interval is a tuple (which is represented as list)
  [i(Border1, Close1, Border2, Close2), ...,
   i(BorderN1, CloseN1, BorderN2, CloseN2)]

").


:- use_module(library(lists),[append/3, reverse/2]).
% ---------------------------------------------------------------------------
%              utility
% ---------------------------------------------------------------------------
max(A,B,A):-
	A > B, !.
max(A,B,B):-
	A =< B.

min(A,B,A):-
	A =< B, !.
min(A,B,B):-
	A > B.

% ---------------------------------------------------------------------------
% ++++++++++++++++++ Interval element level operation ++++++++++++++++++++++
% ---------------------------------------------------------------------------

% ---------------------------------------------------------------------------
is_elmt_intersect(i(A,_,B,_), i(K,_,L,_)):-
	(
	    A =< L, L =< B
	;
	    A =< K, K =< B
	;
	    K =< A, A =< L
	;
	    K =< B, B =< L
	).


% ---------------------------------------------------------------------------
:- pred elmt_intersect(i(A,CA, B,CB), i(K,CK, L,CL), R)#"Obtain intersection
of interval in 1-dimensional space. We have several cases where the result
is depend on whether the interval boundaries are open or close. ".
% ---------------------------------------------------------------------------
%all close
elmt_intersect(i(A,1, B,1), i(K,1, L,1), R):-
	max(A, K, E1),
	min(B, L, E2),
	(
	    E1 =< E2
	->
	    R = [i(E1,1, E2,1)] %result close
	;
	    R = []
	).
%two or more open
elmt_intersect(i(A,_, B,_), i(K,0, L,0), R):-
	max(A, K, E1),
	min(B, L, E2),
	(
	    E1 =< E2
	->
	    R = [i(E1,0, E2,0)] %result open
	;
	    R = []
	).
elmt_intersect(i(A,0, B,0), i(K,_, L,_), R):-
	max(A, K, E1),
	min(B, L, E2),
	(
	    E1 =< E2
	->
	    R = [i(E1,0, E2,0)] %result open
	;
	    R = []
	).
%one open result open
%rule of thumb: open when the closed node of open segment is not
%               boundary of the result
elmt_intersect(i(A,0, B,1), i(K,1, L,1), R):-
	max(A, K, E1),
	min(B, L, E2), E2 = L,
	(
	    E1 =< E2
	->
	    R = [i(E1,0, E2,0)] %result open
	;
	    R = []
	).
elmt_intersect(i(A,1, B,0), i(K,1, L,1), R):-
	max(A, K, E1), E1 = K,
	min(B, L, E2),
	(
	    E1 =< E2
	->
	    R = [i(E1,0, E2,0)] %result open
	;
	    R = []
	).
elmt_intersect(i(A,1, B,1), i(K,0, L,1), R):-
	max(A, K, E1),
	min(B, L, E2), E2 = B,
	(
	    E1 =< E2
	->
	    R = [i(E1,0, E2,0)] %result open
	;
	    R = []
	).
elmt_intersect(i(A,1, B,1), i(K,1, L,0), R):-
	max(A, K, E1), E1 = A,
	min(B, L, E2),
	(
	    E1 =< E2
	->
	    R = [i(E1,0, E2,0)] %result open
	;
	    R = []
	).
%one open result close-open
%rule of thumb: close when the closed node of open segment is the
%               boundary of the result. Other node is always open
elmt_intersect(i(A,0, B,1), i(K,1, L,1), R):-
	max(A, K, E1),
	min(B, L, E2), E2 = B,
	(
	    E1 =< E2
	->
	    R = [i(E1,0, E2,1)] %result close-open
	;
	    R = []
	).
elmt_intersect(i(A,1, B,0), i(K,1, L,1), R):-
	max(A, K, E1), E1 = A,
	min(B, L, E2),
	(
	    E1 =< E2
	->
	    R = [i(E1,1, E2,0)] %result close-open
	;
	    R = []
	).
elmt_intersect(i(A,1, B,1), i(K,0, L,1), R):-
	max(A, K, E1),
	min(B, L, E2), E2 = L,
	(
	    E1 =< E2
	->
	    R = [i(E1,0, E2,1)] %result close-open
	;
	    R = []
	).
elmt_intersect(i(A,1, B,1), i(K,1, L,0), R):-
	max(A, K, E1), E1 = K,
	min(B, L, E2),
	(
	    E1 =< E2
	->
	    R = [i(E1,1, E2,0)] %result close-open
	;
	    R = []
	).

% ---------------------------------------------------------------------------
:- pred elmt_split(E1,E2,EC)#"Splits  @var{E1} by inserting @var{E2} inside. 
Precondition @var{E2} subset of @var{E1}. ".
% E1 E1min |---------------|------------|-------------|E1max
% E2              I              II           III
% ---------------------------------------------------------------------------
%close all end point
elmt_split(i(A,1, B,1), i(A,_, B,_), [i(A,1,B,1)]):-!.
% I
elmt_split(i(A,1, B,1), i(A,_, L,_), [i(A,1,L,1),i(L,1,B,1)]):-
	L < B,!.
% II
elmt_split(i(A,1, B,1), i(K,_, L,_), [i(A,1,K,1),i(K,1,L,1), i(L,1,B,1)]):-
	A < K, L < B,!.
% III
elmt_split(i(A,1, B,1), i(K,_, B,_), [i(A,1,K,1),i(K,1,B,1)]):-
	A < K,!.
%close starting point
elmt_split(i(A,1, B,0), i(A,_, B,_), [i(A,1,B,0)]):-!.
% I
elmt_split(i(A,1, B,0), i(A,_, L,_), [i(A,1,L,0),i(L,0,B,0)]):-
	L < B,!.
% II
elmt_split(i(A,1, B,0), i(K,_, L,_), [i(A,1,K,0),i(K,0,L,0), i(L,0,B,0)]):-
	A < K, L < B,!.
% III
elmt_split(i(A,1, B,0), i(K,_, B,_), [i(A,1,K,0),i(K,0,B,0)]):-
	A < K,!.
%close end point
elmt_split(i(A,0, B,1), i(A,_, B,_), [i(A,0,B,1)]):-!.
% I
elmt_split(i(A,0, B,1), i(A,_, L,_), [i(A,0,L,0),i(L,0,B,1)]):-
	L < B,!.
% II
elmt_split(i(A,0, B,1), i(K,_, L,_), [i(A,0,K,0),i(K,0,L,0), i(L,0,B,1)]):-
	A < K, L < B,!.
% III
elmt_split(i(A,0, B,1), i(K,_, B,_), [i(A,0,K,0),i(K,0,B,1)]):-
	A < K,!.

% ---------------------------------------------------------------------------
% +++++++++++++++++++ Interval/Space level operation ++++++++++++++++++++++++
% ---------------------------------------------------------------------------

% ---------------------------------------------------------------------------
:- pred greater_than(I1, I2)#"@var{I1} greater than @var{I2}, and they are
disjoint. The greater than operation is based on the position of element on
the interval. e.g. in intervals
of 3-dimensional [K,L,M] and [P,Q,R],  K and P have the highest priority
and the next position is lower. K > P or K < P, when there's no intersection
between K and P. Intersection is considered as unknown case. However when
at least there is one case where element(I1) > element(I2) we conclude
I1 > I2.

Warning: the definition is very specific of our problem, i.e. checking the
existance of intersection.
".
% ?- greater_than([i(10,1,15,1),i(22,1,24,1)],[i(15,1,115,1),i(5,1,6,1)]).

% yes
% ?- greater_than([i(10,1,15,1),i(22,1,24,1)],[i(16,1,115,1),i(5,1,6,1)]).

% no
% ---------------------------------------------------------------------------
%greater_than(A,B): A>B
greater_than([i(A,_, _B,_)|_], [i(_K,_, L,_)|_]):-
	A > L.
greater_than([El1|I1], [El2|I2]):-
	is_elmt_intersect(El1, El2),
	greater_than(I1,I2).
%greater_than([i(A,B)|I1], [i(K,L)|I2]):-
	% %intersect?
	% (
	%     A =< L, L =< B
	% ;
	%     A =< K, K =< B
	% ;
	%     K =< A, A =< L
	% ;
	%     K =< B, B =< L
	% ),
	% greater_than(I1,I2).

% ---------------------------------------------------------------------------
:- pred qsort(I, ISort)#"Sort interval @var{I}. A sorted interval will
be useful to speed up comparison, particularly when checking whether there
is any intersection among them.".
% ?- qsort([[i(10,1,15,1),i(22,1,24,1)],[i(16,1,115,1),i(5,1,6,1)],[i(10,1,15,1),i(22,1,24,1)]],R).

% R = [[i(10,1,15,1),i(22,1,24,1)],[i(10,1,15,1),i(22,1,24,1)],[i(16,1,115,1),i(5,1,6,1)]] ?
% ---------------------------------------------------------------------------
qsort([X|L],R) :-
	partition(L,X,L1,L2),
	qsort(L2,R2),
        qsort(L1,R1),
        append(R1,[X|R2],R).
qsort([],[]).


partition([],_B,[],[]).
partition([E|R],C,[E|Left1],Right):-
	greater_than(C,E),	%E < C,
	partition(R,C,Left1,Right).
partition([E|R],C,Left,[E|Right1]):-
	\+ greater_than(C,E),   %E >= C,
	partition(R,C,Left,Right1).


% ---------------------------------------------------------------------------
:- pred intersect(I1, I2, R)#"get the intersection between @var{I1} and
@var{I2}. When there is no intersection R=[]" .
% ---------------------------------------------------------------------------
% recc
intersect([E1|I1], [E2|I2], Result):-
	elmt_intersect(E1,E2,RA),
	(
	    RA \= [] ->
	    intersect(I1,I2,R),
	    (
		R \= [] ->
		%Result = [RA|R]
		append(RA,R,Result)
	    ;
		Result = []
	    )
	;
	    Result = []
	).
% base is 1 elmt
intersect([I1],[I2],Result):-
	elmt_intersect(I1,I2,Result).

% ---------------------------------------------------------------------------
:- pred remove(S1, S2, R)#"get the result @var{R} of removing space @var{S2} 
	which intersects with @var{S1} from space @var{S1}. 
The result is a list of space.
".
% ?- remove([i(0,1,10,1),i(0,1,20,1)],[i(0,1,5,1),i(0,1,6,1)],R).
% R = [[i(0,1,5,1),i(6,0,20,1)],[i(5,0,10,1),i(0,1,6,1)],[i(5,1,10,1),i(6,1,20,1)]] ? 
% ---------------------------------------------------------------------------
remove(S1, S2, []):-
	intersect(S1,S2,[]),!.
remove(S1, S2, NewSpaces):-
	intersect(S1,S2,S_Intersect), %S12 \= []
	projection(S1,S_Intersect, ProjectionS),
	build_space(ProjectionS, [], Spaces),
	%preconditions checking can be put here to avoid boundary shift
	batch_shift_boundary(Spaces, S_Intersect, NewSpaces).


% ---------------------------------------------------------------------------
:- pred projection(Orig, Substract, Projection)#"Obtains all segments 
produced by splitting @var{Orig} with @var{Substract}. ".
% ---------------------------------------------------------------------------
projection([], _Substract, []).
projection([OD|Origs], [SD|Subs], [Splits|Proj]):-
	elmt_split(OD,SD,Splits),
	projection(Origs,Subs,Proj).

% ---------------------------------------------------------------------------
:- pred build_space(Segments, Accu,  Spaces)#"@var{Acc} is in reversed order
dimension, thus it must be reversed before returned as @var{Spaces}".
% ?- build_space([[i,j,k],[o,p], [x,y,z]],[],S).

% S = [[i,o,x],[i,o,y],[i,o,z],[i,p,x],[i,p,y],[i,p,z],[j,o,x],[j,o,y],[j,o,z],[j,p,x],[j,p,y],[j,p,z],[k,o,x],[k,o,y],[k,o,z],[k,p,x],[k,p,y],[k,p,z]] ? 

% yes

% ---------------------------------------------------------------------------
%run out child
build_space([], Acc, Spaces):-
	reverse(Acc,Space),
	Spaces = [Space].
%run out sibling
build_space([[]|_],_, []).
%recc
build_space([[I|Is]|Ds], Acc, Spaces):-
	build_space(Ds, [I|Acc], ChildSpaces),
	build_space([Is|Ds], Acc, SiblingSpaces),
	append(ChildSpaces, SiblingSpaces, Spaces).


% ---------------------------------------------------------------------------
:- pred is_adjacent(Space1, Space2)#" Two n-dimensional spaces are adjacent if
on n-1 dimensions they have the same position and on another 1-dimension 
their positions are next to each other.
The code is written in state machine style.

Warning: This code cannot be used for general case. @var{Space1} and 
@var{Space2} are subset of bigger Space. 
Correctness proof of adjacent definition (need to be reviewed):
Space A and B are adjacent if in n-1 dimensions A and B are in the 
_same_position_,  and in the n-th dimension they lie next to each other.
(Note: _same_position_ condition applies when @var{Space1} and 
@var{Space2} are subset of bigger Space. 
If @var{Space1} and @var{Space2} are arbitrary _same_position_ means
_intersect_. For our case we use _same_position_ is enough)
Proof scratch by induction:
1D in 0 dimension _same_position_
   in 1 dimension lie side by side 
2D in 1 dimension _same_position_
   in 1 dimension lie side by side 
3D in 2 dimension _same_position_
   in 1 dimension lie side by side
thus the more dimension they have, the more dimension in _same_position_
nD in n-1 dimension _same_position_
   in 1 dimension lie side by side

".
 %                  State machine
 %   in     +----------+                     (out Code)
 %    |     |          |
 %    +     +          |
 % +--------+----+     | =
 % | same_s  (0) + ----+
 % +-------------+
 %      | \=
 %      |    +---------------+   \=
 %      +---+| adjacent_s (1)|-------------+ (2)
 %           +---------------+
 % 	      +	 	|
 %            |		|=
 % 	      +---------+
% %same
% ?- is_adjacent([i(0,1,3,1),i(0,1,4,1)],[i(0,1,3,1),i(0,1,4,1)]).

% ---------------------------------------------------------------------------
is_adjacent(S1,S2):-
	same_s(S1,S2,1).
same_s(A,A,0).
same_s([i(In,_,Ix,_)|S1], [i(In,_,Ix,_)|S2], Code):-
	same_s(S1,S2,Code).
same_s([i(I1n,_,I1x,_)|S1], [i(I2n,_,I2x,_)|S2], Code):-
	% (   %need further analysis
	%     I1n \= I2n, 
	% ;
	%     I1x \= I2x
	% ),
	%checking one _same_ side is enough 
	I1n \= I2n, 
	(   %adjacent
	    I1n == I2x
	;
	    I1x == I2n
	),
	adjacent_s(S1,S2,Code).
adjacent_s(A,A,1).
adjacent_s([i(I1n,_,_I1x,_)|_S1], [i(I2n,_,_I2x,_)|_S2], 2):-
	%checking one _same_ side is enough 
	I1n \= I2n.
	% (
	%     I1n \= I2n
	% ;
	%     I1x \= I2x
	% ).
adjacent_s([i(In,_,Ix,_)|S1], [i(In,_,Ix,_)|S2], Code):-
	adjacent_s(S1,S2,Code).



% ---------------------------------------------------------------------------
:- pred shift_boundary(Space1, Space2, NewSpace1, Code)#"Change boundary of 
@var{Space1} w.r.t the adjacency with @var{Space2}. The result is given in
@var{NewSpace1}. Depend on the given output @var{Code}, only if they are 
adjacent @var{NewSpace1} will be different from @var{Space1}.
The code is written in state machine style, similar to is_adjacent/2. ".
% ---------------------------------------------------------------------------
shift_boundary(S1,S2,R,1):-
	same_tr(S1,S2,R,1).
shift_boundary(S1,S2,S1,0):-
	same_tr(S1,S2,_,0).
shift_boundary(S1,S2,S1,2):-
	same_tr(S1,S2,_,2).

same_tr(A,A,A,0).
same_tr( [i(In,On,Ix,Ox)|S1], [i(In,_,Ix,_)|S2], [i(In,On,Ix,Ox)|R],Code):-
	same_tr(S1,S2,R,Code).
same_tr([i(I1n,On,I1x,Ox)|S1], [i(I2n,O2n,I2x,O2x)|S2], [NewInterval|R], Code):-
	I1n \= I2n,
	%check in which end point they are adjacent, cannot be both
	% because it is an invalid interval, min > max
	(
	    I1n == I2x,  O2x == 1 ->
	    NewInterval = i(I1n,0,I1x,Ox) %open the adjacent
	;
	    I1n == I2x,  O2x == 0 ->
	    NewInterval = i(I1n,On,I1x,Ox) %don't change
	;
	    I1x == I2n, O2n == 1 ->
	    NewInterval = i(I1n,On,I1x,0) %open the adjacent
	;
	    I1x == I2n, O2n == 0 ->
	    NewInterval = i(I1n,On,I1x,Ox) %don't change
	),
	adjacent_tr(S1,S2,R,Code).
adjacent_tr(A,A,A,1).
adjacent_tr([i(I1n,_,_I1x,_)|_S1], [i(I2n,_,_I2x,_)|_S2], [], 2):- 
%warning just return dummy result because it will not be used
	%checking one _same_ side is enough 
	I1n \= I2n.
	% (
	%     I1n \= I2n
	% ;
	%     I1x \= I2x
	% ).
adjacent_tr([i(In,On,Ix,Ox)|S1], [i(In,_,Ix,_)|S2], [i(In,On,Ix,Ox)|R], Code):-
	adjacent_tr(S1, S2, R, Code).


% ---------------------------------------------------------------------------
:- pred batch_shift_boundary(Spaces, AdjacentSpace, NewSpaces)#"shifts 
boundary of a list of space @var{Spaces}. We also remove the the space which 
is the same as @var{AdjacentSpace} from @var{Space}".
% ---------------------------------------------------------------------------
batch_shift_boundary([], _AdjacentSpace, []).
batch_shift_boundary([Sp|Spaces], AdjacentSpace, Result):-
	shift_boundary(Sp, AdjacentSpace, NewSp1, Code),
	(
	    Code \= 0 ->
	    Result = [NewSp1|NewSpaces]
	;
	    Result = NewSpaces
	),
	batch_shift_boundary(Spaces, AdjacentSpace, NewSpaces).


% ---------------------------------------------------------------------------	
:- pred batch_remove(T, F, Result)#"@var{T} and @var{F} are list of space.
Remove all part of @var{T} which intersect with @var{F}. 
The termination of this code is guaranteed by the decreasing of @var{F}

Warning: This operation is terribly slow.
Complexity analysis scratch: 
Given T with m element and F with n element, at least there are m x n 
interesection test operation. When there's intersection we'll split
T into some other space that will be add into T and then compared with
the subsequence F. 
Therefore T may grow but F always decreases as termination guarantee.
".
% ---------------------------------------------------------------------------
batch_remove(T, [], T).
batch_remove(T, [F1|F], Result):-
	batch_remove_1F(T, F1, Result1F), % may occur Result1 = T
	batch_remove(Result1F, F, Result).

% ---------------------------------------------------------------------------
% ---------------------------------------------------------------------------
batch_remove_1F([], _F1, []).
batch_remove_1F([T1|T], F1, Result):-
	intersect(T1, F1, Isect),
	batch_remove_1F(T, F1, Result1),
	(
	    Isect = [] ->
	    Result=[T1|Result1]
	;
	    remove(T1, Isect, ResultRmv),
	    append(ResultRmv, Result1, Result)
	).

% ------------------------  Evaluation ---------------------------------------------------

% ?- batch_remove([[i(0,1,10,1),i(0,1,20,1)]],[[i(0,1,5,1),i(0,1,6,1)], [i(5,1,12,1),i(10,1,13,1)]],R).

% R = [
% [i(0,1,5,1),i(6,0,10,0)], 1
% [i(0,1,5,0),i(10,0,13,0)], 2
% [i(0,1,5,1),i(13,0,20,1)], 3
% [i(5,1,5,1),i(6,0,10,0)], 4
% [i(5,1,5,1),i(13,0,20,1)], 5
% [i(5,0,10,1),i(0,1,6,1)], 6
% [i(5,1,10,1),i(6,1,10,0)], 7
% [i(5,1,10,1),i(13,0,20,1)] 8
% ] ? 

% yes
% ?- batch_remove([[i(0,1,10,1),i(0,1,20,1)]],[[i(0,1,5,1),i(0,1,6,1)], [i(5,0,12,0),i(10,0,13,0)]],R).

% R = [[i(0,1,5,1),i(6,0,10,0)],[i(0,1,5,1),i(10,0,13,0)],[i(0,1,5,1),i(13,0,20,1)],[i(5,1,5,1),i(6,0,10,0)],[i(5,1,5,1),i(13,0,20,1)],[i(5,0,10,1),i(0,1,6,1)],[i(5,1,10,1),i(6,1,10,1)],[i(5,1,10,1),i(13,1,20,1)]] ? 

% yes
% ?- 


% ?- batch_remove([[i(0,1,10,1),i(0,1,20,1)]],[[i(0,1,5,1),i(0,1,6,1)], [i(6,1,12,1),i(11,1,13,1)]],R).

% R = [
% [i(0,1,5,1),i(6,0,20,1)], 1
% [i(5,0,10,1),i(0,1,6,1)], 2
% [i(5,1,6,1),i(6,1,11,1)], 3
% [i(5,1,6,0),i(11,1,13,1)], 4
% [i(5,1,6,1),i(13,1,20,1)], 5
% [i(6,1,10,1),i(6,1,11,0)], 6
% [i(6,1,10,1),i(13,0,20,1)] 7
% ] ? 

% yes
