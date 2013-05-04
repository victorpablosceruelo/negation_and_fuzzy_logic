:- module(chtrees_pcpe,[test/0, check_ndnl/2, count_atoms/2]).

:- use_package(assertions).
:- use_module(spec(sp_clauses), [orig_clause/3]).


:- pred count_atoms(+Cht,-Atoms) # "Returns an estimation of the
	number of @var{Atoms} to be created in the SLD tree
	represented by @var{Cht}.".

count_atoms(_,0):-!.
count_atoms(Cht,N):-
	prettyvars(Cht),
	meta_count_atoms([Cht],N).


meta_count_atoms([],0).
meta_count_atoms([Cht|T],N):-
	count_atoms_chtree(Cht,N1),
	meta_count_atoms(T,N2),
	N is N1 + N2.



count_atoms_chtree([],0):-!.
count_atoms_chtree([Path],N):-!, % deterministic: count body atoms
	body_sizes(Path,N).
count_atoms_chtree(Cht,Atoms):-
	next_branches(Cht,Br),
	add_body_atoms(Br,N1),
	init_partition(Br,InitP),
	partition(Cht,InitP,P),
	end_partition(P,EndP),
	length(Br,Heads),
	meta_count_atoms(EndP,N2),
	Atoms is N1 + N2 + Heads - 1.



add_body_atoms([],0).
add_body_atoms([A|T],N):-
	body_sizes([A],N1),
	add_body_atoms(T,N2),
	N is N1 + N2.


next_branches(Cht,Br):-
	next(Cht,NB),
	sort(NB,Br).

next([[A|_]],[A]).
next([[A|_]|T],[A|R]):-
	next(T,R).

init_partition([],[]).
init_partition([A|T],[(A,[])|R]):-
	init_partition(T,R).

end_partition([],[]).
end_partition([(_,P)|T],[P|R]):-
	end_partition(T,R).

partition([],P,P).
partition([Path|T],CP,P):-
	Path=[Descriptor|Tail],
	insert_into_partition(CP,Descriptor,Tail,NP),
	partition(T,NP,P).

insert_into_partition([(D,L)|T],D,Tail,[(D,[Tail|L])|T]):-!.
insert_into_partition([C|T],D,Tail,[C|R]):-
	insert_into_partition(T,D,Tail,R).


:- pred body_sizes(+Col,-N) # "Returns the amount of literals @var{N}
	in the body of clauses in @var{Col}".

body_sizes([],0).
body_sizes([(_:Cl)|T],N):-
	number(Cl),!,
	orig_clause(_,Body,Cl),
	length(Body,N1),
	body_sizes(T,N2),
	N is N2 + N1 -1 .
body_sizes([_|T],N):-
	body_sizes(T,N).
	



:- pred check_ndnl(+Tree, -B) #"Checks whether the characteristic tree
	@var{Tree} contains nondeterministic decisions taken using
	non-leftmost unfolding rules, returning in @var{B} either
	@tt{0} or an estimation of the amount of times a call is
	duplicated".



check_ndnl([_],0):-!.
check_ndnl(Chtree,V):-
	\+ all_ones(Chtree),!,
	check_nd(Chtree,0,V).
check_ndnl(_,0).


all_ones([]):-!.
all_ones(Chtree):-
	findall(C,(
	       member(P,Chtree),
	       member((C:_),P)),
	       L),
	sort(L,LS),!,
	LS == [1].

check_trees([],N,N).
check_trees([T|Ts],N1,N):-
	check_nd(T,0,N2),
	N3 is N1 + N2,
	check_trees(Ts,N3,N).

check_nd([],N,N):-!.
check_nd([_],N,N):-!.
check_nd(Chtree,N,V):-
	consume_common_path(Chtree,NCht),
	next_level(NCht,Lev,_),
	sort(Lev,SLev),
	nonleftmost(SLev,S),
	partition_for(SLev,NCht,Forest),
	delete_empty(Forest,NF),
	check_trees(NF,0,V1),
	V is V1 + S + N.

partition_for([],[],[]).
partition_for([A|As],Paths,[Tree|T]):-
	extract_tree(Paths,A,Tree,Rest),
	partition_for(As,Rest,T).


extract_tree([],_,[],[]).
extract_tree([[A|As]|T],A,[As|Tree],Rest):-!,
	extract_tree(T,A,Tree,Rest).
extract_tree([B|T],A,Tree,[B|Rest]):-
	extract_tree(T,A,Tree,Rest).

	


consume_common_path(Cht,R):-
	next_level(Cht,Lev,Next),
	common(Lev),!,
	delete_empty(Next,NN),
	consume_common_path(NN,R).
consume_common_path(Cht,Cht).
	
delete_empty([],[]).
delete_empty([[]|T],R):-!,
	delete_empty(T,R).
delete_empty([A|T],[A|R]):-
	delete_empty(T,R).


next_level([[A|R]],[A],[R]).
next_level([[]|T],R,S):-!,
	next_level(T,R,S).
next_level([[A|C]|T],[A|R],[C|S]):-
	next_level(T,R,S).


common(L):-
	sort(L,S),
	S = [_],!.
common(L):-
	common_(L).

common_([]).
common_([(_:C)|T]):-
	\+ int(C),
	common_(T).

nonleftmost(Paths,0):-
	all_ones([Paths]),!.
nonleftmost([(Pos:_)|T],S):-
	length(T,Cl),
	S is (Pos - 1)*Cl.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test:-
	trees(T),
	test_all(T).

test_all([]).
test_all([T|Ts]):-
	check_ndnl(T,_),
%	display('ndnl '),display(V),nl,
%	count_atoms(T,A),
%	display('atoms '),display(A),nl,
	test_all(Ts).

trees([
[[:(1,10)]],
[[:(1,3)],[:(1,4)],[:(1,5)],[:(1,6)],[:(1,7)],[:(1,8)]],
[[:(1,1)],[:(1,2),:(2,3),:(1,1),:(1,1),:(1,6)],[:(1,2),:(2,4)],[:(1,2),:(2,5)]],
[[:(1,1)],[:(1,2),:(1,3),:(1,1),:(1,1),:(2,6)],[:(1,2),:(1,4)],[:(1,2),:(1,5)]],
[[:(1,2),:(2,3)],[:(1,2),:(2,4)],[:(1,2),:(2,5)],[:(1,2),:(2,6)],[:(1,2),:(2,7)],[:(1,2),:(2,8)]],
[[:(1,5),:(1,(>(4,1),[])),:(1,5),:(1,(>(5,1),[])),:(1,5),:(1,(>(6,1),[])),:(1,5),:(1,(>(7,1),[])),:(1,3)],[:(1,5),:(1,(>(4,1),[])),:(1,5),:(1,(>(5,1),[])),:(1,5),:(1,(>(6,1),[])),:(1,5),:(1,(>(7,1),[])),:(1,4)],[:(1,5),:(1,(>(4,1),[])),:(1,5),:(1,(>(5,1),[])),:(1,5),:(1,(>(6,1),[])),:(1,5),:(1,(>(7,1),[])),:(2,5)]],
[[:(1,2),:(1,4),:(2,3),:(2,1),:(2,2),:(2,3),:(2,1),:(2,1),:(2,6),:(2,7),:(2,6)],[:(1,2),:(1,4),:(2,4)],[:(1,2),:(1,4),:(2,5)],[:(1,2),:(1,5),:(2,3),:(2,2),:(2,3),:(2,1),:(2,1),:(2,6),:(2,1),:(2,6)],[:(1,2),:(1,5),:(2,4)],[:(1,2),:(1,5),:(2,5)]],
[[:(1,5),:(1,(>(7,6),[])),:(1,4),:(2,3)],[:(1,5),:(1,(>(7,6),[])),:(1,4),:(2,4)],[:(1,5),:(1,(>(7,6),[])),:(1,4),:(2,5)],[:(1,5),:(1,(>(7,6),[])),:(1,5),:(2,3)],[:(1,5),:(1,(>(7,6),[])),:(1,5),:(2,4)],[:(1,5),:(1,(>(7,6),[])),:(1,5),:(2,5)]],
[[:(1,1),:(1,9),:(1,11),:(1,(>(2,0),[])),:(1,(is(_73471,1),[=(_73471,1)])),:(2,11),:(2,(>(1,0),[])),:(2,(is(_73503,0),[=(_73503,0)])),:(3,10),:(3,2),:(4,3)],[:(1,1),:(1,9),:(1,11),:(1,(>(2,0),[])),:(1,(is(_73344,1),[=(_73344,1)])),:(2,11),:(2,(>(1,0),[])),:(2,(is(_73376,0),[=(_73376,0)])),:(3,10),:(3,2),:(4,4)],[:(1,1),:(1,9),:(1,11),:(1,(>(2,0),[])),:(1,(is(_73217,1),[=(_73217,1)])),:(2,11),:(2,(>(1,0),[])),:(2,(is(_73249,0),[=(_73249,0)])),:(3,10),:(3,2),:(4,5)],[:(1,1),:(1,9),:(1,11),:(1,(>(2,0),[])),:(1,(is(_73090,1),[=(_73090,1)])),:(2,11),:(2,(>(1,0),[])),:(2,(is(_73122,0),[=(_73122,0)])),:(3,10),:(3,2),:(4,6)],[:(1,1),:(1,9),:(1,11),:(1,(>(2,0),[])),:(1,(is(_72963,1),[=(_72963,1)])),:(2,11),:(2,(>(1,0),[])),:(2,(is(_72995,0),[=(_72995,0)])),:(3,10),:(3,2),:(4,7)],[:(1,1),:(1,9),:(1,11),:(1,(>(2,0),[])),:(1,(is(_72838,1),[=(_72838,1)])),:(2,11),:(2,(>(1,0),[])),:(2,(is(_72870,0),[=(_72870,0)])),:(3,10),:(3,2),:(4,8)]],
[[:(1,2),:(1,2),:(1,2),:(1,2),:(1,2),:(1,2),:(1,2),:(1,2),:(1,2),:(1,2),:(1,1),:(1,3),:(1,4),:(1,3),:(1,4),:(1,4),:(1,3),:(1,4),:(1,4),:(1,4),:(1,3),:(1,4),:(1,4),:(1,4),:(1,4),:(1,3),:(1,4),:(1,4),:(1,4),:(1,4),:(1,4),:(1,3),:(1,4),:(1,4),:(1,4),:(1,4),:(1,4),:(1,4),:(1,3),:(1,4),:(1,4),:(1,4),:(1,4),:(1,4),:(1,4),:(1,4),:(1,3),:(1,4),:(1,4),:(1,4),:(1,4),:(1,4),:(1,4),:(1,4),:(1,4),:(1,3),:(1,4),:(1,4),:(1,4),:(1,4),:(1,4),:(1,4),:(1,4),:(1,4),:(1,4),:(1,3)],[:(1,2),:(1,2),:(1,2),:(1,2),:(1,2),:(1,2),:(1,2),:(1,2),:(1,2),:(1,2),:(1,2)]]
]).
