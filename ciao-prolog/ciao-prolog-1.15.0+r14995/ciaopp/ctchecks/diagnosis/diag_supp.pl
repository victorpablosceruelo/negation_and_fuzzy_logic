:- module(_,_,[]).
:- use_module(spec(s_simpspec), [make_atom/2]).
:- use_module(program(clidlist), [atom2data/5, clid2data/4]).
:- use_module(program(clause_db), [source_clause/3]).
:- use_module(library(lists), [length/2]).
:- use_module(library(idlists), [member_0/2]).
:- use_module(library(terms_vars), [varset/2]).

% Auxilliary stuff
get_prev_lit(SgKey,PrevKey) :-
	is_clause_id(SgKey),!,
	get_last_lit(SgKey,PrevKey).
get_prev_lit(SgKey,PrevKey) :-
	atom2data(SgKey,F,A,C,G),
	G1 is G - 1,
	G1 > 0,
	make_atom([F,A,C,G1],PrevKey).

get_clause_id(SgKey,ClKey) :-
	atom2data(SgKey,F,A,C,_),
	make_atom([F,A,C],ClKey),!.
get_clause_id(SgKey,SgKey).

find_lit(((Lit:Key),_),Lit:Key) :-!.
find_lit((_,Lits),Lit) :-!,
	find_lit(Lits,Lit).
find_lit((Lit:Key),Lit:Key) :-!.
find_lit(_,Lit) :-
	displayq('NOT FOUND IN BODY'(Lit)),fail.

is_clause_id(Id) :-
	Id \== !,
	\+ atom2data(Id,_,_,_,_),
	clid2data(Id,_,_,Z),
	Z \= 0.

get_last_lit(ClKey,LastKey) :-
	source_clause(ClKey,clause(_,Body),_),
	find_last(Body,LastKey).

find_last((_,B),Last) :-
	find_last(B,Last).
find_last(_:Key,Key).

get_next_lit(SgKey,NextKey) :-
	is_last_lit(SgKey),!,
	get_clause_id(SgKey,NextKey).
get_next_lit(SgKey,NextKey) :-
	atom2data(SgKey,F,A,C,G),
	G1 is G + 1,
	make_atom([F,A,C,G1],NextKey).
get_first_lit_cl(ClKey,FKey) :-
	clid2data(ClKey,F,A,C),
	make_atom([F,A,C,1],FKey).

is_last_lit(SgKey) :-
	get_clause_id(SgKey,ClKey),
	get_last_lit(ClKey,SgKey).


get_var_name(V,[V0|_],[N|_],N) :-
	V == V0,!.
get_var_name(V,[_|Vs],[_|Ns],N) :-
	get_var_name(V,Vs,Ns,N).


guess_vars(B,A) :-
	length(B,Max),
	gen_list(Max,1,[_],A),
	subset(A,B).

subset([A],[A|_]).
subset(S,[A|Ss]) :-
	subset(S1,Ss),
	( S = S1
	; S = [A|S1]
	).


gen_list(_,_,L,L).
gen_list(Max,N1,L,L1):-
	N1 < Max,
	N2 is N1 + 1,
	gen_list(Max,N2,[_|L],L1).


subset_0([],_).
subset_0([S|Ss],Sup) :-
	member_0(S,Sup),
	subset_0(Ss,Sup).


range(ASubs,Range) :-
	varset(ASubs,Range).


get_init_lit(_,Head,Lit,Key) :-
	\+ atom2data(Key,_,_,_,_),!,
	Lit = Head.
get_init_lit(Body,_,Lit,Key) :-
%	get_prev_lit(Key,PKey),
	find_lit(Body,(Lit:Key)).


