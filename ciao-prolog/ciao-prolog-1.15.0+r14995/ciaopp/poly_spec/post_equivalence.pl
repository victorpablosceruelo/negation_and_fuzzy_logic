:- module(post_equivalence, [check_equivs/1], [assertions, isomodes]).

:- doc(title,"Checking equivalence of candidate solutions in PCPE").

:- doc(author, "Claudio Ochoa").

:- doc(module," This module performs a post optimization, by
           collapsing those candidate solutions that are equivalent
           (isomorphic).").

:- use_module(customization).
:- use_module(control_pcpe).
:- use_module(spec(sp_clauses), [orig_predicate_names/1]).
:- use_module(spec(spec), [versions/2]).
:- use_module(spec(s_simpspec), [make_atom_list/2, make_atom/2]).
:- use_module(spec(spec_multiple), [reunion_phase/4, split_phase/1]).
:- use_module(spec(min_unf), [clean_up_min_unf_facts/0]).
:- use_module(spec(isomorphism), [qisomorphic_trees/2]).
:- use_module(program(p_unit), [type_of_goal/2]).
:- use_module(library(write)).
:- use_module(library(lists)).
:- use_module(library(sort)).
:- use_module(library(terms_check), [variant/2]).
:- use_module(library(aggregates), [findall/3]).

:- pred check_equivs(+Sols) #"Given a list of solutions @var{Sols}, it
	checks how many of them are equivalent, in the sense that they
	have the same number (and type) of atoms in the global
	control, and their characteristic trees are quasiisomorphic".

check_equivs(Sols):-
	build_code_chtrees(Sols,1,SolTrees),
	length(Sols,L1),	
	write('Amount of sols '),write(L1),nl,
% 	filter_atoms(Sols,FS),
% 	check_equiv_atoms(FS,L2),
	sort_all(SolTrees,SST),
	check_equiv_atoms(SST,L3),	
%	write('Equivs before '),write(L2),nl,
	write('Equivs after '),write(L3),nl.
%	write_solutions(L,1),

check_equiv_atoms(Sols,L):-
	group_by_len(Sols,[],Lsols),
	same_atoms(Lsols,L).

/*
:- pred filter_atoms(+Sols,-) #"Given a list of solutions, it returns
	a list containing list of atoms of every solution".

filter_atoms([],[]).
filter_atoms([e([],V,_)|T],[At|R]):-
	atoms(V,At),
%	complete_atoms(At,S),
	filter_atoms(T,R).

:- pred atoms(+Sol,-) #"Filters all atoms of a given solution
	@var{Sol}".

atoms([],[]).
atoms([t(A,_,_)|T],[A|R]):-
	atoms(T,R).
*/

remove_equivs([],[]).
remove_equivs([LL|T],R):-
	sort(LL,SLL),
	partition_equivs(SLL,L1),
	remove_equivs(T,L2),
	append(L1,L2,R).

partition_equivs([Sol],[Sol]).
partition_equivs([Sol1,Sol2|T],R):-
	same_trees(Sol1,Sol2),!,
	partition_equivs([Sol2|T],R).
partition_equivs([Sol1,Sol2|T],[Sol1|R]):-
	partition_equivs([Sol2|T],R).

same_trees([],[]).
same_trees([(S1,_)|T1],[(S2,_)|T2]):-
	qisomorphic_trees(S1,S2),!,
	same_trees(T1,T2).

add_signatures([],_,[]).
add_signatures([Sol|T],Keys,[(Sign,Sol)|R]):-
	signature(Keys,Sol,Sign),
	add_signatures(T,Keys,R).

signature([],_,[]).
signature([Key|T],Sol,[N|R]):-
	atoms_of_key(Sol,Key,N),
	signature(T,Sol,R).

atoms_of_key([],_,0).
atoms_of_key([(_,[A|_])|R],Key,N):-
	functor(A,Name,Ar),
	make_atom([Name,Ar],Key),!,
	atoms_of_key(R,Key,N1),
	N is N1 + 1.
atoms_of_key([_|R],Key,N):-	
	atoms_of_key(R,Key,N).

sort_all([],[]).
sort_all([H|T],[SH|ST]):-
	sort(H,SH),
	sort_all(T,ST).

:- pred group_by_len(+Sols,+,-) #" Partitions the set of solutions
	@var{Sols} into different groups, where members of every group
	have the same number of atoms in the global control".

group_by_len([],L,L).
group_by_len([S|T],Curr,F):-
	insert_by_len(Curr,S,NC),
	group_by_len(T,NC,F).

insert_by_len([],S,[(L,[S])]):-
	length(S,L).
insert_by_len([(L,LL)|T],S,[(L,[S|LL])|T]):-
	length(S,L).
insert_by_len([H|T],S,[H|NT]):-
	insert_by_len(T,S,NT).


:- pred same_atoms(+,-) # "Returns the number of solutions that could
	be potentially collapsed into one".


same_atoms([],0).
same_atoms([(_,LL)|T],N):-
	same_versions_pred(LL,N1),
	same_atoms(T,N2),
	N is N1 + N2.

:- pred same_versions_pred(+,-) #"Given a group of solutions having
	the same number of atoms in the global control, it adds a
	signature to all of them in order to determine how many
	subgroups there are having the same predicate names for all
	atoms".

same_versions_pred(Sols,N):-
	orig_predicate_names(Preds),
	make_atom_list(Preds,Keys),
	add_signatures(Sols,Keys,NSols),
	length(Sols,L1),
	sort(NSols,SSols),
	group_by_signature(SSols,[],GSols),
	remove_equivs(GSols,FSols),
	length(FSols,L2),
	N is L1 - L2.

group_by_signature([(_,Sol)],L,[[Sol|L]]).
group_by_signature([(Sign,Sol1),(Sign,Sol2)|T],L,R):-
	!,
	group_by_signature([(Sign,Sol2)|T],[Sol1|L],R).
group_by_signature([(_Sign1,Sol1),(Sign2,Sol2)|T],L,[[Sol1|L]|R]):-
	group_by_signature([(Sign2,Sol2)|T],[],R).


:- pred build_code_chtrees(+Sols,+,-) # "Generates the characteristic
	tree of every solution in @var{Sols}".


build_code_chtrees([],_,[]).
build_code_chtrees([e([],Visited,_Value)|Sols],N,[V|R]):-
	reverse(Visited,Visited_r),
	empty_assoc(Empty),
	add_atoms_to_dict(Visited,1,Empty,Dict),
	gen_code(Visited_r,[],Code),	
	compute_parents(Code,Dict,CT),
	clean_up_min_unf_facts,
	orig_predicate_names(Preds),
	make_atom_list(Preds,Keys),
	group_by_preds(Keys,CT),
	split_phase(Keys),
	retrieve_versions(Dict,V),
	N1 is N + 1,
	build_code_chtrees(Sols,N1,R).




add_atoms_to_dict([],_,L,L).
add_atoms_to_dict([t(A,AG,_)|T],Id,Curr_dict,Final_dict):-
	add_assoc(A,AG,Curr_dict,Id,New_dict),
	Nid is Id + 1,
	add_atoms_to_dict(T,Nid,New_dict,Final_dict).


gen_code([],_,[]).
gen_code([t(_A,AG,_GU)|Tuples],Coded,Ch_Trees):-
	member(BG,Coded),
	variant(AG,BG),!,
	gen_code(Tuples,Coded,Ch_Trees).
gen_code([t(A,AG,GU)|Tuples],Coded,[t(A,AG,Res,Ch_Tree)|Ch_Trees]):-
	GU = [(_,Ind_U)|_],
	local(Ind_U,Unfold),
	unfold(Unfold,AG,Res,Ch_Tree),
	gen_code(Tuples,[AG|Coded],Ch_Trees).

:- pred compute_parents/3.

compute_parents(L,D,NL):-
	forward_pass(L,D,1,PL),
	write('Dict '),write(D),nl,
	write('PL '),write(PL),nl,
	build_parent_list(L,PL,D,NL).

forward_pass([],_,_,[]).
forward_pass([t(A,AG,Clauses,_Cht)|T],Dict,RootN,PL):-
	get_assoc(A,Dict,Id),	
	(type_of_goal(exported,AG) ->
	    Cl_num = RootN,
	    length(Clauses,Len),
	    NRN is RootN + Len
	;
	    Cl_num = 1,
	    NRN = RootN),
	parent(Clauses,Cl_num,A,Id,Dict,P),
	forward_pass(T,Dict,NRN,R),
	append(P,R,PL).

parent([],_,_,_,_,[]).
parent([clause(_,B)|T],Cl,Ag,Pid,Dict,PL):-
	functor(Ag,N,A),
	get_parents(B,clid(N,A,Cl,1),Pid,Dict,P),
	Ncl is Cl + 1,
	parent(T,Ncl,Ag,Pid,Dict,R),
	append(P,R,PL).

get_parents([],_,_,_,[]).
get_parents([Ag|T],clid(N,A,Cl,Lit),Pid,Dict,[(Id,(Call,Pid))|R]):-
	get_assoc(Ag,Dict,Id),!,
	make_atom([N,A,Cl,Lit],Call),
	NLit is Lit + 1,
	get_parents(T,clid(N,A,Cl,NLit),Pid,Dict,R).
get_parents([_|T],clid(N,A,Cl,Lit),Pid,Dict,R):-
	NLit is Lit + 1,
	get_parents(T,clid(N,A,Cl,NLit),Pid,Dict,R).
	
build_parent_list([],_,_,[]).
build_parent_list([t(A,Ag,Cl,S)|T],PL,Dict,[([(P,Id)],S,Ag,Cl)|R]):-
	get_assoc(A,Dict,Id),
	findall(L,member((Id,L),PL),P),
	build_parent_list(T,PL,Dict,R).

group_by_preds([],_).
group_by_preds([Key|Preds],L):-
	filter_code_by_key(L,Key,Ls),
	reunion_phase(Ls,Key,false,_), % versions are asserted
	group_by_preds(Preds,L).

filter_code_by_key([],_,[]).
filter_code_by_key([(P,S,Ag,Cl)|T],Key,[(P,S,Ag,Cl)|R]):-
	functor(Ag,N,A),
	atom_number(AN,A),
	make_atom([N,AN],Key),!,
	filter_code_by_key(T,Key,R).
filter_code_by_key([_|T],Key,R):-
	filter_code_by_key(T,Key,R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
write_solutions([],_).
write_solutions([Sol|T],N):-
	write('Solution '),write(N),write(': '),nl,
	write_sol(Sol),nl,
	N1 is N + 1,
	write_solutions(T,N1).

write_sol([]).
write_sol([(K,L)|S]):-
	write(K),write(' '),
	write_preds(L),nl,
	write_sol(S).

write_preds([]).
write_preds([(FC,_)|T]):-
	write_parents(FC),write('-'),
	write_preds(T).

write_parents([]).
write_parents([(Pred,_)|T]):-
	write(Pred),write(','),
	write_parents(T).
*/
retrieve_versions(Dict,Atoms):-
%	first rename preds
	findall((Key,Vers), 
	        current_fact(versions(Key,Vers)),
		L),
	filter_versions(L,Dict,Atoms).

filter_versions([],_,[]).
filter_versions([(_,L)|S],D,V):-
	get_versions(L,D,V1),
	filter_versions(S,D,V2),
	append(V1,V2,V).

get_versions([],_,[]).
get_versions([(FC,S)|T],D,[(S,At)|R]):-
	equiv_atoms(FC,D,At),
	get_versions(T,D,R).


equiv_atoms([],_,[]).
equiv_atoms([(_,Id)|T],D,[Val|R]):-
	inverse_get_assoc(Id,D,Val),
	equiv_atoms(T,D,R).
/*
print_versions:-
	findall(Vers, current_fact(versions(_Key,Vers)), L),
	write(L).

print_atoms([]).
print_atoms([t(_A,AG,UG)|T]):-
	copy_term(AG,AG1),
	numbervars(AG1,0,_),
	write(AG1),write(' - '),write(UG),nl,
	print_atoms(T).

process_chtrees(Ch_Trees):-
	length(Ch_Trees,L),
	write('Printing '),write(L),write(' trees'),nl,
	print_ch_tree(Ch_Trees,1).

print_ch_tree([],_).
print_ch_tree([H|T],N):-
	write(N),write(':'),write(H),nl,
	N1 is N + 1,
	print_ch_tree(T,N1).
*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

empty_assoc([]).

add_assoc(A,Ag,Curr_dict,Id,[t(A,Ag,Id)|Curr_dict]):-
	\+ get_assoc(A,Curr_dict,Id).

get_assoc(L,Dict,Id):-
	functor(L,Functor,Arity),
	functor(NL,Functor,Arity),
	member(t(NL,_,I),Dict),
	variant(NL,L),!,
	I=Id.


inverse_get_assoc(Id,Dict,L):-
	member(t(L,_,Id),Dict).
