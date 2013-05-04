:- module(common_spec,
	[ 
	    complete_configurations/6,
	    filter_atoms/2,
	    filter_covered/3,
	    initial_id/2
	],
	[]).

:- use_package(assertions).

:- doc(title,"Common code for bf and df pcpe").

:- doc(author, "Germ@'{a}n Puebla").
:- doc(author, "Claudio Ochoa").

:- doc(module," This module contains code that is shared by both
	traversals of the search space of pcpe, breadth-first and
	depth-first.").

:- use_module(db_pcpe).
:- use_module(search_tree_pcpe, [add_ids/7]).
:- use_module(oracle, [assess/3]).
:- use_module(oracle_calibration).
:- use_module(spec(spec_support), [non_static/1]).
:- use_module(ciaopp(preprocess_flags), 
	[ current_pp_flag/2 ]).
:- use_module(program(p_unit), [type_of_goal/2]).
:- use_module(library(terms_check), 
	[ variant/2, instance/2 ]).
:- use_module(library(lists), [append/3]).

:- pred complete_configurations(+ConfsIn,+TV,+V,+,-ConfsOut,-Tree) #
	"Takes a list @var{ConfsIn} of configurations, retrieves the
	atoms in the leaves of every SLD tree of every configuration,
	and builds the new configurations @var{ConfsOut} to be
	processed in later steps of @tt{PCPE}. These atoms are
	filtered from already visited atoms contained in @var{V} and
	from atoms already known to be processed later, contained in
	@var{TV}. A representation of the search space is returned in
	@var{Tree}. ".


complete_configurations(ConfsIn,TV,V,Int,ConfsOut,Tree):-
	start_siblings,
	complete_configurations_(ConfsIn,TV,V,Int,ConfsOut,Tree),
	end_siblings.


complete_configurations_([],_To_Visit,_Visited,_,[],nodes(Mid,Mid,[],[])):-!.
complete_configurations_([t(A,AG,GU,Tree,Unf_stats)|Filtered],To_Visit,Visited,Int,[C|Confs],nodes(MId,NmId,Childs,Sols)):-
	C=e(New_To_Visit,New_Visited,Value),
	New_Visited = [t(A,AG,GU)|Visited],
	leaves(Tree,Leaves),
	filter_visited(Leaves,New_Visited,NL),
	(Int == intermediate ->
	     filter_covered(NL,New_Visited,New_Leaves)
	;    New_Leaves = NL),
	(current_pp_flag(poly_filter_equiv,on) ->
	    filter_variant(New_Leaves,To_Visit,Filtered_Leaves)
	;
	    Filtered_Leaves = New_Leaves),
	append(Filtered_Leaves,To_Visit,NTo_Visit),%%% Queue vs Stack?? 
	((current_pp_flag(poly_strategy,oracle);oracle_calibration(second)) ->
	    assess(o(A,New_Visited,Tree,Unf_stats,Filtered_Leaves),write,Value);Value=''),
	add_ids(NTo_Visit,Value,MId,Tmp_Id,Childs1,Sols1,New_To_Visit),
	(current_pp_flag(output_info,high) ->    
	    append(Childs1,Childs2,Childs),
	    append(Sols1,Sols2,Sols)
	;
	    true),
	complete_configurations_(Filtered,To_Visit,Visited,Int,Confs,nodes(Tmp_Id,NmId,Childs2,Sols2)).



:- pred filter_covered(+A,+B,-C) : (list(A) , list(B) , list(C)) #
	"Returns in @var{C} all covered atoms from @var{A}, i.e.,
	atoms not being instances of generalized atoms in @var{B}".

filter_covered([],_,[]).
filter_covered([A|T],Vis,R):-
	functor(A,Name,Arity),
	functor(B,Name,Arity),
	member(t(_,B,_),Vis),
	instance(A,B),!,
	filter_covered(T,Vis,R).
filter_covered([A|T],Vis,[A|R]):-
	filter_covered(T,Vis,R).

:- pred filter_visited(+A,+Vis,-B) : (list(A) , list(Vis) , list(C)) #
	"Takes a list of atoms @var{A}, and collects into a list
	@var{B} those atoms do not belonging to any configuration in
	@var{Vis}".

filter_visited(A,Vis,B):-
	filter_atoms(Vis,Atoms),
	filter_variant(A,Atoms,B).

:- pred filter_variant(+A,+B,-C) : (list(A) , list(B) , list(C)) #
	"Takes a list of atoms @var{A} and builds a new list @var{C}
	containing those atoms not variant of any atom in @var{B}".

filter_variant([],_,[]).
filter_variant([A|As],Atoms,New_Atoms):-
	member(B,Atoms),
	variant(A,B),!,
	filter_variant(As,Atoms,New_Atoms).
filter_variant([A|As],Atoms,[A|New_Atoms]):-	
	filter_variant(As,[A|Atoms],New_Atoms).

:- pred filter_atoms(A,B) : list * term => list * list # "Takes a list
	of temporary configurations @var{A} and builds a new list
	@var{B} containing the atoms of @var{A}".

filter_atoms([],[]).
filter_atoms([t(A,_,_)|T],[A|R]):-
	filter_atoms(T,R).

:- pred leaves(+Resultants,-Atoms) # "Collects a set @var{Atoms} of
          leaves from a given SLD tree @var{Resultants}".

leaves(Resultants,Atoms):-
	leaves_all_resultants(Resultants,[],Atoms).

leaves_all_resultants([],Atoms,Atoms).
leaves_all_resultants([R|Resultants],Some_Atoms,Atoms):-
	get_leaves(R,More_Atoms),
	append(More_Atoms,Some_Atoms,Tmp_Atoms),
	leaves_all_resultants(Resultants,Tmp_Atoms,Atoms).

get_leaves(clause(_,Body),Leaves):-
	get_leaves_body(Body,Leaves).

get_leaves_body([],[]).
get_leaves_body([\+Atom|Body],[CAtom|More_Leaves]):-
	copy_term(Atom,CAtom),!,
	get_leaves_body(Body,More_Leaves).
get_leaves_body([Atom|Body],Leaves):-
	type_of_goal(imported,Atom),!,
	get_leaves_body(Body,Leaves).
get_leaves_body([Atom|Body],Leaves):-
	non_static(Atom),!,
	get_leaves_body(Body,Leaves).
get_leaves_body([Atom|Body],[CAtom|More_Leaves]):-
	copy_term(Atom,CAtom),
	get_leaves_body(Body,More_Leaves).



:- true pred initial_id(A,B) : (list(A), term(B)) => (list(A), list(B))
	# "Adds an initial id to the first query in @var{A}. This id
	is later used to build the search space tree representation".

initial_id([],[]).
initial_id([Q|Qs],[(Q,1)|Qs]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


