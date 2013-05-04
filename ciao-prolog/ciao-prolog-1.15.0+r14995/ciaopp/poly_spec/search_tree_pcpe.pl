:- module(search_tree_pcpe, [ gen_tree/1, merge_trees/2, add_ids/7 ], []).

:- use_package(assertions).

:- doc(title,"Generation of Tree of Search Space of PCPE").

:- doc(author, "Claudio Ochoa").

:- doc(module," This module generates a dot file containing the
	tree of the search space of poly-controlled partial
	evaluation.").

:- use_module(program(itf_db), [curr_file/2]).
:- use_module(library(gendot)).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(sort)).
:- use_module(library(lists), [append/3]).
:- use_module(library(write), [prettyvars/1]).

:- pred gen_tree(+Tree) 
	#"Generates a dot file containing a representation of the
	search space as defined in @var{Tree}".

gen_tree(Tree):-
%        display('***** TREE *****'),nl,
	nonvar(Tree),
	curr_file(F,_),
	atom_concat([Base,'.pl'],F),
	preproc_labels(Tree,NT),
	gendot(NT,Base,tree).
gen_tree(_).

:- pred preproc_labels(+InTree,-OutTree) 
	#"This is code specific for treatment of predicate names,
         removes the module name, and pretty prints the variables".

preproc_labels([],[]).
preproc_labels([(Id,Label,Childs)|T],[(Id,'',Childs)|NT]):-
	var(Label),!,
	preproc_labels(T,NT).
preproc_labels([(Id,Label,Childs)|T],[(Id,NLabel,Childs)|NT]):-
	copy_term(Label,NL),
	NL =.. [N | Args],
	atom_concat([_,':',Name],N),
	NLabel =..[Name | Args],
	prettyvars(NLabel),!,
	preproc_labels(T,NT).
preproc_labels([(Id,Label,Childs)|T],[(Id,Label,Childs)|NT]):-
	preproc_labels(T,NT).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred merge_trees(+,-) 
	#"Takes a list of nodes and merges those trees having the same
         Id into one".

merge_trees(Nodes,Tree):-
%	prettyvars(Trees),
	sort(Nodes,Nodes_s),
	merge_trees_(Nodes_s,Tree).


merge_trees_([],[]).
merge_trees_([(Id,L,C1),(Id,_,C2)|T],T2):-
	!,
	append(C1,C2,C3),
	merge_trees_([(Id,L,C3)|T],T2).
merge_trees_([(Id,L,C1)|T],[(Id,L,C2)|T2]):-
	sort(C1,C2),
	merge_trees_(T,T2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Tree is generated ONLY if output_info is high

:- pred add_ids(+,+,+,-,-,-,-) #"It assigns an id to be later shown in
	the generated graphical tree (dot file)".

add_ids([],Label,Mid,NID,[(NID,Label)],[(NID,AMid,[])],[]):-
	atom_number(AMid,Mid),
	NID is Mid + 1.
add_ids([A|T],Label,Mid,NID,[(NID,Label)],[],[(A,NID)|T]):-
	NID is Mid + 1.

