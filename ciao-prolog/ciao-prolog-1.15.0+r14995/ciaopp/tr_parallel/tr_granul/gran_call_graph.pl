:- module(gran_call_graph, [find_gran_call_field/4], [assertions]). 

:- doc(author,"Pedro L@'{o}pez").  

:- doc(module,"Procedures for handling the call graph used for granularity control.").  

%  
%  The structure of the call graph:
%	cg(Pred/Arity,edge,type,component).
%

%
%  Insert an entry Entry for predicate Pred in call graph CG.
%  If it is already in, return the entry as Entry; 
%  otherwise an entry is inserted and returned.
%

/*
insert_gran_call_entry(CG,Pred,Entry) :-
	var(CG),
	Entry = cg(Pred,_,_,_),
	CG = [Entry|_].
insert_gran_call_entry(CG,Pred,Entry) :-
	nonvar(CG),
	CG = [Entry|_],
	Entry = cg(Pred,_,_,_).
insert_gran_call_entry(CG,Pred,Entry) :-
	nonvar(CG),
	CG = [E|G],
	E \== cg(Pred,_,_,_),
	insert_gran_call_entry(G,Pred,Entry).

%
%  Insert a literal into the edge field of the call graph.
%
insert_gran_call_field(CG,Pred,edge,Lit) :-
	insert_gran_call_entry(CG,Pred,cg(Pred,EdgeList,_,_)),
	insert_gran_call_edge(EdgeList,Lit).
%
%  Insert the type for predicate Pred into the call graph.
%  
insert_gran_call_field(CG,Pred,type,Type) :-
	insert_gran_call_entry(CG,Pred,cg(Pred,_,Type,_)).
%
%  Insert a component number into the component field of the call graph.
%
insert_gran_call_field(CG,Pred,component,Component) :-
	insert_gran_call_entry(CG,Pred,cg(Pred,_,_,Component)).

insert_gran_call_edge(EdgeList,Lit) :-
	var(EdgeList),
	EdgeList = [Lit|_].
insert_gran_call_edge(EdgeList,Lit) :-
	nonvar(EdgeList),
	EdgeList = [Lit|_].
insert_gran_call_edge(EdgeList,Lit) :-
	nonvar(EdgeList),
	EdgeList = [E|Edge],
	E \== Lit,
	insert_gran_call_edge(Edge,Lit).
*/

%
%  Find the entry for predicate Pred in call graph.
%  If it is in, return the entry as Entry; 
%  otherwise Entry is returned as a variable.
%
find_gran_call_entry(CG,_,_) :-
	var(CG).
find_gran_call_entry(CG,Pred,Entry) :-
	nonvar(CG),
	CG = [Entry|_],
	Entry = cg(Pred,_,_,_).
find_gran_call_entry(CG,Pred,Entry) :-
	nonvar(CG),
	CG = [E|G],
	E \== cg(Pred,_,_,_),
	find_gran_call_entry(G,Pred,Entry).

%
%  Find a field for the predicate Pred in call graph.
%
find_gran_call_field(CG,Pred,edge,EdgeList) :-
	find_gran_call_entry(CG,Pred,cg(Pred,EdgeList,_,_)).
find_gran_call_field(CG,Pred,type,Type) :-
	find_gran_call_entry(CG,Pred,cg(Pred,_,Type,_)).
find_gran_call_field(CG,Pred,component,Component) :-
	find_gran_call_entry(CG,Pred,cg(Pred,_,_,Component)).


