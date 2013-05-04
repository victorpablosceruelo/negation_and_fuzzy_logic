:- module(_
	,[
	  atomic_types/1,   
	  atomic_type/1,
	  non_atomic_type/1,
	  superclasses/2,
	  ancestor/2,
	  ancestor_or_same/2,
	  type_and_descendants/2,
	  descendants/2,
	  least_common_ancestor/3,
	  field/3,
	  defined/2,
	  reachability/2,

	  retractall_metainfo/0

	 ]
	,[]).

:- discontiguous [subclass/2, defines/2, fields/2].

:- multifile assertall_metainfo/1.

:- multifile subclass/2.
:- multifile fields/2.
:- multifile defines/2.

:- dynamic subclass/2.
:- dynamic fields/2.
:- dynamic defines/2.


:- use_module(library(lists), [append/3]).
:- use_module(library(sort)).
:- use_module(library(sets)).
:- use_module(library(aggregates)).
:- use_module(library(dynamic), [
	                          asserta/1,
                                  retractall/1
                                 ]).

% For debugging purposes, just (un)comment
:- multifile issue_debug_messages/1.
:- data issue_debug_messages/1.
%issue_debug_messages( java_semantics ).


%%%%%%%%%%%%%  Load/unload of metainformation      %%%%%%%%%%%%%%%


retractall_metainfo:- 
	retractall(subclass(_,_)),
	retractall(fields(_,_)),
	retractall(defines(_,_)).

assertall_metainfo(_):-
	asserta(subclass(any,object)),
	asserta(subclass(any,int)),
	asserta(subclass(any,char)),
	asserta(subclass(any,void)).

	

%%%%%%%%%%%%% Basic information about types on Java %%%%%%%%%%%%%%%

atomic_types([int,char,void]).

atomic_type(T):-
	atomic_types(Atomic),
	member(T,Atomic).


% whether the single atom X is an atomic type or not
non_atomic_type(X):-
	atomic_type(X),
	!,
	fail.
non_atomic_type(_).
	     

% subclasses, starting at parent
superclasses(any,[]):- !.
superclasses(C,[AC|RA]):-
	subclass(AC,C),
        superclasses(AC,RA).

% C1 is a (strict) ancestor of C2 (transitive)
ancestor(C1,C2):-
	superclasses(C2,SC),
	member(C1,SC).

% C1 is ancestor of C2 or they are the same
ancestor_or_same(C1,C1):-
	!.
ancestor_or_same(C1,C2):-
	ancestor(C1,C2).


% Given two types T1 and T2, return the least type T st T1<=T, T2<=T 
least_common_ancestor(C,C,C):-!.
least_common_ancestor(C1,C2,An):-
	superclasses(C1,AC1),
	NAC1 = [C1|AC1],
	superclasses(C2,AC2),
	NAC2 = [C2|AC2],
	append(_,[An|_],NAC1),
	append(_,[An|_],NAC2),
	!.


% For a given type, gather all its descendants and the type itself
type_and_descendants(Type,CD):-
	descendants(Type,Des),
	insert(Des,Type,CD).
	

% For a given type, gather all its descendants
descendants(C,SDes):-
	findall(X,subclass(C,X),L1),
	descendants_(L1,L2),
	ord_union(L1,L2,Des),
	sort(Des,SDes).

descendants_([],[]).
descendants_([C|RC],Des):-
	!,
	descendants(C,L1),
	descendants_(RC,L2),
	ord_union(L1,L2,Des).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% set of classes transitively reachable from Class, including itself
% equivalent to find the connected subgraph that includes Class
reachability(Class,Reach):-
	reachability_(Class,[],Reach).

reachability_(Class,Done,[]):-
	ord_member(Class,Done),
	!.
reachability_(Class,Done,Class_Reach):-
	fields(Class,Fields),
	!,
	insert(Done,Class,NDone),
	map_reachability(Fields,NDone,Fields_Reach),
	insert(Fields_Reach,Class,Class_Reach).
reachability_(Class,_,[Class]).


map_reachability([],_,[]).
map_reachability([(_,Type)|RT],Done,Reach):-
	ord_member(Type,Done),
	!,
	map_reachability(RT,Done,Reach).
map_reachability([(_,Type)|RT],Done,Reach):-
	reachability_(Type,Done,Type_Reach),
	ord_union(Done,Type_Reach,NDone),
	map_reachability(RT,NDone,RType_Reach),
	ord_union(Type_Reach,RType_Reach,Reach).



% return all fields of Class, including the inherited ones!
field(Class,Field,K):-
	fields(Class,Fields),
	member((Field,K),Fields),
	!.
field(Class,Field,K):-
	ancestor(Ancestor,Class),
	field(Ancestor,Field,K).


% true if class K defines method Method
defined(K,Method):-
	defines(K,Method).





