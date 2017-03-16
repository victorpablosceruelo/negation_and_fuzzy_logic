:- module(transform,
	[ cleanup_clauses/0,
	  determine_r_flag/3,
	  read_clause/3,
	  transform_clauses/5,
	  body_info0/4
	],
	[ assertions ]).

:- use_module(library(sets), [ord_member/2]).
:- use_module(library(terms_vars), [varset/2]).

:- use_module(spec(s_simpspec), [make_atom/2]).
:- use_module(program(p_unit), [type_of_goal/2]).
:- use_module(plai(domains), [combined_special_builtin/3, special_builtin/6]).

:- data clause/6.

read_clause(SgKey,RFlag,X):-
	current_fact(clause(SgKey,RFlag,Head,Vars_u,K,Body)),
	X = clause(Head,Vars_u,K,Body).

cleanup_clauses:-
	retractall_fact(clause(_,_,_,_,_,_)).
%-----------------------------------------------------------------------------
% transform_clauses(+,+,+,+)
% transform_clauses(Clauses,Dicts,RecursCl,RecursPs)
% It transforms the program into a suitable format and recorda all clauses
% in the database. 
% If Clause is a directive, the new format is (Clause,DK) and nothing 
% is recordered in the data base
% Otherwise, the new format is (clause(Head,NewBody),Clid) were Clid is the
% atom 'F/A/N', F is the predicate symbol of the head, A is its arity and N is 
% the clause number, and NewBody is the result of substituting each Atom for
% Atom:Key were key is an atom 'F/A/N/M', and M indicates the position of the 
% atom in the clause). 
% Special cases are:
%    - if the clause is a fact, NewBody is 'true'
%    - no :Key is added to cuts 
% The register kept in the database is:
%         recorda_internal(Key,clause(Rflag,Head,Vars,Clid,B))
% were Key is the atom 'F/A', Rflag is the recursiveness flag for the clause
% (r if it is recursive, nr if it is not), and Clid is used as key to identify
% the exit point of the clause.
% B is a list of lists of elements g(Key,Sv,Rflag,SgKey,Sg), one
% for each literal in the body.
%-----------------------------------------------------------------------------
transform_clauses([Clause:ClId|Clauses],[D|Ds],[R|Rs],Ps,AbsInt):-
	clause_info(Clause,ClId,D,R,Ps,AbsInt),
	transform_clauses(Clauses,Ds,Rs,Ps,AbsInt).
transform_clauses([],[],[],_Ps,_AbsInt).

clause_info(directive(_),_,_,_,_,_). %% do nothing...
clause_info(clause(Head,Body),Clid,_D,Rflag,_,_AbsInt):-
%	(Body = true ; Body = (!)), !,  %%%%% Warning
	Body = true, !,
	functor(Head,F,A),
	make_atom([F,A],Key),
	varset(Head,Vars),
%	vars_names_dict(D,Vars,_),
	assertz_fact(clause(Key,Rflag,Head,Vars,Clid,true)).
clause_info(clause(Head,Body),Clid,_D,Rflag,Ps,AbsInt):-
	body_info0(Body,Ps,AbsInt,B),
	functor(Head,F,A),
	make_atom([F,A],Key),
	varset((Head,Body),Vars),
%	vars_names_dict(D,Vars,_),
%	display(clause(Key,Rflag,Head,Vars,Clid,B)), nl,
	assertz_fact(clause(Key,Rflag,Head,Vars,Clid,B)).

body_info0((Atom,Atoms),Ps,AbsInt,Ats):- !,
	Ats = (At,Ats0),
	body_info0(Atom,Ps,AbsInt,At),
	body_info0(Atoms,Ps,AbsInt,Ats0).
body_info0(Atom:Key,Ps,AbsInt,At):- !,
	atom_info(Atom,Key,Ps,AbsInt,At).
body_info0((!),Ps,AbsInt,At):-
	atom_info((!),(!),Ps,AbsInt,At).

atom_info(Subgoal,Id,_Ps,_AbsInt,Goal) :-
	var(Subgoal), !,
	Goal=g(Id,Subgoal,'$var',no,Subgoal).
atom_info(Subgoal,Id,Ps,AbsInt,g(Id,Svars,Info,SgKey,Goal)) :-
	varset(Subgoal,Svars),
	functor(Subgoal,F,A),
	make_atom([F,A],SgKey),
	atom_meta_builtin_info(SgKey,Subgoal,Ps,AbsInt,Info,Goal).

atom_meta_builtin_info(_SgKey,Subgoal,Ps,AbsInt,Info,Goal):-
	type_of_goal(metapred(Type,_Meta),Subgoal),
	type_of_goal(imported,Subgoal), !,
	Info = '$meta'(Type,Bodies,Data),
	functor(Subgoal,F,A),
	functor(Goal,F,A),
	meta_info(Subgoal,A,Ps,AbsInt,Bodies,Data,Goal).
atom_meta_builtin_info(SgKey,Subgoal,Ps,AbsInt,Info,Goal):-
	combined_special_builtin(AbsInt,Subgoal,SgKey,Domains), !,
	Goal = Subgoal,
	map_atom_builtin_info(Domains,SgKey,Subgoal,Ps,Info).
atom_meta_builtin_info(SgKey,Subgoal,Ps,AbsInt,Info,Subgoal):-
	atom_builtin_info(SgKey,Subgoal,Ps,AbsInt,Info).

map_atom_builtin_info([],_SgKey,_Subgoal,_Ps,[]).
map_atom_builtin_info([AbsInt|Domains],SgKey,Subgoal,Ps,[I|Info]):-
	atom_builtin_info(SgKey,Subgoal,Ps,AbsInt,I),
	map_atom_builtin_info(Domains,SgKey,Subgoal,Ps,Info).

atom_builtin_info(SgKey,Subgoal,_Ps,AbsInt,Info):-
	builtin_info(Subgoal,SgKey,AbsInt,Type,TypeGoal,Condvars), !,
%	type_of_goal(imported,Subgoal), !,
	Info = '$built'(Type,TypeGoal,Condvars).
atom_builtin_info(_SgKey,Subgoal,Ps,_AbsInt,Rflag):-
	functor(Subgoal,F,A),
	determine_r_flag(Ps,F/A,Rflag).

builtin_info(Subgoal,_SgKey,AbsInt,Type,TypeGoal,Condvars):-
	type_of_goal(builtin(TypeGoal),Subgoal),
	functor(TypeGoal,TF,TA),
	make_atom([TF,TA],SgKey),
	special_builtin(AbsInt,SgKey,TypeGoal,Subgoal,Type,Condvars), !.
builtin_info(Subgoal,SgKey,AbsInt,Type,Subgoal,Condvars):-
	special_builtin(AbsInt,SgKey,Subgoal,Subgoal,Type,Condvars).

combined_special_builtin(AbsInt,Subgoal,_SgKey,Domains):-
	type_of_goal(builtin(TypeGoal),Subgoal),
	functor(TypeGoal,TF,TA),
	make_atom([TF,TA],SgKey),
	domains:combined_special_builtin(AbsInt,SgKey,Domains).
combined_special_builtin(AbsInt,_Subgoal,SgKey,Domains):-
	domains:combined_special_builtin(AbsInt,SgKey,Domains).

meta_info(_,0,_Ps,_,[],_,_) :- !.
meta_info(G,A,Ps,AbsInt,Bodies,Data,NewG):-
	A > 0,
	arg(A,G,GA),
	arg(A,NewG,Term),
	( nonvar(GA),
	  GA='$'(Term,Goal,Type)
	-> ( Type=goal
	   -> body_info0(Goal,Ps,AbsInt,Body),
	      Bodies=[Body|Bodies0],
	      Data=Data0
	    ; % Type=data,
	      Data=[Goal|Data0],
	      Bodies=Bodies0 )
	 ; Term=GA,
	   Bodies=Bodies0 ),
	A1 is A-1,
	meta_info(G,A1,Ps,AbsInt,Bodies0,Data0,NewG).

determine_r_flag(notarjan,_P,Rflag):-!,
	Rflag = r. % if no tarjan done we have to assume recursive...
determine_r_flag(Ps,P,Rflag):-
	ord_member(P,Ps), !,
	Rflag=r.
determine_r_flag(_Ps,_P,nr).





	
	