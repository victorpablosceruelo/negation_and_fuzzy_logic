% Predicates dependent on the clause representation. 

% Operations over clauses in the format that 
% comes into the non-failure analysis. 

get_head_body_id_clause((clause(Head, Body), Id), Head, Body, Id).


% Determine the type of a clause.
% Clauses are clasified as:
% directive
% fact
% rule
% other
/*
clause_type1(Clause, directive):-
    functor(Clause, (:-), 1), 
    !. 
clause_type1((clause(_H, B), _Id), Type):-
    !,
    (B == true -> 
               Type = fact
               ;  
               Type = rule).
clause_type1(_, other).
*/
clause_type1(directive(D) :_, directive, Clause0) :-
	!,
	Clause0 = (:- D).
clause_type1(clause(H, B) : Id, Type, Clause0) :-
	!,
	( B == true ->
	    Type = fact
	;
	    Type = rule ),
	Clause0 = (clause(H, B), Id).
clause_type1(Clause, directive, Clause0) :-
	functor(Clause, (:-), 1),
	!,
	Clause0 = Clause.
clause_type1((clause(H, B), Id), Type, Clause0) :-
	!,
	( B == true ->
	    Type = fact
	;
	    Type = rule ),
	Clause0 = (clause(H, B), Id).
clause_type1(Cl, other, Cl).


handle_rule(TAB, Rule) :-
	get_head_body_id_clause(Rule, Head, Body, ClauseId),
	varset((Head :- Body), ClVars),
	flat_body_list1(Body, Flat_Body_List),
	functor(Head, F, N),
	translate_to_internal_format(Flat_Body_List, F/N, NFBodyList),
	NFClause = (clause(Head, NFBodyList, ClVars), ClauseId),
	insert_field(TAB, F/N, clause, NFClause).

handle_fact(TAB, Fact) :-
	get_head_body_id_clause(Fact, Head, _Body, ClauseId),
	varset(Head, ClVars),
	NFClause = (clause(Head, [], ClVars), ClauseId),
	functor(Head, F, N),
	insert_field(TAB, F/N, clause, NFClause).

get_key_and_concrete_literal_from_external_literal(ExterLiteral, Key,
	    ConcLiteral) :-
	ExterLiteral == (!),
	!,
	Key = nokey,
	ConcLiteral = (!).
get_key_and_concrete_literal_from_external_literal(ExterLiteral, Key,
	    ConcLiteral) :-
	ExterLiteral = Lit:Id,
	!,
	Key = Id,
	ConcLiteral = Lit.
get_key_and_concrete_literal_from_external_literal(ExterLiteral, Key,
	    ConcLiteral) :-
	Key = nokey,
	ConcLiteral = false,
	warning_message(
	    "Literal ~q has been translated into ~q with key value ~q", [
		ExterLiteral, ConcLiteral, Key]).


%% Operations over clauses in the format used by the non-failure analysis.


get_body_of_clause(Clause, Body) :-
	Clause = (clause(_Head, Body, _ClVars), _ClauseId).
get_head_of_clause(Clause, Head) :-
	Clause = (clause(Head, _Body, _ClVars), _ClauseId).
get_head_and_body_of_clause(Clause, Head, Body) :-
	Clause = (clause(Head, Body, _ClVars), _ClauseId).
get_body_and_vars_of_clause(Clause, Body, ClVars) :-
	Clause = (clause(_Head, Body, ClVars), _ClauseId).

there_are_no_more_clauses(Clauses) :- var(Clauses).

get_first_clause_id(Clauses, Id) :-
	nonvar(Clauses),
	Clauses = [(_Clause, Id)|_].

% there_are_no_more_clauses(Clauses):- nonvar(Clauses), Clauses = []. 
