
:- use_module(library(lists),[append/3]).

% Esta explicacion no corresponde con este predicado
% normalize(Sentence,SentList,Module) sustitutes Sentence in 
% the program Module that is being compilated by the list of 
% sentences SentList. The result clauses are discontinous.
% It is not expanded the predicate stored_pred/2 to obtain
% the non meta_term of the name of a predicate.
%normalize((Head :- Body),(stored_clause(Head_norm,List_Body_norm)),_):-
%	!, % without this cut all clauses unifies too with the next clause 
%	Head=..[Functor|Old_Args],    % of normalize that is for facts
%	free_list(Old_Args,New_Vars),
%       Head_norm=..[Functor|New_Vars],
%	equal(Old_Args, New_Vars, Listequal),
%	conj_to_list(Body,ListBody),
%	append(Listequal,ListBody,List_Body_norm).

%normalize(Fact, (stored_clause(Fact_norm,Listequal)),_):-
%	Fact=..[Functor|Old_Args],
%	free_list(Old_Args,New_Vars),
%	Fact_norm=..[Functor|New_Vars],
%	equal(Old_Args, New_Vars, Listequal).

% free_list(N,L) returns a list L2 of n free variables
% if L1 is a list of n arguments.
free_list([],[]).
free_list([_Arg|Rest],[_NewVar|L]):-
	free_list(Rest,L).

% equal(Rest,Vars,Eqs) returns in Eqs the list of equalities
% between the elements of Rest and Vars that are in the same
% position of the lists.
equal([],[],[]).
equal([Arg|Rest],[Var|Vars],[(Var=Arg)|Eqs]):-
	equal(Rest,Vars,Eqs).

% conj_to_list(Conj,List) provides in List the elements of
% the conjunction Conj.
conj_to_list((A,B),[A|ListB]):-
	!,
	conj_to_list(B,ListB).
conj_to_list(A,[A]).	

% normalice(Sentence,SentList,Module) sustitutes Sentence in 
% the program Module that is being compilated by the list of 
% sentences SentList. The result clauses are continous
normalize(end_of_file,Cls,_):- 
	!,
	findall(CL,(retract_fact(pre_clause(CL))),Cls,Cls1),
	findall(CL,(retract_fact(pre_stored(CL))),Cls1,Clauses),
	findall(CL,(retract_fact(pre_pred(CL))),Clauses,[stored_pred('dist:dist'(X,Y),dist(X,Y)),end_of_file]).
normalize((Head :- Body), [],_):-
	!,
	assert_pred(Head),
	assertz_fact(pre_clause((Head :- Body))),
	!, % without this cut all clauses unifies too with the next clause 
	Head=..[Functor|Old_Args],    % of normalize that is for facts
	free_list(Old_Args,New_Vars),
        Head_norm=..[Functor|New_Vars],
	equal(Old_Args, New_Vars, Listequal),
	conj_to_list(Body,ListBody),
	append(Listequal,ListBody,List_Body_norm).
        assertz_fact(pre_stored(stored_clause(Head_norm,List_Body_norm))).

normalize(Fact, [], _):-
	assert_pred(Fact),
	assertz_fact(pre_clause(Fact)),
	Fact=..[Functor|Old_Args],
	Fact_norm=..[Functor|New_Vars],
        free_list(Old_Args,New_Vars),
	equal(Old_Args, New_Vars, Listequal).
	assertz_fact(pre_stored(stored_clause(Fact_norm,Listequal))).

% assert_pred(Head) asserts the fact pre_pred(Pred,Pred) if it is not
% asserted yet where Pred is Head with free vars in the arguments
assert_pred(Head):-
	functor(Head,Name,Arity),
	free_list(Arity,Args),
	Pred=..[Name|Args],
	\+ pre_pred(Pred),!,
	assertz_fact(pre_pred(stored_pred(Pred,Pred))).
assert_pred(_Head).



