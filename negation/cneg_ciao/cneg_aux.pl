
:- module(cneg_aux,
	[findall/4, debug/2, debug_list/2, debug_nl/0, msg/2, msg_aux/2, msg_nl/0,
	 first/2, second/2, unify_terms/2, functor_local/4,
	 memberchk_local/2, retrieve_element_from_list/2, term_to_meta/2,
	 setof_local/3, varsbag_local/4, varset_local/2, 
%	 remove_duplicates/3,
	 goal_clean_up/2,
	 goal_is_conjunction/3, goal_is_disjunction/3, 
	 goal_is_disequality/4, goal_is_equality/3,
	 look_for_the_relevant_clauses/2, frontier_contents/4,
	 qualify_string_name/3, remove_qualification/2, term_name_is_qualified/1,
	 replace_in_term_var_by_value/4, % replace_in_args_var_by_value/4,
	 replace_in_term_variables_by_values/4,
	 add_to_list_if_not_there/3, append/3,
	 var_is_in_formulae/2, 
	 terms_are_equal/2 ],[]).

:- use_module(library(aggregates),[setof/3]).
:- use_module(library(write), _).

% To access predicates from anywhere.
:- multifile cneg_processed_pred/4.
:- multifile cneg_dynamic_cl/6.
:- multifile cneg_static_cl/3.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Use the following sentences to enable/disable debugging.
debug_is_on('yes').
% debug_is_on('no').

%%% Debug (by VPC).
debug(Msg, Clause) :-
	debug_is_on('yes'), 
	!, % No backtracking allowed.
	write('% DBG % '),
	write(Msg), 
	write(' :: '),
	write(Clause), nl, 
	!. % No backtracking allowed.
debug(_Msg, _Clause) :- 
	debug_is_on('no').

debug_nl :-
	debug_is_on('yes'),
	!, nl.
debug_nl :- 
	debug_is_on('no'), !.


debug_list(_Msg, []) :-
	!. % No backtracking allowed.
debug_list(Msg, [Cl]) :-
	!, % No backtracking allowed.
	debug(Msg, Cl).
debug_list(Msg, [Cl|Cls]) :- 
	!, % No backtracking allowed.
	debug(Msg, Cl),
	debug_list(Msg, Cls).

msg(Msg1, Msg2) :-
	msg_aux(Msg1, ' :: '),
	msg_aux(Msg2, ' '), 
	msg_nl, !.

msg_aux(Msg1, Msg2) :-
	write(Msg1), 
	write(Msg2), 
	!. % No backtracking allowed.
msg_nl :- nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- meta_predicate findall(?,goal,?,?).
findall(Template, Generator, List, Tail) :-
        save_solutions(-Template, Generator),
        list_solutions(List, Tail).

:- data solution/1.
:- meta_predicate save_solutions(?,goal).
save_solutions(Template, Generator) :-
        asserta_fact(solution('-')),
        call(Generator),
        asserta_fact(solution(Template)),
        fail.
save_solutions(_,_).

list_solutions(List, Tail) :-
        retract_fact(solution(Term)), !,
        list_solutions_aux(Term,Tail,List).
list_solutions_aux('-',L,L) :- !.
list_solutions_aux(-Term,Sofar,List) :-
        retract_fact(solution(NewTerm)), !,
        list_solutions_aux(NewTerm, [Term|Sofar],List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

first([Head|_Tail], Head) :- !.
second([_Head|Tail], Second) :- !,
	first(Tail, Second).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%unify_terms(Term1, Term2) :-
%	debug_clause('unify_terms', unify_terms(Term1, Term2)), 
%	fail.

unify_terms(Term1, Term2) :- 
	var(Term1), 
	var(Term2), !,
	Term2 = Term1.

unify_terms(Term1, Term2) :- 
	functor(Term1, Name, Arity),
	functor(Term2, Name, Arity), !,
	Term1=..[Name|Args1],
	Term2=..[Name|Args2],
	unify_list_of_terms(Args1, Args2).

unify_list_of_terms([], []) :- !.
unify_list_of_terms([Arg1], [Arg2]) :- !,
	unify_terms(Arg1, Arg2).
unify_list_of_terms([Arg1|Args1], [Arg2|Args2]) :- !,
	unify_terms(Arg1, Arg2),
	unify_list_of_terms(Args1, Args2).

unify_variables(Term, Term) :- !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_to_list_if_not_there(Elto, List, List) :- memberchk_local(Elto, List), !.
add_to_list_if_not_there(Elto, List, [Elto|List]) :- !.	

% remove_duplicates(L_In, L_Tmp, L_Out) L2 tiene los elementos de L1 sin repetidos 
%remove_duplicates([], L_Out, L_Out).
%remove_duplicates([X|L_In], L_Tmp_In, L_Out):-
%	add_to_list_if_not_there(X, L_Tmp_In, L_Tmp_Out), !,
%	remove_duplicates(L_In, L_Tmp_Out, L_Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% La semantica de memberchk no es igual en todos los Prolog:
% en XSB prolog unifica mientras q en Ciao prolog no.
% Para mantener coherencia (y evitar problemas) lo redefinimos.

% From Ciao Prolog (idlists library).
% memberchk_local(X, [Y|_]) :- X == Y, !.
% memberchk_local(X, [_|L]) :- memberchk_local(X, L).

memberchk_local(T1, [T2]) :-
	terms_are_equal(T1,T2).
memberchk_local(T1, [T2|_L]) :- 
	terms_are_equal(T1,T2).
memberchk_local(T1, [_T2|L]) :- 
        memberchk_local(T1, L).

retrieve_element_from_list([Element|_List], Element).
retrieve_element_from_list([_Other_Element|List], Element) :-
	retrieve_element_from_list(List, Element).

var_is_in_formulae(Var, Formulae) :-
	varsbag_local(Formulae, [], [], Vars_Set), !,
	retrieve_element_from_list(Vars_Set, Var).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

terms_are_equal(T1, T2) :- % Same var
        var(T1),
        var(T2),
        T1==T2,!.
terms_are_equal(T1, T2) :- % Different vars.
        (   var(T1) ;
            var(T2)),
        !, fail. % Fail

terms_are_equal(T1, T2) :- % For functors.
        functor_local(T1, Name, Arity, Args1),
        functor_local(T2, Name, Arity, Args2),
        !,
        terms_are_equal_list(Args1, Args2).

terms_are_equal(_T1, _T2) :- % Other things
%       write(terms_are_equal(T1, T2)), write(' ERROR '), nl,
        !, fail. % Fail

terms_are_equal_list([],[]) :- !. % Empty lists.
terms_are_equal_list([T1|L1],[T2|L2]) :- !, % Lists.
        terms_are_equal(T1, T2),
        terms_are_equal_list(L1,L2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
functor_local(Head, Name, Arity, Args) :-
        functor(Head, Name, Arity),
        Arity > 0, !,
        Head=..[Name|Args].

functor_local(Head, Name, Arity, []) :-
        functor(Head, Name, Arity),
        Arity == 0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% From "/usr/lib/ciao/ciao-1.13/lib/lists.pl"
append([], L, L).
append([E|Es], L, [E|R]) :- append(Es, L, R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

goal_clean_up(Goal, NewGoal) :-
	term_to_meta(Tmp_Goal, Goal), !,
	goal_clean_up(Tmp_Goal, NewGoal).

goal_clean_up(Goal, NewGoal) :-
	functor_local(Goal, Goal_Name, Arity, Arguments),
	name(Goal_Name, Goal_Name_String),
	term_name_is_qualified(Goal_Name_String), !,
	remove_qualification(Goal_Name_String, NewGoal_Name_String),
	name(NewGoal_Name, NewGoal_Name_String), 
	functor_local(NewGoal, NewGoal_Name, Arity, Arguments).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

goal_is_disjunction((G1;G2), G1, G2) :- !.
goal_is_disjunction(Goal, G1, G2) :-
	goal_is_aux('basiccontrol:;', 2, Goal, G1, G2), !.
goal_is_conjunction((G1,G2), G1, G2) :- !.
goal_is_conjunction(Goal, G1, G2) :- 
	goal_is_aux('basiccontrol:,', 2, Goal, G1, G2), !.
goal_is_disequality(dist(X,Y), X, Y, []) :- !.
goal_is_disequality(cneg_diseq(X,Y), X, Y, []) :- !.
goal_is_disequality(cneg_diseq(X,Y,FreeVars), X, Y, FreeVars) :- !.
goal_is_disequality(Goal, X, Y, []) :- 
	goal_is_aux('=/=', 2, Goal, X, Y), !.
goal_is_equality((X=Y), X, Y) :- !.
goal_is_equality(Goal, X, Y) :- 
	goal_is_aux('eq', 2, Goal, X, Y), !.
goal_is_equality(Goal, X, Y) :- 
	goal_is_aux('cneg_eq', 2, Goal, X, Y), !.

goal_is_aux(Name, Arity, Goal, G1, G2) :-
	functor(Goal, Name, Arity), !,
	arg(1, Goal, G1),
	arg(2, Goal, G2).

% Ensure you do this before calling predicates here !!!
%	name(Name, NameString),

qualify_string_name(Qualification, Name, NewName) :-
	fail_if_string_name_has_semicolon(Name), !,
	qualify_string_name_not_qualified(Qualification, Name, NewName).
qualify_string_name(Qualification, Name, NewName) :-
	remove_qualification(Name, NameTmp), 
	qualify_string_name(Qualification, NameTmp, NewName).

fail_if_string_name_has_semicolon(Name) :-
	name_has_semicolon(Name), !, fail.
fail_if_string_name_has_semicolon(_Name) :- !.

qualify_string_name_not_qualified(Qualification, Name, NewName) :-
	string(Qualification),
	string(Name),
	append(Qualification, ":", Prefix), 
	append(Prefix, Name, NewName),
	string(NewName), 
	!.

qualify_string_name_not_qualified(Qualification, Name, NewName) :-
	debug('qualify_string_name_not_qualified :: FAILED (ensure args are string) :: Qualification', Qualification), 
	debug('qualify_string_name_not_qualified :: FAILED (ensure args are string) :: Name', Name), 
	debug('qualify_string_name_not_qualified :: FAILED (ensure args are string) :: NewName', NewName), 
	!.

%	name(Name, NameString),
%	name(Qualification, QualificationString), 
%     ....
%	name(NewName, NewNameString).

term_name_is_qualified(Name) :- name_has_semicolon(Name).
name_has_semicolon(Any) :-
	string(Any), !, 
	name_has_semicolon_aux(Any).
name_has_semicolon(Any) :-
	debug('name_has_semicolon :: NOT a string', Any), fail.

name_has_semicolon_aux([]) :- !, fail.
name_has_semicolon_aux([A]) :- 
	semicolon_string([A]), !.
name_has_semicolon_aux([A|_Others]) :- 
	semicolon_string([A]), !.
name_has_semicolon_aux([_A|Others]) :- !,
	name_has_semicolon_aux(Others).

semicolon_string(":") :- !.
% semicolon_string(Other) :- msg('Other', Other), fail.

remove_qualification(NameIn, NameOut) :-
	name_has_semicolon(NameIn), !,
	remove_qualification_aux(NameIn, NameOut).
remove_qualification(NameIn, NameIn) :- !.
%	debug_clause('WARNING: remove_qualification :: NOT a string qualified', NameIn).

remove_qualification_aux([A|Name], Name) :-
	semicolon_string([A]), !.
remove_qualification_aux([_A|NameIn], NameOut) :-
	remove_qualification_aux(NameIn, NameOut).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% replace_in_term_var_by_value(Term1, Var, Value, Term2) 
% returns Term2 that is Term1 when Var is substituted by Value.
replace_in_term_var_by_value(Term,Var,Value,Value):-
	var(Term), 
	var(Var),
	Term==Var,!.
replace_in_term_var_by_value(Term,Var,_Value,Term):-
	var(Term),
	var(Var),
	Term\==Var,!.
replace_in_term_var_by_value(Term,Var,Value,Term1):-
	var(Var),
	Term=..[Functor|Args],
	replace_in_args_var_by_value(Args,Var,Value,Args1),
	Term1=..[Functor|Args1].

% replace_in_args_var_by_value(Args_In, Var, Value, Args_Out) 
% applies replace_in_term_var_by_value/4 to all the arguments in Args.
replace_in_args_var_by_value([], _Var, _Value, []).
replace_in_args_var_by_value([Arg_In|Args_In], Var, Value, [Arg_Out|Args_Out]):-
	replace_in_term_var_by_value(Arg_In, Var, Value, Arg_Out),
	replace_in_args_var_by_value(Args_In, Var, Value, Args_Out).

% replace_in_term_variables_by_values(LVars,LValues,Term1,Term2) returns in Term2 the same term
% Term1 but substituting each variable Vi from the position i of LVars
% by the value Valuei from the position i of LValues
replace_in_term_variables_by_values(Term, [], [], Term).
replace_in_term_variables_by_values(Term_In, [Var|LVars], [Value|LValues], Term_Out):-
	var(Var),!, % Don't do this if Var is not a variable.
	replace_in_term_var_by_value(Term_In, Var, Value, Term_Tmp),
	replace_in_term_variables_by_values(Term_Tmp, LVars, LValues, Term_Out).
replace_in_term_variables_by_values(Term_In, [_Var|LVars], [_Value|LValues], Term_Out):-
	replace_in_term_variables_by_values(Term_In, LVars, LValues, Term_Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Look for those saved clauses with same name and arity.	
%look_for_the_relevant_clauses(Goal, _Frontier) :-
%	functor(Goal, Name, Arity),  % Security
%	Name \== ',', Name \== ';',    % Issues
%	cneg_processed_pred(Name, Arity, SourceFileName, _Occurences), 
%	cneg_dynamic_cl(Name, Arity, SourceFileName, Head, Body),
%	debug_clause('look_for_the_relevant_clauses', cneg_dynamic_cl(Name, Arity, SourceFileName, Head, Body)),
%	fail.

%look_for_the_relevant_clauses(_Goal, _Frontier) :-
%	cneg_processed_pred(G, H, I, J), 
%	debug_clause('look_for_the_relevant_clauses', cneg_processed_pred(G, H, I, J)),
%	fail.

look_for_the_relevant_clauses(Goal, Frontier) :-
	functor(Goal, Name, Arity),  % Security
	Name \== ',', Name \== ';',    % Issues
	!, % Backtracking forbiden.
	cneg_processed_pred(Name, Arity, SourceFileName, _Occurences), 
%	debug_clause('look_for_the_relevant_clauses :: (Name, Arity, SourceFileName)', (Name, Arity, SourceFileName)),
	setof_local(frontier(Head, Body, FrontierTest), 
	cneg_dynamic_cl(Name, Arity, SourceFileName, Head, Body, FrontierTest), Frontier).

frontier_contents(frontier(Head, Body, FrontierTest), Head, Body, FrontierTest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

varset_local(X,Xs) :- 
        varsbag_local(X,[],[], Xs).
%        sort(Xs_uns,Xs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%varsbag_local(X, OtherBag, Bag_In, Bag_In) :- 
%	debug_clause('varsbag_local(X, OtherBag, Bag_In)', varsbag_local(X, OtherBag, Bag_In)),
%	fail.

varsbag_local(X, OtherBag, Bag_In, Bag_Out) :- 
        var(X), !,
%	debug_clause('varsbag_local_variable(X, OtherBag, Bag_In, Bag_Out)', varsbag_local_variable(X, OtherBag, Bag_In, Bag_Out)),
	varsbag_local_variable(X, OtherBag, Bag_In, Bag_Out),
%	debug_clause('varsbag_local_variable(X, OtherBag, Bag_In, Bag_Out)', varsbag_local_variable(X, OtherBag, Bag_In, Bag_Out)),
	!.

varsbag_local(Term, OtherBag, Bag_In, Bag_Out) :-
	functor(Term, _Name, Arity),
        varsbag_go_inside(Arity, Term, OtherBag, Bag_In, Bag_Out).

varsbag_go_inside(0, _Term, _OtherBag, Bag, Bag) :- !.
varsbag_go_inside(Arity, Term, OtherBag, Bag_In, Bag_Out) :-
        NewArity is Arity-1,
        arg(Arity, Term, Argument),
        varsbag_local(Argument, OtherBag, Bag_In, Bag_Tmp), !,
        varsbag_go_inside(NewArity, Term, OtherBag, Bag_Tmp, Bag_Out).

varsbag_local_variable(X, OtherBag, Bag, Bag) :-
	memberchk_local(X, OtherBag), % Var is in the other bag.
	!.
varsbag_local_variable(X, _OtherBag, Bag, Bag) :- 
	memberchk_local(X, Bag), % Var is alredy in the bag.
	!.
varsbag_local_variable(X, _OtherBag, Bag, [X|Bag]) :- 
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

term_to_meta(X, '$:'(X)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setof_local(Things, GoalCondition, Bag) :-
	setof(Things, GoalCondition, Bag).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

