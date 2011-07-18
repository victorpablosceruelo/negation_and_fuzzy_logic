
:- module(cneg_aux,
	[
	    findall/4, append/3,
	    debug_msg/3, debug_msg_list/3, 
	    debug_msg_aux/3, debug_msg_nl/1, 
	    list_head/2, list_tail/2, unify_terms/2, functor_local/4,
	    memberchk/2, term_to_meta/2,
	    setof_local/3, filter_out_nonvars/2,
	    varsbag_local/4, varsbag_remove_var/3, varsbag_difference/3, 
	    varsbag_addition/3, varsbag_intersection/3,  
	    goal_clean_up/2,
	    goal_is_conjunction/3, goal_is_disjunction/3, 
	    goal_is_disequality/4, goal_is_equality/3,
	    qualify_string_name/3, remove_qualification/2, 
	    % term_name_is_qualified/1,
	    % replace_in_term_var_by_value/4, % replace_in_args_var_by_value/4,
	    % replace_in_term_variables_by_values/4,
	    add_to_list_if_not_there/3, 
	    terms_are_equal/2,
	    cneg_aux_equality/2
	],
	[assertions]).

:- use_module(library(aggregates),[setof/3]).
:- use_module(library(write), _).

:- comment(title, "Auxiliary predicates for Constructive Negation").

:- comment(author, "V@'{i}ctor Pablos Ceruelo").

:- comment(summary, "This module offers some predicates needed both by Constructive Negation
	Transformation Program, by Constructive Negation Library and by Disequalities Management.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Use the following sentences to enable/disable debugging.
debug_msg_is_on(Level) :- Level > 0.

% 0 -> crazy debug.
% 1 -> debug.
% 2 -> normal msgs.

%%% Debug (by VPC).
debug_msg(Level, Msg, Clause) :-
	debug_msg_aux(Level, '% cneg', ' :: '),
	debug_msg_aux(Level, Msg, ' :: '),
	debug_msg_aux(Level, Clause, ''),
	debug_msg_nl(Level).

debug_msg_aux(Level, Msg, Clause) :-
	debug_msg_is_on(Level),
	write(Msg), 
	write(Clause).
debug_msg_aux(Level, _Msg, _Clause) :- 
	\+(debug_msg_is_on(Level)).

debug_msg_nl(Level) :-
	debug_msg_is_on(Level),
	nl.
debug_msg_nl(Level) :- 
	\+(debug_msg_is_on(Level)).

debug_msg_list(Level, Msg, []) :-
	debug_msg(Level, Msg, []),
	!. % No backtracking allowed.
debug_msg_list(Level, Msg, [Cl|Cls]) :- 
	!, % No backtracking allowed.
	debug_msg(Level, Msg, Cl),
	debug_msg_list(Level, Msg, Cls).

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

list_head([Head | _Tail], Head).
list_tail([_Head | Tail], Tail).
list_snd_head(List, Second) :- !,
	list_tail(List, Tail),
	list_tail(Tail, Second).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%unify_terms(Term1, Term2) :-
%	debug_msg_clause('unify_terms', unify_terms(Term1, Term2)), 
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

add_to_list_if_not_there(Elto, List, List) :- memberchk(Elto, List), !.
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
% memberchk(X, [Y|_]) :- X == Y, !.
% memberchk(X, [_|L]) :- memberchk(X, L).

memberchk(T1, [T2]) :-
	terms_are_equal(T1,T2).
memberchk(T1, [T2|_L]) :- 
	terms_are_equal(T1,T2).
memberchk(T1, [_T2|L]) :- 
        memberchk(T1, L).

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
goal_is_disequality(diseq(X,Y), X, Y, []) :- !.
goal_is_disequality(diseq(X,Y, FreeVars), X, Y, FreeVars) :- !.
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
	debug_msg(0, 'qualify_string_name_not_qualified :: FAILED (ensure args are string) :: Qualification', Qualification), 
	debug_msg(0, 'qualify_string_name_not_qualified :: FAILED (ensure args are string) :: Name', Name), 
	debug_msg(0, 'qualify_string_name_not_qualified :: FAILED (ensure args are string) :: NewName', NewName), 
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
	debug_msg(0, 'name_has_semicolon :: NOT a string', Any), fail.

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
%	debug_msg_clause('WARNING: remove_qualification :: NOT a string qualified', NameIn).

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

filter_out_nonvars([], []) :- !.
filter_out_nonvars([Var | Vars_In], [Var | Vars_Out]) :-
	var(Var), !,
	filter_out_nonvars(Vars_In, Vars_Out).
filter_out_nonvars([_Var | Vars_In], Vars_Out) :-
	filter_out_nonvars(Vars_In, Vars_Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

varsbag_remove_var(Var, VarsBag_In, VarsBag_Out) :-
	var(Var), !,
	filter_out_nonvars(VarsBag_In, VarsBag_Aux),
	varsbag_remove_var_aux(Var, VarsBag_Aux, VarsBag_Out).
varsbag_remove_var(_Var, VarsBag_In, VarsBag_Out) :-
	filter_out_nonvars(VarsBag_In, VarsBag_Out).

varsbag_remove_var_aux(_Var_In, [], []) :- !.
varsbag_remove_var_aux(Var_In, [Var | VarsBag_In], VarsBag_Out) :-
	var(Var_In), var(Var),
	Var_In == Var, !,
	varsbag_remove_var_aux(Var_In, VarsBag_In, VarsBag_Out).
varsbag_remove_var_aux(Var_In, [Var | VarsBag_In], [Var | VarsBag_Out]) :-
	var(Var_In), var(Var), !,
	varsbag_remove_var_aux(Var_In, VarsBag_In, VarsBag_Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

varsbag_addition(VarsBag_1_In, VarsBag_2_In, VarsBag_Out) :-
	filter_out_nonvars(VarsBag_1_In, VarsBag_1_Aux),
	filter_out_nonvars(VarsBag_2_In, VarsBag_2_Aux),
	cneg_aux:append(VarsBag_1_Aux, VarsBag_2_Aux, VarsBag_In),
	varsbag_local(VarsBag_In, [], [], VarsBag_Out).

varsbag_difference(VarsBag_1_In, VarsBag_2_In, VarsBag_Out) :-
%	debug_msg('varsbag_difference(VarsBag_1_In, VarsBag_2_In)', (VarsBag_1_In, VarsBag_2_In)),
	filter_out_nonvars(VarsBag_1_In, VarsBag_1_Aux),
	filter_out_nonvars(VarsBag_2_In, VarsBag_2_Aux),
%	debug_msg('varsbag_difference(VarsBag_1_Aux, VarsBag_2_Aux)', (VarsBag_1_Aux, VarsBag_2_Aux)),
	varsbag_difference_aux(VarsBag_1_Aux, VarsBag_2_Aux, VarsBag_Out).
%	debug_msg('varsbag_difference :: VarsBag_Out', VarsBag_Out).

varsbag_difference_aux([], _VarsBag, []) :- !.
varsbag_difference_aux([Var | Vars_In], VarsBag, Vars_Out) :-
	memberchk(Var, VarsBag), !,
	varsbag_difference_aux(Vars_In, VarsBag, Vars_Out).
varsbag_difference_aux([Var | Vars_In], VarsBag, [Var | Vars_Out]) :-
	varsbag_difference_aux(Vars_In, VarsBag, Vars_Out).

varsbag_intersection([], _VarsBag_In_2, []) :- !.
varsbag_intersection([Var_1 | VarsBag_In_1], VarsBag_In_2, [Var_1 | VarsBag_Out]) :-
	memberchk(Var_1, VarsBag_In_2), !,
	varsbag_intersection(VarsBag_In_1, VarsBag_In_2, VarsBag_Out).
varsbag_intersection([_Var_1 | VarsBag_In_1], VarsBag_In_2, VarsBag_Out) :-
	varsbag_intersection(VarsBag_In_1, VarsBag_In_2, VarsBag_Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%varsbag_local(X, OtherBag, Bag_In, Bag_In) :- 
%	debug_msg_clause('varsbag_local(X, OtherBag, Bag_In)', varsbag_local(X, OtherBag, Bag_In)),
%	fail.

varsbag_local(X, OtherBag, Bag_In, Bag_Out) :- 
        var(X), !,
%	debug_msg_clause('varsbag_local_variable(X, OtherBag, Bag_In, Bag_Out)', varsbag_local_variable(X, OtherBag, Bag_In, Bag_Out)),
	varsbag_local_variable(X, OtherBag, Bag_In, Bag_Out),
%	debug_msg_clause('varsbag_local_variable(X, OtherBag, Bag_In, Bag_Out)', varsbag_local_variable(X, OtherBag, Bag_In, Bag_Out)),
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
	memberchk(X, OtherBag), % Var is in the other bag.
	!.
varsbag_local_variable(X, _OtherBag, Bag, Bag) :- 
	memberchk(X, Bag), % Var is alredy in the bag.
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

cneg_aux_equality(X, X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
