
:- module(cneg_aux,
	[
	    findall/4, functor_local/4,
	    list_head_and_tail/3, add_to_list_if_not_there/3, 
	    memberchk/2, term_to_meta/2,
	    % setof_local/3, 
	    filter_out_nonvars/2,
	    varsbag/4, varsbag_remove_var/3, varsbag_difference/3, 
	    varsbag_union/3, varsbag_intersection/3,  
	    varsbag_clean_up/2,
	    goal_clean_up/2,
	    goal_is_conjunction/3, goal_is_disjunction/3, 
	    goal_is_disequality/6, goal_is_equality/6,
	    goal_is_not_conj_disj_eq_diseq_dneg/1,
	    goal_is_not_conj_disj_neg/1,
	    goal_is_not_negation/1,
	    goal_is_negation/4,
	    goal_is_cneg_rt/4,
	    terms_are_equal/2, unify_terms/2,
	    %	cneg_aux_equality/2,
	    qualify_string_name/3, 
	    % term_name_is_qualified/1, remove_qualification/2, 
	    replace_in_term_vars_by_values/4,
	    replace_in_term_var_by_value/4,
	    replace_in_terms_list_var_by_value/4,
	    retrieve_element_from_list/2,
	    split_body_with_disjunctions_into_bodies/2,
	    split_body_into_E_IE_NIE_aux/7,
	    goals_join_by_conjunction/3
	],
	[assertions]).

% :- use_module(library(aggregates),[setof/3]).
:- use_module(library('cneg/cneg_basics')).
:- use_module(library('cneg/cneg_msgs')).
:- reexport(library('cneg/cneg_basics')). 
:- reexport(library('cneg/cneg_msgs')).

% To access pre-frontiers from anywhere.
:- multifile cneg_pre_frontier/6.
:- multifile call_to/3.
:- multifile file_debug_is_activated/1.

:- comment(title, "Auxiliary predicates for Constructive Negation").

:- comment(author, "V@'{i}ctor Pablos Ceruelo").

:- comment(summary, "This module offers some predicates needed both by Constructive Negation
	Transformation Program, by Constructive Negation Library and by Disequalities Management.").

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

list_head_and_tail([Head | Tail], Head, Tail).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%unify_terms(Term1, Term2) :-
%	echo_msg_clause('unify_terms', unify_terms(Term1, Term2)), 
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

% unify_variables(Term, Term) :- !.

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

% Esto es un memberchk un tanto bruto (ver memberchk debajo). 
retrieve_element_from_list([Element|_List], Element).
retrieve_element_from_list([_Other_Element|List], Element) :-
	retrieve_element_from_list(List, Element).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% La semantica de memberchk no es igual en todos los Prolog:
% en XSB prolog unifica mientras q en Ciao prolog no.
% Para mantener coherencia (y evitar problemas) lo redefinimos.

% From Ciao Prolog (idlists library).
% memberchk(X, [Y|_]) :- X == Y, !.
% memberchk(X, [_|L]) :- memberchk(X, L).

memberchk(_T1, []) :- !, fail.
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
        functor_local(T1, Name1, Arity1, Args1),
        functor_local(T2, Name2, Arity2, Args2),
	Name1 == Name2,
	Arity1 == Arity2,
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
	goal_is_aux_2a('basiccontrol:;', Goal, G1, G2), !.

goal_is_not_disjunction(Goal) :-
	nonvar(Goal), % Security issues.
	goal_is_disjunction(Goal, _G1, _G2), !, fail.
goal_is_not_disjunction(Goal) :-
	nonvar(Goal), % Security issues.
	!.
goal_is_not_disjunction(Goal) :-
	var(Goal), 
	echo_msg(1, '', 'cneg_aux', 'goal_is_not_disjunction', 'Goal can not be a variable. ERROR.'),
	!, fail.

goal_is_conjunction((G1,G2), G1, G2) :- !.
goal_is_conjunction(Goal, G1, G2) :- 
	goal_is_aux_2a('basiccontrol:,', Goal, G1, G2), !.

goal_is_not_conjunction(Goal) :-
	nonvar(Goal), % Security issues.
	goal_is_conjunction(Goal, _G1, _G2), !, fail.
goal_is_not_conjunction(Goal) :-
	nonvar(Goal), % Security issues.
	!.
goal_is_not_conjunction(Goal) :-
	var(Goal), 
	echo_msg(1, '', 'cneg_aux', 'goal_is_not_conjunction', 'Goal can not be a variable. ERROR.'),
	!, fail.

% goal_is_disequality(Goal, Arg_1, Arg_2, EQV, UQV) 
goal_is_disequality(Goal, Arg_1, Arg_2, [], [], []) :- 
	goal_is_aux_2a('=/=', Goal, Arg_1, Arg_2), !.
goal_is_disequality(Goal, Arg_1, Arg_2, [], [], UQV) :- 
	goal_is_aux_3a('disequality', Goal, Arg_1, Arg_2, UQV), !.
goal_is_disequality(Goal, Arg_1, Arg_2, GV, EQV, UQV) :- 
	goal_is_aux_5a('diseq_geuqv', Goal, Arg_1, Arg_2, GV, EQV, UQV), !.

goal_is_not_disequality(Goal) :-
	nonvar(Goal), % Security issues.
	goal_is_disequality(Goal, _Arg_1, _Arg_2, _GV, _EQV, _UQV), !, fail.
goal_is_not_disequality(Goal) :-
	nonvar(Goal), % Security issues.
	!.
goal_is_not_disequality(Goal) :-
	var(Goal), 
	echo_msg(1, '', 'cneg_aux', 'goal_is_not_disequality', 'Goal can not be a variable. ERROR.'),
	!, fail.

% goal_is_equality(Goal, Arg_1, Arg_2, EQV, UQV) 
goal_is_equality(Goal, Arg_1, Arg_2, [], [], []) :- 
	goal_is_aux_2a('=', Goal, Arg_1, Arg_2), !.
goal_is_equality(Goal, Arg_1, Arg_2, [], [], UQV) :- 
	goal_is_aux_3a('equality', Goal, Arg_1, Arg_2, UQV), !.
goal_is_equality(Goal, Arg_1, Arg_2, GV, EQV, UQV) :- 
	goal_is_aux_5a('eq_geuqv', Goal, Arg_1, Arg_2, GV, EQV, UQV), !.

goal_is_not_equality(Goal) :-
	nonvar(Goal), % Security issues.
	goal_is_equality(Goal, _Arg_1, _Arg_2, _GV, _EQV, _UQV), !, fail.
goal_is_not_equality(Goal) :-
	nonvar(Goal), % Security issues.
	!.
goal_is_not_equality(Goal) :-
	var(Goal), 
	echo_msg(1, '', 'cneg_aux', 'goal_is_not_equality', 'Goal can not be a variable. ERROR.'),
	!, fail.

goal_is_aux_2a(Name, Goal, Arg_1, Arg_2) :-
	functor(Goal, Name, 2), !,
	arg(1, Goal, Arg_1),
	arg(2, Goal, Arg_2).

goal_is_aux_3a(Name, Goal, Arg_1, Arg_2, Arg_3) :-
	functor(Goal, Name, 3), !,
	arg(1, Goal, Arg_1),
	arg(2, Goal, Arg_2),
	arg(3, Goal, Arg_3).

goal_is_aux_5a(Name, Goal, Arg_1, Arg_2, Arg_3, Arg_4, Arg_5) :-
	functor(Goal, Name, 5), !,
	arg(1, Goal, Arg_1),
	arg(2, Goal, Arg_2),
	arg(3, Goal, Arg_3),
	arg(4, Goal, Arg_4),
	arg(5, Goal, Arg_5).

valid_negation_preds('cneg', 2).
valid_negation_preds('cneg_tr', 2).
valid_negation_preds('cneg_rt', 2).
valid_negation_preds('cneg_rt', 5).

goal_is_negation(Goal, UQV, 'compute', SubGoal) :-
	valid_negation_preds(Proposal, Arity), 
	Arity == 2,
	functor(Goal, Proposal, Arity), !,
	arg(1, Goal, UQV),
	arg(2, Goal, SubGoal).

% cneg_rt(UQV, GoalVars, Goal, Depth_Level, Trace)
goal_is_negation(Goal, UQV, GoalVars, SubGoal) :-
	valid_negation_preds(Name, Arity), 
	Arity == 5,
	functor(Goal, Name, Arity), !,
 	arg(1, Goal, UQV),
 	arg(2, Goal, GoalVars),
	arg(3, Goal, SubGoal).
% 	arg(4, Goal, Depth_Level),
%	arg(5, Goal, Trace).

goal_is_not_negation(Goal) :-
	nonvar(Goal), % Security issues.
	goal_is_negation(Goal, _UQV, _GoalVars, _SubGoal), !, fail.
goal_is_not_negation(Goal) :-
	nonvar(Goal), % Security issues.
	!.
goal_is_not_negation(Goal) :-
	var(Goal), 
	echo_msg(1, '', 'cneg_aux', 'goal_is_not_negation', 'Goal can not be a variable. ERROR.'),
	!, fail.

goal_is_cneg_rt(Goal, UQV, GoalVars, SubGoal) :-
	goal_is_negation(Goal, UQV_In, GoalVars_In, SubGoal), !,
 	(
	    (   % Need to compute GoalVars.
		GoalVars_In == 'compute', !, 
		varsbag_clean_up(UQV_In, UQV),
		varsbag(SubGoal, UQV, [], GoalVars)
	    )
	;  
	    (   % Need to compute UQV.
		UQV_In == 'compute', !, 
		varsbag(GoalVars_In, [], [], GoalVars),
		varsbag(SubGoal, GoalVars, [], UQV) 
	    )
	;
	    (   % Nothing to compute.
		GoalVars_In \== 'compute', 
		UQV_In \== 'compute', !, 
		GoalVars = GoalVars_In, 
		UQV = UQV_In
	    )
	), !.

goal_is_not_conj_disj_neg(Goal) :-
	nonvar(Goal),
	functor_local(Goal, Name, _Arity, _Args),
	goal_is_not_conjunction(Name),
	goal_is_not_disjunction(Name),
	goal_is_not_negation(Name).

goal_is_not_conj_disj_eq_diseq_dneg(Goal) :-
	nonvar(Goal),
	functor_local(Goal, Name, _Arity, _Args),
	goal_is_not_conjunction(Name),
	goal_is_not_disjunction(Name),
	goal_is_not_equality(Name),
	goal_is_not_disequality(Name),
	goal_is_not_negation(Name).

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
	echo_msg(0, '', 'cneg_aux', 'qualify_string_name_not_qualified :: FAILED (ensure args are string) :: Qualification', Qualification), 
	echo_msg(0, '', 'cneg_aux', 'qualify_string_name_not_qualified :: FAILED (ensure args are string) :: Name', Name), 
	echo_msg(0, '', 'cneg_aux', 'qualify_string_name_not_qualified :: FAILED (ensure args are string) :: NewName', NewName), 
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
	echo_msg(0, '', 'cneg_aux', 'name_has_semicolon :: NOT a string', Any), fail.

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
%	echo_msg_clause('WARNING: remove_qualification :: NOT a string qualified', NameIn).

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
	replace_in_terms_list_var_by_value(Args,Var,Value,Args1),
	Term1=..[Functor|Args1].

% replace_in_args_var_by_value(Args_In, Var, Value, Args_Out) 
% applies replace_in_term_var_by_value/4 to all the arguments in Args.
replace_in_terms_list_var_by_value([], _Var, _Value, []).
replace_in_terms_list_var_by_value([Arg_In|Args_In], Var, Value, [Arg_Out|Args_Out]):-
	replace_in_term_var_by_value(Arg_In, Var, Value, Arg_Out),
	replace_in_terms_list_var_by_value(Args_In, Var, Value, Args_Out).

% replace_in_term_variables_by_values(LVars,LValues,Term1,Term2) returns in Term2 the same term
% Term1 but substituting each variable Vi from the position i of LVars
% by the value Valuei from the position i of LValues
replace_in_term_vars_by_values(Term, [], [], Term).
replace_in_term_vars_by_values(Term_In, [Var|LVars], [Value|LValues], Term_Out):-
	var(Var),!, % Don't do this if Var is not a variable.
	replace_in_term_var_by_value(Term_In, Var, Value, Term_Tmp),
	replace_in_term_vars_by_values(Term_Tmp, LVars, LValues, Term_Out).
replace_in_term_vars_by_values(Term_In, [_Var|LVars], [_Value|LValues], Term_Out):-
	replace_in_term_vars_by_values(Term_In, LVars, LValues, Term_Out).

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

% This predicate removes the elements in a varsbag 
% that have stopped being variables 
% (that have been unified with something different from a variable).
% varsbag_clean_up(Vars_In, Vars_Out) :- 
varsbag_clean_up([], []) :- !.
varsbag_clean_up([Var | Vars_In], [Var | Vars_Out]) :- 
	var(Var), !,
	varsbag_clean_up(Vars_In, Vars_Out).
varsbag_clean_up([_Var | Vars_In], Vars_Out) :- 
	varsbag_clean_up(Vars_In, Vars_Out).

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

varsbag_union(VarsBag_1_In, VarsBag_2_In, VarsBag_Out) :-
	filter_out_nonvars(VarsBag_1_In, VarsBag_1_Aux),
	filter_out_nonvars(VarsBag_2_In, VarsBag_2_Aux),
	cneg_basics:append(VarsBag_1_Aux, VarsBag_2_Aux, VarsBag_In),
	cneg_aux:varsbag(VarsBag_In, [], [], VarsBag_Out).

varsbag_difference(VarsBag_1_In, VarsBag_2_In, VarsBag_Out) :-
%	echo_msg('varsbag_difference(VarsBag_1_In, VarsBag_2_In)', (VarsBag_1_In, VarsBag_2_In)),
	filter_out_nonvars(VarsBag_1_In, VarsBag_1_Aux),
	filter_out_nonvars(VarsBag_2_In, VarsBag_2_Aux),
%	echo_msg('varsbag_difference(VarsBag_1_Aux, VarsBag_2_Aux)', (VarsBag_1_Aux, VarsBag_2_Aux)),
	varsbag_difference_aux(VarsBag_1_Aux, VarsBag_2_Aux, VarsBag_Out).
%	echo_msg('varsbag_difference :: VarsBag_Out', VarsBag_Out).

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
%	echo_msg_clause('varsbag_local(X, OtherBag, Bag_In)', varsbag_local(X, OtherBag, Bag_In)),
%	fail.

varsbag(X, OtherBag, Bag_In, Bag_Out) :- 
        var(X), !,
	echo_msg(0, '', 'cneg_aux', 'varsbag_local_variable(X, OtherBag, Bag_In, Bag_Out)', varsbag_local_variable(X, OtherBag, Bag_In, Bag_Out)),
	varsbag_local_variable(X, OtherBag, Bag_In, Bag_Out),
	echo_msg(0, '', 'cneg_aux', 'varsbag_local_variable(X, OtherBag, Bag_In, Bag_Out)', varsbag_local_variable(X, OtherBag, Bag_In, Bag_Out)),
	!.

varsbag(Term, OtherBag, Bag_In, Bag_Out) :-
	functor(Term, _Name, Arity),
        varsbag_go_inside(Arity, Term, OtherBag, Bag_In, Bag_Out).

varsbag_go_inside(0, _Term, _OtherBag, Bag, Bag) :- !.
varsbag_go_inside(Arity, Term, OtherBag, Bag_In, Bag_Out) :-
        NewArity is Arity-1,
        arg(Arity, Term, Argument),
        cneg_aux:varsbag(Argument, OtherBag, Bag_In, Bag_Tmp), !,
        varsbag_go_inside(NewArity, Term, OtherBag, Bag_Tmp, Bag_Out).

varsbag_local_variable(X, OtherBag, Bag, Bag) :-
	var(X),
	cneg_aux:memberchk(X, OtherBag), % Var is in the other bag.
	!.
varsbag_local_variable(X, _OtherBag, Bag, Bag) :- 
	var(X),
	cneg_aux:memberchk(X, Bag), % Var is alredy in the bag.
	!.
varsbag_local_variable(X, _OtherBag, Bag, [X|Bag]) :- 
	var(X),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

term_to_meta(X, '$:'(X)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%setof_local(Things, GoalCondition, Bag) :-
%	setof(Things, GoalCondition, Bag).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% cneg_aux_equality(X, X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%split_body_with_disjunctions_into_bodies(Body, Bodies)
split_body_with_disjunctions_into_bodies(Body, Bodies) :- 
	echo_msg(0, '', 'cneg_aux', 'INFO :: split_body_with_disjunctions_into_bodies :: Goal ', Body), 
	split_body_with_disjunctions_into_bodies_aux(Body, Bodies),
	echo_msg(0, '', 'cneg_aux', 'INFO :: split_body_with_disjunctions_into_bodies :: Goals ', Bodies), 
	!.

split_body_with_disjunctions_into_bodies_aux(Body, Bodies) :- 
	goal_is_conjunction(Body, Body_Conj_1, Body_Conj_2), !,
	split_body_with_disjunctions_into_bodies_aux(Body_Conj_1, Bodies_Conj_1),
	split_body_with_disjunctions_into_bodies_aux(Body_Conj_2, Bodies_Conj_2),
	combine_sub_bodies_by_conjunction(Bodies_Conj_1, Bodies_Conj_2, Bodies).

split_body_with_disjunctions_into_bodies_aux(Body, Bodies) :- 
	goal_is_disjunction(Body, Body_Disj_1, Body_Disj_2), !,
	split_body_with_disjunctions_into_bodies_aux(Body_Disj_1, Body_Result_1),
	split_body_with_disjunctions_into_bodies_aux(Body_Disj_2, Body_Result_2),
	append(Body_Result_1, Body_Result_2, Bodies).

split_body_with_disjunctions_into_bodies_aux(Body, [Body]) :- % Goal is something else.
	!,
	split_body_with_disjunctions_into_bodies_by_pass(Body), 
	!.

split_body_with_disjunctions_into_bodies_by_pass(Body) :-
	goal_is_cneg_rt(Body, _UQV, _GoalVars, _SubGoal).
split_body_with_disjunctions_into_bodies_by_pass(Body) :- 
	goal_is_not_conj_disj_neg(Body), !.

combine_sub_bodies_by_conjunction([], _List_2, []) :- !.
combine_sub_bodies_by_conjunction([Elto | List_1], List_2, Result) :-
	combine_sub_bodies_by_conjunction_aux(Elto, List_2, Result_1),
	combine_sub_bodies_by_conjunction(List_1, List_2, Result_2),
	append(Result_1, Result_2, Result).

combine_sub_bodies_by_conjunction_aux(_Elto_1, [], []) :- !.
combine_sub_bodies_by_conjunction_aux(Elto_1, [Elto_2 | List], [(Elto_1, Elto_2) | More_Results]) :-
	combine_sub_bodies_by_conjunction_aux(Elto_1, List, More_Results).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% goals_join_by_conjunction(Goal_1, Goal_2, Result) :-
goals_join_by_conjunction(true, true, true) :- !.
goals_join_by_conjunction(true, Goal_2, Goal_2) :- !.
goals_join_by_conjunction(Goal_1, true, Goal_1) :- !.
goals_join_by_conjunction(Goal_1, Goal_2, (Goal_1, Goal_2)) :- !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

split_bodies_into_E_IE_NIE([], []) :- !.
split_bodies_into_E_IE_NIE([Body | Bodies], [([E], [IE], [NIE], [Body]) | Split_Bodies]) :- !,
	split_body_into_E_IE_NIE_aux(Body, true, true, true, E, IE, NIE),
	split_bodies_into_E_IE_NIE(Bodies, Split_Bodies).

split_body_into_E_IE_NIE_aux(Body, _E_In, _IE_In, _NIE_In, _E_Out, _IE_Out, _NIE_Out) :- 
	goal_is_disjunction(Body, _G1, _G2), !, 
	echo_msg(1, '', 'cneg_rt', 'ERROR: split_body_into_E_IE_NIE can not deal with disjunctions. Body', Body),
	fail.

split_body_into_E_IE_NIE_aux(Body, E_In, IE_In, NIE_In, E_Out, IE_Out, NIE_Out) :- 
	goal_is_conjunction(Body, G1, G2), !,
	split_bodies_into_E_IE_NIE_aux(G1, E_In, IE_In, NIE_In, E_Aux, IE_Aux, NIE_Aux),
	split_bodies_into_E_IE_NIE_aux(G2, E_Aux, IE_Aux, NIE_Aux, E_Out, IE_Out, NIE_Out).
split_body_into_E_IE_NIE_aux(Body, E_In, IE_In, NIE_In, E_Out, IE_In, NIE_In) :-
	goal_is_equality(Body, _Arg_1, _Arg_2, _GV, _EQV, _UQV), !,
	goals_join_by_conjunction(E_In, Body, E_Out).
split_body_into_E_IE_NIE_aux(Body, E_In, IE_In, NIE_In, E_In, IE_Out, NIE_In) :-
	goal_is_disequality(Body, _Arg_1, _Arg_2, _GV, _EQV, _UQV), !,
	goals_join_by_conjunction(IE_In, Body, IE_Out).

split_body_into_E_IE_NIE_aux(Body, E_In, IE_In, NIE_In, E_In, IE_In, NIE_Out) :- !,
	goals_join_by_conjunction(NIE_In, Body, NIE_Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
