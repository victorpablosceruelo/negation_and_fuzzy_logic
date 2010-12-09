
:- module(cneg_aux,
	[
	    findall/4, append/3,
	    cneg_msg/3, cneg_msg_list/3, cneg_msg_nl/1,
	    first/2, second/2, unify_terms/2, functor_local/4,
	    memberchk_local/2, term_to_meta/2,
	    setof_local/3, filter_out_nonvars/2,
	    varsbag_local/4, varsbag_difference/3, 
	    varsbag_addition/3, varsbag_remove_var/3,
	    goal_clean_up/2,
	    goal_is_conjunction/3, goal_is_disjunction/3, 
	    goal_is_disequality/4, goal_is_equality/3,
	    qualify_string_name/3, remove_qualification/2, 
	    terms_are_equal/2,
	    cneg_aux_equality/2
	]).

%:- use_module(library(aggregates),[setof/3]).
%:- use_module(library(write), _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Use the following sentences to enable/disable debugging.
cneg_msg_are_enabled(Level) :- Level > 0.
% > 0 -> Debug all
% > 1 -> Debug important
% > 2 -> Msgs

%%% Debug (by VPC).
cneg_msg(Level, Msg, Clause) :-
	cneg_msg_are_enabled(Level), 
	!, % No backtracking allowed.
	write('% DBG % '),
	write(Msg), 
	write(' :: '),
	write(Clause), nl, 
	!. % No backtracking allowed.
cneg_msg(Level, _Msg, _Clause) :-
	\+(cneg_msg_are_enabled(Level)).

cneg_msg_nl(Level) :-
	cneg_msg_are_enabled(Level),
	!, nl.
cneg_msg_nl(Level) :- 
	\+(cneg_msg_are_enabled(Level)).

cneg_msg_list(Level, Msg, []) :-
	cneg_msg(Level, Msg, []),
	!. % No backtracking allowed.
cneg_msg_list(Level, Msg, [Cl|Cls]) :- 
	!, % No backtracking allowed.
	cneg_msg(Level, Msg, Cl),
	cneg_msg_list(Level, Msg, Cls).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- meta_predicate findall(?,+,?,?).
findall(Template, Generator, List, Tail) :-
        save_solutions(-Template, Generator),
        list_solutions(List, Tail).

:- dynamic solution/1.
:- meta_predicate save_solutions(?,+).
save_solutions(Template, Generator) :-
        asserta(solution('-')),
        call(Generator),
        asserta(solution(Template)),
        fail.
save_solutions(_,_).

list_solutions(List, Tail) :-
        retract(solution(Term)), !,
        list_solutions_aux(Term,Tail,List).
list_solutions_aux('-',L,L) :- !.
list_solutions_aux(-Term,Sofar,List) :-
        retract(solution(NewTerm)), !,
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
%	cneg_msg(1, 'unify_terms', unify_terms(Term1, Term2)), 
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
	term_name_has_semicolon(Goal_Name_String), !,
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
	term_name_has_semicolon(Name), !,
	remove_qualification(Name, NameTmp), 
	qualify_string_name(Qualification, NameTmp, NewName).
qualify_string_name(Qualification, Name, NewName) :-	
	string(Qualification),
	string(Name),
	append(Qualification, ":", Prefix), 
	append(Prefix, Name, NewName),
	string(NewName), 
	!.

qualify_string_name(Qualification, Name, _NewName) :-
	type_error(chars, Qualification),
	type_error(chars, Name),
%	cneg_msg(1, 'qualify_string_name :: FAILED (ensure args are string) :: Qualification', Qualification), 
%	cneg_msg(1, 'qualify_string_name :: FAILED (ensure args are string) :: Name', Name), 
	!, fail.

%	name(Name, NameString),
%	name(Qualification, QualificationString), 
%     ....
%	name(NewName, NewNameString).

term_name_has_semicolon(Term) :-
%	string(Any), !,
	(
	 is_of_type(chars, Term)
	;
	 is_of_type(codes, Term)
	),
	!,
	term_name_has_semicolon_aux(Term).
term_name_has_semicolon(Term) :-
	(
	 term_name_type_error(Term, chars)
	;
	 term_name_type_error(Term, string)
	;
	 term_name_type_error(Term, atom)
	;
	 term_name_type_error(Term, codes)
	;
	 term_name_type_error(Term, text)
	),
	!, fail.

term_name_type_error(Term, Type) :-
	is_of_type(Type, Term),
	cneg_msg(1, 'term_name_has_semicolon :: Term: ', Term),
	cneg_msg(1, 'term_name_has_semicolon :: Type', Type).


term_name_has_semicolon_aux([]) :- !, fail.
term_name_has_semicolon_aux([A]) :- 
	semicolon_string([A]), !.
term_name_has_semicolon_aux([A|_Others]) :- 
	semicolon_string([A]), !.
term_name_has_semicolon_aux([_A|Others]) :- !,
	term_name_has_semicolon_aux(Others).

semicolon_string(":") :- !.

remove_qualification(NameIn, NameOut) :-
	term_name_has_semicolon(NameIn), !,
	remove_qualification_aux(NameIn, NameOut).
remove_qualification(NameIn, NameIn) :- !.
%	cneg_msg(1, 'WARNING: remove_qualification :: NOT a string qualified', NameIn).

remove_qualification_aux([A|Name], Name) :-
	semicolon_string([A]), !.
remove_qualification_aux([_A|NameIn], NameOut) :-
	remove_qualification_aux(NameIn, NameOut).

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
%	cneg_msg(1, 'varsbag_difference(VarsBag_1_In, VarsBag_2_In)', (VarsBag_1_In, VarsBag_2_In)),
	filter_out_nonvars(VarsBag_1_In, VarsBag_1_Aux),
	filter_out_nonvars(VarsBag_2_In, VarsBag_2_Aux),
%	cneg_msg(1, 'varsbag_difference(VarsBag_1_Aux, VarsBag_2_Aux)', (VarsBag_1_Aux, VarsBag_2_Aux)),
	varsbag_difference_aux(VarsBag_1_Aux, VarsBag_2_Aux, VarsBag_Out).
%	cneg_msg(1, 'varsbag_difference :: VarsBag_Out', VarsBag_Out).

varsbag_difference_aux([], _VarsBag, []) :- !.
varsbag_difference_aux([Var | Vars_In], VarsBag, Vars_Out) :-
	memberchk_local(Var, VarsBag), !,
	varsbag_difference_aux(Vars_In, VarsBag, Vars_Out).
varsbag_difference_aux([Var | Vars_In], VarsBag, [Var | Vars_Out]) :-
	varsbag_difference_aux(Vars_In, VarsBag, Vars_Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%varsbag_local(X, OtherBag, Bag_In, Bag_In) :- 
%	cneg_msg(1, 'varsbag_local(X, OtherBag, Bag_In)', varsbag_local(X, OtherBag, Bag_In)),
%	fail.

varsbag_local(X, OtherBag, Bag_In, Bag_Out) :- 
        var(X), !,
%	cneg_msg(1, 'varsbag_local_variable(X, OtherBag, Bag_In, Bag_Out)', varsbag_local_variable(X, OtherBag, Bag_In, Bag_Out)),
	varsbag_local_variable(X, OtherBag, Bag_In, Bag_Out),
%	cneg_msg(1, 'varsbag_local_variable(X, OtherBag, Bag_In, Bag_Out)', varsbag_local_variable(X, OtherBag, Bag_In, Bag_Out)),
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

cneg_aux_equality(X, X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
