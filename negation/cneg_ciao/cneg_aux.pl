
:- module(cneg_aux,
	[
	    echo_msg/5, echo_msg_2pm/6, echo_msg_3pm/7,
	    findall/4, append/3, functor_local/4,
	    list_head_and_tail/3, add_to_list_if_not_there/3, 
	    memberchk/2, term_to_meta/2,
	    % setof_local/3, 
	    filter_out_nonvars/2,
	    varsbag/4, varsbag_remove_var/3, varsbag_difference/3, 
	    varsbag_addition/3, varsbag_intersection/3,  
	    goal_clean_up/2,
	    goal_is_conjunction/3, goal_is_disjunction/3, 
	    goal_is_disequality/5, goal_is_equality/5,
	    goal_is_not_conj_disj_eq_diseq_dneg/1,
	    goal_is_not_conj_disj_neg/1,
	    goal_is_not_negation/1,
	    goal_is_negation/4,
	    terms_are_equal/2, unify_terms/2,
	    %	cneg_aux_equality/2,
	    qualify_string_name/3, 
	    % term_name_is_qualified/1, remove_qualification/2, 
	    replace_in_term_var_by_value/4,
	    % replace_in_args_var_by_value/4,
	    % replace_in_term_variables_by_values/4,
	    retrieve_element_from_list/2,
	    split_goal_with_disjunctions_into_goals/3,
	    generate_empty_trace/1,
	    generate_conjunction_trace/3
	],
	[assertions]).

:- use_module(library(aggregates),[setof/3]).
:- use_module(library(prolog_sys), [statistics/0]).
:- use_module(library(write), [write/1, write/2]).

% To access pre-frontiers from anywhere.
:- multifile cneg_pre_frontier/6.
:- multifile call_to/3.

:- comment(title, "Auxiliary predicates for Constructive Negation").

:- comment(author, "V@'{i}ctor Pablos Ceruelo").

:- comment(summary, "This module offers some predicates needed both by Constructive Negation
	Transformation Program, by Constructive Negation Library and by Disequalities Management.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic defined_stream_to_file/2.

% Use the following sentences to enable/disable debugging.

% 0 -> no debug.
% 1 -> std output + debugging.
% 2 -> only debugging.

%%% Debug (by VPC).
get_stream_to_file(File_Name_Atom, _Stream) :-
	var(File_Name_Atom), !, 
	echo_msg(1, '', 'fail', 'get_stream_to_file can not work if File_Name is a variable', ''),
	!, fail.
get_stream_to_file(File_Name_Atom, Stream) :-
	defined_stream_to_file(File_Name_Atom, Stream), !.
get_stream_to_file(File_Name_Atom, Stream) :-
	name(File_Name_Atom, File_Name_String_1), % Convert atom to string.
	append("debug_pkg_cneg_file_", File_Name_String_1, File_Name_String_2),
	append(File_Name_String_2, ".pl", File_Name_String_3),
	name(FN_Out, File_Name_String_3),	% Convert string to atom.
%	open(FN_Out,write,Stream), % This empties the file !!!
	open(FN_Out, append, Stream),
	assertz_fact(defined_stream_to_file(File_Name_Atom, Stream)), !,
	echo_howto_information(2, File_Name_Atom).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% echo_msg(Echo_Level, Mode, File_Name, Pre_Msg, Msg).
echo_msg(0, _Mode, _File_Name, _Pre_Msg, _Msg) :- !. % No debugging.

echo_msg(Echo_Level, 'aux', File_Name, Pre_Msg, Msg) :- !,
	echo_msg_aux(Echo_Level, File_Name, Pre_Msg),
	echo_msg_aux(Echo_Level, File_Name, Msg).

echo_msg(Echo_Level, 'list', File_Name, Pre_Msg, Msg) :-
	(   (echo_msg_list(Echo_Level, File_Name, 1, Pre_Msg, Msg), !)
	;
	    (echo_msg(Echo_Level, 'normal', File_Name, Pre_Msg, Msg), !)
	).

echo_msg(Echo_Level, 'logo', File_Name, _Pre_Msg, _Msg) :-
	echo_msg_aux(Echo_Level, File_Name, '% '), 
	echo_msg_aux(Echo_Level, File_Name, File_Name), 
	echo_msg_aux(Echo_Level, File_Name, ' :: '), 
	!.

echo_msg(Echo_Level, 'statistics', File_Name, _Pre_Msg, Msg) :-
	echo_msg_statistics(Echo_Level, File_Name, Msg), !.

echo_msg(Echo_Level, 'nl', File_Name, _Pre_Msg, _Msg) :-
	echo_msg_aux(Echo_Level, File_Name, '\n'), !.

echo_msg(Echo_Level, 'separation', File_Name, _Pre_Msg, _Msg) :-
	echo_msg_separation(Echo_Level, File_Name).	

echo_msg(Echo_Level, Mode, File_Name, Pre_Msg, Msg) :-
	  echo_msg_3pm(Echo_Level, Mode, File_Name, Pre_Msg, '', '', Msg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

echo_msg_2pm(Echo_Level, Mode, File_Name, Pre_Msg_1, Pre_Msg_2, Msg) :-
	echo_msg_3pm(Echo_Level, Mode, File_Name, Pre_Msg_1, Pre_Msg_2, '', Msg).

echo_msg_3pm(Echo_Level, Mode, File_Name, Pre_Msg_1, Pre_Msg_2, Pre_Msg_3, Msg) :-
	(Mode \== 'aux'), (Mode \== 'list'), (Mode \== 'logo'), 
	 (Mode \== 'statistics'), (Mode \== 'nl'), (Mode \== 'separation'), !,
	echo_msg(Echo_Level, 'logo', File_Name, Pre_Msg_1, Msg),
	echo_msg_aux(Echo_Level, File_Name, Pre_Msg_1),
	echo_msg_aux(Echo_Level, File_Name, Pre_Msg_2),
	echo_msg_aux(Echo_Level, File_Name, Pre_Msg_3),
	echo_msg_aux(Echo_Level, File_Name, ' :: '),
	echo_msg_aux(Echo_Level, File_Name, Msg),
	echo_msg(Echo_Level, 'nl', File_Name, '', ''), 
	!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

echo_msg_is_a_list([]).
echo_msg_is_a_list([_Elto|_List]).

echo_msg_list(Echo_Level, File_Name, Index, Pre_Msg, []) :-
	echo_msg_list_aux(Echo_Level, File_Name, Index, Pre_Msg, []),
	!. % No backtracking allowed.
echo_msg_list(Echo_Level, File_Name, Index, Pre_Msg, [Msg|Msgs]) :- 
	!, % No backtracking allowed.
	echo_msg_list_aux(Echo_Level, File_Name, Index, Pre_Msg, Msg),
	NewIndex is Index +1,
	echo_msg_list(Echo_Level, File_Name, NewIndex, Pre_Msg, Msgs).

echo_msg_list_aux(Echo_Level, File_Name, Index, Pre_Msg, Msg) :-
	echo_msg(Echo_Level, 'logo', File_Name, Pre_Msg, Msg),
	echo_msg_aux(Echo_Level, File_Name, Pre_Msg),
	echo_msg_aux(Echo_Level, File_Name, ' (list '),
	echo_msg_aux(Echo_Level, File_Name, Index),
	echo_msg_aux(Echo_Level, File_Name, ')'),
	echo_msg_aux(Echo_Level, File_Name, ' :: '),
	echo_msg_aux(Echo_Level, File_Name, Msg),
	echo_msg(Echo_Level, 'nl', File_Name, '', '').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
echo_msg_aux(0, _File_Name, _Msg) :- !.
echo_msg_aux(1, File_Name, Msg) :-
	echo_msg_aux(2, File_Name, Msg),
	write(Msg).
echo_msg_aux(2, File_Name, Msg) :-
	get_stream_to_file(File_Name, Stream_1),
	write(Stream_1, Msg),
	get_stream_to_file('with_all_debug_msgs', Stream_2),
	write(Stream_2, Msg).
			
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%echo_msg_nl(0, _File_Name) :- !.
%echo_msg_nl(1, File_Name) :-
%	echo_msg_nl(2, File_Name),
%	nl.
%echo_msg_nl(2, File_Name) :-
%	get_stream_to_file(File_Name, Stream_1),
%	nl(Stream_1),
%	get_stream_to_file('with_all_debug_msgs', Stream_2),
%	nl(Stream_2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

echo_msg_separation(Echo_Level, File_Name) :-
	echo_msg(Echo_Level, 'nl', File_Name, '', ''), 
	echo_msg_separation_aux(Echo_Level, File_Name), 
	echo_msg_separation_aux(Echo_Level, File_Name),
	echo_msg(Echo_Level, 'nl', File_Name, '', '').

echo_msg_separation_aux(Echo_Level, File_Name) :-
	echo_msg(Echo_Level, 'logo', File_Name, '', ''), 
	echo_msg_aux(Echo_Level, File_Name, '-----------------------------------------------'),
	echo_msg_aux(Echo_Level, File_Name, '-----------------------------------------------'),
	echo_msg(Echo_Level, 'nl', File_Name, '', '').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

echo_msg_statistics(0, _File_Name, _Msg) :- !. % No backtracking, please.
echo_msg_statistics(1, File_Name, Msg) :-
	echo_msg_statistics_aux(Msg),
	echo_msg_statistics(2, File_Name, Msg),
	!. % No backtracking, please.
echo_msg_statistics(2, File_Name, Msg) :-
	current_output(StdOut_Stream), % Save stdout stream.
	get_stream_to_file(File_Name, Statistics_Stream),
	set_output(Statistics_Stream), % Redirect stdout to stream.
	echo_msg_statistics_aux(Msg),
	set_output(StdOut_Stream), % Recover stdout stream.
	!. % No backtracking, please.

echo_msg_statistics_aux(Msg) :-
	nl, write(Msg), nl, nl, 
	statistics, !. % Write statistics to file.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

echo_howto_information(Echo_Level, File_Name) :-
	echo_msg_separation(Echo_Level, File_Name),
	echo_msg(Echo_Level, 'nl', File_Name, '', ''),
	echo_msg(Echo_Level, '', File_Name, 'Info for debugging', ' '),
	echo_msg(Echo_Level, 'nl', File_Name, '', ''),
	echo_msg(Echo_Level, '', File_Name, 'Basic frontier', 'frontier(Goal, Head, Body, FrontierTest)'),
	echo_msg(Echo_Level, '', File_Name, 'frontier_E_IE_NIE', 'frontier_E_IE_NIE(E, IE, NIE)'),
	echo_msg(Echo_Level, '', File_Name, 'frontier_E_IE_NIE_ied', 'frontier_E_IE_NIE_ied(E, IE_Imp, IE_Exp, IE_Dumb, NIE_Imp, NIE_Exp, NIE_Dumb)'),
	echo_msg(Echo_Level, '', File_Name, 'Vars_Info', 'vars_info(GoalVars, UQV, ImpVars, ExpVars, RelVars, UQ_to_EQ_Vars, Dumb_Vars)'),
	echo_msg_separation(Echo_Level, File_Name),
	echo_msg(Echo_Level, 'nl', File_Name, '', '').

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
	goal_is_aux_2a('basiccontrol:;', Goal, G1, G2), !.

goal_is_conjunction((G1,G2), G1, G2) :- !.
goal_is_conjunction(Goal, G1, G2) :- 
	goal_is_aux_2a('basiccontrol:,', Goal, G1, G2), !.

% goal_is_disequality(Goal, Arg_1, Arg_2, EQV, UQV) 
goal_is_disequality(Goal, Arg_1, Arg_2, [], []) :- 
	goal_is_aux_2a('=/=', Goal, Arg_1, Arg_2), !.
goal_is_disequality(Goal, Arg_1, Arg_2, [], UQV) :- 
	goal_is_aux_3a('disequality', Goal, Arg_1, Arg_2, UQV), !.
goal_is_disequality(Goal, Arg_1, Arg_2, EQV, []) :- 
	goal_is_aux_3a('diseq_eqv', Goal, Arg_1, Arg_2, EQV), !.
goal_is_disequality(Goal, Arg_1, Arg_2, [], UQV) :- 
	goal_is_aux_3a('diseq_uqv', Goal, Arg_1, Arg_2, UQV), !.
goal_is_disequality(Goal, Arg_1, Arg_2, EQV, UQV) :- 
	goal_is_aux_4a('diseq_euqv', Goal, Arg_1, Arg_2, EQV, UQV), !.

% goal_is_equality(Goal, Arg_1, Arg_2, EQV, UQV) 
goal_is_equality(Goal, Arg_1, Arg_2, [], []) :- 
	goal_is_aux_2a('=', Goal, Arg_1, Arg_2), !.
goal_is_equality(Goal, Arg_1, Arg_2, [], UQV) :- 
	goal_is_aux_3a('equality', Goal, Arg_1, Arg_2, UQV), !.
goal_is_equality(Goal, Arg_1, Arg_2, EQV, []) :- 
	goal_is_aux_3a('eq_eqv', Goal, Arg_1, Arg_2, EQV), !.
goal_is_equality(Goal, Arg_1, Arg_2, [], UQV) :- 
	goal_is_aux_3a('eq_uqv', Goal, Arg_1, Arg_2, UQV), !.
goal_is_equality(Goal, Arg_1, Arg_2, EQV, UQV) :- 
	goal_is_aux_4a('eq_euqv', Goal, Arg_1, Arg_2, EQV, UQV), !.

goal_is_aux_2a(Name, Goal, Arg_1, Arg_2) :-
	functor(Goal, Name, 2), !,
	arg(1, Goal, Arg_1),
	arg(2, Goal, Arg_2).

goal_is_aux_3a(Name, Goal, Arg_1, Arg_2, Arg_3) :-
	functor(Goal, Name, 3), !,
	arg(1, Goal, Arg_1),
	arg(2, Goal, Arg_2),
	arg(3, Goal, Arg_3).

goal_is_aux_4a(Name, Goal, Arg_1, Arg_2, Arg_3, Arg_4) :-
	functor(Goal, Name, 4), !,
	arg(1, Goal, Arg_1),
	arg(2, Goal, Arg_2),
	arg(3, Goal, Arg_3),
	arg(4, Goal, Arg_4).

goal_name_is_not_conjunction(Name) :-
	Name \== ',',
	Name \== 'basiccontrol:,'.

goal_name_is_not_disjunction(Name) :-
	Name \== 'basiccontrol:;',
	Name \== ';'.

goal_name_is_not_equality(Name) :-
	Name \== '=', 
	Name \== 'eq',
	Name \== 'cneg_eq',
	Name \== 'equality'.

goal_name_is_not_disequality(Name) :-
	Name \== 'dist',  
	Name \== 'diseq',
	Name \== 'dist',
	Name \== 'cneg_diseq',
	Name \== 'disequality',
	Name \== '=/='.

goal_name_is_not_negation(Name) :-
	Name \== 'cneg',
	Name \== 'cneg_tr',
	Name \== 'cneg_rt_Stuckey',
	Name \== 'cneg_rt_Chan',
	Name \== 'cneg_rt_New',
	Name \== 'cneg_rt'.

valid_names_for_negation_preds('cneg').
valid_names_for_negation_preds('cneg_tr').
valid_names_for_negation_preds('cneg_rt').
valid_names_for_negation_preds('cneg_rt_Stuckey').
valid_names_for_negation_preds('cneg_rt_Chan').
valid_names_for_negation_preds('cneg_rt_New').


goal_is_negation(Goal, UQV, SubGoal, Proposal) :-
	valid_names_for_negation_preds(Proposal), 
	functor(Goal, Proposal, 2), !,
	arg(1, Goal, UQV),
	arg(2, Goal, SubGoal).

goal_is_not_conj_disj_neg(Goal) :-
	nonvar(Goal),
	functor_local(Goal, Name, _Arity, _Args),
	goal_name_is_not_conjunction(Name),
	goal_name_is_not_disjunction(Name),
	goal_name_is_not_negation(Name).

goal_is_not_conj_disj_eq_diseq_dneg(Goal) :-
	nonvar(Goal),
	functor_local(Goal, Name, _Arity, _Args),
	goal_name_is_not_conjunction(Name),
	goal_name_is_not_disjunction(Name),
	goal_name_is_not_equality(Name),
	goal_name_is_not_disequality(Name),
	goal_name_is_not_negation(Name).

goal_is_not_negation(Goal) :-
	nonvar(Goal),
	functor_local(Goal, Name, _Arity, _Args),
	goal_name_is_not_negation(Name).

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
	cneg_aux:memberchk(X, OtherBag), % Var is in the other bag.
	!.
varsbag_local_variable(X, _OtherBag, Bag, Bag) :- 
	cneg_aux:memberchk(X, Bag), % Var is alredy in the bag.
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

%setof_local(Things, GoalCondition, Bag) :-
%	setof(Things, GoalCondition, Bag).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cneg_aux_equality(X, X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%split_goal_with_disjunctions_into_goals(Body, Bodies)
split_goal_with_disjunctions_into_goals(Body, Negation_Predicate, Bodies) :- 
	echo_msg(0, '', 'cneg_aux', 'INFO :: split_goal_with_disjunctions_into_goals :: Goal ', Body), 
	split_goal_with_disjunctions_into_goals_aux(Body, Negation_Predicate, Bodies),
	echo_msg(0, '', 'cneg_aux', 'INFO :: split_goal_with_disjunctions_into_goals :: Goals ', Bodies), 
	!.

split_goal_with_disjunctions_into_goals_aux(Body, Negation_Predicate, Bodies) :- 
	goal_is_conjunction(Body, Body_Conj_1, Body_Conj_2), !,
	split_goal_with_disjunctions_into_goals_aux(Body_Conj_1, Negation_Predicate, Bodies_Conj_1),
	split_goal_with_disjunctions_into_goals_aux(Body_Conj_2, Negation_Predicate, Bodies_Conj_2),
	combine_sub_bodies_by_conjunction(Bodies_Conj_1, Bodies_Conj_2, Bodies).

split_goal_with_disjunctions_into_goals_aux(Body, Negation_Predicate, Bodies) :- 
	goal_is_disjunction(Body, Body_Disj_1, Body_Disj_2), !,
	split_goal_with_disjunctions_into_goals_aux(Body_Disj_1, Negation_Predicate, Body_Result_1),
	split_goal_with_disjunctions_into_goals_aux(Body_Disj_2, Negation_Predicate, Body_Result_2),
	append(Body_Result_1, Body_Result_2, Bodies).

split_goal_with_disjunctions_into_goals_aux(Body, Negation_Predicate, [NewBody]) :- % Goal is something else.
	!,
	translate_problematic_predicates(Body, Negation_Predicate, NewBody), 
	!.

% Example for Negation_Predicate = 'cneg_tr'
translate_problematic_predicates(Body, Negation_Predicate, NewBody) :-
	goal_is_negation(Body, UQV, SubGoal, _Proposal), !,
	functor_local(NewBody, Negation_Predicate, 2, [UQV |[ SubGoal ]]).

translate_problematic_predicates(Body, _Negation_Predicate, Body) :- 
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

generate_empty_trace(trace([], _Out)).
generate_conjunction_trace(trace(In, Out), trace(In, Aux), trace(Aux, Out)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
