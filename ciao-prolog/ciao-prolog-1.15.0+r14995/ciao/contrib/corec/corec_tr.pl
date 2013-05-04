:- module(corec_tr, _, %[sentence/3],
	[dcg]).

:- use_module(library(lists)).
:- use_module(library(hiordlib)).
:- use_module(library(aggregates)).
:- use_module(library(terms), [atom_concat/2]).

:- include(library(corec(corec_ops))).

conj2list(A, [A]):- var(A), !.
conj2list((A,B), [A|L]) :- !, conj2list(B, L).
conj2list(A, [A]).

:-data(cmptr/2).
get_c(Mod, J):-
	cmptr(Mod, I), 
	retract_fact(cmptr(Mod, I)), 
	J is I + 1, 
	assertz_fact(cmptr(Mod, J)). 
get_c(Mod, 1):-
	assertz_fact(cmptr(Mod, 1)). 

:-data(corec_pred/5).
:-data(saved_clause/3).

is_corec(Pred, Mod):-
	functor(Pred, Func, Arity),
	is_corec_func(Func, Arity, Mod).

is_corec_func(Func, Arity, Mod):-
	corec_pred(Func, Arity, Mod, _Pattern, _WithGoal).

add_corec_pred(Pattern, WithGoal, Mod):-
	functor(Pattern, Func, Arity),
	(
	    is_corec_func(Func, Arity, Mod) ->
	    throw(already_defined_as_corecursive(Func/Arity))
	;
	    assertz_fact(corec_pred(Func, Arity, Mod, Pattern, WithGoal))
	).

	
sentence(0, _, Mod) :- !, 
	retractall_fact(corec_pred(_Func, _Arity, Mod, _Pattern, _WithGoal)),
	fail.
sentence(end_of_file, Clauses, Mod):-!,
	generate_corec_pre(Mod, Clauses, [end_of_file]).
sentence((:- corecursion Pred with Goal), [], Mod):-!,
	add_corec_pred(Pred, Goal, Mod).
sentence((:- corecursion Pred), [], Mod):-!,
	add_corec_pred(Pred, true, Mod).
sentence((Head :- Body), [(NHead :- Body)], Mod):-!,
	is_corec(Head, Mod), 
	rename_fact(Head, NHead).
sentence(Fact, [NFact], Mod):- 
	is_corec(Fact, Mod), 
	rename_fact(Fact, NFact).


generate_corec_pre(Mod) -->
	{retract_fact(corec_pred(_Func, _Arity, Mod, Pattern, WithGoal)) }, !,
	{generate_head(Pattern, Mod, Head, Call, PreviousCall, SavedCall, Matcher, WithGoal)}, 
	[(Head :- '$get_stack'(PreviousCall), Matcher), 
	 (Head :- '$push_stack'(SavedCall), Call, '$pop_stack')], 
	generate_corec_pre(Mod).
generate_corec_pre(_Mod) --> [].

generate_head(Pattern, Mod, Head, Call, PreviousCall, SavedCall, Matcher, WithGoal):-
	functor(Pattern, F, A),
	atom_number(AA, A), 
	Pattern =.. [F|PatArgs], 
	atom_concat(['$corec_', F,'_',AA], NF), 
	process_pattern_args(PatArgs, Args, PreArgs, SArgs, Matcher, WithGoal), 
	Head =.. [F|Args], 
	atom_concat([Mod, ':', NF], PNF),
	PreviousCall =.. [PNF|PreArgs],
	SavedCall =.. [PNF|SArgs],
	Call =.. [NF |Args].

process_pattern_args([], [], [], []) --> [].
process_pattern_args([X|T1], [X|T2], T3, T4) -->
	{var(X)}, !,
	process_pattern_args(T1, T2, T3, T4).
process_pattern_args([Pred|T1], [X|T2], [Y|T3], [X|T4]) -->
	add_goal(Pred, [Y,X]),
	process_pattern_args(T1, T2, T3, T4).

add_goal(Pred, ExtraArgs, (G1, G), G):-
	Pred =.. [F|Args],
	append(Args, ExtraArgs, Args_), 
	G1 =.. [F|Args_].

rename_fact(Fact, NFact):-
	functor(Fact, F, A), 
	atom_number(AA, A),
	Fact =.. [F|Args], 
	atom_concat(['$corec_', F, '_', AA], NF), 
	NFact =.. [NF|Args]. 

