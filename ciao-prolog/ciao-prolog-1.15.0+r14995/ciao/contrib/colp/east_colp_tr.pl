:- module(east_colp_tr, [east_sentence/3], []).

:- use_module(library(lists)).
:- use_module(library(hiordlib)).
:- use_module(library(aggregates)).

:- set_prolog_flag(multi_arity_warnings,off).

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

generate_alternative_name(F1, F2):-
	    atom_concat('$colp_', F1, F2). 
	    
:-data(predicate/5).
:-data(saved_clause/3).
:-data(iterative_deepening/1).

register_predicate(F1, A, M):-
	register_predicate(F1, A, M, _).
register_predicate(F1, A, M, F2):- 
	(
	    predicate(F1, A, M, _, F2) ->
	    true
	;
	    generate_alternative_name(F1, F2), 
	    assertz_fact(predicate(F1, A, M, induc, F2))
	).
new_coinductive(F1, A, M):-
	(
	    predicate(F1, A, M, Type, _) ->
	    (
		Type = coind(_) ->
		true
	    ;
		retract_fact(predicate(F1, A, M, _, F2)),
		get_c(M, I),
		assertz_fact(predicate(F1, A, M, coind(I), F2))
	    )
	;
	    generate_alternative_name(F1, F2),
	    get_c(M, I),
	    assertz_fact(predicate(F1, A, M, coind(I), F2))
	).

add_coinductive([], _Mod).
add_coinductive([F/A|T], Mod):-
	new_coinductive(F, A, Mod),
	add_coinductive(T, Mod).



full_with([], _X).
full_with([X|L], X):-full_with(L, X).

generate_entries(Clauses, Tail, Mod):-
	findall(predicate(F1, A, F2), predicate(F1, A, _, _, F2), L), 
	generate_entries(L, Clauses, Tail, Mod).
generate_entries([], Tail, Tail, _Mod).
generate_entries([predicate(F1, A, F2)|T1], [(Head :- Body)|T2], Tail, Mod):-
	functor(Head, F1, A), Head =.. [_| Args], 
	( cmptr(Mod, I) -> true; I=0 ),
	 functor(Arg, '$colp', I), Arg =.. [_|L], full_with(L, []), 
	(
	    iterative_deepening(Mod) -> 
	    Added_Args = [Arg, K, 0],
	    Body = ('$for'(K), G)
	;
	    Added_Args = [Arg],
	    Body = G
	),
	lists:append(Args, Added_Args, Args2), 
	G =.. [F2|Args2], 
	generate_entries(T1, T2, Tail, Mod).

/*
generate_block_declaration(F, A, Mod, Args):-
	lists:length(L1, A), 
	full_with(L1, '-'),
	(
	    iterative_deepening(Mod) -> 
	    Args_Supp = [?,?,?]
	;
	    Args_Supp = [?]
	),
	lists:append(L1, Args_Supp, L),    
	Args=..[F|L].


generate_block_declarations([], Tail, Tail, _Mod).
generate_block_declarations([F/A|T1], [(:-block(Args))|T2], Tail, Mod):-
	generate_block_declaration(F, A, Mod, Args),
	generate_block_declarations(T1, T2, Tail, Mod).


generate_block_declarations(Clauses, Tail, Mod):-
	findall(F2/A, predicate(_, A, _, coind(_), F2), L), 
	generate_block_declarations(L, Clauses, Tail, Mod).
*/

translate_goal(X, X, _, _, _):-var(X), !.
translate_goal((X1,X2), (Y1, Y2), Mod, Arg, ID):-!,
	translate_goal(X1, Y1, Mod, Arg, ID),
	translate_goal(X2, Y2, Mod, Arg, ID).
translate_goal((X1;X2), (Y1; Y2), Mod, Arg, ID):-!,
	translate_goal(X1, Y1, Mod, Arg, ID),
	translate_goal(X2, Y2, Mod, Arg, ID).
translate_goal(M:G, M:G, _Mod, _Arg, _ID):-!.
translate_goal(X, Y, Mod, Arg, (I,J)):- 
	functor(X, F1, A), 
	predicate(F1, A, Mod, _, F2), !, 
	X =.. [_|Args],
	(
	    iterative_deepening(Mod) ->
	    Supp_Args = [Arg, I, J]
	;
	    Supp_Args = [Arg]
	),
	lists:append(Args, Supp_Args, Args2), 
	Y =.. [F2|Args2].
translate_goal(X, X, _, _, _).	

translate_clause(Head, Body, HeadTrans, BodyTrans, Mod):-
	functor(Head, F1, A), 
	(
	    predicate(F1, A, Mod, Type, F2) ->
	    Head =.. [_| Args], 
	    (
		iterative_deepening(Mod) ->
		Args_Supp = [Mem, I, J],
		Body_Ind = (I > J, J2 is J +1, Goal)
	    ;
		Args_Supp = [Mem],
		Body_Ind = Goal
	    ),
	    lists:append(Args, Args_Supp, Args2), HeadTrans =.. [F2|Args2],
	    (
		Type = coind(L) ->
		BodyTrans = ('$colp2'(L, Mem, Head), Body_Ind), K = J2  % Goal), K = J	      
	    ;
		BodyTrans = Body_Ind, K = J2
	    )
	;
	    HeadTrans = Head, BodyTrans = Goal
	),
	translate_goal(Body, Goal, Mod, Mem, (I, K)). 

translate_clauses(Clauses, Tail, Mod):-
	(
	    retract_fact(saved_clause(Head,Body,Mod)) ->
	    translate_clause(Head, Body, HeadTrans, BodyTrans, Mod), 
	    Clauses = [(HeadTrans:-BodyTrans)|Tail1], 
	    translate_clauses(Tail1, Tail, Mod)
	;
	    Clauses=Tail
	).

treat_block_declaration((A1,B1), (A2,B2), Mod):-!, 
	treat_block_declaration(A1, A2, Mod),
	treat_block_declaration(B1, B2, Mod).
treat_block_declaration(Spec1, Spec2, Mod):-
	functor(Spec1, F1, A),
	register_predicate(F1, A, Mod, F2),
	Spec1 =.. [_|L1],
	(
	    iterative_deepening(Mod) ->
	    Supp_Args = [?,?,?]
	;
	    Supp_Args = [?]
	),
	lists:append(L1, Supp_Args, L2), 
	Spec2 =.. [F2|L2].

generate_coinductive_pre([], Tail, Tail, _Mod).
generate_coinductive_pre([pred(F1, A, I, F2)|T], 
	  [(:-discontiguous(F2/N)), (Head :- '$colp1'(I, Mem, Goal)) |Tail1],
	  Tail2, Mod):-
        functor(Goal, F1, A), Goal =.. [_|Args],
	(
	    iterative_deepening(Mod) ->
	    Supp_Args = [Mem, _, _], N is A +3
	;
	    Supp_Args = [Mem], N is A +1
	),
	lists:append(Args, Supp_Args, L2), 
	Head =.. [F2|L2],
        generate_coinductive_pre(T,Tail1, Tail2, Mod).
        

generate_coinductive_pre(Tail1, Tail2, Mod):-
	findall(pred(F1, A, I, F2), predicate(F1, A, Mod, coind(I), F2), L), 
	generate_coinductive_pre(L, Tail1, Tail2, Mod).

east_sentence(A, B, C):-
	sentence_tr(A, B, C),
 	true.
	
sentence_tr(0, [(:- use_module(library(colp(east_colp_rt))))], Mod):-!,
	retractall_fact(cmptr(Mod, _)),
	retractall_fact(predicate(_, _, Mod, _, _)),
	retractall_fact(saved_clause(_, _, Mod)),
	retractall_fact(iterative_deepening(Mod)).
sentence_tr(end_of_file, Clauses, Mod):-!,
	generate_entries(Clauses, Tail1, Mod), 
%	generate_block_declarations(###Tail1###, ###Tail2###, Mod),
	generate_coinductive_pre(Tail1, Tail2, Mod),
	translate_clauses(Tail2, Tail3, Mod), 
	Tail3 = [end_of_file].
sentence_tr((:-coinductive(Args)), [], Mod):-!,
	(
	    list(Args) ->
	    Args1 = Args
	;
	    conj2list(Args, Args1)
	), 
	add_coinductive(Args1, Mod).
sentence_tr((:-block(Args1)), (:-block(Args2)), Mod):-
	treat_block_declaration(Args1, Args2, Mod).
sentence_tr((:-breadth_first), [], Mod):-
	assertz_fact(iterative_deepening(Mod)).
sentence_tr((Head :- Body), [], Mod):-!,
	functor(Head, F, A),
	register_predicate(F, A, Mod),
	assertz_fact(saved_clause(Head, Body, Mod)).
sentence_tr(Fact, [], Mod):- \+ (functor(Fact, ':-', 1)), !,
	functor(Fact, F, A),
	register_predicate(F, A, Mod),
	assertz_fact(saved_clause(Fact, true, Mod)).
sentence_tr(A, [A], _Mod).


