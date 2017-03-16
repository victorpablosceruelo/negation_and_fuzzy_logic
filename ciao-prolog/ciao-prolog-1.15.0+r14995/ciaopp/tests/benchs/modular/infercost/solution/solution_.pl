%
%  solution.pl			Nai-Wei Lin			December, 1991
%
%  This file contains the procedures for performing the solution size
%  analysis for the predicates in the program in topologically sorted order.
%

%
%  Perform the solution size analysis for a strongly connected component.
%
solution_analysis(Comp,BT,ST,Comp,Size,Adg,Gvars,Ldg,Sol) :-
	solution_analysis(Comp,BT,ST,Comp,Size,Adg,Gvars,Ldg,[],Sol).

solution_analysis([],_,_,_,_,_,_,_,_,[]).
solution_analysis([Pred|CompList],BT,ST,Comp,[Size|SList],[Adg|AList],
		  [Gvars|GList],[Ldg|LList],RSol,[Sol|OList]) :-
	find_symbol_field(ST,Pred,det,Sol1),
	(var(Sol1) ->
		(find_symbol_field(ST,Pred,clause,Clauses),
		 solution_clauses(Clauses,BT,ST,Comp,Size,Adg,Gvars,Ldg,
			RSol,Sol2),
		 %write(Sol2),nl,
		 solve_complexity_equ(Pred,ST,Comp,Sol2,Size,TSol),
		 %write(TSol),nl,
		 Sol3 = [TSol]);
		Sol3 = Sol1),
	solution_analysis(CompList,BT,ST,Comp,SList,AList,GList,LList,
		[comp(Pred,Sol3)|RSol],OList),
	remove_recursive_comps(Sol3,ST,det,Sol),
	insert_symbol_field(ST,Pred,det,Sol).

/*
%
%  Perform the solution size analysis for a predicate.
%
solution_predicate(Pred,BT,ST,Comp,Size,Adg,Gvars,Ldg,Sol) :-
	find_symbol_field(ST,Pred,det,Sol1),
	(var(Sol1) ->
		(find_symbol_field(ST,Pred,clause,Clauses),
		 solution_clauses(Clauses,BT,ST,Comp,Size,Adg,Gvars,Ldg,_,Sol2),
		 find_symbol_field(ST,Pred,mutex,Mutex),
		 solve_comp_equs(Pred,ST,Comp,Sol2,Size,Mutex,Sol),
		 insert_symbol_field(ST,Pred,det,[Sol]));
		Sol = Sol1).
*/

%
%  Perform the solution size analysis for the set of clauses in a predicate.
%
solution_clauses(Clauses,_,_,_,_,_,_,_,_,[]) :-
	var(Clauses).
solution_clauses(Clauses,BT,ST,Comp,[Size|SList],[Adg|AList],[Gvars|GList],
		 [Ldg|LList],RSol,[Sol|OList]) :-
	nonvar(Clauses),
	Clauses = [Clause|CList],
	solution_clause(Clause,BT,ST,Comp,Size,Adg,Gvars,Ldg,RSol,Sol),
	solution_clauses(CList,BT,ST,Comp,SList,AList,GList,LList,RSol,OList).

%
%  Perform the solution size analysis for a clause.
%
solution_clause(Clause,BT,ST,Comp,Size,Adg,Gvars,Ldg,RSol,Sol) :-
	clause_type(Clause,Type),
	solution_clause(Type,Clause,BT,ST,Comp,Size,Adg,Gvars,Ldg,RSol,Sol).

solution_clause(2,Clause,BT,ST,Comp,Size,Adg,Gvars,Ldg,RSol,Sol) :-
	Clause = (Head :- Body),
	(fail_body(Body) ->
		Sol = 0;
		(solution_head_input(Head,BT,ST,Clause,Adg,Gvars,Ldg),
		 no_of_cuts(Body,Cuts),
		 collect_test_literals(Body,BT,ST,Lits),
		 body_binding(1,Body,BT,ST,Comp,Clause,Size,Adg,Gvars,Ldg,
			det,Cuts,Lits,RSol),
		 solution_head_output(Head,Clause,Adg,Gvars,Ldg,Lits,Sol))).
solution_clause(3,Fact,BT,ST,_,_,Adg,Gvars,Ldg,_,Sol) :-
	solution_head_input(Fact,BT,ST,Fact,Adg,Gvars,Ldg),
	solution_head_output(Fact,Fact,Adg,Gvars,Ldg,[],Sol).

%
no_of_cuts((Lit,Body),No) :-
	no_of_cuts(Body,Nos),
	(Lit == (!) ->
		No is Nos+1;
		No = Nos).
no_of_cuts(Lit,No) :-
	nonsequence(Lit),
	(Lit == (!) ->
		No = 1;
		No = 0).

%
collect_test_literals(Lit,BT,ST,Lits) :-
	nonsequence(Lit),
	functor(Lit,F,A),
	literal_property(BT,ST,F/A,(mode),Mode),
	(test_predicate(Mode) ->
		(literal_property(BT,ST,F/A,relation,Rel),
		 (Rel \== inf ->
			Lits = [test(Lit,Rel)];
			Lits = []));
		Lits = []).
collect_test_literals((Lit,Body),BT,ST,Lits) :-
	functor(Lit,F,A),
	literal_property(BT,ST,F/A,(mode),Mode),
	(test_predicate(Mode) ->
		(literal_property(BT,ST,F/A,relation,Rel),
		 (Rel \== inf ->
			Lits = [test(Lit,Rel)|RestLits];
			Lits = RestLits));
		Lits = RestLits),
	collect_test_literals(Body,BT,ST,RestLits).
