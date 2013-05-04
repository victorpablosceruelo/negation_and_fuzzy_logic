%
%  comp_diff_equ.pl		Nai-Wei Lin			February, 1992
%
%  This file contains the programs for solving the complexity difference 
%  equations.
%

%
%  Solve the complexity equation for a predicate.
%  Size is the size functions for the clauses of the predicate;
%  while SIZE is the computed size functions for the predicate.
%
% Commented out by PLG (25-Mar-97)
 %% solve_complexity_equ(Pred,ST,_,Comp,Size,Sol) :-
 %% 	find_symbol_field(ST,Pred,size,SIZE),
 %% 	find_symbol_field(ST,Pred,(mode),Mode),
 %% 	(inconsistent_sizes(Mode,SIZE) ->
 %% 		Sol1 = inf;	% size functions for the pred undefined 
 %% 		(construct_clause_complexity_equs(Pred,ST,Size,Comp,ClauseEqu),
 %% 		 (mutual_exclusive_complexity(Pred,ST,ClauseEqu,ClassEqu) ->
 %% 			solve_comp_equs(ST,Pred,ClassEqu,Sol1);
 %% 			Sol1 = inf))),	% sizes in cluster inconsistent 
 %% 	simplification(Sol1,Sol).
% Added by PLG (25-Mar-97)

solve_complexity_equ(Pred,ST,_,Complexity,Size,Sol) :-
	find_symbol_field(ST,Pred,size,SIZE),
	find_symbol_field(ST,Pred,(mode),Mode),
	(inconsistent_sizes(Mode,SIZE) ->
		Sol1 = inf % size functions for the pred undefined 
                ;	        
                up_low_construct_clause_complexity_equs(Pred,ST,Size,Complexity,Sol1)
        ),	
	simplification(Sol1,Sol).

 %% up_low_construct_clause_complexity_equs(Pred,ST,Size,Complexity,Sol1)
 %% 
 %% Pred: predicate/arity.
 %% ST: symbol table.
 %% Size: size relations corresponding to clauses. A list per
 %% predicate, each item is another list (one per clause).  
 %%       Eg. [[0,0],[$(0,1),q(2,2,$(0,1)-1)+1]]
 %% q(2,2,$(0,1)-1) means the size of the second (output) argument of
 %% q/2 for an input of size $(0,1)-1.
 %% The first argument is the arity, the second is the output position
 %% to which the size refers to. This syntax is interpreted slightly
 %% different in the context of time-(complexity) analysis.
 %% Complexity: Eg. [1,q(2,1,$(0,1)-1)+1]
 %% q(2,1,$(0,1)-1) means the cost (time) of the predicate q/2 for an
 %% input of size $(0,1)-1. The first argument is the predicate arity, the second
 %% is irrelevant, and is always 1.
 %% Sol1: solution $(0,1)+1.
 %% 
 %% The previous data correspond to the following predicate.
 %% 
 %% :- entry(p(X,Y), [ground(X),list(X),var(Y)]).
 %% 
 %% p([],[]).
 %% p([X|Y], [X1|Y1]):- X1 is X + 1, p(Y, Y1). 
 %% p([X|Y], [X1|Y1]):- X1 is X + 7, q(Y, Y1). 
 %% 
 %% q([],[]). 
 %% q([X|Y], [X1|Y1]):- X1 is X + 2, q(Y, Y1).


up_low_construct_clause_complexity_equs(Pred,ST,Size,Complexity,Sol1):-
       construct_clause_complexity_equs(Pred,ST,Size,Complexity,ClauseEqu),
       approximation(Approx),
       (Approx == lower ->
                 find_symbol_field(ST,Pred,measure,Measure),
                 solve_comp_equs(ST,Pred,ClauseEqu,Sol1)
                 ;
		 (Approx == upper, 
                  (mutual_exclusive_complexity(Pred,ST,ClauseEqu,ClassEqu) ->
			solve_comp_equs(ST,Pred,ClassEqu,Sol1);
			Sol1 = inf % sizes in cluster inconsistent 
                  )
                 )
       ).
% End added
%
%  Test if the sizes are inconsistent.
%
inconsistent_sizes(Mode,SIZE) :-
	inconsistent_sizes(Mode,SIZE,1).

inconsistent_sizes([],[],0).
inconsistent_sizes([+|Mode],[bot|Size],_) :- 
	inconsistent_sizes(Mode,Size,0).
inconsistent_sizes([+|_],[S|_],_) :- S \== bot, !, fail.
inconsistent_sizes([-|Mode],[_|Size],I) :- inconsistent_sizes(Mode,Size,I).

%
%  Construct the complexity equation for each clause from the input size 
%  and complexity function for the clause.
%
construct_clause_complexity_equs(Pred,ST,Size,Comp,Equ) :-
	find_symbol_field(ST,Pred,(mode),Mode),
	input_sizes(Size,Mode,ISize),
	construct_comp_equs(ISize,Comp,1,Equ).

%
%  Compose the complexity equation for each mutually exclusive cluster
%  from the complexity equations of its clauses.
%
mutual_exclusive_complexity(Pred,ST,ClauseEqu,ClassEqu) :-
	find_symbol_field(ST,Pred,mutex,Mutex),
	mutual_exclusive_comp(Mutex,ClauseEqu,ClassEqu).

mutual_exclusive_comp([],_,[]).
mutual_exclusive_comp([MutexClass|MList],ClauseComp,[Comp|CList]) :-
	mutual_exclusive_comp1(MutexClass,ClauseComp,Comp),
	mutual_exclusive_comp(MList,ClauseComp,CList).

% Extra checks on the consistent input sizes are also performed.
% The form of the input size for all clauses are the same.
mutual_exclusive_comp1([],_,equ(_,_,Zero)) :-
	normal_form(0,Zero).
mutual_exclusive_comp1([ClauseNum|CList],ClauseComp,equ(ClauseNum,ISize,Comp)):-
	ith_list_element(ClauseNum,ClauseComp,CompEqu1),
	CompEqu1 = equ(ClauseNum,ISize,Comp1),
	mutual_exclusive_comp1(CList,ClauseComp,CompEqu2),
	CompEqu2 = equ(_,ISize,Comp2),
	add_expr(Comp1,Comp2,Comp).

%
%  Solve the complexity equations corresponding to the mutually exclusive
%  clusters of a predicate.
%
solve_comp_equs(ST,Pred,Equ,Sol) :-
	classify_equs(Equ,DEqu,BEqu),
	%write(DEqu),nl,
	%write(BEqu),nl,
	solve_comp_equs1(ST,Pred,DEqu,BEqu,NSol),
	general_form(NSol,Sol).

solve_comp_equs1(ST,Pred,DE,BE,Sol) :-
	abnormal_equs(BE,S),
	%write(S),nl,
	(general_form(S,0) ->
		solve_comp_equs2(DE,BE,ST,Pred,Sol);
                set_worst_result(Sol)).
                % Commented out by PLG
       		% Sol = inf).

solve_comp_equs2([],BE,_,_,Sol) :-
	solve_comp_non_diff_equs(BE,Sol).
solve_comp_equs2(DE,BE,ST,Pred,Sol) :-
	DE \== [],
	(indirect_recursion(DE,Pred) ->
		(solve_comp_non_diff_equs(DE,S1),
		 solve_comp_non_diff_equs(BE,S2),
		% Commented out by PLG (25-Mar-97) 
                % max_expr(S1,S2,Sol)
                % Added by PLG (25-Mar-97)
                up_low_approximation_expr(S1,S2,Sol)
                );
		solve_comp_diff_equs(DE,BE,ST,Pred,Sol)).

%
solve_comp_diff_equs(DE,BE,ST,Pred,Sol) :-
	exprs_one_index_reducible(DE,Pred,1,IS,RDE,Pos),
	(BE == [] ->
		implicit_boundary_condition(ST,Pred,Pos,RBE); % implicit failure
		boundary_condition_one_index_reducible(BE,IS,Pos,RBE)),
	%write(RBE),nl,
        % Added by PLG 
        replace_values_in_equs(RDE, RBE, Pred, NewRDE),
        solve_one_index_comp_diff_equs(NewRDE,RBE,ST,Pred,Pos,Sol),!.
        % End added.
        % Commented out by PLG:
	% solve_one_index_comp_diff_equs(RDE,RBE,ST,Pred,Pos,Sol),!.
solve_comp_diff_equs(DE,BE,_,Pred,Sol) :-
	exprs_two_indices_reducible(DE,Pred,1,IS,Pos),
	adjust_pos(Pos,NPos),
	reduce_two_indices_exprs(DE,Pred,1,IS,NPos,NDE),
	NPos = [Pos1,Pos2],
	POS1 is Pos1-2,
	POS2 is Pos2-2,
	boundary_condition_two_indices_reducible(BE,IS,[POS1,POS2],NBE),
	solve_two_indices_diff_equs(NDE,NBE,Pred,Sol),!.
% Commented out PLG
% solve_comp_diff_equs(_,_,_,_,inf).
% Added by PLG
solve_comp_diff_equs(_,_,_,_,Sol):-
      approximation(Approx),
        (Approx == lower -> 
           normal_form(0, Sol)
           ;
           Approx == upper, Sol = inf).
%End added.

%
%
% Commented out by PLG (25-Mar-97)
 %% solve_one_index_comp_diff_equs([],_,_,_,_,Zero) :- normal_form(0,Zero).
 %% solve_one_index_comp_diff_equs([equ(_,Var,OC)|DE],BE,ST,Pred,Pos,Sol) :-
 %% 	%diff_equ_type(OC,Var,Pred,An1,An2,Bn,Type),
 %% 	%general_form(OC,R),
 %% 	%write(R),nl,
 %% 	%write(Var),nl,
 %% 	(indirect_recursion_expr(OC,Pred) ->
 %% 		Sol1 = OC;
 %% 		solve_typed_diff_equ(comp,OC,BE,Var,ST,Pred,Pos,Sol1)),
 %% 	%solve_one_index_comp_diff_equ(Type,BE,Var,An1,An2,Bn,ST,Pred,Pos,Sol1),
 %% 	%general_form(Sol1,GSol),
 %% 	%write(GSol),nl,
 %% 	solve_one_index_comp_diff_equs(DE,BE,ST,Pred,Pos,Sol2),
 %% 	max_expr(Sol1,Sol2,Sol).
% Added by PLG (25-Mar-97)

solve_one_index_comp_diff_equs([DEqu],BE,ST,Pred,Pos,Sol):-!,
        solve_one_index_one_comp_diff_equation(DEqu,BE,ST,Pred,Pos,Sol).
solve_one_index_comp_diff_equs([DEqu|DE],BE,ST,Pred,Pos,Sol):-
        solve_one_index_one_comp_diff_equation(DEqu,BE,ST,Pred,Pos,Sol1),
	solve_one_index_comp_diff_equs(DE,BE,ST,Pred,Pos,Sol2),
	up_low_approximation_expr(Sol1,Sol2,Sol).

solve_one_index_one_comp_diff_equation(equ(_,Var,OC),BE,ST,Pred,Pos,Sol1) :-
	(indirect_recursion_expr(OC,Pred) ->
		Sol1 = OC;
		solve_typed_diff_equ(comp,OC,BE,Var,ST,Pred,Pos,Sol1)).
% End added

solve_one_index_comp_diff_equ(second_order,BE,Var,An1,An2,Bn,_,_,_,Sol) :-
	!,
	boundary_conds(BE,Ival),
	solve_diff_equ(second_order,Var,An1,An2,Bn,Ival,Sol).
solve_one_index_comp_diff_equ(Type,BE,Var,An1,An2,Bn,ST,Pred,Pos,Sol) :-
	solve_one_index_comp_diff_equ1(BE,Type,Var,An1,An2,Bn,ST,Pred,Pos,Sol).

% Commented out by PLG (25-Mar-97)
 %% solve_one_index_comp_diff_equ1([],_,_,_,_,_,_,_,_,Zero) :- normal_form(0,Zero).
 %% solve_one_index_comp_diff_equ1([E|BE],Type,Var,An1,An2,Bn,ST,Pred,Pos,Sol) :-
 %% 	E = equ(_,Index,Val),
 %% 	(integer(Index) ->
 %% 		(boundary_conds([E],Ival),
 %% 		 %write(Ival),nl,
 %% 		 solve_diff_equ(Type,Var,An1,An2,Bn,Ival,Sol1));
 %% 		(comp_boundary_cond(E,ST,Pred,Pos,Ival),
 %% 		 %write(Ival),nl,
 %% 		 solve_diff_equ(Type,Var,An1,An2,Bn,Ival,TS),
 %% 		 %write(TS),nl,
 %% 		 max_expr(TS,Val,Sol1))),
 %% 	%general_form(Sol1,GSol),
 %% 	%write(GSol),nl,
 %% 	solve_one_index_comp_diff_equ1(BE,Type,Var,An1,An2,Bn,ST,Pred,Pos,Sol2),
 %% 	max_expr(Sol1,Sol2,Sol).

% Added by PLG (25-Mar-97)

solve_one_index_comp_diff_equ1([E],Type,Var,An1,An2,Bn,ST,Pred,Pos,Sol):-!,
        solve_one_index_one_comp_diff_equation1(E,Type,Var,An1,An2,Bn,ST,Pred,Pos,Sol).
solve_one_index_comp_diff_equ1([E|BE],Type,Var,An1,An2,Bn,ST,Pred,Pos,Sol):-
        solve_one_index_one_comp_diff_equation1(E,Type,Var,An1,An2,Bn,ST,Pred,Pos,Sol1),
	solve_one_index_comp_diff_equ1(BE,Type,Var,An1,An2,Bn,ST,Pred,Pos,Sol2),
	up_low_approximation_expr(Sol1,Sol2,Sol).

solve_one_index_one_comp_diff_equation1(E,Type,Var,An1,An2,Bn,ST,Pred,Pos,Sol1) :-
	E = equ(_,Index,Val),
	(integer(Index) ->
		(boundary_conds([E],Ival),
		 solve_diff_equ(Type,Var,An1,An2,Bn,Ival,Sol1));
		(comp_boundary_cond(E,ST,Pred,Pos,Ival),
		 solve_diff_equ(Type,Var,An1,An2,Bn,Ival,TS),
		 up_low_approximation_expr(TS,Val,Sol1))).


%
%  Compute the boundary condition for a complexity difference equation.
%
comp_boundary_cond(equ(Num,Var,Val),ST,Pred,Pos,[val(Iind,Ival)]) :-
	min_unifiable_term_sizes(ST,Pred,Num,Pos,MinSize),
	(integer(MinSize) ->
		(normal_form(MinSize,Iind),
		 general_form(Val,Nval),
		 substitute(Nval,Var,MinSize,Sval),
		 normal_form(Sval,Ival));
		(Iind = bot,
		 Ival = bot)).

min_unifiable_term_sizes(ST,Pred,Num,Pos,MinSize) :-
	find_symbol_field(ST,Pred,mutex,Mutex),
	mutex_cluster(Mutex,Num,Cluster),
	(min_unifiable_term_sizes1(Cluster,ST,Pred,Pos,_,MS) ->
		MinSize = MS;
		MinSize = bot).

mutex_cluster([],_,[]).
mutex_cluster([M|Mutex],Num,Cluster) :-
	(member(M,Num) ->
		Cluster = M;
		mutex_cluster(Mutex,Num,Cluster)).

min_unifiable_term_sizes1([],_,_,_,MinSize,MinSize) :- integer(MinSize).
min_unifiable_term_sizes1([Num|Cluster],ST,Pred,Pos,MinSize,FMinSize) :-
	min_unifiable_term_size(ST,Pred,Num,Pos,MinSize1),
	(var(MinSize) ->
		(integer(MinSize1),
		 min_unifiable_term_sizes1(Cluster,ST,Pred,Pos,MinSize1,
			FMinSize));
		(integer(MinSize1),
		 MinSize =:= MinSize1,
		 min_unifiable_term_sizes1(Cluster,ST,Pred,Pos,MinSize1,
			FMinSize))).

implicit_boundary_condition(ST,Pred,Pos,[equ(_,Size,Zero)]) :-
	find_symbol_field(ST,Pred,measure,Measure),
	find_symbol_field(ST,Pred,(mode),Mode),
	real_pos(Mode,Pos,1,RealPos),
	ith_list_element(RealPos,Measure,M),
	find_symbol_field(ST,Pred,clause,Clauses),
	min_unifiable_implicit_term_size(Clauses,M,RealPos,MinSize),
	%write(MinSize),nl,
	Size is MinSize-1,
	normal_form(0,Zero).

min_unifiable_implicit_term_size(Clauses,_,_,inf) :-
	var(Clauses).
min_unifiable_implicit_term_size(Clauses,M,Pos,MinSize) :-
	nonvar(Clauses),
	Clauses = [C|Cs],
	min_unifiable_term_size1(M,C,Pos,MS1),
	min_unifiable_implicit_term_size(Cs,M,Pos,MS2),
        % Warning: check this!
	minimum(MS1,MS2,MinSize).



