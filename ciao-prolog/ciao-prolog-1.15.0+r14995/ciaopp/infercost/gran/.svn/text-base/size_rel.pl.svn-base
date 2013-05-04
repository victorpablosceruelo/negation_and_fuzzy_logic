:- module(size_rel, [input_arg_size_relation/6], [assertions]).

%
%  size_relations.pl			Pedro Lopez-Garcia		October, 1992
%  

%
%  This file contains the procedures to obtain the expresions of the sizes of 
%  the input positions in the body of a clause as a function of the input 
%  positions in the head.
%
%  A list of "iasize(Term/ArgNum ,Size)" is built.
%

:- use_module(infercost(algebraic(simpl_form)), [simplification/2]).
:- use_module(infercost(dependency(adg)), [find_adg_field/4]).
:- use_module(infercost(dependency(position)), 
	[
	    gen_clause_pos/2,
	    pos_argnum/2,
	    pos_litnum/2
	]).
:- use_module(infercost(gran(size_rel_basic)), [generate_size_key/4]).
:- use_module(infercost(gran(gran_table)), [insert_gran_field/4]).
:- use_module(infercost(init(symtable)), 
	[
	    find_symbol_field/4,
	    find_symbol_field_clause/3,
	    literal_property/5
	]).
:- use_module(infercost(init(builtin)), 
	[second_order_predicate_pred_arg/2]).
:- use_module(infercost(size(size_)), 
	[
	    explicit_output_size/8,
	    implicit_output_size/8
	]).
:- use_module(infercost(size(normalize_)), 
	[
	    normalize/13,
	    init_normalize_queue/3
	]).
:- use_module(infercost(size(clause)), 
	[
	    ith_clause_literal/3,
	    number_of_literals/3
	]).
:- use_module(infercost(top(utility)), [ith_list_element/3]).


%
%  Perform the argument size analysis for a strongly connected component.
%

input_arg_size_relation([],_,_,_,_,_).
input_arg_size_relation([Pred|CompList],BT,ST,[Adg|AList],[Gvars|GList],GT):-
	find_symbol_field_clause(ST,Pred,Clauses),
	input_arg_size_rel_clauses(Clauses,BT,ST,Adg,Gvars,Size),
	insert_gran_field(GT,Pred,sizes,Size),
	input_arg_size_relation(CompList,BT,ST,AList,GList,GT).

%
%  Perform the argument size analysis for the set of clauses in a predicate.
%

input_arg_size_rel_clauses(Clauses,_,_,_,_,[]):-
	var(Clauses).
input_arg_size_rel_clauses(Clauses,BT,ST,[Adg|AList],[Gvars|GList],
                           [Size|SList]):-
	nonvar(Clauses),
	Clauses = [Clause|CList],
	in_arg_size_rel_clause(Clause,BT,ST,Adg,Gvars,Size),
	input_arg_size_rel_clauses(CList,BT,ST,AList,GList,SList).

%
%  Perform the argument size analysis for a clause.
%

in_arg_size_rel_clause(Clause,BT,ST,Adg,Gvars,Size) :-
	gen_clause_pos(Adg,InPos),
        input_arg_size_rel_func(InPos,Clause,BT,ST,Adg,Gvars,Size).
%
%  Compute the size functions of the input positions in the body of a clause.
%

input_arg_size_rel_func([],_,_,_,_,_,[]).
input_arg_size_rel_func([InPos|IPList],Clause,BT,ST,Adg,Gvars,
                        [ Entry |SList]) :-
        clause_term_measure_lit(BT,ST,Clause,InPos,Term,Measure,Lit),
	in_arg_size_rel_func_pos(InPos,Measure,Term,Clause,BT,ST,Adg,Gvars,
                                 Size, ExplSize),
        simplification(Size,SimSize),
        simplification(ExplSize,SimExplSize),
        pos_argnum(InPos,ArgNum),
        pos_litnum(InPos,LitNum),
        generate_size_key(Lit,LitNum,ArgNum,Key),
        Entry = iasize(Key,se(SimSize,SimExplSize)), 
	input_arg_size_rel_func(IPList,Clause,BT,ST,Adg,Gvars,SList).

clause_term_measure_lit(BT,ST,Clause,Pos,Term,Measure,NewLit) :-
	pos_litnum(Pos,LitNum),
	(LitNum > 0 ->
		(arg(2,Clause,Body),
		 number_of_literals(Body,1,Num),
		 (LitNum > Num ->
			NewNum is LitNum-Num;	% assume depth-one single pred
			NewNum = LitNum));
		NewNum = LitNum),
	ith_clause_literal(NewNum,Clause,Lit),
	((LitNum > 0, LitNum > Num) ->
		second_order_predicate_pred_arg(Lit,NewLit);
		NewLit = Lit),
	pos_argnum(Pos,ArgNum),
	arg(ArgNum,NewLit,Term),
	functor(NewLit,F,A),
	literal_property(BT,ST,F/A,measure,MeasureList),
	ith_list_element(ArgNum,MeasureList,Measure).
	

%
%  Compute the size function for an input position in the body of a clause . 
%  


 %% in_arg_size_rel_func_pos(InPos,Measure,Term,Clause,BT,ST,Adg,Gvars,Size):-
 %%       var(Term),
 %%       implicit_output_size(Measure,Term,Adg,Clause,BT,ST,Gvars,Size1),
 %%       ciao:swap_caslog_approximation(OldApprox, _Approx),
 %%       implicit_output_size(Measure,Term,Adg,Clause,BT,ST,Gvars,Size2),
 %%       ciao:caslog_approximation(OldApprox),
 %%       ( (Size1 == Size2, Size1 \== bot)
 %%          -> Size = Size1
 %%         ; 
 %%         in_arg_size_rel_func_pos_1(InPos,Measure,Term,Clause,BT,ST,Adg,Gvars,Size)).     
 %% 
 %% in_arg_size_rel_func_pos_1(InPos,Measure,Term,Clause,BT,ST,Adg,Gvars,Size):-
 %%       explicit_output_size(Measure,Clause,BT,ST,Adg,Gvars,Term,Size1),
 %%       sr_normalize_size_function(Size1,InPos,BT,ST,Clause,Adg,Gvars,NorSize1),
 %%       ciao:swap_caslog_approximation(OldApprox, _Approx),
 %%       explicit_output_size(Measure,Clause,BT,ST,Adg,Gvars,Term,Size2),
 %%       sr_normalize_size_function(Size2,InPos,BT,ST,Clause,Adg,Gvars,NorSize2),
 %%       ciao:caslog_approximation(OldApprox),
 %%       (NorSize1 == NorSize2 ->
 %%              Size = NorSize1
 %%              ;
 %%              Size = Size1).

 %% % Old: Commented out by PLG 12 Oct 97
 %% in_arg_size_rel_func_pos(InPos,Measure,Term,Clause,BT,ST,Adg,Gvars,Size):-
 %% 	(var(Term) ->
 %% 		implicit_output_size(Measure,Term,Adg,Clause,BT,ST,
 %% 				     Gvars,Size1);
 %% 		Size1 = bot),
 %% 	%write(Size1),nl,
 %% 	(Size1 == bot ->
 %% 		(explicit_output_size(Measure,Clause,BT,ST,Adg,Gvars,Term,
 %% 				   Size2),
 %% 		 %write(Size2),nl,
 %% 		 sr_normalize_size_function(Size2,InPos,BT,ST,Clause,Adg,
 %% 					 Gvars,Size));
 %% 		Size = Size1).
 %% 

in_arg_size_rel_func_pos(InPos,Measure,Term,Clause,BT,ST,Adg,Gvars,Size,ExplSize):-
	(var(Term) ->
		implicit_output_size(Measure,Term,Adg,Clause,BT,ST,
				     Gvars,Size1);
		Size1 = bot),
	(Size1 == bot ->
		(explicit_output_size(Measure,Clause,BT,ST,Adg,Gvars,Term,
				   ExplSize),
		 sr_normalize_size_function(ExplSize,InPos,BT,ST,Clause,Adg,
					    Gvars,Size))
                ;
		Size = Size1,
                ExplSize = Size1).


%
%  Normalize the size function corresponding to an input position in the body 
%  of a clause.
%

sr_normalize_size_function( Size,Pos,BT,ST,Clause,Adg,Gvars,NSize) :-
	gen_clause_pos(Adg,PosSet),
	find_adg_field(Adg,Pos,pred,PredPos),
	init_normalize_queue(PredPos,QHead,QTail),
        ith_clause_literal(0,Clause,Lit),
        functor(Lit,F,N),
	find_symbol_field(ST,F/N,size,ISize),
        normalize(Size,QHead,QTail,BT,ST,[],Clause,Adg,Gvars,PosSet,ISize,
                     [],NSize).

/*
gen_input_positions(Adg,InPos):-
	gen_clause_pos(Adg,PosSet),
        gen_input_pos(PosSet,Adg,InPos).

gen_input_pos([],_,[]).

gen_input_pos([Pos|PosList],Adg,[Pos|InPosList]):-
	find_adg_field(Adg,Pos,(mode),+),
        pos_litnum(Pos,LitNum),
	LitNum > 0,
        gen_input_pos(PosList,Adg,InPosList).

gen_input_pos([Pos|PosList],Adg,InPosList):-
	find_adg_field(Adg,Pos,(mode),-),
        gen_input_pos(PosList,Adg,InPosList).

gen_input_pos([Pos|PosList],Adg,InPosList):-
	find_adg_field(Adg,Pos,(mode),+),
        pos_litnum(Pos,0),
        gen_input_pos(PosList,Adg,InPosList).
*/
