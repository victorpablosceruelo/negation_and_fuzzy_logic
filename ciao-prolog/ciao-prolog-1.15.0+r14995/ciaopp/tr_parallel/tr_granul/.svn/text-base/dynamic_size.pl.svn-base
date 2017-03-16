:- module(dynamic_size,[
                       generate_initial_litnodes/8 
                       ],
                       [assertions]). 

:- doc(author,"Pedro L@'{o}pez").

% ciao library:
:- use_module(library(sort), [sort/2]). 
:- use_module(library(lists), [append/3]). 
:- use_module(library(aggregates), [findall/3]). 

% Ciaopp library
:- use_module(infercost(gran(gran_table)), [find_gran_field/4]). 
:- use_module(infercost(init), [find_symbol_field/4, 
                               clause_type/2 
                              ]). 

% Own library
:- use_module(g_utility, [get_arg_num_list/2, 
                          ins_without_dup/2,        
                          close_open_list/1,        
                          set_diff/3,               
                          set_inclusion/2,          
                          set_union/3,              
                          insert_in_open_list/2,    
                          combinations/2,             
                          is_in_open_list/2 ]).     
:- use_module(basic_trans, [get_literal_pred/4]).   
:- use_module(need_size_table, [find_size/3]).      

 %% Consider strongrly connected component.
 %% 
 %% Construct all possible transformation nodes.
 %% 
 %% A irreducible transformation has the format:
 %%    it(Label,Trans)
 %% 
 %% Label = label(Pred/Ar,Inar,OutAr)
 %% 
 %% Inar =list of arguments (number) needed to perform the computation.
 %% OutAr = list of arguments whose size is computed.
 %% 
 %% Trans = list of transformation nodes (the first one is the entry point).
 %% 
 %% Trans = [Node1, ...., NodeN]
 %% 
 %% Nodei = tn(Label,Graphs)
 %% 
 %% Graphs = [Clause1Graph, ...,ClauseNGraph] (in correspondence)
 %% 
 %% ClauseiGraph = sdg(OutNodes, LitNodes)
 %% 
 %% InNodes:
 %% OutNodes = [($(0, N1), SizeNode), ..., ($(0, Nn), SizeNode)] 
 %%  Example [($(0, 3), $(0, 2))]  
 %% LitNodes: litnode(Litnum, Label, SuppliedSizesgraph)
 %% 
 %% SuppliedSizesgraph = [($(Litnum, ArgNum), SizeNode), ..., ] 
 %% 
 %% A computation size specification = (Pred,OutSizes)

 %% To create a transformation node we need to create a size dependency
 %% graph for each clause. 
 %% 
 %% 1) Create one choice point for each size dependency graph: the
 %%    choice of a size dependency graph is independent from other size
 %%    dependency graphs. A transformation node is a list of size
 %%    dependency graphs obtained by computing all combinations.
 %%  
 %% 2) Create one choice point for each transformation node: here the
 %%    choice of a size dependency graph is dependent from other size
 %%  dependency graphs.


 % InSizes: list of input arguments. It is supposed that their sizes
 % are supplied by the head, since they are needed to perform the
 % spawning test.

generate_initial_litnodes(NeededBodySizes, SizeRel, InSizes, 
                          Body, GT, ST, LitNodes, IrTrans):-
        gen_sd_graph_body_lit_nodes(NeededBodySizes, _VisiSizes, SizeRel,
                       InSizes, Labels, VisiLabels, Body, GT, ST, LitNodes),
        search_irreducible_transformation(Labels, VisiLabels, GT, ST, [],
                                          IrTrans).


search_irreducible_transformation(Labels,_VisiLabels, _GT,_ST,InTran,InTran):-
	var(Labels),!.
search_irreducible_transformation(Labels,VisiLabels, GT,ST,InTran,OuTran):-
	nonvar(Labels),
	Labels = [Label|RestLabels],
	ins_without_dup(VisiLabels,Label), 
	generate_node(Label,RestLabels,VisiLabels,GT,ST,Node),
	search_irreducible_transformation(RestLabels,VisiLabels,GT,ST,
                                          [Node|InTran],OuTran).


generate_node(Label,Labels,VisiLabels,GT,ST,tn(Label,Graphs)):-
	Label = label(Pred, InSizes, OutSizes),
	find_symbol_field(ST,Pred,clause,Clauses),
	find_gran_field(GT,Pred,sizes,SizeRels),
	generate_sd_graphs(Clauses, InSizes, OutSizes,Labels,VisiLabels,
                           SizeRels,GT,ST,Graphs).


 %% generate_sd_graphs(+Clauses, +InSizes, +OutSizes, +SizeRel, -Graphs)
 %% Clauses: open list with clauses.
 %% InSizes: closed list with the supplied size argument numbers, needed
 %%          for size computation. Are the input arguments of the label
 %%          the node corresponds to. 
 %% OutSizes: closed list with the computed size argument numbers (whose
 %%         size is to be computed by the transformed predicate). Are the
 %%         output arguments of the label the node corresponds to.
 %% SizeRel: Size relations info.
 %% Graphs: closed list with size dependency graphs. One per clause.

generate_sd_graphs(Clauses,_InSizes,_OutSizes,_Labels,_VisiLabels,_SizeRel,
                   _GT,_ST,[]):-
        var(Clauses), !.
generate_sd_graphs(Clauses,InSizes,OutSizes,Labels,VisiLabels,
                   [SizeRel|SizRels],GT,ST,[Graph|RGraphs]):-
        nonvar(Clauses), 
	Clauses = [Cl|Rest],
	generate_one_sd_graph(OutSizes,SizeRel,InSizes,Labels,VisiLabels,
                              Cl,GT,ST,Graph),
	generate_sd_graphs(Rest,InSizes,OutSizes,Labels,VisiLabels,SizRels,
                           GT,ST,RGraphs).


generate_one_sd_graph(OutSizes,SizeRel,InSizes,Labels,VisiLabels,Cl,GT,
                      ST,Graph):-
        clause_type(Cl, Type),
        gen_sd_graph_t(Type,OutSizes,SizeRel,InSizes,Labels,VisiLabels,
                       Cl,GT,ST,Graph).


% A fact
gen_sd_graph_t(3,OutSizes,SizeRel,InSizes,_Labels,_VisiLabels,Head,_GT,
              _ST,sdg(HeadEgdes,[])):-
        gen_sd_graph_edges(OutSizes,_VisiSizes,Head,0,SizeRel,InSizes,
                           HeadEgdes,_NeededBodySizes).
% Rule
gen_sd_graph_t(2,OutSizes,SizeRel,InSizes,Labels,VisiLabels,(Head:-Body),
               GT,ST,sdg(HeadEgdes,LitNodes)):-
        gen_sd_graph_edges(OutSizes,VisiSizes,Head,0,SizeRel,InSizes,
                           HeadEgdes,NeededBodySizes),
	gen_sd_graph_body_lit_nodes(NeededBodySizes,VisiSizes,SizeRel,
                           InSizes,Labels,VisiLabels,Body,GT,ST,LitNodes),
	close_open_list(LitNodes).

 %% Note that it is possible to avoid checking that a litnode is in the
 %% list by procesing all body size vars together, so that the
 %%         transformation node is generated in two alternative phases.
 %%    1) generate all size relations and collect all needed body sizes
 %%    2) Generate a set of labels from these needed body sizes and 
 %%       process each label. We get a literal node for each label.
 %% 

gen_sd_graph_body_lit_nodes(NeededBodySizes,_VisiSizes,_SizeRel,_InSizes,
                            _Labels,_VisiLabels,_Body,_GT,_ST,_):-
        var(NeededBodySizes), !.
gen_sd_graph_body_lit_nodes(NeededBodySizes,VisiSizes,SizeRel,InSizes,
                            Labels,VisiLabels,Body,GT,ST,LitNodes):-
	nonvar(NeededBodySizes), 
	NeededBodySizes = [BodySize|RestNBS],
	ins_without_dup(VisiSizes, BodySize),
	BodySize = $(LitNum,ArgNum),
	get_literal_pred(LitNum,Body,Literal,Pred),
	gen_body_choice_labels(LitNum,ArgNum,VisiLabels,Labels,RestNBS,
                               VisiSizes,Pred,GT,ST,ChoiceLabels),
        member(Label, ChoiceLabels),   
	(
	    there_is_a_lit_node_with_the_same_label(LitNodes, LitNum,Label) 
        -> true
	; 
	    actualize_labels(Labels, VisiLabels, Label),
	    Label = label(Pred, InSizs, _OutSizs),
	    gen_sd_graph_edges(InSizs,VisiSizes,Literal,LitNum,SizeRel,
                               InSizes,LitGraph,RestNBS),
	    LiNode = litnode(LitNum, Label, LitGraph),
	    insert_in_open_list(LitNodes, LiNode)
	),
	gen_sd_graph_body_lit_nodes(RestNBS,VisiSizes,SizeRel,InSizes,
                                    Labels,VisiLabels,Body,GT,ST,LitNodes).


there_is_a_lit_node_with_the_same_label(LitNodes, _LitNum, _Label):-
	var(LitNodes),
	!,
	fail.
there_is_a_lit_node_with_the_same_label(LitNodes, LitNum,Label):-
	nonvar(LitNodes),
	LitNodes = [Node|_RestNodes],
	Node = litnode(LitNum1, Label1, _LitGraph),
	LitNum1 == LitNum,
	Label == Label1,!.
there_is_a_lit_node_with_the_same_label(LitNodes, LitNum,Label):-
	nonvar(LitNodes),
	LitNodes = [_Node|RestNodes],
	there_is_a_lit_node_with_the_same_label(RestNodes, LitNum, Label).


actualize_labels(Labels, VisiLabels, Label):-
	(
	    is_in_open_list(VisiLabels, Label) -> true
	; 
	    ins_without_dup(Labels, Label)
	).


gen_body_choice_labels(LitNum,ArgNum,VisiLabels,Labels,NeededBodySizes,
                       VisiSizes,Pred,GT,ST,AllChoices):-
        create_needed_output_arg_size_nums(VisiSizes,LitNum,OutArgNums),
	create_needed_output_arg_size_nums(NeededBodySizes,LitNum,OutArgNums),
	close_open_list(OutArgNums),
	sort(OutArgNums, SorOutArgNums),
	get_valid_labels(Pred,SorOutArgNums,VisiLabels,ChoiceLabels),
	get_valid_labels(Pred,SorOutArgNums,Labels,ChoiceLabels),
	get_valid_labels(Pred,[ArgNum],VisiLabels,ChoiceLabels),    
	get_valid_labels(Pred,[ArgNum],Labels,ChoiceLabels),
	ins_without_dup(ChoiceLabels,label(Pred,[],SorOutArgNums)),
	close_open_list(ChoiceLabels),
	find_gran_field(GT,Pred,sizes,PredSizeRels),
	generate_labels((Pred,SorOutArgNums), PredSizeRels, ST,
                        FactAndRestLabels),
        set_diff(FactAndRestLabels, ChoiceLabels, DiffChoices),
	append(ChoiceLabels, DiffChoices, AllChoices). 


create_needed_output_arg_size_nums(SizeVars,_LitNum,_OutArgNums):-
	var(SizeVars),!.
create_needed_output_arg_size_nums(SizeVars,LitNum,OutArgNums):-
	nonvar(SizeVars),
	SizeVars = [$(LitNum,ArgNum)|Rest],!,
	ins_without_dup(OutArgNums,ArgNum),
	create_needed_output_arg_size_nums(Rest,LitNum,OutArgNums).
create_needed_output_arg_size_nums(SizeVars,LitNum,OutArgNums):-
	nonvar(SizeVars),
	SizeVars = [_|Rest],
	create_needed_output_arg_size_nums(Rest,LitNum,OutArgNums).


get_valid_labels(_Pred,_OutArgNums,Labels,_ChoiceLabels):-
	var(Labels),!.
get_valid_labels(Pred,OutArgNums,Labels,ChoiceLabels):-
	nonvar(Labels),
	Labels = [Lab|Rest],
	Lab = label(Pred,_In,Out),
	set_inclusion(OutArgNums,Out),
	!,
	ins_without_dup(ChoiceLabels,Lab),
	get_valid_labels(Pred,OutArgNums,Rest,ChoiceLabels).
get_valid_labels(Pred,OutArgNums,Labels,ChoiceLabels):-
	nonvar(Labels),
	Labels = [_Lab|Rest],
	get_valid_labels(Pred,OutArgNums,Rest,ChoiceLabels).

 %% gen_sd_graph_edges(+OutSizes,+VisiSizes,+Literal,+LitNum,+SizeRel,
 %%                    +InSizes,-Egdes,?NeededBodySizes)
 %% OutSizes: list of argument numbers whose size has to be computed by a
 %% transformed literal and/or supplied by the head and/or by an
 %% arithmetic operation.
 %% VisiSizes: closed list with visited body size variables.
 %%  a body size variables has the format $(LitNum,ArgNum)
 %% Literal:Literal number to which OutSizes refers to (LitNum = 0 for the
 %%         output arguments of the head.
 %% LitNum: Literal number to which OutSizes refers to (LitNum = 0 for the
 %%         output arguments of the head.
 %% SizeRel: Size relation info.
 %% InSizes: list with argument numbers wose size is supplied to compute
 %%         the size of output arguments od the head.
 %% Egdes: list of edges of the form [($(LitNum,ArgNum),SizeExpression),...,] 
 %%        Example of SizeExpression: $(2,3) + $(0,2) + 1.
 %% NeededBodySizes: open list with body size variables appearing in the
 %%         size expressions. No duplicated allowed.

gen_sd_graph_edges([],_VisiSizes,_Literal,_LitNum,_SizeRel,_InSizes,
                   [], _NeededBodySizes):-!.
gen_sd_graph_edges([ArgNum|OutSizes],VisiSizes,Literal,LitNum,SizeRel,InSizes,
                   [Egde|REgdes],NeededBodySizes):-
        generate_size_var(LitNum,ArgNum,SizeVar),
	generate_size_key(Literal,LitNum,ArgNum,Key),
	create_all_size_rel_choices(Key,InSizes,SizeRel,Exps),
	member(Exp, Exps),
	Exp = (Express, BodySizes),
	actualize_body_sizes(BodySizes, VisiSizes, NeededBodySizes),
	Egde = (SizeVar,Express),
	ins_without_dup(VisiSizes,SizeVar), 
	gen_sd_graph_edges(OutSizes,VisiSizes,Literal,LitNum,SizeRel,
                           InSizes,REgdes,NeededBodySizes).


generate_size_var(LitNum,ArgNum,$(LitNum,ArgNum)).

% actualize_body_sizes(+BodySizes, +VisiSizes, ?NeededBodySizes)
% Insert without duplicated body variables from BodySizes which are
% not in VisiSizes to NeededBodySizes.
 %% BodySizes: an open  list with body size variables. 
 %% VisiSizes: closed list with visited body size variables.
 %% NeededBodySizes: open list with body size variables which are not
 %%      visited (i.e. are not in VisiSizes). No duplicated are allowed.

actualize_body_sizes(BodySizes, _VisiSizes, _NeededBodySizes):-
	var(BodySizes),!.
actualize_body_sizes(BodySizes, VisiSizes, NeededBodySizes):-
	nonvar(BodySizes),
	BodySizes = [Bsize|Rest],
	ins_without_dup(NeededBodySizes,Bsize),
	actualize_body_sizes(Rest, VisiSizes, NeededBodySizes).


%% Choice point.
% Warning!, creates only one choice at moment.
create_all_size_rel_choices(Key,Insizes, SizeRel,[(Exp,BodySizes)]):-  
	find_size(SizeRel, Key, Exp),
	get_size_vars_from_exp(Exp,HeadSizes,BodySizes), 
	close_open_list(HeadSizes), 
	set_inclusion(HeadSizes, Insizes).

:- doc(bug, "Bad design: generate_size_key/2 violates Demitry's law.").

:- reexport(infercost(gran(size_rel_basic)), [generate_size_key/4]).

%% get_size_vars_from_exp(+Exp, -HeadSizes, -BodySizes) 
%% Exp: a size expression.
%% HeadSizes: open list with head argument numbers used in Exp
%%                 (e.g. [1,2,,...|_]).
%% BodySizes: open list with body size variables used in Exp
%%                 (e.g. [$(1,2), ..|_]).

get_size_vars_from_exp(Exp, _HeadSizes, _BodySizes):- 
	number(Exp),
	!.	
get_size_vars_from_exp($(N), HeadSizes, _BodySizes):- 
	number(N),
	!,
	ins_without_dup(HeadSizes, N).
get_size_vars_from_exp($(N,M), HeadSizes, _BodySizes):-
	number(N),
	N =:= 0,
	number(M),
	!,
	ins_without_dup(HeadSizes, N).
get_size_vars_from_exp($(N,M), _HeadSizes, BodySizes):-
	number(N),
	N > 0,
	number(M),
	!,
	ins_without_dup(BodySizes, $(N,M)).
get_size_vars_from_exp($(I), _HeadSizes, _BodySizes):-
	atom(I),
	!. % An index variable.
get_size_vars_from_exp(Exp, HeadSizes, BodySizes):- 
	functor(Exp,F,N),
        N>0,
	F \== $,
 	function_get_size_vars_from_exp(N,Exp,HeadSizes,BodySizes).


function_get_size_vars_from_exp(0,_Exp,_HeadSizes,_BodySizes):-!.
function_get_size_vars_from_exp(N,Exp,HeadSizes,BodySizes):-
	N > 0,
	arg(N,Exp,Arg),
        get_size_vars_from_exp(Arg, HeadSizes, BodySizes), 
	N1 is N-1,
	function_get_size_vars_from_exp(N1,Exp,HeadSizes,BodySizes).


% generate_labels(CompSizeSpec,SizeRels,ST,Labels).
% Generates a list of labels (Labels) for the computation size specification
% CompSizeSpec to be used in the searh algorithm. Each label is a choice.
% The list is ordered following an heuristics.

generate_labels((Pred/A,OutSizes),SizeRels,ST,Labels):-
	find_symbol_field(ST,(Pred/A),clause,Clauses),
   % generate a list with all the (input and output)  argument numbers of 
   % Pred/A.   
        get_arg_num_list(A,ArgNumList),
   % OutSizes is a list with the argument numbers whose size is going to be
   % computed by the transformation. PotInSizes are the rest of argument 
   % numbers
   % and can be used potentially to create labels, more precisely, to create
   % the set SuppArgs in a lalel label(Pred/A,SuppArgs,CompArgs).
        set_diff(ArgNumList,OutSizes,PotInSizes),
	generate_needed_sizes_from_facts(Clauses,PotInSizes,OutSizes,
                                         SizeRels, [], NeededSizes),
        findall(Comb, combinations(PotInSizes,Comb), AllCombs),
	sort(AllCombs,SAllCombs),
	set_diff(SAllCombs, NeededSizes, DiffChoices),
	append(NeededSizes, DiffChoices, AllChoices),
	create_all_labels(AllChoices, OutSizes, Pred/A,Labels).


create_all_labels([],_OutSizes, _Pred, []).
create_all_labels([NeedSiz|NeededSizes], OutSizes, Pred,
	          [label(Pred,NeedSiz,OutSizes)|Labels]):-
        create_all_labels(NeededSizes,OutSizes,Pred,Labels).


generate_needed_sizes_from_facts(Clauses,_InSizes,_OutSizes,_SizeRel,
                                 InNeeded,InNeeded):-
        var(Clauses), !.
generate_needed_sizes_from_facts(Clauses, InSizes,OutSizes,
                                 [SizeRel|SizRels], InNeeded, OutNeeded):-
        nonvar(Clauses), 
	Clauses = [Cl|Rest],
	clause_type(Cl, Type),
	Type =:= 3, !, % is a fact.
	gen_needed_input_sizes(OutSizes,InSizes,SizeRel,Cl,InNeeded,
                               TemNeeded),
	generate_needed_sizes_from_facts(Rest,InSizes,OutSizes,SizRels,
                                         TemNeeded,OutNeeded).
generate_needed_sizes_from_facts(Clauses, InSizes,OutSizes,
                                 [_SizeRel|SizRels], InNeeded, OutNeeded):-
        nonvar(Clauses), 
	Clauses = [Cl|Rest],
	clause_type(Cl, Type),
	Type =\= 3, % not a fact.
	generate_needed_sizes_from_facts(Rest,InSizes,OutSizes,SizRels,
                                         InNeeded, OutNeeded).


gen_needed_input_sizes(OutSizes,InSizes,SizeRel,Fact,InNeeded,Needed):-
	needed_input_sizes(OutSizes,InSizes,Fact,0,SizeRel,InNeeded, Needed).

   
needed_input_sizes([],_InSizes,_Literal,_LitNum,_SizeRel, InNeeded,InNeeded):-
	!.
needed_input_sizes([ArgNum|OutSizes],InSizes,Literal,LitNum,SizeRel,InNeeded,
                   NeededSizes):-
        generate_size_key(Literal,LitNum,ArgNum,Key),
	create_all_needed_size_choices(Key,InSizes,SizeRel,HeadSizes),
  % HeadSizes is a list containing lists of items in InSizes.
  % Example [[1,2,3], [1,2], [1,3]].
  % each item is an ordered list.
  % Choice size relation to use
        member(HeadSiz, HeadSizes),
  % was: member_choice(HeadSizes, HeadSiz),
        set_union(HeadSiz,InNeeded,NewNeeded), 
	needed_input_sizes(OutSizes,InSizes,Literal,LitNum,SizeRel,NewNeeded,
                           NeededSizes).


create_all_needed_size_choices(Key,Insizes,SizeRel,Choices):-
	findall(HeadSizes,create_needed_size_choices(Key,Insizes,SizeRel,
                                                     HeadSizes),Choices).
create_needed_size_choices(Key,Insizes,SizeRel,HeadSizes):-  
	find_size(SizeRel, Key, Exp),
     % This will bactrak in the future, or alternatively will return
     % a list (in this case some modification is needed).
        get_size_vars_from_exp(Exp,HeadSiz,_BodySizes), 
        close_open_list(HeadSiz),
        sort(HeadSiz,HeadSizes), 
     % A restriction to be met
        set_inclusion(HeadSizes, Insizes).



