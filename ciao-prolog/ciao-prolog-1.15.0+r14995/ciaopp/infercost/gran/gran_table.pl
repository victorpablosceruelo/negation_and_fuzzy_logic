:- module(gran_table,
	[
	    compute_exact_size_relations/3,   
	    find_gran_clauses_dicts/4,  
	    find_gran_field/4,  
	    insert_gran_field/4, 
	    insert_gran_clauses_type_dicts/5, 
	    get_pred_clauses_sizes_time_info/4  
	], [assertions]).

:- doc(author,"Pedro L@'{o}pez").  

:- doc(module,"Procedures for handling the granul table.
   The structure of the granul table is: 
@begin{verbatim}
	st(Pred/Arity,seq_clauses,par_clauses,ann_clauses,type,
                     sizes,var_dictionary).
@end{verbatim}
").


get_pred_clauses_sizes_time_info(Pred, Info, Par_Clauses, SizeRel):- 
	Info = info(GT, _ST, _NT, _NS),
	find_gran_entry(GT, Pred, Entry),
	Entry = st(Pred, _Seq_Clauses, Par_Clauses, 
                   _Ann_Clauses, _Type, SizeRel, _Var_Dict).
	% find_name_field(NT, Pred, actual_sizes, ASizeRel).

find_gran_clauses_dicts(GT, Pred, Clauses, Dicts):-
	find_gran_entry(GT,Pred,st(Pred,_,Clauses,_,_,_,Dicts)).

insert_gran_clauses_type_dicts(GT, Pred, Clauses, Type, Dicts):-
	Entry = st(Pred, _, Clauses,_, Type, _, Dicts),
	insert_gran_entry(GT,Pred,Entry).



:- pred insert_gran_entry(ST,Pred,Entry) # "Insert an entry for
   predicate Pred in the granul table.  If it is already in, return
   the entry as Entry; otherwise, an entry Entry for Pred is inserted
   and returned.".

insert_gran_entry(ST,Pred,Entry) :- 
	var(ST),
	!,
	Entry = st(Pred,_,_,_,_,_,_),
	ST = [Entry|_].
insert_gran_entry(ST,Pred,Entry) :- 
	nonvar(ST),
	(
	    ST = [Entry|_],
	    Entry = st(Pred,_,_,_,_,_,_) ->
	    true
	;
	    ST = [E|S],
	    E \== st(Pred,_,_,_,_,_,_),
	    insert_gran_entry(S,Pred,Entry)
	).

:- pred insert_gran_field(ST, Pred, Type, ClauseList) 
   # "Insert a clause for predicate Pred into the granul table.".

insert_gran_field(ST, Pred, Type, ClauseList) :-
	insert_gran_field_(Type, ST, Pred, ClauseList).

% Insert an open list of (sequential) clauses.
insert_gran_field_(seq_clauses,    ST, Pred, ClauseList) :-
	insert_gran_entry(ST, Pred, st(Pred,ClauseList,_,_,_,_,_)).
% Insert a (parallel) clause.
insert_gran_field_(par_clauses,    ST, Pred, Clause) :-
	insert_gran_entry(ST, Pred, st(Pred,_,ClauseList,_,_,_,_)),
	insert_gran_clause(ClauseList, Clause).
% Insert an open list of (annotated) clauses.
insert_gran_field_(ann_clauses,    ST, Pred, ClauseList) :-
	insert_gran_entry(ST, Pred, st(Pred,_,_,ClauseList,_,_,_)).
insert_gran_field_(type,           ST, Pred, Type) :-
	insert_gran_entry(ST, Pred, st(Pred,_,_,_,Type,_,_)).
insert_gran_field_(sizes,          ST, Pred, Sizes) :-
	insert_gran_entry(ST, Pred, st(Pred,_,_,_,_,Sizes,_)).
insert_gran_field_(var_dictionary, ST, Pred, Dict) :-
	insert_gran_entry(ST, Pred, st(Pred,_,_,_,_,_,DictList)),
	insert_gran_clause(DictList, Dict).

:- pred insert_gran_clause(ClauseList,Clause) # "Insert a clause at the end
   of a List.  Does not check if there are duplicated clauses.".

insert_gran_clause(ClauseList,Clause) :-
	var(ClauseList),
	!,
	ClauseList = [Clause|_].
insert_gran_clause(ClauseList,Clause) :-
	nonvar(ClauseList),
	ClauseList = [_|CList],
	insert_gran_clause(CList,Clause).


:- pred find_gran_entry(ST,Pred,Entry) # "Find the entry for predicate
   @var{Pred} in the granul table.  If it is in, return the entry as
   @var{Entry}; otherwise, @var{Entry} is returned as a variable.".

find_gran_entry(ST,_,_) :- 
	var(ST),
	!.
find_gran_entry(ST, Pred, Entry) :- 
	nonvar(ST),
	(
	    ST = [Entry|_],
	    Entry = st(Pred,_,_,_,_,_,_) ->
	    true
	;
	    ST = [E|S],
	    E = st(Pred1,_,_,_,_,_,_),
	    Pred \== Pred1,
	    find_gran_entry(S, Pred, Entry)
	).

:- pred find_gran_field(ST, Pred, Type, ClauseList)  
   # "Find a field for predicate Pred in granul table.".

find_gran_field(ST, Pred, Type, ClauseList) :-
	find_gran_field_(Type, ST, Pred, ClauseList).

find_gran_field_(seq_clauses,ST,Pred,ClauseList) :-
	find_gran_entry(ST,Pred,st(Pred,ClauseList,_,_,_,_,_)).
find_gran_field_(par_clauses,ST,Pred,ClauseList) :-
	find_gran_entry(ST,Pred,st(Pred,_,ClauseList,_,_,_,_)).
find_gran_field_(ann_clauses,ST,Pred,ClauseList) :-
	find_gran_entry(ST,Pred,st(Pred,_,_,ClauseList,_,_,_)).
find_gran_field_(type,ST,Pred,Type) :-
	find_gran_entry(ST,Pred,st(Pred,_,_,_,Type,_,_)).
find_gran_field_(sizes,ST,Pred,Sizes) :-
	find_gran_entry(ST,Pred,st(Pred,_,_,_,_,Sizes,_)).
find_gran_field_(var_dictionary,ST,Pred,DictList) :-
	find_gran_entry(ST,Pred,st(Pred,_,_,_,_,_,DictList)).

%-------------------------------------------------------------------

compute_exact_size_relations(GT, _NewGT, _):-
	var(GT),
	!.
compute_exact_size_relations(GT, NewGT, ExactGT):-
	nonvar(GT),
	nonvar(NewGT),
	GT = [st(Pred/Arity, Seqclau, Parclau, Annclau, Type, Size,
	    Dict)|GTList],
	NewGT = [st(Pred/Arity,_,_,_,_,NewSize,_)|NewGTList],
	compute_exact_size(Size, NewSize, ExactSize),
	ExactGT = [st(Pred/Arity, Seqclau, Parclau, Annclau, Type, ExactSize,
	    Dict)|ExactGTList],
    compute_exact_size_relations(GTList, NewGTList, ExactGTList).

compute_exact_size([],            [],                  []).
compute_exact_size([ClSiz|RSize], [NewClSiz|NewRSize], [ExClSiz|ExRSize]):-
	compute_one_clause_exact_size(ClSiz, NewClSiz, ExClSiz),
	compute_exact_size(RSize, NewRSize, ExRSize).

compute_one_clause_exact_size([],          [],
	    []).
compute_one_clause_exact_size([Siz|RSize], [NewSiz|NewRSize],
	    [ExSiz|ExRSize]):-
	compute_one_literal_exact_size(Siz,NewSiz,ExSiz),
	compute_one_clause_exact_size(RSize,NewRSize,ExRSize). 

compute_one_literal_exact_size(iasize(Lit, se(Siz, ExplSize)),
	    iasize(_NewLit, se(NewSiz, NewExplSize)), iasize(Lit, ExSiz) ):-
	(
	    Siz == NewSiz ->
	    ExSiz = Siz
	;
	    (
		ExplSize == NewExplSize -> 
		ExSiz = ExplSize
	    ;
		ExSiz = bot
	    )
	).

%-------------------------------------------------------------------
