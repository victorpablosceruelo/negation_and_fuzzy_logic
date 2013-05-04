:- module(symtable,
	[
	    insert_symbol_entry/3,
	    insert_symbol_field/4,
	    find_symbol_field/4,
	    find_symbol_entry/3,
	    literal_property/5,
	    find_symbol_field_clause/3,
	    get_input_arglist_from_st/3
	], [assertions]).

%
%  This module contains the procedures for handling the symbol table.
%
%  The structure of the symbol table:
%	st(Pred/Arity,clause,mode,measure,mutex,det,size,solution,time)
%

%
%  Insert an entry for predicate Pred in the symbole table.
%  If it is already in, return the entry as Entry;
%  otherwise, an entry Entry for Pred is inserted and returned.
%

insert_symbol_entry(ST,Pred,Entry) :- 
	var(ST),
	!,
	Entry = st(Pred,_,_,_,_,_,_,_,_,_),
	ST = [Entry|_].
insert_symbol_entry(ST,Pred,Entry) :- 
	nonvar(ST),
	(
	    ST = [Entry|_],
	    Entry = st(Pred,_,_,_,_,_,_,_,_,_) ->
	    true
	;
	    ST = [E|S],
	    E \== st(Pred,_,_,_,_,_,_,_,_,_),
	    insert_symbol_entry(S,Pred,Entry)
	).

insert_symbol_clause(ClauseList,Clause) :-
	var(ClauseList),
	!,
	ClauseList = [Clause|_].
insert_symbol_clause(ClauseList,Clause) :-
	nonvar(ClauseList),
	ClauseList = [C|CList],
	(
	    C == Clause ->
	    true
	;
	    insert_symbol_clause(CList,Clause)
	).

insert_symbol_field(ST, Pred, Type, Symbol) :-
	insert_symbol_field_(Type, ST, Pred, Symbol).

%
%  Insert a clause for predicate Pred into the symbol table.
%

insert_symbol_field_(clause,   ST, Pred, Clause) :-
	insert_symbol_entry(ST,Pred,st(Pred,ClauseList,_,_,_,_,_,_,_,_)),
	insert_symbol_clause(ClauseList,Clause).

%
%  Insert a declaration for predicate Pred into the symbol table.
%

insert_symbol_field_((mode),   ST, Pred, Mode) :-
	insert_symbol_entry(ST, Pred, st(Pred,_,Mode,_,_,_,_,_,_,_)).
insert_symbol_field_(measure,  ST, Pred, Measure) :-
	insert_symbol_entry(ST, Pred, st(Pred,_,_,Measure,_,_,_,_,_,_)).
insert_symbol_field_(mutex,    ST, Pred, Mutex) :-
	insert_symbol_entry(ST, Pred, st(Pred,_,_,_,Mutex,_,_,_,_,_)).
insert_symbol_field_(det,      ST, Pred, Det) :-
	insert_symbol_entry(ST, Pred, st(Pred,_,_,_,_,Det,_,_,_,_)).
insert_symbol_field_(size,     ST, Pred, Size) :-
	insert_symbol_entry(ST, Pred, st(Pred,_,_,_,_,_,Size,_,_,_)).
insert_symbol_field_(relation, ST, Pred, Solution) :-
	insert_symbol_entry(ST, Pred, st(Pred,_,_,_,_,_,_,Solution,_,_)).
insert_symbol_field_(time,     ST, Pred, Time) :-
	insert_symbol_entry(ST, Pred, st(Pred,_,_,_,_,_,_,_,Time,_)).
insert_symbol_field_(domain,   ST, Pred, Domain) :-
	insert_symbol_entry(ST, Pred, st(Pred,_,_,_,_,_,_,_,_,Domain)).

%
%  Find the entry for predicate Pred in the symbole table.
%  If it is in, return the entry as Entry;
%  otherwise, Entry is returned as a variable.
%

find_symbol_entry(ST,_,_) :- 
	var(ST),
	!.
find_symbol_entry(ST,Pred,Entry) :- 
	nonvar(ST),
	(
	    ST = [Entry|_],
	    Entry = st(Pred,_,_,_,_,_,_,_,_,_) ->
	    true
	;
	    ST = [E|S],
	    E = st(Pred1,_,_,_,_,_,_,_,_,_),
	    Pred \== Pred1,
	    find_symbol_entry(S,Pred,Entry)
	).

%
%  Find a field for predicate Pred in symbol table.
%

find_symbol_field_clause(ST,Pred,ClauseList) :-
	find_symbol_field(ST,Pred,clause,ClauseKeyList),
	clause_list(ClauseKeyList, ClauseList).

clause_list(ClauseKeyList, ClauseKeyList) :-
	var(ClauseKeyList),
	!.
clause_list([Clause:_Key|ClauseKeyList], [Clause|ClauseList]) :-
	clause_list(ClauseKeyList, ClauseList).

find_symbol_field(ST, Pred, Type, ClauseList) :-
	find_symbol_field_(Type, ST, Pred, ClauseList).

find_symbol_field_(clause,  ST, Pred, ClauseList) :-
	find_symbol_entry(ST, Pred, st(Pred,ClauseList,_,_,_,_,_,_,_,_)).
find_symbol_field_((mode),  ST, Pred, Mode) :-
	find_symbol_entry(ST, Pred, st(Pred,_,Mode,_,_,_,_,_,_,_)).
find_symbol_field_(measure, ST, Pred, Measure) :-
	find_symbol_entry(ST, Pred, st(Pred,_,_,Measure,_,_,_,_,_,_)).
find_symbol_field_(mutex,   ST, Pred, Mutex) :-
	find_symbol_entry(ST, Pred, st(Pred,_,_,_,Mutex,_,_,_,_,_)).
find_symbol_field_(det,     ST, Pred, Det) :-
	find_symbol_entry(ST, Pred, st(Pred,_,_,_,_,Det,_,_,_,_)).
find_symbol_field_(size,     ST, Pred, Size) :-
	find_symbol_entry(ST, Pred, st(Pred,_,_,_,_,_,Size,_,_,_)).
find_symbol_field_(relation, ST, Pred, Solution) :-
	find_symbol_entry(ST, Pred, st(Pred,_,_,_,_,_,_,Solution,_,_)).
find_symbol_field_(time,     ST, Pred, Time) :-
	find_symbol_entry(ST, Pred, st(Pred,_,_,_,_,_,_,_,Time,_)).
find_symbol_field_(domain,   ST, Pred, Domain) :-
	find_symbol_entry(ST, Pred, st(Pred,_,_,_,_,_,_,_,_,Domain)).

%
%  Get a property of a literal from symbol table.
%

literal_property(BT,ST,Pred,PropName,Property) :-
	find_symbol_field(BT,Pred,PropName,Prop1),
	(
	    var(Prop1) ->
	    find_symbol_field(ST,Pred,PropName,Property)
	;
	    Property = Prop1
	).

get_input_arglist_from_st(ST, Pred, InArgList):- 
    find_symbol_field(ST, Pred, (mode), ModeList),
    create_input_arglist(1, ModeList, InArgList).

create_input_arglist(_, [], []):-
	!.
create_input_arglist(ArgNum, [+|ModeList], [ArgNum|AList]):-
        !,
        NewArgNum is ArgNum + 1,
	create_input_arglist(NewArgNum, ModeList, AList).
create_input_arglist(ArgNum, [_|ModeList], AList):-
	NewArgNum is ArgNum + 1,
	create_input_arglist(NewArgNum, ModeList, AList).
