%
%  symtable.pl			Nai-Wei Lin			November, 1991
%
%  This file contains the procedures for handling the symbol table.
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
	Entry = st(Pred,_,_,_,_,_,_,_,_,_),
	ST = [Entry|_].
insert_symbol_entry(ST,Pred,Entry) :- 
	nonvar(ST),
	ST = [Entry|_],
	Entry = st(Pred,_,_,_,_,_,_,_,_,_).
insert_symbol_entry(ST,Pred,Entry) :- 
	nonvar(ST),
	ST = [E|S],
	E \== st(Pred,_,_,_,_,_,_,_,_,_),
	insert_symbol_entry(S,Pred,Entry).

%
%  Insert a clause for predicate Pred into the symbol table.
%
insert_symbol_field(ST,Pred,clause,Clause) :-
	insert_symbol_entry(ST,Pred,st(Pred,ClauseList,_,_,_,_,_,_,_,_)),
	insert_symbol_clause(ClauseList,Clause).

insert_symbol_clause(ClauseList,Clause) :-
	var(ClauseList),
	ClauseList = [Clause|_].
insert_symbol_clause(ClauseList,Clause) :-
	nonvar(ClauseList),
	ClauseList = [C|_],
	C == Clause.
insert_symbol_clause(ClauseList,Clause) :-
	nonvar(ClauseList),
	ClauseList = [C|CList],
	C \== Clause,
	insert_symbol_clause(CList,Clause).

%
%  Insert a declaration for predicate Pred into the symbol table.
%
insert_symbol_field(ST,Pred,(mode),Mode) :-
	insert_symbol_entry(ST,Pred,st(Pred,_,Mode,_,_,_,_,_,_,_)).
insert_symbol_field(ST,Pred,measure,Measure) :-
	insert_symbol_entry(ST,Pred,st(Pred,_,_,Measure,_,_,_,_,_,_)).
insert_symbol_field(ST,Pred,mutex,Mutex) :-
	insert_symbol_entry(ST,Pred,st(Pred,_,_,_,Mutex,_,_,_,_,_)).
insert_symbol_field(ST,Pred,det,Det) :-
	insert_symbol_entry(ST,Pred,st(Pred,_,_,_,_,Det,_,_,_,_)).
insert_symbol_field(ST,Pred,size,Size) :-
	insert_symbol_entry(ST,Pred,st(Pred,_,_,_,_,_,Size,_,_,_)).
insert_symbol_field(ST,Pred,relation,Solution) :-
	insert_symbol_entry(ST,Pred,st(Pred,_,_,_,_,_,_,Solution,_,_)).
insert_symbol_field(ST,Pred,time,Time) :-
	insert_symbol_entry(ST,Pred,st(Pred,_,_,_,_,_,_,_,Time,_)).
insert_symbol_field(ST,Pred,domain,Domain) :-
	insert_symbol_entry(ST,Pred,st(Pred,_,_,_,_,_,_,_,_,Domain)).

%
%  Find the entry for predicate Pred in the symbole table.
%  If it is in, return the entry as Entry;
%  otherwise, Entry is returned as a variable.
%
find_symbol_entry(ST,_,_) :- 
	var(ST).
find_symbol_entry(ST,Pred,Entry) :- 
	nonvar(ST),
	ST = [Entry|_],
	Entry = st(Pred,_,_,_,_,_,_,_,_,_),!.
find_symbol_entry(ST,Pred,Entry) :- 
	nonvar(ST),
	ST = [E|S],
	E = st(Pred1,_,_,_,_,_,_,_,_,_),
	Pred \== Pred1,
	find_symbol_entry(S,Pred,Entry).

%
%  Find a field for predicate Pred in symbol table.
%
find_symbol_field(ST,Pred,clause,ClauseList) :-
	find_symbol_entry(ST,Pred,st(Pred,ClauseList,_,_,_,_,_,_,_,_)).
find_symbol_field(ST,Pred,(mode),Mode) :-
	find_symbol_entry(ST,Pred,st(Pred,_,Mode,_,_,_,_,_,_,_)).
find_symbol_field(ST,Pred,measure,Measure) :-
	find_symbol_entry(ST,Pred,st(Pred,_,_,Measure,_,_,_,_,_,_)).
find_symbol_field(ST,Pred,mutex,Mutex) :-
	find_symbol_entry(ST,Pred,st(Pred,_,_,_,Mutex,_,_,_,_,_)).
find_symbol_field(ST,Pred,det,Det) :-
	find_symbol_entry(ST,Pred,st(Pred,_,_,_,_,Det,_,_,_,_)).
find_symbol_field(ST,Pred,size,Size) :-
	find_symbol_entry(ST,Pred,st(Pred,_,_,_,_,_,Size,_,_,_)).
find_symbol_field(ST,Pred,relation,Solution) :-
	find_symbol_entry(ST,Pred,st(Pred,_,_,_,_,_,_,Solution,_,_)).
find_symbol_field(ST,Pred,time,Time) :-
	find_symbol_entry(ST,Pred,st(Pred,_,_,_,_,_,_,_,Time,_)).
find_symbol_field(ST,Pred,domain,Domain) :-
	find_symbol_entry(ST,Pred,st(Pred,_,_,_,_,_,_,_,_,Domain)).

%
%  Get a property of a literal from symbol table.
%
literal_property(BT,ST,Pred,PropName,Property) :-
	find_symbol_field(BT,Pred,PropName,Prop1),
	(var(Prop1) ->
		find_symbol_field(ST,Pred,PropName,Property);
		Property = Prop1).

%
%  Print out the symbol table.
%
print_symbol_table(ST) :-
	tell(symbol_table),
	p_symbol_table(ST),
	told.

p_symbol_table(ST) :-
	var(ST).
p_symbol_table(ST) :-
	nonvar(ST),
	ST = [E|S],
	write(E),
	nl,
	p_symbol_table(S).

