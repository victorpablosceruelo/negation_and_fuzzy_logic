:- module(names_table,
	[
	    find_name_entry/3,  
	    insert_name_field/4 
	],
	[assertions]).    

:- doc(author,"Pedro L@'{o}pez").  

:- doc(module, "Procedures for handling the names table.").  

%
%  The structure of the names table is:
%	st(Pred/Arity,seq_name,gran_name,arg_map,actual_sizes,test).
%
%  where: 
%
%  Pred/Arity: is the key (predicate name and arity).
%  seq_name: name of the sequential version of the predicate.
%  gran_name: name of the version that performs granularity control.
%  arg_map: list containig the numbers of the input arguments of the head.
%  actual_sizes: contains the expresions that compute the size of input 
%                arguments in the body. Is a list of lists. There is a list 
%                for each clause. Each of these lists contains items of the 
%                form iasize(Lit/Arg,FSize), where FSize is the size function 
%                corresponding to the argument number arg of the literal Lit.
%
%  test: structure of the form c_func(FunName,AV), where FunName is the 
%  name of the function that compute the cost of the predicate, and AV
%  is a list with the numbers of the input argiments of the head of which 
%  this function depends on.
%

%
%  Insert an entry for predicate Pred in the names table.
%  If it is already in, return the entry as Entry;
%  otherwise, an entry Entry for Pred is inserted and returned.
%

insert_name_entry(ST,Pred,Entry) :- 
	var(ST),
	Entry = st(Pred,_,_,_,_,_),
	ST = [Entry|_].
insert_name_entry(ST,Pred,Entry) :- 
	nonvar(ST),
	ST = [Entry|_],
	Entry = st(Pred,_,_,_,_,_).
insert_name_entry(ST,Pred,Entry) :- 
	nonvar(ST),
	ST = [E|S],
	E \== st(Pred,_,_,_,_,_),
	insert_name_entry(S,Pred,Entry).


%  Insert the type for predicate Pred into the names table.
%


insert_name_field(ST,Pred,seq_name,Name) :-
	insert_name_entry(ST,Pred,st(Pred, Name,_,_,_,_)).
insert_name_field(ST,Pred,gran_name,Name) :-
	insert_name_entry(ST,Pred,st(Pred,_,Name,_,_,_)).
insert_name_field(ST,Pred,arg_map,ArgList) :-
	insert_name_entry(ST,Pred,st(Pred,_,_,ArgList,_,_)).
insert_name_field(ST,Pred,actual_sizes,Sizes) :-
	insert_name_entry(ST,Pred,st(Pred,_,_,_,Sizes,_)).
insert_name_field(ST,Pred,test,TimeFunc) :-
	insert_name_entry(ST,Pred,st(Pred,_,_,_,_,TimeFunc)).


%
%  Find the entry for predicate Pred in the names table.
%  If it is in, return the entry as Entry;
%  otherwise, Entry is returned as a variable.
%

find_name_entry(ST,_,_) :- 
	var(ST).
find_name_entry(ST,Pred,Entry) :- 
	nonvar(ST),
	ST = [Entry|_],
	Entry = st(Pred,_,_,_,_,_),!.
find_name_entry(ST,Pred,Entry) :- 
	nonvar(ST),
	ST = [E|S],
	E = st(Pred1,_,_,_,_,_),
	Pred \== Pred1,
	find_name_entry(S,Pred,Entry).

 
 %% %
 %% %  Find a field for predicate Pred in names table.
 %% %
 %% 
 %% find_name_field(ST,Pred,seq_name,Name) :-
 %% 	find_name_entry(ST,Pred,st(Pred,Name,_,_)).
 %% find_name_field(ST,Pred,gran_name,Name) :-
 %% 	find_name_entry(ST,Pred,st(Pred,_,Name,_)).
 %% find_name_field(ST,Pred,arg_map,ArgList) :-
 %% 	find_name_entry(ST,Pred,st(Pred,_,_,ArgList)).
 %% find_name_field(ST,Pred,actual_sizes,Sizes) :-
 %% 	find_name_entry(ST,Pred,st(Pred,_,_,_,Sizes,_)).
 %% find_name_field(ST,Pred,test,Test) :-
 %% 	find_name_entry(ST,Pred,st(Pred,_,_,_,_,Test)).








