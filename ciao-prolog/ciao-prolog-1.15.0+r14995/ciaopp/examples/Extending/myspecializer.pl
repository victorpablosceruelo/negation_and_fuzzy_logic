
:- module(myspecializer,[],[assertions]).

:- use_module(program(clidlist), 
	[ clause_key/2, rewrite_source_clause/4,
	  inverse_rewrite_source_program/2 % instrumental
	]).
:- use_module(program(p_unit), [replace_program/2]). 
:- use_module(ciaopp(tr_syntax), [traverse_clauses/5]).
:- use_module(library(vndict), [create_dict/2]).
% instrumental
:- use_module(library(hiordlib), [map/3]).

% ------------------------------------------------------------------------
:- doc(module,"This module implements a naive specializer using the
	CiaoPP interface.").
:- doc(author,"Francisco Bueno").

:- doc(bug,"Currently, the specialization algorithm is a bit too 
	naive!").

% ------------------------------------------------------------------------
:- doc(transformation/1,"Declares that this transformation exists.").
:- multifile transformation/1.

transformation(myspec).

:- push_prolog_flag(multi_arity_warnings,off).
:- doc(transformation/4,"Hook for the user entry point.").
:- multifile transformation/4.

transformation(myspec,Cls0,Ds0,_Info):-
	% transform program to my own format
	ciaopp_to_mine(Cls0,Cls1),
	% my program specialization
	specialize(Cls1,Ds0,Cls2,Ds2),
	% transform back to standard ciaopp format
	mine_to_ciaopp(Cls2,Ds2,Cls3,Ds3),
	% return program back to ciaopp DB
	replace_program(Cls3,Ds3).

:- pop_prolog_flag(multi_arity_warnings).

:- doc(specialize/4,"Specializes the program in my own format.").

specialize(Cls0,_Ds0,Cls1,Ds1):-
	specialization_algorithm(Cls0,Cls1),
	% create dictionaries for ciaopp (since I throw them away)
	map(Cls1,create_dict,Ds1).

:- doc(specialization_algorithm/2,"Specializes the program using my 
	own very nice specialization algorithm.").

specialization_algorithm(Cls,Cls). % rather naive...

% ------------------------------------------------------------------------

ciaopp_to_mine(Cls0,Cls2):-
	% take all keys out
	inverse_rewrite_source_program(Cls0,Cls1),
	% ciaopp representation to mine
	map(Cls1,ciaopp2mine,Cls2).

ciaopp2mine(clause(H,B),(H:-B)).

mine_to_ciaopp(Cls0,Ds0,Cls4,Ds3):-
	% from my representation to ciaopp's
	map(Cls0,mine2ciaopp,Cls1),
	% remove any disjunctions I might have introduced
	map(Cls1,add_key,Cls2), % required by traverse_clauses/5
	traverse_clauses(Cls2,Ds0,all,Cls3,Ds3),
	% add keys for literals
	map(Cls3,transform_back,Cls4).

mine2ciaopp(X,Y):- ciaopp2mine(Y,X).

add_key(clause(H,B),clause(H,B):Key):-
	clause_key(H,Key).

transform_back(clause(H,B):Key,Clause:Key):-
	rewrite_source_clause(H,B,Key,Clause).

% ------------------------------------------------------------------------
% That's it!
