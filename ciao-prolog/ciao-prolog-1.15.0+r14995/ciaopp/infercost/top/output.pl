:- module(output,
	[
	    print_size/2,
	    print_relation/2,
	    print_solution/2,
	    print_time/2,
	    output_comp_dec/2
	], [assertions]).

:- use_module(library(write)).

:- use_module(infercost(init(symtable)), [find_symbol_field/4]).
:- use_module(infercost(top(utility)), [compound/1]).

%
%  Print out the argument size functions.
%
print_size([],_).
print_size([Pred|CList],ST) :-
	nl,
	write('* Size functions for predicate '),
	write(Pred), 
	write(' :'),
	nl, nl, 
	find_symbol_field(ST,Pred,size,Size),
	output_comp_dec(Size,NSize),
	write(NSize), nl,
	print_size(CList,ST).

%
%  Print out the relation size functions.
%
print_relation([],_).
print_relation([Pred|CList],ST) :-
	nl,
	write('* Relation functions for predicate '),
	write(Pred), 
	write(' :'),
	nl, nl, 
	find_symbol_field(ST,Pred,relation,Relation),
	output_comp_dec(Relation,NRelation),
	write([NRelation]), nl,
	print_relation(CList,ST).

%
%  Print out the solution size functions.
%
print_solution([],_).
print_solution([Pred|CList],ST) :-
	nl,
	write('* Solution functions for predicate '),
	write(Pred), 
	write(' :'),
	nl, nl, 
	find_symbol_field(ST,Pred,det,Solution),
	output_comp_dec(Solution,NSolution),
	write(NSolution), nl,
	print_solution(CList,ST).

%
%  Print out the time functions.
%
print_time([],_).
print_time([Pred|CList],ST) :-
	nl,
	write('* Time functions for predicate '),
	write(Pred), 
	write(' :'),
	nl, nl, 
	find_symbol_field(ST,Pred,time,Time),
	output_comp_dec(Time,NTime),
	write(NTime), nl,
	print_time(CList,ST).

%
%  Format the output complexity functions.
%
output_comp_dec(Exp,Exp) :-
	atomic(Exp).
output_comp_dec(Exp,NExp) :-
	compound(Exp),
	functor(Exp,F,N),
	(F/N == '$'/2 ->
		(arg(1,Exp,Arg1),
		 arg(2,Exp,Arg2),
		 ((Arg1 =:= 0, integer(Arg2)) ->
			NExp =.. ['$',Arg2];
			NExp = Exp));
		(functor(NExp,F,N),
		 output_comp_dec(N,Exp,NExp))).

:- push_prolog_flag(multi_arity_warnings,off).

output_comp_dec(0,_,_).
output_comp_dec(N,Exp,NExp) :-
	N > 0,
	arg(N,Exp,Arg),
	output_comp_dec(Arg,NArg),
	arg(N,NExp,NArg),
	N1 is N-1,
	output_comp_dec(N1,Exp,NExp).

:- pop_prolog_flag(multi_arity_warnings).
