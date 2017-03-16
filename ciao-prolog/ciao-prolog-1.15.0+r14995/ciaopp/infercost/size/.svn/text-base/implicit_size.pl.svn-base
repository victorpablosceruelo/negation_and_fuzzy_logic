:- module(implicit_size, [implicit_var_size/6], [assertions]).

%
%  implicit_size.pl		Nai-Wei Lin			December, 1991
%
%  This file contains the procedures for finding the implicit size of
%  a variable at the input position of the head.
%

:- use_module(infercost(top(utility)), [maximum/3]).
:- use_module(infercost(init(builtin)), 
	[second_order_predicate_pred_arg/2]).
:- use_module(infercost(size(ground_size)), 
	[
	    ground_term_size/3,
	    ground_depth/3
	]).
:- use_module(infercost(dependency(adg)), [find_adg_field/4]).
:- use_module(infercost(dependency(position)), [pos_litnum/2, pos_argnum/2]).
:- use_module(infercost(size(clause)), 
	[
	    number_of_literals/3,
	    ith_clause_literal/3
	]).

%
%  Compute the implicit size of a variable.
%
implicit_var_size(void,_,_,_,_,0) :- !.
implicit_var_size(_,Term,_,_,_,bot) :-
	nonvar(Term),
	!.
implicit_var_size(Measure,Term,Pos,Adg,Clause,Size) :- % Added Nov 9, 05 -PLG
	var(Term),
        implicit_var_size([Pos],Measure,Clause,Term,Size1),
        (
	    Size1 == bot ->
	    find_adg_field(Adg,Pos,succ,Succ),
	    implicit_var_size(Succ,Measure,Clause,Term,Size)
	;
	    Size = Size1
	).
 %% implicit_var_size(Measure,Term,Pos,Adg,Clause,Size) :-
 %% 	var(Term),
 %% 	find_adg_field(Adg,Pos,succ,Succ),
 %% 	implicit_var_size(Succ,Measure,Clause,Term,Size).


%
%  Compute the implicit sizes of the successors of a variable.
%

:- push_prolog_flag(multi_arity_warnings, off).

implicit_var_size(      PList, _Measure, _Clause, _Term, bot) :-
	var(PList),
	!.
implicit_var_size([]         , _Measure, _Clause, _Term, bot) :-
	!.
implicit_var_size([Pos|PList],  Measure,  Clause,  Term, Size) :-
	pos_litnum(Pos, 0),
	!,
	implicit_var_size(PList, Measure, Clause, Term, Size).
implicit_var_size([Pos|PList], Measure, Clause, Term, Size) :-
	pos_litnum(Pos, LitNum),
	%write(LitNum),nl,
	LitNum > 0,
	arg(2,Clause,Body),
	number_of_literals(Body,1,Num),
	%write(Num),nl,
	(LitNum > Num ->
		NLitNum is LitNum-Num;
		NLitNum = LitNum),
	ith_clause_literal(NLitNum,Clause,Lit),
	(LitNum > Num ->
		second_order_predicate_pred_arg(Lit,NLit);
		NLit = Lit),
	functor(NLit,NF,NA),
	pos_argnum(Pos,ArgNum),
	arg(ArgNum,NLit,Arg),
	%write(Arg),nl,
	(Arg == Term ->
		implicit_var_size(Measure,NF/NA,Pos,NLit,Size1);
		Size1 = bot),
	implicit_var_size(PList,Measure,Clause,Term,Size2),
	maximum(Size1,Size2,Size).

%
%  Compute the implicit sizes of a position under a particular measure.
%

% This predicate needs to be tested

implicit_var_size(int,Name,Pos,Lit,Size) :-
	implicit_int(Name,Pos,Lit,Size).
implicit_var_size(length,Name,Pos,Lit,Size) :-
	implicit_length(Name,Pos,Lit,Size).
implicit_var_size(size,Name,Pos,Lit,Size) :-
	implicit_size(Name,Pos,Lit,Size).
implicit_var_size(depth(Child),Name,Pos,Lit,Size) :-
	implicit_depth(Child,Name,Pos,Lit,Size).

:- pop_prolog_flag(multi_arity_warnings).

%
%  Compute the implicit sizes of a position under measure int.
%
implicit_int((=:=)/2,Pos,Lit,Size) :-
	pos_argnum(Pos,1),
	arg(2,Lit,Arg),
	ground_term_size(int,Arg,Size).
implicit_int((=:=)/2,Pos,Lit,Size) :-
	pos_argnum(Pos,2),
	arg(1,Lit,Arg),
	ground_term_size(int,Arg,Size).
implicit_int((==)/2,Pos,Lit,Size) :-
	pos_argnum(Pos,1),
	arg(2,Lit,Arg),
	ground_term_size(int,Arg,Size).
implicit_int((==)/2,Pos,Lit,Size) :-
	pos_argnum(Pos,2),
	arg(1,Lit,Arg),
	ground_term_size(int,Arg,Size).
implicit_int((=)/2,Pos,Lit,Size) :-
	pos_argnum(Pos,1),
	arg(2,Lit,Arg),
	ground_term_size(int,Arg,Size).
implicit_int((=)/2,Pos,Lit,Size) :-
	pos_argnum(Pos,2),
	arg(1,Lit,Arg),
	ground_term_size(int,Arg,Size).
implicit_int(Name,_,_,bot) :-
	Name \== (=:=)/2,
	Name \== (==)/2,
	Name \== (=)/2.

%
%  Compute the implicit sizes of a position under measure length.
%

% This predicate needs to be tested

implicit_length((==)/2,Pos,Lit,Size) :-
	pos_argnum(Pos,1),
	arg(2,Lit,Arg),
	ground_term_size(length,Arg,Size).
implicit_length((==)/2,Pos,Lit,Size) :-
	pos_argnum(Pos,2),
	arg(1,Lit,Arg),
	ground_term_size(length,Arg,Size).
implicit_length((=)/2,Pos,Lit,Size) :-
	pos_argnum(Pos,1),
	arg(2,Lit,Arg),
	ground_term_size(length,Arg,Size).
implicit_length((=)/2,Pos,Lit,Size) :-
	pos_argnum(Pos,2),
	arg(1,Lit,Arg),
	ground_term_size(length,Arg,Size).
implicit_length(Name,_,_,bot) :-
	Name \== (==)/2,
	Name \== (=)/2.

%
%  Compute the implicit sizes of a position under measure size.
%
implicit_size(atom/1,_,_,1).
implicit_size(atomic/1,_,_,1).
implicit_size(number/1,_,_,1).
implicit_size(integer/1,_,_,1).
implicit_size(float/1,_,_,1).
implicit_size((==)/2,Pos,Lit,Size) :-
	pos_argnum(Pos,1),
	arg(2,Lit,Arg),
	ground_term_size(size,Arg,Size).
implicit_size((==)/2,Pos,Lit,Size) :-
	pos_argnum(Pos,2),
	arg(1,Lit,Arg),
	ground_term_size(size,Arg,Size).
implicit_size((=)/2,Pos,Lit,Size) :-
	pos_argnum(Pos,1),
	arg(2,Lit,Arg),
	ground_term_size(size,Arg,Size).
implicit_size((=)/2,Pos,Lit,Size) :-
	pos_argnum(Pos,2),
	arg(1,Lit,Arg),
	ground_term_size(size,Arg,Size).
implicit_size(Name,_,_,bot) :-
	Name \== atom/1,
	Name \== atomic/1,
	Name \== number/1,
	Name \== integer/1,
	Name \== float/1,
	Name \== (==)/2,
	Name \== (=)/2.

%
%  Compute the implicit sizes of a position under measure depth.
%
implicit_depth(_,atom/1,_,_,0).
implicit_depth(_,atomic/1,_,_,0).
implicit_depth(_,number/1,_,_,0).
implicit_depth(_,integer/1,_,_,0).
implicit_depth(_,float/1,_,_,0).
implicit_depth(Child,(==)/2,Pos,Lit,Size) :-
	pos_argnum(Pos,1),
	arg(2,Lit,Arg),
	ground_depth(depth(Child),Arg,Size).
implicit_depth(Child,(==)/2,Pos,Lit,Size) :-
	pos_argnum(Pos,2),
	arg(1,Lit,Arg),
	ground_depth(depth(Child),Arg,Size).
implicit_depth(Child,(=)/2,Pos,Lit,Size) :-
	pos_argnum(Pos,1),
	arg(2,Lit,Arg),
	ground_depth(depth(Child),Arg,Size).
implicit_depth(Child,(=)/2,Pos,Lit,Size) :-
	pos_argnum(Pos,2),
	arg(1,Lit,Arg),
	ground_depth(depth(Child),Arg,Size).
implicit_depth(_,Name,_,_,bot) :-
	Name \== atom/1,
	Name \== atomic/1,
	Name \== number/1,
	Name \== integer/1,
	Name \== float/1,
	Name \== (==)/2,
	Name \== (=)/2.
