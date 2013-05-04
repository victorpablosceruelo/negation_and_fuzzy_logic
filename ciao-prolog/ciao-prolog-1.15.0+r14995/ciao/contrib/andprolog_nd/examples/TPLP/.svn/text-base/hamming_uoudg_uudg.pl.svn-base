:- module( _, [main/0, ham/1 , main/1], [dcg , assertions , nativeprops , regtypes , andprolog_nd] ).


:- use_module(library(operators),[op/3]).

 %% :- load_compilation_module(library(runtime_ops_tr)).
 %% 
 %% :- add_sentence_trans(runtime_op/2).
 %% 
 %% :- include(library(runtime_ops)).
 %% 
 %% :- include(library(dcg)).

:- use_module(library(default_predicates)).

:- use_module(library(queues)).

:- use_module(library(lists)).

:- use_module(library(write)).
:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(system)).
:- use_module(library(format), [format/2]).
:- use_module(library(between)).
:- use_module(library(dynamic)).
:- use_module(library(aggregates), [findall/3]).
:- use_module('./common_bench').

:- push_prolog_flag(multi_arity_warnings, off).

:- dynamic time_exec/1.

main :-
% 	set_prolog_flag(gc, off),
% 	between(1,8,_),
%         statistics(walltime, [T1,_]),
 	ham(500).
%         statistics(walltime, [T2,_]),
%         Delta is T2 - T1,
% 	format("-- main=~f ms.~n", [Delta]),
% 	asserta_fact(time_exec(Delta)),
% 	fail.
% main :-
% 	findall(Time,(time_exec(Time),retract_fact(time_exec(Time))),L),
% 	delete_min_max(L,L2),
% 	average_list(L2,Average),
% 	format("     Time Average     = ~f ms.~n~n", [Average]).

main([Atom]) :-
        atom_codes(Atom,Codes),
        number_codes(Number,Codes),
        ham(Number) .
main(_1) :-
        display('Usage: hamming <number of Hamming numbers>'),
        nl .

ham(Number) :-
        emptyq(Twos_),
        emptyq(Threes_),
        emptyq(Fives_),
        enqueue(1,Twos_,Twos,S1),
        enqueue(1,Threes_,Threes,S2),
        enqueue(1,Fives_,Fives,S3),
        hamming(Number,Twos,Threes,Fives,S1,S2,S3), ! .

hamming(0,_1,_2,_3,_,_,_).
hamming(N,Twos,Threes,Fives,S2s,_S3s,_S5s) :-
        N>0,
	(
	    (S2s > 20) ->
	     firstq(Twos,FTwo) '&!'
	     firstq(Threes,FThree) '&!'
	     firstq(Fives,FFive),
	     select_smaller(FTwo,FThree,FFive,Smaller),
	     try_dequeue(Twos,Smaller,NewTwos)'&!>' H6,
	     try_dequeue(Threes,Smaller,NewThrees)'&!>' H7,
	     try_dequeue(Fives,Smaller,NewFives)'&!>' H8,
	     New2 is Smaller*2,
	     New3 is Smaller*3,
	     New5 is Smaller*5,
	     H6 '<&!',
	     enqueue(New2,NewTwos,OtherTwos,S2snew)'&!>' H12,
	     H7 '<&!',
	     enqueue(New3,NewThrees,OtherThrees,S3snew)'&!>' H13,
	     H8 '<&!',
	     enqueue(New5,NewFives,OtherFives,S5snew),
	     N1 is N-1,
	     H12 '<&!',
	     H13 '<&!'
	;
	     firstq(Twos,FTwo),
	     firstq(Threes,FThree),
	     firstq(Fives,FFive),
	     select_smaller(FTwo,FThree,FFive,Smaller),
	     try_dequeue(Threes,Smaller,NewThrees),
	     try_dequeue(Fives,Smaller,NewFives),
	     try_dequeue(Twos,Smaller,NewTwos),
	     New2 is Smaller*2,
	     New3 is Smaller*3,
	     New5 is Smaller*5,
	     enqueue(New2,NewTwos,OtherTwos,S2snew),
	     enqueue(New3,NewThrees,OtherThrees,S3snew),
	     enqueue(New5,NewFives,OtherFives,S5snew),
	     N1 is N-1
	),
        hamming(N1,OtherTwos,OtherThrees,OtherFives,S2snew,S3snew,S5snew) .

emptyq([]).

firstq(Q,X) :-
        append(_1,[X],Q) .

enqueue(X,Q,[X|Q],S) :-
	length([X|Q],S).

dequeue(Q,W,NQ) :-
        append(NQ,[W],Q) .

try_dequeue(Q,What,NewQ) :-
        firstq(Q,What),
        !,
        dequeue(Q,What,NewQ) .
try_dequeue(Q,_What,NewQ) :-
        Q=NewQ .

select_smaller(A,B,C,Min) :-
        min(A,B,Ma),
        min(Ma,C,Min) .

min(A,B,C) :-
        A<B,
        !,
        C=A .
min(_A,B,C) :-
        C=B .


