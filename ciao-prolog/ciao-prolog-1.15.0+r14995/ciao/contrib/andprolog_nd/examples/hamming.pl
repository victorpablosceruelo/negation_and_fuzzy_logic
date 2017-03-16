:- module(hamming,
	[
	    seq/2,
	    par/2,
	    par_nondet/2,
	    data/1
	],
	[andprolog_nd]).

:- use_module(library(lists), [append/3, length/2]).
:- use_module(library(sort)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

size(1000).

data(X) :- size(X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

seq(N,true) :- ham_seq(N).
par(N,true) :- ham_par(N).
par_nondet(N,true) :- ham_par_nondet(N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ham_seq(Number) :-
        emptyq(Twos_),
        emptyq(Threes_),
        emptyq(Fives_),
        enqueue(1,Twos_,Twos,S1),
        enqueue(1,Threes_,Threes,S2),
        enqueue(1,Fives_,Fives,S3),
        hamming_seq(Number,Twos,Threes,Fives,S1,S2,S3), ! .

hamming_seq(0,_1,_2,_3,_,_,_).
hamming_seq(N,Twos,Threes,Fives,_S2s,_S3s,_S5s) :-
        N>0,
	firstq(Twos,FTwo),
	firstq(Threes,FThree),
	firstq(Fives,FFive),
	select_smaller(FTwo,FThree,FFive,Smaller),
	try_dequeue(Twos,Smaller,NewTwos),
	try_dequeue(Threes,Smaller,NewThrees),
	try_dequeue(Fives,Smaller,NewFives),
	New2 is Smaller*2,
	New3 is Smaller*3,
	New5 is Smaller*5,
	enqueue(New2,NewTwos,OtherTwos,S2snew),
	enqueue(New3,NewThrees,OtherThrees,S3snew),
	enqueue(New5,NewFives,OtherFives,S5snew),
	N1 is N-1,
        hamming_seq(N1,OtherTwos,OtherThrees,OtherFives,S2snew,S3snew,S5snew).

ham_par(Number) :-
        emptyq(Twos_),
        emptyq(Threes_),
        emptyq(Fives_),
        enqueue(1,Twos_,Twos,S1),
        enqueue(1,Threes_,Threes,S2),
        enqueue(1,Fives_,Fives,S3),
        hamming_par(Number,Twos,Threes,Fives,S1,S2,S3), ! .

hamming_par(0,_1,_2,_3,_,_,_) :- !.
hamming_par(N,Twos,Threes,Fives,S2s,_S3s,_S5s) :-
	(
	    (S2s > 20) ->
	     firstq(Twos,FTwo) '&!'
	     firstq(Threes,FThree) '&!'
	     firstq(Fives,FFive),
	     select_smaller(FTwo,FThree,FFive,Smaller),
	     try_dequeue(Twos,Smaller,NewTwos) '&!>' H6,
	     try_dequeue(Threes,Smaller,NewThrees) '&!>' H7,
	     try_dequeue(Fives,Smaller,NewFives) '&!>' H8,
	     New2 is Smaller*2,
	     New3 is Smaller*3,
	     New5 is Smaller*5,
	     H6 '<&!',
	     enqueue(New2,NewTwos,OtherTwos,S2snew) '&!>' H12,
	     H7 '<&!',
	     enqueue(New3,NewThrees,OtherThrees,S3snew) '&!>' H13,
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
        hamming_par(N1,OtherTwos,OtherThrees,OtherFives,S2snew,S3snew,S5snew).

ham_par_nondet(Number) :-
        emptyq(Twos_),
        emptyq(Threes_),
        emptyq(Fives_),
        enqueue(1,Twos_,Twos,S1),
        enqueue(1,Threes_,Threes,S2),
        enqueue(1,Fives_,Fives,S3),
        hamming_par_nondet(Number,Twos,Threes,Fives,S1,S2,S3).

hamming_par_nondet(0,_1,_2,_3,_,_,_) :- !.
hamming_par_nondet(N,Twos,Threes,Fives,S2s,_S3s,_S5s) :-
	(
	    (S2s > 20) ->
	     firstq(Twos,FTwo) &
	     firstq(Threes,FThree) &
	     firstq(Fives,FFive),
	     select_smaller(FTwo,FThree,FFive,Smaller),
	     try_dequeue(Twos,Smaller,NewTwos) &> H6,
	     try_dequeue(Threes,Smaller,NewThrees) &> H7,
	     try_dequeue(Fives,Smaller,NewFives) &> H8,
	     New2 is Smaller*2,
	     New3 is Smaller*3,
	     New5 is Smaller*5,
	     H6 <& ,
	     enqueue(New2,NewTwos,OtherTwos,S2snew) &> H12,
	     H7 <& ,
	     enqueue(New3,NewThrees,OtherThrees,S3snew) &> H13,
	     H8 <& ,
	     enqueue(New5,NewFives,OtherFives,S5snew),
	     N1 is N-1,
	     H12 <& ,
	     H13 <&
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
        hamming_par_nondet(N1,OtherTwos,OtherThrees,OtherFives,S2snew,S3snew,S5snew).

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


