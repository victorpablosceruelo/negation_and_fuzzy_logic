:- module(ground_size,
	[
	    ground_size/2,
	    ground_term_size/3,
	    ground_depth/3,
	    
	    ground_depth_/4,
	    ground_size_/3,
	    ground_int/2,
	    ground_length/2

	], [assertions, isomodes]).
%
%  ground_size.pl		Nai-Wei Lin			December, 1991
%
%  This file contains the procedures for determining the size of a ground
%  term.
%

% Modified by Edison Mera  2006-07-06.

:- use_module(infercost(top(utility)), 
	[
	    noninteger/1,
	    nonlist/1,
	    add/3,
	    compound/1,
	    add/3,
	    member/2,
	    maximum/3
	]).

:- test ground_term_size(A,B,C):(A=void,B=patatin)=>C=0.
:- test ground_term_size(A,B,C):(A=int,B=5)=>C=5.
:- test ground_term_size(A,B,C):(A=length,B=5)=>C=bot.
:- test ground_term_size(A,B,C):(A=length,B=[_,5,6,7,a|_])=>C=bot.
:- test ground_term_size(A,B,C):(A=length,B=[_,5,6,7,a])=>C=5.
:- test ground_term_size(A,B,C):(A=size,B= -5)=>C=1.
:- test ground_term_size(A,B,C):(A=size,B=f(a,b,c(-5,3,4),b))=>C=8.
:- test ground_term_size(A,B,C):(A=size,B=f(a,b,c(-5,3,4),b(_)))=>C=bot.
:- test ground_term_size(A,B,C):(A=depth([1,2]),B=f(a(b(c)),c(-5,3,4),b(_)))=>C=3.


:- pred ground_term_size(+atm, +term, -term) # "Determine the size of a ground term".

ground_term_size(void, _, 0).
ground_term_size(int, Term, Size) :-
	ground_int(Term, Size).
ground_term_size(length, Term, Size) :-
	ground_length(Term, Size).
ground_term_size(size, Term, Size) :-
	ground_size(Term, Size).
ground_term_size(depth(ChildList), Term, Size) :-
	ground_depth(ChildList, Term, Size).


:- pred ground_int(+term, -term) # "Determine the size of a ground
   term under measure int.".

:- test ground_int(A,B):(A=5)=>B=5.
:- test ground_int(A,B):(A=toy)=>B=bot.

ground_int(Int, bot) :-
	noninteger(Int),
	!.
ground_int(Int, Int) :-
	integer(Int).


:- pred ground_length(+term, -term) # "Determine the size of a ground
   term under measure length.".

:- test ground_length(A,B):(A=[bot|toy])=>B=bot.
:- test ground_length(A,B):(A=[1,2,3])=>B=3.
:- test ground_length(A,B):(A=[1,2,3,_])=>B=4.

ground_length(Term, bot) :-
	nonlist(Term),
	!.
ground_length([], 0) :-
	!.
ground_length([_|T], Size) :-
	ground_term_size(length, T, Size1),
	add(Size1, 1, Size).


:- pred ground_size(+term, -term) # "Determine the size of a ground
   term under measure size.".

:- test ground_size(A,B):(A=p(a,b,c(d,e)))=>B=6.
:- test ground_size(A,B):(A=p(a,b,c(d,_)))=>B=bot.
:- test ground_size(A,B):(A=[1,2,3])=>B=7.

ground_size(Term, bot) :-
	var(Term),
	!.
ground_size(Term, 1) :-
	atomic(Term),
	!.
ground_size(Term, Size) :-
	compound(Term),
	functor(Term, _, N),
	ground_size_(N, Term, Size1),
	add(Size1, 1, Size).

:- test ground_size_(A,B,C):(A=3,B=p(a,b,c(d)))=>C=4.
:- test ground_size_(A,B,C):(A=3,B=p(a,b,c(d,e,f)))=>C=6.
:- test ground_size_(A,B,C):(A=3,B=p(a,D,c(d,e,f)))=>C=bot.

ground_size_(0, _, 0) :-
	!.
ground_size_(N, Term, Size) :-
	N > 0,
	arg(N, Term, Arg),
	ground_size(Arg, Size1),
	N1 is N-1,
	ground_size_(N1, Term, Size2),
	add(Size1, Size2, Size).

:- pred ground_depth(+list, +term, -term) # "Determine the size of a
   ground term under measure depth.".

:- test ground_depth(A,B,C):(A=[1,2],B=f(a(b),c(d(e)),f(g,h)))=>C=3.
:- test ground_depth(A,B,C):(A=[1,2],B=f(a,_,c(d(e)),f(_,h)))=>C=1.
:- test ground_depth(A,B,C):(A=[1,2],B=f(_,_,c(d(e)),f(_,h)))=>C=bot.
:- test ground_depth(A,B,C):(A=[1,2],B=f(_,a(b),c(d(e)),f(_,h)))=>C=2.

ground_depth(_, Term, bot) :-
	var(Term),
	!.
ground_depth(_, Term, 0) :-
	atomic(Term),
	!.
ground_depth(ChildList, Term, Size) :-
	compound(Term),
	functor(Term, _, N),
	ground_depth_(ChildList, N, Term, Size1),
	add(Size1, 1, Size).

:- test ground_depth_(A,B,C,D):(A=[1,2],B=4,C=f(a,E,c(d(e)),f(F,h)))=>D= 0.
:- test ground_depth_(A,B,C,D):(A=[1,2],B=4,C=f(E,a(b),c(d(e)),f(F,h)))=>D=1.

ground_depth_(_, 0, _, bot) :-
	!.
ground_depth_(ChildList, N, Term, Size) :-
	N > 0,
	(
	    utility:member(ChildList, N) ->
	    (
		arg(N, Term, Arg),
		ground_depth(ChildList, Arg, Size1)
	    )
	;
	    Size1 = bot
	),
	N1 is N - 1,
	ground_depth_(ChildList, N1, Term, Size2),
	maximum(Size1, Size2, Size).
