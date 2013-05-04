:- module(pal,
	[
	    seq/2,
	    par/2,
	    par_nondet/2,
	    data/1
	],
	[andprolog_nd]).

:- use_module(library(lists), [append/3, length/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

size(15).
gc(7).

data(X) :- size(N), gen_list(N,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

seq(L,X) :- palindrome_seq(L,X).
par(L,X) :- gc(GC), palindrome_par_gc(L,X,GC).
par_nondet(L,X) :- gc(GC), palindrome_par_nondet_gc(L,X,GC).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Versions without granularity control
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

palindrome_seq([],[]).
palindrome_seq([First|L1],L2) :-
        palindrome_seq(L1,Ls2),
        palindrome_seq(L1,Lg2),
        append(Ls2,[First|Lg2],L2).

palindrome_par([],[]).
palindrome_par([First|L1],L2) :-
        palindrome_par(L1,Ls2) '&!'
        palindrome_par(L1,Lg2),
        append(Ls2,[First|Lg2],L2).

palindrome_par_nondet([],[]).
palindrome_par_nondet([First|L1],L2) :-
        palindrome_par_nondet(L1,Ls2) &
        palindrome_par_nondet(L1,Lg2),
        append(Ls2,[First|Lg2],L2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Versions with granularity control
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

palindrome_par_gc(X,Y,T) :-
	length(X,L),
	palindrome_par_gc_(X,Y,T,L).
palindrome_par_gc_([],[],_,_).
palindrome_par_gc_([First|L1],L2,T,L) :-
        (
	    L>T  ->
	    L_1 is L-1,
	    L_2 is L-1,
	    palindrome_par_gc_(L1,Ls2,T,L_1) '&!'
	    palindrome_par_gc_(L1,Lg2,T,L_2)
        ;
	    palindrome_seq(L1,Ls2),
	    palindrome_seq(L1,Lg2)
        ),
	append(Ls2,[First|Lg2],L2).

palindrome_par_nondet_gc(X,Y,T) :-
	length(X,L),
	palindrome_par_nondet_gc_(X,Y,T,L).
palindrome_par_nondet_gc_([],[],_,_).
palindrome_par_nondet_gc_([First|L1],L2,T,L) :-
        (
	    L>T  ->
	    L_1 is L-1,
	    L_2 is L-1,
	    palindrome_par_nondet_gc_(L1,Ls2,T,L_1) &
	    palindrome_par_nondet_gc_(L1,Lg2,T,L_2)
        ;
	    palindrome_seq(L1,Ls2),
	    palindrome_seq(L1,Lg2)
        ),
	append(Ls2,[First|Lg2],L2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_list(0, []).
gen_list(M, [V|Ns]):- 
        M > 0,
        M1 is M - 1,
        V is M*M*M mod 10,
        gen_list(M1, Ns).
