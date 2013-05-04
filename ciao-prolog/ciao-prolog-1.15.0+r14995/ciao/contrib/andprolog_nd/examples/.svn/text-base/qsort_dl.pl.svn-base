:- module(qsort_dl,
	[
	    seq/2,
	    par/2,
	    par_nondet/2,
	    data/1
	], [andprolog_nd]).

:- use_module(library(lists), [append/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

size(10000).
gc(300).

data(X) :- size(N), gen_list(N,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

seq(L,X) :- qsortdl_seq(L,X).
par(L,X) :- gc(GC), size(N), qsortdl_par_gc(L,N,GC,X).
par_nondet(L,X) :- gc(GC), size(N), qsortdl_par_nondet_gc(L,N,GC,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

qsortdl_seq(As,Bs):- 
	qsortdl_seq_(As,Bs,[]).

qsortdl_seq_([X|L],R,R2) :-
	partition(L,X,L1,L2),
        qsortdl_seq_(L2,R1,R2),
	qsortdl_seq_(L1,R,[X|R1]).
qsortdl_seq_([],R,R).

qsortdl_par(As,Bs):- 
	qsortdl_par_(As,Bs,[]).

qsortdl_par_([X|L],R,R2) :-
	partition(L,X,L1,L2),
        qsortdl_par_(L2,R1,R2) '&!'
	qsortdl_par_(L1,R,[X|R1]).
qsortdl_par_([],R,R).

qsortdl_par_nondet(As,Bs):- 
	qsortdl_par_nondet_(As,Bs,[]).

qsortdl_par_nondet_([X|L],R,R2) :-
	partition(L,X,L1,L2),
        qsortdl_par_nondet_(L2,R1,R2) &
	qsortdl_par_nondet_(L1,R,[X|R1]).
qsortdl_par_nondet_([],R,R).

partition([],_,[],[]).
partition([E|R],C,[E|Left1],Right):- E < C, !,
	partition(R,C,Left1,Right).
partition([E|R],C,Left,[E|Right1]):-
	partition(R,C,Left,Right1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Versions with granularity control
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

qsortdl_par_gc(As,I,G,Bs):- 
	qsortdl_par_gc_(As,I,G,Bs,[]).

qsortdl_par_gc_([X|L],I,G,R,R2) :-
	(
	    I > G ->
	    partition_gc(L,X,L1,L2,0,N1),
	    N2 is I - N1,
	    qsortdl_par_gc_(L1,N1,G,R,[X|R1]) '&!'
	    qsortdl_par_gc_(L2,N2,G,R1,R2)
	;
	    qsortdl_seq_([X|L],R,R2)
	).
qsortdl_par_gc_([],_,_,R,R).

qsortdl_par_nondet_gc(As,I,G,Bs):- 
	qsortdl_par_nondet_gc_(As,I,G,Bs,[]).

qsortdl_par_nondet_gc_([X|L],I,G,R,R2) :-
	(
	    I > G ->
	    partition_gc(L,X,L1,L2,0,N1),
	    N2 is I - N1,
	    qsortdl_par_nondet_gc_(L1,N1,G,R,[X|R1]) &
	    qsortdl_par_nondet_gc_(L2,N2,G,R1,R2)
	;
	    qsortdl_seq_([X|L],R,R2)
	).
qsortdl_par_nondet_gc_([],_,_,R,R).

partition_gc([], _Piv, [], [], LenL1, LenL1).
partition_gc([E|R], C, [E|Left1], Right, L1LenIn, L1LenOut) :-
        E<C, !,
        L1LenMid is L1LenIn + 1,
        partition_gc(R, C, Left1, Right, L1LenMid, L1LenOut).
partition_gc([E|R], C, Left, [E|Right1], L1lenIn, L1LenOut) :-
        E>=C,
        partition_gc(R, C, Left, Right1, L1lenIn, L1LenOut).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_list(0, []) :- !.
gen_list(M, [V|Ns]):- 
        M > 0,
        M1 is M - 1,
        V is M*M*M mod 7919,
        gen_list(M1, Ns).
