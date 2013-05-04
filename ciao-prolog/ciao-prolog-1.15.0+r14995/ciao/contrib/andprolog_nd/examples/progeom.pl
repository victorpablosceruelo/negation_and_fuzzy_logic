:- module(progeom,
	[
	    seq/2,
	    par_nondet/2,
	    data/1
	],
	[andprolog_nd]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

size(5).
gc(2).

data(X) :- size(X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

seq(N,X) :- pds_seq(N,X).
par_nondet(N,X) :- gc(GC), pds_par(N,GC,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mymember(X,[X|_Xs]).
mymember(X,[_1|Xs]) :-
        mymember(X,Xs) .

iota(N,List) :-
        iota1(0,N,List) .

iota1(K,K,[]) :- !.
iota1(K,N,[K|List]) :-
        K1 is K+1,
        iota1(K1,N,List) .

dif([],_1,_2,[],[]).
dif([S|Ss],Val,Mod,[D|Ds],[D2|D2s]) :-
        D is Val-S,
        D2 is Mod-D,
        dif(Ss,Val,Mod,Ds,D2s) .

rev([],L,L).
rev([X|Xs],Y,L) :-
        rev(Xs,[X|Y],L) .

mergedelete([],L,L).
mergedelete([D|Ds],[D|R],L2) :-
        mergedelete(Ds,R,L2) .
mergedelete([D|Ds],[X|R],[X|L2]) :-
        D@>X,
        mergedelete([D|Ds],R,L2) .

check_seq([],_1,L,L,_2) :- !.
check_seq(S,Choice,Old,L3,Modulus) :-
        dif(S,Choice,Modulus,Ds,Dds),
        mergedelete(Ds,Old,L2),
        rev(Dds,[],Rds),
        mergedelete(Rds,L2,L3).

check_par([],_1,_2,_3,L,L,_4) :- !.
check_par(S,Size,Gran,Choice,Old,L3,Modulus) :-
        dif(S,Choice,Modulus,Ds,Dds),
	(
	    Size < Gran ->
	    mergedelete(Ds,Old,L2) &
            rev(Dds,[],Rds)
	;
	    mergedelete(Ds,Old,L2),
            rev(Dds,[],Rds)
	),
        mergedelete(Rds,L2,L3).

pds1_seq([],_1,[],_2) :- !.
pds1_seq(Unused,List,[Choice|Rest],Mod) :-
        mymember(Choice,Unused),
        check_seq(List,Choice,Unused,U3,Mod),
        pds1_seq(U3,[Choice|List],Rest,Mod) .

pds1_par([],_1,_2,_3,[],_4) :- !.
pds1_par(Unused,List,Size,Gran,[Choice|Rest],Mod) :-
        mymember(Choice,Unused),
        check_par(List,Size,Gran,Choice,Unused,U3,Mod),
	Size1 is Size + 1,
        pds1_par(U3,[Choice|List],Size1,Gran,Rest,Mod) .

pds_seq(Order,[0|Ans]) :-
        N is Order*(Order+1)+1,
        iota(N,[0|List]),
        pds1_seq(List,[0],Ans,N).

pds_par(Order,Gran,[0|Ans]) :-
        N is Order*(Order+1)+1,
        iota(N,[0|List]),
        pds1_par(List,[0],1,Gran,Ans,N).

