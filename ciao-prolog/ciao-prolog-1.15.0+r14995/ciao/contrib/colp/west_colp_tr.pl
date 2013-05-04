:- module(west_colp_tr, [west_sentence/3,
	                 west_trace/1], []).

west_sentence(A, B, _M):-
	sentence(A, B).

:- dynamic coinductive/4.
:- dynamic inductive/4.
:- data trace/0.

west_trace(on) :-
	assertz_fact(trace).
west_trace(off) :-
	retractall_fact(trace).

decl_coinductive(F/N, (S :- Body)) :-
	( 
	    trace ->
	    Body =  ( mytrace(getval(SF,L), getval(SF,L)),
			(
			    mytrace(in_stack(S,L),in_stack(S,L))
			;
			    mytrace(\+ in_stack(S,L),\+ in_stack(S,L)),
			    mytrace(setval(SF,[S|L]),setval(SF,[S|L])),
			    mytrace(NS, NS)
			)
		   )
	    ;
	    Body = ( getval(SF,L), 
	                ( 
			    in_stack(S,L)
			; 
			    \+ in_stack(S,L),
			    setval(SF,[S|L]),
			    NS
			)
		   )
	),
	functor(S,F,N), atom_concat(coinductive_,F,NF),
	functor(NS,NF,N), match_args(N,S,NS),
	atom_concat(stack_,F,SFn), atomic_concat(SFn,N,SF),
	assertz_fact(coinductive(S,F,N,NS)).

decl_inductive(F/N, (S :- 
                      getval(SF,L), not_in_stack(S,L),
	              setval(SF,[S|L]), NS)) :-
  functor(S,F,N), atom_concat(inductive_,F,NF),
  functor(NS,NF,N), match_args(N,S,NS),
  atom_concat(stack_,F,SFn), atomic_concat(SFn, N, SF),
  assertz_fact(inductive(S,F,N,NS)).

match_args(0,_,_) :- !.
match_args(I,S1,S2) :- arg(I,S1,A), arg(I,S2,A),
                       I1 is I-1, match_args(I1,S1,S2).

%-----------------------------------------------------

sentence(0, [(:- use_module(library(colp(west_colp_rt))))]) :-
	retractall_fact(coinductive(_,_,_,_)),
	retractall_fact(inductive(_,_,_,_)).

sentence(end_of_file, [end_of_file]).

sentence((H:-B), (NH:-B)) :-
	coinductive(H,_F,_N,NH), !.

sentence((H:-B),(NH:-B) ) :-
	inductive(H,_F,_N,NH), !.

sentence((:- coinductive(F)), S):- !,
	decl_coinductive(F, S).

sentence((:- inductive(F)), S):- !,
	decl_inductive(F, S).

sentence((:- D), (:- D)) :- !.

sentence(H,NH) :-
	coinductive(H,_F,_N,NH), !.

sentence(H,NH) :-
	inductive(H,_F,_N,NH), !.
sentence(A, A).     

%-----------------------------------------------------------

atomic_concat(A,N,B):-
	atom_number(N2, N),
	atom_concat(A,N2,B).


