:- module(exec_time, 
	[
	    exec_times/1
	]).

 %% Executing smm
 %%     First answer: 0 milliseconds
 %%     All answers: 0 milliseconds
 %% Executing crypta
 %%     First answer: 36 milliseconds
 %%     All answers: 249 milliseconds
 %% Executing dec_seqs
 %%     First answer: 374 milliseconds
 %%     All answers: 381 milliseconds
 %% Executing dgr
 %%     First answer: 37 milliseconds
 %%     All answers: 44 milliseconds
 %% Executing magic3
 %%     First answer: 0 milliseconds
 %%     All answers: 4 milliseconds
 %% Executing magic4
 %%     First answer: 2 milliseconds
 %%     All answers: 36350 milliseconds
 %% Executing pythagoras
 %%     First answer: 0 milliseconds
 %%     All answers: 0 milliseconds
 %% Executing queens
 %%     First answer: 4 milliseconds
 %%     All answers: 55 milliseconds
 %% Executing labeling
 %%     First answer: 0 milliseconds
 %%     All answers: 6592 milliseconds
 %% Executing alpha
 %%     First answer: 56943 milliseconds
 %%     All answers: 134128 milliseconds
 %% Executing smm
 %%     First answer: 40 milliseconds
 %%     All answers: 58 milliseconds
 %% Executing crypta
 %%     First answer: 11988 milliseconds
 %%     All answers: 109682 milliseconds
 %% Executing dec_seqs
 %%     First answer: 1252 milliseconds
 %%     All answers: 1252 milliseconds
 %% Executing dgr
 %%     First answer: 5492 milliseconds
 %%     All answers: 6692 milliseconds
 %% Executing magic3
 %%     First answer: 15 milliseconds
 %%     All answers: 155 milliseconds
 %% Executing magic4
 %%     First answer: 119 milliseconds
 %%     All answers: 4122285 milliseconds
 %% Executing pythagoras
 %%     First answer: 1588 milliseconds
 %%     All answers: 2317392 milliseconds
 %% Executing queens
 %%     First answer: 160 milliseconds
 %%     All answers: 2732 milliseconds
 %% Executing labeling
 %%     First answer: 0 milliseconds
 %%     All answers: 5688 milliseconds
 %% Executing alpha
        -----
  
:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(between)).
:- use_module(library(format)).

:- use_module(smm, [do/0]).
:- use_module(crypta, [do/0]).
:- use_module(dec_seqs, [do/0]).
:- use_module(dgr, [do/0]).
:- use_module(magic3, [do/0]).
:- use_module(magic4, [do/0]).
:- use_module(pythagoras, [do/0]).
:- use_module(queens, [do/0]).
:- use_module(labeling, [do/0]).
:- use_module(alpha, [do/0]).

 %% programs([smm,crypta,dec_seqs,dgr,magic3,magic4,pythagoras,queens,labeling,alpha]).
 %% programs([alpha]).
programs([crypta,alpha,labeling]).

exec_times(Nt) :-
	N is Nt * 1000,
	programs(Ps),
	member(P,Ps),
	display('Executing '), display(P),nl,
	exec_time(N,first(P:do),T1),
	format("    First answer: ~d milliseconds~n", T1),
	exec_time(N,P:do,T2),
	format("    All answers: ~d milliseconds~n", T2),
	fail.

exec_times(_).

first(X) :-
	call(X), !.

exec_time(NT,Pred,T) :-
        statistics(runtime,[_,_]),
	(call(Pred), fail; true),
        statistics(runtime,[_,Tt]),
	get_time(Tt,NT,T,Pred).
 
get_time(Tt,NT,Tt,_) :-
	Tt >= NT, !.

get_time(Tt,NT,T,Pred) :-
	N is NT / (Tt+1),
        statistics(runtime,[_,_]),
        (
            between(1,N,_),
	    (call(Pred), fail; true),
            fail
        ;
            true
        ),
        statistics(runtime,[_,Tfin]),
        (
            between(1,N,_),
            fail
        ;
            true
        ),
        statistics(runtime,[_,Tfin2]),
        T is (Tfin - Tfin2) / N.

