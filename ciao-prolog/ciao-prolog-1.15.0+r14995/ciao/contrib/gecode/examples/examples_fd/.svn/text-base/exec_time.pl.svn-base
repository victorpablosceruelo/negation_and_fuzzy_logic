:- module(exec_time, 
	[
	    exec_times/1
	]).

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

programs([smm,crypta,dec_seqs,dgr,magic3,magic4,pythagoras,queens,labeling,alpha]).

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

