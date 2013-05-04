:- module(backtr_join, _, []).

:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(system)).
:- use_module(library(format), [format/2]).
:- use_module(library(lists), [append/3]).
:- use_module(library(apll_parback)).
:- use_module(library(odd)).
:- use_module(library(concurrency)).

:- push_prolog_flag(multi_arity_warnings, off).

:- op(950, xfy, [&>, &]).
:- op(950, xf, [<&]).


main :-
	set_prolog_flag(gc, off),

	start_thread(agent(fifo)),
	start_thread(agent(fifo)),
	start_thread(agent(fifo)),
	start_thread(agent(fifo)),
	start_thread(agent(fifo)),
	start_thread(agent(fifo)),
	start_thread(agent(fifo)),
	start_thread(agent(fifo)),
	start_thread(agent(fifo)),
	start_thread(agent(fifo)),
	start_thread(agent(fifo)),
	start_thread(agent(fifo)),
	start_thread(agent(fifo)),
	start_thread(agent(fifo)),
	start_thread(agent(fifo)),
	start_thread(agent(fifo)),
	start_thread(agent(fifo)),
	start_thread(agent(fifo)),
	start_thread(agent(fifo)),
	start_thread(agent(fifo)),
	start_thread(agent(fifo)),
	start_thread(agent(fifo)),
	start_thread(agent(fifo)),
	start_thread(agent(fifo)),
	start_thread(agent(fifo)),
	start_thread(agent(fifo)),
	start_thread(agent(fifo)),
	start_thread(agent(fifo)),
	start_thread(agent(fifo)),
	start_thread(agent(fifo)),
	start_thread(agent(fifo)),
	true.

main(X,Y) :-
	set_prolog_flag(gc, off),

	p(X,_,_) &> Hp,
	q(Y,_) &> Hq,

	Hp <&,
	Hq <& .

main(A,B,C,D) :-
	set_prolog_flag(gc, off),

	g1(A,B,C,D),
% 	g11(A,B,C,D),
%  	g4(A,B,C),
% 	g3(A,B,C,D),
% 	g33(A,B,C),
% 	g2(A,B),
	display('Output:'), display(' a'(A)), display(' b'(B)),
	display(' c'(C)), display(' d'(D)), nl,
	true.

main(A,B,C,D,E,F) :-
	set_prolog_flag(gc, off),

	g6(A,B,C,D,E,F),
% 	display('Output:'), display(' a'(A)), display(' b'(B)),
% 	display(' c'(C)), display(' d'(D)), display(' e'(E)), 
% 	display(' f'(F)), nl,
	true.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% &>/2 and <&/1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- meta_predicate((goal&goal)).
A & B :-
	A &> H,
	B,
	H <& .
% 	'$metachoice'(M), display(local(M)), nl.


:- meta_predicate((goal&>_)).
Goal &> Handler :-
	push_goal(Goal,nondet,Handler),
% 	pause(1),
	undo(cancellation(Handler)),
% 	(
	release_some_suspended_thread.
% 	;
% 	    display(cancellation(Handler)), nl,
% 	    cancellation(Handler),
% 	    fail
% 	).


Handler <& :-
% 	undo(republish_goal(Handler)),
	enter_mutex_self,
% 	pause(1),
	(
	    goal_available(Handler) ->
	    exit_mutex_self,
	    retrieve_goal(Handler,Goal),
	    call(Goal)
% 	    save_init_execution(Handler),
%  	    call_handler_local(Handler)
% 	    save_end_execution(Handler),
% 	    undo(move_execution_top(Handler))
% 	    (
% 		true
% 	    ;
% 		move_execution_top(Handler),
% 		fail
% 	    )
	;
	    perform_some_other_work(Handler,fifo)
	).
Handler <& :-
	push_goal(Handler),
	release_some_suspended_thread,
	fail.


sending_event(_,_).
sending_event(Handler,GS) :-
	enter_mutex_self,
	goal_finished(Handler),
	set_goal_tobacktrack(Handler),
	enter_mutex_remote(Handler),
	send_event(Handler),
	release_remote(Handler),
	exit_mutex_remote(Handler),
	perform_some_other_work(Handler,GS).


perform_some_other_work(Handler,GS) :-
	(
	    goal_finished(Handler) ->
	    exit_mutex_self,
	    sending_event(Handler,GS)
	;
	    (
		goal_failed(Handler) ->
		exit_mutex_self,
		fail
	    ;
		(
		    read_event(H) ->
		    exit_mutex_self,
		    move_execution_top(H),
		    fail
		;
		    (
			find_goal(GS,H) ->
			exit_mutex_self,
			save_init_execution(H),
			call_handler(H),
% 			save_end_execution(H),
			enter_mutex_self
		    ;
			suspend
		    ),
		    perform_some_other_work(Handler,GS)
		)
	    )
	).
perform_some_other_work(Handler,GS) :-
	enter_mutex_self,
	(
	    goal_failed(Handler) -> exit_mutex_self, fail
	;
	    perform_some_other_work(Handler,GS)
	).


work(GS) :-
	(
	    read_event(Handler) ->
	    exit_mutex_self,
	    move_execution_top(Handler),
	    fail
	;
	    (
		find_goal(GS,Handler) ->
		exit_mutex_self,
		save_init_execution(Handler),
		call_handler(Handler)
% 		save_end_execution(Handler)
	    ;
		suspend,
		work(GS)
	    )
        ).


% work(GS,Handler) :-
% 	(
% 	    read_event(Handler) ->
% 	    exit_mutex_self,
% 	    move_execution_top(Handler),
% 	    fail
% 	;
% 	    (
% 		find_goal(GS,Handler) ->
% 		retrieve_goal(Handler,Goal),
% 		exit_mutex_self,
% 		save_init_execution(Handler),
% 		call_handler(Handler,Goal)
% 	    ;
% 		suspend,
% 		enter_mutex_self,
% 		work(GS,Handler)
%             )
%         ).


% work(GS, PrevCall, ThisCall) :-
% 	(
% 	    read_event(Handler) ->
% 	    exit_mutex_self,
% 	    (
% 		move_execution_top(Handler) ->
% 		display(true), nl,
% 		true
% 	    ;
% 		display(false), nl,
% 		'$metacut'(PrevCall)
% 	    ),
% 	    fail
% 	;
% 	    (
% 		find_goal(GS,Handler) ->
% 		exit_mutex_self,
% 		save_init_execution(Handler),
% 		call_handler(Handler),
% 		'$metachoice'(ThisCall)
% 	    ;
% 		suspend,
% 		enter_mutex_self,
% 		work(GS, PrevCall, ThisCall)
%             )
%         ).
% % work(_,_) :- fail.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Code for backtracking
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% call_handler_local(Handler) :-
% 	retrieve_goal(Handler,Goal),
% 	call(Goal).
% 	save_end_execution(Handler),
% 	set_goal_finished(Handler).
% call_handler_local(Handler) :-
% 	set_goal_failed(Handler),
% 	metacut_garbage_slots(Handler),
% 	fail.


call_handler(Handler) :-
	retrieve_goal(Handler,Goal),
% 	exit_mutex_self,
	call(Goal),
% 	display(call_handler_finished(Handler,Goal)), nl,
	enter_mutex(Handler),
	save_end_execution(Handler),
	set_goal_finished(Handler),
	release(Handler),
	exit_mutex(Handler).
call_handler(Handler) :-
% 	display(call_handler_failed(Handler)), nl,
	enter_mutex(Handler),
	set_goal_failed(Handler),
	release(Handler),
	metacut_garbage_slots(Handler),
	exit_mutex(Handler),
	fail.
% call_handler(_) :- fail.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% New threads
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

agent(GS) :-
	enter_mutex_self,
	work(GS),
	agent(GS).
agent(GS) :- agent(GS).


% agent(GS):- 
% 	'$metachoice'(InitialChPt), 
% 	agent(GS, InitialChPt).
% agent(GS):- display(exiting), nl, agent(GS).

% agent(GS, PrevCall) :-
% 	enter_mutex_self,
% 	work(GS, PrevCall, ThisCall),
% 	agent(GS, ThisCall).
% agent(GS, PrevCall) :- agent(GS, PrevCall).
% % agent(_) :- fail.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Goals + facts
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

g1(A,B,C,D) :-
	u(D) & v(C) & r(B) & s(A).
% 	u(D) &> U,
% 	v(C) &> V,
% 	r(B) &> R,
% 	s(A),
% 	R <&,
% 	U <&,
% 	V <& .

g6(A,B,C,D,E,F) :-
	u(F) & v(E) & u(D) & v(C) & r(B) & s(A).
% 	u(F) &> HF,
% 	v(E) &> HE,
% 	u(D) &> HD,
% 	v(C) &> HC,
% 	r(B) &> HB,
% 	s(A),
% 	pause(1),
% 	display(a(A,B,C,D,E,F)), nl,
% 	HB <&,
% 	pause(1),
% 	display(b(A,B,C,D,E,F)), nl,
% 	HC <&,
% 	pause(1),
% 	display(c(A,B,C,D,E,F)), nl,
% 	HD <&,
% 	pause(1),
% 	display(d(A,B,C,D,E,F)), nl,
% 	HE <&,
% 	pause(1),
% 	display(e(A,B,C,D,E,F)), nl,
% 	HF <&,
% 	pause(1),
% 	display(f(A,B,C,D,E,F)), nl.


g5(FA,FB,FC,FD) :-
	(u(D), DF is D * 5, fib_seq_chpt(DF, FD)) &
	(v(C), CF is C * 5, fib_seq_chpt(CF, FC)) &
	(r(B), BF is B * 5, fib_seq_chpt(BF, FB)) &
	(s(A), AF is A * 5, fib_seq_chpt(AF, FA)).

g11(A,B,C,D) :-
	u(D1) & v(C1) & r(B1) & s(A1),
	A = A1, B = B1, C = C1, D = D1.

g2(A,B) :-
	u(B) & v(A).

g3(A,B,C,D) :-
	g2(C,D) & r(B) & s(A).

g33(B,C,D) :-
	g2(C,D) & r(B).

g4(A,B,C) :-
	u(C) & v(B) & r(A).

p(a,a,a).
p(b,a,a).
p(c,a,a).

q(1,1).
q(2,1).
q(3,1).

r(1).
r(2).
r(3).

s(1).
s(2).
s(3).

u(1).
u(2).
u(3).

v(1).
v(2).
v(3).

% r(1) :- display(r1), nl.
% r(2) :- display(r2), nl.
% r(3) :- display(r3), nl.

% s(1) :- display(s1), nl.
% s(2) :- display(s2), nl.
% s(3) :- display(s3), nl.

% u(1) :- display(u1), nl.
% u(2) :- display(u2), nl.
% u(3) :- display(u3), nl.

% v(1) :- display(v1), nl.
% v(2) :- display(v2), nl.
% v(3) :- display(v3), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

qsort(X) :-
	set_prolog_flag(gc, off),
	gen_list(X,L),
        statistics(walltime, [T1,_]),
	qsort_gc(L,X,1000,_),
        statistics(walltime, [T2,_]),
        Delta is T2 - T1,
	format("-- qsort(~f), ~f ms.~n", [X,Delta]).

qsort :-
	set_prolog_flag(gc, off),
	start_thread(agent(fifo)),
	start_thread(agent(fifo)),
	start_thread(agent(fifo)),
	start_thread(agent(fifo)),
	start_thread(agent(fifo)),
	start_thread(agent(fifo)),
	start_thread(agent(fifo)),
	repeat,
% 	pause(1),
	display('---------------------'), nl,
	qsort(10000),
	fail.

qsort_gc([], _InputLen, _GranLevel, []).
qsort_gc([X|L], InputLen, GLev, R) :-
        (
            InputLen > GLev ->
            partition_gc(L, X, L1, L2, 0, N1),
            N2 is InputLen - N1,
            qsort_gc(L1, N1, GLev, R1) & qsort_gc(L2, N2, GLev, R2),
            append(R1, [X|R2], R)
        ;
            qsort_seq([X|L], R)
        ).

partition_gc([], _Piv, [], [], LenL1, LenL1).
partition_gc([E|R], C, [E|Left1], Right, L1LenIn, L1LenOut) :-
        E<C, !,
        L1LenMid is L1LenIn + 1,
        partition_gc(R, C, Left1, Right, L1LenMid, L1LenOut).
partition_gc([E|R], C, Left, [E|Right1], L1lenIn, L1LenOut) :-
        E>=C,
        partition_gc(R, C, Left, Right1, L1lenIn, L1LenOut).

gen_list(0, []).
gen_list(M, [V|Ns]):- 
        M > 0,
        M1 is M - 1,
        V is M*M*M mod 7919,
        gen_list(M1, Ns).

qsort_seq([], []).
qsort_seq([X|L], R) :-
	partition(L, X, L1, L2),
	qsort_seq(L2, R2), 
        qsort_seq(L1, R1), 
        append(R1, [X|R2], R).

partition([], _B, [], []).
partition([E|R], C, [E|Left1], Right) :- 
	E < C, !,
	partition(R, C, Left1, Right).
partition([E|R], C, Left, [E|Right1]) :-
	E >= C,
	partition(R, C, Left, Right1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fibo(X,Y) :-
	set_prolog_flag(gc, off),
        statistics(walltime, [T1,_]),
% 	fib_det_gc(X,15,Y),
	fib_par(X,Y),
        statistics(walltime, [T2,_]),
        Delta is T2 - T1,
	format("-- fibo(~f), ~f ms.~n", [X,Delta]).

fibo :-
	set_prolog_flag(gc, off),
	start_thread(agent(fifo)),
	start_thread(agent(fifo)),
	start_thread(agent(fifo)),
	start_thread(agent(fifo)),
	start_thread(agent(fifo)),
	start_thread(agent(fifo)),
	start_thread(agent(fifo)),
	repeat,
% 	pause(1),
	display('---------------------'), nl,
	fibo(20,_),
	fail.

fib_seq(0, 0).
fib_seq(1, 1).
fib_seq(N, F) :-
        N > 1,
        N1 is N - 1,
        N2 is N - 2,
        fib_seq(N1, F1),
        fib_seq(N2, F2),
        F is F1 + F2.

fib_seq_chpt(X, X) :- X < 2.
fib_seq_chpt(N, F) :-
        N > 1,
        N1 is N - 1,
        N2 is N - 2,
        fib_seq_chpt(N1, F1),
        fib_seq_chpt(N2, F2),
        F is F1 + F2.

fib_det_gc(0, _, 0).
fib_det_gc(1, _, 1).
fib_det_gc(N, Level, F) :-
        N > 1,
        (
            N =< Level ->
            fib_seq(N, F)
        ;
            N1 is N - 1,
            N2 is N - 2,
            fib_det_gc(N1, Level, F1) & fib_det_gc(N2, Level, F2),
            F is F1 + F2
        ).

fib_par(0, 0).
fib_par(1, 1).
fib_par(N, F) :-
        N > 1,
        N1 is N - 1,
        N2 is N - 2,
        fib_par(N1, F1) &
        fib_par(N2, F2),
        F is F1 + F2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

qu(X,Y) :-
	set_prolog_flag(gc, off),
        statistics(walltime, [T1,_]),
	queens(X,Y),
        statistics(walltime, [T2,_]),
        Delta is T2 - T1,
	format("-- queens(~f)=", [X]),
	display(Y),
	format(", ~f ms.~n", [Delta]).

qu :-
	set_prolog_flag(gc, off),
	start_thread(agent(fifo)),
	start_thread(agent(fifo)),
	start_thread(agent(fifo)),
	start_thread(agent(fifo)),
	start_thread(agent(fifo)),
	start_thread(agent(fifo)),
	start_thread(agent(fifo)),
	repeat,
% 	pause(1),
	display('---------------------'), nl,
	qu(8,_),
	fail.

queens(N, Qs):-
        queens_list(N, Ns),
        solve_queens(Ns, [], Qs).    % To place, placed, result

solve_queens([], Qs, Qs).
solve_queens(Unplaced, Placed, Qs):-
        select_queen(Q, Unplaced, NewUnplaced),
        no_attack(Q, Placed) &
        solve_queens(NewUnplaced, [Q|Placed], Qs).

no_attack(Q, Safe):- no_attack_acc(Safe, Q, 1).

no_attack_acc([], _Queen, _Nb).
no_attack_acc([Y|Ys], Queen, Nb):-
        Queen =\= Y + Nb,
        Queen =\= Y - Nb,
        Nb1 is Nb + 1,
        no_attack_acc(Ys, Queen, Nb1).

select_queen(X, [X|Ys], Ys).
select_queen(X, [Y|Ys], [Y|Zs]):-
        select_queen(X, Ys, Zs).

queens_list(0, []).
queens_list(N, [N|Ns]):-
        N > 0,
        N1 is N - 1,
        queens_list(N1, Ns).

