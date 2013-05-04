:- module(queen_1, [safe/2], [assertions, nativeprops,
		ciaopp(tests(resources)),
		predefres(res_all)]).

:- doc(author, "Nai-Wei Lin").
:- doc(author, "Edison Mera").

:- doc(module, "This program plays the n-queens game.").

:- resource res_steps.

:- head_cost(ub, res_steps, 1).
:- literal_cost(ub, res_steps, 0).

:- head_cost(lb, res_steps, 1).
:- literal_cost(ub, res_steps, 0).

:- trust_default + cost(ub, res_steps, 0).
:- trust_default + cost(lb, res_steps, 0).

:- entry safe/2 : int * var.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% :- mode(safe/2,[+,-]).
% :- measure(safe/2,[int,length]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
safe(N, Queens) :-
	extend(N, N, Queens).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% :- mode(extend/3,[+,+,-]).
% :- measure(extend/3,[int,int,length]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
extend(0, _, []).
extend(M, N, [q(M, Q)|Selected]) :-
	M > 0,
	M1 is M -1,
	extend(M1, N, Selected),
	choose(N, Q),
	consistent(q(M, Q), Selected).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% :- mode(consistent/2,[+,+]).
% :- measure(consistent/2,[void,length]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
consistent(_, []).
consistent(Q, [Q1|Rest]) :-
	noattack(Q, Q1),
	consistent(Q, Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% :- mode(noattack/2,[+,+]). 
% :- measure(noattack/2,[void,void]). 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
noattack(q(X1, Y1), q(X2, Y2)) :-
	Y1 =\= Y2,
	X is X1 - X2,
	Y is Y1 - Y2,
	Z is Y2 - Y1,
	X =\= Y,
	X =\= Z.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% :- mode(choose/2,[+,-]).
% :- measure(choose/2,[int,int]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
choose(N, N) :-
	N > 0.
choose(N, M) :-
	N > 0,
	N1 is N -1,
	choose(N1, M).
