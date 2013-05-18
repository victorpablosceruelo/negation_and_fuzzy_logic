:- module(ex_lists_ops,_,[cneg]).
% :- module(ex_lists_ops,_,[.(cneg), .(debugger_pkg)]).

test('t1', cneg(append1(X,Y,Z)), 'should_succeed', fail_and_forget_it((X, Y, Z)), 'should_fail').
test('t2', cneg(prefix(P,L)), 'should_succeed', fail_and_forget_it((P, L)), 'should_fail').
test('t3', cneg(suffix(S,L)), 'should_succeed', fail_and_forget_it((S, L)), 'should_fail').
test('t4', cneg(sublist(L1,L2)), 'should_succeed', fail_and_forget_it((L1, L2)), 'should_fail').

test('t5', cneg(append1(X,Y,Z)), 'should_succeed', append1(X,Y,Z), 'should_fail').
test('t6', cneg(prefix(P,L)), 'should_succeed', prefix(P,L), 'should_fail').
test('t7', cneg(suffix(S,L)), 'should_succeed', suffix(S,L), 'should_fail').
test('t8', cneg(sublist(L1,L2)), 'should_succeed', sublist(L1,L2), 'should_fail').


append1([],L,L).
append1([X|L],L1,[X|R]):-
	append1(L,L1,R).

prefix(P,L):- 
	append1(P,_S,L).
suffix(S,L):- 
	append1(_P,S,L).

sublist([],_List).
sublist([El|Sub],List):- 
	prefix(P,List),
	suffix([El|Sub],P).

no_append1(X,Y,Z):-cneg(append1(X,Y,Z)).
no_prefix(P,L):-cneg(prefix(P,L)).
no_suffix(S,L):-cneg(suffix(S,L)).
no_sublist(L1,L2):-cneg(sublist(L1,L2)).
