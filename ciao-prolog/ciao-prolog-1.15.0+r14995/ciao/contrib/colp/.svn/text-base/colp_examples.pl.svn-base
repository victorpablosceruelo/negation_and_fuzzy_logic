:- module(colp_examples, _, [colp, expander]).

:- colp(impl(west)).
%:- colp(trace).

:- coinductive(comember/2).

comember(X, [X | _]).
comember(X, [_ | T]):- comember(X, T).


/*
%-----------------------------------------------------
% Examples: Co-inductive predicates.
%-----------------------------------------------------
:- coinductive(coList/3).
:- coinductive(bitStr/1).
:- coinductive(co_append/3).
:- coinductive(co_member/2).
:- coinductive(state/2).
:- inductive(drop/3).
%:- table(drop/3).
%:- write_depth(10,10).

%-----------------------------------------------------
coList([1|L],A,B) :- coList(L,A,B).

bit(0).
bit(1).
bitStr([H|T]) :- bit(H), bitStr(T).

co_append([], X, X). 
co_append([H|T], Y, [H|Z]) :- co_append(T, Y, Z).

%-----------------------------------------------------
drop(H,[H|T], T).
drop(H,[_|T],T1) :- drop(H,T,T1).

co_member(X,L) :- drop(X,L,L1), co_member(X,L1).

%-----------------------------------------------------
%:- table work/0.

state(s0,[s0,is1|T]) :- enter, work, state(s1,T).
state(s1,[s1|T]) :- exit,state(s2,T).
state(s2,[s2|T]) :- m_repeat,state(s0,T).
state(s0,[s0|T]) :- error,state(s3,T).
state(s3,[s3|T]) :- m_repeat,state(s0,T).

work :- work.
work.
enter.
exit.
m_repeat.
error.

%-----------------------------------------------------
:- inductive(anc/2).

pr(a,b).
pr(b,c).
anc(X,Y) :- anc(X,Z), anc(Z,Y).
anc(X,Y) :- pr(X,Y).

%-----------------------------------------------------
:- coinductive(as1/0).
%:- table as2/0.

as1 :- \+ as2.
as2 :- \+ as1.

%---------------------------------------------------------------
*/

 

