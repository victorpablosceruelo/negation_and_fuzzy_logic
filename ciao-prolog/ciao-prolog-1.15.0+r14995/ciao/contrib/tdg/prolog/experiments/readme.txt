----------------------------------------------
Steps to TDG automatically (using the drivers)
----------------------------------------------

1) Load 'driver_tdg_ciao.pl'. 
Call 'pre_tdg(N)' where N is the example id (see 'example_list.pl')
This produces steps 1,2 and 3 of manual TDG.

2) Load 'driver_tdg_sicstus.pl'.
...


-------------------------------------
Steps to TDG manually (e.g. tests.pl)
-------------------------------------

0) Open Ciao in 'tdg/experiments/'.

1) load_lp_program('tests.pl').
This creates the file 'tests_ir.pl' containing the clause_/3
representation of the program together with the EF meta_interpreter.

2) auto_pe(tests_ir). 
The file 'tests_ef.pl' with the EF version of 'tests.pl' is created by
PE of 'tests_ir.pl'

3) load_ef_program('tests_ef.pl').  
The file 'tests_ef_ir.pl' is created. It contains the clause_/3
representation of the program together with the Vanilla+ and the BC(k)
meta-interpreters.

4) Load 'tests_ef_ir.pl' in Sicstus and run it (see below for goal examples).




--------------
Goal examples:
--------------

?- domain([X,Y,Z],-100,100),
solve_bck([main(app/3,[[X,Y,Z],L1,L2],A,Cs)],10),once(labeling([ff],[X,Y,Z])).

?- length(L,N),domain([N],0,5),member(X,L),domain([X],-100,100),solve_bck([main(app/3,[L,L1,L2],A,Cs)],4),once(labeling([ff],[X,N])).
%%% This does not converge after giving solutions for N=4

?- domain([N],0,5),domain([X],-100,100),solve_bck([main(app/3,[L,L1,L2],A,Cs)],4),length(L,N),member(X,L),once(labeling([ff],[X,N])).
%%% This one DO converge.

?- domain([N],0,5),domain([X],-100,100),solve_bck([main(app/3,[L,L1,L2],A,Cs)],4),length(L,N),(member(X,L);L=[]),once(labeling([ff],[X,N])).
%%% This one considers the empty list as well.

?- domain([X],-100,100),solve_bck([main(app/3,[L,L1,L2],A,Cs)],4),(member(X,L);L=[]),once(labeling([ff],[X])).
%%% Here there is no restriction on the length of the list. Much
better!!!
%%% But member/2 does behave as we want. We need something like list(L,int) instead

?- solve_bck([main(app/3,[L,L1,L2],A,Cs)],4),domain(L,-100,100),once(labeling([ff],L)).
%%% This seems to behave perfect

?- solve_bck([main(gat/3,[L,N,X],A,Cs)],4),collect_vars(L,LVars),domain([N,X|LVars],-100,100),once(labeling([ff],[N,X|LVars])).

%%% This behaves fine except for the failing branch (see notes).
%%% It works perfect if I add this clause by hand to tests_ef.pl (I
%%% think the first one is sufficient)
main(memb/2,[_1,[]],no,0).
main(memb/2,[_1,[]],A,B) :-
        failed_builtin_1,
        exec_11(_2,_3,A,B) .

?- solve_bck([main(qsort/2,[L,L_p],yes,Cs)],5),collect_vars(L,LVars),domain(LVars,1,10),once(labeling([ff],LVars)).
%% Note that I take only the "yes" results as there are no <no,0> results.

?- solve_bck([main(partition/4,[L,X,L1,L2],yes,Cs)],4),collect_vars(L,LVars),domain([X|LVars],1,10),once(labeling([ff],[X|LVars])).
%% Note that I take only the "yes" results as there are no <no,0> results.

?- solve_bck([main(sorted_list/1,[in(L)],Ans,CPs)],3),collect_vars(L,Vars),domain(Vars,0,10),once(labeling([ff],Vars)).
L = [],
Ans = yes,
CPs = 2,
Vars = [] ? ;
L = [0],
Ans = yes,
CPs = 1,
Vars = [0] ? ;
L = [0,1],
Ans = yes,
CPs = 1,
Vars = [0,1] ? ;
L = [0,1,2],
Ans = yes,
CPs = 1,
Vars = [0,1,2] ? ;
L = [0,1,2,0|_A],
Ans = no,
CPs = 0,
Vars = [0,1,2,0] ? ;
L = [0,1,0|_A],
Ans = no,
CPs = 0,
Vars = [0,1,0] ? ;
L = [0,0|_A],
Ans = no,
CPs = 0,
Vars = [0,0] ? ;
no
% source_info
