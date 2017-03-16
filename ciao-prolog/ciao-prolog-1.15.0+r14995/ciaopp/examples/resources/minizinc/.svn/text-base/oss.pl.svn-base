:- module(_, [main/5], [ciaopp(examples(resources(minizinc)))]).

:- entry main/5 : int * int * int * array * var.

/* MZN
int: Endtime;
int: NMachines;
int: NJobs;

set of int: Machines = 1..NMachines;
set of int: Jobs = 1..NJobs;

array[Machines,Jobs] of int: duration;

array[Machines,Jobs] of var 0..Endtime: start;
var 0..Endtime: makespan;
*/ 

/*  Possible data file
Endtime = 1509;
NMachines = 3;
NJobs = 3;

duration = [|661,6,333|168,489,343|171,505,324|];

Ciao:
main(1509,3,3,a(array(a(array(661,6,333)),
	            a(array(168,489,343)),
		    a(array(171,505,324)))),Start).
*/

main(Endtime,NMachines,NJobs,Duration,Start):- 
     % array[Machines,Jobs] of var 0..Endtime: start;
     Size is NMachines-1 +1,
     build_array(Start,Size),
     init_i(Size,0,Endtime,NJobs,Start),
     % var 0..Endtime: makespan;
     in(Makespan,0,Endtime),
     %first constraint
     Index1 is NMachines-1+1,  
     forall1_m(Index1,NMachines,NJobs,Start,Duration),
     %second constraint
     Index2 is NJobs-1+1,  
     forall2_j(Index2,NJobs,NMachines,Start,Duration),
     %third constraint
     Index3 is NMachines-1+1,  
     forall3_m(Index3,NMachines,NJobs,Start,Duration,Makespan).

init_i(0,_DL,_DU,_Uj,_A) :- !.
init_i(Ind,DL,DU,Uj,A):-
    % Ind > 0,
    element_array(Ind,A,Ai),
    Size is Uj-1+1,
    build_array(Ai,Size),
    init_ij(Size,DL,DU,Ai),
    NInd is Ind - 1,
    init_i(NInd,DL,DU,Uj,A).

init_ij(0,_DL,_DU,_A) :- !.
init_ij(Ind,DL,DU, A):-
    % Ind > 0,
    element_dint(Ind,A,Ai),
    % Ai in DL..DU, 
    in(Ai,DL,DU),
    NInd is Ind - 1,
    init_ij(NInd,DL,DU,A).

/*
predicate not_at_the_same_time(Machines: m1, Jobs: j1, Machines: m2, Jobs: j2) =
			start[m1,j1] + duration[m1,j1] <= start[m2,j2]
		\/	start[m2,j2] + duration[m2,j2] <= start[m1,j1];
*/


not_at_the_same_time(M1,J1,M2,J2,Start,Duration):-
    % start[m1,j1] + duration[m1,j1] <= start[m2,j2]
    element2d_dint(M1,J1,Start,S1mj),
    element2d_dint(M1,J1,Duration,D1mj),
    element2d_dint(M2,J2,Start,S2mj),
    plus_dint_dint_var(S1mj,D1mj,Tmp2),
    reif_leq_dint_dint_var(Tmp2,S2mj,A),
    % start[m2,j2] + duration[m2,j2] <= start[m1,j1];
    element2d_dint(M2,J2,Duration,D2mj),
    plus_dint_dint_var(S2mj,D2mj,Tmp1),
    reif_leq_dint_dint_var(Tmp1,S1mj,B),
    or(A,B,1).


/*
constraint
	forall(m in Machines)(
		forall(j1,j2 in Jobs where j1 < j2)(
			not_at_same_time(m,j1,m,j2)
		)
	);
*/

% % Uncomment this trusts to find the exact bounds (EMM):
% :- trust pred forall1_m(Ind,_1,_2,_3,_4)
%          : ( int(Ind), int(_1), int(_2), array(_3), array(_4) )
%         => ( int(Ind), int(_1), int(_2), array(_3), array(_4), size(lb,Ind,int(Ind)), size(lb,_1,int(_1)), size(lb,_2,int(_2)), size(lb,_3,size(_3)), size(lb,_4,size(_4)) )
%          + ( cost(lb,alldiff,0), cost(lb,constraints,5*((int(_2)*(int(_2)-1)/2)*int(Ind))), cost(lb,numvars,3*((int(_2)*(int(_2)-1)/2)*int(Ind))) ).

% :- trust pred forall1_m(Ind,_1,_2,_3,_4)
%          : ( int(Ind), int(_1), int(_2), array(_3), array(_4) )
%         => ( int(Ind), int(_1), int(_2), array(_3), array(_4), size(ub,Ind,int(Ind)), size(ub,_1,int(_1)), size(ub,_2,int(_2)), size(ub,_3,size(_3)), size(ub,_4,size(_4)) )
%          + ( cost(ub,alldiff,0), cost(ub,constraints,5*((int(_2)*(int(_2)-1)/2)*int(Ind))), cost(ub,numvars,3*((int(_2)*(int(_2)-1)/2)*int(Ind))) ).

% :- trust pred forall2_j(Ind,_1,_2,_3,_4)
%          : ( int(Ind), int(_1), int(_2), array(_3), array(_4) )
%         => ( int(Ind), int(_1), int(_2), array(_3), array(_4), size(lb,Ind,int(Ind)), size(lb,_1,int(_1)), size(lb,_2,int(_2)), size(lb,_3,size(_3)), size(lb,_4,size(_4)) )
%          + ( cost(lb,alldiff,0), cost(lb,constraints,5*((int(_2)*(int(_2)-1)/2)*int(Ind))), cost(lb,numvars,3*((int(_2)*(int(_2)-1)/2)*int(Ind))) ).

% :- trust pred forall2_j(Ind,_1,_2,_3,_4)
%          : ( int(Ind), int(_1), int(_2), array(_3), array(_4) )
%         => ( int(Ind), int(_1), int(_2), array(_3), array(_4), size(ub,Ind,int(Ind)), size(ub,_1,int(_1)), size(ub,_2,int(_2)), size(ub,_3,size(_3)), size(ub,_4,size(_4)) )
%          + ( cost(ub,alldiff,0), cost(ub,constraints,5*((int(_2)*(int(_2)-1)/2)*int(Ind))), cost(ub,numvars,3*((int(_2)*(int(_2)-1)/2)*int(Ind))) ).

forall1_m(0, _, _, _, _) :- !.
forall1_m(Ind,NMachines,NJobs,Start,Duration):-
        % Ind > 0,
        Index_m is NMachines - Ind + 1, 
        %
        Ind_j1 is NJobs - 1 + 1,
        forall1_mj1(Ind_j1, NJobs, Index_m, Start, Duration),
        %
	NInd is Ind-1,
        forall1_m(NInd, NMachines,NJobs,Start,Duration).

forall1_mj1(0, _, _, _, _) :- !.
forall1_mj1(Ind,NJobs,Index_m,Start,Duration):-
        % Ind > 0,
        Index_j1 is NJobs - Ind + 1, 
        %
        Ind_j2 is NJobs - 1 + 1,
        forall1_mj1j2(Ind_j2,NJobs,Index_m,Index_j1,Start,Duration),
        %
	NInd is Ind-1,
        forall1_mj1(NInd, NJobs, Index_m, Start,Duration).

forall1_mj1j2(0, _, _, _, _, _) :- !.
forall1_mj1j2(Ind,NJobs,Index_m,Index_j1,Start,Duration):-
        % Ind > 0,
        Index_j2 is NJobs - Ind + 1, 
        %
	M is Index_m-1+1,
	J1 is Index_j1-1+1,
	J2 is Index_j2-1+1,
        %
        ( less(J1,J2) -> 
	  not_at_the_same_time(M,J1,M,J2,Start,Duration)
	;
          true
	),
        %
	NInd is Ind-1,
        forall1_mj1j2(NInd,NJobs,Index_m, Index_j1, Start,Duration).

/*
constraint
	forall(j in Jobs)(
		forall(m1,m2 in Machines where m1 < m2)(
			not_at_same_time(m1,j,m2,j)
		)
	);
*/


forall2_j(0, _, _, _, _) :- !.
forall2_j(Ind,NJobs,NMachines,Start,Duration):-
        % Ind > 0,
        Index_j is NJobs - Ind + 1, 
        %
        Ind_m1 is NMachines - 1 + 1,
        forall2_jm1(Ind_m1, NMachines, Index_j, Start,Duration),
        %
	NInd is Ind-1,
        forall2_j(NInd,NJobs,NMachines,Start,Duration).

forall2_jm1(0, _, _, _, _) :- !.
forall2_jm1(Ind,NMachines,Index_j,Start,Duration):-
        % Ind > 0,
        Index_m1 is NMachines - Ind + 1, 
        %
        Ind_m2 is NMachines - 1 + 1,
        forall2_jm1m2(Ind_m2, NMachines, Index_j, Index_m1, Start,Duration),
        %
	NInd is Ind-1,
        forall2_jm1(NInd, NMachines, Index_j, Start,Duration).

forall2_jm1m2(0, _, _, _, _,_) :- !.
forall2_jm1m2(Ind,NMachines,Index_j,Index_m1,Start,Duration):-
        % Ind > 0,
        Index_m2 is NMachines - Ind + 1, 
        %
	J is Index_j-1+1,
	M1 is Index_m1-1+1,
	M2 is Index_m2-1+1,
        ( less(M1,M2) -> 
	  not_at_the_same_time(M1,J,M2,J,Start,Duration)
	;
          true
	),
        %
	NInd is Ind-1,
        forall2_jm1m2(NInd,NMachines,Index_j, Index_m1, Start,Duration).

/*
constraint
	forall(m in Machines)(
		forall(j in Jobs)(
			start[m,j] + duration[m,j] <= makespan
		)
	);

*/

forall3_m(0, _, _, _, _,_) :- !.
forall3_m(Ind,NMachines,NJobs,Start,Duration,Makespan):-
        % Ind > 0,
        Index_m is NMachines - Ind + 1, 
        %
        Ind_j is NJobs - 1 + 1,
        forall3_mj(Ind_j, NJobs, Index_m, Start,Duration,Makespan),
        %
	NInd is Ind-1,
        forall3_m(NInd, NMachines,NJobs,Start,Duration,Makespan).

forall3_mj(0, _, _, _, _, _) :- !.
forall3_mj(Ind,NJobs,Index_m,Start,Duration,Makespan):-
        % Ind > 0,
        Index_j is NJobs - Ind + 1, 
        %
        %start[m,j] + duration[m,j] <= makespan
	M is Index_m-1+1,
	J is Index_j-1+1,
	element2d_dint(M,J,Start,Smj),
	element2d_dint(M,J,Duration,Dmj),
	plus_dint_dint_var(Smj,Dmj,Tmp),
	leq(Tmp,Makespan),
        %
	NInd is Ind-1,
        forall3_mj(NInd, NJobs, Index_m, Start,Duration,Makespan).
