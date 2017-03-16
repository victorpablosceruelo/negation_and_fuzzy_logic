:- module(_1,[main/5],[ciaopp(examples(resources(minizinc))),assertions,nativeprops,basicmodes,regtypes,isomodes]).

:- entry main(_1,_2,_3,_4,_5)
         : ( int(_1), int(_2), int(_3), array(_4), var(_5) ).

:- true pred main(Endtime,NMachines,NJobs,Duration,Start)
         : ( int(Endtime), int(NMachines), int(NJobs), array(Duration), term(Start) )
        => ( int(Endtime), int(NMachines), int(NJobs), array(Duration), array(Start) ).

:- true pred main(Endtime,NMachines,NJobs,Duration,Start)
         : ( mshare([[Start]]), var(Start), ground([Endtime,NMachines,NJobs,Duration]) )
        => ground([Endtime,NMachines,NJobs,Duration,Start]).

:- true pred main(Endtime,NMachines,NJobs,Duration,Start)
         : ( int(Endtime), int(NMachines), int(NJobs), array(Duration), var(Start) )
        => ( int(Endtime), int(NMachines), int(NJobs), array(Duration), array(Start) )
         + ( not_fails, covered ).

:- true pred main(Endtime,NMachines,NJobs,Duration,Start)
         : ( int(Endtime), int(NMachines), int(NJobs), array(Duration), var(Start) )
        => ( int(Endtime), int(NMachines), int(NJobs), array(Duration), array(Start), size(lb,Endtime,int(Endtime)), size(lb,NMachines,int(NMachines)), size(lb,NJobs,int(NJobs)), size(lb,Duration,size(Duration)), size(lb,Start,0) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,2*(int(NJobs)*int(NMachines))), cost(lb,numvars,int(NJobs)*int(NMachines)+1) ).

:- true pred main(Endtime,NMachines,NJobs,Duration,Start)
         : ( int(Endtime), int(NMachines), int(NJobs), array(Duration), var(Start) )
        => ( int(Endtime), int(NMachines), int(NJobs), array(Duration), array(Start), size(ub,Endtime,int(Endtime)), size(ub,NMachines,int(NMachines)), size(ub,NJobs,int(NJobs)), size(ub,Duration,size(Duration)), size(ub,Start,inf) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,5*(exp(int(NJobs),2)*int(NMachines))+5*(exp(int(NMachines),2)*int(NJobs))+2*(int(NJobs)*int(NMachines))), cost(ub,numvars,3*(exp(int(NJobs),2)*int(NMachines))+3*(exp(int(NMachines),2)*int(NJobs))+int(NJobs)*int(NMachines)+1) ).

main(Endtime,NMachines,NJobs,Duration,Start) :-
        minizinc_rt:(Size is NMachines-1+1),
        build_array(Start,Size),
        init_i(Size,0,Endtime,NJobs,Start),
        in(Makespan,0,Endtime),
        minizinc_rt:(Index1 is NMachines-1+1),
        forall1_m(Index1,NMachines,NJobs,Start,Duration),
        minizinc_rt:(Index2 is NJobs-1+1),
        forall2_j(Index2,NJobs,NMachines,Start,Duration),
        minizinc_rt:(Index3 is NMachines-1+1),
        forall3_m(Index3,NMachines,NJobs,Start,Duration,Makespan).

:- true pred init_i(Ind,_DL,_DU,_Uj,_A)
         : ( int(Ind), rt0(_DL), int(_DU), int(_Uj), array(_A) )
        => ( int(Ind), rt0(_DL), int(_DU), int(_Uj), array(_A) ).

:- true pred init_i(Ind,_DL,_DU,_Uj,_A)
         : ground([Ind,_DL,_DU,_Uj,_A])
        => ground([Ind,_DL,_DU,_Uj,_A]).

:- true pred init_i(Ind,_DL,_DU,_Uj,_A)
         : ( int(Ind), rt0(_DL), int(_DU), int(_Uj), array(_A) )
        => ( int(Ind), rt0(_DL), int(_DU), int(_Uj), array(_A) )
         + ( not_fails, covered ).

:- true pred init_i(Ind,_DL,_DU,_Uj,_A)
         : ( int(Ind), rt0(_DL), int(_DU), int(_Uj), array(_A) )
        => ( int(Ind), rt0(_DL), int(_DU), int(_Uj), array(_A), size(lb,Ind,int(Ind)), size(lb,_DL,int(_DL)), size(lb,_DU,int(_DU)), size(lb,_Uj,int(_Uj)), size(lb,_A,size(_A)) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,0), cost(lb,numvars,int(_Uj)*int(Ind)) ).

:- true pred init_i(Ind,_DL,_DU,_Uj,_A)
         : ( int(Ind), rt0(_DL), int(_DU), int(_Uj), array(_A) )
        => ( int(Ind), rt0(_DL), int(_DU), int(_Uj), array(_A), size(ub,Ind,int(Ind)), size(ub,_DL,int(_DL)), size(ub,_DU,int(_DU)), size(ub,_Uj,int(_Uj)), size(ub,_A,size(_A)) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,0), cost(ub,numvars,int(_Uj)*int(Ind)) ).

init_i(0,_DL,_DU,_Uj,_A) :- !.
init_i(Ind,DL,DU,Uj,A) :-
        element_array(Ind,A,Ai),
        minizinc_rt:(Size is Uj-1+1),
        build_array(Ai,Size),
        init_ij(Size,DL,DU,Ai),
        minizinc_rt:(NInd is Ind-1),
        init_i(NInd,DL,DU,Uj,A).

:- true pred init_ij(Ind,_DL,_DU,_A)
         : ( int(Ind), rt0(_DL), int(_DU), array(_A) )
        => ( int(Ind), rt0(_DL), int(_DU), array(_A) ).

:- true pred init_ij(Ind,_DL,_DU,_A)
         : ground([Ind,_DL,_DU,_A])
        => ground([Ind,_DL,_DU,_A]).

:- true pred init_ij(Ind,_DL,_DU,_A)
         : ( int(Ind), rt0(_DL), int(_DU), array(_A) )
        => ( int(Ind), rt0(_DL), int(_DU), array(_A) )
         + ( not_fails, covered ).

:- true pred init_ij(Ind,_DL,_DU,_A)
         : ( int(Ind), rt0(_DL), int(_DU), array(_A) )
        => ( int(Ind), rt0(_DL), int(_DU), array(_A), size(lb,Ind,int(Ind)), size(lb,_DL,int(_DL)), size(lb,_DU,int(_DU)), size(lb,_A,size(_A)) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,0), cost(lb,numvars,int(Ind)) ).

:- true pred init_ij(Ind,_DL,_DU,_A)
         : ( int(Ind), rt0(_DL), int(_DU), array(_A) )
        => ( int(Ind), rt0(_DL), int(_DU), array(_A), size(ub,Ind,int(Ind)), size(ub,_DL,int(_DL)), size(ub,_DU,int(_DU)), size(ub,_A,size(_A)) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,0), cost(ub,numvars,int(Ind)) ).

init_ij(0,_DL,_DU,_A) :- !.
init_ij(Ind,DL,DU,A) :-
        element_dint(Ind,A,Ai),
        in(Ai,DL,DU),
        minizinc_rt:(NInd is Ind-1),
        init_ij(NInd,DL,DU,A).

:- true pred not_at_the_same_time(M1,J1,M2,J2,Start,Duration)
         : ( int(M1), int(J1), int(M2), int(J2), array(Start), array(Duration) )
        => ( int(M1), int(J1), int(M2), int(J2), array(Start), array(Duration) ).

:- true pred not_at_the_same_time(M1,J1,M2,J2,Start,Duration)
         : ground([M1,J1,M2,J2,Start,Duration])
        => ground([M1,J1,M2,J2,Start,Duration]).

:- true pred not_at_the_same_time(M1,J1,M2,J2,Start,Duration)
         : ( int(M1), int(J1), int(M2), int(J2), array(Start), array(Duration) )
        => ( int(M1), int(J1), int(M2), int(J2), array(Start), array(Duration) )
         + ( not_fails, covered ).

:- true pred not_at_the_same_time(M1,J1,M2,J2,Start,Duration)
         : ( int(M1), int(J1), int(M2), int(J2), array(Start), array(Duration) )
        => ( int(M1), int(J1), int(M2), int(J2), array(Start), array(Duration), size(lb,M1,int(M1)), size(lb,J1,int(J1)), size(lb,M2,int(M2)), size(lb,J2,int(J2)), size(lb,Start,size(Start)), size(lb,Duration,size(Duration)) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,5), cost(lb,numvars,3) ).

:- true pred not_at_the_same_time(M1,J1,M2,J2,Start,Duration)
         : ( int(M1), int(J1), int(M2), int(J2), array(Start), array(Duration) )
        => ( int(M1), int(J1), int(M2), int(J2), array(Start), array(Duration), size(ub,M1,int(M1)), size(ub,J1,int(J1)), size(ub,M2,int(M2)), size(ub,J2,int(J2)), size(ub,Start,size(Start)), size(ub,Duration,size(Duration)) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,5), cost(ub,numvars,3) ).

not_at_the_same_time(M1,J1,M2,J2,Start,Duration) :-
        element2d_dint(M1,J1,Start,S1mj),
        element2d_dint(M1,J1,Duration,D1mj),
        element2d_dint(M2,J2,Start,S2mj),
        plus_dint_dint_var(S1mj,D1mj,Tmp2),
        reif_leq_dint_dint_var(Tmp2,S2mj,A),
        element2d_dint(M2,J2,Duration,D2mj),
        plus_dint_dint_var(S2mj,D2mj,Tmp1),
        reif_leq_dint_dint_var(Tmp1,S1mj,B),
        or(A,B,1).

:- true pred forall1_m(Ind,_1,_2,_3,_4)
         : ( int(Ind), int(_1), int(_2), array(_3), array(_4) )
        => ( int(Ind), int(_1), int(_2), array(_3), array(_4) ).

:- true pred forall1_m(Ind,_1,_2,_3,_4)
         : ground([Ind,_1,_2,_3,_4])
        => ground([Ind,_1,_2,_3,_4]).

:- true pred forall1_m(Ind,_1,_2,_3,_4)
         : ( int(Ind), int(_1), int(_2), array(_3), array(_4) )
        => ( int(Ind), int(_1), int(_2), array(_3), array(_4) )
         + ( not_fails, covered ).

:- true pred forall1_m(Ind,_1,_2,_3,_4)
         : ( int(Ind), int(_1), int(_2), array(_3), array(_4) )
        => ( int(Ind), int(_1), int(_2), array(_3), array(_4), size(lb,Ind,int(Ind)), size(lb,_1,int(_1)), size(lb,_2,int(_2)), size(lb,_3,size(_3)), size(lb,_4,size(_4)) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,0), cost(lb,numvars,0) ).

:- true pred forall1_m(Ind,_1,_2,_3,_4)
         : ( int(Ind), int(_1), int(_2), array(_3), array(_4) )
        => ( int(Ind), int(_1), int(_2), array(_3), array(_4), size(ub,Ind,int(Ind)), size(ub,_1,int(_1)), size(ub,_2,int(_2)), size(ub,_3,size(_3)), size(ub,_4,size(_4)) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,5*(exp(int(_2),2)*int(Ind))), cost(ub,numvars,3*(exp(int(_2),2)*int(Ind))) ).

forall1_m(0,_1,_2,_3,_4) :- !.
forall1_m(Ind,NMachines,NJobs,Start,Duration) :-
        minizinc_rt:(Index_m is NMachines-Ind+1),
        minizinc_rt:(Ind_j1 is NJobs-1+1),
        forall1_mj1(Ind_j1,NJobs,Index_m,Start,Duration),
        minizinc_rt:(NInd is Ind-1),
        forall1_m(NInd,NMachines,NJobs,Start,Duration).

:- true pred forall1_mj1(Ind,_1,_2,_3,_4)
         : ( int(Ind), int(_1), int(_2), array(_3), array(_4) )
        => ( int(Ind), int(_1), int(_2), array(_3), array(_4) ).

:- true pred forall1_mj1(Ind,_1,_2,_3,_4)
         : ground([Ind,_1,_2,_3,_4])
        => ground([Ind,_1,_2,_3,_4]).

:- true pred forall1_mj1(Ind,_1,_2,_3,_4)
         : ( int(Ind), int(_1), int(_2), array(_3), array(_4) )
        => ( int(Ind), int(_1), int(_2), array(_3), array(_4) )
         + ( not_fails, covered ).

:- true pred forall1_mj1(Ind,_1,_2,_3,_4)
         : ( int(Ind), int(_1), int(_2), array(_3), array(_4) )
        => ( int(Ind), int(_1), int(_2), array(_3), array(_4), size(lb,Ind,int(Ind)), size(lb,_1,int(_1)), size(lb,_2,int(_2)), size(lb,_3,size(_3)), size(lb,_4,size(_4)) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,0), cost(lb,numvars,0) ).

:- true pred forall1_mj1(Ind,_1,_2,_3,_4)
         : ( int(Ind), int(_1), int(_2), array(_3), array(_4) )
        => ( int(Ind), int(_1), int(_2), array(_3), array(_4), size(ub,Ind,int(Ind)), size(ub,_1,int(_1)), size(ub,_2,int(_2)), size(ub,_3,size(_3)), size(ub,_4,size(_4)) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,5*(int(_1)*int(Ind))), cost(ub,numvars,3*(int(_1)*int(Ind))) ).

forall1_mj1(0,_1,_2,_3,_4) :- !.
forall1_mj1(Ind,NJobs,Index_m,Start,Duration) :-
        minizinc_rt:(Index_j1 is NJobs-Ind+1),
        minizinc_rt:(Ind_j2 is NJobs-1+1),
        forall1_mj1j2(Ind_j2,NJobs,Index_m,Index_j1,Start,Duration),
        minizinc_rt:(NInd is Ind-1),
        forall1_mj1(NInd,NJobs,Index_m,Start,Duration).

:- true pred forall1_mj1j2(Ind,_1,_2,_3,_4,_5)
         : ( int(Ind), int(_1), int(_2), int(_3), array(_4), array(_5) )
        => ( int(Ind), int(_1), int(_2), int(_3), array(_4), array(_5) ).

:- true pred forall1_mj1j2(Ind,_1,_2,_3,_4,_5)
         : ground([Ind,_1,_2,_3,_4,_5])
        => ground([Ind,_1,_2,_3,_4,_5]).

:- true pred forall1_mj1j2(Ind,_1,_2,_3,_4,_5)
         : ( int(Ind), int(_1), int(_2), int(_3), array(_4), array(_5) )
        => ( int(Ind), int(_1), int(_2), int(_3), array(_4), array(_5) )
         + ( not_fails, covered ).

:- true pred forall1_mj1j2(Ind,_1,_2,_3,_4,_5)
         : ( int(Ind), int(_1), int(_2), int(_3), array(_4), array(_5) )
        => ( int(Ind), int(_1), int(_2), int(_3), array(_4), array(_5), size(lb,Ind,int(Ind)), size(lb,_1,int(_1)), size(lb,_2,int(_2)), size(lb,_3,int(_3)), size(lb,_4,size(_4)), size(lb,_5,size(_5)) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,0), cost(lb,numvars,0) ).

:- true pred forall1_mj1j2(Ind,_1,_2,_3,_4,_5)
         : ( int(Ind), int(_1), int(_2), int(_3), array(_4), array(_5) )
        => ( int(Ind), int(_1), int(_2), int(_3), array(_4), array(_5), size(ub,Ind,int(Ind)), size(ub,_1,int(_1)), size(ub,_2,int(_2)), size(ub,_3,int(_3)), size(ub,_4,size(_4)), size(ub,_5,size(_5)) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,5*int(Ind)), cost(ub,numvars,3*int(Ind)) ).

forall1_mj1j2(0,_1,_2,_3,_4,_5) :- !.
forall1_mj1j2(Ind,NJobs,Index_m,Index_j1,Start,Duration) :-
        minizinc_rt:(Index_j2 is NJobs-Ind+1),
        minizinc_rt:(M is Index_m-1+1),
        minizinc_rt:(J1 is Index_j1-1+1),
        minizinc_rt:(J2 is Index_j2-1+1),
        'forall1_mj1j2/6/2/$disj/1'(Start,Duration,M,J1,J2),
        minizinc_rt:(NInd is Ind-1),
        forall1_mj1j2(NInd,NJobs,Index_m,Index_j1,Start,Duration).

:- true pred 'forall1_mj1j2/6/2/$disj/1'(Start,Duration,M,J1,J2)
         : ( array(Start), array(Duration), int(M), int(J1), int(J2) )
        => ( array(Start), array(Duration), int(M), int(J1), int(J2) ).

:- true pred 'forall1_mj1j2/6/2/$disj/1'(Start,Duration,M,J1,J2)
         : ground([Start,Duration,M,J1,J2])
        => ground([Start,Duration,M,J1,J2]).

:- true pred 'forall1_mj1j2/6/2/$disj/1'(Start,Duration,M,J1,J2)
         : ( array(Start), array(Duration), int(M), int(J1), int(J2) )
        => ( array(Start), array(Duration), int(M), int(J1), int(J2) )
         + ( not_fails, covered ).

:- true pred 'forall1_mj1j2/6/2/$disj/1'(Start,Duration,M,J1,J2)
         : ( array(Start), array(Duration), int(M), int(J1), int(J2) )
        => ( array(Start), array(Duration), int(M), int(J1), int(J2), size(lb,Start,size(Start)), size(lb,Duration,size(Duration)), size(lb,M,int(M)), size(lb,J1,int(J1)), size(lb,J2,int(J2)) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,0), cost(lb,numvars,0) ).

:- true pred 'forall1_mj1j2/6/2/$disj/1'(Start,Duration,M,J1,J2)
         : ( array(Start), array(Duration), int(M), int(J1), int(J2) )
        => ( array(Start), array(Duration), int(M), int(J1), int(J2), size(ub,Start,size(Start)), size(ub,Duration,size(Duration)), size(ub,M,int(M)), size(ub,J1,int(J1)), size(ub,J2,int(J2)) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,5), cost(ub,numvars,3) ).

'forall1_mj1j2/6/2/$disj/1'(Start,Duration,M,J1,J2) :-
        less(J1,J2),
        !,
        not_at_the_same_time(M,J1,M,J2,Start,Duration).
'forall1_mj1j2/6/2/$disj/1'(Start,Duration,M,J1,J2).

:- true pred forall2_j(Ind,_1,_2,_3,_4)
         : ( int(Ind), int(_1), int(_2), array(_3), array(_4) )
        => ( int(Ind), int(_1), int(_2), array(_3), array(_4) ).

:- true pred forall2_j(Ind,_1,_2,_3,_4)
         : ground([Ind,_1,_2,_3,_4])
        => ground([Ind,_1,_2,_3,_4]).

:- true pred forall2_j(Ind,_1,_2,_3,_4)
         : ( int(Ind), int(_1), int(_2), array(_3), array(_4) )
        => ( int(Ind), int(_1), int(_2), array(_3), array(_4) )
         + ( not_fails, covered ).

:- true pred forall2_j(Ind,_1,_2,_3,_4)
         : ( int(Ind), int(_1), int(_2), array(_3), array(_4) )
        => ( int(Ind), int(_1), int(_2), array(_3), array(_4), size(lb,Ind,int(Ind)), size(lb,_1,int(_1)), size(lb,_2,int(_2)), size(lb,_3,size(_3)), size(lb,_4,size(_4)) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,0), cost(lb,numvars,0) ).

:- true pred forall2_j(Ind,_1,_2,_3,_4)
         : ( int(Ind), int(_1), int(_2), array(_3), array(_4) )
        => ( int(Ind), int(_1), int(_2), array(_3), array(_4), size(ub,Ind,int(Ind)), size(ub,_1,int(_1)), size(ub,_2,int(_2)), size(ub,_3,size(_3)), size(ub,_4,size(_4)) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,5*(exp(int(_2),2)*int(Ind))), cost(ub,numvars,3*(exp(int(_2),2)*int(Ind))) ).

forall2_j(0,_1,_2,_3,_4) :- !.
forall2_j(Ind,NJobs,NMachines,Start,Duration) :-
        minizinc_rt:(Index_j is NJobs-Ind+1),
        minizinc_rt:(Ind_m1 is NMachines-1+1),
        forall2_jm1(Ind_m1,NMachines,Index_j,Start,Duration),
        minizinc_rt:(NInd is Ind-1),
        forall2_j(NInd,NJobs,NMachines,Start,Duration).

:- true pred forall2_jm1(Ind,_1,_2,_3,_4)
         : ( int(Ind), int(_1), int(_2), array(_3), array(_4) )
        => ( int(Ind), int(_1), int(_2), array(_3), array(_4) ).

:- true pred forall2_jm1(Ind,_1,_2,_3,_4)
         : ground([Ind,_1,_2,_3,_4])
        => ground([Ind,_1,_2,_3,_4]).

:- true pred forall2_jm1(Ind,_1,_2,_3,_4)
         : ( int(Ind), int(_1), int(_2), array(_3), array(_4) )
        => ( int(Ind), int(_1), int(_2), array(_3), array(_4) )
         + ( not_fails, covered ).

:- true pred forall2_jm1(Ind,_1,_2,_3,_4)
         : ( int(Ind), int(_1), int(_2), array(_3), array(_4) )
        => ( int(Ind), int(_1), int(_2), array(_3), array(_4), size(lb,Ind,int(Ind)), size(lb,_1,int(_1)), size(lb,_2,int(_2)), size(lb,_3,size(_3)), size(lb,_4,size(_4)) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,0), cost(lb,numvars,0) ).

:- true pred forall2_jm1(Ind,_1,_2,_3,_4)
         : ( int(Ind), int(_1), int(_2), array(_3), array(_4) )
        => ( int(Ind), int(_1), int(_2), array(_3), array(_4), size(ub,Ind,int(Ind)), size(ub,_1,int(_1)), size(ub,_2,int(_2)), size(ub,_3,size(_3)), size(ub,_4,size(_4)) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,5*(int(_1)*int(Ind))), cost(ub,numvars,3*(int(_1)*int(Ind))) ).

forall2_jm1(0,_1,_2,_3,_4) :- !.
forall2_jm1(Ind,NMachines,Index_j,Start,Duration) :-
        minizinc_rt:(Index_m1 is NMachines-Ind+1),
        minizinc_rt:(Ind_m2 is NMachines-1+1),
        forall2_jm1m2(Ind_m2,NMachines,Index_j,Index_m1,Start,Duration),
        minizinc_rt:(NInd is Ind-1),
        forall2_jm1(NInd,NMachines,Index_j,Start,Duration).

:- true pred forall2_jm1m2(Ind,_1,_2,_3,_4,_5)
         : ( int(Ind), int(_1), int(_2), int(_3), array(_4), array(_5) )
        => ( int(Ind), int(_1), int(_2), int(_3), array(_4), array(_5) ).

:- true pred forall2_jm1m2(Ind,_1,_2,_3,_4,_5)
         : ground([Ind,_1,_2,_3,_4,_5])
        => ground([Ind,_1,_2,_3,_4,_5]).

:- true pred forall2_jm1m2(Ind,_1,_2,_3,_4,_5)
         : ( int(Ind), int(_1), int(_2), int(_3), array(_4), array(_5) )
        => ( int(Ind), int(_1), int(_2), int(_3), array(_4), array(_5) )
         + ( not_fails, covered ).

:- true pred forall2_jm1m2(Ind,_1,_2,_3,_4,_5)
         : ( int(Ind), int(_1), int(_2), int(_3), array(_4), array(_5) )
        => ( int(Ind), int(_1), int(_2), int(_3), array(_4), array(_5), size(lb,Ind,int(Ind)), size(lb,_1,int(_1)), size(lb,_2,int(_2)), size(lb,_3,int(_3)), size(lb,_4,size(_4)), size(lb,_5,size(_5)) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,0), cost(lb,numvars,0) ).

:- true pred forall2_jm1m2(Ind,_1,_2,_3,_4,_5)
         : ( int(Ind), int(_1), int(_2), int(_3), array(_4), array(_5) )
        => ( int(Ind), int(_1), int(_2), int(_3), array(_4), array(_5), size(ub,Ind,int(Ind)), size(ub,_1,int(_1)), size(ub,_2,int(_2)), size(ub,_3,int(_3)), size(ub,_4,size(_4)), size(ub,_5,size(_5)) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,5*int(Ind)), cost(ub,numvars,3*int(Ind)) ).

forall2_jm1m2(0,_1,_2,_3,_4,_5) :- !.
forall2_jm1m2(Ind,NMachines,Index_j,Index_m1,Start,Duration) :-
        minizinc_rt:(Index_m2 is NMachines-Ind+1),
        minizinc_rt:(J is Index_j-1+1),
        minizinc_rt:(M1 is Index_m1-1+1),
        minizinc_rt:(M2 is Index_m2-1+1),
        'forall2_jm1m2/6/2/$disj/1'(Start,Duration,J,M1,M2),
        minizinc_rt:(NInd is Ind-1),
        forall2_jm1m2(NInd,NMachines,Index_j,Index_m1,Start,Duration).

:- true pred 'forall2_jm1m2/6/2/$disj/1'(Start,Duration,J,M1,M2)
         : ( array(Start), array(Duration), int(J), int(M1), int(M2) )
        => ( array(Start), array(Duration), int(J), int(M1), int(M2) ).

:- true pred 'forall2_jm1m2/6/2/$disj/1'(Start,Duration,J,M1,M2)
         : ground([Start,Duration,J,M1,M2])
        => ground([Start,Duration,J,M1,M2]).

:- true pred 'forall2_jm1m2/6/2/$disj/1'(Start,Duration,J,M1,M2)
         : ( array(Start), array(Duration), int(J), int(M1), int(M2) )
        => ( array(Start), array(Duration), int(J), int(M1), int(M2) )
         + ( not_fails, covered ).

:- true pred 'forall2_jm1m2/6/2/$disj/1'(Start,Duration,J,M1,M2)
         : ( array(Start), array(Duration), int(J), int(M1), int(M2) )
        => ( array(Start), array(Duration), int(J), int(M1), int(M2), size(lb,Start,size(Start)), size(lb,Duration,size(Duration)), size(lb,J,int(J)), size(lb,M1,int(M1)), size(lb,M2,int(M2)) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,0), cost(lb,numvars,0) ).

:- true pred 'forall2_jm1m2/6/2/$disj/1'(Start,Duration,J,M1,M2)
         : ( array(Start), array(Duration), int(J), int(M1), int(M2) )
        => ( array(Start), array(Duration), int(J), int(M1), int(M2), size(ub,Start,size(Start)), size(ub,Duration,size(Duration)), size(ub,J,int(J)), size(ub,M1,int(M1)), size(ub,M2,int(M2)) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,5), cost(ub,numvars,3) ).

'forall2_jm1m2/6/2/$disj/1'(Start,Duration,J,M1,M2) :-
        less(M1,M2),
        !,
        not_at_the_same_time(M1,J,M2,J,Start,Duration).
'forall2_jm1m2/6/2/$disj/1'(Start,Duration,J,M1,M2).

:- true pred forall3_m(Ind,_1,_2,_3,_4,_5)
         : ( int(Ind), int(_1), int(_2), array(_3), array(_4), dint(_5) )
        => ( int(Ind), int(_1), int(_2), array(_3), array(_4), dint(_5) ).

:- true pred forall3_m(Ind,_1,_2,_3,_4,_5)
         : ground([Ind,_1,_2,_3,_4,_5])
        => ground([Ind,_1,_2,_3,_4,_5]).

:- true pred forall3_m(Ind,_1,_2,_3,_4,_5)
         : ( int(Ind), int(_1), int(_2), array(_3), array(_4), dint(_5) )
        => ( int(Ind), int(_1), int(_2), array(_3), array(_4), dint(_5) )
         + ( not_fails, covered ).

:- true pred forall3_m(Ind,_1,_2,_3,_4,_5)
         : ( int(Ind), int(_1), int(_2), array(_3), array(_4), dint(_5) )
        => ( int(Ind), int(_1), int(_2), array(_3), array(_4), dint(_5), size(lb,Ind,int(Ind)), size(lb,_1,int(_1)), size(lb,_2,int(_2)), size(lb,_3,size(_3)), size(lb,_4,size(_4)), size(lb,_5,size(_5)) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,2*(int(_2)*int(Ind))), cost(lb,numvars,0) ).

:- true pred forall3_m(Ind,_1,_2,_3,_4,_5)
         : ( int(Ind), int(_1), int(_2), array(_3), array(_4), dint(_5) )
        => ( int(Ind), int(_1), int(_2), array(_3), array(_4), dint(_5), size(ub,Ind,int(Ind)), size(ub,_1,int(_1)), size(ub,_2,int(_2)), size(ub,_3,size(_3)), size(ub,_4,size(_4)), size(ub,_5,size(_5)) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,2*(int(_2)*int(Ind))), cost(ub,numvars,0) ).

forall3_m(0,_1,_2,_3,_4,_5) :- !.
forall3_m(Ind,NMachines,NJobs,Start,Duration,Makespan) :-
        minizinc_rt:(Index_m is NMachines-Ind+1),
        minizinc_rt:(Ind_j is NJobs-1+1),
        forall3_mj(Ind_j,NJobs,Index_m,Start,Duration,Makespan),
        minizinc_rt:(NInd is Ind-1),
        forall3_m(NInd,NMachines,NJobs,Start,Duration,Makespan).

:- true pred forall3_mj(Ind,_1,_2,_3,_4,_5)
         : ( int(Ind), int(_1), int(_2), array(_3), array(_4), dint(_5) )
        => ( int(Ind), int(_1), int(_2), array(_3), array(_4), dint(_5) ).

:- true pred forall3_mj(Ind,_1,_2,_3,_4,_5)
         : ground([Ind,_1,_2,_3,_4,_5])
        => ground([Ind,_1,_2,_3,_4,_5]).

:- true pred forall3_mj(Ind,_1,_2,_3,_4,_5)
         : ( int(Ind), int(_1), int(_2), array(_3), array(_4), dint(_5) )
        => ( int(Ind), int(_1), int(_2), array(_3), array(_4), dint(_5) )
         + ( not_fails, covered ).

:- true pred forall3_mj(Ind,_1,_2,_3,_4,_5)
         : ( int(Ind), int(_1), int(_2), array(_3), array(_4), dint(_5) )
        => ( int(Ind), int(_1), int(_2), array(_3), array(_4), dint(_5), size(lb,Ind,int(Ind)), size(lb,_1,int(_1)), size(lb,_2,int(_2)), size(lb,_3,size(_3)), size(lb,_4,size(_4)), size(lb,_5,size(_5)) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,2*int(Ind)), cost(lb,numvars,0) ).

:- true pred forall3_mj(Ind,_1,_2,_3,_4,_5)
         : ( int(Ind), int(_1), int(_2), array(_3), array(_4), dint(_5) )
        => ( int(Ind), int(_1), int(_2), array(_3), array(_4), dint(_5), size(ub,Ind,int(Ind)), size(ub,_1,int(_1)), size(ub,_2,int(_2)), size(ub,_3,size(_3)), size(ub,_4,size(_4)), size(ub,_5,size(_5)) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,2*int(Ind)), cost(ub,numvars,0) ).

forall3_mj(0,_1,_2,_3,_4,_5) :- !.
forall3_mj(Ind,NJobs,Index_m,Start,Duration,Makespan) :-
        minizinc_rt:(Index_j is NJobs-Ind+1),
        minizinc_rt:(M is Index_m-1+1),
        minizinc_rt:(J is Index_j-1+1),
        element2d_dint(M,J,Start,Smj),
        element2d_dint(M,J,Duration,Dmj),
        plus_dint_dint_var(Smj,Dmj,Tmp),
        leq(Tmp,Makespan),
        minizinc_rt:(NInd is Ind-1),
        forall3_mj(NInd,NJobs,Index_m,Start,Duration,Makespan).


:- regtype rt0/1.

rt0(0).


