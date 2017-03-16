:- module(_1,[hanoi/5],[assertions,regtypes,nativeprops,ciaopp(examples(resources(exectimehl))),basicmodes]).

:- export(elem/1).

:- prop elem/1+regtype.

:- trust comp hanoi(N,A,B,C,M)
         + ( size_metric(A,void), size_metric(B,void), size_metric(C,void) ).

:- entry hanoi(A,B,C,D,E)
         : ( num(A), elem(B), elem(C), elem(D), var(E) ).

:- true pred hanoi(N,A,B,C,M)
         : ( num(N), elem(A), elem(B), elem(C), term(M) )
        => ( num(N), elem(A), elem(B), elem(C), list(M,^(mv('hanoi_hlm:elem','hanoi_hlm:elem'))) ).

:- true pred hanoi(N,A,B,C,M)
         : ( mshare([[M]]), var(M), ground([N,A,B,C]) )
        => ground([N,A,B,C,M]).

:- true pred hanoi(N,A,B,C,M)
         : ( num(N), elem(A), elem(B), elem(C), var(M) )
        => ( num(N), elem(A), elem(B), elem(C), list(M,^(mv('hanoi_hlm:elem','hanoi_hlm:elem'))) )
         + ( not_fails, covered ).

:- true pred hanoi(N,A,B,C,M)
         : ( num(N), elem(A), elem(B), elem(C), var(M) )
        => ( num(N), elem(A), elem(B), elem(C), list(M,^(mv('hanoi_hlm:elem','hanoi_hlm:elem'))), size(lb,N,int(N)), size(lb,A,0), size(lb,B,0), size(lb,C,0), size(lb,M,exp(2,int(N))-1.0) )
         + cost(lb,exectime_model4,1083.116048373564*(exp(2,int(N)-1)*int(N))+1645.869551500905*exp(2,int(N))-1126.038445952107).

:- true pred hanoi(N,A,B,C,M)
         : ( num(N), elem(A), elem(B), elem(C), var(M) )
        => ( num(N), elem(A), elem(B), elem(C), list(M,^(mv('hanoi_hlm:elem','hanoi_hlm:elem'))), size(ub,N,int(N)), size(ub,A,0), size(ub,B,0), size(ub,C,0), size(ub,M,exp(2,int(N))-1.0) )
         + cost(ub,exectime_model4,1154.953145342745*(exp(2,int(N)-1)*int(N))+1765.634276719053*exp(2,int(N))-1201.620999862537).

hanoi(0,_A,_1,_C,[]) :- !.
hanoi(N,A,B,C,M) :-
        N1 is N-1,
        hanoi(N1,A,C,B,M1),
        hanoi(N1,B,A,C,M2),
        append(M1,[mv(A,C)],T),
        append(T,M2,M).

:- true pred append(_1,L,_2)
         : ( list(_1,^(mv('hanoi_hlm:elem','hanoi_hlm:elem'))), list(L,^(mv('hanoi_hlm:elem','hanoi_hlm:elem'))), term(_2) )
        => ( list(_1,^(mv('hanoi_hlm:elem','hanoi_hlm:elem'))), list(L,^(mv('hanoi_hlm:elem','hanoi_hlm:elem'))), list(_2,^(mv('hanoi_hlm:elem','hanoi_hlm:elem'))) ).

:- true pred append(_1,L,_2)
         : ( mshare([[_2]]), var(_2), ground([_1,L]) )
        => ground([_1,L,_2]).

:- true pred append(_1,L,_2)
         : ( list(_1,^(mv('hanoi_hlm:elem','hanoi_hlm:elem'))), list(L,^(mv('hanoi_hlm:elem','hanoi_hlm:elem'))), var(_2) )
        => ( list(_1,^(mv('hanoi_hlm:elem','hanoi_hlm:elem'))), list(L,^(mv('hanoi_hlm:elem','hanoi_hlm:elem'))), list(_2,^(mv('hanoi_hlm:elem','hanoi_hlm:elem'))) )
         + ( not_fails, covered ).

:- true pred append(_1,L,_2)
         : ( list(_1,^(mv('hanoi_hlm:elem','hanoi_hlm:elem'))), list(L,^(mv('hanoi_hlm:elem','hanoi_hlm:elem'))), var(_2) )
        => ( list(_1,^(mv('hanoi_hlm:elem','hanoi_hlm:elem'))), list(L,^(mv('hanoi_hlm:elem','hanoi_hlm:elem'))), list(_2,^(mv('hanoi_hlm:elem','hanoi_hlm:elem'))), size(lb,_1,length(_1)), size(lb,L,length(L)), size(lb,_2,length(L)+length(_1)) )
         + cost(lb,exectime_model4,541.558024186782*length(_1)+378.4291019696336).

:- true pred append(_1,L,_2)
         : ( list(_1,^(mv('hanoi_hlm:elem','hanoi_hlm:elem'))), list(L,^(mv('hanoi_hlm:elem','hanoi_hlm:elem'))), var(_2) )
        => ( list(_1,^(mv('hanoi_hlm:elem','hanoi_hlm:elem'))), list(L,^(mv('hanoi_hlm:elem','hanoi_hlm:elem'))), list(_2,^(mv('hanoi_hlm:elem','hanoi_hlm:elem'))), size(ub,_1,length(_1)), size(ub,L,length(L)), size(ub,_2,length(L)+length(_1)) )
         + cost(ub,exectime_model4,577.4765726713725*length(_1)+407.2549684374599).

append([],L,L).
append([X|L1],L2,[X|L3]) :-
        append(L1,L2,L3).

:- prop elem(_1)
         + regtype.

:- true pred elem(_1)
         : term(_1)
        => elem(_1).

:- true pred elem(_1)
         : mshare([[_1]])
        => ground([_1]).

:- true pred elem(_1)
         : term(_1)
        => elem(_1)
         + ( possibly_fails, not_covered ).

:- true pred elem(_1)
         : term(_1)
        => ( elem(_1), size(lb,_1,size(_1)) )
         + cost(lb,exectime_model4,0).

:- true pred elem(_1)
         : term(_1)
        => ( elem(_1), size(ub,_1,size(_1)) )
         + cost(ub,exectime_model4,286.6148910670593).

elem(a).
elem(b).
elem(c).


