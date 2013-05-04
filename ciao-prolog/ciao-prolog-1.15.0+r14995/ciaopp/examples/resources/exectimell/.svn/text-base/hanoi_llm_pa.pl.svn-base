:- module(_1,[hanoi/5],[assertions,regtypes,ciaopp(examples(resources(exectimell))),nativeprops,basicmodes]).

:- doc(author,"Edison Mera").

:- export(elem/1).

:- prop elem/1+regtype.

:- trust comp hanoi(N,A,B,C,M)
         + ( size_metric(A,void), size_metric(B,void), size_metric(C,void) ).

:- entry hanoi(A,B,C,D,E)
         : ( num(A), elem(B), elem(C), elem(D), var(E) ).

:- true pred hanoi(N,A,B,C,M)
         : ( num(N), elem(A), elem(B), elem(C), term(M) )
        => ( num(N), elem(A), elem(B), elem(C), rt16(M) ).

:- true pred hanoi(N,A,B,C,M)
         : ( mshare([[M]]), var(M), ground([N,A,B,C]) )
        => ground([N,A,B,C,M]).

:- true pred hanoi(N,A,B,C,M)
         : ( num(N), elem(A), elem(B), elem(C), var(M) )
        => ( num(N), elem(A), elem(B), elem(C), rt16(M) )
         + ( not_fails, covered ).

:- true pred hanoi(N,A,B,C,M)
         : ( num(N), elem(A), elem(B), elem(C), var(M) )
        => ( num(N), elem(A), elem(B), elem(C), rt16(M), size(lb,N,int(N)), size(lb,A,0), size(lb,B,0), size(lb,C,0), size(lb,M,exp(2,int(N))-1.0) )
         + ( cost(lb,exectime,19861.24799999999*(exp(2,int(N)-1)*int(N))-11763.67199999999*exp(2,int(N)-1)+5743.278000000001*exp(2,int(N))-11486.556), cost(lb,exectime_me,19861.24799999999*(exp(2,int(N)-1)*int(N))-11763.67199999999*exp(2,int(N)-1)+5743.278000000001*exp(2,int(N))-11486.556), cost(lb,wamcount,52*(exp(2,int(N)-1)*int(N))-32*exp(2,int(N)-1)+16.5*exp(2,int(N))-33.0) ).

:- true pred hanoi(N,A,B,C,M)
         : ( num(N), elem(A), elem(B), elem(C), var(M) )
        => ( num(N), elem(A), elem(B), elem(C), rt16(M), size(ub,N,int(N)), size(ub,A,0), size(ub,B,0), size(ub,C,0), size(ub,M,exp(2,int(N))-1.0) )
         + ( cost(ub,exectime,19861.24799999999*(exp(2,int(N)-1)*int(N))-11763.67199999999*exp(2,int(N)-1)+5743.278000000001*exp(2,int(N))-11486.556), cost(ub,exectime_me,19861.24799999999*(exp(2,int(N)-1)*int(N))-11763.67199999999*exp(2,int(N)-1)+5743.278000000001*exp(2,int(N))-11486.556), cost(ub,wamcount,52*(exp(2,int(N)-1)*int(N))-32*exp(2,int(N)-1)+16.5*exp(2,int(N))-33.0) ).

hanoi(1,A,_1,C,[mv(A,C)]) :- !.
hanoi(N,A,B,C,M) :-
        N1 is N-1,
        hanoi(N1,A,C,B,M1),
        hanoi(N1,B,A,C,M2),
        append(M1,[mv(A,C)],T),
        append(T,M2,M).

:- true pred append(_1,L,_2)
         : ( list(_1,^(mv('hanoi_llm:elem','hanoi_llm:elem'))), rt16(L), term(_2) )
        => ( list(_1,^(mv('hanoi_llm:elem','hanoi_llm:elem'))), rt16(L), rt16(_2) ).

:- true pred append(_1,L,_2)
         : ( mshare([[_2]]), var(_2), ground([_1,L]) )
        => ground([_1,L,_2]).

:- true pred append(_1,L,_2)
         : ( list(_1,^(mv('hanoi_llm:elem','hanoi_llm:elem'))), rt16(L), var(_2) )
        => ( list(_1,^(mv('hanoi_llm:elem','hanoi_llm:elem'))), rt16(L), rt16(_2) )
         + ( not_fails, covered ).

:- true pred append(_1,L,_2)
         : ( list(_1,^(mv('hanoi_llm:elem','hanoi_llm:elem'))), rt16(L), var(_2) )
        => ( list(_1,^(mv('hanoi_llm:elem','hanoi_llm:elem'))), rt16(L), rt16(_2), size(lb,_1,length(_1)), size(lb,L,length(L)), size(lb,_2,length(L)+length(_1)) )
         + ( cost(lb,exectime,9930.623999999996*length(_1)+2819.484), cost(lb,exectime_me,9930.623999999996*length(_1)+2819.484), cost(lb,wamcount,26*length(_1)+7) ).

:- true pred append(_1,L,_2)
         : ( list(_1,^(mv('hanoi_llm:elem','hanoi_llm:elem'))), rt16(L), var(_2) )
        => ( list(_1,^(mv('hanoi_llm:elem','hanoi_llm:elem'))), rt16(L), rt16(_2), size(ub,_1,length(_1)), size(ub,L,length(L)), size(ub,_2,length(L)+length(_1)) )
         + ( cost(ub,exectime,9930.623999999996*length(_1)+2819.484), cost(ub,exectime_me,9930.623999999996*length(_1)+2819.484), cost(ub,wamcount,26*length(_1)+7) ).

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
         + ( cost(lb,exectime,0), cost(lb,exectime_me,0), cost(lb,wamcount,0) ).

:- true pred elem(_1)
         : term(_1)
        => ( elem(_1), size(ub,_1,size(_1)) )
         + ( cost(ub,exectime,2922.216), cost(ub,exectime_me,2922.216), cost(ub,wamcount,7) ).

elem(a).
elem(b).
elem(c).


:- regtype rt16/1.

rt16([mv(A,B)|C]) :-
        elem(A),
        elem(B),
        list(C,^mv('hanoi_llm:elem','hanoi_llm:elem')).


