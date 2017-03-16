:- module(_1,[palindro/2],[assertions,regtypes,ciaopp(examples(resources(exectimell))),nativeprops,basicmodes]).

:- entry palindro(_1,_2)
         : ( list(_1,int), var(_2) ).

:- true pred palindro(_1,L2)
         : ( list(_1,int), term(L2) )
        => ( list(_1,int), list(L2,int) ).

:- true pred palindro(_1,L2)
         : ( mshare([[L2]]), var(L2), ground([_1]) )
        => ground([_1,L2]).

:- true pred palindro(_1,L2)
         : ( list(_1,int), var(L2) )
        => ( list(_1,int), list(L2,int) )
         + ( not_fails, covered ).

:- true pred palindro(_1,L2)
         : ( list(_1,int), var(L2) )
        => ( list(_1,int), list(L2,int), size(lb,_1,length(_1)), size(lb,L2,exp(2,length(_1))-1.0) )
         + ( cost(lb,exectime,9930.623999999996*(exp(2,length(_1)-1)*length(_1))+4568.652000000005*exp(2,length(_1))-2975.916000000005), cost(lb,exectime_me,9930.623999999996*(exp(2,length(_1)-1)*length(_1))+4568.652000000005*exp(2,length(_1))-2975.916000000005), cost(lb,wamcount,26*(exp(2,length(_1)-1)*length(_1))+15.0*exp(2,length(_1))-11.0) ).

:- true pred palindro(_1,L2)
         : ( list(_1,int), var(L2) )
        => ( list(_1,int), list(L2,int), size(ub,_1,length(_1)), size(ub,L2,exp(2,length(_1))-1.0) )
         + ( cost(ub,exectime,9930.623999999996*(exp(2,length(_1)-1)*length(_1))+4568.652000000005*exp(2,length(_1))-2975.916000000005), cost(ub,exectime_me,9930.623999999996*(exp(2,length(_1)-1)*length(_1))+4568.652000000005*exp(2,length(_1))-2975.916000000005), cost(ub,wamcount,26*(exp(2,length(_1)-1)*length(_1))+15.0*exp(2,length(_1))-11.0) ).

palindro([],[]).
palindro([First|L1],L2) :-
        palindro(L1,Ls2),
        palindro(L1,Lg2),
        append(Ls2,[First|Lg2],L2).

:- true pred append(_1,L,_2)
         : ( list(_1,int), rt20(L), term(_2) )
        => ( list(_1,int), rt20(L), rt20(_2) ).

:- true pred append(_1,L,_2)
         : ( mshare([[_2]]), var(_2), ground([_1,L]) )
        => ground([_1,L,_2]).

:- true pred append(_1,L,_2)
         : ( list(_1,int), rt20(L), var(_2) )
        => ( list(_1,int), rt20(L), rt20(_2) )
         + ( not_fails, covered ).

:- true pred append(_1,L,_2)
         : ( list(_1,int), rt20(L), var(_2) )
        => ( list(_1,int), rt20(L), rt20(_2), size(lb,_1,length(_1)), size(lb,L,length(L)), size(lb,_2,length(L)+length(_1)) )
         + ( cost(lb,exectime,9930.623999999996*length(_1)+2819.484), cost(lb,exectime_me,9930.623999999996*length(_1)+2819.484), cost(lb,wamcount,26*length(_1)+7) ).

:- true pred append(_1,L,_2)
         : ( list(_1,int), rt20(L), var(_2) )
        => ( list(_1,int), rt20(L), rt20(_2), size(ub,_1,length(_1)), size(ub,L,length(L)), size(ub,_2,length(L)+length(_1)) )
         + ( cost(ub,exectime,9930.623999999996*length(_1)+2819.484), cost(ub,exectime_me,9930.623999999996*length(_1)+2819.484), cost(ub,wamcount,26*length(_1)+7) ).

append([],L,L).
append([H|L],L1,[H|R]) :-
        append(L,L1,R).


:- regtype rt20/1.

rt20([A|B]) :-
        int(A),
        list(B,int).


