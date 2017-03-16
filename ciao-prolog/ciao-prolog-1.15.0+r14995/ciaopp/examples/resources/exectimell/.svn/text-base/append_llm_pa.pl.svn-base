:- module(_1,[append/3],[assertions,nativeprops,regtypes,ciaopp(examples(resources(exectimell))),basicmodes]).

:- doc(author,"Edison Mera").

:- doc(module,"This program appends two lists.").

:- entry append(Xs,Ys,Zs)
         : ( list(Xs,num), list(Ys,num), var(Zs) ).

:- true pred append(Xs,Ys,Zs)
         : ( list(Xs,num), list(Ys,num), term(Zs) )
        => ( list(Xs,num), list(Ys,num), list(Zs,num) ).

:- true pred append(Xs,Ys,Zs)
         : ( mshare([[Zs]]), var(Zs), ground([Xs,Ys]) )
        => ground([Xs,Ys,Zs]).

:- true pred append(Xs,Ys,Zs)
         : ( list(Xs,num), list(Ys,num), var(Zs) )
        => ( list(Xs,num), list(Ys,num), list(Zs,num) )
         + ( not_fails, covered ).

:- true pred append(Xs,Ys,Zs)
         : ( list(Xs,num), list(Ys,num), var(Zs) )
        => ( list(Xs,num), list(Ys,num), list(Zs,num), size(lb,Xs,length(Xs)), size(lb,Ys,length(Ys)), size(lb,Zs,length(Ys)+length(Xs)) )
         + ( cost(lb,exectime,9930.623999999996*length(Xs)+2819.484), cost(lb,exectime_me,9930.623999999996*length(Xs)+2819.484), cost(lb,wamcount,26*length(Xs)+7) ).

:- true pred append(Xs,Ys,Zs)
         : ( list(Xs,num), list(Ys,num), var(Zs) )
        => ( list(Xs,num), list(Ys,num), list(Zs,num), size(ub,Xs,length(Xs)), size(ub,Ys,length(Ys)), size(ub,Zs,length(Ys)+length(Xs)) )
         + ( cost(ub,exectime,9930.623999999996*length(Xs)+2819.484), cost(ub,exectime_me,9930.623999999996*length(Xs)+2819.484), cost(ub,wamcount,26*length(Xs)+7) ).

append([],Y,Y).
append([X|Xs],Ys,[X|Zs]) :-
        append(Xs,Ys,Zs).


