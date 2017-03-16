:- module(_1,[append/3],[assertions,nativeprops,regtypes,ciaopp(tests(resources)),res_exectime_hlm(auto(res_exectime_hlm_63)),basicmodes]).

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
         + cost(lb,exectime_hlm_63,541.558024186782*length(Xs)+378.4291019696336).

:- true pred append(Xs,Ys,Zs)
         : ( list(Xs,num), list(Ys,num), var(Zs) )
        => ( list(Xs,num), list(Ys,num), list(Zs,num), size(ub,Xs,length(Xs)), size(ub,Ys,length(Ys)), size(ub,Zs,length(Ys)+length(Xs)) )
         + cost(ub,exectime_hlm_63,577.4765726713725*length(Xs)+407.2549684374599).

:- true pred append(Xs,Ys,Zs)
         : ( list(Xs,num), list(Ys,num), var(Zs) )
        => ( list(Xs,num), list(Ys,num), list(Zs,num), size_lb(Xs,length(Xs)), size_lb(Ys,length(Ys)), size_lb(Zs,length(Ys)+length(Xs)), size_ub(Xs,length(Xs)), size_ub(Ys,length(Ys)), size_ub(Zs,length(Ys)+length(Xs)) )
         + ( steps_lb(length(Xs)+1), steps_ub(length(Xs)+1) ).

append([],Y,Y).
append([X|Xs],Ys,[X|Zs]) :-
        append(Xs,Ys,Zs).


