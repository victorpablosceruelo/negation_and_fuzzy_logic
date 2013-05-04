:- module(_1,[append/3],[assertions,nativeprops,regtypes,ciaopp(tests(resources)),predefres(res_all),basicmodes]).

:- redefining(append/3).

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
         + ( cost(lb,giunif,length(Xs)+1), cost(lb,gounif,length(Xs)), cost(lb,nargs,3*length(Xs)+3), cost(lb,steps,length(Xs)+1), cost(lb,viunif,3*length(Xs)+1), cost(lb,vounif,2*length(Xs)+1) ).

:- true pred append(Xs,Ys,Zs)
         : ( list(Xs,num), list(Ys,num), var(Zs) )
        => ( list(Xs,num), list(Ys,num), list(Zs,num), size(ub,Xs,length(Xs)), size(ub,Ys,length(Ys)), size(ub,Zs,length(Ys)+length(Xs)) )
         + ( cost(ub,giunif,length(Xs)+1), cost(ub,gounif,length(Xs)), cost(ub,nargs,3*length(Xs)+3), cost(ub,steps,length(Xs)+1), cost(ub,viunif,3*length(Xs)+1), cost(ub,vounif,2*length(Xs)+1) ).

append([],Y,Y).
append([X|Xs],Ys,[X|Zs]) :-
        append(Xs,Ys,Zs).


