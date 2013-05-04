:- module(_1,[append/3],[assertions,nativeprops,regtypes,ciaopp(tests(resources)),predefres(res_all),basicmodes]).

:- redefining(append/3).

:- doc(author,"Edison Mera").

:- doc(module,"This program appends two lists.").

:- resource res_steps.

:- resource calls_to_append_by_3.

:- resource res_steps_2.

:- head_cost(ub,res_steps,1).

:- literal_cost(ub,res_steps,0).

:- head_cost(lb,res_steps,1).

:- literal_cost(lb,res_steps,0).

:- head_cost(ub,calls_to_append_by_3,3).

:- literal_cost(ub,calls_to_append_by_3,3).

:- head_cost(ub,res_steps_2,1).

:- literal_cost(ub,res_steps_2,0).

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
         + ( cost(lb,giunif,length(Xs)+1), cost(lb,gounif,length(Xs)), cost(lb,nargs,3*length(Xs)+3), cost(lb,res_steps,length(Xs)+1), cost(lb,steps,length(Xs)+1), cost(lb,viunif,3*length(Xs)+1), cost(lb,vounif,2*length(Xs)+1) ).

:- true pred append(Xs,Ys,Zs)
         : ( list(Xs,num), list(Ys,num), var(Zs) )
        => ( list(Xs,num), list(Ys,num), list(Zs,num), size(ub,Xs,length(Xs)), size(ub,Ys,length(Ys)), size(ub,Zs,length(Ys)+length(Xs)) )
         + ( cost(ub,calls_to_append_by_3,6*length(Xs)+3), cost(ub,giunif,length(Xs)+1), cost(ub,gounif,length(Xs)), cost(ub,nargs,3*length(Xs)+3), cost(ub,res_steps,length(Xs)+1), cost(ub,res_steps_2,length(Xs)+1), cost(ub,steps,length(Xs)+1), cost(ub,viunif,3*length(Xs)+1), cost(ub,vounif,2*length(Xs)+1) ).

:- true pred append(Xs,Ys,Zs)
         : ( list(Xs,num), list(Ys,num), var(Zs) )
        => ( list(Xs,num), list(Ys,num), list(Zs,num), size_lb(Xs,length(Xs)), size_lb(Ys,length(Ys)), size_lb(Zs,length(Ys)+length(Xs)), size_ub(Xs,length(Xs)), size_ub(Ys,length(Ys)), size_ub(Zs,length(Ys)+length(Xs)) )
         + ( steps_lb(length(Xs)+1), steps_ub(length(Xs)+1) ).

append([],Y,Y).
append([X|Xs],Ys,[X|Zs]) :-
        append(Xs,Ys,Zs).


