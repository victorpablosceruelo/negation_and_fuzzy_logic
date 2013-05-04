:- module(_1,[reverse/2],[assertions,regtypes,ciaopp(tests(resources)),predefres(res_all),nativeprops,basicmodes]).

:- resource res_steps.

:- literal_cost(ub,res_steps,0).

:- trust comp reverse(X,Y)
         + head_cost(ub,res_steps,1).

:- entry reverse(Xs,Ys)
         : ( var(Ys), list(Xs,num) ).

:- true pred reverse(X,Y)
         : ( list(X,num), term(Y) )
        => ( list(X,num), list(Y,num) ).

:- true pred reverse(X,Y)
         : ( mshare([[Y]]), var(Y), ground([X]) )
        => ground([X,Y]).

:- true pred reverse(X,Y)
         : ( list(X,num), var(Y) )
        => ( list(X,num), list(Y,num) )
         + ( not_fails, covered ).

:- true pred reverse(X,Y)
         : ( list(X,num), var(Y) )
        => ( list(X,num), list(Y,num), size(lb,X,length(X)), size(lb,Y,length(X)) )
         + ( cost(lb,giunif,0.5*exp(length(X),2)+3.5*length(X)+1), cost(lb,gounif,0.5*exp(length(X),2)-0.5*length(X)+1), cost(lb,nargs,1.5*exp(length(X),2)+3.5*length(X)+2), cost(lb,steps,0.5*exp(length(X),2)+1.5*length(X)+1), cost(lb,viunif,1.5*exp(length(X),2)+1.5*length(X)), cost(lb,vounif,exp(length(X),2)+length(X)) ).

:- true pred reverse(X,Y)
         : ( list(X,num), var(Y) )
        => ( list(X,num), list(Y,num), size(ub,X,length(X)), size(ub,Y,length(X)) )
         + ( cost(ub,giunif,0.5*exp(length(X),2)+3.5*length(X)+1), cost(ub,gounif,0.5*exp(length(X),2)-0.5*length(X)+1), cost(ub,nargs,1.5*exp(length(X),2)+3.5*length(X)+2), cost(ub,res_steps,0.5*exp(length(X),2)+1.5*length(X)+1), cost(ub,steps,0.5*exp(length(X),2)+1.5*length(X)+1), cost(ub,viunif,1.5*exp(length(X),2)+1.5*length(X)), cost(ub,vounif,exp(length(X),2)+length(X)) ).

:- true pred reverse(X,Y)
         : ( list(X,num), var(Y) )
        => ( list(X,num), list(Y,num), size_lb(X,length(X)), size_lb(Y,length(X)), size_ub(X,length(X)), size_ub(Y,length(X)) )
         + ( steps_lb(0.5*exp(length(X),2)+1.5*length(X)+1), steps_ub(0.5*exp(length(X),2)+1.5*length(X)+1) ).

reverse([],[]).
reverse([X|T],L) :-
        reverse(T,L1),
        concat(L1,[X],L).

:- trust comp concat(X,Y,Z)
         + head_cost(ub,res_steps,1).

:- true pred concat(X,Y,Z)
         : ( list(X,num), rt2(Y), term(Z) )
        => ( list(X,num), rt19(Y), rt19(Z) ).

:- true pred concat(X,Y,Z)
         : ( mshare([[Z]]), var(Z), ground([X,Y]) )
        => ground([X,Y,Z]).

:- true pred concat(X,Y,Z)
         : ( list(X,num), rt2(Y), var(Z) )
        => ( list(X,num), rt19(Y), rt19(Z) )
         + ( not_fails, covered ).

:- true pred concat(X,Y,Z)
         : ( list(X,num), rt2(Y), var(Z) )
        => ( list(X,num), rt19(Y), rt19(Z), size(lb,X,length(X)), size(lb,Y,length(Y)), size(lb,Z,length(Y)+length(X)) )
         + ( cost(lb,giunif,length(X)+1), cost(lb,gounif,length(X)), cost(lb,nargs,3*length(X)+3), cost(lb,steps,length(X)+1), cost(lb,viunif,3*length(X)+1), cost(lb,vounif,2*length(X)+1) ).

:- true pred concat(X,Y,Z)
         : ( list(X,num), rt2(Y), var(Z) )
        => ( list(X,num), rt19(Y), rt19(Z), size(ub,X,length(X)), size(ub,Y,length(Y)), size(ub,Z,length(Y)+length(X)) )
         + ( cost(ub,giunif,length(X)+1), cost(ub,gounif,length(X)), cost(ub,nargs,3*length(X)+3), cost(ub,res_steps,length(X)+1), cost(ub,steps,length(X)+1), cost(ub,viunif,3*length(X)+1), cost(ub,vounif,2*length(X)+1) ).

:- true pred concat(X,Y,Z)
         : ( list(X,num), rt2(Y), var(Z) )
        => ( list(X,num), rt19(Y), rt19(Z), size_lb(X,length(X)), size_lb(Y,length(Y)), size_lb(Z,length(Y)+length(X)), size_ub(X,length(X)), size_ub(Y,length(Y)), size_ub(Z,length(Y)+length(X)) )
         + ( steps_lb(length(X)+1), steps_ub(length(X)+1) ).

concat([],Y,Y).
concat([X|Xs],Ys,[X|Zs]) :-
        concat(Xs,Ys,Zs).


:- regtype rt2/1.

rt2([A]) :-
        num(A).


:- regtype rt19/1.

rt19([A|B]) :-
        num(A),
        list(B,num).


