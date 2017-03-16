:- module(_1,[dummy/3],[assertions,nativeprops,ciaopp(tests(resources)),predefres(res_all),basicmodes,regtypes]).

:- doc(author,"Edison Mera").

:- resource res_steps.

:- head_cost(ub,res_steps,1).

:- literal_cost(ub,res_steps,0).

:- head_cost(lb,res_steps,1).

:- literal_cost(lb,res_steps,0).

:- trust_default+cost(ub,res_steps,0).

:- trust_default+cost(lb,res_steps,0).

:- entry dummy(A,B,C)
         : ( list(A,num), num(B), var(C) ).

:- true pred dummy(A,B,C)
         : ( list(A,num), num(B), term(C) )
        => ( list(A,num), num(B), rt20(C) ).

:- true pred dummy(A,B,C)
         : ( mshare([[C]]), var(C), ground([A,B]) )
        => ground([A,B,C]).

:- true pred dummy(A,B,C)
         : ( list(A,num), num(B), var(C) )
        => ( list(A,num), num(B), rt20(C) )
         + ( not_fails, covered ).

:- true pred dummy(A,B,C)
         : ( list(A,num), num(B), var(C) )
        => ( list(A,num), num(B), rt20(C), size(lb,A,length(A)), size(lb,B,int(B)), size(lb,C,0) )
         + ( cost(lb,giunif,length(A)+2), cost(lb,gounif,length(A)+2), cost(lb,nargs,4*length(A)+7), cost(lb,res_steps,length(A)+2), cost(lb,steps,length(A)+2), cost(lb,viunif,3*length(A)+3), cost(lb,vounif,3*length(A)+1) ).

:- true pred dummy(A,B,C)
         : ( list(A,num), num(B), var(C) )
        => ( list(A,num), num(B), rt20(C), size(ub,A,length(A)), size(ub,B,int(B)), size(ub,C,inf) )
         + ( cost(ub,giunif,length(A)+2), cost(ub,gounif,length(A)+2), cost(ub,nargs,4*length(A)+7), cost(ub,res_steps,length(A)+2), cost(ub,steps,length(A)+2), cost(ub,viunif,3*length(A)+3), cost(ub,vounif,3*length(A)+1) ).

:- true pred dummy(A,B,C)
         : ( list(A,num), num(B), var(C) )
        => ( list(A,num), num(B), rt20(C), size_lb(A,length(A)), size_lb(B,int(B)), size_lb(C,0), size_ub(A,length(A)), size_ub(B,int(B)), size_ub(C,inf) )
         + ( steps_lb(length(A)+2), steps_ub(length(A)+2) ).

dummy(A,B,R) :-
        partition(A,B,C,D),
        R=(C,D).

:- true pred partition(_2,_1,Left,Right)
         : ( list(_2,num), num(_1), term(Left), term(Right) )
        => ( list(_2,num), num(_1), list(Left,num), list(Right,num) ).

:- true pred partition(_2,_1,Left,Right)
         : ( mshare([[Left],[Right]]), var(Left), var(Right), ground([_2,_1]) )
        => ground([_2,_1,Left,Right]).

:- true pred partition(_2,_1,Left,Right)
         : ( list(_2,num), num(_1), var(Left), var(Right) )
        => ( list(_2,num), num(_1), list(Left,num), list(Right,num) )
         + ( not_fails, covered ).

:- true pred partition(_2,_1,Left,Right)
         : ( list(_2,num), num(_1), var(Left), var(Right) )
        => ( list(_2,num), num(_1), list(Left,num), list(Right,num), size(lb,_2,length(_2)), size(lb,_1,int(_1)), size(lb,Left,0), size(lb,Right,0) )
         + ( cost(lb,giunif,length(_2)+1), cost(lb,gounif,length(_2)+2), cost(lb,nargs,4*length(_2)+4), cost(lb,res_steps,length(_2)+1), cost(lb,steps,length(_2)+1), cost(lb,viunif,3*length(_2)+1), cost(lb,vounif,3*length(_2)) ).

:- true pred partition(_2,_1,Left,Right)
         : ( list(_2,num), num(_1), var(Left), var(Right) )
        => ( list(_2,num), num(_1), list(Left,num), list(Right,num), size(ub,_2,length(_2)), size(ub,_1,int(_1)), size(ub,Left,length(_2)), size(ub,Right,length(_2)) )
         + ( cost(ub,giunif,length(_2)+1), cost(ub,gounif,length(_2)+2), cost(ub,nargs,4*length(_2)+4), cost(ub,res_steps,length(_2)+1), cost(ub,steps,length(_2)+1), cost(ub,viunif,3*length(_2)+1), cost(ub,vounif,3*length(_2)) ).

:- true pred partition(_2,_1,Left,Right)
         : ( list(_2,num), num(_1), var(Left), var(Right) )
        => ( list(_2,num), num(_1), list(Left,num), list(Right,num), size_lb(_2,length(_2)), size_lb(_1,int(_1)), size_lb(Left,0), size_lb(Right,0), size_ub(_2,length(_2)), size_ub(_1,int(_1)), size_ub(Left,length(_2)), size_ub(Right,length(_2)) )
         + ( steps_lb(length(_2)+1), steps_ub(length(_2)+1) ).

partition([],_1,[],[]).
partition([E|R],C,[E|Left1],Right) :-
        E<C,
        !,
        partition(R,C,Left1,Right).
partition([E|R],C,Left,[E|Right1]) :-
        E>=C,
        partition(R,C,Left,Right1).


:- regtype rt20/1.

rt20((A,B)) :-
        list(A,num),
        list(B,num).


