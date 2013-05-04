:- module(_1,[qsort/2],[assertions,ciaopp(tests(resources)),predefres(res_all),nativeprops,basicmodes,regtypes]).

:- entry qsort(A,B)
         : ( list(A,num), var(B) ).

:- true pred qsort(A,B)
         : ( list(A,num), term(B) )
        => ( list(A,num), list(B,num) ).

:- true pred qsort(A,B)
         : ( mshare([[B]]), var(B), ground([A]) )
        => ground([A,B]).

:- true pred qsort(A,B)
         : ( list(A,num), var(B) )
        => ( list(A,num), list(B,num) )
         + ( not_fails, covered ).

:- true pred qsort(A,B)
         : ( list(A,num), var(B) )
        => ( list(A,num), list(B,num), size(lb,A,length(A)), size(lb,B,1) )
         + ( cost(lb,giunif,length(A)+6), cost(lb,gounif,length(A)+4), cost(lb,nargs,4*length(A)+12), cost(lb,steps,length(A)+5), cost(lb,viunif,3*length(A)+4), cost(lb,vounif,3*length(A)+1) ).

:- true pred qsort(A,B)
         : ( list(A,num), var(B) )
        => ( list(A,num), list(B,num), size(ub,A,length(A)), size(ub,B,exp(2,length(A))-1.0) )
         + ( cost(ub,giunif,sum($(j),1,length(A),exp(2,length(A)- $(j))* $(j))+exp(2,length(A)-1)*length(A)+3.0*exp(2,length(A))-2.0), cost(ub,gounif,sum($(j),1,length(A),exp(2,length(A)- $(j))* $(j))+exp(2,length(A)-1)*length(A)+exp(2,length(A))), cost(ub,nargs,sum($(j),1,length(A),4*(exp(2,length(A)- $(j))* $(j)))+3*(exp(2,length(A)-1)*length(A))+4.0*exp(2,length(A))-2.0), cost(ub,steps,sum($(j),1,length(A),exp(2,length(A)- $(j))* $(j))+exp(2,length(A)-1)*length(A)+2.0*exp(2,length(A))-1.0), cost(ub,viunif,sum($(j),1,length(A),3*(exp(2,length(A)- $(j))* $(j)))+3*(exp(2,length(A)-1)*length(A))-2.0*exp(2,length(A))+2.0), cost(ub,vounif,sum($(j),1,length(A),3*(exp(2,length(A)- $(j))* $(j)))+2*(exp(2,length(A)-1)*length(A))-3.0*exp(2,length(A))+3.0) ).

:- true pred qsort(A,B)
         : ( list(A,num), var(B) )
        => ( list(A,num), list(B,num), size_lb(A,length(A)), size_lb(B,1), size_ub(A,length(A)), size_ub(B,exp(2,length(A))-1.0) )
         + ( steps_lb(length(A)+5), steps_ub(sum($(j),1,length(A),exp(2,length(A)- $(j))* $(j))+exp(2,length(A)-1)*length(A)+2.0*exp(2,length(A))-1.0) ).

qsort([X|L],R) :-
        partition(L,X,L1,L2),
        qsort(L2,R2),
        qsort(L1,R1),
        append(R1,[X|R2],R).
qsort([],[]).

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
         + ( cost(lb,giunif,length(_2)+1), cost(lb,gounif,length(_2)+2), cost(lb,nargs,4*length(_2)+4), cost(lb,steps,length(_2)+1), cost(lb,viunif,3*length(_2)+1), cost(lb,vounif,3*length(_2)) ).

:- true pred partition(_2,_1,Left,Right)
         : ( list(_2,num), num(_1), var(Left), var(Right) )
        => ( list(_2,num), num(_1), list(Left,num), list(Right,num), size(ub,_2,length(_2)), size(ub,_1,int(_1)), size(ub,Left,length(_2)), size(ub,Right,length(_2)) )
         + ( cost(ub,giunif,length(_2)+1), cost(ub,gounif,length(_2)+2), cost(ub,nargs,4*length(_2)+4), cost(ub,steps,length(_2)+1), cost(ub,viunif,3*length(_2)+1), cost(ub,vounif,3*length(_2)) ).

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

:- true pred append(_1,X,_2)
         : ( list(_1,num), rt13(X), term(_2) )
        => ( list(_1,num), rt13(X), rt13(_2) ).

:- true pred append(_1,X,_2)
         : ( mshare([[_2]]), var(_2), ground([_1,X]) )
        => ground([_1,X,_2]).

:- true pred append(_1,X,_2)
         : ( list(_1,num), rt13(X), var(_2) )
        => ( list(_1,num), rt13(X), rt13(_2) )
         + ( not_fails, covered ).

:- true pred append(_1,X,_2)
         : ( list(_1,num), rt13(X), var(_2) )
        => ( list(_1,num), rt13(X), rt13(_2), size(lb,_1,length(_1)), size(lb,X,length(X)), size(lb,_2,length(X)+length(_1)) )
         + ( cost(lb,giunif,length(_1)+1), cost(lb,gounif,length(_1)), cost(lb,nargs,3*length(_1)+3), cost(lb,steps,length(_1)+1), cost(lb,viunif,3*length(_1)+1), cost(lb,vounif,2*length(_1)+1) ).

:- true pred append(_1,X,_2)
         : ( list(_1,num), rt13(X), var(_2) )
        => ( list(_1,num), rt13(X), rt13(_2), size(ub,_1,length(_1)), size(ub,X,length(X)), size(ub,_2,length(X)+length(_1)) )
         + ( cost(ub,giunif,length(_1)+1), cost(ub,gounif,length(_1)), cost(ub,nargs,3*length(_1)+3), cost(ub,steps,length(_1)+1), cost(ub,viunif,3*length(_1)+1), cost(ub,vounif,2*length(_1)+1) ).

:- true pred append(_1,X,_2)
         : ( list(_1,num), rt13(X), var(_2) )
        => ( list(_1,num), rt13(X), rt13(_2), size_lb(_1,length(_1)), size_lb(X,length(X)), size_lb(_2,length(X)+length(_1)), size_ub(_1,length(_1)), size_ub(X,length(X)), size_ub(_2,length(X)+length(_1)) )
         + ( steps_lb(length(_1)+1), steps_ub(length(_1)+1) ).

append([],X,X).
append([H|X],Y,[H|Z]) :-
        append(X,Y,Z).


:- regtype rt13/1.

rt13([A|B]) :-
        num(A),
        list(B,num).


