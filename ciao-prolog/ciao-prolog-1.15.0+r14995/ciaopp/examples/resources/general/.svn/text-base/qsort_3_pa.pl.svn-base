:- module(_1,[qsort/2],[assertions,regtypes,nativeprops,ciaopp(tests(resources)),library(resdefs(resources_decl)),basicmodes]).

:- load_resource_module(qsort_res).

:- resource lists_parallelized.

:- head_cost(ub,lists_parallelized,'qsort_3:delta_lists_parallelized').

:- literal_cost(ub,lists_parallelized,0).

:- trust_default+cost(ub,lists_parallelized,0).

:- trust_default+cost(lb,lists_parallelized,0).

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
        => ( list(A,num), list(B,num), size(ub,A,length(A)), size(ub,B,exp(2,length(A))-1.0) )
         + cost(ub,lists_parallelized,0).

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
        => ( list(_2,num), num(_1), list(Left,num), list(Right,num), size(ub,_2,length(_2)), size(ub,_1,int(_1)), size(ub,Left,length(_2)), size(ub,Right,length(_2)) )
         + cost(ub,lists_parallelized,0).

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
        => ( list(_1,num), rt13(X), rt13(_2), size(ub,_1,length(_1)), size(ub,X,length(X)), size(ub,_2,length(X)+length(_1)) )
         + cost(ub,lists_parallelized,0).

append([],X,X).
append([H|X],Y,[H|Z]) :-
        append(X,Y,Z).


:- regtype rt13/1.

rt13([A|B]) :-
        num(A),
        list(B,num).


