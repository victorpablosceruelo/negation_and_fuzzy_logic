:- module(_1,[powset/2],[assertions,regtypes,ciaopp(examples(resources(exectimell))),nativeprops,basicmodes]).

:- entry powset(A,B)
         : ( list(A,int), var(B) ).

:- true pred powset(A,B)
         : ( list(A,int), term(B) )
        => ( list(A,int), rt146(B) ).

:- true pred powset(A,B)
         : ( mshare([[B]]), var(B), ground([A]) )
        => ground([A,B]).

:- true pred powset(A,B)
         : ( list(A,int), var(B) )
        => ( list(A,int), rt146(B) )
         + ( not_fails, covered ).

:- true pred powset(A,B)
         : ( list(A,int), var(B) )
        => ( list(A,int), rt146(B), size(lb,A,length(A)), size(lb,B,exp(2,length(A))) )
         + ( cost(lb,exectime,6129.251999999998*exp(2,length(A)+1)+11829.3*length(A)-8156.087999999995), cost(lb,exectime_me,6129.251999999998*exp(2,length(A)+1)+11829.3*length(A)-8156.087999999995), cost(lb,wamcount,16.0*exp(2,length(A)+1)+32*length(A)-21.0) ).

:- true pred powset(A,B)
         : ( list(A,int), var(B) )
        => ( list(A,int), rt146(B), size(ub,A,length(A)), size(ub,B,exp(2,length(A))) )
         + ( cost(ub,exectime,6129.251999999998*exp(2,length(A)+1)+11829.3*length(A)-8156.087999999995), cost(ub,exectime_me,6129.251999999998*exp(2,length(A)+1)+11829.3*length(A)-8156.087999999995), cost(ub,wamcount,16.0*exp(2,length(A)+1)+32*length(A)-21.0) ).

powset([],[[]]).
powset([X|L],P) :-
        powset(L,P0),
        appendelem(P0,X,P,P0).

:- true pred appendelem(_1,_X,T,_2)
         : ( list(_1,nlist(int)), int(_X), term(T), rt146(_2) )
        => ( list(_1,nlist(int)), int(_X), rt146(T), rt146(_2) ).

:- true pred appendelem(_1,_X,T,_2)
         : ( mshare([[T]]), var(T), ground([_1,_X,_2]) )
        => ground([_1,_X,T,_2]).

:- true pred appendelem(_1,_X,T,_2)
         : ( list(_1,nlist(int)), int(_X), var(T), rt146(_2) )
        => ( list(_1,nlist(int)), int(_X), rt146(T), rt146(_2) )
         + ( not_fails, covered ).

:- true pred appendelem(_1,_X,T,_2)
         : ( list(_1,nlist(int)), int(_X), var(T), rt146(_2) )
        => ( list(_1,nlist(int)), int(_X), rt146(T), rt146(_2), size(lb,_1,length(_1)), size(lb,_X,int(_X)), size(lb,T,length(_2)+length(_1)), size(lb,_2,length(_2)) )
         + ( cost(lb,exectime,12258.504*length(_1)+3428.64), cost(lb,exectime_me,12258.504*length(_1)+3428.64), cost(lb,wamcount,32*length(_1)+8) ).

:- true pred appendelem(_1,_X,T,_2)
         : ( list(_1,nlist(int)), int(_X), var(T), rt146(_2) )
        => ( list(_1,nlist(int)), int(_X), rt146(T), rt146(_2), size(ub,_1,length(_1)), size(ub,_X,int(_X)), size(ub,T,length(_2)+length(_1)), size(ub,_2,length(_2)) )
         + ( cost(ub,exectime,12258.504*length(_1)+3428.64), cost(ub,exectime_me,12258.504*length(_1)+3428.64), cost(ub,wamcount,32*length(_1)+8) ).

appendelem([],_X,T,T).
appendelem([L|Ls],X,[[X|L]|Rs],T) :-
        appendelem(Ls,X,Rs,T).


:- regtype rt147/1.

rt147([]).
rt147([A|B]) :-
        int(A),
        nlist(B,int).


:- regtype rt146/1.

rt146([A|B]) :-
        rt147(A),
        list(B,nlist(int)).


