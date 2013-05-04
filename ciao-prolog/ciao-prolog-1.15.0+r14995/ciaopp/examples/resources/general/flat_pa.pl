:- module(_1,[flat/2],[assertions,nativeprops,regtypes,ciaopp(tests(resources)),predefres(res_steps),basicmodes]).

:- entry flat(_1,_2)
         : ( gnd(_1), var(_2) ).

:- true pred flat(X,_1)
         : ( term(X), term(_1) )
        => ( term(X), non_empty_list(_1) ).

:- true pred flat(X,_1)
         : ( mshare([[X],[_1]]), var(_1) )
        => mshare([[X],[X,_1],[_1]]).

:- true pred flat(X,_1)
         : ( term(X), var(_1) )
        => ( term(X), non_empty_list(_1) )
         + ( not_fails, covered ).

:- true pred flat(X,_1)
         : ( term(X), var(_1) )
        => ( term(X), non_empty_list(_1), size(lb,X,size(X)), size(lb,_1,length(_1)) )
         + cost(lb,steps,1).

:- true pred flat(X,_1)
         : ( term(X), var(_1) )
        => ( term(X), non_empty_list(_1), size(ub,X,size(X)), size(ub,_1,length(_1)) )
         + cost(ub,steps,2).

flat(X,[X]) :-
        atomic(X),
        !.
flat(X,[F|List]) :-
        functor(X,F,N),
        flat_(N,X,List).

:- trust comp flat_(X,Y,Z)
         + ( cost(ub,steps,1), cost(lb,steps,1) ).

:- true pred flat_(X,Y,Z)
         : ( term(X), term(Y), term(Z) )
        => ( num(X), term(Y), list(Z) ).

:- true pred flat_(X,Y,Z)
         : ( mshare([[X],[X,Y],[Y],[Z]]), var(Z) )
        => ( mshare([[Y],[Y,Z],[Z]]), ground([X]) ).

:- true pred flat_(X,Y,Z)
         : ( term(X), term(Y), var(Z) )
        => ( num(X), term(Y), list(Z) )
         + ( possibly_fails, not_covered ).

:- true pred flat_(X,Y,Z)
         : ( term(X), term(Y), var(Z) )
        => ( num(X), term(Y), list(Z), size(lb,X,0), size(lb,Y,size(Y)), size(lb,Z,length(Z)) )
         + cost(lb,steps,1).

:- true pred flat_(X,Y,Z)
         : ( term(X), term(Y), var(Z) )
        => ( num(X), term(Y), list(Z), size(ub,X,bot), size(ub,Y,size(Y)), size(ub,Z,length(Z)) )
         + cost(ub,steps,1).

flat_(0,_1,[]).
flat_(N,X,List) :-
        N>0,
        arg(N,X,Arg),
        flat(Arg,List1),
        N1 is N-1,
        flat_(N1,X,List2),
        append(List1,List2,List).

:- true pred append(_1,L,_2)
         : ( list(_1), list(L), term(_2) )
        => ( list(_1), list(L), list(_2) ).

:- true pred append(_1,L,_2)
         : ( mshare([[_1],[_1,L],[L],[_2]]), var(_2) )
        => mshare([[_1,L,_2],[_1,_2],[L,_2]]).

:- true pred append(_1,L,_2)
         : ( list(_1), list(L), var(_2) )
        => ( list(_1), list(L), list(_2) )
         + ( not_fails, covered ).

:- true pred append(_1,L,_2)
         : ( list(_1), list(L), var(_2) )
        => ( list(_1), list(L), list(_2), size(lb,_1,length(_1)), size(lb,L,length(L)), size(lb,_2,length(_2)) )
         + cost(lb,steps,0).

:- true pred append(_1,L,_2)
         : ( list(_1), list(L), var(_2) )
        => ( list(_1), list(L), list(_2), size(ub,_1,length(_1)), size(ub,L,length(L)), size(ub,_2,length(_2)) )
         + cost(ub,steps,inf).

append([],L,L).
append([H|L],L1,[H|R]) :-
        append(L,L1,R).


