:- module(_1,[flat/2],[assertions,regtypes,predefres(res_steps),nativeprops,basicmodes]).

:- doc(author,"Nai-Wei Lin").

:- doc(author,"Edison Mera").

:- doc(module,"This program flattens a term into a list of atom.").

:- resource res_steps_1.

:- resource res_steps_2.

:- head_cost(ub,res_steps_1,1).

:- head_cost(lb,res_steps_1,1).

:- head_cost(ub,res_steps_2,2).

:- head_cost(lb,res_steps_2,2).

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
         + ( cost(lb,res_steps_1,1), cost(lb,res_steps_2,2), cost(lb,steps,1) ).

:- true pred flat(X,_1)
         : ( term(X), var(_1) )
        => ( term(X), non_empty_list(_1), size(ub,X,size(X)), size(ub,_1,length(_1)) )
         + ( cost(ub,res_steps_1,inf), cost(ub,res_steps_2,inf), cost(ub,steps,inf) ).

flat(X,[X]) :-
        atomic(X),
        !.
flat(X,[F|List]) :-
        functor(X,F,N),
        flat_(N,X,List).

:- true pred flat_(N,_1,List)
         : ( term(N), term(_1), term(List) )
        => ( num(N), term(_1), list(List) ).

:- true pred flat_(N,_1,List)
         : ( mshare([[N],[N,_1],[_1],[List]]), var(List) )
        => ( mshare([[_1],[_1,List],[List]]), ground([N]) ).

:- true pred flat_(N,_1,List)
         : ( term(N), term(_1), var(List) )
        => ( num(N), term(_1), list(List) )
         + ( possibly_fails, not_covered ).

:- true pred flat_(N,_1,List)
         : ( term(N), term(_1), var(List) )
        => ( num(N), term(_1), list(List), size(lb,N,0), size(lb,_1,size(_1)), size(lb,List,length(List)) )
         + ( cost(lb,res_steps_1,0), cost(lb,res_steps_2,0), cost(lb,steps,0) ).

:- true pred flat_(N,_1,List)
         : ( term(N), term(_1), var(List) )
        => ( num(N), term(_1), list(List), size(ub,N,bot), size(ub,_1,size(_1)), size(ub,List,length(List)) )
         + ( cost(ub,res_steps_1,inf), cost(ub,res_steps_2,inf), cost(ub,steps,inf) ).

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
         + ( cost(lb,res_steps_1,0), cost(lb,res_steps_2,0), cost(lb,steps,0) ).

:- true pred append(_1,L,_2)
         : ( list(_1), list(L), var(_2) )
        => ( list(_1), list(L), list(_2), size(ub,_1,length(_1)), size(ub,L,length(L)), size(ub,_2,length(_2)) )
         + ( cost(ub,res_steps_1,inf), cost(ub,res_steps_2,inf), cost(ub,steps,inf) ).

append([],L,L).
append([H|L],L1,[H|R]) :-
        append(L,L1,R).


