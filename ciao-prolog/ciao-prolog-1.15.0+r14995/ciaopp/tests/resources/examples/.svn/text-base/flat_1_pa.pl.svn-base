:- module(_1,[flat/2],[assertions,regtypes,resdefs,nativeprops,basicmodes]).

:- doc(author,"Nai-Wei Lin").

:- doc(author,"Edison Mera").

:- doc(module,"This program flattens a term into a list of atom.").

:- resource res_steps.

:- trust comp flat(X,Y)
         + ( head_cost(ub,res_steps,1), head_cost(lb,res_steps,1) ).

:- entry flat(_1,_2)
         : ( gnd(_1), var(_2) ).

:- true pred flat(X,Y)
         : ( term(X), term(Y) )
        => ( term(X), non_empty_list(Y) ).

:- true pred flat(X,Y)
         : ( mshare([[X],[Y]]), var(Y) )
        => mshare([[X],[X,Y],[Y]]).

:- true pred flat(X,Y)
         : ( term(X), var(Y) )
        => ( term(X), non_empty_list(Y) )
         + ( not_fails, covered ).

:- true pred flat(X,Y)
         : ( term(X), var(Y) )
        => ( term(X), non_empty_list(Y), size(lb,X,size(X)), size(lb,Y,length(Y)) )
         + cost(lb,res_steps,1).

:- true pred flat(X,Y)
         : ( term(X), var(Y) )
        => ( term(X), non_empty_list(Y), size(ub,X,size(X)), size(ub,Y,length(Y)) )
         + cost(ub,res_steps,inf).

flat(X,[X]) :-
        atomic(X),
        !.
flat(X,[F|List]) :-
        functor(X,F,N),
        flat_(N,X,List).

:- trust comp flat_(X,Y,Z)
         + ( head_cost(ub,res_steps,1), head_cost(lb,res_steps,1) ).

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
         + cost(lb,res_steps,0).

:- true pred flat_(X,Y,Z)
         : ( term(X), term(Y), var(Z) )
        => ( num(X), term(Y), list(Z), size(ub,X,bot), size(ub,Y,size(Y)), size(ub,Z,length(Z)) )
         + cost(ub,res_steps,inf).

flat_(0,_1,[]).
flat_(N,X,List) :-
        N>0,
        arg(N,X,Arg),
        flat(Arg,List1),
        N1 is N-1,
        flat_(N1,X,List2),
        append(List1,List2,List).

:- trust comp append(X,Y,Z)
         + ( head_cost(ub,res_steps,1), head_cost(lb,res_steps,1) ).

:- true pred append(X,Y,Z)
         : ( list(X), list(Y), term(Z) )
        => ( list(X), list(Y), list(Z) ).

:- true pred append(X,Y,Z)
         : ( mshare([[X],[X,Y],[Y],[Z]]), var(Z) )
        => mshare([[X,Y,Z],[X,Z],[Y,Z]]).

:- true pred append(X,Y,Z)
         : ( list(X), list(Y), var(Z) )
        => ( list(X), list(Y), list(Z) )
         + ( not_fails, covered ).

:- true pred append(X,Y,Z)
         : ( list(X), list(Y), var(Z) )
        => ( list(X), list(Y), list(Z), size(lb,X,length(X)), size(lb,Y,length(Y)), size(lb,Z,length(Z)) )
         + cost(lb,res_steps,0).

:- true pred append(X,Y,Z)
         : ( list(X), list(Y), var(Z) )
        => ( list(X), list(Y), list(Z), size(ub,X,length(X)), size(ub,Y,length(Y)), size(ub,Z,length(Z)) )
         + cost(ub,res_steps,inf).

append([],L,L).
append([H|L],L1,[H|R]) :-
        append(L,L1,R).


