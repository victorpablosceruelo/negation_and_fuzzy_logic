:- module(_1,[add_polynomials/3],[assertions,regtypes,ciaopp(examples(resources(exectimehl))),nativeprops,basicmodes]).

:- prop factor/1+regtype.

:- prop factor(_1)
         + regtype.

factor((A,N)) :-
        num(A),
        num(N).

:- entry add_polynomials(X,Y,Z)
         : ( list(X,factor), list(Y,factor), var(Z) ).

:- true pred add_polynomials(X,Y,Z)
         : ( list(X,factor), list(Y,factor), term(Z) )
        => ( list(X,factor), list(Y,factor), list(Z,factor) ).

:- true pred add_polynomials(X,Y,Z)
         : ( mshare([[Z]]), var(Z), ground([X,Y]) )
        => ground([X,Y,Z]).

:- true pred add_polynomials(X,Y,Z)
         : ( list(X,factor), list(Y,factor), var(Z) )
        => ( list(X,factor), list(Y,factor), list(Z,factor) )
         + ( possibly_fails, not_covered ).

:- true pred add_polynomials(X,Y,Z)
         : ( list(X,factor), list(Y,factor), var(Z) )
        => ( list(X,factor), list(Y,factor), list(Z,factor), size(lb,X,length(X)), size(lb,Y,length(Y)), size(lb,Z,length(Y)+length(X)) )
         + cost(lb,exectime_model4,0).

:- true pred add_polynomials(X,Y,Z)
         : ( list(X,factor), list(Y,factor), var(Z) )
        => ( list(X,factor), list(Y,factor), list(Z,factor), size(ub,X,length(X)), size(ub,Y,length(Y)), size(ub,Z,length(Y)+length(X)) )
         + cost(ub,exectime_model4,1196.442420853618*length(Y)+1196.442420853618*length(X)+814.5099368749197).

add_polynomials([],Poly,Poly) :- !.
add_polynomials(Poly,[],Poly) :- !.
add_polynomials([(Ai,Ni)|Poly1],[(Aj,Nj)|Poly2],[(Ai,Ni)|Poly]) :-
        Ni>Nj,
        !,
        add_polynomials(Poly1,[(Aj,Nj)|Poly2],Poly).
add_polynomials([(Ai,Ni)|Poly1],[(Aj,Nj)|Poly2],[(A,Ni)|Poly]) :-
        Ni=:=Nj,
        !,
        A is Ai+Aj,
        add_polynomials(Poly1,Poly2,Poly).
add_polynomials([(Ai,Ni)|Poly1],[(Aj,Nj)|Poly2],[(Aj,Nj)|Poly]) :-
        Ni<Nj,
        !,
        add_polynomials([(Ai,Ni)|Poly1],Poly2,Poly).


