:- module(_1,[parse/2],[assertions,nativeprops,ciaopp(tests(resources)),library(resdefs(resources_decl)),basicmodes,regtypes]).

:- load_resource_module(grammar_res).

:- resource phrases.

:- head_cost(ub,phrases,'grammar:delta_phrases').

:- literal_cost(ub,phrases,0).

:- trust_default+cost(ub,phrases,0).

:- entry parse(X,Y)
         : ( list(X,atm), var(Y) ).

:- true pred parse(X,Y)
         : ( list(X,atm), term(Y) )
        => ( rt4(X), rt48(Y) ).

:- true pred parse(X,Y)
         : ( mshare([[Y]]), var(Y), ground([X]) )
        => ( mshare([[Y]]), ground([X]) ).

:- true pred parse(X,Y)
         : ( list(X,atm), var(Y) )
        => ( rt4(X), rt48(Y) )
         + ( possibly_fails, covered ).

:- true pred parse(X,Y)
         : ( list(X,atm), var(Y) )
        => ( rt4(X), rt48(Y), size(ub,X,length(X)), size(ub,Y,size(Y)) )
         + cost(ub,phrases,24).

parse(S0,Meaning) :-
        np(S0,S1,Meaning),
        verb(S1,[],Meaning).
parse(S0,Meaning) :-
        np(S0,S1,Meaning),
        verb(S1,S2,Meaning),
        S2=[].

:- true pred np(Si,So,S)
         : ( list(Si,atm), term(So), term(S) )
        => ( rt4(Si), list(So,atm), rt28(S) ).

:- true pred np(Si,So,S)
         : ( mshare([[So],[S]]), var(So), var(S), ground([Si]) )
        => ( mshare([[S]]), ground([Si,So]) ).

:- true pred np(Si,So,S)
         : ( list(Si,atm), var(So), var(S) )
        => ( rt4(Si), list(So,atm), rt28(S) )
         + ( possibly_fails, covered ).

:- true pred np(Si,So,S)
         : ( list(Si,atm), var(So), var(S) )
        => ( rt4(Si), list(So,atm), rt28(S), size(ub,Si,length(Si)), size(ub,So,length(Si)-2), size(ub,S,size(S)) )
         + cost(ub,phrases,8).

np(Si,So,S) :-
        det(Si,St,T),
        noun(St,So,N),
        comb(T,N,S).

:- true pred comb(_1,_2,_3)
         : ( rt5(_1), rt21(_2), term(_3) )
        => ( rt5(_1), rt21(_2), rt28(_3) ).

:- true pred comb(_1,_2,_3)
         : ( mshare([[_3]]), var(_3), ground([_1,_2]) )
        => ( mshare([[_3]]), ground([_1,_2]) ).

:- true pred comb(_1,_2,_3)
         : ( rt5(_1), rt21(_2), var(_3) )
        => ( rt5(_1), rt21(_2), rt28(_3) )
         + ( possibly_fails, not_covered ).

:- true pred comb(_1,_2,_3)
         : ( rt5(_1), rt21(_2), var(_3) )
        => ( rt5(_1), rt21(_2), rt28(_3), size(ub,_1,1), size(ub,_2,size(_2)), size(ub,_3,size(_3)) )
         + cost(ub,phrases,2).

comb(a,N-s,s(s,N,_V)).
comb(the,N-P,s(P,N,_V)).

:- true pred det(_1,S,_2)
         : ( list(_1,atm), term(S), term(_2) )
        => ( rt4(_1), list(S,atm), rt5(_2) ).

:- true pred det(_1,S,_2)
         : ( mshare([[S],[_2]]), var(S), var(_2), ground([_1]) )
        => ground([_1,S,_2]).

:- true pred det(_1,S,_2)
         : ( list(_1,atm), var(S), var(_2) )
        => ( rt4(_1), list(S,atm), rt5(_2) )
         + ( possibly_fails, not_covered ).

:- true pred det(_1,S,_2)
         : ( list(_1,atm), var(S), var(_2) )
        => ( rt4(_1), list(S,atm), rt5(_2), size(ub,_1,length(_1)), size(ub,S,length(_1)-1), size(ub,_2,1) )
         + cost(ub,phrases,2).

det([a|S],S,a).
det([the|S],S,the).

:- true pred noun(_1,S,_2)
         : ( list(_1,atm), term(S), term(_2) )
        => ( rt23(_1), list(S,atm), rt21(_2) ).

:- true pred noun(_1,S,_2)
         : ( mshare([[S],[_2]]), var(S), var(_2), ground([_1]) )
        => ground([_1,S,_2]).

:- true pred noun(_1,S,_2)
         : ( list(_1,atm), var(S), var(_2) )
        => ( rt23(_1), list(S,atm), rt21(_2) )
         + ( possibly_fails, not_covered ).

:- true pred noun(_1,S,_2)
         : ( list(_1,atm), var(S), var(_2) )
        => ( rt23(_1), list(S,atm), rt21(_2), size(ub,_1,length(_1)), size(ub,S,length(_1)-1), size(ub,_2,3) )
         + cost(ub,phrases,4).

noun([book|S],S,book-s).
noun([books|S],S,book-p).
noun([box|S],S,box-s).
noun([boxes|S],S,box-p).

:- true pred verb(_1,So,_2)
         : ( list(_1,atm), term(So), rt28(_2) )
        => ( rt70(_1), list(So,atm), rt48(_2) ).

:- true pred verb(_1,So,_2)
         : ( mshare([[So],[_2]]), ground([_1]) )
        => ( mshare([[_2]]), ground([_1,So]) ).

:- true pred verb(_1,So,_2)
         : ( list(_1,atm), term(So), rt28(_2) )
        => ( rt70(_1), list(So,atm), rt48(_2) )
         + ( possibly_fails, not_covered ).

:- true pred verb(_1,So,_2)
         : ( list(_1,atm), term(So), rt28(_2) )
        => ( rt70(_1), list(So,atm), rt48(_2), size(ub,_1,length(_1)), size(ub,So,length(_1)-1), size(ub,_2,size(_2)) )
         + cost(ub,phrases,4).

verb([falls|So],So,s(s,_N,fall)).
verb([fall|So],So,s(p,_N,fall)).
verb([flies|So],So,s(s,_N,fly)).
verb([fly|So],So,s(p,_N,fly)).


:- regtype rt5/1.

rt5(a).
rt5(the).


:- regtype rt21/1.

rt21(A-B) :-
        rt22(A),
        rt18(B).


:- regtype rt28/1.

rt28(s(A,B,C)) :-
        rt18(A),
        rt22(B),
        term(C).


:- regtype rt4/1.

rt4([A|B]) :-
        rt5(A),
        list(B,atm).


:- regtype rt24/1.

rt24(book).
rt24(books).
rt24(box).
rt24(boxes).


:- regtype rt23/1.

rt23([A|B]) :-
        rt24(A),
        list(B,atm).


:- regtype rt18/1.

rt18(p).
rt18(s).


:- regtype rt22/1.

rt22(book).
rt22(box).


:- regtype rt49/1.

rt49(fall).
rt49(fly).


:- regtype rt48/1.

rt48(s(A,B,C)) :-
        rt18(A),
        rt22(B),
        rt49(C).


:- regtype rt53/1.

rt53(fall).
rt53(falls).
rt53(flies).
rt53(fly).


:- regtype rt70/1.

rt70([A|B]) :-
        rt53(A),
        list(B,atm).


