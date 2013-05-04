:- module(_1,[powset/2,int_list/1],[assertions,regtypes,nativeprops,ciaopp(examples(resources(exectimehl))),basicmodes]).

:- prop int_list/1+regtype.

:- prop int_list(_1)
         + regtype.

:- true pred int_list(_1)
         : term(_1)
        => list(_1,character_code).

:- true pred int_list(_1)
         : mshare([[_1]])
        => ground([_1]).

:- true pred int_list(_1)
         : term(_1)
        => list(_1,character_code)
         + ( possibly_fails, not_covered ).

int_list([]).
int_list([X|L]) :-
        int(X),
        int_list(L).

:- entry powset(A,B)
         : ( int_list(A), var(B) ).

:- true pred powset(A,B)
         : ( int_list(A), term(B) )
        => ( list(A,character_code), rt153(B) ).

:- true pred powset(A,B)
         : ( mshare([[B]]), var(B), ground([A]) )
        => ground([A,B]).

:- true pred powset(A,B)
         : ( int_list(A), var(B) )
        => ( list(A,character_code), rt153(B) )
         + ( not_fails, covered ).

:- true pred powset(A,B)
         : ( int_list(A), var(B) )
        => ( list(A,character_code), rt153(B), size(lb,A,length(A)), size(lb,B,exp(2,length(A))) )
         + cost(lb,exectime_model4,345.5108114404464*exp(2,length(A)+1)+825.9499775241763*length(A)-212.8836154300491).

:- true pred powset(A,B)
         : ( int_list(A), var(B) )
        => ( list(A,character_code), rt153(B), size(ub,A,length(A)), size(ub,B,exp(2,length(A))) )
         + cost(ub,exectime_model4,368.5101795475302*exp(2,length(A)+1)+887.4808783217427*length(A)-230.4637292153913).

powset([],[[]]).
powset([X|L],P) :-
        powset(L,P0),
        append_elem(P0,X,P,P0).

:- true pred append_elem(_1,_X,T,_2)
         : ( list(_1,nlist(int)), int(_X), term(T), rt153(_2) )
        => ( list(_1,nlist(int)), int(_X), rt153(T), rt153(_2) ).

:- true pred append_elem(_1,_X,T,_2)
         : ( mshare([[T]]), var(T), ground([_1,_X,_2]) )
        => ground([_1,_X,T,_2]).

:- true pred append_elem(_1,_X,T,_2)
         : ( list(_1,nlist(int)), int(_X), var(T), rt153(_2) )
        => ( list(_1,nlist(int)), int(_X), rt153(T), rt153(_2) )
         + ( not_fails, covered ).

:- true pred append_elem(_1,_X,T,_2)
         : ( list(_1,nlist(int)), int(_X), var(T), rt153(_2) )
        => ( list(_1,nlist(int)), int(_X), rt153(T), rt153(_2), size(lb,_1,length(_1)), size(lb,_X,int(_X)), size(lb,T,length(_2)+length(_1)), size(lb,_2,length(_2)) )
         + cost(lb,exectime_model4,691.0216228808929*length(_1)+433.8555520315054).

:- true pred append_elem(_1,_X,T,_2)
         : ( list(_1,nlist(int)), int(_X), var(T), rt153(_2) )
        => ( list(_1,nlist(int)), int(_X), rt153(T), rt153(_2), size(ub,_1,length(_1)), size(ub,_X,int(_X)), size(ub,T,length(_2)+length(_1)), size(ub,_2,length(_2)) )
         + cost(ub,exectime_model4,737.0203590950605*length(_1)+469.5480920740583).

append_elem([],_X,T,T).
append_elem([L|Ls],X,[[X|L]|Rs],T) :-
        append_elem(Ls,X,Rs,T).


:- regtype rt154/1.

rt154([]).
rt154([A|B]) :-
        int(A),
        nlist(B,int).


:- regtype rt153/1.

rt153([A|B]) :-
        rt154(A),
        list(B,nlist(int)).


