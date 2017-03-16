:- module(_1,[powset/2],[assertions,regtypes,nativeprops,ciaopp(tests(resources)),library(resdefs(resources_decl)),basicmodes]).

:- use_module(power_set_types,[int_list/1]).

:- load_resource_module(power_set_res).

:- resource output_elements.

:- head_cost(ub,output_elements,'power_set:delta_output_elements').

:- literal_cost(ub,output_elements,0).

:- entry powset(A,B)
         : ( int_list(A), var(B) ).

:- true pred powset(A,B)
         : ( int_list(A), term(B) )
        => ( list(A,character_code), rt152(B) ).

:- true pred powset(A,B)
         : ( mshare([[B]]), var(B), ground([A]) )
        => ground([A,B]).

:- true pred powset(A,B)
         : ( int_list(A), var(B) )
        => ( list(A,character_code), rt152(B) )
         + ( not_fails, covered ).

:- true pred powset(A,B)
         : ( int_list(A), var(B) )
        => ( list(A,character_code), rt152(B), size(ub,A,length(A)), size(ub,B,exp(2,length(A))) )
         + cost(ub,output_elements,0.5*exp(2,length(A)+1)).

powset([],[[]]).
powset([X|L],P) :-
        powset(L,P0),
        append_elements(P0,X,P,P0).

:- true pred append_elements(_1,_X,T,_2)
         : ( list(_1,nlist(int)), int(_X), term(T), rt152(_2) )
        => ( list(_1,nlist(int)), int(_X), rt152(T), rt152(_2) ).

:- true pred append_elements(_1,_X,T,_2)
         : ( mshare([[T]]), var(T), ground([_1,_X,_2]) )
        => ground([_1,_X,T,_2]).

:- true pred append_elements(_1,_X,T,_2)
         : ( list(_1,nlist(int)), int(_X), var(T), rt152(_2) )
        => ( list(_1,nlist(int)), int(_X), rt152(T), rt152(_2) )
         + ( not_fails, covered ).

:- true pred append_elements(_1,_X,T,_2)
         : ( list(_1,nlist(int)), int(_X), var(T), rt152(_2) )
        => ( list(_1,nlist(int)), int(_X), rt152(T), rt152(_2), size(ub,_1,length(_1)), size(ub,_X,int(_X)), size(ub,T,length(_2)+length(_1)), size(ub,_2,length(_2)) )
         + cost(ub,output_elements,length(_1)).

append_elements([],_X,T,T).
append_elements([L|Ls],X,[R|Rs],T) :-
        append_element(L,X,R),
        append_elements(Ls,X,Rs,T).

:- true pred append_element(L,X,_1)
         : ( nlist(L,int), int(X), term(_1) )
        => ( nlist(L,int), int(X), rt148(_1) ).

:- true pred append_element(L,X,_1)
         : ( mshare([[_1]]), var(_1), ground([L,X]) )
        => ground([L,X,_1]).

:- true pred append_element(L,X,_1)
         : ( nlist(L,int), int(X), var(_1) )
        => ( nlist(L,int), int(X), rt148(_1) )
         + ( not_fails, covered ).

:- true pred append_element(L,X,_1)
         : ( nlist(L,int), int(X), var(_1) )
        => ( nlist(L,int), int(X), rt148(_1), size(ub,L,size(L)), size(ub,X,int(X)), size(ub,_1,bot) )
         + cost(ub,output_elements,1).

append_element(L,X,[X|L]).


:- regtype rt148/1.

rt148([A|B]) :-
        int(A),
        nlist(B,int).


:- regtype rt153/1.

rt153([]).
rt153([A|B]) :-
        int(A),
        nlist(B,int).


:- regtype rt152/1.

rt152([A|B]) :-
        rt153(A),
        list(B,nlist(int)).


