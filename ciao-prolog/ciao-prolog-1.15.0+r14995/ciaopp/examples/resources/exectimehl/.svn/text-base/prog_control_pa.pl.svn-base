:- module(_1,[list1/1,listn/2,listt/2,listr/1],[assertions,regtypes,nativeprops,ciaopp(examples(resources(exectimehl))),basicmodes]).

:- prop int_list/1+regtype.

:- prop int_list(_1)
         + regtype.

int_list([]).
int_list([A|B]) :-
        int(A),
        int_list(B).

:- entry list1(_1)
         : int_list(_1).

:- true pred list1(_1)
         : int_list(_1)
        => list(_1,character_code).

:- true pred list1(_1)
         : ground([_1])
        => ground([_1]).

:- true pred list1(_1)
         : int_list(_1)
        => list(_1,character_code)
         + ( not_fails, covered ).

:- true pred list1(_1)
         : int_list(_1)
        => ( list(_1,character_code), size(lb,_1,length(_1)) )
         + cost(lb,exectime_model4,339.4698396348448*length(_1)+270.3780660499357).

:- true pred list1(_1)
         : int_list(_1)
        => ( list(_1,character_code), size(ub,_1,length(_1)) )
         + cost(ub,exectime_model4,359.5858325138823*length(_1)+286.6148910670593).

list1([]).
list1([_1|A]) :-
        list1(A).

:- entry listr(_1)
         : int_list(_1).

:- true pred listr(_1)
         : int_list(_1)
        => list(_1,character_code).

:- true pred listr(_1)
         : ground([_1])
        => ground([_1]).

:- true pred listr(_1)
         : int_list(_1)
        => list(_1,character_code)
         + ( not_fails, covered ).

:- true pred listr(_1)
         : int_list(_1)
        => ( list(_1,character_code), size(lb,_1,length(_1)) )
         + cost(lb,exectime_model4,1068.341473443129*length(_1)+270.3780660499357).

:- true pred listr(_1)
         : int_list(_1)
        => ( list(_1,character_code), size(ub,_1,length(_1)) )
         + cost(ub,exectime_model4,1124.34583590445*length(_1)+286.6148910670593).

listr([]).
listr([_1|A]) :-
        listr(A),
        dummy,
        dummy,
        dummy,
        dummy.

:- true pred dummy.

:- true pred dummy.

:- true pred dummy
         + ( not_fails, covered ).

:- true pred dummy
         + cost(lb,exectime_model4,182.217908452071).

:- true pred dummy
         + cost(ub,exectime_model4,191.190000847642).

dummy.

:- entry listn(_1,_2)
         : ( int_list(_1), var(_2) ).

:- true pred listn(_1,_2)
         : ( int_list(_1), term(_2) )
        => ( list(_1,character_code), list(_2,character_code) ).

:- true pred listn(_1,_2)
         : ( mshare([[_2]]), var(_2), ground([_1]) )
        => ground([_1,_2]).

:- true pred listn(_1,_2)
         : ( int_list(_1), var(_2) )
        => ( list(_1,character_code), list(_2,character_code) )
         + ( not_fails, covered ).

:- true pred listn(_1,_2)
         : ( int_list(_1), var(_2) )
        => ( list(_1,character_code), list(_2,character_code), size(lb,_1,length(_1)), size(lb,_2,length(_1)) )
         + cost(lb,exectime_model4,486.1315741249102*length(_1)+353.5517553631832).

:- true pred listn(_1,_2)
         : ( int_list(_1), var(_2) )
        => ( list(_1,character_code), list(_2,character_code), size(ub,_1,length(_1)), size(ub,_2,length(_1)) )
         + cost(ub,exectime_model4,515.1834490347741*length(_1)+377.1339059467205).

listn([],[]).
listn([A|B],[A|C]) :-
        listn(B,C).

:- entry listt(_1,_2)
         : ( int_list(_1), var(_2) ).

:- true pred listt(_1,_2)
         : ( int_list(_1), term(_2) )
        => ( list(_1,character_code), list(_2,^(f('basic_props:int','basic_props:int','basic_props:int','basic_props:int','basic_props:int','basic_props:int','basic_props:int','basic_props:int','basic_props:int','basic_props:int'))) ).

:- true pred listt(_1,_2)
         : ( mshare([[_2]]), var(_2), ground([_1]) )
        => ground([_1,_2]).

:- true pred listt(_1,_2)
         : ( int_list(_1), var(_2) )
        => ( list(_1,character_code), list(_2,^(f('basic_props:int','basic_props:int','basic_props:int','basic_props:int','basic_props:int','basic_props:int','basic_props:int','basic_props:int','basic_props:int','basic_props:int'))) )
         + ( not_fails, covered ).

:- true pred listt(_1,_2)
         : ( int_list(_1), var(_2) )
        => ( list(_1,character_code), list(_2,^(f('basic_props:int','basic_props:int','basic_props:int','basic_props:int','basic_props:int','basic_props:int','basic_props:int','basic_props:int','basic_props:int','basic_props:int'))), size(lb,_1,length(_1)), size(lb,_2,length(_1)) )
         + cost(lb,exectime_model4,834.120903464421*length(_1)+353.5517553631832).

:- true pred listt(_1,_2)
         : ( int_list(_1), var(_2) )
        => ( list(_1,character_code), list(_2,^(f('basic_props:int','basic_props:int','basic_props:int','basic_props:int','basic_props:int','basic_props:int','basic_props:int','basic_props:int','basic_props:int','basic_props:int'))), size(ub,_1,length(_1)), size(ub,_2,length(_1)) )
         + cost(ub,exectime_model4,872.7485183867858*length(_1)+377.1339059467205).

listt([],[]).
listt([A|B],[f(A,A,A,A,A,A,A,A,A,A)|C]) :-
        listt(B,C).


