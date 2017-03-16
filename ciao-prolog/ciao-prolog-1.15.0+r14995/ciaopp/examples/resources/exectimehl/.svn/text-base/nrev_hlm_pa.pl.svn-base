:- module(_1,[nrev/2],[assertions,regtypes,nativeprops,ciaopp(examples(resources(exectimehl))),basicmodes]).

:- export(short_int_list/1).

:- prop short_int_list/1+regtype.

:- prop short_int_list(_1)
         + regtype.

:- true pred short_int_list(_1)
         : term(_1)
        => list(_1,character_code).

:- true pred short_int_list(_1)
         : mshare([[_1]])
        => ground([_1]).

:- true pred short_int_list(_1)
         : term(_1)
        => list(_1,character_code)
         + ( possibly_fails, not_covered ).

short_int_list([]).
short_int_list([A|B]) :-
        int(A),
        short_int_list(B).

:- entry nrev(_1,_2)
         : ( short_int_list(_1), var(_2) ).

:- true pred nrev(_1,C)
         : ( short_int_list(_1), term(C) )
        => ( list(_1,character_code), list(C,character_code) ).

:- true pred nrev(_1,C)
         : ( mshare([[C]]), var(C), ground([_1]) )
        => ground([_1,C]).

:- true pred nrev(_1,C)
         : ( short_int_list(_1), var(C) )
        => ( list(_1,character_code), list(C,character_code) )
         + ( not_fails, covered ).

:- true pred nrev(_1,C)
         : ( short_int_list(_1), var(C) )
        => ( list(_1,character_code), list(C,character_code), size(lb,_1,length(_1)), size(lb,C,length(_1)) )
         + cost(lb,exectime_model4,270.779012093391*exp(length(_1),2)+634.3037040258087*length(_1)+353.5517553631832).

:- true pred nrev(_1,C)
         : ( short_int_list(_1), var(C) )
        => ( list(_1,character_code), list(C,character_code), size(ub,_1,length(_1)), size(ub,C,length(_1)) )
         + cost(ub,exectime_model4,288.7382863356863*exp(length(_1),2)+675.6839429619188*length(_1)+377.1339059467205).

nrev([],[]).
nrev([B|A],C) :-
        nrev(A,D),
        append(D,[B],C).

:- true pred append(_1,A,_2)
         : ( list(_1,character_code), rt1(A), term(_2) )
        => ( list(_1,character_code), rt5(A), rt5(_2) ).

:- true pred append(_1,A,_2)
         : ( mshare([[_2]]), var(_2), ground([_1,A]) )
        => ground([_1,A,_2]).

:- true pred append(_1,A,_2)
         : ( list(_1,character_code), rt1(A), var(_2) )
        => ( list(_1,character_code), rt5(A), rt5(_2) )
         + ( not_fails, covered ).

:- true pred append(_1,A,_2)
         : ( list(_1,character_code), rt1(A), var(_2) )
        => ( list(_1,character_code), rt5(A), rt5(_2), size(lb,_1,length(_1)), size(lb,A,length(A)), size(lb,_2,length(A)+length(_1)) )
         + cost(lb,exectime_model4,541.558024186782*length(_1)+378.4291019696336).

:- true pred append(_1,A,_2)
         : ( list(_1,character_code), rt1(A), var(_2) )
        => ( list(_1,character_code), rt5(A), rt5(_2), size(ub,_1,length(_1)), size(ub,A,length(A)), size(ub,_2,length(A)+length(_1)) )
         + cost(ub,exectime_model4,577.4765726713725*length(_1)+407.2549684374599).

append([],A,A).
append([B|A],D,[B|C]) :-
        append(A,D,C).


:- regtype rt1/1.

rt1([A]) :-
        int(A).


:- regtype rt5/1.

rt5([A|B]) :-
        int(A),
        list(B,character_code).


