:- module(_1,[palindrome/2],[assertions,regtypes,ciaopp(examples(resources(exectimehl))),nativeprops,basicmodes]).

:- export(int_list/1).

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
int_list([A|L]) :-
        int(A),
        int_list(L).

:- entry palindrome(_1,_2)
         : ( int_list(_1), var(_2) ).

:- true pred palindrome(_1,L2)
         : ( int_list(_1), term(L2) )
        => ( list(_1,character_code), list(L2,character_code) ).

:- true pred palindrome(_1,L2)
         : ( mshare([[L2]]), var(L2), ground([_1]) )
        => ground([_1,L2]).

:- true pred palindrome(_1,L2)
         : ( int_list(_1), var(L2) )
        => ( list(_1,character_code), list(L2,character_code) )
         + ( not_fails, covered ).

:- true pred palindrome(_1,L2)
         : ( int_list(_1), var(L2) )
        => ( list(_1,character_code), list(L2,character_code), size(lb,_1,length(_1)), size(lb,L2,exp(2,length(_1))-1.0) )
         + cost(lb,exectime_model4,541.558024186782*(exp(2,length(_1)-1)*length(_1))+649.7968529671534*exp(2,length(_1))-296.2450976039701).

:- true pred palindrome(_1,L2)
         : ( int_list(_1), var(L2) )
        => ( list(_1,character_code), list(L2,character_code), size(ub,_1,length(_1)), size(ub,L2,exp(2,length(_1))-1.0) )
         + cost(ub,exectime_model4,577.4765726713725*(exp(2,length(_1)-1)*length(_1))+694.4623252667227*exp(2,length(_1))-317.3284193200022).

palindrome([],[]).
palindrome([First|L1],L2) :-
        palindrome(L1,Ls2),
        palindrome(L1,Lg2),
        append(Ls2,[First|Lg2],L2).

:- true pred append(_1,L,_2)
         : ( list(_1,character_code), rt5(L), term(_2) )
        => ( list(_1,character_code), rt5(L), rt5(_2) ).

:- true pred append(_1,L,_2)
         : ( mshare([[_2]]), var(_2), ground([_1,L]) )
        => ground([_1,L,_2]).

:- true pred append(_1,L,_2)
         : ( list(_1,character_code), rt5(L), var(_2) )
        => ( list(_1,character_code), rt5(L), rt5(_2) )
         + ( not_fails, covered ).

:- true pred append(_1,L,_2)
         : ( list(_1,character_code), rt5(L), var(_2) )
        => ( list(_1,character_code), rt5(L), rt5(_2), size(lb,_1,length(_1)), size(lb,L,length(L)), size(lb,_2,length(L)+length(_1)) )
         + cost(lb,exectime_model4,541.558024186782*length(_1)+378.4291019696336).

:- true pred append(_1,L,_2)
         : ( list(_1,character_code), rt5(L), var(_2) )
        => ( list(_1,character_code), rt5(L), rt5(_2), size(ub,_1,length(_1)), size(ub,L,length(L)), size(ub,_2,length(L)+length(_1)) )
         + cost(ub,exectime_model4,577.4765726713725*length(_1)+407.2549684374599).

append([],L,L).
append([H|L],L1,[H|R]) :-
        append(L,L1,R).


:- regtype rt5/1.

rt5([A|B]) :-
        int(A),
        list(B,character_code).


