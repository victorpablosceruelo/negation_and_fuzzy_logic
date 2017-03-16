:- module(_1,[p/0],[assertions,ciaopp(tests(resources)),predefres(res_steps),nativeprops,basicmodes,regtypes]).

:- doc(author,"Edison Mera").

:- doc(module,"The cost of p must not be infinite (in fact is 8).
	If you like to see the behavior supposing that get_list/1 is
	implemented, uncomment get_list/1 implementation and comment
	out the trust and impl_defined declaration.").

:- doc(bug,"Here we wrote 5 as size of the first argument,
	but... What happen if it is a global constant?.  We can have a
	parametric cost analysis!. -- EMM").

:- impl_defined([get_list/1]).

:- entry p.

:- true pred p.

:- true pred p.

:- true pred p
         + ( not_fails, covered ).

:- true pred p
         + cost(lb,steps,8).

:- true pred p
         + cost(ub,steps,8).

p :-
        get_list(L),
        q(L).

:- true pred q(_1)
         : list(_1)
        => list(_1).

:- true pred q(_1)
         : mshare([[_1]])
        => mshare([[_1]]).

:- true pred q(_1)
         : list(_1)
        => list(_1)
         + ( not_fails, covered ).

:- true pred q(_1)
         : list(_1)
        => ( list(_1), size(lb,_1,length(_1)) )
         + cost(lb,steps,length(_1)+1).

:- true pred q(_1)
         : list(_1)
        => ( list(_1), size(ub,_1,length(_1)) )
         + cost(ub,steps,length(_1)+1).

q([]).
q([_1|L]) :-
        q(L).


