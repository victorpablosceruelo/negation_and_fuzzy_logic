:- module(_1,[main/3],[assertions,nativeprops,regtypes,ciaopp(tests(resources)),library(resdefs(resources_decl)),basicmodes]).

:- use_module(insert_stores_paper_aux,[manager_id/1,store_info/1,stores/1,manager/1,managers/1]).

:- resource ins_stores.

:- resource acc_stores.

:- head_cost(ub,ins_stores,0).

:- literal_cost(ub,ins_stores,0).

:- head_cost(ub,acc_stores,0).

:- literal_cost(ub,acc_stores,0).

:- impl_defined([list_store/1,insert/3,select_stores/2,select_manager_name/3]).

:- entry main(S,L,M)
         : ( stores(S), stores(L), managers(M) ).

:- true pred main(S,L,M)
         : ( stores(S), stores(L), managers(M) )
        => ( stores(S), stores(L), managers(M) ).

:- true pred main(S,L,M)
         : ground([S,L,M])
        => ground([S,L,M]).

:- true pred main(S,L,M)
         : ( stores(S), stores(L), managers(M) )
        => ( stores(S), stores(L), managers(M) )
         + ( possibly_fails, covered ).

:- true pred main(S,L,M)
         : ( stores(S), stores(L), managers(M) )
        => ( stores(S), stores(L), managers(M), size(ub,S,length(S)), size(ub,L,length(L)), size(ub,M,length(M)) )
         + ( cost(ub,acc_stores,length(L)+length(S)), cost(ub,ins_stores,length(L)) ).

main(Stores,L,Managers) :-
        loop_insert(L,Stores,Managers),
        select_stores(Stores,Table),
        print_store_info(Table,Managers).

:- true pred loop_insert(_1,_Stores,_Managers)
         : ( stores(_1), stores(_Stores), managers(_Managers) )
        => ( stores(_1), stores(_Stores), managers(_Managers) ).

:- true pred loop_insert(_1,_Stores,_Managers)
         : ground([_1,_Stores,_Managers])
        => ground([_1,_Stores,_Managers]).

:- true pred loop_insert(_1,_Stores,_Managers)
         : ( stores(_1), stores(_Stores), managers(_Managers) )
        => ( stores(_1), stores(_Stores), managers(_Managers) )
         + ( not_fails, covered ).

:- true pred loop_insert(_1,_Stores,_Managers)
         : ( stores(_1), stores(_Stores), managers(_Managers) )
        => ( stores(_1), stores(_Stores), managers(_Managers), size(ub,_1,length(_1)), size(ub,_Stores,length(_Stores)), size(ub,_Managers,length(_Managers)) )
         + ( cost(ub,acc_stores,length(_1)), cost(ub,ins_stores,length(_1)) ).

loop_insert([],_Stores,_Managers).
loop_insert([H1|T1],Stores,Managers) :-
        insert(Stores,Managers,H1),
        loop_insert(T1,Stores,Managers).

:- true pred print_store_info(_2,_1)
         : ( stores(_2), managers(_1) )
        => ( stores(_2), managers(_1) ).

:- true pred print_store_info(_2,_1)
         : ground([_2,_1])
        => ground([_2,_1]).

:- true pred print_store_info(_2,_1)
         : ( stores(_2), managers(_1) )
        => ( stores(_2), managers(_1) )
         + ( possibly_fails, not_covered ).

:- true pred print_store_info(_2,_1)
         : ( stores(_2), managers(_1) )
        => ( stores(_2), managers(_1), size(ub,_2,length(_2)), size(ub,_1,length(_1)) )
         + ( cost(ub,acc_stores,0), cost(ub,ins_stores,0) ).

print_store_info([],_1).
print_store_info([store(Id,Addr,Man_Id)|Cs],Managers) :-
        print(Id),
        print(Addr),
        select_manager_name(Managers,Man_Id,Man_Name),
        print(Man_Name),
        print_store_info(Cs,Managers).

:- true pred print(_1)
         : rt12(_1)
        => rt12(_1).

:- true pred print(_1)
         : ground([_1])
        => ground([_1]).

:- true pred print(_1)
         : rt12(_1)
        => rt12(_1)
         + ( not_fails, covered ).

:- true pred print(_1)
         : rt12(_1)
        => ( rt12(_1), size(ub,_1,size(_1)) )
         + ( cost(ub,acc_stores,0), cost(ub,ins_stores,0) ).

print(_1).


:- regtype rt12/1.

rt12(100001).
rt12(100002).
rt12(100003).
rt12(100004).
rt12(100005).
rt12(A) :-
        atm(A).


