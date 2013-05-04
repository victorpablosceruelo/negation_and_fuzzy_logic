:- module(insert_stores_paper, [main/3],
	    [assertions, nativeprops, regtypes,
		ciaopp(tests(resources)),
		library(resdefs(resources_decl))]).

:- use_module(insert_stores_paper_aux,
	    [manager_id/1, store_info/1, stores/1, manager/1, managers/1]).

:- resource ins_stores.
:- resource acc_stores.
:- head_cost(ub, ins_stores, 0).
:- literal_cost(ub, ins_stores, 0).
:- head_cost(ub, acc_stores, 0).
:- literal_cost(ub, acc_stores, 0).

:- impl_defined([list_store/1, insert/3, select_stores/2,
		select_manager_name/3]).

:- entry main(S, L, M):(stores(S), stores(L), managers(M)).
main(Stores, L, Managers) :-
% 	list_store(L),
	loop_insert(L, Stores, Managers),
	select_stores(Stores, Table),
	print_store_info(Table, Managers).

% :- trust pred list_store(A) => (stores(A), size(A, length(A)))
% 	+ ( cost(ub, ins_stores, 0), cost(ub, acc_stores, 0),
% 	    not_fails, is_det ).

loop_insert([],      _Stores, _Managers).
loop_insert([H1|T1], Stores,  Managers) :-
	insert(Stores, Managers, H1),
	loop_insert(T1, Stores, Managers).

print_store_info([],                           _).
print_store_info([store(Id, Addr, Man_Id)|Cs], Managers) :-
	print(Id),
	print(Addr),
	select_manager_name(Managers, Man_Id, Man_Name),
	print(Man_Name),
	print_store_info(Cs, Managers).

print(_).

%%  BEGIN SQL sentences  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- trust pred insert(S, Id, SI) : (stores(S), managers(Id), store_info(SI))
	+ ( cost(ub, ins_stores, 1), cost(ub, acc_stores, 1),
	    not_fails, is_det, relations(inf) ).

:- trust pred select_stores(S, O) :
	(stores(S), var(O)) => (stores(S), stores(O), size(O, length(S)))
	+ ( cost(ub, ins_stores, 0), cost(ub, acc_stores, length(S)),
	    not_fails, is_det, relations(inf) ).

:- trust pred select_manager_name(M, Id, N) :
	(managers(M), manager_id(Id), var(N)) => manager(N)
	+ ( cost(ub, ins_stores, 0), cost(ub, acc_stores, 0),
	    not_fails, is_det, relations(inf) ).
%%  END SQL sentences  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
