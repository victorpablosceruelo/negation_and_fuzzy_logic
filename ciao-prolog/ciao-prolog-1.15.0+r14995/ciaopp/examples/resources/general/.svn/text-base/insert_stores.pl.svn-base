:- module(insert_stores, [main/2, delta_stores/2],
	    [assertions, nativeprops, regtypes, resdefs]).

:- use_module(resources(resources_basic)).
% Note loop_insert is reduced to two lists as arguments instead four. The
% problem is that recurrence solver does not support equations with more than
% two variables.

:- entry main(S, M) : (stores(S), managers(M)).
main(Stores, Managers) :-
	list_store_id(L1),
	list_store_address(L2),
%  	list_cof_arrays(L3),
%  	list_queries(L4),
%       loop_insert(L1,L2,L3,L4,Stores),
	process_stores(L1, L2, Stores, Managers).

process_stores(L1, L2, Stores, Managers) :-
	loop_insert(L1, L2, Stores),
	select_stores(Stores, Table),
	print_store_info(Table, Managers).

list_store_id([100001, 100002, 100003, 100004, 100005]).
list_store_address(['888 Main Street Rancho Alegre CA 94049',
		'1560 Alder Ochos_Pinos CA 94049',
		'4344 First_Street Verona CA 94545',
		'321 Sandy_Way La_Playa CA 94544',
		'1000 Clover_Road Happyville CA 90566']).
/*
list_cof_arrays([
'Colombian,French_Roast,Espresso,Colombian_Decaf,French_Roast_Decaf',
'Colombian,French_Roast,Espresso,Colombian_Decaf,French_Roast_Decaf,Kona,Kona_Decaf',
'Colombian,French_Roast,Espresso,Colombian_Decaf,French_Roast_Decaf,Kona,Kona_Decaf',
'Colombian,French_Roast,Espresso,Colombian_Decaf,French_Roast_Decaf,Kona,Kona_Decaf', 'Colombian,French_Roast,Espresso,Colombian_Decaf,French_Roast_Decaf'
	    ]).
list_queries(['SELECT OID FROM MANAGERS WHERE MGR_ID = 000001',
		'SELECT OID FROM MANAGERS WHERE MGR_ID = 000001',
		'SELECT OID FROM MANAGERS WHERE MGR_ID = 000002',
		'SELECT OID FROM MANAGERS WHERE MGR_ID = 000002',
		'SELECT OID FROM MANAGERS WHERE MGR_ID = 000003']).
*/

% loop_insert([],[],[],[],_Stores).
% loop_insert([H1|T1],[H2|T2],[H3|T3],[H4|T4],Stores):-
% 	insert(Stores,H1,H2,H3,H4),
% 	loop_insert(T1,T2,T3,T4,Stores).


loop_insert([],      [],      _Stores).
loop_insert([H1|T1], [H2|T2], Stores) :-
	insert(Stores, H1, H2),
	loop_insert(T1, T2, Stores).

:- trust pred insert(S, Id, Addr) : (stores(S), store_id(Id), address(Addr)).
:- trust comp insert(S, Id, Addr) + ( cost(ub, ins_stores, 1),
	    cost(ub, acc_stores, 1) ).
insert(_S, _Id, _Addr).


% :- trust pred insert(S,Id,Addr,Coff_Array,Query): (stores(S),store_id(Id),address(Addr),cof_array(Coff_Array),
%                                                    query(Query)).
% :- trust comp insert(S,Id,Addr,Coff_Array,Query) + ( cost(ub,ins_stores,1),
%                                                      cost(ub,acc_stores,1)).
% insert(_S,_Id,_Addr,_Coff_Array,_Query).

% insert into STORES values (100003, ADDRESS(4344, 'First_Street', 'Verona','CA', '94545'), 
% 			   COF_ARRAY('Colombian', 'French_Roast', 'Espresso', 
% 			             'Colombian_Decaf', 'French_Roast_Decaf', 
% 				     'Kona', 'Kona_Decaf'), 
% 			  "(SELECT OID FROM MANAGERS WHERE MGR_ID = 000002))")

:- trust pred select_stores(S, O) : (stores(S), var(O)) => (stores(S)).
:- trust comp select_stores(S, O) + ( cost(ub, ins_stores, 0),
	    cost(ub, acc_stores, length(S)) ).
select_stores(Stores, Stores).
% select *
% from STORES

:- trust pred select_manager_name(M, Id, N) :
	(managers(M), manager_id(Id), var(N)) => (manager(N)).
:- trust comp select_manager_name(M, Id, N)
	+ (cost(ub, ins_stores, 0), cost(ub, acc_stores, 0)).
select_manager_name(_Managers, _Man_Id, manager_name).
% select MANAGER 
% from MANAGERS
% where OID = Man_Id


print_store_info([],                                 _).
print_store_info([(Id, Addr, Cof_array, Man_Id)|Cs], Managers) :-
	print(Id),
	print(Addr),
	print(Cof_array),
	select_manager_name(Managers, Man_Id, Man_Name),
	print(Man_Name),
	print_store_info(Cs, Managers).
print(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                    Database Tables
%  -------------------          --------------            
% |    STORES         |        |   MANAGERS   |  
%  -------------------          --------------   
% | store_id          |        |  manager     |
% | address           |        |  manager_id  |     
% | cof_array         |         --------------
% | manager_id        |           
%  -------------------            
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- regtype store_id/1.
store_id(N) :- num(N).

:- regtype address/1.
address(S) :- atm(S).

:- regtype cof_array/1.
cof_array(X) :- atm(X).

:- regtype manager_id/1.
manager_id(S) :- atm(S).

:- regtype stores/1.
stores([]).
stores([(Id, Addr, Cof_Array, Man_Id)|Xs]) :-
	store_id(Id),
	address(Addr),
	cof_array(Cof_Array),
	manager_id(Man_Id),
	stores(Xs).

% :- regtype query/1.
% query(X) :- atm(X).

:- regtype manager/1.
manager(X) :- atm(X).

:- regtype managers/1.
managers([]).
managers([(Name, Id)|Xs]) :-
	manager(Name),
	manager_id(Id),
	managers(Xs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- load_resource_module(insert_stores).

:- resource ins_stores.
:- resource acc_stores.

:- head_cost(ub, ins_stores, delta_stores).
:- literal_cost(ub, ins_stores, 0).
:- head_cost(ub, acc_stores, delta_stores).
:- literal_cost(ub, acc_stores, 0).

% delta_stores(LitInfo, inf ) :-  
% 	litinfo_get_lit( LitInfo, Head ),
% 	( Head = 'insert_stores:insert'(_,_,_) ; 
%           Head = 'insert_stores:select_stores'(_,_) ; 
%           Head = 'insert_stores:select_manager_name'(_,_,_)),
% 	!.
delta_stores(LitInfo, inf) :-
	litinfo_get_lit(LitInfo, Head),
	is_pred(Head),
	!.
delta_stores(_LitInfo, 0).

is_pred('insert_stores:insert'(_, _, _)).
is_pred('insert_stores:select_stores'(_, _)).
is_pred('insert_stores:select_manager_name'(_, _, _)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
