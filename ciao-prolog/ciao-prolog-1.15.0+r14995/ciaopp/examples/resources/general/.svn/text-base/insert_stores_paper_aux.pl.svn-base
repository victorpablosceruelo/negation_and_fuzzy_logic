:- module(_, [store_id/1, address/1, manager_id/1, store_info/1, stores/1,
		query/1, manager/1, managers/1],
	    [assertions, nativeprops, regtypes]).


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
% store_id( N ) :- num( N ).
store_id(100001).
store_id(100002).
store_id(100003).
store_id(100004).
store_id(100005).

:- regtype address/1.
% address( S ) :- atm( S ).

address('888 Main Street Rancho Alegre CA 94049').
address('1560 Alder Ochos_Pinos CA 94049').
address('4344 First_Street Verona CA 94545').
address('321 Sandy_Way La_Playa CA 94544').
address('1000 Clover_Road Happyville CA 90566').

:- regtype manager_id/1.
% manager_id( S ) :- int( S ).
manager_id(100001).
manager_id(100002).
manager_id(100003).
manager_id(100004).
manager_id(100005).

:- regtype store_info/1.
store_info(store(A, B, C)) :-
	store_id(A),
	address(B),
	manager_id(C).

:- regtype stores/1.
stores([]).
stores([X|Xs]) :-
	store_info(X),
	stores(Xs).

:- regtype query/1.
query(X) :- atm(X).

:- regtype manager/1.
manager(X) :- atm(X).

:- regtype managers/1.
managers([]).
managers([(Name, Id)|Xs]) :-
	manager(Name),
	manager_id(Id),
	managers(Xs).
