:- module(blackhole, 
	[
	    times/2,
	    p/0
	], []).

:- use_package(library(tabling)).
:- const_table_module(library(difference_constraints(difference_constraints_tab))).
:- use_package(library(difference_constraints)).

:- use_module(library(write),[print/1]).

:- use_module(library(lists), [length/2, reverse/2]).

:- include(times).

p :-
	length(X, 52),
	length(Y, 52),
	get_elem(1,Y,Y_1),
	Y_1 #= 1,
	get_elem(1,X,X_1),
	X_1 #= 1,
	iter_layout(17,X),
	mylabeling(1,Y,X),
	print_sol(Y).

print_sol([]) :- !.
print_sol([H|R]) :- 
	difference_constraints_get_value(H,V),
	simb(V,Simb),
	display(Simb), display(' '),
	print_sol(R).

iter_layout(1,_) :- !.
iter_layout(N,X) :- 
	N1 is N - 1,
	layout(N,[A,B,C]),
	numb(A,Na), numb(B,Nb), numb(C,Nc),
	get_elem(Na,X,Xa), get_elem(Nb,X,Xb), get_elem(Nc,X,Xc),
	Xa - Xb #=< -1,
	Xb - Xc #=< -1,
	iter_layout(N1,X).

mylabeling(N,[Y1,Y2],X) :- !,
	neighbours(Y1,Y2),
	difference_constraints_get_value(Y2,V),
	put_elem(V,X,N).


mylabeling(N,[Y1,Y2|R],X) :- !,
	neighbours(Y1,Y2),
	difference_constraints_get_value(Y2,V),
	check_X(V,X),
	put_elem(V,X,N),
	N1 is N + 1,
	mylabeling(N1,[Y2|R],X).

check_X(V,_) :-
	simb(V,S),
	layout(_,[S,_,_]),
	!.
check_X(V,X) :-
	simb(V,S),
	layout(_,[PrevS,S,_]),
	!,
	numb(PrevS,PrevV),
	get_elem(PrevV,X,Prev),
	difference_constraints_get_value(Prev,_).
check_X(V,X) :-
	simb(V,S),
	layout(_,[_,PrevS,S]),
	numb(PrevS,PrevV),
	get_elem(PrevV,X,Prev),
	difference_constraints_get_value(Prev,_).


layout(1, ['7p','Jc','2c']).
layout(2, ['3r','4p','5t']).
layout(3, ['5p','Kr','Tc']).
layout(4, ['Tt','Qr','3t']).
layout(5, ['6p','Tp','8t']).
layout(6, ['Jt','Tr','Ac']).
layout(7, ['Jp','At','2p']).
layout(8, ['4r','9p','8p']).
layout(9, ['7c','9c','5r']).
layout(10,['9r','Qp','Kc']).
layout(11,['7r','Kp','Qt']).
layout(12,['2t','Qc','4c']).
layout(13,['3c','5c','6t']).
layout(14,['7t','Kt','6c']).
layout(15,['3p','8c','Ar']).
layout(16,['6r','Jr','4t']).
layout(17,['9t','2r','8r']).

simb(1, 'Ap').
simb(2, '2p').
simb(3, '3p').
simb(4, '4p').
simb(5, '5p').
simb(6, '6p').
simb(7, '7p').
simb(8, '8p').
simb(9, '9p').
simb(10,'Tp').
simb(11,'Jp').
simb(12,'Qp').
simb(13,'Kp').
simb(14,'Ac').
simb(15,'2c').
simb(16,'3c').
simb(17,'4c').
simb(18,'5c').
simb(19,'6c').
simb(20,'7c').
simb(21,'8c').
simb(22,'9c').
simb(23,'Tc').
simb(24,'Jc').
simb(25,'Qc').
simb(26,'Kc').
simb(27,'At').
simb(28,'2t').
simb(29,'3t').
simb(30,'4t').
simb(31,'5t').
simb(32,'6t').
simb(33,'7t').
simb(34,'8t').
simb(35,'9t').
simb(36,'Tt').
simb(37,'Jt').
simb(38,'Qt').
simb(39,'Kt').
simb(40,'Ar').
simb(41,'2r').
simb(42,'3r').
simb(43,'4r').
simb(44,'5r').
simb(45,'6r').
simb(46,'7r').
simb(47,'8r').
simb(48,'9r').
simb(49,'Tr').
simb(50,'Jr').
simb(51,'Qr').
simb(52,'Kr').

numb('Ap', 1).
numb('2p', 2).
numb('3p', 3).
numb('4p', 4).
numb('5p', 5).
numb('6p', 6).
numb('7p', 7).
numb('8p', 8).
numb('9p', 9).
numb('Tp',10).
numb('Jp',11).
numb('Qp',12).
numb('Kp',13).
numb('Ac',14).
numb('2c',15).
numb('3c',16).
numb('4c',17).
numb('5c',18).
numb('6c',19).
numb('7c',20).
numb('8c',21).
numb('9c',22).
numb('Tc',23).
numb('Jc',24).
numb('Qc',25).
numb('Kc',26).
numb('At',27).
numb('2t',28).
numb('3t',29).
numb('4t',30).
numb('5t',31).
numb('6t',32).
numb('7t',33).
numb('8t',34).
numb('9t',35).
numb('Tt',36).
numb('Jt',37).
numb('Qt',38).
numb('Kt',39).
numb('Ar',40).
numb('2r',41).
numb('3r',42).
numb('4r',43).
numb('5r',44).
numb('6r',45).
numb('7r',46).
numb('8r',47).
numb('9r',48).
numb('Tr',49).
numb('Jr',50).
numb('Qr',51).
numb('Kr',52).

neighbours(A,B) :-
	A #= 1,
	neighbours_A(B).
neighbours(A,B) :-
	A #= 2,
	neighbours_2(B).
neighbours(A,B) :-
	A #= 3,
	neighbours_3(B).
neighbours(A,B) :-
	A #= 4,
	neighbours_4(B).
neighbours(A,B) :-
	A #= 5,
	neighbours_5(B).
neighbours(A,B) :-
	A #= 6,
	neighbours_6(B).
neighbours(A,B) :-
	A #= 7,
	neighbours_7(B).
neighbours(A,B) :-
	A #= 8,
	neighbours_8(B).
neighbours(A,B) :-
	A #= 9,
	neighbours_9(B).
neighbours(A,B) :-
	A #= 10,
	neighbours_T(B).
neighbours(A,B) :-
	A #= 11,
	neighbours_J(B).
neighbours(A,B) :-
	A #= 12,
	neighbours_Q(B).
neighbours(A,B) :-
	A #= 13,
	neighbours_K(B).
neighbours(A,B) :-
	A #= 14,
	neighbours_A(B).
neighbours(A,B) :-
	A #= 15,
	neighbours_2(B).
neighbours(A,B) :-
	A #= 16,
	neighbours_3(B).
neighbours(A,B) :-
	A #= 17,
	neighbours_4(B).
neighbours(A,B) :-
	A #= 18,
	neighbours_5(B).
neighbours(A,B) :-
	A #= 19,
	neighbours_6(B).
neighbours(A,B) :-
	A #= 20,
	neighbours_7(B).
neighbours(A,B) :-
	A #= 21,
	neighbours_8(B).
neighbours(A,B) :-
	A #= 22,
	neighbours_9(B).
neighbours(A,B) :-
	A #= 23,
	neighbours_T(B).
neighbours(A,B) :-
	A #= 24,
	neighbours_J(B).
neighbours(A,B) :-
	A #= 25,
	neighbours_Q(B).
neighbours(A,B) :-
	A #= 26,
	neighbours_K(B).
neighbours(A,B) :-
	A #= 27,
	neighbours_A(B).
neighbours(A,B) :-
	A #= 28,
	neighbours_2(B).
neighbours(A,B) :-
	A #= 29,
	neighbours_3(B).
neighbours(A,B) :-
	A #= 30,
	neighbours_4(B).
neighbours(A,B) :-
	A #= 31,
	neighbours_5(B).
neighbours(A,B) :-
	A #= 32,
	neighbours_6(B).
neighbours(A,B) :-
	A #= 33,
	neighbours_7(B).
neighbours(A,B) :-
	A #= 34,
	neighbours_8(B).
neighbours(A,B) :-
	A #= 35,
	neighbours_9(B).
neighbours(A,B) :-
	A #= 36,
	neighbours_T(B).
neighbours(A,B) :-
	A #= 37,
	neighbours_J(B).
neighbours(A,B) :-
	A #= 38,
	neighbours_Q(B).
neighbours(A,B) :-
	A #= 39,
	neighbours_K(B).
neighbours(A,B) :-
	A #= 40,
	neighbours_A(B).
neighbours(A,B) :-
	A #= 41,
	neighbours_2(B).
neighbours(A,B) :-
	A #= 42,
	neighbours_3(B).
neighbours(A,B) :-
	A #= 43,
	neighbours_4(B).
neighbours(A,B) :-
	A #= 44,
	neighbours_5(B).
neighbours(A,B) :-
	A #= 45,
	neighbours_6(B).
neighbours(A,B) :-
	A #= 46,
	neighbours_7(B).
neighbours(A,B) :-
	A #= 47,
	neighbours_8(B).
neighbours(A,B) :-
	A #= 48,
	neighbours_9(B).
neighbours(A,B) :-
	A #= 49,
	neighbours_T(B).
neighbours(A,B) :-
	A #= 50,
	neighbours_J(B).
neighbours(A,B) :-
	A #= 51,
	neighbours_Q(B).
neighbours(A,B) :-
	A #= 52,
	neighbours_K(B).

neighbours_A(A) :-
	A #= 2.
neighbours_A(A) :-
	A #= 13.
neighbours_A(A) :-
	A #= 15.
neighbours_A(A) :-
	A #= 26.
neighbours_A(A) :-
	A #= 28.
neighbours_A(A) :-
	A #= 39.
neighbours_A(A) :-
	A #= 41.
neighbours_A(A) :-
	A #= 52.

neighbours_2(A) :-
	A #= 3.
neighbours_2(A) :-
	A #= 14.
neighbours_2(A) :-
	A #= 16.
neighbours_2(A) :-
	A #= 27.
neighbours_2(A) :-
	A #= 29.
neighbours_2(A) :-
	A #= 40.
neighbours_2(A) :-
	A #= 42.
neighbours_2(A) :-
	A #= 1.

neighbours_3(A) :-
	A #= 4.
neighbours_3(A) :-
	A #= 15.
neighbours_3(A) :-
	A #= 17.
neighbours_3(A) :-
	A #= 28.
neighbours_3(A) :-
	A #= 30.
neighbours_3(A) :-
	A #= 41.
neighbours_3(A) :-
	A #= 43.
neighbours_3(A) :-
	A #= 2.

neighbours_4(A) :-
	A #= 4.
neighbours_4(A) :-
	A #= 16.
neighbours_4(A) :-
	A #= 18.
neighbours_4(A) :-
	A #= 29.
neighbours_4(A) :-
	A #= 31.
neighbours_4(A) :-
	A #= 42.
neighbours_4(A) :-
	A #= 44.
neighbours_4(A) :-
	A #= 3.

neighbours_5(A) :-
	A #= 6.
neighbours_5(A) :-
	A #= 17.
neighbours_5(A) :-
	A #= 19.
neighbours_5(A) :-
	A #= 30.
neighbours_5(A) :-
	A #= 32.
neighbours_5(A) :-
	A #= 43.
neighbours_5(A) :-
	A #= 45.
neighbours_5(A) :-
	A #= 4.

neighbours_6(A) :-
	A #= 7.
neighbours_6(A) :-
	A #= 18.
neighbours_6(A) :-
	A #= 20.
neighbours_6(A) :-
	A #= 31.
neighbours_6(A) :-
	A #= 33.
neighbours_6(A) :-
	A #= 44.
neighbours_6(A) :-
	A #= 46.
neighbours_6(A) :-
	A #= 5.

neighbours_7(A) :-
	A #= 8.
neighbours_7(A) :-
	A #= 19.
neighbours_7(A) :-
	A #= 21.
neighbours_7(A) :-
	A #= 32.
neighbours_7(A) :-
	A #= 34.
neighbours_7(A) :-
	A #= 45.
neighbours_7(A) :-
	A #= 47.
neighbours_7(A) :-
	A #= 6.

neighbours_8(A) :-
	A #= 9.
neighbours_8(A) :-
	A #= 20.
neighbours_8(A) :-
	A #= 22.
neighbours_8(A) :-
	A #= 33.
neighbours_8(A) :-
	A #= 35.
neighbours_8(A) :-
	A #= 46.
neighbours_8(A) :-
	A #= 48.
neighbours_8(A) :-
	A #= 7.

neighbours_9(A) :-
	A #= 10.
neighbours_9(A) :-
	A #= 21.
neighbours_9(A) :-
	A #= 23.
neighbours_9(A) :-
	A #= 34.
neighbours_9(A) :-
	A #= 36.
neighbours_9(A) :-
	A #= 47.
neighbours_9(A) :-
	A #= 49.
neighbours_9(A) :-
	A #= 8.

neighbours_T(A) :-
	A #= 11.
neighbours_T(A) :-
	A #= 22.
neighbours_T(A) :-
	A #= 24.
neighbours_T(A) :-
	A #= 35.
neighbours_T(A) :-
	A #= 37.
neighbours_T(A) :-
	A #= 48.
neighbours_T(A) :-
	A #= 50.
neighbours_T(A) :-
	A #= 9.

neighbours_J(A) :-
	A #= 12.
neighbours_J(A) :-
	A #= 23.
neighbours_J(A) :-
	A #= 25.
neighbours_J(A) :-
	A #= 36.
neighbours_J(A) :-
	A #= 38.
neighbours_J(A) :-
	A #= 49.
neighbours_J(A) :-
	A #= 51.
neighbours_J(A) :-
	A #= 10.

neighbours_Q(A) :-
	A #= 13.
neighbours_Q(A) :-
	A #= 24.
neighbours_Q(A) :-
	A #= 26.
neighbours_Q(A) :-
	A #= 37.
neighbours_Q(A) :-
	A #= 39.
neighbours_Q(A) :-
	A #= 50.
neighbours_Q(A) :-
	A #= 52.
neighbours_Q(A) :-
	A #= 11.

neighbours_K(A) :-
	A #= 14.
neighbours_K(A) :-
	A #= 25.
neighbours_K(A) :-
	A #= 27.
neighbours_K(A) :-
	A #= 38.
neighbours_K(A) :-
	A #= 40.
neighbours_K(A) :-
	A #= 51.
neighbours_K(A) :-
	A #= 1.
neighbours_K(A) :-
	A #= 12.

get_elem(1, [E|_],E) :- !.
get_elem(N, [_|R],E) :- 
	N1 is N - 1,
	get_elem(N1,R,E).

put_elem(1, [E|_], C) :- !,
	E #= C.
	
put_elem(N, [_|R], C) :- 
	N1 is N - 1,
	put_elem(N1,R,C).
