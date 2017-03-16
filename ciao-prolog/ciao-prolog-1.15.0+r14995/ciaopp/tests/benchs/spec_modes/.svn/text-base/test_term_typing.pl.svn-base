:- module(test_term_typing,[
      	p_var/0,
      	p_nonvar/0,
    	p_atom/0,
    	p_integer/0,
    	p_float/0 ,
     	p_number/0,
  	p_atomic/0
   	, 
	p_ground/0,
	p_ground/1
	],[]).

%% var

p_var:-
	var(X),
	q_yes(X).
p_var:-
	X = 2,
	var(X),
	q_no(X).
p_var:-
	var(1),
	q_no(_X).
%% nonvar
p_nonvar:-
	nonvar(X),
	q_no(X).
p_nonvar:-
	X = 2,
	nonvar(X),
	q_yes(X).
p_nonvar:-
	nonvar(1),
	q_yes(_X).

%% atom
p_atom:-
	atm(X),
	q_no(X).
p_atom:-
	atom(X),
	q_no(X).
p_atom:-
	X = a,
	atom(X),
	q_yes(X).
p_atom:-
	atom(a),
	q_yes(_X).
p_atom:-
	atom(2),
	q_no(_X).
p_atom:-
	X = 2,
	atom(X),
	q_no(X).

%% integer
p_integer:-
	integer(X),
	q_no(X).
p_integer:-
	X = 2,
	integer(X),
	q_yes(X).
p_integer:-
	integer(2),
	q_yes(_X).

%% float
p_float:-
	float(X),
	q_no(X).
p_float:-
	X = 2.3,
	float(X),
	q_yes(X).
p_float:-
	float(2.3),
	q_yes(_X).

%% number
p_number:-
	number(X),
	q_no(X).
p_number:-
	X = 2.3,
	number(X),
	q_yes(X).
p_number:-
	number(2.3),
	q_yes(_X).

%% atomic
p_atomic:-
	atomic(X),
	q_no(X).
p_atomic:-
	X = 2.3,
	atomic(X),
	q_yes(X).
p_atomic:-
	atomic(2.3),
	q_yes(_X).

%% ground
p_ground:-
	ground(X),
	q_no(X).
p_ground:-
	ground([X,3]),
	q_no(X).
p_ground:-
	X=2,
	ground([X,3]),
	q_yes(X).
p_ground:-
	X = 2.3,
	ground(X),
	q_yes(X).
p_ground:-
	ground(2.3),
	q_yes(_X).

p_ground(X):-
	ground([X,3]).

p_ground(X):-
	X=a,
	ground([X,3]),
	q_yes(X).

p_ground(X):-
	ground([X,_Y,3]),
	q_no(X).


q_yes(_X).	
q_yes(_X).

q_no(_X).	
q_no(_X).
