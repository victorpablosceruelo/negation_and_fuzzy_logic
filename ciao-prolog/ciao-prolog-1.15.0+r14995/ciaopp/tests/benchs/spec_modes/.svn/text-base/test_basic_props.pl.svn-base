:- module(test_basic_props,[
	p_term/0 
   ,	p_int/0 
   ,	p_nnegint/0 
   ,	p_flt/0 
   ,	p_num/0 
   ,	p_atm/0 
   ,	p_struct/0 
   ,	p_gnd/0
   ,	p_constant/0
%% ,	p_callable/0 
%% ,	p_operator_specifier/0 
   ,	p_list/0 
%% ,	p_list/0 
%% ,	p_member/0
%% ,	p_sequence/0 
%% ,	p_sequence_or_list/0 
%% ,	p_character_code/0 
%% ,	p_string/0
%% ,	p_predname/0 
%% ,	p_atm_or_atm_list/0
%%       compat/2,inst/2,
%%          iso/1, not_further_inst/2, sideff/2,
%% 	 regtype/1, native/1, native/2,
%% 	 eval/1, equiv/2	
	],[]).

%% term

p_term:-
	term(X),
	q_yes(X).
p_term:-
	X = 2,
	term(X),
	q_yes(X).
p_term:-
	term(1),
	q_yes(_X).
%% int
p_int:-
	int(X),
	q_yes(X).
p_int:-
	integer(X),
	q_yes(X).
p_int:-
	X = 2,
	int(X),
	q_yes(X).
p_int:-
	int(2),
	q_yes(_X).

p_int:-
	X = a,
	int(X),
	q_no(X).

%% nnegint
p_nnegint:-
	nnegint(X),
	q_yes(X).
p_nnegint:-
	X = -2,
	nnegint(X),
	q_no(X).
p_nnegint:-
	nnegint(-2),
	q_no(_X).
p_nnegint:-
	X = 2,
	nnegint(X),
	q_yes(X).
p_nnegint:-
	nnegint(2),
	q_yes(_X).

%% flt
p_flt:-
	flt(X),
	q_yes(X).
p_flt:-
	X = 2.3,
	flt(X),
	q_yes(X).
p_flt:-
	flt(2.3),
	q_yes(_X).

p_flt:-
	flt(a),
	q_no(_X).
p_flt:-
	X = a,
	flt(X),
	q_no(X).

%% num
p_num:-
	num(X),
	q_yes(X).
p_num:-
	X = 2.3,
	num(X),
	q_yes(X).
p_num:-
	num(2.3),
	q_yes(_X).

p_num:-
	X = a,
	num(X),
	q_no(X).
p_num:-
	num(a),
	q_no(_X).

%% atm
p_atm:-
	atm(X),
	q_yes(X).
p_atm:-
	X = a,
	atm(X),
	q_yes(X).
p_atm:-
	atm(a),
	q_yes(_X).
p_atm:-
	atm(2),
	q_no(_X).
p_atm:-
	X = 2,
	atm(X),
	q_no(X).

%% struct
p_struct:-
	struct(X),
	q_yes(X).
p_struct:-
	X = a,
	struct(X),
	q_no(X).
p_struct:-
	struct(a),
	q_no(_X).
p_struct:-
	struct(f(2)),
	q_yes(_X).
p_struct:-
	X = f(2),
	struct(X),
	q_yes(X).

%% gnd
p_gnd:-
	gnd(X),
	q_yes(X).
p_gnd:-
	X = 2.3,
	gnd(X),
	q_yes(X).
p_gnd:-
	gnd(2.3),
	q_yes(_X).

%% constant
p_constant:-
	constant(X),
	q_yes(X).
p_constant:-
	X = 2.3,
	constant(X),
	q_yes(X).
p_constant:-
	constant(2.3),
	q_yes(_X).
p_constant:-
	X = a,
	constant(X),
	q_yes(X).
p_constant:-
	constant(a),
	q_yes(_X).
p_constant:-
	X = f(a),
	constant(X),
	q_no(X).
p_constant:-
	constant(f(a)),
	q_no(_X).

%% list
p_list:-
	list(X).
p_list:-
	X = 2,
	list(X),
	q_no(X).
p_list:-
	list(1),
	q_no(_X).
p_list:-
	X = [2],
	list(X),
	q_yes(X).
p_list:-
	list([2]),
	q_yes(_X).
%% %% float
%% p_float:-
%% 	float(X),
%% 	q_no(X).
%% p_float:-
%% 	X = 2.3,
%% 	float(X),
%% 	q_yes(X).
%% p_float:-
%% 	float(2.3),
%% 	q_yes(_X).
%% 
%% %% number
%% p_number:-
%% 	number(X),
%% 	q_no(X).
%% p_number:-
%% 	X = 2.3,
%% 	number(X),
%% 	q_yes(X).
%% p_number:-
%% 	number(2.3),
%% 	q_yes(_X).
%% 


q_yes(_X).	
q_yes(_X).

q_no(_X).	
q_no(_X).
