:- module(_,[check/0],[]).

:- use_module(ciaopp(ciaopp)).
:- use_module(plai(fixpo_ops)).

check:-
	program(Program),
	module(Program),
	atom_concat(Program,'.cert',Cert_Name),
	restore(Cert_Name),
	store_previous_analysis(Domain),
	domain(Domain),
	checker(Checker),
	push_pp_flag(fixpoint,Checker),
	analyze(Domain),
	push_pp_flag(fixpoint,Checker).
	
program(deriv).
domain(shfr).
checker(check_di3).
	
	
