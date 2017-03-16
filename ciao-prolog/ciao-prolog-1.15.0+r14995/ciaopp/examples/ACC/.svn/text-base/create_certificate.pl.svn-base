:- module(_,[create/0],[]).

:- use_module(ciaopp(ciaopp)).

create:-
	set_pp_flag(dump_pred,nodep),
	set_pp_flag(dump_pp,off),
	set_pp_flag(fixpoint,di),
	program(Program),
	module(Program),
	domain(Domain),
	analyze(Domain),
	atom_concat(Program,'.cert',Cert_Name),
	dump(Cert_Name).
	
program(deriv).
domain(shfr).
	
	
