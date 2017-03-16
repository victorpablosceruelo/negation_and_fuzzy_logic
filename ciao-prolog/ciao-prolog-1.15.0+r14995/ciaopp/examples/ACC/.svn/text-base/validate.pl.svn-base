% ciaoc -u ~/MySVN/Systems/CiaoDE/ciaopp/paths validate

:- module(_,[main/1],[]).

:- use_module(ciaopp(driver)).
:- use_module(ciaopp(preprocess_flags)).
:- use_module(program(p_dump)).
:- use_module(plai(fixpo_ops)).
:- use_module(infer(infer_db)).

:- use_module(library(messages)).

main(Args):-
	Args = [Program],!,
	module(Program),
	atom_concat(Program,'.cert',Cert_Name),
	restore(Cert_Name),
	domain(Domain),
	store_previous_analysis(Domain),
	checker(Checker),
	push_pp_flag(fixpoint,Checker),
	catch(analyze(Domain),certif_error(X),
        error_message("Certificate and program do not match.")),
	pop_pp_flag(fixpoint),
	(var(X)->
	    acheck
	;
	    halt(-1)).
main([Exec|_]):-
	display('Usage: '),
	display(Exec),
	display(' Filename').
	
checker(check_di3).
	
	
