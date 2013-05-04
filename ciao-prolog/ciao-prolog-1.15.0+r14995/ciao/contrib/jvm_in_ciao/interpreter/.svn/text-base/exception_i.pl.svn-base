
exception_caught(M,H,PC,Loc,PCb) :-
	method_body(M,BM),
	bytecodeMethod_exceptionHandlers(BM,ExL),
	exception_caught_(ExL,H,PC,Loc,PCb).
exception_caught_([Ex|_ExL],H,PC,Loc,PCb) :-
	match_handler(Ex,H,PC,Loc),
	exceptionHandler_handler(Ex,PCb).
exception_caught_([Ex|ExL],H,PC,Loc,PCb) :-
	not_match_handler(Ex,H,PC,Loc),
	exception_caught_(ExL,H,PC,Loc,PCb).

exception_uncaught(M,H,PC,Loc) :-
	method_body(M,BM),
	bytecodeMethod_exceptionHandlers(BM,ExL),
	exception_uncaught_(ExL,H,PC,Loc).
exception_uncaught_([],_H,_PC,_Loc).
exception_uncaught_([Ex|ExL],H,PC,Loc) :-
	not_match_handler(Ex,H,PC,Loc),
	exception_uncaught_(ExL,H,PC,Loc).

match_handler(ExH,_H,PC,_Loc) :-
	exceptionHandler_catchType(ExH,none),
	exceptionHandler_isPCinRange(ExH,PC).
match_handler(ExH,H,PC,loc(Loc)) :-
	exceptionHandler_catchType(ExH,CL1),
	exceptionHandler_isPCinRange(ExH,PC),
	get_object(H,Loc,object(locationObject(CL2),_)), % res_susc
	subclass_name(CL2,CL1). % res_susc

not_match_handler(ExH,_H,PC,_Loc) :-
	exceptionHandler_notPCinRange(ExH,PC).
not_match_handler(ExH,H,PC,loc(Loc)) :-
	exceptionHandler_isPCinRange(ExH,PC),
	exceptionHandler_catchType(ExH,CL1),
	get_object(H,Loc,object(locationObject(CL2),_)), % res_susc
	not_subclass_name(CL2,CL1). % res_susc
