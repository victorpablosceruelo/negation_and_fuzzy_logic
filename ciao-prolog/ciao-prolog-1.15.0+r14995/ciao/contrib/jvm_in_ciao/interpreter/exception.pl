:- module(exception,[exception_caught/5,exception_uncaught/4,
	             lookup_handlers/5,not_lookup_handlers/5],
		     [assertions, nortchecks, regtypes]).

:- use_module(library(jvm_in_ciao(interpreter(domain)))).
:- use_module(library(jvm_in_ciao(interpreter(jvml)))).
:- use_module(library(jvm_in_ciao(interpreter(heap_operations))), [get_object/3]).

:- include('exception_i.pl').

% Decide whether the given handler catches the exception pointed by a location
:- calls handler_catch/4 : exceptionHandler*heap*pc*location.
:- calls handler_catch/4 : ground*ground*ground*ground.
:- success handler_catch/4 : exceptionHandler*heap*pc*location => exceptionHandler*heap*pc*location.
handler_catch(Ex,_H,PC,_Loc):-
	exceptionHandler_catchType(Ex,none),
	exceptionHandler_isPCinRange(Ex,PC).
handler_catch(Ex,H,PC,Loc):-
	exceptionHandler_catchType(Ex,CL1),
	exceptionHandler_isPCinRange(Ex,PC),
	heap_typeof(H,Loc,locationObject(CL2)),
	subclass_name(CL2,CL1).

% Lookup in the given list of exception handlers if one of them catches the current exception
:- entry lookup_handlers/5 : list(exceptionHandler)*heap*pc*location*var.

:- pred lookup_handlers/5 : list(exceptionHandler)*heap*pc*location*var => 
	list(exceptionHandler)*heap*pc*location*pc + eval.
:- trust comp lookup_handlers(L,H,PC,Loc,PCb) : (ground(L),ground(PC)) + eval.
:- trust comp lookup_handlers/5 + sideff(free).
lookup_handlers([Ex|_ExL],H,PC,Loc,PCb):-
	handler_catch(Ex,H,PC,Loc),
	exceptionHandler_handler(Ex,PCb).
lookup_handlers([Ex|ExL],H,PC,Loc,PCb):-
	\+ handler_catch(Ex,H,PC,Loc),
	lookup_handlers(ExL,H,PC,Loc,PCb).

:- trust comp not_lookup_handlers(L,H,PC,Loc,PCb) : (ground(L),ground(PC)) + eval.
:- trust comp not_lookup_handlers/5 + sideff(free).
not_lookup_handlers([Ex|ExL],H,PC,Loc,PCb):-
	\+ handler_catch(Ex,H,PC,Loc),
	not_lookup_handlers(ExL,H,PC,Loc,PCb).
not_lookup_handlers([],_,_,_,_).


/*
% Lookup in the callstack if one frame catches the thrown exception
:- calls catch_in_state/2 : state*var. 
:- success catch_in_state/2 : state*var => state*state.
:- success catch_in_state(_1,_2) : state*var => (_1=stE(_,_,_),_2=stE(_,_,_)).
catch_in_state(stE(H,frE(M,PC,Loc,L),SF),stE(H,frE(M,PCb,Loc,L),SF)):-
	method_body(M,BM),
	bytecodeMethod_exceptionHandlers(BM,ExL),
	lookup_handlers(ExL,H,PC,Loc,PCb).
catch_in_state(stE(H,frE(_,PC,Loc,_),[fr(M,PCb,_,L)|SF]),State2):-
	method_body(M,BM),
	bytecodeMethod_exceptionHandlers(BM,ExL),
	\+ lookup_handlers(ExL,H,PC,Loc,_),
	catch_in_state(stE(H,frE(M,PCb,Loc,L),SF),State2).
*/
