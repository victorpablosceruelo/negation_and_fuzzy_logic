:- use_module(library(tdg(prolog(residual_calls))), [res_call/1]).
:- use_module(library(pe_types)).
:- use_module(library(lists), [append/3, length/2]).
:- use_module(library(aggregates), [findall/3, setof/3]).
:- use_module(library(between), [between/3]).

% Prolog meta-interpreter with explicit failure and number of choice pts.

% State = st(PP,Goal,CPs,OutVars,Ans,NCPs) where PP = pp(Pred/Ar,ClId,Pt),
%   CPs is the stack of choice/continuation points (they are PP's),
%   OutVars is the copy of the output vars used in the corresponding evaluation,
%   NCPs is the number of choice points (total/in_calls).
%   Ans is A/AI where A \in {yes,no}, AI \in {first-try,after-retry}.

% PE annotations
:- trust comp exec/3 : f_sig * pt_state * i_sig + pe_type.
:- regtype pt_state/1.
pt_state(st(PP,G,CPs,OutVars,Ans,NCPs)) :- 
	f_sig(PP),f_sig(G),f_sig(CPs),f_sig(OutVars),i_sig(Ans),f_sig(NCPs).

main(Pred/Ar,Args,Answer,TNCPs) :-
	pred_(Pred/Ar,_),
	build_initial_state(Pred/Ar,Args,OutVars,S0),
	exec(Args,S0,st(_PP,_G,_CPs,OutVars_p,Answer,TNCPs/_)),
	OutVars_p = OutVars.

:- trust comp exec/3 + (sideff(free),bind_ins).
:- trust comp exec(_,st(PP,_,_,_,yes,_),_) : div_pt_non_first(PP) + memo.
:- prop div_pt/1.
:- prop div_pt_non_first/1.
div_pt_non_first(pp(P/Ar,ClId,Pt)) :- 
	div_pt(pp(P/Ar,ClId,Pt)), (ClId > 1; Pt > 1).
:- prop no_div_pt/1.
no_div_pt(PP) :- \+ div_pt(PP).

exec(_,st(_PP,[],[],OutVs,yes/AI,NCPs),st(_,_,_,OutVs,yes/AI,NCPs)).
exec(_,st(_PP,[],[_CP|_CPs],OutVs,yes/AI,NCPs),st(_,_,_,OutVs,yes/AI,NCPs)).
exec(Args,st(pp(Pred/Ar,_,_),[],[CP|CPs],_,yes/_,TNCPs/0),Sf) :- 
	CP = pp(Pred/Ar,ClId,_Pt),
	build_fresh_args(Args,Args_p,_,OutVars_p),
	build_goal(Pred/Ar,ClId,Args_p,Goal),
	TNCPs_p is TNCPs - 1,
	exec(Args,st(CP,Goal,CPs,OutVars_p,yes/'after-retry',TNCPs_p/0),Sf).
exec(_,st(_PP,_,[],OVs,no/AI,TNCPs/0),st(_,_,_,OVs,no/AI,TNCPs/0)).
exec(Args,st(_,_,[CP|CPs],_,no/AI,TNCPs/0),Sf) :- 
	CP = pp(Pred/Ar,ClId,_Pt),
	build_fresh_args(Args,Args_p,_,OutVars_p),
	build_goal(Pred/Ar,ClId,Args_p,Goal),
	TNCPs_p is TNCPs - 1,
	exec(Args,st(CP,Goal,CPs,OutVars_p,yes/AI,TNCPs_p/0),Sf).
exec(Args,st(pp(Pred/Ar,ClId,Pt),[A|As],CPs,OutVars,yes/AI,TNCPs/ENCPs),Sf) :-
	internal(A),
	functor(A,A_f,A_ar),
	A =..[A_f|A_args],
	next(Pt,Pt2),
%	main(A_f/A_ar,A_args,Ans/AI_p,ENCPs_p),
	res_call(main(A_f/A_ar,A_args,Ans/AI_p,ENCPs_p)), % should be main(...)
	TNCPs_p is TNCPs + ENCPs_p, ENCPs_pp is ENCPs + ENCPs_p,
	compose_answer_info(AI,AI_p,AI_pp),
	exec(Args,st(pp(Pred/Ar,ClId,Pt2),As,CPs,OutVars,Ans/AI_pp,TNCPs_p/ENCPs_pp),Sf).
exec(Args,st(pp(Pred/Ar,ClId,Pt),[A|As],CPs,OutVars,yes/AI,NCPs),Sf) :-
	builtin(A),
	next(Pt,Pt2),
	run_builtin(pp(Pred/Ar,ClId,Pt),A,Ans), % I assume builtins are deterministic
	exec(Args,st(pp(Pred/Ar,ClId,Pt2),As,CPs,OutVars,Ans/AI,NCPs),Sf).

:- trust comp run_builtin(PP,_,_) : div_pt(PP) + memo.
:- trust comp run_builtin(PP,_,_) : no_div_pt(PP) + eval.
:- trust comp run_builtin/3 + (sideff(free),bind_ins).
:- trust comp run_builtin/3 : i_sig * f_sig * f_sig + pe_type.
run_builtin(_,A,yes) :- call(A).
run_builtin(_,A,no) :- \+ call(A).%failed_builtin(A).

:- trust comp next/2 + (eval,sideff(free),bind_ins).
next(Pt,Pt2) :- Pt2 is Pt+1.

:- trust comp internal/1 + (eval,sideff(free),bind_ins).
internal(A) :- 
	functor(A,A_f,A_ar),
	pred_(A_f/A_ar,_).

:- trust comp builtin/1 + (eval,sideff(free),bind_ins).
builtin(A) :- \+ internal(A).

:- trust comp failed_builtin/1 + (memo,sideff(free),bind_ins).
failed_builtin(A) :- 
	\+ call(A). 

:- trust comp compose_answer_info/3 + (eval,sideff(free),bind_ins).
compose_answer_info('after-retry',_,'after-retry').
compose_answer_info('first-try',AI_p,AI_p).

:- trust comp build_initial_state/4 + (eval,sideff(free),bind_ins).
build_initial_state(Pred/Ar,Args,OutVars,S0) :-
	pred_(Pred/Ar,ModesList),
	member((Modes,NCl),ModesList),
	build_check_args(Args,Modes),
	build_fresh_args(Args,Args_p,OutVars,OutVars_p),
	build_goal(Pred/Ar,1,Args_p,Goal),
	findall(pp(Pred/Ar,I,1),between(2,NCl,I),CPs),
	TNCPs is NCl - 1,
	S0 = st(pp(Pred/Ar,1,1),Goal,CPs,OutVars_p,yes/'first-try',TNCPs/0).

build_check_args([],[]).
build_check_args([Arg1|Args],[Mode1|Modes]) :-
	functor(Arg1,Mode1,1),
	build_check_args(Args,Modes).

:- trust comp build_fresh_args/4 + (eval,sideff(free),bind_ins).
build_fresh_args([],[],[],[]).
build_fresh_args([in(Arg1)|Args],[in(Arg1)|Args_p],OutVars,OutVars_p) :-
	build_fresh_args(Args,Args_p,OutVars,OutVars_p).
build_fresh_args([out(Arg1)|Args],[out(Arg1_p)|Args_p],[Arg1|OutVars],[Arg1_p|OutVars_p]) :-
	copy_term(Arg1,Arg1_p),
	build_fresh_args(Args,Args_p,OutVars,OutVars_p).

:- trust comp build_goal/4 + (eval,sideff(free),bind_ins).
build_goal(Pred/_,ClId,Args,Goal) :-
	A0 =..[Pred|Args],
	clause_(A0,Goal,ClId).

:- trust comp clause_/3 + (eval,sideff(free),bind_ins).


