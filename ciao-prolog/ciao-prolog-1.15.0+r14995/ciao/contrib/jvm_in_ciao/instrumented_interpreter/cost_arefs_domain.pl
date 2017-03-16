% State Definition: 
%   Abstract State = ast(AOS,LVs,Cost) where ALVs, and
%     AOS represents resp. LVs and OS with abstractions, i.e.,
%     refs = "longest-reachable paths" and numbers = numbers.

:- trust comp alpha_initial_state/1 + (eval,sideff(free)).
alpha_initial_state(ast([],[_,_],0)).

:- trust comp alpha_step(_,_,AS1,_) : ground(AS1) + (eval,sideff(free)).
% The second argument provides an optional context. Here it is not used
alpha_step(aaload,_,ast(OS,LVs,C),ast(OS,LVs,C_p)) :- C_p is C + 1.
alpha_step(aastore,_,ast(OS,LVs,C),ast(OS,LVs,C_p)) :- C_p is C + 1.
alpha_step(aconst_null,_,ast(OS,LVs,C),ast(OS,LVs,C_p)) :- C_p is C + 1.
alpha_step(aload(X),_,ast(OS,LVs,C),ast([V|OS],LVs,C_p)) :- 
	localVar_get(LVs,X,V),
	C_p is C + 1.
alpha_step(anewarray(T),_,ast(OS,LVs,C),ast(OS,LVs,C_p)) :- C_p is C + 1.
alpha_step(areturn,_,ast(OS,LVs,C),ast(OS,LVs,C_p)) :- C_p is C + 1.
alpha_step(arraylength,_,ast(OS,LVs,C),ast(OS,LVs,C_p)) :- C_p is C + 1.
alpha_step(astore(X),_,ast([V|OS],LVs,C),ast(OS,LVs_p,C_p)) :- 
	localVar_update(LVs,X,V,LVs_p),
	C_p is C + 1.
alpha_step(athrow,_,ast(OS,LVs,C),ast(OS,LVs,C_p)) :- C_p is C + 1.
alpha_step(baload,_,ast(OS,LVs,C),ast(OS,LVs,C_p)) :- C_p is C + 1.
alpha_step(bastore,_,ast(OS,LVs,C),ast(OS,LVs,C_p)) :- C_p is C + 1.
alpha_step(checkcast(T),_,ast(OS,LVs,C),ast(OS,LVs,C_p)) :- C_p is C + 1.
alpha_step(const(_T,Z),_,ast(OS,LVs,C),ast(OS,LVs,C_p)) :- C_p is C + 1.
alpha_step(dup,_,ast([V|OS],LVs,C),ast([V,V|OS],LVs,C_p)) :- C_p is C + 1.
alpha_step(dup_x1,_,ast(OS,LVs,C),ast(OS,LVs,C_p)) :- C_p is C + 1.
alpha_step(dup_x2,_,ast(OS,LVs,C),ast(OS,LVs,C_p)) :- C_p is C + 1.
alpha_step(dup2,_,ast(OS,LVs,C),ast(OS,LVs,C_p)) :- C_p is C + 1.
alpha_step(dup2_x1,_,ast(OS,LVs,C),ast(OS,LVs,C_p)) :- C_p is C + 1.
alpha_step(dup2_x2,_,ast(OS,LVs,C),ast(OS,LVs,C_p)) :- C_p is C + 1.
alpha_step(getfield(F),_,ast([R1|OS],LVs,C),ast([R2|OS],LVs,C_p)) :- 
	F = fieldSignature(_,refType(_)),
	R2 < R1,
	C_p is C + 1.
alpha_step(getfield(F),_,ast([_R1|OS],LVs,C),ast([_R2|OS],LVs,C_p)) :- 
	F = fieldSignature(_,primitiveType(_)),
	C_p is C + 1.
alpha_step(getstatic(F),_,ast(OS,LVs,C),ast(OS,LVs,C_p)) :- C_p is C + 1.
alpha_step(goto(O),_,ast(OS,LVs,C),ast(OS,LVs,C_p)) :- C_p is C + 1.
alpha_step(i2b,_,ast(OS,LVs,C),ast(OS,LVs,C_p)) :- C_p is C + 1.
alpha_step(i2s,_,ast(OS,LVs,C),ast(OS,LVs,C_p)) :- C_p is C + 1.
alpha_step(ibinop(Op),_,ast(OS,LVs,C),ast(OS,LVs,C_p)) :- C_p is C + 1.
alpha_step(iaload,_,ast(OS,LVs,C),ast(OS,LVs,C_p)) :- C_p is C + 1.
alpha_step(iastore,_,ast(OS,LVs,C),ast(OS,LVs,C_p)) :- C_p is C + 1.
alpha_step(if_acmpeq(_O),_,ast(OS,LVs,C),ast(OS,LVs,C_p)) :- C_p is C + 1.
alpha_step(if_acmpne(_O),ast(OS,LVs,C),ast(OS,LVs,C_p)) :- C_p is C + 1.
alpha_step(if_icmp(Cmp,O),_,ast(OS,LVs,C),ast(OS,LVs,C_p)) :- C_p is C + 1.
alpha_step(if0(Cmp,O),_,ast(OS,LVs,C),ast(OS,LVs,C_p)) :- C_p is C + 1.


alpha_step(ifnonnull(_O),_,ast(OS,LVs,C),ast(OS,LVs,C_p)) :- C_p is C + 1.
%alpha_step(ifnull(_O),ast([0|OS],LVs,C),ast(OS,LVs,C_p)) :- 
%	C_p is C + 1.
%alpha_step(ifnull(_O),ast([R|OS],LVs,C),ast(OS,LVs,C_p)) :- 
%	R > 0,
%	C_p is C + 1.
alpha_step(iinc(X,Z),_,ast(OS,LVs,C),ast(OS,LVs,C_p)) :- C_p is C + 1.
alpha_step(iload(X),_,ast(OS,LVs,C),ast(OS,LVs,C_p)) :- C_p is C + 1.
alpha_step(ineg,_,ast(OS,LVs,C),ast(OS,LVs,C_p)) :- C_p is C + 1.
alpha_step(instanceof(T),_,ast(OS,LVs,C),ast(OS,LVs,C_p)) :- C_p is C + 1.
%alpha_step(invokespecial(Mid),ast(OS,LVs,C),ast(OS,LVs,C_p)) :- C_p is C + 1.
%alpha_step(invokestatic(Mid),ast(OS,LVs,C),ast(OS,LVs,C_p)) :- C_p is C + 1.
%alpha_step(invokevirtual(Mid),ast(OS,LVs,C),ast(OS,LVs,C_p)) :- C_p is C + 1.
alpha_step(invoke_issd(Mid),_,ast(OS,LVs,C),ast(OS_p,LVs,C_p)) :- 
	alpha_invoke_issd_eval_part(Mid,OS,OS_p),
	C_p is C + 1.
alpha_step(ireturn,_,ast(OS,LVs,C),ast(OS,LVs,C_p)) :- C_p is C + 1.
alpha_step(istore(X),_,ast(OS,LVs,C),ast(OS,LVs,C_p)) :- C_p is C + 1.
alpha_step(lookupswitch(Def,LK),_,ast(OS,LVs,C),ast(OS,LVs,C_p)) :- C_p is C + 1.
alpha_step(new(_),_,ast(OS,LVs,C),ast([_|OS],LVs,C_p)) :- C_p is C + 1.
alpha_step(newarray(_T),_,ast(OS,LVs,C),ast(OS,LVs,C_p)) :- C_p is C + 1.
alpha_step(nop,_,ast(OS,LVs,C),ast(OS,LVs,C_p)) :- C_p is C + 1.
alpha_step(pop,_,ast(OS,LVs,C),ast(OS,LVs,C_p)) :- C_p is C + 1.
alpha_step(pop2,_,ast(OS,LVs,C),ast(OS,LVs,C_p)) :- C_p is C + 1.
alpha_step(putfield(_F),_,ast(OS,LVs,C),ast(OS,LVs,C_p)) :- C_p is C + 1.
alpha_step(putstatic(F),_,ast(OS,LVs,C),ast(OS,LVs,C_p)) :- C_p is C + 1.
alpha_step(return,_,ast(OS,LVs,C),ast(OS,LVs,C_p)) :- C_p is C + 1.
alpha_step(saload,_,ast(OS,LVs,C),ast(OS,LVs,C_p)) :- C_p is C + 1.
alpha_step(sastore,_,ast(OS,LVs,C),ast(OS,LVs,C_p)) :- C_p is C + 1.
alpha_step(swap,_,ast(OS,LVs,C),ast(OS,LVs,C_p)) :- C_p is C + 1.
alpha_step(tableswitch(_,_,_,_,_),_,ast(OS,LVs,C),ast(OS,LVs,C_p)) :- C_p is C + 1.

alpha_invoke_issd_eval_part(Mid,OS,OSb) :-
	%resolve_method(P,CN,Mid,Mb),
	methodSignature_parameters(Mid,Param),
	length(Param,NbParam),
	length(RInArgs,NbParam),
	methodSignature_result(Mid,MidResult),
	(MidResult == none -> OutArgs = []
                            ; OutArgs = [_Out]),
	%(method_isStatic(Mb) -> (
	(1 == 2 -> (
	    append(RInArgs,PreOS,OS),
	    reverse(RInArgs,InArgs),
	    append(OutArgs,PreOS,OSb))
	; (
	    append(RInArgs,[This|PreOS],OS),
	    reverse(RInArgs,InArgs_p),
	    InArgs = [This|InArgs_p],
	    append(OutArgs,PreOS,OSb))).




alpha_step(_,AS1,AS2) :- AS2 is AS1 + 1.

alpha_add(A,B,C) :- C is A + B.
