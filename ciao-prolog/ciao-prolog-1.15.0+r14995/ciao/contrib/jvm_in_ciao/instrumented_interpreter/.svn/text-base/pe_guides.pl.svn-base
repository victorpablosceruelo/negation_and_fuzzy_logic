
%% PE_TYPE'S ANNOTATIONS AND DECLARATIONS
:- use_module(library(pe_types)).

:- trust comp execute/5: const * pt_state * const * dyn * const + pe_type.

:- regtype pt_state/1.
pt_state(st(H,F,CS)) :- dyn(H),pt_frame(F),list(CS,pt_frame).
pt_state(stE(H,F,CS)) :- dyn(H),pt_frameE(F),list(CS,pt_frame).

:- regtype pt_frame/1.
pt_frame(fr(M,PC,LV,S)) :- pt_method(M),f_sig(PC),dyn(LV),dyn(S).
:- regtype pt_frameE/1.
pt_frameE(frE(M,PC,LV,S)) :- pt_method(M),f_sig(PC),dyn(LV),dyn(S).

:- regtype pt_method/1.
pt_method(method(MSig,bytecodeMethod(X,Y,Z,methodId(IdC,IdM),Exs),
	         Final,Static,Public)) :-
	const(MSig),const(X),const(Y),const(Z),f_sig(IdC),f_sig(IdM),
	const(Exs),const(Final),const(Static),const(Public).
% The bytecodeMethod arg could be none (empty methods).


:- trust comp fetch_instruction/2 + (eval,sideff(free)).

% In the following, we state by means of 'eval' annotations the conditions under 
% which the step/5 predicate could be evaluated in PE time
:- trust comp step(Inst,_Step,_P,_S1,_S2) : eval_inst(Inst) + eval.
:- trust comp step(Inst,_Step,_P,S1,_S2) : 
               (ground_top_inst(Inst),ground_top(S1)) + eval.
:- trust comp step(Inst,_Step,_P,S1,_S2) : 
               (ground_2top_inst(Inst),ground_2top(S1)) + eval.
:- trust comp step(Inst,_Step,_P,S1,_S2) : 
               (null_subtop_inst(Inst),null_subtop(S1)) + eval.
:- trust comp step(Inst,_Step,_P,S1,_S2) : 
               (null_subsubtop_inst(Inst),null_subsubtop(S1)) + eval.
%:- trust comp step(_Inst,_Step,_P,S1,_S2) : exception_state(S1) + eval.
:- trust comp step/5 + sideff(free).


:- regtype eval_inst/1.
eval_inst(aconst_null).
eval_inst(aload(_)).
%eval_inst(areturn). % Commented due to AHR
%eval_inst(arraylength). % Commented due to AHR
eval_inst(astore(_)).
eval_inst(athrow).
%eval_inst(checkcast(_)). % Commented due to AHR
eval_inst(const(_,_)).
eval_inst(dup).
eval_inst(dup_x1).
eval_inst(dup_x2).
eval_inst(dup2).
eval_inst(dup2_x1).
eval_inst(dup2_x2).
%eval_inst(getfield(_)). % Commented due to AHR
eval_inst(getstatic(_)).
eval_inst(goto(_)).
%eval_inst(if_acmpeq(_)). % In ISSD the test V1 \= V2, should remain as residual
%eval_inst(if_acmpne(_)). %  if one of them is a variable.
eval_inst(ifnonnull(_)).
eval_inst(ifnull(_)).
eval_inst(iload(_)).
%eval_inst(instanceof(_)). % Commented due to AHR
eval_inst(invokespecial(_)).
eval_inst(invokestatic(_)).
%eval_inst(invokevirtual(_)). % Commented due to AHR
eval_inst(ireturn).
eval_inst(istore(_)).
eval_inst(new(_)).
eval_inst(nop).
eval_inst(pop).
eval_inst(pop2).
%eval_inst(putfield(_)). % Commented due to AHR
%eval_inst(putstatic(_)). % Commented due to AHR
eval_inst(return).
eval_inst(swap).

:- regtype ground_top_inst/1.
ground_top_inst(anewarray(_)).
ground_top_inst(i2b).
ground_top_inst(i2s).
ground_top_inst(if0(_,_)).
ground_top_inst(ineg).
ground_top_inst(lookupswitch(_,_)).
ground_top_inst(newarray(_)).
ground_top_inst(tableswitch(_,_,_,_,_)).
ground_top_inst(arraylength).
ground_top_inst(getfield(_)).
ground_top_inst(areturn).
ground_top_inst(checkcast(_)).
ground_top_inst(instanceof(_)).

:- regtype ground_top/1.
ground_top(st(_H,fr(_M,_PC,[Top|_S],_L),_SF)) :- gnd(Top).

:- regtype ground_2top_inst/1.
ground_2top_inst(ibinop(_)).
ground_2top_inst(if_icmp(_,_)).

:- regtype ground_2top/1.
ground_2top(st(_H,fr(_M,_PC,[Top1,Top2|_S],_L),_SF)) :- gnd(Top1),gnd(Top2).

:- regtype null_subtop_inst/1.
null_subtop_inst(aaload).
null_subtop_inst(baload).
null_subtop_inst(iaload).
null_subtop_inst(saload).
null_subtop_inst(putfield(_)).

:- regtype null_subtop/1.
null_subtop(st(_H,fr(_M,_PC,[_Top1,null|_S],_L),_SF)).

:- regtype null_subsubtop_inst/1.
null_subsubtop_inst(aastore).
null_subsubtop_inst(bastore).
null_subsubtop_inst(iastore).
null_subsubtop_inst(sastore).

:- regtype null_subsubtop/1.
null_subsubtop(st(_H,fr(_M,_PC,[_Top1,_Top2,null|_S],_L),_SF)).

:- regtype exception_state/1.
exception_state(stE(_H,_FrE,_SF)).

:- trust comp invoke_issd_evalpart/12 + (eval,sideff(free)).
