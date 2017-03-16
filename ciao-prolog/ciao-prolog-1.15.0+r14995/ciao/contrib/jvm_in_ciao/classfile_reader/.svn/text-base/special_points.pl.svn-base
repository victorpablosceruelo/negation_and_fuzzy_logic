:- module(special_points,[infer_special_points/2]).

:- use_module(stats).
:- use_module(classfile_reader, [get_bytecode/1]).

:- data n/5.
:- data e/3.

infer_special_points([],_) :- !.
infer_special_points(Ts,FOut) :-
	build_trust_assrt(FOut),
	format(FOut,":- regtype special_point/2.\n",[]),
	build_sp_regtype(Ts,FOut),
	compute_cfg,
	build_sp_defs(Ts,FOut),
	cleanup_cfg.

build_trust_assrt(FOut) :-
%	format(FOut,":- trust comp execute(st(_,fr(method(_,bytecodeMethod(_,_,_,methodId(~q,M),_),_,_,_),PC,_,_),_),_) : special_point(M,PC) + memo.\n\n",['0']).
	format(FOut,":- trust comp execute(st(_,fr(method(_,bytecodeMethod(_,_,_,methodId(_,M),_),_,_,_),PC,_,_),_),_) : special_point(M,PC) + memo.\n\n",[]).

build_sp_regtype([zeros|Ts],FOut) :-
	format(FOut,"special_point(_,0).\n",[]),
	build_sp_regtype(Ts,FOut).
build_sp_regtype([Other|Ts],FOut) :-
	format(FOut,"special_point(M,PC) :- ~q_point(M,PC).\n",[Other]),
	build_sp_regtype(Ts,FOut).
build_sp_regtype([],_FOut).

build_sp_defs([Flag|Fs],FOut) :-
	sp_defs_for(Flag,FOut),
	build_sp_defs(Fs,FOut).
build_sp_defs([],_).

sp_defs_for(zeros,_FOut).
sp_defs_for(conv,FOut) :-
	format(FOut,"\n:- regtype conv_point/2.\n",[]),
	conv_points(FOut).
sp_defs_for(div,FOut) :-
	format(FOut,"\n:- regtype div_point/2.\n",[]),
	div_points(FOut).
sp_defs_for(exception,FOut) :-
	format(FOut,"\n:- regtype exception_point/2.\n",[]),
	exception_points(FOut).

compute_cfg :- 
	initialize_cfg,
	take_method(M),
	stats:init_method_stats(M), %% stats
	get_bytecode(bytecode(I,'0',M,Inst,Offnext)),
	stats:inc_method_num_insts(M), %% stats
	compute_node_outs(I,M,Inst,Offnext,Outs),
	collect_node_ins(I,M,Ins),
	exception_flag_inst(Inst,ExcFlag),
	assertz_fact(n(I,M,Ins,Outs,ExcFlag)),
	fail.
compute_cfg :- adjust_pending_ins.

initialize_cfg :-
	get_bytecode(bytecode(0,'0',M,_,_)),
	assertz_fact(n(-1,M,[],[0],0)), 
	assertz_fact(e(0,-1,M)),
	fail.
initialize_cfg.

take_method(M) :- get_bytecode(bytecode(0,'0',M,_Inst,_Offnext)).

compute_node_outs(I,M,Inst,Offnext,[Inext,Ibranch]) :-
	cond_branch_inst(Inst,Offbranch),!,
	Inext is I + Offnext,
	Ibranch is I + Offbranch,
	assertz_fact(e(Inext,I,M)),
	assertz_fact(e(Ibranch,I,M)).
compute_node_outs(I,M,Inst,_Offnext,[Ibranch]) :-
	uncond_branch_inst(Inst,Offbranch),!,
	Ibranch is I + Offbranch,
	assertz_fact(e(Ibranch,I,M)).
compute_node_outs(_I,_M,Inst,_Offnext,[]) :-
	return_inst(Inst),!.
compute_node_outs(I,M,_Inst,Offnext,[Inext]) :-
	Inext is I + Offnext,
	assertz_fact(e(Inext,I,M)).

collect_node_ins(I,M,Ins) :-
	findall(From,e(I,From,M),Ins),
	retractall_fact(e(I,_,M)).

adjust_pending_ins :-
	retract_fact(e(To,From,M)),
	retract_fact(n(To,M,Ins,Outs,ExF)),
	assertz_fact(n(To,M,[From|Ins],Outs,ExF)),
	fail.
adjust_pending_ins.

conv_points(FOut) :-
	current_fact(n(PC,M,[_,_|_],_,_)),
	% stats
	stats:inc_class_num_convs('0'), inc_method_num_convs(M),
	% end stats
	format(FOut,"conv_point(~q,~q).\n",[M,PC]),
	fail.
conv_points(FOut) :-
	format(FOut,"conv_point(~q,~q).\n",[-1,-1]). % To avoid warnings

div_points(FOut) :- 
	current_fact(n(PC,M,_,[_,_|_],_)),
	% stats
	stats:inc_class_num_divs('0'), inc_method_num_divs(M),
	% end stats
	format(FOut,"div_point(~q,~q).\n",[M,PC]),
	fail.
div_points(FOut) :- 
	format(FOut,"div_point(~q,~q).\n",[-1,-1]). % To avoid warnings

exception_points(FOut) :- 
	current_fact(n(PC,M,_,_,1)),
	% stats
	stats:inc_class_num_divs('0'), inc_method_num_divs(M),
	% end stats
	format(FOut,"exception_point(~q,~q).\n",[M,PC]),
	fail.
exception_points(FOut) :-
	format(FOut,"exception_point(~q,~q).\n",[-1,-1]). % To avoid warnings


cleanup_cfg :-
	retractall_fact(n(_,_,_,_,_)),
	retractall_fact(e(_,_,_)).

/*
print_cfg :-
	current_fact(n(A,B,C,D,E)),
	format("n(~k,~k,~k,~k,~k)\n",[A,B,C,D,E]),
	fail.
print_cfg :- 
	current_fact(e(A,B,C)),
	writeq(e(A,B,C)),nl,
	fail.
print_cfg.

branch_inst(Inst,Offset) :- cond_branch_inst(Inst,Offset).
branch_inst(Inst,Offset) :- uncond_branch_inst(Inst,Offset).
*/

cond_branch_inst(if0(_,O),O).
cond_branch_inst(if_icmp(_,O),O).
cond_branch_inst(if_acmpeq(_,O),O).
cond_branch_inst(if_acmpne(_,O),O).
cond_branch_inst(ifnull(O),O).
cond_branch_inst(ifnonnull(O),O).
cond_branch_inst(lcmp,1).

uncond_branch_inst(goto(O),O).
uncond_branch_inst(goto_w(O),O).

return_inst(return).
return_inst(areturn).
return_inst(ireturn).

exception_flag_inst(Inst,1) :- exception_susc_inst(Inst),!.
exception_flag_inst(_,0).

exception_susc_inst(aaload).
exception_susc_inst(aastore).
exception_susc_inst(anewarray(_)).
exception_susc_inst(arraylength).
exception_susc_inst(athrow).
exception_susc_inst(baload).
exception_susc_inst(caload).
exception_susc_inst(saload).
exception_susc_inst(laload).
exception_susc_inst(faload).
exception_susc_inst(daload).
exception_susc_inst(bastore).
exception_susc_inst(castore).
exception_susc_inst(sastore).
exception_susc_inst(lastore).
exception_susc_inst(fastore).
exception_susc_inst(dastore).
exception_susc_inst(checkcast(_)).
exception_susc_inst(getfield(_)).
exception_susc_inst(ibinop(divInt)).
exception_susc_inst(ibinop(remInt)).
exception_susc_inst(iaload).
exception_susc_inst(iastore).
exception_susc_inst(invokespecial(_)).
exception_susc_inst(invokevirtual(_)).
exception_susc_inst(newarray(_)).
exception_susc_inst(putfield(_)).
