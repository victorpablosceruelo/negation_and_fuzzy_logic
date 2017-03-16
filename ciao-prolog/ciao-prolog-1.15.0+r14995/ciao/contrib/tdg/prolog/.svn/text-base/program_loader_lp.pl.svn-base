:- module(program_loader_lp,[load_lp_program/1,load_lp_program/2,
	                     load_ef_program/1,load_ef_program/2,
	                     clause_/3,pred_/2,cleanup_all/0]).

:- use_module(library(filenames)).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(prolog_sys), [statistics/2]).

:- data option/1.
:- data clause_/3.
:- data pred_/2.

cleanup_all :-
	retractall_fact(clause_(_,_,_)),
	retractall_fact(pred_(_,_)),
	cleanup_aux.
cleanup_aux :-
	retractall_fact(option(_)).

smart_cleanup :-
	(\+ option(assert) -> cleanup_all 
	                    ; cleanup_aux).

% Available options -> {create_file,assert,print,clpfd}

load_lp_program(FN) :- load_lp_program(FN,[create_file,modes]).
load_lp_program(FileName,Opts) :-
	smart_cleanup,
	statistics(runtime,[T0,_]),
	process_options(Opts),
	prepare_streams(FileName,FIn,FOut,IRName_base),
	(option(create_file) -> write_header_lp(FOut,IRName_base) ; true),
	read_program(FIn,FOut),
	(option(create_file) -> portray_preds(FOut) ; true),
	close(FIn),
	(option(create_file) -> close(FOut) ; true),
	(option(print) -> print_clauses ; true),
	smart_cleanup,
	statistics(runtime,[Tn,_]),
	display_stats(T0,Tn,IRName_base).

load_ef_program(FN) :- load_ef_program(FN,[create_file]).
load_ef_program(FileName,Opts) :-
	smart_cleanup,
	statistics(runtime,[T0,_]),
	process_options(Opts),
	prepare_streams(FileName,FIn,FOut,IRName_base),
	(option(create_file) -> write_header_ef(FOut,IRName_base) ; true),
	read_program(FIn,FOut),
	(option(create_file) -> portray_preds(FOut) ; true),
	close(FIn),
	(option(create_file) -> close(FOut) ; true),
	(option(print) -> print_clauses ; true),
	smart_cleanup,
	statistics(runtime,[Tn,_]),
	display_stats(T0,Tn,IRName_base).

process_options(Opts) :-
	member(Opt,Opts),
	assertz_fact(option(Opt)),
	fail.
process_options(_).

prepare_streams(FileName,FIn,FOut,IRName_base) :-
	(extension(FileName,'.pl') -> FileName_p = FileName
	                            ; atom_concat(FileName,'.pl',FileName_p)),
	open(FileName_p,read,FIn),
	(option(create_file) -> (
            no_path_file_name(FileName_p,File_np),
	    basename(File_np,File_base),
	    atom_concat(File_base,'_ir',IRName_base),
	    atom_concat(IRName_base,'.pl',IRName),
	    open(IRName,write,FOut))
	;
	    true).

portray_preds(FOut) :-
	pred_(P,LModes),
	reverse(LModes,LModes_r),
	portray_clause(FOut,pred_(P,LModes_r)),
	fail.
portray_preds(_).

write_header_lp(FOut,IRName_base) :-
	format(FOut,":- module(~q,[main/4],[assertions,regtypes]).\n\n",[IRName_base]),
	format(FOut,":- include(library(tdg(prolog(prolog_interpreter_i)))).\n\n",[]),
	atom_concat(IRName,'_ir',IRName_base),
	format(FOut,":- include(library('tdg/prolog/experiments/~q_div_pts')).\n\n",[IRName]).


write_header_ef(FOut,IRName_base) :-
	format(FOut,":- module(~q,[solve_bck/2,solve/1]).\n\n",[IRName_base]),
	format(FOut,":- use_module(library(clpfd)).\n\n",[]),
	format(FOut,":- include('../vanilla_interpreter_i').\n\n",[]),
	format(FOut,":- include('../bck_interpreter_i').\n\n",[]).

read_program(FIn,FOut) :-
	read(FIn,Line),
	(Line == end_of_file -> true
	                    ; (parse_line(Line,FOut),
			       read_program(FIn,FOut))).

parse_line(Line,_FOut) :-
	parse_directive(Line,_Dir),!.
parse_line(Line,FOut) :-
	parse_clause(Line,Head,Body),!,
	body2list(Body,Body_l),
	set_pred(Head,_PSig,Count),
	(option(clpfd) -> arith_to_clpfd_l(Body_l,Body_l_p) 
	                ; Body_l_p = Body_l),
	(option(create_file) -> 
	     portray_clause(FOut,clause_(Head,Body_l_p,Count))
	;
	     true),
	(option(assert) -> assertz_fact(clause_(Head,Body_l_p,Count)) ; true).

parse_directive(':-'(Dir),Dir).
parse_clause(':-'(Head,Body),Head,Body) :- !.
parse_clause(Head,Head,true).

set_pred(H,Pred/Ar,N_p) :-
	functor(H,Pred,Ar),
	H =..[Pred|Args],
	(option(modes) -> get_modes(Args,Modes) ; Modes = [mode]),
	(pred_(Pred/Ar,LModes) -> 
	    (retractall_fact(pred_(Pred/Ar,_)),
	     (member((Modes,N),LModes) -> 
	          (select((Modes,N),LModes,LModes_r),N_p is N+1,
		   NewLModes = [(Modes,N_p)|LModes_r])
	     ; 
		  (N_p is 1,NewLModes = [(Modes,N_p)|LModes])))
	;    
	    (N_p = 1, NewLModes = [(Modes,N_p)])),
	assertz_fact(pred_(Pred/Ar,NewLModes)).

get_modes([],[]).
get_modes([in(_)|RArgs],[in|RModes]) :- get_modes(RArgs,RModes).
get_modes([out(_)|RArgs],[out|RModes]) :- get_modes(RArgs,RModes).

get_counter(_,1).

body2list(Body,[First|More]):-
	nonvar(Body),
	Body = (First,Rest),
        !,
        body2list(Rest,More).
body2list(Body,LBody):-
	nonvar(Body),
	Body = true,!,
	LBody = [].
body2list(Last,[Last]).

arith_to_clpfd_l([],[]).
arith_to_clpfd_l([A|As],[A_p|As_p]) :-
%	arith_to_clpfd(A,A_p),
	split_atom(A,M,F,Args),
	clpfd_conversion(M,F,Args,A_p),
	arith_to_clpfd_l(As,As_p).

arith_to_clpfd(A,A_p) :-
	split_atom(A,arithmetic,F,Args),!,
	clpfd_conversion(arithmetic,F,Args,A_p).
arith_to_clpfd(A,A_p) :-
	split_atom(A,term_basic,F,Args),!,
	clpfd_conversion(term_basic,F,Args,A_p).
arith_to_clpfd(A,A).

clpfd_conversion(_,call,[Call],call(Call_p)) :- !,
	Call =..[F|Args],
	clpfd_conversion(_,F,Args,Call_p).
clpfd_conversion(_,is,[A,B],'#='(A,B)) :- !.
clpfd_conversion(_,\=,[A,B],'#\\='(A,B)) :- !.
clpfd_conversion(_,=\=,[A,B],'#\\='(A,B)) :- !.
clpfd_conversion(_,\==,[A,B],'#\\='(A,B)) :- !.
clpfd_conversion(_,<,[A,B],'#<'(A,B)) :- !.
clpfd_conversion(_,>,[A,B],'#>'(A,B)) :- !.
clpfd_conversion(_,=<,[A,B],'#=<'(A,B)) :- !.
clpfd_conversion(_,>=,[A,B],'#>='(A,B)) :- !.
clpfd_conversion(M,F,Args,A_p) :-
	build_atom(A_p,M,F,Args).

split_atom(Atom,M,F,Args) :-
	Atom =..[Atom_f|Args],
	atom_concat([M,:,F],Atom_f),!.
split_atom(Atom,'',Atom_f,Args) :-
	Atom =..[Atom_f|Args].

build_atom(Atom,'',F,Args) :- !,
	Atom =..[F|Args].
build_atom(Atom,M,F,Args) :-
	atom_concat([M,:,F],Atom_f),
	Atom =..[Atom_f|Args].


print_clauses :-
	clause_(H,B,Id),
	write(clause_(H,B,Id)),nl,
	fail.
print_clauses.

display_stats(T0,Tn,IRName_base) :-
	T is Tn - T0,
	atom_concat(IRName_base,'.pl',File),
	format("\nProgram parsed and loaded in ~q",[T]),
	format("\nOutput written in file ~q\n",[File]).
