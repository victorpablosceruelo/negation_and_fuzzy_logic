:- module(auto_pe,[auto_pe/2,auto_pe/1],[assertions]).

:- use_module(library(compiler), [use_module/1]).
:- use_module(library(filenames), [no_path_file_name/2, basename/2]).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(format), [format/2]).
:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(write), [write/1]).
:- use_module(ciaopp(preprocess_flags)).
:- use_module(ciaopp(driver), [module/2, analyze/2, transform/1]).
:- use_module(ciaopp(printer), [output/1]).

:- use_module(library(tdg(trace_instrumenter)), [instrument_traces_/1]).
:- use_module(pe1_post_processing, [post_process_pe1/0, post_process_pe1/1]).

:- data option/1.

auto_pe(FileToPe) :- 
	auto_pe(FileToPe,[local_control(df_hom_emb_as),global_control(hom_emb_t),
	                  hom_emb_nums(off),verbosity(low),post_process_pe1]).%trace_instrumentation

auto_pe(FileToPe,Options) :-
	cleanup_aux,
	basename(FileToPe,ModuleName),
 	use_module(ModuleName),
	set_pp_flags,
	process_options(Options),
	module(ModuleName,_),
	statistics(runtime,[T0,_]),
 	analyze(pd,C_PD),
	write(C_PD),
	atom_concat(ModuleNameNoIR,'_ir',ModuleName),
	atom_concat([ModuleNameNoIR,'_ef','.pl'],OutputFile),
 	transform(codegen_af),
	statistics(runtime,[Tn,_]),
	display_stats(T0,Tn),
	(option(post_process_pe1) -> post_process_pe1 ; true),
	(option(trace_instrumentation) -> instrument_traces_([clpq(off)]) ; true),
 	output(OutputFile),
	cleanup_aux.

cleanup_aux :- retractall_fact(option(_)).

process_options(Opts) :-
	member(Opt,Opts),
	Opt =..[F|Args],
	(Args = [Arg],set_pp_flag(F,Arg) -> true
	                                  ; assertz_fact(option(Opt))),
	fail.
process_options(_).


set_pp_flags :-
	set_pp_flag(fixpoint,di),
	set_pp_flag(local_control,df_hom_emb_as),
	set_pp_flag(global_control,hom_emb_t),
	set_pp_flag(sim_ari_exp,off),
	set_pp_flag(unf_bra_fac,0),
	set_pp_flag(verbosity,low),
	set_pp_flag(hom_emb_nums,off),
	set_pp_flag(inter_opt_arg_filt,off),
	set_pp_flag(memo_ignore,[id]),
	set_pp_flag(pe_type_ignore,[]),
	set_pp_flag(gen_emb_atm,none).

display_stats(T0,Tn) :-
	T is Tn - T0,
	format("Partial Evaluation Time: ~q",[T]).
