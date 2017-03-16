
/* -------------------------------------------------------------
   PS figs for the Internals Documentation
   -------------------------------------------------------------

   How to make them:
   Run Ciao on this program in the corresponding directory.
*/

:- load_compilation_module(library(bundle_registry(bundle_registry_load))).
:- use_module('/home/clip/Systems/ciao/etc/components.pl').
:- use_module('/home/clip/Systems/ciao/etc/xmrefs.pl').

fig(Part):-
	( Part=ciaopp
	-> components(ciaopp,_,Files)
	 ; relevant_files(Part,Files,components)
	),
	set_files(Files),
	xmlinks.

/* Then save daVinci graph as a .ps : Print - Portrait - Scale 8x13 approx
   Do not resize the daVinci window!!!!
   and then put the .ps in doc/figs
   -------------------------------------------------------------
*/

components(ciaopp,[],
	[ciaopp(ciaopp),ciaopp(driver),ciaopp(printer),
	 ciaopp(preprocess_flags),
	 infer(infer),infercost(infercost),resources(resources),infernf(infernf),
	 plai(plai),program(p_unit),syntax(tr_syntax)]).
components(p_unit,[program(p_unit)],
	[program(assrt_db),program(assrt_norm),program(clause_db),
	 program(itf_db),program(p_asr),program(p_abs),program(clidlist)]).
components(plai,[plai(plai)],
	[plai(domains),plai(fixpo_plai),plai(normalize),plai(plai_db),
	 plai(re_analysis),plai(tarjan),plai(trace_fixp),plai(transform),
	 plai(trust),plai(view_fixp)]).
         % program(p_unit),ciaopp(preprocess_flags)]
components(infernf,[infernf(infernf)],
	[infernf(cover),infernf(in_out),infernf(nfnf),infernf(nfbool),
	 infernf(nfgraph),infernf(nfsets),infernf(nfsupport),
	 infernf(nftable),infernf(nftypes),infernf(subproblems),infernf(tests),
	 infer(infer_db)]).
         % typeslib(typeslib),plai(tarjan)]
components(infercost,[infercost(infercost_old)],
	[infercost(dependency),infercost(determinacy),infercost(init),
	 infercost(gran(size_rel)),infercost(size),infercost(solution),
	 infercost(time)]).
         % infer(infer),infer(infer_db),ciaopp(preprocess_flags)]). 
components(resources,[resources(resources)],
	[resources(dependency_res),resources(determinacy_res),resources(init_res),
	 resources(gran_res(size_rel_res)),resources(size_res),resources(solution_res),
	 resources(time_res)]).
         % infer(infer),infer(infer_db),ciaopp(preprocess_flags)]). 
components(infer,[infer(infer)],
	[infer(infer),infer(modes),infer(vartypes),infer(infer_db),
	 typeslib(typeslib)]).

/* -------------------------------------------------------------
*/
