/*             Copyright (C)1990-2002 UPM-CLIP				*/

:- module(domains,[
	% initialization
	init_abstract_domain/2,
	% basic domain operations
	amgu/5,
	call_to_entry/9,
	exit_to_prime/8,
	project/5,
	extend/5,
	widen/4,
	widencall/4,
	normalize_asub/3,
%	do_compute_lub/3,
	compute_lub/3,
	compute_clauses_lub/4,
%	compute_lub_general/3,
%	lub_all/4,
	glb/4,
	less_or_equal/3,
	less_or_equal_proj/5,
	identical_abstract/3,
	identical_proj/5,
	identical_proj_1/7,
	fixpoint_covered/3,
	abs_sort/3,
	augment_asub/4,
	augment_two_asub/4,
	% specialized operations (including builtin handling)
	abs_subset/3,
	eliminate_equivalent/3,
	call_to_success_fact/9,
	body_succ_builtin/9,
	special_builtin/6,
	combined_special_builtin/3,
	split_combined_domain/4,
	concrete/4,
        part_conc/5,
	multi_part_conc/4,
	% properties directly from domain
        obtain_info/5,
	% properties to domain and viceversa
	info_to_asub/5,
	full_info_to_asub/4,
	asub_to_info/5,
	asub_to_out/5,
	asub_to_native/5,
	unknown_call/5,
	unknown_call/4,
	unknown_entry/4,
	unknown_entry/3,
	empty_entry/3,
	% too particular operations:
	collect_types_in_abs/4,
	rename_types_in_abs/4,
%	propagate_downwards_closed/4,
%	del_real_conjoin/4,
%	del_hash/4,
%	more_instantiate/3,
%	convex_hull/4,
%	compute_lub_el/4,
%	extend_free/4,
%	del_check_cond/6,
%	del_impose_cond/5,
        dom_statistics/2,
	abstract_instance/5,
	contains_parameters/2
        ],
	[assertions,regtypes
	]).

:- use_module(program(p_unit), [native_prop/2, native_props/2]).
:- use_module(ciaopp(preprocess_flags), [set_pp_flag/2, current_pp_flag/2, push_pp_flag/2]).
:- use_module(plai(plai_errors), [compiler_error/1]).
:- use_module(plai(fixpo_ops), [each_exit_to_prime/8, each_abs_sort/3]).

:- use_module(library(terms_check), [variant/2]).
:- use_module(library(terms_vars),  [varset/2]).
:- use_module(library(sets),        [ord_subset/2]).
:- use_module(library(sort)).
:- use_module(library(messages), [warning_message/2]).
:- use_module(library(assertions(native_props)), [linear/1]).

% PD domain
:- use_module(domain(pd)).
% PD domain with bottom
:- use_module(domain(pdb)).
% toy domain
:- use_module(domain(gr)).
:- use_module(domain(java_nullity)). % for java programs
% def/fr/frdef
:- use_module(domain(def)).
:- use_module(domain(fd)).
:- use_module(domain(fr_top)).
%% :- include(plai(fros)).
%% :- include(plai(fross23)).
%% :- include(plai(kulordsets)).
%% :- include(plai(kulordsetsext)).
%% :- include(plai(min_df_aux)).
%% :- include(plai(min_df_top)).
%% :- include(plai(min_fr_aux)).
%% :- include(plai(min_shared)).
% lsign
:- use_module(domain(lsign)).
% sharing
%% :- include(plai(shabsub)).
:- use_module(domain(share)).
:- use_module(domain(shfret)).
:- use_module(domain(shareson)).
:- use_module(domain(shfrson)).
:- use_module(domain(sondergaard)).
:- use_module(domain(oo_son)).
:- use_module(domain(oo_shnltau)).
:- use_module(domain(share_amgu)).
:- use_module(domain(share_clique)).
:- use_module(domain(bshare(bshare))).
% depthk
:- use_module(domain(aeq_top)).
:- use_module(domain(depthk)).
:- use_module(domain(top_path_sharing)).
% types
:- use_module(domain(eterms)).
:- use_module(domain(svterms)).
:- use_module(domain(termsd)).
:- use_module(domain(ptypes)).
:- use_module(domain(polyhedra)).
:- use_module(domain(oo_types)).
:- use_module(domain(deftypes)).

:- use_module(domain(java_cha)).
% nonfailure
:- use_module(domain(nfplai)).

% determinism
:- use_module(domain(detplai)).

:- use_module(library(terms_check), [instance/2]).

:- use_module(infer(low_level_props), [decide_low_level_format/4]).
% ------------------------------------------------------------------------

:- doc(title,"Plug-in points for abstract domains").

:- doc(author,"Maria Garcia de la Banda").
:- doc(author,"Francisco Bueno").

:- doc(module,"This module contains the predicates for selecting
   the abstract operations that correspond to an analysis domain. The
   selection depends on the name of the domain given as first argument
   to all predicates. Whenever a new domain is added to the system, a
   new clause for each predicate exported here will be needed to call
   the corresponding domain operation in the domain module. Some local
   operations used but not exported by this module would have to be
   defined, too. See the following chapter for an example domain module.

   Adding an analysis domain to PLAI requires only changes in this module.
   However, in order for other CiaoPP operations to work, you may need to
   change other modules. See, for example, module @tt{infer_dom}.

   In this chapter, arguments referred to as @tt{Sv}, @tt{Hv}, @tt{Fv},
   @tt{Qv}, @tt{Vars} are lists of program variables and are supposed to 
   always be sorted. Abstract substitutions are referred to as @tt{ASub}, 
   and are also supposed sorted (except where indicated), although this 
   depends on the domain.
").

:- doc(bug,"When interpreting assertions (and native) should take 
	into account things like sourcename(X):- atom(X) and
        true pred atom(X) => atm(X).").
:- doc(bug,"@pred{body_succ_builtin/9} seems to introduce spurious 
	choice-points.").
:- doc(bug,"Property @tt{covered/2} is not well understood by the
	domains.").
:- doc(bug,"Operation @tt{amgu/5} is missing.").

%------------------------------------------------------------------------%
%                    Meaning of the Program Variables                    %
%                                                                        %
%  AbsInt  : identifier of the abstract interpreter being used           %
%  Sg      : Subgoal being analysed                                      %
%  SgKey   : Subgoal key (represented by functor/arity)                  %
%  Head    : Head of the clause being analysed                           %
%  Sv      : Subgoal variables                                           %
%  Hv      : Head variables                                              %
%  Fv      : Free variables in the body of the clause being considered   %
%  Vars    : Any possible set of variables                               %
%  Call    : Abstract call substitution                                  %
%  Proj    : Call projected onto Sv                                      %
%  Entry   : Abstract entry substitution (i.e. the abstract subtitution  %
%            obtained after the abstract unification of Sg and Head      %
%            projected onto Hv + Fv)                                     %
%  Exit    : Abstract exit substitution (i.e. the abstract subtitution   %
%            obtained after the analysis of the clause being considered  %
%            projected onto Hv)                                          %
%  Prime   : Abstract prime substitution (i.e. the abstract subtitution  %
%            obtained after the analysis of the clause being considered  %
%            projected onto Sv)                                          %
%  Succ    : Abstract success substitution (i.e. the abstract subtitution%
%            obtained after the analysis of the clause being considered  %
%            extended to the variables of the clause in which Sg appears)%
%  ASub    : Any possible abstract substitution                          %
%  R_flag  : Flag which represents the recursive characteristics of a    %
%            predicate. It will be "nr" in case the predicate be non     % 
%            recursive. Otherwise it will be r (recursive)
% List     : (can be represented as OldList,List,AddList,IdList,NewList) %
%            current the list of nodes which a given node depends on.    %
% _s       : The suffix _s means that the term to which the variable is  %
%            bound to has been sorted. By default they are always sorted %
%            thus _s is added only when it appears neccessary to say it  %
%            explicitely                                                 %
% _uns     : The suffix _uns means that the term to which the variable   %
%            is bound is not sorted                                      %
% ExtraInfo: Info computed during the call_to_entry that can be reused   %
%            during the exit_to_prime step                               %
%------------------------------------------------------------------------%

:- doc(aidomain(AbsInt),"Declares that @var{AbsInt} identifies
	an abstract domain.").
:- multifile aidomain/1.

aidomain(pd).
aidomain(pdb).
% gr and sharing
aidomain(def).
aidomain(gr).
aidomain(java_nullity).
aidomain(share).
aidomain(shareson).
aidomain(shfr).
aidomain(shfrson).
aidomain(shfrnv).
aidomain(son).
aidomain(oo_son).
aidomain(oo_shnltau).
aidomain(share_amgu).
aidomain(sharefree_amgu).
aidomain(shfrlin_amgu).
aidomain(share_clique).
aidomain(sharefree_clique).
aidomain(share_clique_1).
aidomain(share_clique_def).
aidomain(sharefree_clique_def).
aidomain(bshare).

% structure
aidomain(aeq).
aidomain(depth).
aidomain(path).
% types
aidomain(eterms).
aidomain(ptypes).
aidomain(svterms).
aidomain(terms).
aidomain(polyhedra).
aidomain(oo_types).
aidomain(deftypes).
aidomain(java_cha).
% constraints
aidomain(difflsign).
aidomain(fr).
aidomain(frdef).
aidomain(lsign).
% computation
aidomain(det).
aidomain(nf).


%-------------------------------------------------------------------------
% amgu(+,+,+,+,-)                                                        %
%-------------------------------------------------------------------------
:- doc(amgu(AbsInt,Sg,Head,ASub,AMGU),"Perform the abstract unification
   @var{AMGU} between @var{Sg} and @var{Head} given an initial abstract
   substitution @var{ASub} and abstract domain @var{AbsInt}.").

amgu(share,Sg,Head,ASub,NewASub):- 
	share_amgu(Sg,Head,ASub,NewASub).
amgu(share_amgu,Sg,Head,ASub,NewASub):- 
	share_amgu(Sg,Head,ASub,NewASub).
amgu(shfr,Sg,Head,ASub,NewASub):- 
	sharefree_amgu(Sg,Head,ASub,NewASub).
amgu(sharefree_amgu,Sg,Head,ASub,NewASub):- 
	sharefree_amgu(Sg,Head,ASub,NewASub).
amgu(shfrlin_amgu,Sg,Head,ASub,NewASub):-
	shfrlin_amgu(Sg,Head,ASub,NewASub).
amgu(share_clique,Sg,Head,ASub,NewASub):- 
	share_clique_amgu(Sg,Head,ASub,NewASub).
amgu(sharefree_clique,Sg,Head,ASub,NewASub):- 
	sharefree_clique_amgu(Sg,Head,ASub,NewASub).
amgu(bshare,Sg,Head,ASub,NewASub):-
	bshare_amgu(Sg,Head,ASub,NewASub).
%amgu(_AbsInt,_T0,_T1,_ASub,_NewASub):- throw(not_implemented(amgu)).

%------------------------------------------------------------------------
% augment_asub(+,+,+,-)                                                 %
%------------------------------------------------------------------------

:- doc(augment_asub(AbsInt,ASub,Vars,ASub0), "Augment the abstract
   substitution @var{ASub} adding the variables @var{Vars} and then
   resulting the abstract substitution @var{ASub0}.").

augment_asub(share_amgu,ASub,Vars,ASub0):- 
	share_amgu_extend_asub(ASub,Vars,ASub0).
augment_asub(share,ASub,Vars,ASub0):- 
	share_amgu_extend_asub(ASub,Vars,ASub0).
augment_asub(bshare,ASub,Vars,ASub0):- 
	bshare_extend_asub(ASub,Vars,ASub0).
augment_asub(sharefree_amgu,ASub,Vars,ASub0):- 
	sharefree_amgu_extend_asub(ASub,Vars,ASub0).
augment_asub(shfr,ASub,Vars,ASub0):- 
	sharefree_amgu_extend_asub(ASub,Vars,ASub0).
augment_asub(shfrlin_amgu,ASub,Vars,ASub0):- 
 	shfrlin_extend_asub(ASub,Vars,ASub0).
augment_asub(share_clique,ASub,Vars,ASub0):- 
	share_clique_extend_asub(ASub,Vars,ASub0).
augment_asub(sharefree_clique,ASub,Vars,ASub0):- 
	sharefree_clique_extend_asub(ASub,Vars,ASub0).
%augment_asub(_AbsInt,_ASub,_Vars,_ASub0):- throw(not_implemented(extend_asub)).

%------------------------------------------------------------------------
% augment_two_asub(+,+,+,-)                                             %
%------------------------------------------------------------------------

:- doc(augment_two_asub(AbsInt,ASub0,ASub1,ASub), "@var{ASub} is an
           abstract substitution resulting of augmenting two abstract
           substitutions: @var{ASub0} and @var{ASub1} whose domains are
           disjoint.").

augment_two_asub(share,ASub0,ASub1,ASub):- 
	share_amgu_extend_two_asub(ASub0,ASub1,ASub).
augment_two_asub(share_amgu,ASub0,ASub1,ASub):- 
	share_amgu_extend_two_asub(ASub0,ASub1,ASub).
augment_two_asub(bshare,ASub0,ASub1,ASub):- 
	bshare_augment_two_asub(ASub0,ASub1,ASub).

%augment_two_asub(_AbsInt,_ASub0,_ASub1,_ASub):- throw(not_implemented(extend_two_asub)).
%-------------------------------------------------------------------------
% call_to_entry(+,+,+,+,+,+,+,-,-)                                       %
%-------------------------------------------------------------------------
:- doc(call_to_entry(AbsInt,Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo),
   "Obtains the abstract substitution @var{Entry} which results from
   adding the abstraction of the unification @var{Sg} = @var{Head} to
   abstract substitution @var{Proj} (the call substitution for
   @var{Sg} projected on its variables @var{Sv}) and then projecting
   the resulting substitution onto @var{Hv} (the variables of
   @var{Head}) plus @var{Fv} (the free variables of the relevant
   clause). @var{ExtraInfo} is information which may be reused later
   in other abstract operations.").

call_to_entry(pd,_Sv,_Sg,_Hv,_Head,_Fv,Proj,Proj,_ExtraInfo).
call_to_entry(pdb,_Sv,_Sg,_Hv,_Head,_Fv,Proj,Proj,_ExtraInfo).
call_to_entry(gr,Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo):-
	gr_call_to_entry(Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo).
call_to_entry(java_nullity,Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo):-
	java_nullity_call_to_entry(Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo).
call_to_entry(share,Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo):-
	share_call_to_entry(Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo).
call_to_entry(bshare,Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo):-
	bshare_call_to_entry(Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo).
call_to_entry(share_amgu,Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo):-
	share_amgu_call_to_entry(Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo).
call_to_entry(share_clique,Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo):-
	share_clique_call_to_entry(Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo).
call_to_entry(share_clique_1,Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo):-
	share_clique_1_call_to_entry(Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo).
call_to_entry(share_clique_def,Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo):-
	share_clique_def_call_to_entry(Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo).
call_to_entry(sharefree_clique_def,Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo):-
	sharefree_clique_def_call_to_entry(Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo).
call_to_entry(sharefree_amgu,Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo):-
	sharefree_amgu_call_to_entry(Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo).
call_to_entry(shfrlin_amgu,Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo):-
	shfrlin_call_to_entry(Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo).
call_to_entry(sharefree_clique,Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo):-
	sharefree_clique_call_to_entry(Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo).
call_to_entry(shfr,Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo):-
	shfr_call_to_entry(Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo).
call_to_entry(shfret,Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo):-
	shfret_call_to_entry(Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo).
call_to_entry(shfrnv,Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo):-
	shfrnv_call_to_entry(Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo).
call_to_entry(son,_,Sg,Hv,Head,_,Proj,Entry,ExtraInfo):-
	son_call_to_entry(Hv,Sg,Head,Proj,Entry,ExtraInfo).
call_to_entry(oo_son,Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo):-
	oo_son_call_to_entry(Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo).
call_to_entry(oo_shnltau,Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo):-
	oo_shnltau_call_to_entry(Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo).
call_to_entry(shareson,Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo):-
	shareson_call_to_entry(Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo).
call_to_entry(shfrson,Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo):-
	shfrson_call_to_entry(Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo).
call_to_entry(path,_Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo):-
	path_call_to_entry(Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo).
call_to_entry(depth,_Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo):-
	depthk_call_to_entry(Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo).
call_to_entry(eterms,_Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo):-
	eterms_call_to_entry(Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo).
call_to_entry(svterms,_Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo):-
	svterms_call_to_entry(Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo).
call_to_entry(terms,_Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo):-
	terms_call_to_entry(Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo).
call_to_entry(ptypes,_Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo):-
	terms_call_to_entry(Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo).
call_to_entry(deftypes,_Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo):-
	deftypes_call_to_entry(Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo).
call_to_entry(oo_types,Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo):-
	oo_types_call_to_entry(Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo).
call_to_entry(java_cha,Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo):-
	java_cha_call_to_entry(Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo).
call_to_entry(polyhedra,Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo):-
	polyhedra_call_to_entry(Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo).
call_to_entry(fr,_Sv,Sg,Hv,Head,_Fv,Proj,Entry,_ExtraInfo):-
	fr_call_to_entry(Sg,Hv,Head,Proj,Entry).
call_to_entry(def,_Sv,Sg,Hv,Head,_Fv,Proj,Entry,ExtraInfo):-
	def_call_to_entry(Sg,Hv,Head,Proj,Entry,ExtraInfo).
call_to_entry(frdef,_Sv,Sg,Hv,Head,_Fv,Proj,Entry,ExtraInfo):-
	fd_call_to_entry(Sg,Hv,Head,Proj,Entry,ExtraInfo).
call_to_entry(aeq,Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo):-
	aeq_call_to_entry(Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo).
call_to_entry(lsign,_Sv,Sg,_Hv,Head,Fv,Proj,Entry,ExtraInfo):-
	lsign_call_to_entry(Sg,Head,Fv,Proj,Entry,ExtraInfo).
call_to_entry(lsigndef,_Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo):-
	lsigndef_call_to_entry(Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo).
call_to_entry(lsignshfr,Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo):-
	lsignshfr_call_to_entry(Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo).
call_to_entry(difflsign,_Sv,Sg,_Hv,Head,Fv,Proj,Entry,ExtraInfo):-
	simple_lsign_call_to_entry(Sg,Head,Fv,Proj,Entry,ExtraInfo).
call_to_entry(sha,_Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo):-
	sha_call_to_entry(Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo).
call_to_entry(typeshfr,Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo):- % AADEBUG
	shfr_call_to_entry(Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo).
call_to_entry(nf,Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo):-
	nf_call_to_entry(Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo).
call_to_entry(det,Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo):-
	det_call_to_entry(Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo).

%------------------------------------------------------------------------%
% exit_to_prime(+,+,+,+,+,+,+,-)                                         %
%-------------------------------------------------------------------------
:- doc(exit_to_prime(AbsInt,Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime),
   "Computes the abstract substitution @var{Prime} which results from
   adding the abstraction of the unification @var{Sg} = @var{Head} to
   abstract substitution @var{Exit} (the exit substitution for a
   clause @var{Head} projected over its variables @var{Hv}),
   projecting the resulting substitution onto @var{Sv}.").

exit_to_prime(pd,_Sg,_Hv,_Head,_Sv,Exit,_ExtraInfo,Exit).
exit_to_prime(pdb,_Sg,_Hv,_Head,_Sv,Exit,_ExtraInfo,Exit).
exit_to_prime(gr,Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime):-
	gr_exit_to_prime(Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime).
exit_to_prime(java_nullity,Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime):-
	java_nullity_exit_to_prime(Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime).
exit_to_prime(share,Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime):-
	share_exit_to_prime(Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime).
exit_to_prime(share_amgu,Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime):-
	share_amgu_exit_to_prime(Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime).
exit_to_prime(share_clique,Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime):-
	share_clique_exit_to_prime(Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime).
exit_to_prime(share_clique_1,Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime):-
	share_clique_1_exit_to_prime(Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime).
exit_to_prime(share_clique_def,Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime):-
	share_clique_def_exit_to_prime(Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime).
exit_to_prime(sharefree_clique_def,Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime):-
	sharefree_clique_def_exit_to_prime(Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime).
exit_to_prime(sharefree_amgu,Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime):-
	sharefree_amgu_exit_to_prime(Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime).
exit_to_prime(shfrlin_amgu,Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime):-
	shfrlin_exit_to_prime(Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime).
exit_to_prime(sharefree_clique,Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime):-
	sharefree_clique_exit_to_prime(Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime).
exit_to_prime(shfr,Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime):-
	shfr_exit_to_prime(Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime).
exit_to_prime(shfret,Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime):-
	shfret_exit_to_prime(Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime).
exit_to_prime(shfrnv,Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime):-
	shfrnv_exit_to_prime(Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime).
exit_to_prime(son,Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime):-
	son_exit_to_prime(Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime).
exit_to_prime(oo_son,Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime):-
	oo_son_exit_to_prime(Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime).
exit_to_prime(oo_shnltau,Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime):-
	oo_shnltau_exit_to_prime(Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime).
exit_to_prime(shareson,Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime):-
	shareson_exit_to_prime(Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime).
exit_to_prime(shfrson,Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime):-
	shfrson_exit_to_prime(Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime).
exit_to_prime(path,Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime):-
	path_exit_to_prime(Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime).
exit_to_prime(depth,Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime):-
	depthk_exit_to_prime(Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime).
exit_to_prime(eterms,Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime):-
	eterms_exit_to_prime(Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime).
exit_to_prime(svterms,Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime):-
	svterms_exit_to_prime(Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime).
exit_to_prime(terms,Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime):-
	terms_exit_to_prime(Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime).
exit_to_prime(ptypes,Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime):-
	terms_exit_to_prime(Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime).
exit_to_prime(deftypes,Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime):-
	deftypes_exit_to_prime(Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime).
exit_to_prime(oo_types,Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime):-
	oo_types_exit_to_prime(Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime).
exit_to_prime(java_cha,Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime):-
	java_cha_exit_to_prime(Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime).
exit_to_prime(polyhedra,Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime):-
	polyhedra_exit_to_prime(Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime).
exit_to_prime(fr,Sg,Hv,Head,Sv,Exit,_ExtraInfo,Prime):-
	fr_exit_to_prime(Exit,Sg,Hv,Head,Sv,Prime).
exit_to_prime(def,Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime):-
	def_exit_to_prime(Exit,ExtraInfo,Hv,Sv,Head,Sg,Prime).
exit_to_prime(frdef,Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime):-
	fd_exit_to_prime(Exit,Sg,Hv,Head,Sv,ExtraInfo,Prime).
exit_to_prime(aeq,Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime):-
	aeq_exit_to_prime(Exit,Sg,Hv,Head,Sv,ExtraInfo,Prime).
exit_to_prime(lsign,Sg,Hv,Head,_Sv,Exit,ExtraInfo,Prime):-
	lsign_exit_to_prime(Sg,Hv,Head,Exit,ExtraInfo,Prime).
exit_to_prime(lsigndef,Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime):-
	lsigndef_exit_to_prime(Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime).
exit_to_prime(lsignshfr,Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime):-
	lsignshfr_exit_to_prime(Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime).
exit_to_prime(difflsign,Sg,Hv,Head,_Sv,Exit,ExtraInfo,Prime):-
	simple_lsign_exit_to_prime(Sg,Hv,Head,Exit,ExtraInfo,Prime).
exit_to_prime(sha,Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime):-
	sha_exit_to_prime(Exit,Hv,Head,Sv,Sg,Prime,ExtraInfo).
exit_to_prime(nf,Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime):-
	nf_exit_to_prime(Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime).
exit_to_prime(det,Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime):-
	det_exit_to_prime(Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime).

%------------------------------------------------------------------------%
% project(+,+,+,+,-)                                                     %
%------------------------------------------------------------------------%
:- doc(project(AbsInt,Vars,HvFv_u,ASub,Proj), "Projects the
   abstract substitution @var{ASub} onto the variables of list
   @var{Vars} resulting in the projected abstract substitution
   @var{Proj}.").

project(pd,_Vars,_,ASub,ASub).
project(pdb,_Vars,_,ASub,ASub).
project(gr,Vars,_,ASub,Proj):-
	gr_project(ASub,Vars,Proj).
project(java_nullity,Vars,_,ASub,Proj):-
	java_nullity_project(ASub,Vars,Proj).
project(share,Vars,_,ASub,Proj):-
	share_project(Vars,ASub,Proj).
project(bshare,Vars,_,ASub,Proj):-
	bshare_project(Vars,ASub,Proj).
project(share_amgu,Vars,_,ASub,Proj):-
	share_project(Vars,ASub,Proj).
project(share_clique,Vars,_,ASub,Proj):-
	share_clique_project(Vars,ASub,Proj).
project(share_clique_1,Vars,_,ASub,Proj):-
	share_clique_1_project(Vars,ASub,Proj).
project(share_clique_def,Vars,_,ASub,Proj):-
	share_clique_def_project(Vars,ASub,Proj).
project(sharefree_clique_def,Vars,_,ASub,Proj):-
	sharefree_clique_def_project(Vars,ASub,Proj).
project(sharefree_amgu,Vars,_,ASub,Proj):-
	shfr_project(ASub,Vars,Proj).
project(shfrlin_amgu,Vars,_,ASub,Proj):-
	shfrlin_project(ASub,Vars,Proj).
project(sharefree_clique,Vars,_,ASub,Proj):-
	sharefree_clique_project(ASub,Vars,Proj).
project(shfr,Vars,_,ASub,Proj):-
	shfr_project(ASub,Vars,Proj).
project(shfret,Vars,_,ASub,Proj):-
	shfret_project(ASub,Vars,Proj).
project(shfrnv,Vars,_,ASub,Proj):-
	shfr_project(ASub,Vars,Proj).
project(son,Vars,_,ASub,Proj):-
	son_project(Vars,ASub,Proj).
project(oo_son,Vars,_,ASub,Proj):-
	oo_son_project(ASub,Vars,Proj).
project(oo_shnltau,Vars,_,ASub,Proj):-
	oo_shnltau_project(ASub,Vars,Proj).
project(shareson,Vars,_,ASub,Proj):-
	shareson_project(Vars,ASub,Proj).
project(shfrson,Vars,_,ASub,Proj):-
	shfrson_project(Vars,ASub,Proj).
project(path,Vars,_,ASub,Proj):-
	path_project(Vars,ASub,Proj).
project(depth,Vars,_,ASub,Proj):-
	depthk_project(Vars,ASub,Proj).
project(depth,Vars,_,ASub,Proj):-
	depthk_project(Vars,ASub,Proj).
project(eterms,Vars,_,ASub,Proj):-
	eterms_project(Vars,ASub,Proj).
project(svterms,Vars,_,ASub,Proj):-
	svterms_project(Vars,ASub,Proj).
project(terms,Vars,_,ASub,Proj):-
	terms_project(Vars,ASub,Proj).
project(deftypes,Vars,_,ASub,Proj):-
	deftypes_project(Vars,ASub,Proj).
project(oo_types,Vars,_,ASub,Proj):-
	oo_types_project(ASub,Vars,Proj).
project(java_cha,Vars,_,ASub,Proj):-
	java_cha_project(ASub,Vars,Proj).
project(polyhedra,Vars,_,ASub,Proj):-
	polyhedra_project(ASub,Vars,Proj).
project(fr,Vars,_,ASub,Proj):-
	fr_project(ASub,Vars,Proj).
project(def,Vars,_,ASub,Proj):-
	def_project(ASub,Vars,Proj).
project(frdef,Vars,_,ASub,Proj):-
	fd_project(ASub,Vars,Proj).
project(aeq,Vars,_,ASub,Proj):-
	aeq_project(ASub,Vars,Proj).
project(lsign,Vars,HvFv,ASub,Proj):-
	lsign_project(ASub,Vars,HvFv,Proj).
% check that HvFv is sorted!
project(lsigndef,Vars,HvFv,ASub,Proj):-
	lsigndef_project(ASub,Vars,HvFv,Proj).
project(lsignshfr,Vars,HvFv,ASub,Proj):-
	lsignshfr_project(ASub,Vars,HvFv,Proj).
project(difflsign,Vars,HvFv,ASub,Proj):-
	simple_lsign_project(ASub,Vars,HvFv,Proj).
project(sha,Vars,_HvFv,ASub,Proj):-
	sha_project(ASub,Vars,Proj).
project(nf,Vars,_HvFv,ASub,Proj):-
	nf_project(ASub,Vars,Proj).
project(det,Vars,_HvFv,ASub,Proj):-
	det_project(ASub,Vars,Proj).

%-------------------------------------------------------------------------
% widencall(+,+,+,-)                                                     %
%-------------------------------------------------------------------------
:- doc(widencall(AbsInt,ASub0,ASub1,ASub),"@var{ASub} is the result of
   widening abstract substitution @var{ASub0} and @var{ASub1}, which
   are supposed to be consecutive call patterns in a fixpoint computation.").

widencall(eterms,Prime0,Prime1,NewPrime):- !,
	eterms_widencall(Prime0,Prime1,NewPrime).
widencall(svterms,Prime0,Prime1,NewPrime):- !,
	svterms_widencall(Prime0,Prime1,NewPrime).
widencall(terms,Prime0,Prime1,NewPrime):- !,
	terms_widencall(Prime0,Prime1,NewPrime).
widencall(ptypes,Prime0,Prime1,NewPrime):- !,
	ptypes_widencall(Prime0,Prime1,NewPrime).
widencall(deftypes,Prime0,Prime1,NewPrime):- !,
	deftypes_widencall(Prime0,Prime1,NewPrime).
widencall(polyhedra,Prime0,Prime1,NewPrime):- !, 
	polyhedra_widencall(Prime0,Prime1,NewPrime). 
widencall(nf,Prime0,Prime1,NewPrime):- !,
	nf_widencall(Prime0,Prime1,NewPrime).
widencall(det,Prime0,Prime1,NewPrime):- !,
	det_widencall(Prime0,Prime1,NewPrime).
widencall(shfret,Prime0,Prime1,NewPrime):- !,
	shfret_widencall(Prime0,Prime1,NewPrime).
%% widencall(AbsInt,Prime0,Prime1,NewPrime):-
%% 	compute_lub(AbsInt,[Prime0,Prime1],NewPrime).

%-------------------------------------------------------------------------
% widen(+,+,+,-)                                                         %
%-------------------------------------------------------------------------
:- doc(widen(AbsInt,ASub0,ASub1,ASub),"@var{ASub} is the result of
   widening abstract substitution @var{ASub0} and @var{ASub1}, which
   are supposed to be consecutive approximations to the same abstract
   value.").

widen(eterms,Prime0,Prime1,NewPrime):- !,
	eterms_widen(Prime0,Prime1,NewPrime).
widen(svterms,Prime0,Prime1,NewPrime):- !,
	svterms_widen(Prime0,Prime1,NewPrime).
widen(terms,Prime0,Prime1,NewPrime):- !,
	terms_widen(Prime0,Prime1,NewPrime).
widen(ptypes,Prime0,Prime1,NewPrime):- !,
	ptypes_widen(Prime0,Prime1,NewPrime).
widen(deftypes,Prime0,Prime1,NewPrime):- !,
	deftypes_widen(Prime0,Prime1,NewPrime).
widen(polyhedra,Prime0,Prime1,NewPrime):- !, 
        polyhedra_widen(Prime0,Prime1,NewPrime). 
widen(nf,Prime0,Prime1,NewPrime):- !,
	nf_widen(Prime0,Prime1,NewPrime).
widen(det,Prime0,Prime1,NewPrime):- !,
	det_widen(Prime0,Prime1,NewPrime).
widen(shfret,Prime0,Prime1,NewPrime):- !,
	shfret_widen(Prime0,Prime1,NewPrime).
widen(AbsInt,Prime0,Prime1,NewPrime):-
	compute_lub(AbsInt,[Prime0,Prime1],NewPrime).

%-------------------------------------------------------------------------
% normalize_asub(+,+,-)                                                  %
%-------------------------------------------------------------------------
:- doc(normalize_asub(AbsInt,ASub0,ASub1),"@var{ASub1} is the
   result of normalizing abstract substitution @var{ASub0}. This is
   required in some domains, specially to perform the widening.").

% some domains need normalization to perform the widening:
normalize_asub(_AbsInt,Prime,Prime).

%-------------------------------------------------------------------------
% compute_lub(+,+,-)                                                     %
%-------------------------------------------------------------------------
:- doc(compute_lub(AbsInt,ListASub,LubASub),"@var{LubASub} is the
   least upper bound of the abstract substitutions in list @var{ListASub}.").

compute_lub(pd,_ListAsub,top).
compute_lub(pdb,ListAsub,LubASub):-
	pdb_compute_lub(ListAsub,LubASub).
compute_lub(gr,ListAsub,LubASub):-
	gr_compute_lub(ListAsub,LubASub).
compute_lub(java_nullity,ListAsub,LubASub):-
	java_nullity_compute_lub(ListAsub,LubASub).
compute_lub(share,ListAsub,LubASub):-
	share_compute_lub(ListAsub,LubASub).
compute_lub(bshare,ListAsub,LubASub):-
	bshare_compute_lub(ListAsub,LubASub).
compute_lub(share_amgu,ListAsub,LubASub):-
	share_compute_lub(ListAsub,LubASub).
compute_lub(share_clique,ListAsub,LubASub):-
	share_clique_compute_lub(ListAsub,LubASub).
compute_lub(share_clique_1,ListAsub,LubASub):-
	share_clique_1_compute_lub(ListAsub,LubASub).
compute_lub(share_clique_def,ListAsub,LubASub):-
	share_clique_def_compute_lub(ListAsub,LubASub).
compute_lub(sharefree_clique_def,ListAsub,LubASub):-
	sharefree_clique_def_compute_lub(ListAsub,LubASub).
compute_lub(sharefree_amgu,ListAsub,LubASub):-
	shfr_compute_lub(ListAsub,LubASub).
compute_lub(shfrlin_amgu,ListAsub,LubASub):-
	shfrlin_compute_lub(ListAsub,LubASub).
compute_lub(sharefree_clique,ListAsub,LubASub):-
	sharefree_clique_compute_lub(ListAsub,LubASub).
compute_lub(shfr,ListAsub,LubASub):-
	shfr_compute_lub(ListAsub,LubASub).
compute_lub(shfret,ListAsub,LubASub):-
	shfret_compute_lub(ListAsub,LubASub).
compute_lub(shfrnv,ListAsub,LubASub):-
	shfrnv_compute_lub(ListAsub,LubASub).
compute_lub(son,ListAsub,LubASub):-
	son_compute_lub(ListAsub,LubASub).
compute_lub(oo_son,ListAsub,LubASub):-
	oo_son_compute_lub(ListAsub,LubASub).
compute_lub(oo_shnltau,ListAsub,LubASub):-
	oo_shnltau_compute_lub(ListAsub,LubASub).
compute_lub(shareson,ListAsub,LubASub):-
	shareson_compute_lub(ListAsub,LubASub).
compute_lub(shfrson,ListAsub,LubASub):-
	shfrson_compute_lub(ListAsub,LubASub).
compute_lub(path,ListAsub,LubASub):-
	path_compute_lub(ListAsub,LubASub).
compute_lub(depth,ListASub,LubASub):-
	depthk_compute_lub(ListASub,LubASub).
compute_lub(eterms,ListASub,LubASub):-
	eterms_compute_lub(ListASub,LubASub).
compute_lub(svterms,ListASub,LubASub):-
	svterms_compute_lub(ListASub,LubASub).
compute_lub(terms,ListASub,LubASub):-
	terms_compute_lub(ListASub,LubASub).
compute_lub(ptypes,ListASub,LubASub):-
	terms_compute_lub(ListASub,LubASub).
compute_lub(deftypes,ListASub,LubASub):-
	deftypes_compute_lub(ListASub,LubASub).
compute_lub(oo_types,ListAsub,LubASub):-
	oo_types_compute_lub(ListAsub,LubASub).
compute_lub(java_cha,ListAsub,LubASub):-
	java_cha_compute_lub(ListAsub,LubASub).
compute_lub(polyhedra,ListASub,LubASub):-
	polyhedra_compute_lub(ListASub,LubASub).
%% VD specific version of lub used at procedure exit
compute_lub(fr,ListASub,LubASub):-
	fr_compute_lub(ListASub,LubASub).
compute_lub(def,ListASub,LubASub):-
	def_compute_lub(ListASub,LubASub).
compute_lub(frdef,ListASub,LubASub):-
	fd_compute_lub(ListASub,LubASub).
compute_lub(aeq,ListASub,LubASub):-
	aeq_compute_lub(ListASub,LubASub).
compute_lub(lsign,ListASub,LubASub):-
	lsign_compute_lub(ListASub,LubASub).
compute_lub(lsigndef,ListASub,LubASub):-
	lsigndef_compute_lub(ListASub,LubASub).
compute_lub(lsignshfr,ListASub,LubASub):-
	lsignshfr_compute_lub(ListASub,LubASub).
compute_lub(difflsign,ListASub,LubASub):-
	lsign_compute_lub(ListASub,LubASub).
compute_lub(sha,ListASub,LubASub):-
	sha_compute_lub(ListASub,LubASub).
compute_lub(typeshfr,ListASub,LubASub):-  %% AADEBUG added
	shfr_compute_lub(ListASub,LubASub). 
compute_lub(nf,ListASub,LubASub):-
	nf_compute_lub(ListASub,LubASub). 
compute_lub(det,ListASub,LubASub):-
	det_compute_lub(ListASub,LubASub). 

/*
%% VD general version of lub used for printing the output
compute_lub_general(fr,ListASub,LubASub):-
	fr_compute_lub_general(ListASub,LubASub).
compute_lub_general(def,ListASub,LubASub):-
	def_compute_lub(ListASub,LubASub).
compute_lub_general(frdef,ListASub,LubASub):-
	fd_compute_lub_general(ListASub,LubASub).
compute_lub_general(aeq,ListASub,LubASub):-
 	aeq_compute_lub(ListASub,LubASub).

fr_compute_lub_general(ListASub,ListASub).
fd_compute_lub_general(ListASub,ListASub).

do_compute_lub(AbsInt,SubstList,Subst):-
	( AbsInt = fr ; AbsInt = fd ), !,
	compute_lub_general(AbsInt,SubstList,Subst).
%% do_compute_lub(AbsInt,SubstList,Subst):-
%% 	there_is_delay, !,
%% 	del_compute_lub(SubstList,AbsInt,Subst).
do_compute_lub(AbsInt,SubstList,Subst):-
	compute_lub(AbsInt,SubstList,Subst).
*/

:- doc(hide,compute_clauses_lub/4).

compute_clauses_lub(nf,Proj,ASub,Lub):- !,
	nf_compute_clauses_lub(ASub,Proj,Lub).
compute_clauses_lub(det,Proj,ASub,Lub):- !,
	det_compute_clauses_lub(ASub,Proj,Lub).
compute_clauses_lub(_AbsInt,_Proj,Lub,Lub).
/*
%------------------------------------------------------------------------%
% lub_all(+,+,+,-)                                                       %
% lub_all(AbsInt,ListPatterns,Goal,LubbedPattern)                        %
% It computes the lub of a set of patterns (AGoal,AProj,APrime) wrt Goal %
% returning the pattern (Goal,Proj,Prime)                                %
%------------------------------------------------------------------------%

lub_all(AbsInt,[(Goal0,Proj0,Prime0)|Patterns],Goal,Lub):-
	varset(Goal,Hv),
	project_pattern(Goal0,Proj0,Prime0,AbsInt,Goal,Hv,Proj,Prime),
	lub_all0(Patterns,Goal,Hv,Proj,Prime,AbsInt,Lub).

lub_all0([(Goal0,Proj0,Prime0)|Patterns],Goal,Hv,Proj1,Prime1,AbsInt,Lub):-
	project_pattern(Goal0,Proj0,Prime0,AbsInt,Goal,Hv,Proj2,Prime2),
	compute_lub_el(AbsInt,Proj1,Proj2,Proj),
	compute_lub_el(AbsInt,Prime1,Prime2,Prime),
	lub_all0(Patterns,Goal,Hv,Proj,Prime,AbsInt,Lub).
lub_all0([],Goal,_Hv,Proj,Prime,_AbsInt,(Goal,Proj,Prime)).

project_pattern(Goal0,Proj0,Prime0,AbsInt,Goal,Hv,Proj,Prime):-
	varset(Goal0,Sv),
	abs_sort(AbsInt,Proj0,Proj_s),
	call_to_entry0(Proj_s,AbsInt,Sv,Goal0,Hv,Goal,[],Proj,_),
	abs_sort(AbsInt,Prime0,Prime_s),
	call_to_entry0(Prime_s,AbsInt,Sv,Goal0,Hv,Goal,[],Prime,_).

call_to_entry0('$bottom',_AbsInt,_Sv,_Goal0,_Hv,_Goal,_Fv,'$bottom',_E):- !.
call_to_entry0(Proj_s,AbsInt,Sv,Goal0,Hv,Goal,Fv,Proj,E):-
	call_to_entry(AbsInt,Sv,Goal0,Hv,Goal,Fv,Proj_s,Proj,E).
*/
%-------------------------------------------------------------------------
% identical_proj(+,+,+,+,+)                                              %
%-------------------------------------------------------------------------
:- doc(identical_proj(AbsInt,Sg,Proj,Sg1,Proj1),
   "Abstract patterns @var{Sg}:@var{Proj} and @var{Sg1}:@var{Proj1} are 
    equivalent in domain @var{AbsInt}. Note that @var{Proj} is assumed 
    to be already sorted.").

identical_proj(AbsInt,Sg,Proj,Sg1,Proj1):-
	variant(Sg,Sg1),
	Sg = Sg1,
        abs_sort(AbsInt,Proj1,Proj1_s),
	identical_abstract(AbsInt,Proj,Proj1_s).

:- doc(identical_proj_1(AbsInt,Sg,Proj,Sg1,Proj1,Prime1,Prime2), "Abstract
patterns @var{Sg}:@var{Proj} and @var{Sg1}:@var{Proj1} are equivalent
in domain @var{AbsInt}. Note that @var{Proj} is assumed to be already
sorted. It is different from @tt{identical_proj/5} because it can be
true although @var{Sg} and @var{Sg1} are not variant").

identical_proj_1(AbsInt,Sg,Proj,Sg1,Proj1,Prime1,Prime2):-
	\+ variant(Sg,Sg1),
	linear(Sg1),

	varset(Sg1,Hv),
	varset(Sg,Hvv),

	functor(Sg,F,A),
	functor(Norm,F,A),
	varset(Norm,Hvnorm),

	call_to_entry(AbsInt,_Sv,Sg,Hvnorm,Norm,[],Proj,Entry,_),
	call_to_entry(AbsInt,_Sv,Sg1,Hvnorm,Norm,[],Proj1,Entry1,_),
	identical_abstract(AbsInt,Entry,Entry1),

	% call_to_entry(AbsInt,_Sv,Sg,Hv,Sg1,[],Proj,Entry,_),
        % abs_sort(AbsInt,Entry,Entry_s),
        % abs_sort(AbsInt,Proj1,Proj1_s),
	% identical_abstract(AbsInt,Proj1_s,Entry_s),

	% call_to_entry(AbsInt,_Sv,Sg1,Hvv,Sg,[],Proj1,Entry1,_),
        % abs_sort(AbsInt,Entry1,Entry1_s),
        % abs_sort(AbsInt,Proj,Proj_s),
	% identical_abstract(AbsInt,Proj_s,Entry1_s),



	each_abs_sort(Prime1,AbsInt,Prime1_s),
	each_exit_to_prime(Prime1_s,AbsInt,Sg,Hv,Sg1,Hvv,(no,Proj),Prime2).


%--------------------------------------------------------------------------
% identical_abstract(+,+,+)                                               %
%--------------------------------------------------------------------------
:- doc(identical_abstract(AbsInt,ASub1,ASub2),
   "Succeeds if, in the particular abstract domain, the two abstract
    substitutions @var{ASub1} and @var{ASub2} are defined on the same
    variables and are equivalent.").

identical_abstract(pd,ASub1,ASub2):-
	ASub1 == ASub2.
identical_abstract(pdb,ASub1,ASub2):-
	ASub1 == ASub2.
identical_abstract(gr,ASub1,ASub2):-
	ASub1 == ASub2.
identical_abstract(java_nullity,ASub1,ASub2):-
	ASub1 == ASub2.
identical_abstract(share,ASub1,ASub2):-
	ASub1 == ASub2.
identical_abstract(bshare,ASub1,ASub2):-
	bshare_identical_abstract(ASub1,ASub2).
identical_abstract(share_amgu,ASub1,ASub2):-
	ASub1 == ASub2.
identical_abstract(share_clique,ASub1,ASub2):-
	share_clique_identical_abstract(ASub1,ASub2).
identical_abstract(share_clique_1,ASub1,ASub2):-
	share_clique_1_identical_abstract(ASub1,ASub2).
identical_abstract(share_clique_def,ASub1,ASub2):-
	share_clique_def_identical_abstract(ASub1,ASub2).
identical_abstract(sharefree_clique_def,ASub1,ASub2):-
	sharefree_clique_def_identical_abstract(ASub1,ASub2).
identical_abstract(sharefree_amgu,ASub1,ASub2):-
	ASub1 == ASub2.
identical_abstract(shfrlin_amgu,ASub1,ASub2):-	
	ASub1 == ASub2.
identical_abstract(sharefree_clique,ASub1,ASub2):-
	sharefree_clique_identical_abstract(ASub1,ASub2).
identical_abstract(shfr,ASub1,ASub2):-
	ASub1 == ASub2.
identical_abstract(shfret,ASub1,ASub2):-
	shfret_identical_abstract(ASub1,ASub2).
identical_abstract(shfrnv,ASub1,ASub2):-
	ASub1 == ASub2.
identical_abstract(son,ASub1,ASub2):-
	ASub1 == ASub2.
identical_abstract(oo_son,ASub1,ASub2):-
	oo_son_identical_abstract(ASub1,ASub2).
identical_abstract(oo_shnltau,ASub1,ASub2):-
	ASub1 == ASub2.
identical_abstract(shareson,ASub1,ASub2):-
	ASub1 == ASub2.
identical_abstract(shfrson,ASub1,ASub2):-
	ASub1 == ASub2.
identical_abstract(path,ASub1,ASub2):-
	ASub1 == ASub2.
identical_abstract(depth,ASub1,ASub2):-
	depthk_identical_abstract(ASub1,ASub2).
identical_abstract(eterms,ASub1,ASub2):-
	eterms_identical_abstract(ASub1,ASub2).
identical_abstract(svterms,ASub1,ASub2):-
	svterms_identical_abstract(ASub1,ASub2).
identical_abstract(terms,ASub1,ASub2):-
	terms_identical_abstract(ASub1,ASub2).
identical_abstract(ptypes,ASub1,ASub2):-
	terms_identical_abstract(ASub1,ASub2).
identical_abstract(deftypes,ASub1,ASub2):-
	deftypes_identical_abstract(ASub1,ASub2).
identical_abstract(oo_types,ASub1,ASub2):-
	ASub1 == ASub2.
identical_abstract(java_cha,ASub1,ASub2):-
	ASub1 == ASub2.
identical_abstract(polyhedra,ASub1,ASub2):-
	polyhedra_identical_abstract(ASub1,ASub2).
identical_abstract(fr,ASub1,ASub2):-
	fr_identical_abstract(ASub1,ASub2).
identical_abstract(def,ASub1,ASub2):-
	ASub1 == ASub2.
identical_abstract(frdef,ASub1,ASub2):-
	fd_identical_abstract(ASub1,ASub2).
identical_abstract(aeq,ASub1,ASub2):-
	aeq_identical_abstract(ASub1,ASub2).
identical_abstract(lsign,ASub1,ASub2):-
	ASub1 == ASub2.
identical_abstract(lsigndef,ASub1,ASub2):-
	ASub1 == ASub2.
identical_abstract(lsignshfr,ASub1,ASub2):-
	ASub1 == ASub2.
identical_abstract(difflsign,ASub1,ASub2):-
	ASub1 == ASub2.
identical_abstract(sha,ASub1,ASub2):-
	sha_identical_abstract(ASub1,ASub2).
identical_abstract(typeshfr,ASub1,ASub2):-  %% AADEBUG
	identical_abstract(shfr,ASub1,ASub2).
identical_abstract(nf,ASub1,ASub2):-
	nf_identical_abstract(ASub1,ASub2).
identical_abstract(det,ASub1,ASub2):-
	det_identical_abstract(ASub1,ASub2).


:- doc(hide,fixpoint_covered/3).

fixpoint_covered(nf,Prime0,Prime1):- !,
	nf_less_or_equal(Prime0,Prime1).
fixpoint_covered(det,Prime0,Prime1):- !,
	det_less_or_equal(Prime0,Prime1).
fixpoint_covered(AbsInt,Prime0,Prime1):-
	current_pp_flag(multi_call,on),!,
	identical_abstract(AbsInt,Prime0,Prime1).
fixpoint_covered(AbsInt,Prime0,Prime1):-
	current_pp_flag(multi_call,off),!,
	less_or_equal(AbsInt,Prime0,Prime1).

%--------------------------------------------------------------------------
% abs_sort(+,+,-)                                                         %
%--------------------------------------------------------------------------
:- doc(abs_sort(AbsInt,ASub_u,ASub),"@var{ASub} is the result of
	sorting abstract substitution @var{ASub_u}.").

abs_sort(pd,ASub,ASub).
abs_sort(pdb,ASub,ASub).
abs_sort(gr,ASub,ASub_s):-
	gr_sort(ASub,ASub_s).
abs_sort(java_nullity,ASub,ASub_s):-
	java_nullity_sort(ASub,ASub_s).
abs_sort(share,ASub,ASub_s):-
	share_sort(ASub,ASub_s).
abs_sort(bshare,ASub,ASub_s):-
	bshare_sort(ASub,ASub_s).
abs_sort(share_amgu,ASub,ASub_s):-
	share_sort(ASub,ASub_s).
abs_sort(share_clique,ASub,ASub_s):-
	share_clique_sort(ASub,ASub_s).
abs_sort(share_clique_1,ASub,ASub_s):-
	share_clique_sort(ASub,ASub_s).
abs_sort(share_clique_def,ASub,ASub_s):-
	share_clique_def_sort(ASub,ASub_s).
abs_sort(sharefree_clique_def,ASub,ASub_s):-
	sharefree_clique_def_sort(ASub,ASub_s).
abs_sort(sharefree_amgu,ASub,ASub_s):-
	shfr_sort(ASub,ASub_s).
abs_sort(shfrlin_amgu,ASub,ASub_s):-
	shfrlin_sort(ASub,ASub_s).
abs_sort(sharefree_clique,ASub,ASub_s):-
	sharefree_clique_sort(ASub,ASub_s).
abs_sort(shfr,ASub,ASub_s):-
	shfr_sort(ASub,ASub_s).
abs_sort(shfret,ASub,ASub_s):-
	shfret_sort(ASub,ASub_s).
abs_sort(shfrnv,ASub,ASub_s):-
	shfr_sort(ASub,ASub_s).
abs_sort(son,ASub,ASub_s):-
	son_sort(ASub,ASub_s).
abs_sort(oo_son,ASub,ASub_s):-
	oo_son_sort(ASub,ASub_s).
abs_sort(oo_shnltau,ASub,ASub_s):-
	oo_shnltau_sort(ASub,ASub_s).
abs_sort(shareson,ASub,ASub_s):-
	shareson_sort(ASub,ASub_s).
abs_sort(shfrson,ASub,ASub_s):-
	shfrson_sort(ASub,ASub_s).
abs_sort(path,ASub,ASub_s):-
	path_sort(ASub,ASub_s).
abs_sort(depth,ASub,ASub_s):-
	depthk_sort(ASub,ASub_s).
abs_sort(eterms,ASub,ASub_s):-
	eterms_sort(ASub,ASub_s).
abs_sort(svterms,ASub,ASub_s):-
	svterms_sort(ASub,ASub_s).
abs_sort(terms,ASub,ASub_s):-
	terms_sort(ASub,ASub_s).
abs_sort(ptypes,ASub,ASub_s):-
	terms_sort(ASub,ASub_s).
abs_sort(deftypes,ASub,ASub_s):-
	terms_sort(ASub,ASub_s).
abs_sort(oo_types,ASub,ASub_s):-
	oo_types_sort(ASub,ASub_s).
abs_sort(java_cha,ASub,ASub_s):-
	java_cha_sort(ASub,ASub_s).
abs_sort(polyhedra,ASub,ASub_s):-
	polyhedra_sort(ASub,ASub_s).
abs_sort(fr,ASub,ASub_s):-
	fr_sort(ASub,ASub_s).
abs_sort(def,ASub,ASub_s):-
	def_sort(ASub,ASub_s).
abs_sort(frdef,ASub,ASub_s):-
	fd_sort(ASub,ASub_s).
abs_sort(aeq,ASub,ASub_s):-
	aeq_sort(ASub,ASub_s).
abs_sort(lsign,ASub,ASub_s):-
	lsign_sort(ASub,ASub_s).
abs_sort(lsigndef,ASub,ASub_s):-
	lsigndef_sort(ASub,ASub_s).
abs_sort(lsignshfr,ASub,ASub_s):-
	lsignshfr_sort(ASub,ASub_s).
abs_sort(difflsign,ASub,ASub_s):-
	simple_lsign_sort(ASub,ASub_s).
abs_sort(sha,ASub,ASub_s):-
	sha_abs_sort(ASub,ASub_s).
abs_sort(typeshfr,ASub,ASub_s):- %% AADEBUG
	abs_sort(shfr,ASub,ASub_s).
abs_sort(nf,ASub,ASub_s):-
	nf_sort(ASub,ASub_s).
abs_sort(det,ASub,ASub_s):-
	det_sort(ASub,ASub_s).

%------------------------------------------------------------------------%
% extend(+,+,+,+,-)                                                      %
%------------------------------------------------------------------------%
:- doc(extend(AbsInt,Prime,Sv,Call,Succ),
   "@var{Succ} is the extension the information given by @var{Prime} (success
    abstract substitution over the goal variables @var{Sv}) to the rest of the
    variables of the clause in which the goal occurs (those over which
    abstract substitution @var{Call} is defined on). I.e., it is like a
    conjunction of the information in @var{Prime} and @var{Call}, except that
    they are defined over different sets of variables, and that @var{Prime} is
    a successor substitution to @var{Call} in the execution of the program.
    ").

extend(pd,Prime,_Sv,_Call,Prime).
extend(pdb,Prime,Sv,Call,Succ):-
	pdb_extend(Prime,Sv,Call,Succ).
extend(gr,Prime,Sv,Call,Succ):-
	gr_extend(Prime,Sv,Call,Succ).
extend(java_nullity,Prime,Sv,Call,Succ):-
	java_nullity_extend(Prime,Sv,Call,Succ).
extend(share,Prime,Sv,Call,Succ):-
	share_extend(Prime,Sv,Call,Succ).
extend(share_amgu,Prime,Sv,Call,Succ):-
	share_extend(Prime,Sv,Call,Succ).
extend(share_clique,Prime,Sv,Call,Succ):-
	share_clique_extend(Prime,Sv,Call,Succ).
extend(share_clique_1,Prime,Sv,Call,Succ):-
	share_clique_1_extend(Prime,Sv,Call,Succ).
extend(share_clique_def,Prime,Sv,Call,Succ):-
	share_clique_def_extend(Prime,Sv,Call,Succ).
extend(sharefree_clique_def,Prime,Sv,Call,Succ):-
	sharefree_clique_def_extend(Prime,Sv,Call,Succ).
extend(sharefree_amgu,Prime,Sv,Call,Succ):-
	shfr_extend(Prime,Sv,Call,Succ).
extend(shfrlin_amgu,Prime,Sv,Call,Succ):-
	shfrlin_extend(Prime,Sv,Call,Succ).
extend(sharefree_clique,Prime,Sv,Call,Succ):-
	sharefree_clique_extend(Prime,Sv,Call,Succ).
extend(shfr,Prime,Sv,Call,Succ):-
	shfr_extend(Prime,Sv,Call,Succ).
extend(shfret,Prime,Sv,Call,Succ):-
	shfret_extend(Prime,Sv,Call,Succ).
extend(shfrnv,Prime,Sv,Call,Succ):-
	shfrnv_extend(Prime,Sv,Call,Succ).
extend(son,Prime,Sv,Call,Succ):-
	son_extend(Prime,Sv,Call,Succ).
extend(oo_son,Prime,Sv,Call,Succ):-
	oo_son_extend(Prime,Sv,Call,Succ).
extend(oo_shnltau,Prime,Sv,Call,Succ):-
	oo_shnltau_extend(Prime,Sv,Call,Succ).
extend(shareson,Prime,Sv,Call,Succ):-
	shareson_extend(Prime,Sv,Call,Succ).
extend(shfrson,Prime,Sv,Call,Succ):-
	shfrson_extend(Prime,Sv,Call,Succ).
extend(path,Prime,Sv,Call,Succ):-
	path_extend(Prime,Sv,Call,Succ).
extend(depth,Prime,Sv,Call,Succ):-
	depthk_extend(Prime,Sv,Call,Succ).
extend(eterms,Prime,Sv,Call,Succ):-
	eterms_extend(Prime,Sv,Call,Succ).
extend(svterms,Prime,Sv,Call,Succ):-
	svterms_extend(Prime,Sv,Call,Succ).
extend(terms,Prime,Sv,Call,Succ):-
	terms_extend(Prime,Sv,Call,Succ).
extend(ptypes,Prime,Sv,Call,Succ):-
	terms_extend(Prime,Sv,Call,Succ).
extend(deftypes,Prime,Sv,Call,Succ):-
	deftypes_extend(Prime,Sv,Call,Succ).
extend(oo_types,Prime,Sv,Call,Succ):-
	oo_types_extend(Prime,Sv,Call,Succ).
extend(java_cha,Prime,Sv,Call,Succ):-
	java_cha_extend(Prime,Sv,Call,Succ).
extend(polyhedra,Prime,Sv,Call,Succ):-
	polyhedra_extend(Prime,Sv,Call,Succ).
extend(fr,Prime,Sv,Call,Succ):-
	fr_extend(Prime,Sv,Call,Succ).
extend(def,Prime,_Sv,Call,Succ):-
	def_extend(Prime,Call,Succ).
extend(frdef,Prime,Sv,Call,Succ):-
	fd_extend(Prime,Sv,Call,Succ).
extend(aeq,Prime,_Sv,Call,Succ):-
	aeq_extend(Prime,Call,Succ).
extend(lsign,Prime,_Sv,Call,Succ):-
	lsign_extend(Prime,Call,Succ).
extend(lsigndef,Prime,Sv,Call,Succ):-
	lsigndef_extend(Prime,Sv,Call,Succ).
extend(lsignshfr,Prime,Sv,Call,Succ):-
	lsignshfr_extend(Prime,Sv,Call,Succ).
extend(difflsign,Prime,_Sv,Call,Succ):-
	simple_lsign_extend(Prime,Call,Succ).
extend(sha,Prime,Sv,Call,Succ):-
	sha_extend(Prime,Sv,Call,_Proj,Succ).
extend(nf,Prime,Sv,Call,Succ):-
	nf_extend(Prime,Sv,Call,Succ).
extend(det,Prime,Sv,Call,Succ):-
	det_extend(Prime,Sv,Call,Succ).

%-------------------------------------------------------------------------
% less_or_equal_proj(+,+,+,+,+)                                          %
%-------------------------------------------------------------------------
:- doc(less_or_equal_proj(AbsInt,Sg,Proj,Sg1,Proj1),
   "Abstract pattern @var{Sg}:@var{Proj} is less general or equivalent to
    abstract pattern @var{Sg1}:@var{Proj1} in domain @var{AbsInt}.").

less_or_equal_proj(AbsInt,Sg,Proj,Sg1,Proj1):-
	variant(Sg,Sg1),
	Sg = Sg1,
        abs_sort(AbsInt,Proj1,Proj1_s),
	less_or_equal(AbsInt,Proj,Proj1_s).

%------------------------------------------------------------------------%
% less_or_equal(+,+,+)                                                   %
%------------------------------------------------------------------------%
:- doc(less_or_equal(AbsInt,ASub0,ASub1),"Succeeds if @var{ASub1}
	is more general or equivalent to @var{ASub0}.").

less_or_equal(pd,_,_).
less_or_equal(pdb,ASub0,ASub1):-
	pdb_less_or_equal(ASub0,ASub1).
less_or_equal(gr,ASub0,ASub1):-
	gr_less_or_equal(ASub0,ASub1).
less_or_equal(java_nullity,ASub0,ASub1):-
	java_nullity_less_or_equal(ASub0,ASub1).
less_or_equal(share,ASub0,ASub1):-
	share_less_or_equal(ASub0,ASub1).
less_or_equal(share_amgu,ASub0,ASub1):-
	share_less_or_equal(ASub0,ASub1).
less_or_equal(share_clique,ASub0,ASub1):-
	share_clique_less_or_equal(ASub0,ASub1).
less_or_equal(share_clique_1,ASub0,ASub1):-
	share_clique_1_less_or_equal(ASub0,ASub1).
less_or_equal(share_clique_def,ASub0,ASub1):-
	share_clique_def_less_or_equal(ASub0,ASub1).
less_or_equal(sharefree_clique_def,ASub0,ASub1):-
	sharefree_clique_def_less_or_equal(ASub0,ASub1).
less_or_equal(sharefree_amgu,ASub0,ASub1):-
	shfr_less_or_equal(ASub0,ASub1).
less_or_equal(shfrlin_amgu,ASub0,ASub1):-
	shfrlin_less_or_equal(ASub0,ASub1).
less_or_equal(sharefree_clique,ASub0,ASub1):-
	sharefree_clique_less_or_equal(ASub0,ASub1).
less_or_equal(shfr,ASub0,ASub1):-
	shfr_less_or_equal(ASub0,ASub1).
less_or_equal(shfret,ASub0,ASub1):-
	shfret_less_or_equal(ASub0,ASub1).
less_or_equal(shfrnv,ASub0,ASub1):-
	shfrnv_less_or_equal(ASub0,ASub1).
less_or_equal(son,ASub0,ASub1):-
	son_less_or_equal(ASub0,ASub1).
less_or_equal(oo_son,ASub0,ASub1):-
	oo_son_less_or_equal(ASub0,ASub1).
less_or_equal(oo_shnltau,ASub0,ASub1):-
	oo_shnltau_less_or_equal(ASub0,ASub1).
less_or_equal(shareson,ASub0,ASub1):-
	shareson_less_or_equal(ASub0,ASub1).
less_or_equal(shfrson,ASub0,ASub1):-
	shfrson_less_or_equal(ASub0,ASub1).
less_or_equal(path,ASub0,ASub1):-
	path_less_or_equal(ASub0,ASub1).
less_or_equal(depth,ASub0,ASub1):-
	depthk_less_or_equal(ASub0,ASub1).
less_or_equal(eterms,ASub0,ASub1):-
	eterms_less_or_equal(ASub0,ASub1).
less_or_equal(svterms,ASub0,ASub1):-
	svterms_less_or_equal(ASub0,ASub1).
less_or_equal(terms,ASub0,ASub1):-
	terms_less_or_equal(ASub0,ASub1).
less_or_equal(ptypes,ASub0,ASub1):-
	terms_less_or_equal(ASub0,ASub1).
less_or_equal(deftypes,ASub0,ASub1):-
	deftypes_less_or_equal(ASub0,ASub1).
less_or_equal(oo_types,ASub0,ASub1):-
	oo_types_less_or_equal(ASub0,ASub1).
less_or_equal(java_cha,ASub0,ASub1):-
	java_cha_less_or_equal(ASub0,ASub1).
less_or_equal(polyhedra,ASub0,ASub1):-
	polyhedra_less_or_equal(ASub0,ASub1).
less_or_equal(fr,ASub0,ASub1):-
	fr_less_or_equal(ASub0,ASub1).
less_or_equal(def,ASub0,ASub1):-
	def_less_or_equal(ASub0,ASub1).
less_or_equal(frdef,ASub0,ASub1):-
	fd_less_or_equal(ASub0,ASub1).
less_or_equal(aeq,ASub0,ASub1):-
	aeq_less_or_equal(ASub0,ASub1).
less_or_equal(lsign,ASub0,ASub1):-
	lsign_less_or_equal(ASub0,ASub1).
less_or_equal(lsigndef,ASub0,ASub1):-
	lsigndef_less_or_equal(ASub0,ASub1).
less_or_equal(lsignshfr,ASub0,ASub1):-
	lsignshfr_less_or_equal(ASub0,ASub1).
less_or_equal(difflsign,ASub0,ASub1):-
	simple_lsign_less_or_equal(ASub0,ASub1).
less_or_equal(sha,ASub0,ASub1):-
	sha_less_or_equal(ASub0,ASub1).
less_or_equal(nf,ASub0,ASub1):-
	nf_less_or_equal(ASub0,ASub1).
less_or_equal(det,ASub0,ASub1):-
	det_less_or_equal(ASub0,ASub1).

%------------------------------------------------------------------------%
% glb(+,+,+,-)                                                           %
%------------------------------------------------------------------------%
:- doc(glb(AbsInt,ASub0,ASub1,GlbASub),"@var{GlbASub} is the greatest
	lower bound of abstract substitutions @var{ASub0} and @var{ASub1}.").

glb(_AbsInt,'$bottom',_ASub1,'$bottom'):- !.
glb(_AbsInt,_ASub0,'$bottom','$bottom'):- !.
glb(pd,_,_,top).
glb(pdb,_,_,top).
glb(gr,ASub0,ASub1,ASub):-
	gr_glb(ASub0,ASub1,ASub).
glb(java_nullity,ASub0,ASub1,ASub):-
	java_nullity_glb(ASub0,ASub1,ASub).
glb(share,ASub0,ASub1,ASub):-
	share_glb(ASub0,ASub1,ASub).
glb(share_amgu,ASub0,ASub1,ASub):-
	share_glb(ASub0,ASub1,ASub).
glb(share_clique,ASub0,ASub1,ASub):-
	share_clique_glb(ASub0,ASub1,ASub).
glb(share_clique_1,ASub0,ASub1,ASub):-
	share_clique_1_glb(ASub0,ASub1,ASub).
glb(share_clique_def,ASub0,ASub1,ASub):-
	share_clique_def_glb(ASub0,ASub1,ASub).
glb(sharefree_clique_def,ASub0,ASub1,ASub):-
	sharefree_clique_def_glb(ASub0,ASub1,ASub).
glb(sharefree_amgu,ASub0,ASub1,ASub):-
	shfr_glb(ASub0,ASub1,ASub).
glb(shfrlin_amgu,ASub0,ASub1,ASub):-
	shfrlin_glb(ASub0,ASub1,ASub).
glb(sharefree_clique,ASub0,ASub1,ASub):-
	sharefree_clique_glb(ASub0,ASub1,ASub).
glb(shfr,ASub0,ASub1,ASub):-
	shfr_glb(ASub0,ASub1,ASub).
glb(shfret,ASub0,ASub1,ASub):-
	shfret_glb(ASub0,ASub1,ASub).
glb(shfrnv,_ASub0,_ASub1,_ASub):-
	compiler_error(op_not_implemented(glb)),
	fail.
%	shfrnv_glb(ASub0,ASub1,ASub).
glb(son,ASub0,ASub1,ASub):-
	son_glb(ASub0,ASub1,ASub).
glb(oo_son,ASub0,ASub1,ASub):-
	oo_son_glb(ASub0,ASub1,ASub).
glb(oo_shnltau,ASub0,ASub1,ASub):-
	oo_shnltau_glb(ASub0,ASub1,ASub).
glb(shareson,_ASub0,_ASub1,_ASub):-
	compiler_error(op_not_implemented(glb)),
	fail.
%	shareson_glb(ASub0,ASub1,ASub).
glb(shfrson,_ASub0,_ASub1,_ASub):-
	compiler_error(op_not_implemented(glb)),
	fail.
%	shfrson_glb(ASub0,ASub1,ASub).
glb(path,_ASub0,_ASub1,_ASub):-
	compiler_error(op_not_implemented(glb)),
	fail.
%	path_glb(ASub0,ASub1,ASub).
glb(depth,ASub0,ASub1,ASub):-
	depthk_glb(ASub0,ASub1,ASub).
glb(eterms,ASub0,ASub1,ASub):-
	eterms_glb(ASub0,ASub1,ASub).
glb(svterms,ASub0,ASub1,ASub):-
	svterms_glb(ASub0,ASub1,ASub).
glb(terms,ASub0,ASub1,ASub):-
	terms_glb(ASub0,ASub1,ASub).
glb(ptypes,ASub0,ASub1,ASub):-
	terms_glb(ASub0,ASub1,ASub).
glb(deftypes,ASub0,ASub1,ASub):-
	deftypes_glb(ASub0,ASub1,ASub).
glb(oo_types,ASub0,ASub1,ASub):-
	oo_types_glb(ASub0,ASub1,ASub).
glb(java_cha,ASub0,ASub1,ASub):-
	java_cha_glb(ASub0,ASub1,ASub).
glb(polyhedra,ASub0,ASub1,ASub):-
	polyhedra_glb(ASub0,ASub1,ASub).
glb(fr,_ASub0,_ASub1,_ASub):-
	compiler_error(op_not_implemented(glb)),
	fail.
%	fr_glb(ASub0,ASub1,ASub).
glb(def,ASub0,ASub1,ASub):-
	def_glb(ASub0,ASub1,ASub).
glb(frdef,_ASub0,_ASub1,_ASub):-
	compiler_error(op_not_implemented(glb)),
	fail.
%	fd_glb(ASub0,ASub1,ASub).
glb(aeq,_ASub0,_ASub1,_ASub):-
	compiler_error(op_not_implemented(glb)),
	fail.
%	aeq_glb(ASub0,ASub1,ASub).
glb(lsign,_ASub0,_ASub1,_ASub):-
	compiler_error(op_not_implemented(glb)),
	fail.
%	lsign_glb(ASub0,ASub1,ASub).
glb(lsigndef,_ASub0,_ASub1,_ASub):-
	compiler_error(op_not_implemented(glb)),
	fail.
%	lsigndef_glb(ASub0,ASub1,ASub).
glb(lsignshfr,_ASub0,_ASub1,_ASub):-
	compiler_error(op_not_implemented(glb)),
	fail.
%	lsignshfr_glb(ASub0,ASub1,ASub).
glb(difflsign,_ASub0,_ASub1,_ASub):-
	compiler_error(op_not_implemented(glb)),
	fail.
%	simple_lsign_glb(ASub0,ASub1,ASub).
glb(sha,_ASub0,_ASub1,_ASub):-
	compiler_error(op_not_implemented(glb)),
	fail.
%	sha_glb(ASub0,ASub1,ASub).
glb(typeshfr,ASub0,ASub1,ASub):-
	glb(shfr,ASub0,ASub1,ASub).
glb(nf,ASub0,ASub1,ASub):-
	nf_glb(ASub0,ASub1,ASub).
glb(det,ASub0,ASub1,ASub):-
	det_glb(ASub0,ASub1,ASub).

%------------------------------------------------------------------------%
% eliminate_equivalent(+,+,-)                                            %
%------------------------------------------------------------------------%
:- doc(eliminate_equivalent(AbsInt,TmpLSucc,LSucc),
   "The list @var{LSucc} is reduced wrt the list @var{TmpLSucc} in that it
    does not contain abstract substitutions which are equivalent.").

eliminate_equivalent(pd,TmpLSucc,LSucc):-
	sort(TmpLSucc,LSucc).
eliminate_equivalent(pdb,TmpLSucc,LSucc):-
	sort(TmpLSucc,LSucc).
eliminate_equivalent(gr,TmpLSucc,LSucc):-
	sort(TmpLSucc,LSucc).
eliminate_equivalent(java_nullity,TmpLSucc,LSucc):-
	sort(TmpLSucc,LSucc).
eliminate_equivalent(share,TmpLSucc,LSucc):-
	sort(TmpLSucc,LSucc).
eliminate_equivalent(share_amgu,TmpLSucc,LSucc):-
	sort(TmpLSucc,LSucc).
eliminate_equivalent(share_clique,TmpLSucc,LSucc):-
	share_clique_eliminate_equivalent(TmpLSucc,LSucc).
eliminate_equivalent(share_clique_1,TmpLSucc,LSucc):-
	share_clique_1_eliminate_equivalent(TmpLSucc,LSucc).
eliminate_equivalent(share_clique_def,TmpLSucc,LSucc):-
	share_clique_def_eliminate_equivalent(TmpLSucc,LSucc).
eliminate_equivalent(sharefree_clique_def,TmpLSucc,LSucc):-
	sharefree_clique_def_eliminate_equivalent(TmpLSucc,LSucc).
eliminate_equivalent(sharefree_amgu,TmpLSucc,LSucc):-
	sort(TmpLSucc,LSucc).
eliminate_equivalent(shfrlin_amgu,TmpLSucc,LSucc):-
	sort(TmpLSucc,LSucc).
eliminate_equivalent(sharefree_clique,TmpLSucc,LSucc):-
	sharefree_clique_eliminate_equivalent(TmpLSucc,LSucc).
eliminate_equivalent(shfr,TmpLSucc,LSucc):-
	sort(TmpLSucc,LSucc).
eliminate_equivalent(shfret,LSucc,LSucc).
eliminate_equivalent(shfrnv,TmpLSucc,LSucc):-
	sort(TmpLSucc,LSucc).
eliminate_equivalent(son,TmpLSucc,LSucc):-
	sort(TmpLSucc,LSucc).
eliminate_equivalent(oo_son,TmpLSucc,LSucc):-
	sort(TmpLSucc,LSucc).
eliminate_equivalent(oo_shnltau,TmpLSucc,LSucc):-
	sort(TmpLSucc,LSucc).
eliminate_equivalent(shareson,TmpLSucc,LSucc):-
	sort(TmpLSucc,LSucc).
eliminate_equivalent(shfrson,TmpLSucc,LSucc):-
	sort(TmpLSucc,LSucc).
eliminate_equivalent(path,TmpLSucc,LSucc):-
	sort(TmpLSucc,LSucc).
eliminate_equivalent(lsign,TmpLSucc_u,LSucc):-
	sort(TmpLSucc_u,TmpLSucc),
	lsign_eliminate_equivalent(TmpLSucc,LSucc).
eliminate_equivalent(difflsign,TmpLSucc,LSucc):-
	sort(TmpLSucc,LSucc).
eliminate_equivalent(AbsInt,TmpLSucc,LSucc):-
	( AbsInt==depth ; AbsInt==sha ; AbsInt==aeq ),
	absub_eliminate_equivalent(TmpLSucc,AbsInt,LSucc).
eliminate_equivalent(eterms,TmpLSucc,LSucc):-
	sort(TmpLSucc,LSucc).
eliminate_equivalent(svterms,TmpLSucc,LSucc):-
	sort(TmpLSucc,LSucc).
eliminate_equivalent(terms,TmpLSucc,LSucc):-
	sort(TmpLSucc,LSucc).
eliminate_equivalent(ptypes,TmpLSucc,LSucc):-
	sort(TmpLSucc,LSucc).
eliminate_equivalent(deftypes,TmpLSucc,LSucc):-
	sort(TmpLSucc,LSucc).
eliminate_equivalent(oo_types,TmpLSucc,LSucc):-
	sort(TmpLSucc,LSucc).
eliminate_equivalent(java_cha,TmpLSucc,LSucc):-
	sort(TmpLSucc,LSucc).
eliminate_equivalent(polyhedra,TmpLSucc,LSucc):-
	polyhedra_sort(TmpLSucc,LSucc).
eliminate_equivalent(def,TmpLSucc,LSucc):-
	sort(TmpLSucc,LSucc).
eliminate_equivalent(nf,LSucc,LSucc).
eliminate_equivalent(det,LSucc,LSucc).

absub_eliminate_equivalent([],_AbsInt,[]).
absub_eliminate_equivalent([ASub],_AbsInt,[ASub]).
absub_eliminate_equivalent([ASub|LASub],AbsInt,[ASub|NLASub]):-
	take_equivalent_out(LASub,ASub,AbsInt,TmpLASub),
	absub_eliminate_equivalent(TmpLASub,AbsInt,NLASub).

take_equivalent_out([],_ASub,_AbsInt,[]).
take_equivalent_out([ASub0|LASub],ASub,AbsInt,NLASub):-
	equivalent_or_not(ASub0,ASub,AbsInt,NLASub,Tail),
	take_equivalent_out(LASub,ASub,AbsInt,Tail).

equivalent_or_not(ASub0,ASub,AbsInt,NLASub,Tail):-
	identical_abstract(AbsInt,ASub0,ASub), !,
	NLASub=Tail.
equivalent_or_not(ASub0,_ASub,_AbsInt,[ASub0|Tail],Tail).

%------------------------------------------------------------------------%
% abs_subset(+,+,+)                                                      %
%------------------------------------------------------------------------%
:- doc(abs_subset(AbsInt,LASub1,LASub2),
   "Succeeds if each abstract substitution in list @var{LASub1} is equivalent
    to some abstract substitution in list @var{LASub2}.").

abs_subset(AbsInt,LASub1,LASub2):-
	( AbsInt==depth ; AbsInt==sha ), !,
	absub_is_subset(LASub1,AbsInt,LASub2).
abs_subset(lsign,LASub1,LASub2):- !,
	lsign_is_subset(LASub1,LASub2).
abs_subset(_AbsInt,LASub1,LASub2):-
	ord_subset(LASub1,LASub2).

absub_is_subset([],_AbsInt,_LASub2).
absub_is_subset([Sub1|Subs1],AbsInt,LASub2):-
	member(ASub2,LASub2),
	identical_abstract(AbsInt,Sub1,ASub2),
% OR:
%	fixpoint_covered(AbsInt,Sub1,ASub2),
	absub_is_subset(Subs1,AbsInt,LASub2).

%------------------------------------------------------------------------%
% call_to_success_fact(+,+,+,+,+,+,+,-,-)                                %
%------------------------------------------------------------------------%
:- doc(call_to_success_fact(AbsInt,Sg,Hv,Head,Sv,Call,Proj,Prime,Succ),
   "Specialized version of call_to_entry + entry_to_exit + exit_to_prime
    for a fact @var{Head}.").

call_to_success_fact(pd,_Sg,_Hv,_Head,_Sv,Call,_Proj,Call,Call).
call_to_success_fact(pdb,_Sg,_Hv,_Head,_Sv,Call,_Proj,Call,Call).
call_to_success_fact(gr,Sg,Hv,Head,Sv,Call,Proj,Prime,Succ):-
	gr_call_to_success_fact(Sg,Hv,Head,Sv,Call,Proj,Prime,Succ).
call_to_success_fact(java_nullity,Sg,Hv,Head,Sv,Call,Proj,Prime,Succ):-
	java_nullity_call_to_success_fact(Sg,Hv,Head,Sv,Call,Proj,Prime,Succ).
call_to_success_fact(share,Sg,Hv,Head,Sv,Call,Proj,Prime,Succ):-
	share_call_to_success_fact(Sg,Hv,Head,Sv,Call,Proj,Prime,Succ).
call_to_success_fact(share_amgu,Sg,Hv,Head,Sv,Call,Proj,Prime,Succ):-
	share_amgu_call_to_success_fact(Sg,Hv,Head,Sv,Call,Proj,Prime,Succ).
call_to_success_fact(bshare,Sg,Hv,Head,Sv,Call,Proj,Prime,Succ):-
	bshare_call_to_success_fact(Sg,Hv,Head,Sv,Call,Proj,Prime,Succ).
call_to_success_fact(share_clique,Sg,Hv,Head,Sv,Call,Proj,Prime,Succ):-
	share_clique_call_to_success_fact(Sg,Hv,Head,Sv,Call,Proj,Prime,Succ).
call_to_success_fact(share_clique_1,Sg,Hv,Head,Sv,Call,Proj,Prime,Succ):-
	share_clique_1_call_to_success_fact(Sg,Hv,Head,Sv,Call,Proj,Prime,Succ).
call_to_success_fact(share_clique_def,Sg,Hv,Head,Sv,Call,Proj,Prime,Succ):-
	share_clique_def_call_to_success_fact(Sg,Hv,Head,Sv,Call,Proj,Prime,Succ).
call_to_success_fact(sharefree_clique_def,Sg,Hv,Head,Sv,Call,Proj,Prime,Succ):-
	sharefree_clique_def_call_to_success_fact(Sg,Hv,Head,Sv,Call,Proj,Prime,Succ).
call_to_success_fact(sharefree_amgu,Sg,Hv,Head,Sv,Call,Proj,Prime,Succ):-
	sharefree_amgu_call_to_success_fact(Sg,Hv,Head,Sv,Call,Proj,Prime,Succ).
call_to_success_fact(shfrlin_amgu,Sg,Hv,Head,Sv,Call,Proj,Prime,Succ):-
	shfrlin_call_to_success_fact(Sg,Hv,Head,Sv,Call,Proj,Prime,Succ).
call_to_success_fact(sharefree_clique,Sg,Hv,Head,Sv,Call,Proj,Prime,Succ):-
	sharefree_clique_call_to_success_fact(Sg,Hv,Head,Sv,Call,Proj,Prime,Succ).
call_to_success_fact(shfr,Sg,Hv,Head,Sv,Call,Proj,Prime,Succ):-
	shfr_call_to_success_fact(Sg,Hv,Head,Sv,Call,Proj,Prime,Succ).
call_to_success_fact(shfret,Sg,Hv,Head,Sv,Call,Proj,Prime,Succ):-
	shfret_call_to_success_fact(Sg,Hv,Head,Sv,Call,Proj,Prime,Succ).
call_to_success_fact(shfrnv,Sg,Hv,Head,Sv,Call,Proj,Prime,Succ):-
	shfrnv_call_to_success_fact(Sg,Hv,Head,Sv,Call,Proj,Prime,Succ).
call_to_success_fact(son,Sg,Hv,Head,Sv,Call,Proj,Prime,Succ):-
	son_call_to_success_fact(Sg,Hv,Head,Sv,Call,Proj,Prime,Succ).
call_to_success_fact(oo_son,Sg,Hv,Head,Sv,Call,Proj,Prime,Succ):-
	oo_son_call_to_success_fact(Sg,Hv,Head,Sv,Call,Proj,Prime,Succ).
call_to_success_fact(oo_shnltau,Sg,Hv,Head,Sv,Call,Proj,Prime,Succ):-
	oo_shnltau_call_to_success_fact(Sg,Hv,Head,Sv,Call,Proj,Prime,Succ).
call_to_success_fact(shareson,Sg,Hv,Head,Sv,Call,Proj,Prime,Succ):-
	shareson_call_to_success_fact(Sg,Hv,Head,Sv,Call,Proj,Prime,Succ).
call_to_success_fact(shfrson,Sg,Hv,Head,Sv,Call,Proj,Prime,Succ):-
	shfrson_call_to_success_fact(Sg,Hv,Head,Sv,Call,Proj,Prime,Succ).
call_to_success_fact(path,Sg,Hv,Head,Sv,Call,Proj,Prime,Succ):-
	path_call_to_success_fact(Sg,Hv,Head,Sv,Call,Proj,Prime,Succ).
call_to_success_fact(depth,Sg,Hv,Head,Sv,Call,Proj,Prime,Succ):-
	depthk_call_to_success_fact(Sg,Hv,Head,Sv,Call,Proj,Prime,Succ).
call_to_success_fact(eterms,Sg,Hv,Head,Sv,Call,Proj,Prime,Succ):-
	eterms_call_to_success_fact(Sg,Hv,Head,Sv,Call,Proj,Prime,Succ).
call_to_success_fact(svterms,Sg,Hv,Head,Sv,Call,Proj,Prime,Succ):-
	svterms_call_to_success_fact(Sg,Hv,Head,Sv,Call,Proj,Prime,Succ).
call_to_success_fact(terms,Sg,Hv,Head,Sv,Call,Proj,Prime,Succ):-
	terms_call_to_success_fact(Sg,Hv,Head,Sv,Call,Proj,Prime,Succ).
call_to_success_fact(ptypes,Sg,Hv,Head,Sv,Call,Proj,Prime,Succ):-
	terms_call_to_success_fact(Sg,Hv,Head,Sv,Call,Proj,Prime,Succ).
call_to_success_fact(deftypes,Sg,Hv,Head,Sv,Call,Proj,Prime,Succ):-
	deftypes_call_to_success_fact(Sg,Hv,Head,Sv,Call,Proj,Prime,Succ).
call_to_success_fact(oo_types,Sg,Hv,Head,Sv,Call,Proj,Prime,Succ):-
	oo_types_call_to_success_fact(Sg,Hv,Head,Sv,Call,Proj,Prime,Succ).
call_to_success_fact(java_cha,Sg,Hv,Head,Sv,Call,Proj,Prime,Succ):-
	java_cha_call_to_success_fact(Sg,Hv,Head,Sv,Call,Proj,Prime,Succ).
call_to_success_fact(polyhedra,Sg,Hv,Head,Sv,Call,Proj,Prime,Succ):-
	polyhedra_call_to_success_fact(Sg,Hv,Head,Sv,Call,Proj,Prime,Succ).
call_to_success_fact(fr,Sg,Hv,Head,Sv,Call,Proj,Prime,Succ):-
	fr_call_to_success_fact(Proj,Hv,Head,Sv,Sg,Call,Prime,Succ).
call_to_success_fact(def,Sg,Hv,Head,Sv,Call,Proj,Prime,Succ):-
	def_call_to_success_fact(Proj,Hv,Head,Sv,Sg,Call,Prime,Succ).
call_to_success_fact(frdef,Sg,Hv,Head,Sv,Call,Proj,Prime,Succ):-
	fd_call_to_success_fact(Proj,Sg,Hv,Head,Sv,Call,Prime,Succ).
call_to_success_fact(aeq,Sg,Hv,Head,Sv,Call,Proj,Prime,Succ):-
	aeq_call_to_success_fact(Proj,Sg,Hv,Head,Sv,Call,Prime,Succ).
call_to_success_fact(lsign,Sg,_Hv,Head,_Sv,Call,Proj,Prime,Succ):-
	lsign_call_to_success_fact(Sg,Head,Call,Proj,Prime,Succ).
call_to_success_fact(lsigndef,Sg,_Hv,Head,_Sv,Call,Proj,Prime,Succ):-
	lsigndef_call_to_success_fact(Sg,Head,Call,Proj,Prime,Succ).
call_to_success_fact(lsignshfr,Sg,_Hv,Head,_Sv,Call,Proj,Prime,Succ):-
	lsignshfr_call_to_success_fact(Sg,Head,Call,Proj,Prime,Succ).
call_to_success_fact(difflsign,Sg,_Hv,Head,_Sv,Call,Proj,Prime,Succ):-
	lsign_call_to_success_fact(Sg,Head,Call,Proj,Prime,Succ).
call_to_success_fact(sha,Sg,Hv,Head,Sv,_Call,Proj,Prime,Succ):-
	sha_call_to_success_fact(Hv,Head,Sv,Sg,Proj,Prime,Succ).
call_to_success_fact(nf,Sg,Hv,Head,Sv,Call,Proj,Prime,Succ):-
	nf_call_to_success_fact(Sg,Hv,Head,Sv,Call,Proj,Prime,Succ).
call_to_success_fact(det,Sg,Hv,Head,Sv,Call,Proj,Prime,Succ):-
	det_call_to_success_fact(Sg,Hv,Head,Sv,Call,Proj,Prime,Succ).

%------------------------------------------------------------------------%
% special_builtin(+,+,+,-,-)                                             %
%------------------------------------------------------------------------%
:- doc(special_builtin(AbsInt,SgKey,Sg,Subgoal,Type,Condvars),
  "Predicate @var{Sg} is considered a ""builtin"" of type @var{Type} in domain
   @var{AbsInt}. Types are domain dependent. Domains may have two different
   ways to treat these predicates: see @tt{body_succ_builtin/9}.").

special_builtin(pd,SgKey,Sg,_,Type,Condvars):-
	pd_special_builtin(SgKey,Sg,Type,Condvars).
special_builtin(pdb,SgKey,Sg,_,Type,Condvars):-
	pdb_special_builtin(SgKey,Sg,Type,Condvars).
special_builtin(gr,SgKey,Sg,_,Type,Condvars):-
	gr_special_builtin(SgKey,Sg,Type,Condvars).
special_builtin(java_nullity,SgKey,Sg,_,Type,Condvars):-
	java_nullity_special_builtin(SgKey,Sg,Type,Condvars).
special_builtin(share,SgKey,Sg,_,Type,Condvars):-
	share_special_builtin(SgKey,Sg,Type,Condvars).
special_builtin(bshare,SgKey,Sg,_,Type,Condvars):-
	bshare_special_builtin(SgKey,Sg,Type,Condvars).
special_builtin(share_amgu,SgKey,Sg,_,Type,Condvars):-
	share_amgu_special_builtin(SgKey,Sg,Type,Condvars).
special_builtin(share_clique,SgKey,Sg,_,Type,Condvars):-
	share_clique_special_builtin(SgKey,Sg,Type,Condvars).
special_builtin(share_clique_1,SgKey,Sg,_,Type,Condvars):-
	share_clique_special_builtin(SgKey,Sg,Type,Condvars).
special_builtin(share_clique_def,SgKey,Sg,_,Type,Condvars):-
	share_clique_def_special_builtin(SgKey,Sg,Type,Condvars).
special_builtin(sharefree_clique_def,SgKey,Sg,_,Type,Condvars):-
	sharefree_clique_def_special_builtin(SgKey,Sg,Type,Condvars).
special_builtin(sharefree_amgu,SgKey,Sg,_,Type,Condvars):-
	sharefree_amgu_special_builtin(SgKey,Sg,Type,Condvars).
special_builtin(shfrlin_amgu,SgKey,Sg,_,Type,Condvars):-
	shfrlin_special_builtin(SgKey,Sg,Type,Condvars).
special_builtin(sharefree_clique,SgKey,Sg,_,Type,Condvars):-
	sharefree_clique_special_builtin(SgKey,Sg,Type,Condvars).
special_builtin(shareson,SgKey,Sg,_,(TypeSon,TypeSh),(CondSon,CondSh)):-
	share_special_builtin(SgKey,Sg,TypeSh,CondSh),
	son_special_builtin(SgKey,Sg,TypeSon,CondSon).
special_builtin(shfr,SgKey,Sg,_,Type,Condvars):-
	shfr_special_builtin(SgKey,Sg,Type,Condvars).
special_builtin(shfrson,SgKey,Sg,_,(TypeSon,TypeSh),(CondSon,CondSh)):-
	shfr_special_builtin(SgKey,Sg,TypeSh,CondSh),
	son_special_builtin(SgKey,Sg,TypeSon,CondSon).
special_builtin(shfrnv,SgKey,Sg,_,Type,Condvars):-
	shfr_special_builtin(SgKey,Sg,Type,Condvars).
special_builtin(son,SgKey,Sg,_,Type,Condvars):-
	son_special_builtin(SgKey,Sg,Type,Condvars).
special_builtin(oo_son,SgKey,Sg,_,Type,Condvars):-
	oo_son_special_builtin(SgKey,Sg,Type,Condvars).
special_builtin(oo_shnltau,SgKey,Sg,_,Type,Condvars):-
	oo_shnltau_special_builtin(SgKey,Sg,Type,Condvars).
special_builtin(def,SgKey,Sg,_,Type,Condvars):-
	def_special_builtin(SgKey,Sg,Type,Condvars).
special_builtin(fr,SgKey,Sg,_,Type,Condvars):-
	fr_special_builtin(SgKey,Sg,Type,Condvars).
special_builtin(frdef,SgKey,Sg,_,(TypeF,TypeD),(CondF,CondD)):-
	def_special_builtin(SgKey,Sg,TypeD,CondD),
	fr_special_builtin(SgKey,Sg,TypeF,CondF).
special_builtin(path,SgKey,Sg,_,Type,Condvars):-
	path_special_builtin(SgKey,Sg,Type,Condvars).
special_builtin(depth,SgKey,Sg,_,Type,Condvars):-
	depthk_special_builtin(SgKey,Sg,Type,Condvars).
special_builtin(aeq,Sg_key,Sg,_,Type,Info_sg):-
	aeq_special_builtin(Sg_key,Sg,Type,Info_sg).
special_builtin(lsign,SgKey,Sg,_,Type,Condvars):-
	lsign_special_builtin(SgKey,Sg,Type,Condvars).
special_builtin(difflsign,SgKey,Sg,_,Type,Condvars):-
	lsign_special_builtin(SgKey,Sg,Type,Condvars).
special_builtin(sha,SgKey,Sg,_,Type,Condvars):-
	sha_special_builtin(SgKey,Sg,Type,Condvars).
special_builtin(eterms,SgKey,Sg,Subgoal,Type,Condvars):-
	eterms_special_builtin(SgKey,Sg,Subgoal,Type,Condvars).
special_builtin(svterms,SgKey,Sg,Subgoal,Type,Condvars):-
	svterms_special_builtin(SgKey,Sg,Subgoal,Type,Condvars).
special_builtin(terms,SgKey,Sg,_,Type,Condvars):-
	terms_special_builtin(SgKey,Sg,Type,Condvars).
special_builtin(ptypes,SgKey,Sg,_,Type,Condvars):-
	terms_special_builtin(SgKey,Sg,Type,Condvars).
special_builtin(deftypes,SgKey,Sg,_,Type,Condvars):-
	terms_special_builtin(SgKey,Sg,Type,Condvars).
special_builtin(oo_types,SgKey,Sg,_,Type,Condvars):-
	oo_types_special_builtin(SgKey,Sg,Type,Condvars).
special_builtin(java_cha,SgKey,Sg,_,Type,Condvars):-
	java_cha_special_builtin(SgKey,Sg,Type,Condvars).
special_builtin(polyhedra,SgKey,Sg,_Subgoal,Type,Condvars):-
	polyhedra_special_builtin(SgKey,Sg,Type,Condvars).
special_builtin(nf,SgKey,Sg,_,Type,Condvars):-
	nf_special_builtin(SgKey,Sg,Type,Condvars).
special_builtin(det,SgKey,Sg,_,Type,Condvars):-
	det_special_builtin(SgKey,Sg,Type,Condvars).

:- doc(hide,combined_special_builtin/3).

combined_special_builtin(nf,SgKey,Domains):-
	special_builtin(eterms,SgKey,_Sg,SgKey,_Type,_Condvars), !,
	Domains=[eterms,shfr,nf].
combined_special_builtin(nf,SgKey,Domains):-
	special_builtin(shfr,SgKey,_Sg,SgKey,_Type,_Condvars), !,
	Domains=[eterms,shfr,nf].
combined_special_builtin(nf,SgKey,Domains):-
	special_builtin(nf,SgKey,_Sg,SgKey,_Type,_Condvars), !,
	Domains=[eterms,shfr,nf].
combined_special_builtin(shfret,SgKey,Domains):-
	special_builtin(eterms,SgKey,_Sg,SgKey,_Type,_Condvars), !,
	Domains=[eterms,shfr].
combined_special_builtin(shfret,SgKey,Domains):-
	special_builtin(shfr,SgKey,_Sg,SgKey,_Type,_Condvars), !,
	Domains=[eterms,shfr].
% determinism
combined_special_builtin(det,SgKey,Domains):-
	special_builtin(eterms,SgKey,_Sg,SgKey,_Type,_Condvars), !,
	Domains=[eterms,shfr,det].
combined_special_builtin(det,SgKey,Domains):-
	special_builtin(shfr,SgKey,_Sg,SgKey,_Type,_Condvars), !,
	Domains=[eterms,shfr,det].
combined_special_builtin(det,SgKey,Domains):-
	special_builtin(det,SgKey,_Sg,SgKey,_Type,_Condvars), !,
	Domains=[eterms,shfr,det].
 %% combined_special_builtin(det,SgKey,Domains):-!,
 %%        combined_special_builtin(nf,SgKey,Domains).

:- doc(hide,split_combined_domain/4).

split_combined_domain(nf,ASub,ASubs,Doms):-
	nf_split_combined_domain(ASub,ASubs,Doms).
split_combined_domain(det,ASub,ASubs,Doms):-
	det_split_combined_domain(ASub,ASubs,Doms).
split_combined_domain(shfret,ASub,ASubs,Doms):-
	shfret_split_combined_domain(ASub,ASubs,Doms).

%------------------------------------------------------------------------%
% body_succ_builtin(+,+,+,+,+,+,+,+,-)                                   %
%------------------------------------------------------------------------%
:- doc(body_succ_builtin(Type,AbsInt,Sg,Vs,Sv,Hv,Call,Proj,Succ),
   "Specialized version of call_to_entry + entry_to_exit + exit_to_prime +
    extend for predicate @var{Sg} considered a ""builtin"" of type @var{Type}
    in domain @var{AbsInt}. Whether a predicate is ""builtin"" in a domain is
    determined by @tt{special_builtin/5}.  There are two different ways to
    treat these predicates, depending on @var{Type}: @tt{success_builtin}
    handles more usual types of ""builtins"", @tt{call_to_success_builtin}
    handles particular predicates. The later is called when @var{Type} is of
    the form @tt{special(SgKey)}.").

% careful with: sha lsignshfr lsigndef 
%
% These do not have special(_), so ok:
% 	AbsInt \== def, AbsInt \== fr, AbsInt \== fd,
% These do, special care: shareson shfrson
body_succ_builtin((TSon,TSh),shareson,Sg,(CSon,CSh),Sv,HvFv,Call,Proj,Succ):-
	!,
	Call=(Call_son,Call_sh),
	Proj=(Proj_son,Proj_sh),
	body_succ_builtin(TSon,son,Sg,CSon,Sv,HvFv,Call_son,Proj_son,Succ_son),
	body_succ_builtin(TSh,share,Sg,CSh,Sv,HvFv,Call_sh,Proj_sh,Succ_sh),
	shareson_compose(Call,Succ_sh,Succ_son,Succ).
body_succ_builtin((TSon,TSh),shfrson,Sg,(CSon,CSh),Sv,HvFv,Call,Proj,Succ):-
	!,
	Call=(Call_son,Call_sh),
	Proj=(Proj_son,Proj_sh),
	body_succ_builtin(TSon,son,Sg,CSon,Sv,HvFv,Call_son,Proj_son,Succ_son),
	body_succ_builtin(TSh,shfr,Sg,CSh,Sv,HvFv,Call_sh,Proj_sh,Succ_sh),
	shfrson_compose(Call,Succ_sh,Succ_son,Succ).
body_succ_builtin((TSH,not_defined),share_clique_def,Sg,(CSH,_),Sv,HvFv,Call,Proj,Succ):-
	!,
	Call=(Call_SH,Call_def),
	Proj=(Proj_SH,_Proj_def),
	body_succ_builtin(TSH,share_clique,Sg,CSH,Sv,HvFv,Call_SH,Proj_SH,Succ_SH),
	Succ = (Succ_SH,Call_def).
body_succ_builtin((TSH,Tdef),share_clique_def,Sg,(CSH,Cdef),Sv,HvFv,Call,Proj,Succ):-!,
	Call=(Call_SH,Call_def),
	Proj=(Proj_SH,Proj_def),
	body_succ_builtin(Tdef,def,Sg,Cdef,Sv,HvFv,Call_def,Proj_def,Succ_def),
	share_clique_def_compose((Call_SH,Succ_def),NewCall_SH),
	share_clique_def_compose((Proj_SH,Succ_def),NewProj_SH),
	body_succ_builtin(TSH,share_clique,Sg,CSH,Sv,HvFv,NewCall_SH,NewProj_SH,Succ_SH),
	Succ = (Succ_SH,Succ_def).
body_succ_builtin((TSHF,not_defined),sharefree_clique_def,Sg,(CSHF,_),Sv,HvFv,Call,Proj,Succ):-
	!,
	Call=(Call_SHF,Call_def),
	Proj=(Proj_SHF,_Proj_def),
	body_succ_builtin(TSHF,sharefree_clique,Sg,CSHF,Sv,HvFv,Call_SHF,Proj_SHF,Succ_SHF),
	Succ = (Succ_SHF,Call_def).
body_succ_builtin((TSHF,Tdef),sharefree_clique_def,Sg,(CSHF,Cdef),Sv,HvFv,Call,Proj,Succ):-
	Call=(Call_SHF,Call_def),
	Proj=(Proj_SHF,Proj_def),
	body_succ_builtin(Tdef,def,Sg,Cdef,Sv,HvFv,Call_def,Proj_def,Def_succ),
	sharefree_clique_def_compose(Call_SHF,Def_succ,NewCall_SHF),
	sharefree_clique_def_compose(Proj_SHF,Def_succ,NewProj_SHF),
	body_succ_builtin(TSHF,sharefree_clique,Sg,CSHF,Sv,HvFv,NewCall_SHF,NewProj_SHF,Succ_SHF),
	unify_asub_if_bottom((Succ_SHF,Def_succ),Succ),!.
body_succ_builtin(Type,AbsInt,Sg,Condvs,Sv,HvFv_u,Call,Proj,Succ):-
	body_builtin(Type,AbsInt,Sg,Condvs,Sv,HvFv_u,Call,Proj,Succ).

body_builtin(special(SgKey),AbsInt,Sg,_Condvs,Sv,_HvFv_u,Call,Proj,Succ):-
	!,
	call_to_success_builtin(AbsInt,SgKey,Sg,Sv,Call,Proj,Succ).
body_builtin(Type,AbsInt,_Sg,Condvs,Sv,HvFv_u,Call,_Proj,Succ):-
	success_builtin(AbsInt,Type,Sv,Condvs,HvFv_u,Call,Succ), !.
body_builtin(Type,AbsInt,_Sg,_Condvs,_Sv,_HvFv_u,_Call,_Proj,'$bottom'):-
       warning_message("the builtin key ~q is not defined in domain ~w",
	                [Type,AbsInt]).

%------------------------------------------------------------------------------
:- doc(doinclude,success_builtin/7).
:- doc(success_builtin(AbsInt,Type,Sv,Condvars,HvFv_u,Call,Succ),
  "@var{Succ} is the success substitution on domain @var{AbsInt} for a call
   @var{Call} to a goal of a ""builtin"" (domain dependent) type @var{Type}
   with variables @var{Sv}. @var{Condvars} can be used to transfer some
   information from @tt{special_builtin/5}.").

success_builtin(pd,Type,Sv_uns,Condvars,_,Call,Succ):-
	pd_success_builtin(Type,Sv_uns,Condvars,Call,Succ).
success_builtin(pdb,Type,Sv_uns,Condvars,_,Call,Succ):-
	pdb_success_builtin(Type,Sv_uns,Condvars,Call,Succ).
success_builtin(gr,Type,Sv_uns,Condvars,_,Call,Succ):-
	gr_success_builtin(Type,Sv_uns,Condvars,Call,Succ).
success_builtin(java_nullity,Type,Sv_uns,Condvars,_,Call,Succ):-
	java_nullity_success_builtin(Type,Sv_uns,Condvars,Call,Succ).
success_builtin(share,Type,Sv_uns,Condvars,_,Call,Succ):-
	share_success_builtin(Type,Sv_uns,Condvars,Call,Succ).
success_builtin(bshare,Type,Sv_uns,Condvars,_,Call,Succ):-
	bshare_success_builtin(Type,Sv_uns,Condvars,Call,Succ).
success_builtin(share_amgu,Type,Sv_uns,Condvars,_,Call,Succ):-
	share_amgu_success_builtin(Type,Sv_uns,Condvars,Call,Succ).
success_builtin(share_clique,Type,Sv_uns,Condvars,_,Call,Succ):-
	share_clique_success_builtin(Type,Sv_uns,Condvars,Call,Succ).
success_builtin(share_clique_1,Type,Sv_uns,Condvars,_,Call,Succ):-
	share_clique_1_success_builtin(Type,Sv_uns,Condvars,Call,Succ).
success_builtin(sharefree_amgu,Type,Sv_uns,Condvars,_,Call,Succ):-
	sharefree_amgu_success_builtin(Type,Sv_uns,Condvars,Call,Succ).
success_builtin(shfrlin_amgu,Type,Sv_uns,Condvars,_,Call,Succ):-
	shfrlin_success_builtin(Type,Sv_uns,Condvars,Call,Succ).
success_builtin(sharefree_clique,Type,Sv_uns,Condvars,_,Call,Succ):-
	sharefree_clique_success_builtin(Type,Sv_uns,Condvars,Call,Succ).
success_builtin(shfr,Type,Sv_uns,Condvars,_,Call,Succ):-
	shfr_success_builtin(Type,Sv_uns,Condvars,Call,Succ).
success_builtin(shfrnv,Type,Sv_uns,Condvars,_,Call,Succ):-
	shfrnv_success_builtin(Type,Sv_uns,Condvars,Call,Succ).
success_builtin(son,Type,Sv_uns,Condvars,_,Call,Succ):-
	son_success_builtin(Type,Sv_uns,Condvars,Call,Succ).
success_builtin(oo_son,Type,Sv_uns,Condvars,_,Call,Succ):-
	oo_son_success_builtin(Type,Sv_uns,Condvars,Call,Succ).
success_builtin(oo_shnltau,Type,Sv_uns,Condvars,_,Call,Succ):-
	oo_shnltau_success_builtin(Type,Sv_uns,Condvars,Call,Succ).
success_builtin(path,Type,Sv_uns,Condvars,_,Call,Succ):-
	path_success_builtin(Type,Sv_uns,Condvars,Call,Succ).
success_builtin(depth,Type,Sv_uns,Condvars,_,Call,Succ):-
	depthk_success_builtin(Type,Sv_uns,Condvars,Call,Succ).
success_builtin(fr,Type,Sv_uns,Condvars,_,Call,Succ):-
	fr_success_builtin(Type,Sv_uns,Condvars,Call,Succ).
success_builtin(def,Type,_Sv_uns,Condvars,_,Call,Succ):-
	def_success_builtin(Type,Condvars,Call,Succ).
success_builtin(frdef,Type,Sv_uns,Condvars,_,Call,Succ):-
	fd_success_builtin(Type,Sv_uns,Condvars,Call,Succ).
success_builtin(aeq,Type,Sv_uns,Info_sg,_,Call,Succ):-
	aeq_success_builtin(Type,Sv_uns,Info_sg,Call,Succ).
success_builtin(lsign,Type,Sv_uns,Condvars,HvFv_u,Call,Succ):-
	lsign_success_builtin(Type,Sv_uns,Condvars,HvFv_u,Call,Succ).
success_builtin(difflsign,Type,Sv_uns,Condvars,HvFv_u,Call,Succ):-
	simple_lsign_success_builtin(Type,Sv_uns,Condvars,HvFv_u,Call,Succ).
success_builtin(sha,Type,Sv_uns,Condvars,_,Call,Succ):-
	sha_success_builtin(Type,Sv_uns,Condvars,Call,Succ).
success_builtin(eterms,Type,Sv_uns,Condvars,_,Call,Succ):-
	eterms_success_builtin(Type,Sv_uns,Condvars,Call,Succ).
success_builtin(svterms,Type,Sv_uns,Condvars,_,Call,Succ):-
	svterms_success_builtin(Type,Sv_uns,Condvars,Call,Succ).
success_builtin(terms,Type,Sv_uns,Condvars,_,Call,Succ):-
	terms_success_builtin(Type,Sv_uns,Condvars,Call,Succ).
success_builtin(ptypes,Type,Sv_uns,Condvars,_,Call,Succ):-
	terms_success_builtin(Type,Sv_uns,Condvars,Call,Succ).
success_builtin(deftypes,Type,Sv_uns,Condvars,_,Call,Succ):-
	terms_success_builtin(Type,Sv_uns,Condvars,Call,Succ).
success_builtin(oo_types,Type,Sv_uns,Condvars,_,Call,Succ):-
	oo_types_success_builtin(Type,Sv_uns,Condvars,Call,Succ).
success_builtin(java_cha,Type,Sv_uns,Condvars,_,Call,Succ):-
	java_cha_success_builtin(Type,Sv_uns,Condvars,Call,Succ).
success_builtin(polyhedra,Type,Sv_uns,Condvars,_,Call,Succ):-
	polyhedra_success_builtin(Type,Sv_uns,Condvars,Call,Succ).
success_builtin(nf,Type,Sv_uns,Condvars,_,Call,Succ):-
	nf_success_builtin(Type,Sv_uns,Condvars,Call,Succ).
success_builtin(det,Type,Sv_uns,Condvars,_,Call,Succ):-
	det_success_builtin(Type,Sv_uns,Condvars,Call,Succ).

%------------------------------------------------------------------------------
:- doc(doinclude,call_to_success_builtin/7).
:- doc(call_to_success_builtin(AbsInt,Type,Sg,Sv,Call,Proj,Succ),
  "@var{Succ} is the success substitution on domain @var{AbsInt} for a call
   @var{Call} to a goal @var{Sg} with variables @var{Sv} considered of a 
   ""builtin"" (domain dependent) type @var{Type}. @var{Proj} is @var{Call}
   projected on @var{Sv}.").

call_to_success_builtin(pd,_SgKey,_Sg,_Sv,Call,_Proj,Call).
call_to_success_builtin(pdb,_SgKey,_Sg,_Sv,Call,_Proj,Call).
call_to_success_builtin(gr,SgKey,Sg,Sv,Call,Proj,Succ):-
	gr_call_to_success_builtin(SgKey,Sg,Sv,Call,Proj,Succ).
call_to_success_builtin(share,SgKey,Sg,Sv,Call,Proj,Succ):-
	share_call_to_success_builtin(SgKey,Sg,Sv,Call,Proj,Succ).
call_to_success_builtin(share_amgu,SgKey,Sg,Sv,Call,Proj,Succ):-
	share_amgu_call_to_success_builtin(SgKey,Sg,Sv,Call,Proj,Succ).
call_to_success_builtin(bshare,SgKey,Sg,Sv,Call,Proj,Succ):-
	bshare_call_to_success_builtin(SgKey,Sg,Sv,Call,Proj,Succ).
call_to_success_builtin(share_clique,SgKey,Sg,Sv,Call,Proj,Succ):-
	share_clique_call_to_success_builtin(SgKey,Sg,Sv,Call,Proj,Succ).
call_to_success_builtin(share_clique_1,SgKey,Sg,Sv,Call,Proj,Succ):-
	share_clique_1_call_to_success_builtin(SgKey,Sg,Sv,Call,Proj,Succ).
call_to_success_builtin(sharefree_amgu,SgKey,Sg,Sv,Call,Proj,Succ):-
	sharefree_amgu_call_to_success_builtin(SgKey,Sg,Sv,Call,Proj,Succ).
call_to_success_builtin(shfrlin_amgu,SgKey,Sg,Sv,Call,Proj,Succ):-
	shfrlin_call_to_success_builtin(SgKey,Sg,Sv,Call,Proj,Succ).
call_to_success_builtin(sharefree_clique,SgKey,Sg,Sv,Call,Proj,Succ):-
	sharefree_clique_call_to_success_builtin(SgKey,Sg,Sv,Call,Proj,Succ).
call_to_success_builtin(shfr,SgKey,Sg,Sv,Call,Proj,Succ):-
	shfr_call_to_success_builtin(SgKey,Sg,Sv,Call,Proj,Succ).
call_to_success_builtin(shfrnv,SgKey,Sg,Sv,Call,Proj,Succ):-
	shfrnv_call_to_success_builtin(SgKey,Sg,Sv,Call,Proj,Succ).
call_to_success_builtin(son,SgKey,Sg,Sv,Call,Proj,Succ):-
	son_call_to_success_builtin(SgKey,Sg,Sv,Call,Proj,Succ).
call_to_success_builtin(oo_son,SgKey,Sg,Sv,Call,Proj,Succ):-
	oo_son_call_to_success_builtin(SgKey,Sg,Sv,Call,Proj,Succ).
call_to_success_builtin(oo_shnltau,SgKey,Sg,Sv,Call,Proj,Succ):-
	oo_shnltau_call_to_success_builtin(SgKey,Sg,Sv,Call,Proj,Succ).
call_to_success_builtin(depth,_SgKey,Sg,Sv,Call,_Proj,Succ):-
	depthk_call_to_success_builtin(Sg,Sv,Call,Succ).
call_to_success_builtin(sha,SgKey,Sg,Sv,Call,Proj,Succ):-
	sha_call_to_success_builtin(SgKey,Sg,Sv,Call,Proj,Succ).
call_to_success_builtin(eterms,SgKey,Sg,Sv,Call,Proj,Succ):-
	eterms_call_to_success_builtin(SgKey,Sg,Sv,Call,Proj,Succ).
call_to_success_builtin(svterms,SgKey,Sg,Sv,Call,Proj,Succ):-
	svterms_call_to_success_builtin(SgKey,Sg,Sv,Call,Proj,Succ).
call_to_success_builtin(terms,SgKey,Sg,Sv,Call,Proj,Succ):-
	terms_call_to_success_builtin(SgKey,Sg,Sv,Call,Proj,Succ).
call_to_success_builtin(ptypes,SgKey,Sg,Sv,Call,Proj,Succ):-
	terms_call_to_success_builtin(SgKey,Sg,Sv,Call,Proj,Succ).
call_to_success_builtin(deftypes,SgKey,Sg,Sv,Call,Proj,Succ):-
	deftypes_call_to_success_builtin(SgKey,Sg,Sv,Call,Proj,Succ).
call_to_success_builtin(oo_types,SgKey,Sg,Sv,Call,Proj,Succ):-
	oo_types_call_to_success_builtin(SgKey,Sg,Sv,Call,Proj,Succ).
call_to_success_builtin(polyhedra,SgKey,Sg,Sv,Call,Proj,Succ):-
	polyhedra_call_to_success_builtin(SgKey,Sg,Sv,Call,Proj,Succ).
call_to_success_builtin(AbsInt,SgKey,_Sg,_Sv,_Call,_Proj,'$bottom'):-
        warning_message("the builtin key ~q is not defined in domain ~w",
	                [special(SgKey),AbsInt]).

%------------------------------------------------------------------------%
% obtain_info(+,+,+,+,-)                                                 %
%------------------------------------------------------------------------%
:- doc(obtain_info(AbsInt,Prop,Vars,ASub,Info),
   "Obtains variables @var{Info} for which property @var{Prop} holds given
    abstract substitution @var{ASub} on variables @var{Vars} for domain
    @var{AbsInt}.").

obtain_info(shfr,Prop,Vars,ASub,Info):- shfr_obtain(Prop,Vars,ASub,Info).
obtain_info(sharefree_clique,Prop,Vars,ASub,Info):- shfr_obtain(Prop,Vars,ASub,Info).
obtain_info(sharefree_amgu,Prop,Vars,ASub,Info):- shfr_obtain(Prop,Vars,ASub,Info).
obtain_info(shfrlin_amgu,Prop,Vars,(Sh,Fr,_Lin),Info):- shfr_obtain(Prop,Vars,(Sh,Fr),Info).
obtain_info(det,Prop,Vars,ASub,Info):- 
   % asub_to_info(det,ASub,Vars,_OutputUser,CompProps),
   % CompProps = Info.
   det_obtain(Prop,Vars,ASub,Info).
obtain_info(eterms,_Prop,Vars,ASub,Info):-
	asub_to_info(eterms,ASub,Vars,Info,_CompProps).
%------------------------------------------------------------------------%
% info_to_asub(+,+,+,+,-)                                                %
%------------------------------------------------------------------------%
:- doc(info_to_asub(AbsInt,Kind,InputUser,Qv,ASub),
   "Obtains the abstract substitution @var{ASub} on variables @var{Qv} for
    domain @var{AbsInt} from the user supplied information @var{InputUser}
    refering to properties on @var{Qv}. It works by calling
    @tt{input_interface/5} on each property of @var{InputUser} which is a
    native property, so that they are accumulated, and then calls
    @tt{input_user_interface/4}.").

info_to_asub(AbsInt,Kind,InputUser,Qv,ASub):-
	info_to_asub_(InputUser,AbsInt,Kind,_,Input),
	input_user_interface(AbsInt,Input,Qv,ASub).

info_to_asub_([],_AbsInt,_Kind,Acc,Acc).
info_to_asub_([I|Info],AbsInt,_Kind,Acc0,Acc):-
	( native_prop(I,P),
	  input_interface(AbsInt,P,_Kind1,Acc0,Acc1) -> true
	; Acc1=Acc0 ),
	info_to_asub_(Info,AbsInt,_Kind2,Acc1,Acc).

 %% Commented out by PLG 8 Jun 2003
 %% info_to_asub_([],_AbsInt,_Kind,Acc,Acc).
 %% info_to_asub_([I|Info],AbsInt,Kind,Acc0,Acc):-
 %% 	( native_prop(I,P),
 %% 	  input_interface(AbsInt,P,Kind,Acc0,Acc1) -> true
 %% 	; Acc1=Acc0 ),
 %% 	info_to_asub_(Info,AbsInt,Kind,Acc1,Acc).

:- doc(full_info_to_asub(AbsInt,InputUser,Qv,ASub),
   "Same as @tt{info_to_asub(AbsInt,InputUser,Qv,ASub)} except that it fails
    if some property in @var{InputUser} is not native or not relevant to the
    domain @var{AbsInt}.").

full_info_to_asub(AbsInt,InputUser,Qv,ASub):-
	full_info_to_asub_(InputUser,AbsInt,_,Input),
	input_user_interface(AbsInt,Input,Qv,ASub).

full_info_to_asub_([],_AbsInt,Acc,Acc).
full_info_to_asub_([I|Info],AbsInt,Acc0,Acc):-
	native_prop(I,P),
	input_interface(AbsInt,P,perfect,Acc0,Acc1), !, % P is enough (PBC)
	                                                % do not backtrack
	full_info_to_asub_(Info,AbsInt,Acc1,Acc).       % into native_prop

:- doc(doinclude,input_interface/5).
:- doc(input_interface(AbsInt,Prop,Kind,Struc0,Struc1),
   "@var{Prop} is a native property that is relevant to domain @var{AbsInt}
    (i.e., the domain knows how to fully --@var{+Kind}=perfect-- or 
    approximately --@var{-Kind}=approx-- abstract it) and @var{Struct1} is a
    (domain defined) structure resulting of adding the (domain dependent)
    information conveyed by @var{Prop} to structure @var{Struct0}. This way,
    the properties relevant to a domain are being accumulated.").

input_interface(gr,InputUser,Kind,Struct0,Struct1):-
	gr_input_interface(InputUser,Kind,Struct0,Struct1).
input_interface(java_nullity,InputUser,Kind,Struct0,Struct1):-
	java_nullity_input_interface(InputUser,Kind,Struct0,Struct1).
input_interface(share,InputUser,Kind,Struct0,Struct1):-
	share_input_interface(InputUser,Kind,Struct0,Struct1).
input_interface(share_amgu,InputUser,Kind,Struct0,Struct1):-
	share_input_interface(InputUser,Kind,Struct0,Struct1).
input_interface(share_clique,InputUser,Kind,Struct0,Struct1):-
	share_clique_input_interface(InputUser,Kind,Struct0,Struct1).
input_interface(share_clique_1,InputUser,Kind,Struct0,Struct1):-
	share_clique_1_input_interface(InputUser,Kind,Struct0,Struct1).
input_interface(share_clique_def,InputUser,Kind,Struct0,Struct1):-
	share_clique_input_interface(InputUser,Kind,Struct0,Struct1).
input_interface(sharefree_clique_def,InputUser,Kind,Struct0,Struct1):-
	sharefree_clique_input_interface(InputUser,Kind,Struct0,Struct1).
input_interface(sharefree_amgu,InputUser,Kind,Struct0,Struct1):-
	shfr_input_interface(InputUser,Kind,Struct0,Struct1).
input_interface(shfrlin_amgu,InputUser,Kind,Struct0,Struct1):-
	shfrlin_input_interface(InputUser,Kind,Struct0,Struct1).
input_interface(sharefree_clique,InputUser,Kind,Struct0,Struct1):-
	sharefree_clique_input_interface(InputUser,Kind,Struct0,Struct1).
input_interface(shfr,InputUser,Kind,Struct0,Struct1):-
	shfr_input_interface(InputUser,Kind,Struct0,Struct1).
input_interface(shfret,InputUser,Kind,Struct0,Struct1):-
	shfret_input_interface(InputUser,Kind,Struct0,Struct1).
input_interface(shfrnv,InputUser,Kind,Struct0,Struct1):-
	shfrnv_input_interface(InputUser,Kind,Struct0,Struct1).
input_interface(son,InputUser,Kind,Struct0,Struct1):-
	son_input_interface(InputUser,Kind,Struct0,Struct1).
input_interface(oo_son,InputUser,Kind,Struct0,Struct1):-
	oo_son_input_interface(InputUser,Kind,Struct0,Struct1).
input_interface(oo_shnltau,InputUser,Kind,Struct0,Struct1):-
	oo_shnltau_input_interface(InputUser,Kind,Struct0,Struct1).
input_interface(shareson,InputUser,Kind,Struct0,Struct1):-
	shareson_input_interface(InputUser,Kind,Struct0,Struct1).
input_interface(shfrson,InputUser,Kind,Struct0,Struct1):-
	shfrson_input_interface(InputUser,Kind,Struct0,Struct1).
input_interface(path,InputUser,Kind,Struct0,Struct1):-
	path_input_interface(InputUser,Kind,Struct0,Struct1).
input_interface(depth,InputUser,Kind,Struct0,Struct1):-
	depthk_input_interface(InputUser,Kind,Struct0,Struct1).
input_interface(eterms,InputUser,Kind,Struct0,Struct1):-
	eterms_input_interface(InputUser,Kind,Struct0,Struct1).
input_interface(svterms,InputUser,Kind,Struct0,Struct1):-
	svterms_input_interface(InputUser,Kind,Struct0,Struct1).
input_interface(terms,InputUser,Kind,Struct0,Struct1):-
	terms_input_interface(InputUser,Kind,Struct0,Struct1).
input_interface(ptypes,InputUser,Kind,Struct0,Struct1):-
	terms_input_interface(InputUser,Kind,Struct0,Struct1).
input_interface(deftypes,InputUser,Kind,Struct0,Struct1):-
	deftypes_input_interface(InputUser,Kind,Struct0,Struct1).
input_interface(oo_types,InputUser,Kind,Struct0,Struct1):-
	oo_types_input_interface(InputUser,Kind,Struct0,Struct1).
input_interface(java_cha,InputUser,Kind,Struct0,Struct1):-
	java_cha_input_interface(InputUser,Kind,Struct0,Struct1).
input_interface(polyhedra,InputUser,Kind,Struct0,Struct1):-
	polyhedra_input_interface(InputUser,Kind,Struct0,Struct1).
input_interface(fr,InputUser,Kind,Struct0,Struct1):-
	fr_input_interface(InputUser,Kind,Struct0,Struct1).
input_interface(def,InputUser,Kind,Struct0,Struct1):-
	def_input_interface(InputUser,Kind,Struct0,Struct1).
input_interface(frdef,InputUser,Kind,Struct0,Struct1):-
	fd_input_interface(InputUser,Kind,Struct0,Struct1).
input_interface(aeq,InputUser,Kind,Struct0,Struct1):-
	aeq_input_interface(InputUser,Kind,Struct0,Struct1).
input_interface(lsign,InputUser,Kind,Struct0,Struct1):-
	lsign_input_interface(InputUser,Kind,Struct0,Struct1).
input_interface(lsigndef,InputUser,Kind,_Struct0,Struct1):-
	lsigndef_input_interface(InputUser,Kind,Struct1).
input_interface(lsignshfr,InputUser,Kind,Struct0,Struct1):-
	lsignshfr_input_interface(InputUser,Kind,Struct0,Struct1).
input_interface(difflsign,InputUser,Kind,Struct0,Struct1):-
	lsign_input_interface(InputUser,Kind,Struct0,Struct1).
input_interface(sha,InputUser,Kind,Struct0,Struct1):-
	sha_input_interface(InputUser,Kind,Struct0,Struct1).
input_interface(nf,InputUser,Kind,Struct0,Struct1):-
	nf_input_interface(InputUser,Kind,Struct0,Struct1).
input_interface(det,InputUser,Kind,Struct0,Struct1):-
	det_input_interface(InputUser,Kind,Struct0,Struct1).


:- doc(doinclude,input_user_interface/4).
:- doc(input_user_interface(AbsInt,Struct,Qv,ASub),
   "@var{ASub} is the abstraction in @var{AbsInt} of the information collected
    in @var{Struct} (a domain defined structure) on variables @var{Qv}.").

input_user_interface(pd,_InputUser,_Qv,top).
input_user_interface(pdb,_InputUser,_Qv,top).
input_user_interface(gr,InputUser,Qv,ASub):-
	gr_input_user_interface(InputUser,Qv,ASub).
input_user_interface(java_nullity,InputUser,Qv,ASub):-
	java_nullity_input_user_interface(InputUser,Qv,ASub).
input_user_interface(share,InputUser,Qv,ASub):-
	share_input_user_interface(InputUser,Qv,ASub).
input_user_interface(share_amgu,InputUser,Qv,ASub):-
	share_input_user_interface(InputUser,Qv,ASub).
input_user_interface(share_clique,InputUser,Qv,ASub):-
	share_clique_input_user_interface(InputUser,Qv,ASub).
input_user_interface(share_clique_1,InputUser,Qv,ASub):-
	share_clique_input_user_interface(InputUser,Qv,ASub).
input_user_interface(share_clique_def,InputUser,Qv,ASub):-
	share_clique_def_input_user_interface(InputUser,Qv,ASub).
input_user_interface(sharefree_clique_def,InputUser,Qv,ASub):-
	sharefree_clique_def_input_user_interface(InputUser,Qv,ASub).
input_user_interface(sharefree_amgu,InputUser,Qv,ASub):-
	shfr_input_user_interface(InputUser,Qv,ASub).
input_user_interface(shfrlin_amgu,InputUser,Qv,ASub):-
	shfrlin_input_user_interface(InputUser,Qv,ASub).
input_user_interface(sharefree_clique,InputUser,Qv,ASub):-
	sharefree_clique_input_user_interface(InputUser,Qv,ASub).
input_user_interface(shfr,InputUser,Qv,ASub):-
	shfr_input_user_interface(InputUser,Qv,ASub).
input_user_interface(shfret,InputUser,Qv,ASub):-
	shfret_input_user_interface(InputUser,Qv,ASub).
input_user_interface(shfrnv,InputUser,Qv,ASub):-
	shfrnv_input_user_interface(InputUser,Qv,ASub).
input_user_interface(son,InputUser,Qv,ASub):-
	son_input_user_interface(InputUser,Qv,ASub).
input_user_interface(oo_son,InputUser,Qv,ASub):-
	oo_son_input_user_interface(InputUser,Qv,ASub).
input_user_interface(oo_shnltau,InputUser,Qv,ASub):-
	oo_shnltau_input_user_interface(InputUser,Qv,ASub).
input_user_interface(shareson,InputUser,Qv,ASub):-
	shareson_input_user_interface(InputUser,Qv,ASub).
input_user_interface(shfrson,InputUser,Qv,ASub):-
	shfrson_input_user_interface(InputUser,Qv,ASub).
input_user_interface(path,InputUser,Qv,ASub):-
	path_input_user_interface(InputUser,Qv,ASub).
input_user_interface(depth,InputUser,Qv,ASub):-
	depthk_input_user_interface(InputUser,Qv,ASub).
input_user_interface(eterms,InputUser,Qv,ASub):-
	eterms_input_user_interface(InputUser,Qv,ASub).
input_user_interface(svterms,InputUser,Qv,ASub):-
	svterms_input_user_interface(InputUser,Qv,ASub).
input_user_interface(terms,InputUser,Qv,ASub):-
	terms_input_user_interface(InputUser,Qv,ASub).
input_user_interface(ptypes,InputUser,Qv,ASub):-
	terms_input_user_interface(InputUser,Qv,ASub).
input_user_interface(deftypes,InputUser,Qv,ASub):-
	deftypes_input_user_interface(InputUser,Qv,ASub).
input_user_interface(oo_types,InputUser,Qv,ASub):-
	oo_types_input_user_interface(InputUser,Qv,ASub).
input_user_interface(java_cha,InputUser,Qv,ASub):-
	java_cha_input_user_interface(InputUser,Qv,ASub).
input_user_interface(polyhedra,InputUser,Qv,ASub):-
	polyhedra_input_user_interface(InputUser,Qv,ASub).
input_user_interface(fr,InputUser,Qv,ASub):-
	fr_input_user_interface(InputUser,Qv,ASub).
input_user_interface(def,InputUser,_Qv,ASub):-
	def_input_user_interface(InputUser,ASub).
input_user_interface(frdef,InputUser,Qv,ASub):-
	fd_input_user_interface(InputUser,Qv,ASub).
input_user_interface(aeq,InputUser,Qv,ASub):-
	aeq_input_user_interface(InputUser,Qv,ASub).
input_user_interface(lsign,InputUser,Qv,ASub):-
	lsign_input_user_interface(InputUser,Qv,ASub).
input_user_interface(lsigndef,InputUser,_Qv,ASub):-
	lsigndef_input_user_interface(InputUser,ASub).
input_user_interface(lsignshfr,InputUser,Qv,ASub):-
	lsignshfr_input_user_interface(InputUser,Qv,ASub).
input_user_interface(difflsign,InputUser,Qv,ASub):-
	simple_lsign_input_user_interface(InputUser,Qv,ASub).
input_user_interface(sha,InputUser,Qv,ASub):-
	sha_input_user_interface(InputUser,Qv,ASub).
input_user_interface(nf,InputUser,Qv,ASub):-
	nf_input_user_interface(InputUser,Qv,ASub).
input_user_interface(det,InputUser,Qv,ASub):-
	det_input_user_interface(InputUser,Qv,ASub).

%------------------------------------------------------------------------%
% asub_to_info(+,+,+,-,-)                                                %
%------------------------------------------------------------------------%
:- doc(asub_to_info(AbsInt,ASub,Qv,OutputUser,CompProps),
   "Transforms an abstract substitution @var{ASub} on variables @var{Qv} for a
    domain @var{AbsInt} to a list of state properties @var{OutputUser} and
    computation properties @var{CompProps}, such that properties are
    visible in the preprocessing unit. It fails if @var{ASub} represents
    bottom. It works by calling @tt{asub_to_native/4}.").

asub_to_info(AbsInt,ASub,Qv,OutputUser,CompProps):-
	asub_to_native(AbsInt,ASub,Qv,Info,Comp),
	native_props(Info,OutputUser),
	native_props(Comp,CompProps).

:- doc(hide,asub_to_out/5).
asub_to_out(AbsInt,ASub,Qv,OutputUser,CompProps):-
	asub_to_native_out(AbsInt,ASub,Qv,Info,Comp),
	native_props(Info,OutputUser0),
	native_props(Comp,CompProps0),
	decide_low_level_format(OutputUser0,CompProps0,OutputUser,CompProps).
	

:- doc(asub_to_native(AbsInt,ASub,Qv,NativeStat,NativeComp),
   "@var{NativeStat} and @var{NativeComp} are the list of native (state and
    computational, resp.) properties that are the concretization
    of abstract substitution @var{ASub} on variables @var{Qv} for domain
    @var{AbsInt}. These are later translated to the properties which are
    visible in the preprocessing unit.").

asub_to_native(pd,_ASub,_Qv,[true],[]).
asub_to_native(pdb,ASub,_Qv,OutputUser,[]):-
	pdb_asub_to_native(ASub,_Qv,OutputUser).
asub_to_native(gr,ASub,Qv,OutputUser,[]):-
	gr_asub_to_native(ASub,Qv,OutputUser).
asub_to_native(java_nullity,ASub,Qv,OutputUser,[]):-
	java_nullity_asub_to_native(ASub,Qv,OutputUser).
asub_to_native(bshare,ASub,Qv,OutputUser,[]):-
	bshare_asub_to_native(ASub,Qv,OutputUser).
asub_to_native(share,ASub,Qv,OutputUser,[]):-
	share_asub_to_native(ASub,Qv,OutputUser).
asub_to_native(share_amgu,ASub,Qv,OutputUser,_):-
	share_asub_to_native(ASub,Qv,OutputUser).
asub_to_native(share_clique,ASub,Qv,OutputUser,_):-
	share_clique_asub_to_native(ASub,Qv,OutputUser).
asub_to_native(share_clique_1,ASub,Qv,OutputUser,_):-
	share_clique_1_asub_to_native(ASub,Qv,OutputUser).
asub_to_native(share_clique_def,ASub,Qv,OutputUser,_):-
	share_clique_def_asub_to_native(ASub,Qv,OutputUser).
asub_to_native(sharefree_clique_def,ASub,Qv,OutputUser,_):-
	sharefree_clique_def_asub_to_native(ASub,Qv,OutputUser).
asub_to_native(sharefree_amgu,ASub,Qv,OutputUser,_):-
	shfr_asub_to_native(ASub,Qv,OutputUser).
asub_to_native(shfrlin_amgu,ASub,Qv,OutputUser,_):-
	shfrlin_asub_to_native(ASub,Qv,OutputUser).
asub_to_native(sharefree_clique,ASub,Qv,OutputUser,_):-
	sharefree_clique_asub_to_native(ASub,Qv,OutputUser).
asub_to_native(shfr,ASub,Qv,OutputUser,[]):-
	shfr_asub_to_native(ASub,Qv,OutputUser).
asub_to_native(shfret,ASub,Qv,OutputUser,[]):-
	shfret_asub_to_native(ASub,Qv,no,OutputUser).
asub_to_native(shfrnv,ASub,Qv,OutputUser,[]):-
	shfrnv_asub_to_native(ASub,Qv,OutputUser).
asub_to_native(son,ASub,Qv,OutputUser,[]):-
	son_asub_to_native(ASub,Qv,OutputUser).
asub_to_native(oo_son,ASub,Qv,OutputUser,[]):-
	oo_son_asub_to_native(ASub,Qv,OutputUser).
asub_to_native(oo_shnltau,ASub,Qv,OutputUser,[]):-
	oo_shnltau_asub_to_native(ASub,Qv,OutputUser).
asub_to_native(shareson,ASub,Qv,OutputUser,[]):-
	shareson_asub_to_native(ASub,Qv,OutputUser).
asub_to_native(shfrson,ASub,Qv,OutputUser,[]):-
	shfrson_asub_to_native(ASub,Qv,OutputUser).
asub_to_native(path,ASub,Qv,OutputUser,[]):-
	path_asub_to_native(ASub,Qv,OutputUser).
asub_to_native(depth,ASub,_Qv,OutputUser,[]):-
	depthk_asub_to_native(ASub,OutputUser).
asub_to_native(eterms,ASub,_Qv,OutputUser,[]):-
	eterms_asub_to_native(ASub,no,OutputUser).
asub_to_native(svterms,ASub,_Qv,OutputUser,[]):-
	svterms_asub_to_native(ASub,no,OutputUser).
asub_to_native(terms,ASub,_Qv,OutputUser,[]):-
	terms_asub_to_native(ASub,no,OutputUser).
asub_to_native(ptypes,ASub,_Qv,OutputUser,[]):-
	terms_asub_to_native(ASub,no,OutputUser).
asub_to_native(deftypes,ASub,_Qv,OutputUser,[]):-
	deftypes_asub_to_native(ASub,no,OutputUser).
asub_to_native(oo_types,ASub,Qv,OutputUser,[]):-
	oo_types_asub_to_native(ASub,Qv,OutputUser).
asub_to_native(java_cha,ASub,Qv,OutputUser,[]):-
	java_cha_asub_to_native(ASub,Qv,OutputUser).
asub_to_native(polyhedra,ASub,Qv,OutputUser,[]):-
	polyhedra_asub_to_native(ASub,Qv,OutputUser).
asub_to_native(fr,ASub,Qv,OutputUser,[]):-
	fr_output_interface(ASub,Qv,OutputUser).
asub_to_native(def,ASub,_Qv,OutputUser,[]):-
	def_asub_to_native(ASub,OutputUser).
asub_to_native(frdef,ASub,Qv,OutputUser,[]):-
	fd_output_interface(ASub,Qv,OutputUser).
asub_to_native(aeq,ASub,_Qv,OutputUser,[]):-
	aeq_asub_to_native(ASub,OutputUser).
asub_to_native(lsign,ASub,_Qv,OutputUser,[]):-
	lsign_output_interface(ASub,OutputUser).
asub_to_native(lsigndef,ASub,Qv,OutputUser,[]):-
	lsigndef_output_interface(ASub,Qv,OutputUser).
asub_to_native(lsignshfr,ASub,Qv,OutputUser,[]):-
	lsignshfr_output_interface(ASub,Qv,OutputUser).
asub_to_native(difflsign,ASub,_Qv,OutputUser,[]):-
	simple_lsign_output_interface(ASub,OutputUser).
asub_to_native(sha,ASub,Qv,OutputUser,[]):-
	sha_asub_to_native(ASub,Qv,OutputUser).
asub_to_native(nf,ASub,Qv,OutputUser,Comp):-
	nf_asub_to_native(ASub,Qv,no,OutputUser,Comp).
asub_to_native(det,ASub,Qv,OutputUser,Comp):-
	det_asub_to_native(ASub,Qv,no,OutputUser,Comp).

asub_to_native_out(eterms,ASub,_Qv,OutputUser,[]):- !,
	eterms_asub_to_native(ASub,yes,OutputUser).
asub_to_native_out(svterms,ASub,_Qv,OutputUser,[]):- !,
	svterms_asub_to_native(ASub,yes,OutputUser).
asub_to_native_out(terms,ASub,_Qv,OutputUser,[]):- !,
	terms_asub_to_native(ASub,yes,OutputUser).
asub_to_native_out(ptypes,ASub,_Qv,OutputUser,[]):- !,
	terms_asub_to_native(ASub,yes,OutputUser).
asub_to_native_out(deftypes,ASub,_Qv,OutputUser,[]):- !,
	deftypes_asub_to_native(ASub,yes,OutputUser).
asub_to_native_out(nf,ASub,Qv,OutputUser,Comp):- !,
	nf_asub_to_native(ASub,Qv,yes,OutputUser,Comp).
asub_to_native_out(det,ASub,Qv,OutputUser,Comp):- !,
	det_asub_to_native(ASub,Qv,yes,OutputUser,Comp).
asub_to_native_out(shfret,ASub,Qv,OutputUser,[]):- !,
	shfret_asub_to_native(ASub,Qv,yes,OutputUser).
asub_to_native_out(AbsInt,ASub,Qv,Info,Comp):-
	asub_to_native(AbsInt,ASub,Qv,Info,Comp).

%------------------------------------------------------------------------%
% concrete(+,+,+,-)                                                      %
%------------------------------------------------------------------------%
:- doc(concrete(AbsInt,Var,ASub,List),
   "@var{List} are (all) the terms to which @var{Var} can be bound in the 
    concretization of @var{ASub}, if they are a finite number of finite
    terms. Otherwise, the predicate fails.").

concrete(terms,Var,ASub,List):-
	terms_concret(Var,ASub,List).
concrete(eterms,Var,ASub,List):-
	eterms_concret(Var,ASub,List).
concrete(svterms,Var,ASub,List):-
	svterms_concret(Var,ASub,List).
concrete(ptypes,Var,ASub,List):-
	terms_concret(Var,ASub,List).
concrete(deftypes,Var,ASub,List):-
	terms_concret(Var,ASub,List).


%------------------------------------------------------------------------%
% unknown_call(+,+,+,-)                                                  %
%------------------------------------------------------------------------%
:- push_prolog_flag(multi_arity_warnings,off).
:- doc(unknown_call(AbsInt,Vars,Call,Succ),
   "@var{Succ} is the result of adding to @var{Call} the ""topmost""
    abstraction in domain @var{AbsInt} of the variables @var{Vars} involved in
    a literal whose definition is not present in the preprocessing unit.
    I.e., it is like the conjunction of the information in @var{Call} with the
    top for a subset of its variables.").

unknown_call(pd,_Vars,Call,Call).
unknown_call(pdb,_Vars,Call,Call).
unknown_call(gr,Vars,Call,Succ):-
	gr_unknown_call(Call,Vars,Succ).
unknown_call(share,Vars,Call,Succ):-
	share_unknown_call(Call,Vars,Succ).
unknown_call(share_amgu,Vars,Call,Succ):-
	share_unknown_call(Call,Vars,Succ).
unknown_call(share_clique,Vars,Call,Succ):-
	share_clique_unknown_call(Call,Vars,Succ).
unknown_call(share_clique_1,Vars,Call,Succ):-
	share_clique_1_unknown_call(Call,Vars,Succ).
unknown_call(share_clique_def,Vars,Call,Succ):-
	share_clique_def_unknown_call(Call,Vars,Succ).
unknown_call(sharefree_clique_def,Vars,Call,Succ):-
	sharefree_clique_def_unknown_call(Call,Vars,Succ).
unknown_call(sharefree_amgu,Vars,Call,Succ):-
	shfr_unknown_call(Call,Vars,Succ).
unknown_call(shfrlin_amgu,Vars,Call,Succ):-
	shfrlin_unknown_call(Call,Vars,Succ).
unknown_call(sharefree_clique,Vars,Call,Succ):-
	sharefree_clique_unknown_call(Call,Vars,Succ).
unknown_call(shfr,Vars,Call,Succ):-
	shfr_unknown_call(Call,Vars,Succ).
unknown_call(shfret,Vars,Call,Succ):-
	shfret_unknown_call(Call,Vars,Succ).
unknown_call(shfrnv,Vars,Call,Succ):-
	shfr_unknown_call(Call,Vars,Succ).
unknown_call(son,Vars,Call,Succ):-
	son_unknown_call(Call,Vars,Succ).
unknown_call(oo_son,Vars,Call,Succ):-
	oo_son_unknown_call(Call,Vars,Succ).
unknown_call(oo_shnltau,Vars,Call,Succ):-
	oo_shnltau_unknown_call(Call,Vars,Succ).
unknown_call(shareson,Vars,Call,Succ):-
	shareson_unknown_call(Call,Vars,Succ).
unknown_call(shfrson,Vars,Call,Succ):-
	shfrson_unknown_call(Call,Vars,Succ).
unknown_call(path,Vars,Call,Succ):-
	path_unknown_call(Call,Vars,Succ).
unknown_call(depth,Vars,Call,Succ):-
	depthk_unknown_call(Call,Vars,Succ).
unknown_call(eterms,Vars,Call,Succ):-
	eterms_unknown_call(Call,Vars,Succ).
unknown_call(svterms,Vars,Call,Succ):-
	svterms_unknown_call(Call,Vars,Succ).
unknown_call(terms,Vars,Call,Succ):-
	terms_unknown_call(Call,Vars,Succ).
unknown_call(ptypes,Vars,Call,Succ):-
	terms_unknown_call(Call,Vars,Succ).
unknown_call(deftypes,Vars,Call,Succ):-
	terms_unknown_call(Call,Vars,Succ).
unknown_call(oo_types,Vars,Call,Succ):-
	oo_types_unknown_call(Call,Vars,Succ).
unknown_call(polyhedra,Vars,Call,Succ):-
	polyhedra_unknown_call(Call,Vars,Succ).
unknown_call(fr,Vars,Call,Succ):-
	fr_unknown_call(Vars,Call,Succ).
unknown_call(def,Vars,Call,Succ):-
	def_unknown_call(Vars,Call,Succ).
unknown_call(frdef,Vars,Call,Succ):-
	fd_unknown_call(Vars,Call,Succ).
unknown_call(aeq,Vars,Call,Succ):-
	aeq_unknown_call(Vars,Call,Succ).
unknown_call(lsign,Vars,Call,Succ):-
	lsign_unknown_call(Vars,Call,Succ).
unknown_call(lsigndef,Vars,Call,Succ):-
	lsigndef_unknown_call(Call,Vars,Succ).
unknown_call(lsignshfr,Vars,Call,Succ):-
	lsignshfr_unknown_call(Call,Vars,Succ).
unknown_call(difflsign,Vars,Call,Succ):-
	simple_lsign_unknown_call(Vars,Call,Succ).
unknown_call(sha,Vars,Call,Succ):-
	sha_unknown_call(Call,Vars,Succ).
unknown_call(nf,Vars,Call,Succ):-
	nf_unknown_call(Vars,Call,Succ).
unknown_call(det,Vars,Call,Succ):-
	det_unknown_call(Vars,Call,Succ).

%------------------------------------------------------------------------%
% unknown_call(+,+,+,+,-)                                                  %
%------------------------------------------------------------------------%

:- doc(unknown_call(AbsInt,Sg,Vars,Call,Succ), "@var{Succ} is the
   result of adding to @var{Call} the ""topmost"" abstraction in domain
   @var{AbsInt} of the variables @var{Vars} involved in a literal @var{Sg}
   whose definition is not present in the preprocessing unit.  I.e., it is
   like the conjunction of the information in @var{Call} with the top for a
   subset of its variables.").

unknown_call(java_nullity,Sg,Vars,Call,Succ):-
	java_nullity_unknown_call(Sg,Call,Vars,Succ).
unknown_call(java_cha,Sg,Vars,Call,Succ):-
	java_cha_unknown_call(Sg,Call,Vars,Succ).
unknown_call(_AbsInt,_Sg,_Vars,_Call,_Succ):-
	compiler_error(op_not_implemented(unknown_call)),
	fail.
:- pop_prolog_flag(multi_arity_warnings).
	
%------------------------------------------------------------------------%
% unknown_entry(+,+,-)                                                   %
%------------------------------------------------------------------------%
:- push_prolog_flag(multi_arity_warnings,off).
:- doc(unknown_entry(AbsInt,Vars,Entry),
   "@var{Entry} is the ""topmost"" abstraction in domain @var{AbsInt} of 
    variables @var{Vars}.").

unknown_entry(pd,_Qv,'top').
unknown_entry(pdb,_Qv,'top').
unknown_entry(gr,Qv,Call):-
	gr_unknown_entry(Qv,Call).
unknown_entry(share,Qv,Call):-
	share_unknown_entry(Qv,Call).
unknown_entry(share_amgu,Qv,Call):-
	share_unknown_entry(Qv,Call).
unknown_entry(bshare,Qv,Call):-
	bshare_unknown_entry(Qv,Call).
unknown_entry(share_clique,Qv,Call):-
	share_clique_unknown_entry(Qv,Call).
unknown_entry(share_clique_1,Qv,Call):-
	share_clique_1_unknown_entry(Qv,Call).
unknown_entry(share_clique_def,Qv,Call):-
	share_clique_def_unknown_entry(Qv,Call).
unknown_entry(sharefree_clique_def,Qv,Call):-
	sharefree_clique_def_unknown_entry(Qv,Call).
unknown_entry(sharefree_amgu,Qv,Call):-
	shfr_unknown_entry(Qv,Call).
unknown_entry(shfrlin_amgu,Qv,Call):-
	shfrlin_unknown_entry(Qv,Call).
unknown_entry(sharefree_clique,Qv,Call):-
	sharefree_clique_unknown_entry(Qv,Call).
unknown_entry(shfr,Qv,Call):-
	shfr_unknown_entry(Qv,Call).
unknown_entry(shfret,Qv,Call):-
	shfret_unknown_entry(Qv,Call).
unknown_entry(shfrnv,Qv,Call):-
	shfr_unknown_entry(Qv,Call).
unknown_entry(son,Qv,Call):-
	son_unknown_entry(Qv,Call).
unknown_entry(oo_son,Qv,Call):-
	oo_son_unknown_entry(Qv,Call).
unknown_entry(oo_shnltau,Qv,Call):-
	oo_shnltau_unknown_entry(Qv,Call).
unknown_entry(shareson,Qv,Call):-
	shareson_unknown_entry(Qv,Call).
unknown_entry(shfrson,Qv,Call):-
	shfrson_unknown_entry(Qv,Call).
unknown_entry(path,Qv,Call):-
	path_unknown_entry(Qv,Call).
unknown_entry(depth,Qv,Call):-
	depthk_unknown_entry(Qv,Call).
unknown_entry(eterms,Qv,Call):-
	eterms_unknown_entry(Qv,Call).
unknown_entry(svterms,Qv,Call):-
	svterms_unknown_entry(Qv,Call).
unknown_entry(terms,Qv,Call):-
	terms_unknown_entry(Qv,Call).
unknown_entry(ptypes,Qv,Call):-
	terms_unknown_entry(Qv,Call).
unknown_entry(deftypes,Qv,Call):-
	terms_unknown_entry(Qv,Call).
unknown_entry(oo_types,Qv,Call):-
	oo_types_unknown_entry(Qv,Call).
unknown_entry(polyhedra,Qv,Call):-
	polyhedra_unknown_entry(Qv,Call).
unknown_entry(fr,Qv,Call):-
	fr_unknown_entry(Qv,Call).
unknown_entry(def,Qv,Call):-
	def_unknown_entry(Qv,Call).
unknown_entry(frdef,Qv,Call):-
	fd_unknown_entry(Qv,Call).
unknown_entry(aeq,Qv,Call):-
	aeq_unknown_entry(Qv,Call).
unknown_entry(lsign,Qv,Call):-
	lsign_unknown_entry(Qv,Call).
unknown_entry(lsigndef,Qv,Call):-
	lsigndef_unknown_entry(Qv,Call).
unknown_entry(lsignshfr,Qv,Call):-
	lsignshfr_unknown_entry(Qv,Call).
unknown_entry(difflsign,Qv,Call):-
	simple_lsign_unknown_entry(Qv,Call).
unknown_entry(sha,Qv,Call):-
	sha_unknown_entry(Qv,Call).
unknown_entry(nf,Qv,Call):-
	nf_unknown_entry(Qv,Call).
unknown_entry(det,Qv,Call):-
	det_unknown_entry(Qv,Call).

%------------------------------------------------------------------------%
% unknown_entry(+,+,+,-)                                                   %
%------------------------------------------------------------------------%
:- doc(unknown_entry(AbsInt,Sg,Vars,Entry),
   "@var{Entry} is the ""topmost"" abstraction in domain @var{AbsInt} of 
    variables @var{Vars} corresponding to literal @var{Sg}.").

unknown_entry(java_nullity,Sg,Qv,Call):-
	java_nullity_unknown_entry(Sg,Qv,Call).
unknown_entry(java_cha,Sg,Qv,Call):-
	java_cha_unknown_entry(Sg,Qv,Call).
unknown_entry(_AbsInt,_Sg,_Vars,_Call):-
	compiler_error(op_not_implemented(unknown_entry)),
	fail.
:- pop_prolog_flag(multi_arity_warnings).

%------------------------------------------------------------------------%
% empty_entry(+,+,-)                                                     %
%------------------------------------------------------------------------%
:- doc(empty_entry(AbsInt,Vars,Entry),
   "@var{Entry} is the ""empty"" abstraction in domain @var{AbsInt} of
    variables @var{Vars}. I.e., it is the abstraction of a substitution on
    @var{Vars} in which all variables are unbound: free and unaliased.").

empty_entry(pd,_Qv,'top').
empty_entry(pdb,_Qv,'top').
empty_entry(gr,Qv,Call):-
	gr_empty_entry(Qv,Call).
empty_entry(share,Qv,Call):-
	share_empty_entry(Qv,Call).
empty_entry(bshare,Qv,Call):-
	bshare_empty_entry(Qv,Call).
empty_entry(share_amgu,Qv,Call):-
	share_empty_entry(Qv,Call).
empty_entry(share_clique,Qv,Call):-
	share_clique_empty_entry(Qv,Call).
empty_entry(share_clique_1,Qv,Call):-
	share_clique_empty_entry(Qv,Call).
empty_entry(share_clique_def,Qv,Call):-
	share_clique_def_empty_entry(Qv,Call).
empty_entry(sharefree_clique_def,Qv,Call):-
	sharefree_clique_def_empty_entry(Qv,Call).
empty_entry(sharefree_amgu,Qv,Call):-
	shfr_empty_entry(Qv,Call).
empty_entry(shfrlin_amgu,Qv,Call):-
	shfrlin_empty_entry(Qv,Call).
empty_entry(sharefree_clique,Qv,Call):-
	sharefree_clique_empty_entry(Qv,Call).
empty_entry(shfr,Qv,Call):-
	shfr_empty_entry(Qv,Call).
empty_entry(shfret,Qv,Call):-
	shfret_empty_entry(Qv,Call).
empty_entry(shfrnv,Qv,Call):-
	shfr_empty_entry(Qv,Call).
empty_entry(oo_son,Qv,Call):-
	oo_son_empty_entry(Qv,Call).
empty_entry(oo_shnltau,Qv,Call):-
	oo_shnltau_empty_entry(Qv,Call).
empty_entry(son,Qv,Call):-
	son_empty_entry(Qv,Call).
empty_entry(shareson,Qv,Call):-
	shareson_empty_entry(Qv,Call).
empty_entry(shfrson,Qv,Call):-
	shfrson_empty_entry(Qv,Call).
empty_entry(path,Qv,Call):-
	path_empty_entry(Qv,Call).
empty_entry(depth,Qv,Call):-
	depthk_empty_entry(Qv,Call).
empty_entry(eterms,Qv,Call):-
	eterms_empty_entry(Qv,Call).
empty_entry(svterms,Qv,Call):-
	svterms_empty_entry(Qv,Call).
empty_entry(terms,Qv,Call):-
	terms_empty_entry(Qv,Call).
empty_entry(ptypes,Qv,Call):-
	terms_empty_entry(Qv,Call).
empty_entry(deftypes,Qv,Call):-
	terms_empty_entry(Qv,Call).
empty_entry(oo_types,Qv,Call):-
	oo_types_empty_entry(Qv,Call).
empty_entry(polyhedra,Qv,Call):-
	polyhedra_empty_entry(Qv,Call).
empty_entry(fr,Qv,Call):-
	fr_empty_entry(Qv,Call).
empty_entry(def,Qv,Call):-
	def_unknown_entry(Qv,Call).
empty_entry(frdef,Qv,Call):-
	fd_empty_entry(Qv,Call).
empty_entry(aeq,Qv,Call):-
	aeq_empty_entry(Qv,Call).
empty_entry(lsign,Qv,Call):-
	lsign_empty_entry(Qv,Call).
empty_entry(lsigndef,Qv,Call):-
	lsigndef_empty_entry(Qv,Call).
empty_entry(lsignshfr,Qv,Call):-
	lsignshfr_empty_entry(Qv,Call).
empty_entry(difflsign,Qv,Call):-
	simple_lsign_empty_entry(Qv,Call).
empty_entry(sha,Qv,Call):-
	sha_empty_entry(Qv,Call).
empty_entry(nf,Qv,Call):-
	nf_empty_entry(Qv,Call).
empty_entry(det,Qv,Call):-
	det_empty_entry(Qv,Call).

%------------------------------------------------------------------------%
% init_abstract_domain(+,-)                                              %
%------------------------------------------------------------------------%
:- doc(init_abstract_domain(AbsInt,Norm),
   "Initializes abstract domain @var{AbsInt}. Tells whether @var{AbsInt}
    requires a normalized program.").

%% terms_init:- reset_types.
init_abstract_domain(terms,N):- !,
	current_pp_flag(variants,V),
	push_pp_flag(variants,V),
	set_pp_flag(widen,on),
	current_pp_flag(normalize,N).
init_abstract_domain(ptypes,N):- !,
	current_pp_flag(variants,V),
	push_pp_flag(variants,V),
	set_pp_flag(widen,on),
	current_pp_flag(normalize,N).
init_abstract_domain(deftypes,N):- !,
	current_pp_flag(variants,V),
	push_pp_flag(variants,V),
	set_pp_flag(widen,on),
	current_pp_flag(normalize,N).
%	build_defined_types_lattice.
init_abstract_domain(polyhedra,N):- !, 
        current_pp_flag(variants,V), 
        push_pp_flag(variants,V), 
        set_pp_flag(widen,on), 
        current_pp_flag(normalize,N). 
init_abstract_domain(eterms,N):- !,
	current_pp_flag(variants,V),
	push_pp_flag(variants,V),
	set_pp_flag(widen,on),
	current_pp_flag(normalize,N).
init_abstract_domain(svterms,N):- !,
	current_pp_flag(variants,V),
	push_pp_flag(variants,V),
	set_pp_flag(widen,on),
	current_pp_flag(normalize,N).
init_abstract_domain(nf,N):- !,
	push_pp_flag(variants,off),
	set_pp_flag(widen,on),
	current_pp_flag(normalize,N).
init_abstract_domain(det,N):- !,
	push_pp_flag(variants,off),
	set_pp_flag(widen,on),
	current_pp_flag(normalize,N).
init_abstract_domain(shfret,N):- !,
	push_pp_flag(variants,off),
	set_pp_flag(widen,on),
	current_pp_flag(normalize,N).
init_abstract_domain(path,N):- !,
	push_pp_flag(variants,off),
	set_pp_flag(multi_success,on),
	current_pp_flag(normalize,N).
init_abstract_domain(lsign,N):- !,
	push_pp_flag(variants,off),
	N=on.
% init_abstract_domain(oo_shnltau,N):- !,
% 	oo_shnltau_init_abstract_domain,
% 	current_pp_flag(normalize,N).
% init_abstract_domain(oo_types,N):- !,
% 	oo_types_init_abstract_domain,
% 	current_pp_flag(normalize,N).
init_abstract_domain(_AbsInt,Norm):-
	push_pp_flag(variants,off),
	current_pp_flag(normalize,Norm).
/*
%------------------------------------------------------------------------%
% propagate_downwards_closed(+,+,+,-)
% propagate_downwards_closed(AbsInt,ASub1,ASub2,ASub)
% Propagates the downwards closed properties from ASub1 to ASub2
%------------------------------------------------------------------------%

propagate_downwards_closed(def,ASub1,ASub2,ASub):-
	def_downwards_closed(ASub1,ASub2,ASub).
propagate_downwards_closed(shfr,ASub1,ASub2,ASub):-
	shfr_downwards_closed(ASub1,ASub2,ASub).
propagate_downwards_closed(shfrnv,ASub1,ASub2,ASub):-
	shfrnv_downwards_closed(ASub1,ASub2,ASub).
propagate_downwards_closed(aeq,ASub1,ASub2,ASub):-
	aeq_downwards_closed(ASub1,ASub2,ASub).

%------------------------------------------------------------------------%
% del_real_conjoin(+,+,+,-)
% del_real_conjoin(AbsInt,ASub1,ASub2,ASub)
% Propagates the downwards closed properties from ASub1 to ASub2
%------------------------------------------------------------------------%

del_real_conjoin(def,ASub1,ASub2,ASub):-
	def_real_conjoin(ASub1,ASub2,ASub).
del_real_conjoin(shfr,ASub1,ASub2,ASub):-
	shfr_real_conjoin(ASub1,ASub2,ASub).
del_real_conjoin(shfrnv,ASub1,ASub2,ASub):-
	shfrnv_real_conjoin(ASub1,ASub2,ASub).
del_real_conjoin(aeq,ASub1,ASub2,ASub):-
        aeq_real_conjoin(ASub1,ASub2,ASub).

%------------------------------------------------------------------------%
% del_hash(+,+,+,-)
% del_hash(AbsInt,ASub,Vars,N)
% Returns a number which identifies ASub
%------------------------------------------------------------------------%

del_hash(def,ASub,Vars,N):- 
	def_hash(ASub,Vars,N).
del_hash(shfr,ASub,Vars,N):- 
	shfr_hash(ASub,Vars,N).
del_hash(shfrnv,ASub,Vars,N):- 
	shfrnv_hash(ASub,Vars,N).
del_hash(aeq,ASub,Vars,N):- 
	aeq_hash(ASub,Vars,N).

%------------------------------------------------------------------------%
% more_instantiate(+,+,+)
% more_instantiate(AbsInt,ASub1,ASub2)
% Succesdes if ASub2 is possibly more instantiated than ASub1
%------------------------------------------------------------------------%

more_instantiate(def,ASub1,ASub2):-
	def_less_or_equal(ASub2,ASub1).
more_instantiate(shfr,ASub1,ASub2):-
	shfr_more_instantiate(ASub1,ASub2).
more_instantiate(shfrnv,ASub1,ASub2):-
	shfrnv_more_instantiate(ASub1,ASub2).
more_instantiate(aeq,ASub1,ASub2):-
	aeq_more_instantiate(ASub1,ASub2).

%------------------------------------------------------------------------%
% convex_hull(+,+,+,-)
% convex_hull(AbsInt,ASub1,ASub2,Hull)
%------------------------------------------------------------------------%

convex_hull(def,Old,_,Old).
convex_hull(shfr,Old,New,Hull):-
	shfr_convex_hull(Old,New,Hull).
convex_hull(shfrnv,Old,New,Hull):-
	shfrnv_convex_hull(Old,New,Hull).
convex_hull(aeq,Old,New,Hull):-
	aeq_convex_hull(Old,New,Hull).

%------------------------------------------------------------------------%
% compute_lub_el(+,+,+,-)
% compute_lub_el(AbsInt,ASub1,ASub2,Lub)
% Lub is the lub of abstractions ASub1 and ASub2
%------------------------------------------------------------------------%

%% compute_lub_el(gr,ASub1,ASub2,ASub):-
%% 	gr_compute_lub_el(ASub1,ASub2,ASub).
compute_lub_el(def,ASub1,ASub2,ASub):-
	def_compute_lub_el(ASub1,ASub2,ASub).
compute_lub_el(shfr,ASub1,ASub2,ASub):-
	shfr_compute_lub_el(ASub1,ASub2,ASub).
compute_lub_el(shfrnv,ASub1,ASub2,ASub):-
	shfrnv_compute_lub_el(ASub1,ASub2,ASub).
compute_lub_el(share,ASub1,ASub2,ASub):-
	share_lub(ASub1,ASub2,ASub).
compute_lub_el(bshare,ASub1,ASub2,ASub):-
	bshare_compute_lub_el(ASub1,ASub2,ASub).
compute_lub_el(share_amgu,ASub1,ASub2,ASub):-
	share_lub(ASub1,ASub2,ASub).
compute_lub_el(share_clique,ASub1,ASub2,ASub):-
	share_clique_lub_cl(ASub1,ASub2,ASub).
compute_lub_el(share_clique_1,ASub1,ASub2,ASub):-
	share_clique_1_lub_cl(ASub1,ASub2,ASub).
compute_lub_el(share_clique_def,ASub1,ASub2,ASub):-
	share_clique_def_lub_cl(ASub1,ASub2,ASub).
compute_lub_el(sharefree_clique_def,ASub1,ASub2,ASub):-
	sharefree_clique_def_lub_cl(ASub1,ASub2,ASub).
compute_lub_el(sharefree_clique,ASub1,ASub2,ASub):-
	sharefree_clique_compute_lub_el(ASub1,ASub2,ASub).
compute_lub_el(son,ASub1,ASub2,ASub):-
	son_lub(ASub1,ASub2,ASub).
compute_lub_el(oo_son,ASub1,ASub2,ASub):-
	oo_son_lub(ASub1,ASub2,ASub).
compute_lub_el(oo_shnltau,ASub1,ASub2,ASub):-
	oo_shnltau_lub(ASub1,ASub2,ASub).
compute_lub_el(sha,ASub1,ASub2,ASub):-
	sha_lub(ASub1,ASub2,ASub).
compute_lub_el(aeq,ASub1,ASub2,ASub):-
	aeq_lub(ASub1,ASub2,ASub).

%------------------------------------------------------------------------%
% extend_free(+,+,+,-)
% extend_free(AbsInt,ASub,Vars,ExtASub)
% It extends ASub to the new (free) vars in Vars
%------------------------------------------------------------------------%

%% extend_free(gr,ASub1,Vars,ASub):-
%% 	gr_extend_free(ASub1,Vars,ASub).
extend_free(def,ASub,_,ASub).
extend_free(shfr,ASub1,Vars,ASub):-
	shfr_extend_free(ASub1,Vars,ASub).
extend_free(shfrnv,ASub1,Vars,ASub):-
	shfr_extend_free(ASub1,Vars,ASub).
extend_free(aeq,ASub1,Vars,ASub):-
	aeq_extend_free(ASub1,Vars,ASub).

%------------------------------------------------------------------------%
% del_check_cond(+,+,+,+,-,-)                                            %
% del_check_cond(AbsInt,Cond,ASub,Sv,Flag,WConds).                       %
%------------------------------------------------------------------------%
% Determines if a subgoal is definitely awake (Flag = w), definitely     %
% delayed (Flag = d), or possibly awake (Flag = set of abstractions      %
% under which the subgoal can be woken), w.r.t. abstraction ASub         %
%------------------------------------------------------------------------%

del_check_cond(def,Cond,ASub,Sv,Flag,WConds):-
	def_check_cond(Cond,ASub,Sv,Flag,WConds).
del_check_cond(shfr,Cond,ASub,Sv,Flag,WConds):-
	shfr_check_cond(Cond,ASub,Sv,Flag,WConds).
del_check_cond(shfrnv,Cond,ASub,Sv,Flag,WConds):-
	shfrnv_check_cond(Cond,ASub,Sv,Flag,WConds).
del_check_cond(aeq,Cond,ASub,Sv,Flag,WConds):-
	aeq_check_cond(Cond,ASub,Sv,Flag,WConds).

%------------------------------------------------------------------------%
% del_impose_cond(+,+,+,+,-)                                             %
% del_impose_cond(AbsInt,Cond,Sv,ASub,NewASub)                           %
%------------------------------------------------------------------------%

del_impose_cond(def,LCond,Sv,ASub,LASub):-
	def_impose_cond(LCond,Sv,ASub,LASub).
del_impose_cond(shfr,LCond,Sv,ASub,LASub):-
	shfr_impose_cond(LCond,Sv,ASub,LASub).
del_impose_cond(shfrnv,LCond,Sv,ASub,LASub):-
	shfrnv_impose_cond(LCond,Sv,ASub,LASub).
del_impose_cond(aeq,LCond,Sv,ASub,LASub):-
	aeq_impose_cond(LCond,Sv,ASub,LASub).
*/
:- doc(part_conc(AbsInt,Sg,Subs,NSg,NSubs), "This operation
     returns in @var{NSg} an instance of @var{Sg} in which the
     deterministic structure information available in @var{Subs} is
     materialized. The substitution @var{NSubs} refers to the
     variables in @var{NSg}. ").

part_conc(eterms,Sg,Subs,NSg,NSubs):-!,
	eterms_part_conc(Sg,Subs,NSg,NSubs).
part_conc(_AbsInt,Sg,Subs,Sg,Subs).


:- doc(multi_part_conc(AbsInt,Sg,Subs,List), "Similar to part_conc
     but it gives instantiations of goals even in the case types are
     not deterministic, it generates a @var{List} of pairs of goals
     and substitutions. It stops unfolding types as soon as they are
     recursive.").

multi_part_conc(eterms,Sg,Subs,List):-!,
	eterms_multi_part_conc(Sg,Subs,List).
multi_part_conc(_AbsInt,Sg,Subs,[(Sg,Subs)]).



:- doc(collect_types_in_abs(ASub,AbsInt,Types,Tail), "Collects
	the type symbols occurring in @var{ASub} of domain @var{AbsInt}
        in a difference list @var{Types}-@var{Tail}.").

collect_types_in_abs('$bottom',_AbsInt,Types0,Types):- !,
	Types = Types0.
collect_types_in_abs(ASub,AbsInt,Types0,Types):-
	collect_abstypes_abs(AbsInt,ASub,Types0,Types).

collect_abstypes_abs(eterms,ASub,Types0,Types):-
	eterms_collect_abstypes(ASub,Types0,Types).
collect_abstypes_abs(terms,ASub,Types0,Types):-
	terms_collect_abstypes(ASub,Types0,Types).
collect_abstypes_abs(svterms,ASub,Types0,Types):-
	svterms_collect_abstypes(ASub,Types0,Types).
collect_abstypes_abs(ptypes,ASub,Types0,Types):-
	terms_collect_abstypes(ASub,Types0,Types).
collect_abstypes_abs(deftypes,ASub,Types0,Types):-
	deftypes_collect_abstypes(ASub,Types0,Types).

:- doc(rename_types_in_abs(ASub0,AbsInt,Dict,ASub1), "Renames
	the type symbols occurring in @var{ASub0} of domain @var{AbsInt}
        for the corresponding symbols as in (avl-tree) @var{Dict}
        yielding @var{ASub1}.").

rename_types_in_abs('$bottom',_AbsInt,_Dict,ASub):- !,
	ASub = '$bottom'.
rename_types_in_abs(ASub0,AbsInt,Dict,ASub1):-
	rename_abstypes_abs(AbsInt,ASub0,Dict,ASub1).

rename_abstypes_abs(eterms,ASub,(Types,Names),RenASub):- !,
	eterms_rename_abs(ASub,Types,Names,RenASub).
rename_abstypes_abs(terms,ASub,(Types,Names),RenASub):- !,
	terms_rename_abs(ASub,Types,Names,RenASub).
rename_abstypes_abs(svterms,ASub,(Types,Names),RenASub):- !,
	svterms_rename_abs(ASub,Types,Names,RenASub).
rename_abstypes_abs(deftypes,ASub,(Types,Names),RenASub):- !,
	terms_rename_abs(ASub,Types,Names,RenASub).
rename_abstypes_abs(_,ASub,_Rens,ASub).

%-------------------------------------------------------------------------

:- doc(dom_statistics(AbsInt, Info), "Obtains in list @var{Info}
   statistics about the results of the abstract interpreter
   @var{AbsInt}.").

dom_statistics(det, Info):-
	!,
	det_statistics(Info).
dom_statistics(nf, Info):-
	!,
	nf_statistics(Info).
dom_statistics(_AbsInt, []).

:- pred abstract_instance(AbsInt,Sg1,Proj1,Sg2,Proj2) # "The pair
	<Sg1,Proj1> is an abstract instance of the pair <Sg2,Proj2>, i.e., the
        concretization of <Sg1,Proj1> is included in the concretization of
        <Sg2,Proj2>.".

abstract_instance(AbsInt,Sg1,Proj1,Sg2,Proj2):-
	part_conc(AbsInt,Sg1,Proj1,Sg1C,Proj1C),
	part_conc(AbsInt,Sg2,Proj2,Sg2C,Proj2C),
	instance(Sg1C,Sg2C),
	varset(Sg2C,S2Cv),
	varset(Sg1C,S1Cv),
	call_to_entry(AbsInt,S2Cv,Sg2C,S1Cv,Sg1C,[],Proj2C,Entry,_ExtraInfo),
	Entry \== '$bottom',
	less_or_equal(AbsInt,Proj1C,Entry).

%-------------------------------------------------------------------------
:- doc(contains_parameters(AbsInt,Subst), "True if an abstract substitution
@var{Subst} contains type parameters").

% can succeed only for deftypes
contains_parameters(deftypes,Subst) :-!,
	deftypes_contains_parameters(Subst).
% contains_parameters(_,_) :- fail.

%-------------------------------------------------------------------------
/*
aeq_check_cond(_,_,_,_,_). 
aeq_convex_hull(_,_,_).
aeq_downwards_closed(_,_,_).
aeq_extend_free(_,_,_).
aeq_hash(_,_,_).       
aeq_impose_cond(_,_,_,_).
aeq_lub(_,_,_).        
aeq_more_instantiate(_,_). 
aeq_real_conjoin(_,_,_).

def_check_cond(_,_,_,_,_). 
def_downwards_closed(_,_,_).
def_hash(_,_,_).
def_impose_cond(_,_,_,_).
def_real_conjoin(_,_,_).
*/

lsigndef_call_to_entry(_,_,_,_,_,_,_). 
lsigndef_call_to_success_fact(_,_,_,_,_,_).
lsigndef_compute_lub(_,_).
lsigndef_exit_to_prime(_,_,_,_,_,_,_).
lsigndef_extend(_,_,_,_).  
lsigndef_input_user_interface(_,_).
lsigndef_input_interface(_,_,_).
lsigndef_less_or_equal(_,_). 
lsigndef_output_interface(_,_,_).  
lsigndef_output_interface(_,_,_).  
lsigndef_project(_,_,_,_). 
lsigndef_sort(_,_).    
% lsigndef_success_builtin(_,_,_,_,_,_).
lsigndef_unknown_call(_,_,_).  
lsigndef_unknown_entry(_,_).
lsigndef_empty_entry(_,_). 
% lsignshfr_body_succ_builtin(_,_,_,_,_,_).
lsignshfr_call_to_entry(_,_,_,_,_,_,_,_).  
lsignshfr_call_to_success_fact(_,_,_,_,_,_).
lsignshfr_compute_lub(_,_).
lsignshfr_exit_to_prime(_,_,_,_,_,_,_).  
lsignshfr_extend(_,_,_,_). 
lsignshfr_input_user_interface(_,_,_). 
lsignshfr_input_interface(_,_,_,_). 
lsignshfr_less_or_equal(_,_). 
lsignshfr_output_interface(_,_,_).  
lsignshfr_output_interface(_,_,_).  
lsignshfr_project(_,_,_,_).
lsignshfr_sort(_,_).   
lsignshfr_unknown_call(_,_,_). 
lsignshfr_unknown_entry(_,_). 
lsignshfr_empty_entry(_,_). 

sha_abs_sort(_,_).     
% sha_body_succ_builtin(_,_,_,_,_,_).
sha_call_to_entry(_,_,_,_,_,_,_).
sha_call_to_success_builtin(_,_,_,_,_,_). 
sha_call_to_success_fact(_,_,_,_,_,_,_). 
sha_compute_lub(_,_).  
sha_exit_to_prime(_,_,_,_,_,_,_).
sha_extend(_,_,_,_,_).       
sha_identical_abstract(_,_).
sha_input_user_interface(_,_,_). 
sha_input_interface(_,_,_,_). 
sha_less_or_equal(_,_).
% sha_lub(_,_,_).        
% sha_output_interface(_,_).
sha_asub_to_native(_,_,_).
sha_project(_,_,_).      
sha_special_builtin(_,_,_,_).
sha_success_builtin(_,_,_,_,_).
sha_unknown_call(_,_,_).
sha_unknown_entry(_,_).
sha_empty_entry(_,_).

/*
shfr_check_cond(_,_,_,_,_).
% shfr_compute_lub_el(_,_,_). %% commented out by JNL
shfr_convex_hull(_,_,_).
shfr_downwards_closed(_,_,_).
shfr_extend_free(_,_,_).
shfr_hash(_,_,_).
shfr_impose_cond(_,_,_,_).
shfr_more_instantiate(_,_).
shfr_real_conjoin(_,_,_).

shfrnv_check_cond(_,_,_,_,_).
shfrnv_compute_lub_el(_,_,_).  
shfrnv_convex_hull(_,_,_).
shfrnv_downwards_closed(_,_,_). 
shfrnv_hash(_,_,_).    
shfrnv_impose_cond(_,_,_,_).
shfrnv_more_instantiate(_,_). 
shfrnv_real_conjoin(_,_,_).
*/
