% Added testing_case flag.
:- module(preprocess_flags_,[
	push_pp_flag/2,
	pop_pp_flag/1,
	current_pp_flag/2,
	set_pp_flag/2,
	pp_flag/2,
	pp_flag/1,            % it is a type
	valid_flag_value/2,   % (Menu uses them)
	valid_flag_values/2,  % (Menu uses them)
	dump_flags/1,
	save_config/1,
	restore_config/1,
	remove_config/1,
	show_config/1,
	show_configs/0
	],[assertions,regtypes,isomodes,persdb]).

%%------------------------------------------------------------------------

:- doc(bug,"Having changed the names of flags, the rest of ciaopp
	should also be changed to use the new names: current solution
	is a kludge.").

%%------------------------------------------------------------------------

:- use_module(library(lists), [sublist/2]).
:- use_module(library(aggregates)).                % for dump_flags
:- use_module(library(write)).


:- trust pred persistent_dir( Key , Dir ) => atm * atm.
:- trust pred persistent_dir( Key , Dir ) :  atm * var.
:- trust pred persistent_dir( Key , Dir ) :  atm * atm.

persistent_dir(dbdir, '~/.ciao.d/ciaopp_flags').

:- persistent( config/2 , dbdir ).



:- trust pred config( A , B ) => atm * list.
:- trust pred config( A , B ) :  atm * list.
:- trust pred config( A , B ) :  atm * var.
:- trust pred config( A , B ) :  var * var.

:- pred current_pp_flags(Name,Value) 
	: pp_flag * flag_value
	# "Current proprocess flags. These facts can be dynamically updated 
           using @tt{set_pp_flag/2}. Initial facts are flag values by 
           default.".

:- data current_pp_flags/2.


:- discontiguous pp_flag/2.
:- discontiguous current_pp_flags/2.
:- discontiguous valid_flag_values/2.


%:- multifile pp_flag/2 , 
%	     pp_flag/1,
%	     current_pp_flags/2,
%	     valid_flag_values/2.



pp_flag(           dump_pred  , 
 'Whether to include predicate information in .dump files.' ). 
current_pp_flags(  dump_pred  , all ).
valid_flag_values( dump_pred  , member( _  , [all, nodep, off  ] ) ).

pp_flag(           dump_pp    , 'No Help Available' ).
current_pp_flags(  dump_pp    , all ).
valid_flag_values( dump_pp    , member( _  , [all, deps , off  ] ) ).

pp_flag(           dump_ext   , 'No Help Available' ).
current_pp_flags(  dump_ext   , all ).
valid_flag_values( dump_ext   , member( _  , [all, int  , iter ] ) ).

pp_flag(           inter_all          , 'No Help Available' ).
current_pp_flags(  inter_all          , analyze ).
valid_flag_values( inter_all          , member( _ , 
	                                        [
						 analyze, 
						 check_assertions, 
						 optimize] ) ).

pp_flag(           inter_ana          , 'No Help Available' ).
current_pp_flags(  inter_ana          , [types,modes,ana_nf,ana_det,ana_cost]   ).
valid_flag_values( inter_ana          , sublist2( _ , [
						       modes,
						       types,
						       ana_nf,
						       ana_det,
						       ana_cost
						     ] ) ).

pp_flag(           inter_optimize     , 'No Help Available' ).
current_pp_flags(  inter_optimize     , spec ).
valid_flag_values( inter_optimize     , member( _ , [none,spec,parallelize] ) ).

pp_flag(           inter_parallelize  , 'No Help Available' ).
current_pp_flags(  inter_parallelize  , [none] ).
valid_flag_values( inter_parallelize  , sublist2( _ , [none, para_ann,
	                                               para_iap, para_grain] ) ).

pp_flag(           para_ann           , 'No Help Available' ).
current_pp_flags(  para_ann           , none ).
valid_flag_values( para_ann           , member( _ , [none, mel, cdg, udg,
	                                             urlp, crlp] )).

pp_flag(           para_iap           , 'No Help Available' ).
current_pp_flags(  para_iap           , none ).
valid_flag_values( para_iap           , member( _ , [none,pre, post , both] )).

pp_flag(           para_grain         , 'No Help Available' ).
current_pp_flags(  para_grain         , none ).
valid_flag_values( para_grain         , member( _ , [none, gr] )).


%
% paralization flags for auto_inteface 
%


pp_flag(           menu_level         , 'Wether use naive or expert menu' ).
current_pp_flags(  menu_level         , naive ).
valid_flag_values( menu_level         , member( _  , [naive, expert] ) ).


pp_flag(           menu_last_config   , 'Last menu stored configuration used' ).
current_pp_flags(  menu_last_config   , none ).
valid_flag_values( menu_last_config   , is_menu_config( _ ) ).


pp_flag(           menu_config_name   , 'Name of the last menu stored configuration' ).
current_pp_flags(  menu_config_name   , none ).
valid_flag_values( menu_config_name   , atom( _ ) ).


pp_flag(           check_config_ana   , 
	                   'Decides where or not to configure analysis flags' ).
current_pp_flags(  check_config_ana   , off    ).
valid_flag_values( check_config_ana   , member( _ , [on,off] ) ).

pp_flag(           modes              , 'No Help Available' ).
current_pp_flags(  modes              , shfr    ).
valid_flag_values( modes              , modeanalysis( _ ) ).

pp_flag(           types              , 'No Help Available' ).
current_pp_flags(  types              , eterms  ).
valid_flag_values( types              , typeanalysis( _ ) ).

pp_flag(           ana_nf             , 'Type of non-failure analysis: monovariant (nfg), or multi-variant (nf)' ).
current_pp_flags(  ana_nf             , none    ).
valid_flag_values( ana_nf             , member( _ , [none, nf, nfg ] )).

pp_flag(           ana_det            , 'Type of determinacy analysis: multi-variant (det)' ).
current_pp_flags(  ana_det            , none    ).
valid_flag_values( ana_det            , member( _ , [none, det] )).

pp_flag(           ana_cost           , 'Type of cost (and size) analysis: lower bounds (steps_lb), upper bounds (steps_ub), or both (steps_ualb)' ).
current_pp_flags(  ana_cost           , none    ).
valid_flag_values( ana_cost           , member( _ , [none, steps_ub,
	                                             steps_lb, steps_ualb] )).

pp_flag(           ana_size           , 'Type of size analysis: lower bounds (size_lb), upper bounds (size_ub), or both (size_ualb)' ).
current_pp_flags(  ana_size           , none    ).
valid_flag_values( ana_size           , member( _ , [none, size_ub,
	                                             size_lb, size_ualb] )).

pp_flag(           peval_ana          , 'No Help Available' ).
current_pp_flags(  peval_ana          , pd     ).
valid_flag_values( peval_ana          , modetypeanalysis( _ ) ).

pp_flag(           spec_poly          , 'No Help Available' ).
current_pp_flags(  spec_poly          , off    ).
valid_flag_values( spec_poly          , member( _ , [off,mono,poly] ) ).

pp_flag(           assert_ctcheck     , 'Decides wether to run compile time checks' ).
current_pp_flags(  assert_ctcheck     , off ).
valid_flag_values( assert_ctcheck     , member( _ , [on, off] ) ).

pp_flag(           testing_ctchecks   , 'when true add special clauses to the program to allow to perform automatic test case testing.' ).
current_pp_flags(  testing_ctchecks   , false ).
valid_flag_values( testing_ctchecks   , member( _ , [true, false] ) ).

pp_flag(           assert_rtcheck     , 'Decides wether to run compile time rtchecks' ).
current_pp_flags(  assert_rtcheck     , none ).
valid_flag_values( assert_rtcheck     , member( _ , [none, pred, pp_assrt, pp_code])).


pp_flag(           collapse_ai_vers   , 
 'To output all the versions of call/success patterns inferred by analysis or \
just one version (summing-up all of them).' ).
current_pp_flags(  collapse_ai_vers  , on ).
valid_flag_values( collapse_ai_vers  , member(_,[off,on]) ).

pp_flag(           cost_approximation , 'No Help Available' ).
current_pp_flags(  cost_approximation , upper ).
valid_flag_values( cost_approximation , member(_,[lower,upper]) ).

pp_flag(           dbdebug            , 'No Help Available' ).
current_pp_flags(  dbdebug            , off ).
valid_flag_values( dbdebug            , member(_,[off, on]) ).

% *** BE CAREFUL! you cannot do a findall( X, valid_flag_value(opt_unf_depth,X), L)!!!
pp_flag(           depth              , 'No Help Available' ).
current_pp_flags(  depth              , 1).
valid_flag_values( depth              , nnegint(_)).

pp_flag(           unf_depth          , 'The maximum depth of abstractions \
                                         in analyses based on term depth' ).
current_pp_flags(  unf_depth          ,1 ).
valid_flag_values( unf_depth          , nnegint(_) ).

pp_flag(           dump_ai            ,
'Decides wether to print analisys informaion about predicates (aka true assertions). If it is off dump_ai has no effect.' ).
current_pp_flags(  dump_ai            , on ).
valid_flag_values( dump_ai            , member(_,[off,on])).

pp_flag(           error_log          , 
 'Store error messages in a .err file for the module being preprocessed.' ).
current_pp_flags(  error_log          , off ).
valid_flag_values( error_log          , member(_,[off, on])).

pp_flag(           fixpoint           , 'No Help Available' ).
current_pp_flags(  fixpoint           , plai).
valid_flag_values( fixpoint           , member(_,[plai, dd, di, check_di,
                                                  check_di2, check_di3, 
                                                  check_di4, check_di5]) ).

pp_flag(           granularity_threshold, 
 'The threshold on computational cost at which parallel execution pays off' ).
current_pp_flags(  granularity_threshold, 959).
valid_flag_values( granularity_threshold, nnegint(_)).

pp_flag(           multi_success      , 'Whether to allow multivariance \
	                                 on successes' ).
current_pp_flags(  multi_success      , off).
valid_flag_values( multi_success      , member(_,[off,on])).

pp_flag(           vers               , 'Whether to show multiple versions \
	                                 in analysis results' ).
current_pp_flags(  vers               , off ).
valid_flag_values( vers               , member(_,[off,on])).


pp_flag(           pp_info            , 
 'Whether to output analysis information for program points within clauses' ).
current_pp_flags(  pp_info            , off).
valid_flag_values( pp_info            , member(_,[off,on])).

pp_flag(           typedefs_ai        , 'No Help Available' ).
current_pp_flags(  typedefs_ai        , pred).
valid_flag_values( typedefs_ai        , member(_,[rule,pred])).

pp_flag(           typedefs_simp      , 'No Help Available' ).
current_pp_flags(  typedefs_simp      , on).
valid_flag_values( typedefs_simp      , member(_,[off,on])).


pp_flag(           widen              , 'Whether to perform widening' ).
current_pp_flags(  widen              , off).
valid_flag_values( widen              , member(_,[off,on])).

pp_flag(           reuse_fixp_id      , 'No Help Available' ).
current_pp_flags(  reuse_fixp_id      , off).
valid_flag_values( reuse_fixp_id      , member(_,[off,on])).

%% Intermodular analysis activator
pp_flag(           intermod     ,
 'Whether to apply inter-modular analysis techniques, \
such as recovering previous analysis info from related modules' ).
current_pp_flags(  intermod     , off).
valid_flag_values( intermod     , member(_, [off,on,auto])).

%% How success policy is applied.
pp_flag(           success_policy     ,
 'The policy for obtaining success information \
for imported predicates during inter-modular analysis' ).
current_pp_flags(  success_policy     , all).
valid_flag_values( success_policy     , member(_, [best, first, all, top,
	                                           botfirst, botbest, botall, 
						   bottom])). 

%% How the initial guess  is applied.
pp_flag(           initial_guess     ,
 'The policy for obtaining initial guess when computing the  \
analysis of a predicate from the current module' ).
current_pp_flags(  initial_guess     , bottom).
valid_flag_values( initial_guess     , member(_, [botfirst, botbest, botall, 
						   bottom])). 

pp_flag(           entry_policy       , 
 'The policy for obtaining entry call patterns \
for exported predicates during inter-modular analysis' ).
current_pp_flags(  entry_policy       , all).
valid_flag_values( entry_policy       , member(_,[all,top_level,force])). 

pp_flag(           type_eval          , 
 'Whether to attempt concrete evaluation of types being inferred' ).
current_pp_flags(  type_eval          , off).
valid_flag_values( type_eval          , member(_,[on,off])).

pp_flag(           type_precision     , 
 'To use during type analysis only types defined at visible modules or \
also types inferred anew' ).
current_pp_flags(  type_precision     , all).
valid_flag_values( type_precision     , member(_,[defined,all])).

pp_flag(           type_output        , 
 'To output the types inferred for predicates in \
terms only of types defined at visible modules or including types inferred anew' ).
current_pp_flags(  type_output        , all).
valid_flag_values( type_output        , member(_,[defined,all])).


pp_flag(           local_control      , 'The kind of unfolding performed' ).
current_pp_flags(  local_control      , off).
valid_flag_values( local_control      , member(_,[off, orig, inst, det, det_la,
	                                          depth, first_sol,first_sol_d,
						  all_sol, hom_emb,hom_emb_anc,
						  hom_emb_as, df_hom_emb_as, 
						  df_tree_hom_emb, df_hom_emb])).

pp_flag(           comp_rule   , 'The computation rule for the selection of atoms in a goal' ).
current_pp_flags(  comp_rule   , leftmost).
valid_flag_values( comp_rule   , member(_,[leftmost,local_builtin,local_emb]) ).


pp_flag(           global_control     , 'The kind of global control performed' ).
current_pp_flags(  global_control     , hom_emb).
valid_flag_values( global_control     , member(_,[off,id,id_abs,inst,hom_emb])).

pp_flag(           rem_use_cls        , 'Remove useless clauses' ).
current_pp_flags(  rem_use_cls        , off).
valid_flag_values( rem_use_cls        , member(_,[off, pre, post, both])).

pp_flag(           abs_exec           , 'Perform abstract executability' ).
current_pp_flags(  abs_exec           , off).
valid_flag_values( abs_exec           , member(_,[off, on])).

pp_flag(           part_conc          , 
'The kind of partial concretization to be performed' ).
current_pp_flags(  part_conc          , off).
valid_flag_values( part_conc          , member(_,[off,mono,multi])).

pp_flag(           filter_nums        , 
'Whether to filter away numbers in partial evaluation' ).
current_pp_flags(  filter_nums        , off).
valid_flag_values( filter_nums        , member(_,[off,safe,on])).

pp_flag(           exec_unif          , 'No Help Available' ).
current_pp_flags(  exec_unif          , on).
valid_flag_values( exec_unif          , member(_,[off,on])).

pp_flag(           pres_inf_fail          , 'Whether Infinite Failure should be preserved in the specialized program' ).
current_pp_flags(  pres_inf_fail          , off).
valid_flag_values( pres_inf_fail          , member(_,[off,on])).

pp_flag(           inter_opt_arg_filt , 'No Help Available' ).
current_pp_flags(  inter_opt_arg_filt , off).
valid_flag_values( inter_opt_arg_filt , member(_,[off,on])).

pp_flag(           normalize          , 'No Help Available' ).
current_pp_flags(  normalize          , off).
valid_flag_values( normalize          , member(_,[off,on])).

pp_flag(           global_scheduling  , 
 'Global scheduling policy to be used in intermodular analysis' ).
current_pp_flags(  global_scheduling  , depth_first).
valid_flag_values( global_scheduling  , member(_,[depth_first,
	abs_depth_first, naive_top_down, naive_bottom_up])).

pp_flag(           widencall          , 'No Help Available' ).
current_pp_flags(  widencall          , com_child).
valid_flag_values( widencall          , member(_,[com_child,onlyfixp,off])).

pp_flag(           variants           , 'No Help Available' ).
current_pp_flags(  variants           , off).
valid_flag_values( variants           , member(_,[off,on])).

pp_flag(           tmp_dir            , 
'Directory for temporary files, or \'source\' if temporary files 
are to be stored where source files reside.' ).
current_pp_flags(  tmp_dir            , source).
valid_flag_values( tmp_dir            , tmp_dir( _ ) ).

pp_flag(           asr_dir            , 
'Directory for asr files, or \'source\' if asr files 
are to be stored where source files reside.' ).
current_pp_flags(  asr_dir            , source ).
valid_flag_values( asr_dir            , asr_dir( _ ) ).

pp_flag(           process_libraries  , 
'Whether to process libraries or not during intermodular \
analysis / specialization.' ).
current_pp_flags(  process_libraries  , off).
valid_flag_values( process_libraries  , member(_,[off,on]) ).

pp_flag(           ass_not_stat_eval  , 'When runing compile time checks, \
some assertions cannot be marked as checked or false. This flag decides \
what do do in those situations: nothing, report a warning or an error' ).
current_pp_flags(  ass_not_stat_eval  , off ).
valid_flag_values( ass_not_stat_eval  , member(_,[off,warning,error])).

pp_flag(           verbose_ctchecks  , 'When this flag is set to on, \
all check assertions which are verified or falsified are printed' ).
current_pp_flags(  verbose_ctchecks  , off ).
valid_flag_values( verbose_ctchecks  , member(_,[off,on])).

pp_flag(           pred_ctchecks  , 'This flags controls whether, \
to perform predicate level compile-time checking and the algorithm to use' ).
current_pp_flags(  pred_ctchecks  , new ).
valid_flag_values( pred_ctchecks  , member(_,[none,old,new,new_all,new_succ,new_all_succ])).

pp_flag(           pp_ctchecks  , 'This flags controls whether, \
to perform program point compile-time checking and the algorithm to use' ).
current_pp_flags(  pp_ctchecks  , old ).
valid_flag_values( pp_ctchecks  , member(_,[none,old,new])).



tmp_dir(source).
tmp_dir(Dir) :-
	atm(Dir).

asr_dir(source).
asr_dir(Dir) :-
	atm(Dir).


% the following three should be put in common with infer_dom:knows_of/2
% and multifile:analysis/1
modetypeanalysis(X):- modeanalysis(X).
modetypeanalysis(X):- typeanalysis(X), X \== none.


% to allow no mode analysis
modeanalysis(none).

% groundness/sharing
modeanalysis(pd).
modeanalysis(pdb).
modeanalysis(def).
modeanalysis(gr).
modeanalysis(share).
modeanalysis(shareson).
modeanalysis(shfr).
modeanalysis(shfrson).
modeanalysis(shfrnv).
modeanalysis(son).
% structure
modeanalysis(aeq).
modeanalysis(depth).
modeanalysis(path).
% constraints
modeanalysis(difflsign).
modeanalysis(fr).
modeanalysis(frdef).
modeanalysis(lsign).
% types
% see typeanalysis(X).

typeanalysis(none).
typeanalysis(eterms).
typeanalysis(ptypes).
typeanalysis(svterms).
typeanalysis(terms).



sublist2( X , [L] ) :-
	var( X ), !,
	member( X , L ).

sublist2( X , L ) :-
	sublist( X , L ).


the_same_as( X , P ) :-
	valid_flag_value( P , X ).



%%------------------------------------------------------------------------
:- pred pp_flag(Name,Help) 
	# "@var{Name} is a valid preprocess flag.@var{Help} is a
          description of what @var{Name} does.".

:- set_prolog_flag( multi_arity_warnings , off ).

%%------------------------------------------------------------------------

:- regtype pp_flag( Flag )
	# "@var{Flag} is a valid preprocessor flag.".

:- entry pp_flag( Flag ) : var.
:- entry pp_flag( Flag ) : atm.

pp_flag( dump_pred ).
pp_flag( dump_pp ).
pp_flag( dump_ext ).
pp_flag( inter_all ).
pp_flag( inter_ana ).
pp_flag( inter_optimize ).
pp_flag( inter_parallelize ).
pp_flag( para_ann ).
pp_flag( para_iap ).
pp_flag( para_grain ).
pp_flag( menu_level ).
pp_flag( check_config_ana ).
pp_flag( modes ).
pp_flag( types ).
pp_flag( ana_nf ).
pp_flag( ana_det ).
pp_flag( ana_cost ).
pp_flag( peval_ana ).
pp_flag( spec_poly ).
pp_flag( assert_ctcheck ).
pp_flag( testing_ctchecks ).
pp_flag( assert_rtcheck ).
pp_flag( collapse_ai_vers ).
pp_flag( cost_approximation ).
pp_flag( dbdebug ).
pp_flag( depth ).
pp_flag( unf_depth ).
pp_flag( dump_ai ).
pp_flag( error_log ).
pp_flag( fixpoint ).
pp_flag( granularity_threshold ).
pp_flag( multi_success ).
pp_flag( pp_info ).
pp_flag( typedefs_ai ).
pp_flag( typedefs_simp ).
pp_flag( widen ).
pp_flag( reuse_fixp_id ).
pp_flag( intermod ).
pp_flag( success_policy ).
pp_flag( initial_guess ).
pp_flag( entry_policy ).
pp_flag( type_eval ).
pp_flag( type_precision ).
pp_flag( type_output ).
pp_flag( local_control ).
pp_flag( comp_rule ).
pp_flag( global_control ).
pp_flag( rem_use_cls ).
pp_flag( abs_exec ).
pp_flag( part_conc ).
pp_flag( filter_nums ).
pp_flag( exec_unif ).
pp_flag( pres_inf_fail ).
pp_flag( inter_opt_arg_filt ).
pp_flag( normalize ).
pp_flag( global_scheduling ).
pp_flag( widencall ).
pp_flag( variants ).
pp_flag( tmp_dir ).
pp_flag( asr_dir ).
pp_flag( process_libraries ).
pp_flag( ass_not_stat_eval ).


:- set_prolog_flag( multi_arity_warnings , on ).


%%------------------------------------------------------------------------
:- regtype flag_value(V)  
	# "@var{V} is a value for a flag.".

flag_value( X ) :- atm( X ).
flag_value( X ) :- list( X , atm ).


%%------------------------------------------------------------------------
:- entry valid_flag_values( Name , Value ) : atm * var.

:- pred valid_flag_values(Name,Value_Checker) 
	: pp_flag * callable
	# "@var{Value_Checker} is a goal that checks that a value given as
           first argument of this term is a valid value for @var{Name}.".

:- meta_predicate valid_flag_values(?,goal).

%------------------------------------------------------------------------
:- pred current_pp_flag(-Name,-Value) 
	: pp_flag * flag_value
	# "preprocess flag @var{Name} has the value @var{Value}.".

current_pp_flag(analysis_info,Value):- !, current_pp_flag(dump_ai,Value).
current_pp_flag(point_info,Value):- !, current_pp_flag(pp_info,Value).
current_pp_flag(part_concrete,Value):- !, current_pp_flag(part_conc,Value).
%
current_pp_flag(Name,Value):-
	current_pp_flags(Name,Value).

%------------------------------------------------------------------------
:- pred set_pp_flag(+Name,+Value) 
	: pp_flag * flag_value
	# "Sets the @var{Value} for preprocess flag @var{Name}.".

set_pp_flag(analysis_info,Value):- !, set_pp_flag(dump_ai,Value).
set_pp_flag(point_info,Value):- !, set_pp_flag(pp_info,Value).
set_pp_flag(part_concrete,Value):- !, set_pp_flag(part_conc,Value).
%
set_pp_flag(Name,Value):-
	ground(Name),
	ground(Value),
	pp_flag(Name,_),
	valid_flag_value(Name,Value),!,  % checking name and value existence.
	data_facts:retract_fact(current_pp_flags(Name,_)),
	data_facts:assertz_fact(current_pp_flags(Name,Value)).

%%------------------------------------------------------------------------
:- entry valid_flag_value( Name , Value ) : atm * var.
:- entry valid_flag_value( Name , Value ) : atm * flag_value.

:- pred valid_flag_value(Name,Value) 
	: pp_flag * flag_value
	# "@var{Value} is a valid value for @var{Name} preprocess flag.".

% more kludges
valid_flag_value(analysis_info,Value):- 
	!,
	valid_flag_value(dump_ai,Value).

valid_flag_value(Name,Value):-
	valid_flag_values(Name,ValGen),
	arg(1,ValGen,Value),           
	call(ValGen).


:- data old_flag/2.

:- doc(push_pp_flag(Flag, NewValue), "Same as
   @pred{set_pp_flag/2}, but storing current value of @var{Flag} to
   restore it with @pred{pop_pp_flag/1}.").

:- entry push_pp_flag( Flag , Value ) : atm * flag_value.

:- true pred push_pp_flag(+atm, +term).

push_pp_flag(analysis_info,Value):- !, push_pp_flag(dump_ai,Value).
push_pp_flag(point_info,Value):- !, push_pp_flag(pp_info,Value).
push_pp_flag(part_concrete,Value):- !, push_pp_flag(part_conc,Value).
%
push_pp_flag(Flag, NewValue) :-
        nonvar(Flag),
        current_pp_flag(Flag, OldValue),
	set_pp_flag(Flag,NewValue),
        data_facts:asserta_fact(old_flag(Flag, OldValue)).

:- doc(pop_pp_flag(Flag), "Restore the value of @var{Flag}
   previous to the last non-canceled @pred{push_pp_flag/2} on it.").


:- entry pop_pp_flag( Flag ) : atm.

:- true pred pop_pp_flag(+atm).

pop_pp_flag(analysis_info):- !, pop_pp_flag(dump_ai).
pop_pp_flag(point_info):- !, pop_pp_flag(pp_info).
pop_pp_flag(part_concrete):- !, pop_pp_flag(part_conc).
%
pop_pp_flag(Flag) :-
        nonvar(Flag),
        data_facts:retract_fact(old_flag(Flag, OldValue)),
        set_pp_flag(Flag,OldValue).

%-------------------------------------------------------------------------

:- multifile dump_flags_list/2.

:- entry dump_flags( Flag ) : atm.

:- pred dump_flags(Name) 
	: atm

	# "@var{Name} represent the list of flags to be dumped. To
          associate a name (key) with a list, use
          @pred{dump_flags_list}.".


dump_flags( Name ) :-
	dump_flags_list( Name , List ),
	dump_all_flags( List ).

dump_flags( Name ) :-
	message( error , ['Flag list ' , Name , ' unkown']).



dump_flags_list( all , L ) :-
	findall( X , pp_flag(X,_), L ).



dump_all_flags( [A|B] ) :-
	current_pp_flag( A , V ),
	!,
	display( A ) , display( ' = ' ) , display( V ) , nl ,
	dump_all_flags( B ).

dump_all_flags( [A|B] ) :-
	display( 'unkown flag ' ) , display( A ), nl,
	dump_all_flags( B ).
	
dump_all_flags( [] ).


%%------------------------------------------------------------------------

:- use_module(menu_generator_, [get_menu_configs/1]).

is_menu_config( V ) :-
	get_menu_configs( X ),
	member( V , [ none | X ] ).

%%------------------------------------------------------------------------


:- entry save_config( Flag ) : atm.

:- pred save_config( Name ) 
	: atm
	# "Save the current flags configuration under the @var{Name} key.".

save_config( Name ) :-
	findall( (A=B) , current_pp_flags( A , B ) , L ),
	save_flags_list( Name , L ).

save_flags_list( Name , List ) :-
	( persdbrt:retract_fact( config( Name , _ ) ) , fail ; true ),
	persdbrt:assertz_fact( config( Name , List ) ).


:- entry remove_config( Flag ) : atm.

:- pred remove_config( Name ) 
	: atm
	# "Remove the configuration stored with the @var{Name} key.".


remove_config( Name ) :-
	persdbrt:retract_fact( config( Name , _ ) ),
	fail.

remove_config( _Name ).



restore_config( Name ) :-
	config( Name , L ),
	restore_flags_list( L ).

%What happends with non existing flags?
restore_flags_list( [] ) :- !.
restore_flags_list( [(A=B)|As] ) :-
	(set_pp_flag(A,B)->true;true),
	 restore_flags_list( As ).

:- pred show_configs 
	# "Show all stored configs.".


show_configs :-
	findall( Name , config( Name , _ ) , L ),
	display( L ), nl.


:- entry show_config( Flag ) : atm.

:- pred show_config( C )
	: atm
	# "Show specific configuration values pointed by @var{C} key.".
	

show_config( Name ) :-
	config( Name , F ),
	show_config_list( F ),
	fail.

show_config( _ ).


show_config_list( [] ) :- !.
show_config_list( [A|B] ) :-
	write( A ), nl,
	show_config_list( B ).

%%------------------------------------------------------------------------
%% VERSION CONTROL
%%------------------------------------------------------------------------

:- doc(version_maintenance,dir(version)).

:- doc(version(1*0+875,2004/11/15,16:27*18+'CET'), "Added
   naive_top_down, naive_bottom_up values to global_scheduling
   preprocess flag.  (Jesus Correas Fernandez)").

:- doc(version(1*0+848,2004/11/11,12:42*43+'CET'), "Added new
   values for the pred_ctchecks flag: new_all - for all domains,
   new_succ - for improved success assertion checking, new_all_succ -
   for both (Pawel Pietrzak)").

:- doc(version(1*0+831,2004/11/08,21:05*12+'CET'), "Added flags
   @tt{pred_ctchecks} and @tt{pp_ctchecks} which allow having several
   algorithms for compile-time checking available and also turn off
   either of them independently.  (German Puebla)").

:- doc(version(1*0+830,2004/11/08,21:03*53+'CET'), "For
   homogeneity, now the values for the flag check_config_ana is on/off
   instead of yes/no.  (German Puebla) (German Puebla)").

:- doc(version(1*0+817,2004/11/06,00:09*37+'CET'), "Added flag
   @tt{verbose_ctchecks}. When this flag is set to on, all check
   assertions which are verified or falsified are printed.  (German
   Puebla)").

:- doc(version(1*0+787,2004/10/27,18:39*37+'UTC'), "Added value
   @tt{df_tree_hom_emb} for flag @tt{local_control}.  (German
   Puebla)").

:- doc(version(1*0+768,2004/10/20,12:28*45+'CEST'), "Added flag
   @tt{hom_emb_anc} within local control in order to select an
   unfolding rule based on embedding which does not use ancestor
   stacks for keeping track of the ancestor information.
    (Elvira Albert)").

:- doc(version(1*0+743,2004/10/17,23:12*34+'UTC'), "The old flag
   @tt{lub} and its synonym @tt{multivariance} have been replaced by
   flag @tt{multi_success}.  (German Puebla)").

:- doc(version(1*0+736,2004/10/13,02:49*18+'CEST'), "Modified
   inter_ana flag to execute nf and cost analysis.  (David Trallero
   Mena)").

:- doc(version(1*0+731,2004/10/11,21:24*35+'CEST'), "Added a new
   flag (ana_size) to control the type of size analysis: lower bounds
   (size_lb), upper bounds (size_ub), or both (size_ualb). (Pedro
   Lopez Garcia)").

:- doc(version(1*0+708,2004/10/08,17:11*11+'CEST'), "Added flag
   @tt{df_hom_emb_as} to select the depth-first implementation of
   local unfolding with ancestor stacks. The breadth-first
   implementation is now @tt{hom_emb_as}.  (Elvira Albert)").

:- doc(version(1*0+701,2004/10/08,13:11*14+'CEST'), "Added help
   comment for non-failure, determinacy, and cost (and size) analysis
   related flags.  (Pedro Lopez Garcia)").
 
:- doc(version(1*0+693,2004/10/07,18:03*50+'CEST'), "Changed
   success_policy flag value 'bot' to 'bottom' (to make it equal to
   initial_guess values).  (Jesus Correas)").

:- doc(version(1*0+692,2004/10/07,18:03*33+'CEST'), "Added
   initial_guess flag to indicate how to obtain the initial guess when
   performing the analysis of a predicate of the current module.
   (Jesus Correas)").

:- doc(version(1*0+690,2004/10/04,18:30*23+'CEST'), " Added flag
   @tt{comp_rule} for choosing the computation rule to select the
   atoms to be unfolded in the local control of partial evaluation.
   (Elvira Albert)").

:- doc(version(1*0+678,2004/10/01,12:41*28+'CEST'), "Valid flag
   values for ana_cost changed to none, steps_ub, steps_lb, and
   STEPS_UALB (instead of lower, upper and both). (Pedro Lopez Garcia)").

:- doc(version(1*0+673,2004/09/28,16:25*16+'CEST'), "Added flag
   @tt{pres_inf_fail} which controls whether finite failure should be
   preserved during specialization or not.  (German Puebla)").

:- doc(version(1*0+662,2004/09/21,14:23*14+'CEST'), "Added flags:
   menu_last_config and menu_config_name. Needed for saving menu
   configurations in auto_interface.  (David Trallero Mena)").

:- doc(version(1*0+659,2004/09/21,12:33*11+'CEST'), "The
   @pred{modetypeanalysis/1} returned none twice. Fixed.  (David
   Trallero Mena)").

:- doc(version(1*0+651,2004/09/20,20:32*25+'CEST'), "Added help to
   dump_ai flag.  (David Trallero Mena)").

:- doc(version(1*0+641,2004/09/15,01:45*19+'CEST'), "Added entry
   in @pred{valid_flag_value/2} to analysis_info.  (David Trallero
   Mena)").

:- doc(version(1*0+637,2004/09/15,00:29*32+'CEST'), "Added
   @pred{pp_flag} regtype definition.  (David Trallero Mena)").

:- doc(version(1*0+636,2004/09/15,00:28*58+'CEST'), "To be
   coherent , assert_ctchecks is on off instead of none yes.  (David
   Trallero Mena)").

:- doc(version(1*0+635,2004/09/15,00:28*37+'CEST'), "Typo in
   ass_not_stat_eval options: erro -> error.  (David Trallero Mena)").

:- doc(version(1*0+545,2004/07/15,11:22*14+'CEST'), "Added
   @code{asr_dir} flag to set the directory where asr files are to be
   stored. This flag can be either @code{source} to indicate that the
   asr files are stored in the same directory where the source file is
   located, or an atom with a valid directory specification.  (Jesus
   Correas Fernandez)").

:- doc(version(1*0+534,2004/07/07,13:41*41+'CEST'), "Added
   @tt{vers} flag to do transform(vers) at the end of analysis in
   auto_analyze.  (David Trallero Mena)").

:- doc(version(1*0+529,2004/07/07,12:01*11+'CEST'), "'fixpoint'
   option 'di_mod' removed. Now intermodular analysis is integrated
   into 'di' fixpoint.  (Jesus Correas Fernandez)").

:- doc(version(1*0+520,2004/07/05,12:14*04+'UTC'), "Added the flag
   @tt{abs_exec} which controls whether abstract executability should
   be performed directly during local control rather than as a post
   processing phase.  (German Puebla)").

:- doc(version(1*0+519,2004/07/05,12:12*53+'UTC'), "Added the
   value @{id_abs} for flag @tt{global_control}. It differs from
   @tt{id} in that not only the concrete part but also the abstract
   part has to be equivalent in order to reuse a specialized
   definition.  (German Puebla)").

:- doc(version(1*0+518,2004/07/05,12:11*34+'UTC'), "The value
   @{hom_emb_l} for @tt{local_control} has been globally renamed to
   @tt{hom_em_as} which stands for homeomorphic embedding with
   ancestor stacks.  (German Puebla)").

:- doc(version(1*0+517,2004/07/05,12:09*50+'UTC'), "Added flag
   @tt{rem_use_cls} which controls whether removal of useless clauses,
   i.e., those incompatible with the abstract call pattern, should be
   performed during local control.  (German Puebla)").

:- doc(version(1*0+478,2004/06/17,17:27*18+'CEST'), "@tt{intermod}
   flag reintroduced to enable/disable intermodular analysis.  (Jesus
   Correas Fernandez)").

:- doc(version(1*0+474,2004/05/23,21:49*31+'CEST'), "in ':- pred'
   callable and not goal have to be used. Also some typed corrected
   (David Trallero Mena)").

:- doc(version(1*0+465,2004/05/10,17:05*46+'CEST'), "inter_all has
   no \"none\" option now (MH order) (David Trallero Mena)").

:- doc(version(1*0+461,2004/05/03,19:49*15+'CEST'), "menu_level
   flag added (David Trallero Mena)").

:- doc(version(1*0+459,2004/05/02,12:39*41+'ART'), "flag value
   'user' for type_output and type_precision flags changed by
   'defined' (Claudio Vaucheret)").

:- doc(version(1*0+455,2004/04/30,20:22*13+'CEST'),
   "ass_not_eval_flag changed to off, warning, error (David Trallero
   Mena)").

:- doc(version(1*0+454,2004/04/30,19:02*53+'CEST'), "The flag
   @tt{part_conc} now has 3 possible values.  (German Puebla)").

:- doc(version(1*0+446,2004/04/28,11:37*43+'CEST'), "The default
   value for the @tt{ass_not_stat_eval} flag is now @tt{ignore}.
   (German Puebla)").

:- doc(version(1*0+437,2004/04/22,19:53*55+'CEST'),
   "ass_not_stat_eval flag added to define ctchecks behaviour on none
   checked or false assertions (David Trallero Mena)").

:- doc(version(1*0+427,2004/04/19,14:14*04+'CEST'), "Added
   @tt{hom_emb_l} as new value for @tt{local_control}. It is also the
   default value.  (German Puebla)").

:- doc(version(1*0+404,2004/03/31,15:20*17+'CEST'), "Added
   process_libraries flag.  (Jesus Correas Fernandez)").

:- doc(version(1*0+403,2004/03/31,13:19*50+'CEST'), "Added tmp_dir
   flag.  (Jesus Correas Fernandez)").

:- doc(version(1*0+380,2004/03/23,15:44*43+'CET'), "Added
   qualifications to _fact calls to avoid warnings.  (Manuel
   Hermenegildo)").

:- doc(version(1*0+379,2004/03/23,15:37*20+'CET'), "Flag
   entry_policy documented.  (Jesus Correas Fernandez)").

:- doc(version(1*0+372,2004/03/15,18:24*23+'CET'), "added
   save_config and restore_config (David Trallero Mena)").

:- doc(version(1*0+362,2004/03/02,16:00*51+'CET'), "argument added
   to pp_flag for documentation.  (David Trallero Mena)").

:- doc(version(1*0+335,2004/02/23,17:51*58+'CET'), "added 'off'
   value to widencall flag (Claudio Vaucheret)").

:- doc(version(1*0+328,2004/02/20,18:38*20+'CET'), "Flags
   dump_level , dump_level_ext removed.  (German Puebla)").

:- doc(version(1*0+328,2004/02/20,18:30*04+'CET'), "dump_pred,
   dump_pp and dump_ext added. (David Trallero Mena)").

:- doc(version(1*0+314,2004/02/11,19:52*11+'CET'), "dump_flags
   added. It is usefull because you can print the value of the
   different flags. You also can create your own list using
   dump_flags_list multipredicate (David Trallero Mena)").

:- doc(version(1*0+306,2004/02/06,16:57*04+'CET'), "Added flag
   @tt{exec_unif} which selects whether the unifications generated by
   abstract specialization should be executed at specialization time
   or just added to the specialized program.  (German Puebla)").

:- doc(version(1*0+271,2004/01/31,17:32*40+'CET'), "added two new
   flags, @tt{widencall} and @{variants}. The first one chooses which
   policy is used to get two consecutive aproximations to be widened
   at call time. The second decides whether two abstractions can be
   identical although the goals are not variants (Claudio Vaucheret)").

:- doc(version(1*0+261,2004/01/29,18:34*19+'CET'), "'intermod'
   preprocess has been removed as it is no longer needed.  (Jesus
   Correas Fernandez)").

:- doc(version(1*0+254,2004/01/29,15:35*35+'CET'), "elimiated
   di_parents flag, we are using widen flag instead (Claudio
   Vaucheret)").

:- doc(version(1*0+249,2004/01/27,20:25*22+'CET'), " di_parents is
   on by default (Claudio Vaucheret)").

:- doc(version(1*0+3,2004/01/27,13:47*49+'CET'), "again spec is
   abs_spec in inter_optimize (David Trallero Mena)").

:- doc(version(1*0+238,2004/01/26,14:38*21+'CET'), "Added @tt{pd}
   to the list of domains and improved default values of several
   flags.  (German Puebla)").

:- doc(version(1*0+230,2004/01/22,17:19*33+'CET'), "The flag
   @tt{unfold} is now called @tt{local_control}. Additionally, its
   value @tt{onestep} is now more correctly called @tt{inst} for
   @em{instantiation}.  (German Puebla)").

:- doc(version(1*0+228,2004/01/21,12:49*08+'CET'), "Changed names
   of flags. Chapuza total!  (Francisco Bueno Carrillo)").

:- doc(version(1*0+227,2004/01/19,16:56*51+'CET'), "Added 'force'
   value to 'entry_policy' preprocessing flag to reanalyze all the
   entries in the intermodular registry despite their state.  (Jesus
   Correas Fernandez)").

:- doc(version(1*0+226,2004/01/19,16:55*54+'CET'), "Added
   global_scheduling preprocessing flag for modular analysis.  (Jesus
   Correas Fernandez)").

:- doc(version(1*0+225,2004/01/19,15:41*25+'CET'), " changed
   values of flag normalize to the standard on/ off.  (Claudio
   Vaucheret)").

:- doc(version(1*0+223,2004/01/19,14:59*51+'CET'), " added flag
   normalize. Value yes means the program analyzed is the normalized
   (Claudio Vaucheret)").

:- doc(version(1*0+1,2004/01/15,18:43*14+'CET'), "spec_poly flag
   added (David Trallero Mena)").

:- doc(version(1*0+3,2004/01/12,15:32*37+'CET'), "inter_assert_check
   made no sense (David Trallero Mena)").

:- doc(version(1*0+207,2004/01/12,17:47*46+'CET'), "Classified
   analyses in modes/types/etc.  (Francisco Bueno Carrillo)").

:- doc(version(1*0+2,2004/01/09,19:28*21+'CET'), "current_pp_flag
   changed to current_pp_flags (David Trallero Mena)").

:- doc(version(1*0+1,2004/01/08,20:17*00+'CET'), "peval_ana flag added
   (David Trallero Mena)").

:- doc(version(1*0+138,2004/01/05,17:27*02+'CET'), "interface flags
   added (David Trallero Mena)").

:- doc(version(1*0+200,2004/01/02,11:31*36+'CET'), "Added flag
   @tt{arg_filt} which controls whether argument filtering should be
   used when generating code after partial evaluation.  (German
   Puebla)").

:- doc(version(1*0+194,2003/12/31,11:20*56+'CET'), "Added flag
   @tt{part_conc} which determines whether partial concretization
   should be applied or not before unfolding. Note that this is a new
   possibility simply not available in traditional partial evaluation.
   (German Puebla)").

:- doc(version(1*0+181,2003/12/30,12:15*34+'CET'), "Added flag
   @tt{global_control} which allows selecting among different
   strategies for global control during partial evaluation.  (German
   Puebla)").

:- doc(version(1*0+174,2003/12/29,13:58*02+'CET'), "Added the flag
   @tt{unfold} which controls whether unfolding should be performed
   during analysis.  (German Puebla)").

:- doc(version(1*0+173,2003/12/29,13:51*32+'CET'), "Added
   entry_policy preprocessing flag.  (Jesus Correas Fernandez)").

:- doc(version(1*0+155,2003/12/18,11:48*13+'CET'), "Added
   'botfirst', 'botall', 'botbest' and 'bot' values to success_policy
   flag.  (Jesus Correas Fernandez)").

:- doc(version(1*0+154,2003/12/18,11:47*49+'CET'), "'trust' flag
   name changed to 'success_policy' (it is not used to handle trust
   assertions, only for success info of imported predicates).  (Jesus
   Correas Fernandez)").

:- doc(version(1*0+135,2003/10/20,19:36*40+'CEST'), "Added the
   @tt{trust} flag to select how trust assertions are handled when
   there are several substitutions applicable.  (Jesus Correas
   Fernandez)").

:- doc(version(1*0+118,2003/10/07,17:45*49+'CEST'), "Added the
   @tt{dump_level} flag.  (German Puebla)").

:- doc(version(1*0+108,2003/10/03,17:42*19+'CEST'), "Added
   exported predicates @pred{push_pp_flag/2} and @pred{pop_pp_flag/1}.
   (German Puebla)").

:- doc(version(1*0+106,2003/10/03,14:17*46+'CEST'), "Added the
   possibility of selecting checkers (in addition to fixpoints) using
   the fixpoint flag.  (German Puebla)").

:- doc(version(1*0+52,2003/09/02,12:03*02+'CEST'), "Added flag
   error_log.  (Francisco Bueno Carrillo)").

:- doc(version(1*0+49,2003/09/01,13:23*05+'CEST'), "Added a flag
   for controlling whether fixpoint identifiers from a previous
   analysis should be reused.  (German Puebla)").

:- doc(version(1*7+179,2002/01/18,12:05*05+'CET'), "Source file
   creation (Jesus Correas Fernandez)").


%%------------------------------------------------------------------------
