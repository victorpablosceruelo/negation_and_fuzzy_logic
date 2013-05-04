:- module( api_stuff,
	[ % clean
	  cleanup_all/0,
	  cleanup_api/0,
	  %
	  unify_args/3,
	  %
	  create_pre_post/4,
	  create_pre_post/3,
	  % Do not call this predicate % TODO: why?
	  generate_all_internal_data/0
	], 
	[condcomp, assertions, regtypes, ciaopp_options]).


:- doc( bug, 
	 "%% --- clause_* cannot fail
	  %% --- create exception
	  %% --- should api_write consider clause/2 and assertion_read?
	  %% --- WHY THERE ARE DIRECTIVES THAT ARE ASSERTIONS IF THEY ARE IN CANONICAL
	  %%     VERSION!!!
	  %% --- functions like: is_last_clause, is_last_literal..." ).

%% If you uncomment this line, the API will have rt checks!
%% :- compilation_fact( api_rt ).

:- if(defined(mini_pp)).

% cleanup_actions.
% cleanup_assrt.
clean_analysis_info.
cleanup_p_abs.

:- else.

% --- this should be done with hooks!
:- use_module(ciaopp(driver), [clean_analysis_info/0]).
:- use_module(program(p_abs), [cleanup_p_abs/0]).

:- endif.

:- use_module(program(itf_db), [cleanup_itf_db/0]).
:- use_module(program(p_unit), [cleanup_punit/0]).
:- use_module(program(p_asr), [cleanup_code_and_related_assertions/0
				       , cleanup_pasr/0
				       ] ).

:- use_module(ciaopp(api(api_internal_types))).
:- use_module(ciaopp(api(api_order))).
:- use_module(ciaopp(api(api_predcl))).
:- use_module(ciaopp(api(api_direc_assrt))).
:- use_module(ciaopp(api(api_module))).

:- use_module(ciaopp(api(comment_db)), 
	                                        [ cleanup_comment_db/0 ] ).

:- use_module(ciaopp(api(api_printer)), 
	                      [ internal_remember_disj_conj_cls_names/1 ] ).

:- use_package(.(api_internal_dec)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   CLEANING  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred cleanup_api 
# "Clean up all fact that something to do with CiaoPP API.".

cleanup_api :-
	cleanup_actions,
	pr_order_clean,
	cleanup_module,
	cleanup_assrt,
	cleanup_comment_db,
	cleanup_order,
	internal_remember_disj_conj_cls_names( [] ).


:- pred cleanup_all # "Clean all asserted facts. Let CiaoPP DB as if
CiaoPP would be started from scratch".

cleanup_all :-
        cleanup_itf_db,
	clean_analysis_info,
	cleanup_p_abs,
	cleanup_punit,
	cleanup_pasr,
	cleanup_code_and_related_assertions,
	cleanup_api.


%------------------------------------------------------------------------


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   CONVERTING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%
%% BASE
%%

%% Now in library( formulae )


%%
%% LIB
%%

unify_args( [] , [] , [] ) :-
	!.
unify_args( [ A | As ] , [ NA | NAs ] , [ (A,NA) | R ] ) :-
	!,
	unify_args( As , NAs , R ).
unify_args( [] , A , _ ) :-
	!,
	message( error ,
                 ['unify_agrs: list have different size: (2nd arg):\n' , A] ).
unify_args( A , [] , _ ) :-
	message( error ,
                 ['unify_agrs: list have different size: (1st arg):\n' , A] ).


:- regtype pair_t_body/1.

pair_t_body((Pre,Post)) :-
	t_body(Pre),
	t_body(Post).

:- pred create_pre_post( IB , PrePost , B , TB )
	: (t_body( IB ), pair_t_body( PrePost ))
        => (t_body(B), var(TB))

# "The body @var{B} is the result of place, literals @var{Pre} and
  @var{Post} before and after the litheral @var{IB}. @var{TB} is the
  tail of @var{B}.".

create_pre_post( ClCall , (Pre,Post) , B , TB ) :-
	literal_concat( Post, TB     , NPost ),
	literal_concat( Pre , ClCall , A ),
	literal_concat( A   , NPost   , B ).




:- pred create_pre_post( IB , PrePost , B )
	: (t_body( IB ), pair_t_body( PrePost ) )
        => (t_body(B), var(TB))

# "Similar to @pred{create_pre_post/3}, but with no tail.".

create_pre_post( A , B , D ) :- 
	create_pre_post( A , B , C , true ),
	remove_true_literals( C , D ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   API INTERNALS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% this CAN BE ONLY CALLED FROM p_unit!!! TEMPORARY!
generate_all_internal_data :-
	get_key_predicates( [internal] , L ),
	member( Pred , L ),
	get_clauses( Pred , Cls ),
	generate_clause_order( Cls , PO ),
	Pred=PKey,
	cl_order_set( PKey , PO ),
	fail.
generate_all_internal_data.




:- pred generate_clause_order( Cls , ClsKeys ) 
	: (list( Cls , t_cls ) , list( ClsKeys , t_cls_ppi_id ))

# "Return in @var{ClsKeys} the list of program point info of @var{Cls}
  clause list.".

generate_clause_order( []     , [] ).
generate_clause_order( [ cls${ id => ID } | Cs ] , [ID|IDs]  ) :-
	generate_clause_order( Cs , IDs ).

