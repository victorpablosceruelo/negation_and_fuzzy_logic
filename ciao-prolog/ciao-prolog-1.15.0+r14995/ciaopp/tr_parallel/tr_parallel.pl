:- module(tr_parallel, [rewrite_clauses/2],
	[
	    andprolog,
	    api(ciaopp_api)
	]).
:- use_module(program(native), [native_builtin/2, native_property/2]).
:- use_module(program(p_unit), [replace_program/2]).
:- use_module(program(clidlist), [rewrite_source_clause/4]).
:- use_module(ciaopp(preprocess_flags), [current_pp_flag/2]).
:- use_module(annotate, [annotate/8]).
:- use_module(ciaopp(tr_parallel(tr_granul)), [annotate_granularity/3]).

:- use_module(ciaopp(tr_parallel(tr_granul_res)), 
	[ annotate_granularity_res/2]).

:- push_prolog_flag(multi_arity_warnings, off).

:- multifile aidomain/1.

:- multifile transformation/4.


transformation(Ann,Cls,Ds,_Info) :-
 	current_pp_flag(para_iap, IAP),
 	current_pp_flag(para_grain, Gr),
 	current_pp_flag(para_local, Local),
 	current_pp_flag(para_cost, Cost),
 	current_pp_flag(modes, Mode),
	transformation0(Ann,IAP,Gr,Local,Mode,Cost,Cls,Ds,_Info).

transformation0(Ann,IAP,Gr,Local,Mode,Cost,Cls,Ds,_Info) :-
	F =.. [Ann,IAP],
% 	(
% 	    ( Mode \== none,
%               current_fact(domain(AbsInt)),
%               aidomain(AbsInt) ) -> true
% 	;
% 	    AbsInt = none
% 	),
	AbsInt = Mode,
	annotate(Cls,Ds,F,Mode,Local,AbsInt,AnnCls0,NewDs),
	rewrite_clauses(AnnCls0,AnnCls),
        (
	    ( Gr == gr ->
	      % Granularity analysis using 'old' infercost
	      tr_granul:annotate_granularity(AnnCls,NewDs,Cost)
	    ;
              ( Gr == gr_res ) ->
                % Granularity analysis using resources
	        tr_granul_res:annotate_granularity_res(AnnCls,NewDs)                          
            )
	;
	    replace_program(AnnCls,NewDs)
	),
	add_package_to_output([andprolog]).


:- multifile transformation/1.

transformation(mel).
transformation(cdg).
transformation(udg).
transformation(uoudg).
transformation(uudg).
transformation(disjwait).
transformation(tgudg).
transformation(urlp).
transformation(crlp).

:- pop_prolog_flag(multi_arity_warnings).

rewrite_clauses([], []).
rewrite_clauses([clause(H0,B0):Cl_ID|T1], [NewCl:Cl_ID|T2]) :-
	ampersands_clause(clause(H0,B0),clause(H1,B1)),
	rewrite_source_clause(H1, B1, Cl_ID, NewCl),
	rewrite_clauses(T1, T2).

ampersand_idiom_1(A,B,G) :- native_builtin(G,ampersand(A,B)).
ampersand_idiom_2(A,B,G) :- native_builtin(G,ampersand_det(A,B)).
ampersand_idiom_3(A,H,G) :- native_builtin(G,amp_publish_goal(A,H)).
ampersand_idiom_4(A,H,G) :- native_builtin(G,amp_publish_goal_det(A,H)).
% ampersand_idiom_5(H,G)   :- native_builtin(G,amp_get_result(H)).
% ampersand_idiom_6(H,G)   :- native_builtin(G,amp_get_result_det(H)).
ampersand_ground(C,G)    :- native_property(G,ground(C)).
ampersand_indep(C,G)     :- native_property(G,indep(C)).

ampersands_clause(clause(H,B0),clause(H,B)):- !,
	ampersands_body(B0,_,B).
ampersands_clause(Cl,Cl).

ampersands_body((A0&B0),_K,G):- !,
	ampersand_idiom_1(A,B,G),
	ampersands_parexp(A0,_K,A),
	ampersands_parexp(B0,_,B).
ampersands_body((A0'&!'B0),_K,G):- !,
	ampersand_idiom_2(A,B,G),
	ampersands_parexp(A0,_K,A),
	ampersands_parexp(B0,_,B).
ampersands_body((A0&>H),_K,G):- !,
	ampersand_idiom_3(A,H,G),
	ampersands_parexp(A0,_K,A).
ampersands_body((A0'&!>'H),_K,G):- !,
	ampersand_idiom_4(A,H,G),
	ampersands_parexp(A0,_K,A).
ampersands_body((H<&),_K,(H<&)):- !.
ampersands_body((H'<&!'),_K,(H'<&!')):- !.
ampersands_body((A0,B0),_K,(A,B)):- !,
	ampersands_body(A0,_K,A),
	ampersands_body(B0,_,B).
ampersands_body((A0->B0),_K,(A->B)):- !,
	ampersands_body(A0,_K,A),
	ampersands_body(B0,_,B).
ampersands_body((A0;B0),_K,(A;B)):- !,
	ampersands_body(A0,_K,A),
	ampersands_body(B0,_,B).
ampersands_body(indep(X),_,A) :- !,
	ampersand_indep(X,A).
ampersands_body(ground(X),_,A) :- !,
	ampersand_ground(X,A).
ampersands_body(A0:_K,_K,A0) :- !.
ampersands_body(A,_,A).

ampersands_parexp((A0&B0),_K,G):- !,
	ampersand_idiom_1(A,B,G),
	ampersands_parexp(A0,_K,A),
	ampersands_parexp(B0,_,B).
ampersands_parexp((A0'&!'B0),_K,G):- !,
	ampersand_idiom_2(A,B,G),
	ampersands_parexp(A0,_K,A),
	ampersands_parexp(B0,_,B).
ampersands_parexp((A0&>H),_K,G):- !,
	ampersand_idiom_3(A,H,G),
	ampersands_parexp(A0,_K,A).
ampersands_parexp((A0'&!>'H),_K,G):- !,
	ampersand_idiom_4(A,H,G),
	ampersands_parexp(A0,_K,A).
ampersands_parexp((H<&),_K,(H<&)):- !.
ampersands_parexp((H'<&!'),_K,(H'<&!')):- !.
ampersands_parexp((A0,B0),_K,(A,B)):- !,
	ampersands_parexp(A0,_K,A),
	ampersands_parexp(B0,_,B).
ampersands_parexp((A0->B0),_K,(A->B)):- !,
	ampersands_parexp(A0,_K,A),
	ampersands_parexp(B0,_,B).
ampersands_parexp((A0;B0),_K,(A;B)):- !,
	ampersands_parexp(A0,_K,A),
	ampersands_parexp(B0,_,B).
ampersands_parexp(A0:_K,_K,A0) :- !.
ampersands_parexp(A,_,A).

% :- data the_iap/1.

% the_iap(siap).

% iap(X) :- var(X), !, current_fact(the_iap(X)).
% iap(X) :-
% 	valid_iap(X),
% 	retract_fact(the_iap(_)),
% 	asserta_fact(the_iap(X)).

% valid_iap(siap).
% valid_iap(nsiap).

% do_if_not_done(X,Y) :-
% 	domain(X),
% 	!,
% 	Y = false.
% do_if_not_done(X,true) :-
% 	analyze(X).

% clean_analyses(Done1,Done2,Done3,Done4) :-
% 	( (ground(Done1), Done1 = true) -> cleanup_plai_db(det)    ; true ),
% 	( (ground(Done2), Done2 = true) -> cleanup_plai_db(shfr)   ; true ),
% 	( (ground(Done3), Done3 = true) -> cleanup_plai_db(eterms) ; true ),
% 	( (ground(Done4), Done4 = true) -> cleanup_plai_db(nfg)    ; true ).

% ampersands([],[]).
% ampersands([Cl0:Id|Cls0],[Cl:Id|Cls]):-
% 	ampersands_clause(Cl0,Cl),
% 	ampersands(Cls0,Cls).

% ampersands_clause(clause(H,B0),clause(H,B)):- !,
% 	ampersands_body(B0,_,B).
% ampersands_clause(Cl,Cl).

% ampersands_body((A0&B0),K,G):- !,
% 	ampersand_idiom_1(A,B,G),
% 	ampersands_parexp(A0,K,A),
% 	ampersands_parexp(B0,_,B).
% ampersands_body((A0'&!'B0),K,G):- !,
% 	ampersand_idiom_2(A,B,G),
% 	ampersands_parexp(A0,K,A),
% 	ampersands_parexp(B0,_,B).
% ampersands_body((A0&>H),K,G):- !,
% 	ampersand_idiom_3(A,H,G),
% 	ampersands_parexp(A0,K,A).
% ampersands_body((A0'&!>'H),K,G):- !,
% 	ampersand_idiom_4(A,H,G),
% 	ampersands_parexp(A0,K,A).
% ampersands_body((H<&),_K,G):- !,
% 	ampersand_idiom_5(H,G).
% ampersands_body((H'<&!'),_K,G):- !,
% 	ampersand_idiom_6(H,G).
% ampersands_body((A0,B0),K,(A,B)):- !,
% 	ampersands_body(A0,K,A),
% 	ampersands_body(B0,_,B).
% ampersands_body((A0->B0),K,(A->B)):- !,
% 	ampersands_body(A0,K,A),
% 	ampersands_body(B0,_,B).
% ampersands_body((A0;B0),K,(A;B)):- !,
% 	ampersands_body(A0,K,A),
% 	ampersands_body(B0,_,B).
% ampersands_body(indep(X),_,A) :- !,
% 	ampersand_indep(X,A).
% ampersands_body(ground(X),_,A) :- !,
% 	ampersand_ground(X,A).
% ampersands_body(A0:K,K,A0) :- !.
% ampersands_body(A,_,A).

% ampersands_parexp((A0&B0),K,G):- !,
% 	ampersand_idiom_1(A,B,G),
% 	ampersands_parexp(A0,K,A),
% 	ampersands_parexp(B0,_,B).
% ampersands_parexp((A0'&!'B0),K,G):- !,
% 	ampersand_idiom_2(A,B,G),
% 	ampersands_parexp(A0,K,A),
% 	ampersands_parexp(B0,_,B).
% ampersands_parexp((A0&>H),K,G):- !,
% 	ampersand_idiom_3(A,H,G),
% 	ampersands_parexp(A0,K,A).
% ampersands_parexp((A0'&!>'H),K,G):- !,
% 	ampersand_idiom_4(A,H,G),
% 	ampersands_parexp(A0,K,A).
% ampersands_parexp((H<&),_K,G):- !,
% 	ampersand_idiom_5(H,G).
% ampersands_parexp((H'<&!'),_K,G):- !,
% 	ampersand_idiom_6(H,G).
% ampersands_parexp((A0,B0),K,(A,B)):- !,
% 	ampersands_parexp(A0,K,A),
% 	ampersands_parexp(B0,_,B).
% ampersands_parexp((A0->B0),K,(A->B)):- !,
% 	ampersands_parexp(A0,K,A),
% 	ampersands_parexp(B0,_,B).
% ampersands_parexp((A0;B0),K,(A;B)):- !,
% 	ampersands_parexp(A0,K,A),
% 	ampersands_parexp(B0,_,B).
% ampersands_parexp(A0:K,K,A0) :- !.
% ampersands_parexp(A,_,A).

