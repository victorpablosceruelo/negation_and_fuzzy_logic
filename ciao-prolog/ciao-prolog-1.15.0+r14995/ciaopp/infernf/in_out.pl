:- module(in_out,
	    [init_in_out/0,
		get_first_clause_id/2,
		input_nf_analysis_info/3,
		get_head_of_clause/2,
		get_body_of_clause/2,
		get_head_and_body_of_clause/3,
		get_body_and_vars_of_clause/3,
		get_key_and_concrete_literal_from_external_literal/3,
		nf_get_trust_dec/4,
		validate_data/4,
		construct_call_type/3,
		there_are_no_more_clauses/1,
		remove_preds_from_scc_call_graph/3
% translate_rule_list_for_nonfail/2 %% Commented out May-18-2003
	    ],
	    [assertions, isomodes]).

:- use_module(library(messages),   [warning_message/2]).
:- use_module(library(lists),      [append/3]).
:- use_module(library(idlists),    [member_0/2]).
:- use_module(library(terms_vars), [varset/2]).

:- use_module(infernf(nftable),
	    [create_a_litinfo/3,
		insert_field/4,
		insert_mode_type/3,
%validation
		find_entry/3,
		get_literal/2,
		get_literal_key/2,
		get_nfentry_info/7,
		is_a_builtin/3,
		set_flag_value_false/1,
		set_in_top_modetype/3
	    ]).
:- use_module(typeslib(typeslib),
	    [
		set_top_type/1,
		param_type_symbol_renaming/2
	    ]).
%% :- use_module(infernf(nftypes),
%%      [ check_and_assert_type_definition/2
%%      ]).
:- use_module(typeslib(type_support), [closed_var_list/2]).

:- include(infernf(data)).
:- include(infernf(clauses)).
:- include(typeslib(type_ops)). % for interface_translation.pl
% :- include(infernf(interface_translation)). %% Commented out May-18-2003
:- include(infernf(validation)).

init_in_out:-
	retractall_fact('$already_validated$'(_)),
	retractall_fact(trust_dec(_, _, _, _)),
	retractall_fact(user_dec(_)).

%-----------------------------------------------------------------------

% WARNING!: any declaration of arity 1 is considered a type.
% Perhaps we should escape type symbols instead of functors.
is_a_type_declaration(Term, Var, F) :-
	nonvar(Term),
	functor(Term, F, 1),
	arg(1, Term, Arg),
	Arg == Var,
	!.
is_a_type_declaration(Term, Var, Type) :-
% If Term is a parametrc type, use its equivalent non-parametric type symbol.
	nonvar(Term),
	Term =.. [F, Arg|R],
	Arg == Var,
	ParTypeSymbol =.. [F|R],
	param_type_symbol_renaming(ParTypeSymbol, Type).

:- pred input_nf_analysis_info(+ClauseList, +ModeTypes, -Table)

#
"Take the information needed for non-failure analysis and put it in @var{Table}.".

input_nf_analysis_info(ClauseList, ModeTypes, Table) :-
	input_nf_analysis_clauses(ClauseList, Table),
	input_nf_analysis_clauses(ModeTypes,  Table).

input_nf_analysis_clauses([],                  _Table) :- !.
input_nf_analysis_clauses([Clause|ClauseList], Table) :-
	clause_type1(Clause, Type, Clause0),
	handle_clause1(Type, Table, Clause0),
	input_nf_analysis_clauses(ClauseList, Table).

handle_clause1(directive, TAB, Directive) :-
	!,
	handle_direc(Directive, TAB).
handle_clause1(rule, TAB, Rule) :-
	!,
	handle_rule(TAB, Rule).
handle_clause1(fact, TAB, Fact) :-
	!,
	handle_fact(TAB, Fact).
handle_clause1(other, _, _).

handle_direc((:- modetype(MTyp)), Tab) :-
	!,
	functor(MTyp, F, N),
	insert_mode_type(Tab, F/N, MTyp).
% handle_direc((:- typedef(TypSymbol, Defin)), _Tab):- %% Commented out May-18-2003
handle_direc((:- typedef(_TypSymbol, _Defin)), _Tab) :-
	!.
% check_and_assert_type_definition(TypSymbol, Defin). %% Commented out May-18-2003
% Trust declarations are asserted.
handle_direc((:- trust(Head, InTypes, OuTypes)), Tab) :-
	!,
	construct_mode_type_declaration(Head, InTypes, OuTypes, Dec),
	functor(Head, F, N),
	insert_mode_type(Tab, F/N, Dec),
	assertz_fact(trust_dec(F/N, Head, InTypes, OuTypes)).
handle_direc((:- X), _Tab) :-
	!,
	assertz_fact(user_dec(X)).
handle_direc(Direct, _TAB) :-
	warning_message("The directive ~q is ignored.", [Direct]).

construct_mode_type_declaration(Head, InList, OutList, Dec) :-
	functor(Head, F, A),
	functor(Dec,  F, A),
	arg_construct_mode_type_declaration(A, Head, Dec, InList, OutList).

arg_construct_mode_type_declaration(0, _Head, _Dec, _InList, _OutList) :- !.
arg_construct_mode_type_declaration(A, Head,  Dec,  InList,  OutList) :-
	A > 0,
	arg(A, Head, HArg),
	arg_mode_type(InList, OutList, HArg, DecArg),
	arg(A, Dec, DecArg),
	A1 is A - 1,
	arg_construct_mode_type_declaration(A1, Head, Dec, InList, OutList).

%% WARNING!: If an argument has  ``var'' as type then is considered output,
%% otherwise is input.

arg_mode_type(InList, OutList, HArg, DecArg) :-
	find_declaration_in_list(InList,  HArg, InType),
	find_declaration_in_list(OutList, HArg, OuType),
	( is_output_argument(InType) ->
	    DecArg = (out: OuType) ;
	    DecArg = (in: InType)
	).

is_output_argument(var).
% WARNING!: what happens if we have var([X, Y])?
% is_output_argument('$typedef$'(free)).

% WARNING!: If an argument does not have type (or it is not
% understood) then is is assigned the top type.
% It should give a warning message!!
find_declaration_in_list([], _Var, Declar) :- !,
	set_top_type(Declar).
find_declaration_in_list([Term|List], Var, Declar) :-
	( is_a_type_declaration(Term, Var, F) ->
% Commeted by PLG 7-May-1998
% predicate_2_type_symbol(F, Declar)
	    type_dec_translate(F, Declar)
	; find_declaration_in_list(List, Var, Declar)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% May-18-2003 
%% WARNING: type_dec_translate(Type, Type). has been redefined,
%% Has to be deleted.
% Translate rules from pretty format to internal format where 
% "var" type have been replaced by "top" type, which is the worse case
% for non-failure analysis.
% Thus, in the case the analysis infers that the predicate does not
% fail with the types containing "tops", then it implies that the predicate
% does not fail with the types containing "vars"
% In future versions, the non-failure analysis will be able to deal with
% type definitions containing "var" types. 
%

type_dec_translate(Type, Type).

%% WAS %% Commented out May-18-2003
%% type_dec_translate(var, '$typedef$'(var)):- !.
%% type_dec_translate(Type1, Type2):-
%%           for_nonfail_type_translate(Type2, Type1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

construct_call_type(Literal, PPinfo, CallType) :-
	functor(Literal,  F, A),
	functor(CallType, F, A),
	arg_construct_call_type(A, Literal, CallType, PPinfo).

arg_construct_call_type(0, _Literal, _CallType, _PPinfo) :-
	!.
arg_construct_call_type(A, Literal, CallType, PPinfo) :-
	A > 0,
	arg(A, Literal, LitArg),
	find_declaration_in_list(PPinfo, LitArg, CallTypeArg),
	arg(A, CallType, CallTypeArg),
	A1 is A - 1,
	arg_construct_call_type(A1, Literal, CallType, PPinfo).

flat_body_list1((A, B), FlatBody) :- !,
	flat_body_list1(A, FA), flat_body_list1(B, FB),
	append(FA, FB, FlatBody).
% flat_body_list1(Lit, [Lit]):- var(Lit),!.
flat_body_list1(Lit, [Lit]) :- !.

%% flat_body_list1( ( A , B ) , FlatBody):- !,
%%        flat_body_list1( A , FA ), flat_body_list1( B , FB ),
%%        append(FA, FB, FlatBody).
%% % flat_body_list1( Lit:_ , [Lit] ):- var(Lit),!.
%% flat_body_list1( Lit:_ , [Lit] ):-!.
%% flat_body_list1( (!), [(!)] ):-!.

%% translate_to_internal_format(+Lit_List, +HeadPred, -Inter_List)
%% Lit_List: list of original literals to be traslated.
%% HeadPred: predicate in which definition the literals in Lit_List appears.
%% Inter_List: translated list of literals. 

translate_to_internal_format([],             _HeadPred, []) :- !.
translate_to_internal_format([Literal|Body], HeadPred,  [Litinfo|InfoBody]) :-
	create_a_litinfo(Literal, HeadPred, Litinfo),
	translate_to_internal_format(Body, HeadPred, InfoBody).

%% ANNOTATION OF THE PROGRAM.
% Not used any more.

%% nf_annotate(TAB):-
%%      var(TAB), !.
%% nf_annotate([Entry|TAB]):-
%%      get_pred_clauses(Entry, Clauses, Ann_Clauses),
%%      nf_cl_annotate(Clauses, Ann_Clauses),
%%      nf_annotate(TAB).
%% 
%% nf_cl_annotate(Clauses, _Ann_Clauses):-
%%      there_are_no_more_clauses(Clauses), 
%%      !.
%% nf_cl_annotate([Clause|CList], [AClause|Ann_Clauses]):-
%%      get_head_and_body_of_clause(Clause, Head, Body),
%%      (Body = [] 
%%           -> AClause = cl(Head, Dict)
%%           ;  nf_body_annotate(Body, ABody), 
%%              AClause = cl((Head :- ABody), Dict)
%%      ),
%%      nf_cl_annotate(CList, Ann_Clauses).
%% 
%% nf_body_annotate([Lit], ALit):-!,
%%      nf_lit_annotate(Lit, ALit).
%% nf_body_annotate([Lit|Rest], (ALit, ARest)):-
%%      nf_lit_annotate(Lit, ALit),
%%      nf_body_annotate(Rest, ARest).
%% 
%% % MOVED FROM in_out.pl (nf.pl) PBC
%% 
%% nf_lit_annotate(Lit, ALit):-
%%      get_literal_flag(Lit, Literal, Nfail_flag),    
%%      (Nfail_flag == fail 
%%             ->  
%%             (dump_infer(yes) ->
%%                 ALit = (Literal, pragma([possible_fail]));
%%                 ALit = Literal
%%             );
%%             ALit = Literal
%%      ).
%

remove_preds_from_scc_call_graph([SCC|SCCCallGraph], No_Analyzable_Preds, SCC_CG
) :-
	remove_preds_from_scc(SCC, No_Analyzable_Preds, NewSCC),
	( NewSCC = []
	-> SCC_CG = Res_SCC_CG
	; SCC_CG = [NewSCC|Res_SCC_CG]
	),
	remove_preds_from_scc_call_graph(SCCCallGraph, No_Analyzable_Preds,
	    Res_SCC_CG).
remove_preds_from_scc_call_graph([], _No_Analyzable_Preds, []).

remove_preds_from_scc([Pred|SCC], No_Analyzable_Preds, NewSCC) :-
	( member_0(Pred, No_Analyzable_Preds)
	-> NewSCC = Res_SCC
	; NewSCC = [Pred|Res_SCC]
	),
	remove_preds_from_scc(SCC, No_Analyzable_Preds, Res_SCC).
remove_preds_from_scc([], _No_Analyzable_Preds, []).
