:- module(rfuzzy_tr,[fuzzy_pred/3,fuzzy_pred2/3],[]).

:- use_module(library(aggregates), [findall/4]).
:- use_module(library(terms),[copy_args/3]).
:- use_module(library(messages),[error_message/2]).
:- use_module(library(write),[write/1]).

:- use_module(library('compiler/c_itf')).

:- data fpred/1.
:- data fclause/2.
:- data frule/3.
:- data fnegclause/3.
:- data faggr/4.

% Important info to be saved.
:- data rfuzzy_predicate_info/3.

% :- data func_arguments/2.

:- include('rfops').
% :- include(library('clpr/ops')).
:- include(library('clpqr-common/ops')).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

% This is to enable/disable debug.
% do_debug_rfuzzy('No').
do_debug_rfuzzy('Yes').

debug_msg(_Msg1, _Msg2) :- do_debug_rfuzzy('No').
debug_msg( Msg1,  Msg2) :-
	do_debug_rfuzzy('Yes'),
	write('\n% [tr: '), write(Msg1), 
	write(']: '),  write(Msg2).
debug_nl :- do_debug_rfuzzy('No').
debug_nl :- do_debug_rfuzzy('Yes'), write('\n').

rfuzzy_warning_msg(Function, Error) :-
	write('WARNING: '),
	write(Function),
	write(Error), 
	nl.

rfuzzy_error_msg(Function, Error) :-
	write('ERROR: '),
	write(Function),
	write(Error), 
	nl.

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

% fuzzy_pred2(Arg1, Arg2, Arg3) :- 
fuzzy_pred2(Arg1, Arg1, _Arg3) :- 
	debug_msg('clause: arg1', Arg1).
%	original_fuzzy_clause_trans(Arg1, Arg2, Arg3),
%	debug_msg('clause: arg2', Arg2),
%	debug_msg('clause: arg3', Arg3),
%	debug_nl.

fuzzy_pred(Arg1, Arg2, Arg3) :- 
	debug_msg('sent: arg1', Arg1),
	original_fuzzy_sentence_trans(Arg1, Arg2, Arg3),
	debug_msg('sent: arg2', Arg2),
	debug_msg('sent: arg3', Arg3),
	debug_nl.

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

save_predicate_info(Kind, F, A) :-
	save_predicate_info_aux(Kind, F, A),
	save_predicate_info_aux('is_defined', F, A), % Predicate MUST be defined.
	debug_msg('save_predicate_info(Kind, F, A) out ', save_predicate_info(Kind, F, A)).

save_predicate_info_aux(Kind, F, A) :-
	retract_fact(rfuzzy_predicate_info(Kind, F, A)), !, % Retract last
	assertz_fact(rfuzzy_predicate_info(Kind, F, A)).
save_predicate_info_aux(Kind, F, A) :-
	assertz_fact(rfuzzy_predicate_info(Kind, F, A)).

change_name(Prefix, F, R) :-
	translate_prefix(Prefix, Real_Prefix),
	atom_codes(F, F_Chars),
	concat_lists(Real_Prefix, F_Chars, R_Chars),
	atom_codes(R, R_Chars),
	debug_msg('change_name', change_name(Prefix, F, R)).

concat_lists([], N2, N2).
concat_lists([Elto|N1], N2, [Elto|Res]) :-
	concat_lists(N1, N2, Res).

translate_prefix('type', "rfuzzy_type_").
translate_prefix('default_with_no_cond', "rfuzzy_default_with_no_cond_").
translate_prefix('default_with_cond', "rfuzzy_default_with_cond_").
translate_prefix('default', "rfuzzy_default_").
translate_prefix('fact', "rfuzzy_fact_").
translate_prefix('rule', "rfuzzy_rule_").
translate_prefix('function', "rfuzzy_function_").
translate_prefix('normal', "rfuzzy_").
translate_prefix(_X, "rfuzzy_error_error_error_").

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

extract_defaults_and_types(R_In, R_Out) :-

	% Extract info
	retract_fact(rfuzzy_predicate_info('is_defined', F_Main, Num_Args)), 
	debug_msg('build_clauses', fact('is_defined', F_Main, Num_Args)),
	!, % Backtracking is not allowed here

	build_clauses(F_Main, Num_Args, Main_Cl, Normal_Cl, Default_Cl),
	!, % Backtracking not allowed.

	debug_msg('build_clauses', build_clauses(Main_Cl, Normal_Cl, Default_Cl)),
	extract_defaults_and_types([Main_Cl, Normal_Cl, Default_Cl|R_In], R_Out).	

extract_defaults_and_types(R, R).


% This is to call the normal function.
build_clauses( F_Main, Num_Args, 
	( H_Main :- H_Types, ( H_Normal ; H_Default)
	), 
	( H_Normal :- ( H_Fact ; (\+(H_Fact_Aux), H_Function) ;  
			  (\+(H_Fact_Aux), \+(H_Function_Aux), H_Rule) )
	),
	( H_Default :- \+(H_Fact_Aux), \+(H_Function_Aux), \+(H_Rule_Aux), 
	  (Cl_With_Cond ; (\+(Cl_With_Cond_Aux), Cl_With_No_Cond))
	)
	     ) :-

	% Build MAIN functor.
	functor(H_Main, F_Main, Num_Args),           % Create main functor.

	% Build NORMAL functor.
	build_functors(H_Normal, _H_Normal_Aux, F_Main, H_Main, Num_Args, 'normal'),

	% Build DEFAULT functor.
	build_functors(H_Default, _H_Default_Aux, F_Main, H_Main, Num_Args, 'default'),

	build_types(H_Types, _H_Types_Aux, F_Main, H_Main, Num_Args),
	build_h_fact(H_Fact, H_Fact_Aux, F_Main, H_Main, Num_Args),
	build_h_rule(H_Rule, H_Rule_Aux, F_Main, H_Main, Num_Args),
	build_h_function(H_Function, H_Function_Aux, F_Main, H_Main, Num_Args),
	build_cl_with_cond(Cl_With_Cond, Cl_With_Cond_Aux, F_Main, H_Main, Num_Args),
	build_cl_with_no_cond(Cl_With_No_Cond, _, F_Main, H_Main, Num_Args).

build_functors(H, H_Aux, F_Origin, H_Origin, Num_Args, Kind) :-
	% Main functor.
	change_name(Kind, F_Origin, F),    % Change atom name
	functor(H, F, Num_Args),           % Create functor
	copy_args(Num_Args, H, H_Origin),  % Copy arguments.

	% Auxiliar functor.
	New_Num_Args is Num_Args - 1,
	functor(H_Aux, F, Num_Args),              % Create functor
	copy_args(New_Num_Args, H_Aux, H_Origin). % Copy arguments.

build_functor_if_fact_saved(H, H_Aux, F_Origin, H_Origin, Num_Args, Kind) :-

	% Do we have saved facts ??
	retract_fact(rfuzzy_predicate_info(Kind, F_Origin, Num_Args)),
	!, % Backtracking not allowed.

	% Build functor.
        build_functors(H, H_Aux, F_Origin, H_Origin, Num_Args, Kind).

build_fail_functor(H) :-
	functor(H, 'fail', 0).    % Create functor

build_types(H, H_Aux, F_Origin, H_Origin, Num_Args) :-
	build_functor_if_fact_saved(H, H_Aux, F_Origin, H_Origin, Num_Args, 'type').

build_types(H, H, F_Origin, _H_Origin, _Num_Args) :-
	functor(H, 'true', 0), % Create functor
	rfuzzy_warning_msg(F_Origin, ' types are not set. ').

build_h_fact(H, H_Aux, F_Origin, H_Origin, Num_Args) :-
	build_functor_if_fact_saved(H, H_Aux, F_Origin, H_Origin, Num_Args, 'fact').

build_h_fact(H, H, _F_Origin, _H_Origin, _Num_Args) :- build_fail_functor(H).

build_h_rule(H, H_Aux, F_Origin, H_Origin, Num_Args) :-
	build_functor_if_fact_saved(H, H_Aux, F_Origin, H_Origin, Num_Args, 'rule').

build_h_rule(H, H, _F_Origin, _H_Origin, _Num_Args) :- build_fail_functor(H).

build_h_function(H, H_Aux, F_Origin, H_Origin, Num_Args) :-
	build_functor_if_fact_saved(H, H_Aux, F_Origin, H_Origin, Num_Args, 'function').

build_h_function(H, H, _F_Origin, _H_Origin, _Num_Args) :- build_fail_functor(H).

build_cl_with_cond(H, H_Aux, F_Origin, H_Origin, Num_Args) :-
	build_functor_if_fact_saved(H, H_Aux, F_Origin, H_Origin, Num_Args, 'default_with_cond').
	
build_cl_with_cond(H, H, _F_Origin, _H_Origin, _Num_Args) :- build_fail_functor(H).

build_cl_with_no_cond(H, H_Aux, F_Origin, H_Origin, Num_Args) :-
	build_functor_if_fact_saved(H, H_Aux, F_Origin, H_Origin, Num_Args, 'default_with_no_cond').

build_cl_with_no_cond(H, H, F_Origin, _H_Origin, _Num_Args) :- 
	build_fail_functor(H),
	rfuzzy_warning_msg(F_Origin, ' default is not set. ').

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

%default values:
original_fuzzy_sentence_trans((:- default(F/A,X)),(H :- X_In .=. X),_) :-
	number(X), % X must be a number.
	number(A), % A must be a number.

	change_name('default_with_no_cond', F, NewF), 
	N_Args is A + 1,            % We add a new parameter

	functor(H, NewF, N_Args),   % H is our new functor.
	arg(N_Args, H, X_In),          % Argument at position A is X.

	save_predicate_info('default_with_no_cond',F,N_Args).

original_fuzzy_sentence_trans((:- default(F/A,X) => F_Cond/A),(H :- X_In .=. X, H_Cond),_) :-
	number(X), % X must be a number.
	number(A), % A must be a number.

	change_name('default_with_cond', F, NewF),
	N_Args is A + 1,            % We add a new parameter

	functor(H, NewF, N_Args),   % H is our new functor.
	arg(N_Args, H, X_In),          % Argument at position A is X.
	functor(H_Cond, F_Cond, A), % H is our new functor.
	copy_args(A, H_Cond, H),    % Copy args from main functor.

	save_predicate_info('default_with_cond',F,N_Args).

% properties setting
original_fuzzy_sentence_trans((:- set_prop F/Arity => Properties_Decl),(H :- Cls),_):-
	number(Arity), % A must be a number.
	N_Args is Arity + 1, % Real function arity is Arity + 1.

	debug_msg('(:- set_prop F/A => Properties_Decl) ', (:- set_prop F/Arity => Properties_Decl)),

	change_name('type', F, NewF),
	functor(H, NewF, N_Args),     % Build functor.
	!, % Backtracking not allowed.

	trans_each_property(F, H, Arity, 1, Properties_Decl, Cls),
	save_predicate_info('type', F, N_Args).

% transformation predicate.
original_fuzzy_sentence_trans(end_of_file,Cls,_):-
	!,
	extract_defaults_and_types([end_of_file], Cls).
%	extract_defaults_and_types([(:- use_package(clpr)),end_of_file], Cls).

% Only for fuzzy facts.
original_fuzzy_sentence_trans((H value X),NH, _):-

	number(X),                    % X must be a number.
	functor(H,F,N_Args_Orig),     % Extrac functor H name(F) and number_args
	N_Args is N_Args_Orig + 1,    % We add a new parameter

	change_name('fact', F, NewF), % Change name.
	save_predicate_info('fact', F, N_Args),

%       Set new fact name and properties.
	functor(NH,NewF,N_Args),         % NH is our new functor.
	copy_args(N_Args_Orig,NH,H),     % Copy arguments from H to NH.
	arg(N_Args,NH,X),                % Argument at position A is X.

	debug_msg('(H value X) => NH',((H value X) => NH)).

% aggregator definition:
original_fuzzy_sentence_trans((:- aggr A <# I ## M #> F ),(:- op(1190,fx,A)),_):-
	assertz_fact(faggr(A,I,M,F)), !.

original_fuzzy_sentence_trans((:- aggr A ## M #> F ),(:- op(1190,fx,A)),_):-
	assertz_fact(faggr(A,id,M,F)), !.

original_fuzzy_sentence_trans((:- aggr A <# I ## M ),(:- op(1190,fx,A)),_):-
	assertz_fact(faggr(A,I,M,id)), !.

original_fuzzy_sentence_trans((:- aggr A <# I ),(:- op(1190,fx,A)),_):-
	assertz_fact(faggr(A,I,A,id)), !.

original_fuzzy_sentence_trans((:- aggr A #> F ),(:- op(1190,fx,A)),_):-
	assertz_fact(faggr(A,id,A,F)), !.

original_fuzzy_sentence_trans((:- aggr A ## M ),(:- op(1190,fx,A)),_):-
	assertz_fact(faggr(A,id,M,id)), !.

original_fuzzy_sentence_trans((:- aggr A),(:- op(1190,fx,A)),_):-
	assertz_fact(faggr(A,id,A,id)), !.

% function definition.
original_fuzzy_sentence_trans((Head :# Lista),Cls,_) :-
	% list(Lista),
	debug_msg('(Head :# Lista) ', (Head :# Lista)),

	change_name('function', Head, NewHead), % Change atom name
	save_predicate_info('function', Head, 2),

	(   build_straight_lines(NewHead, Lista, Cls);
	    (
		rfuzzy_error_msg(Head, ' syntax is not recognized. Please fix it. '),
		build_fail_functor(Cl1),
		functor(H, NewHead, 2),
		Cls is (H :- Cl1)
	    )
	).

% credibility value:
original_fuzzy_sentence_trans((Head cred (C, D) :~ B0),Clauses,_):-
	trans_rule(Head, C, D, B0, Clauses).

original_fuzzy_sentence_trans((Head :~ B0),Clauses,_):-
	\+has_credibility(Head),
	trans_rule(Head, 'prod', 1, B0, Clauses).

% default
original_fuzzy_sentence_trans(A,A,_) :-
	has_rfuzzy_symbol(A),
	debug_msg('Any other sentence.', A).

original_fuzzy_sentence_trans(A,A,_) :-
	debug_msg('NOT FUZZY CL: ', A).

has_rfuzzy_symbol(( A :~ B )) :- syntax_error(( A :~ B )).
has_rfuzzy_symbol(( A :# B )) :- syntax_error(( A :# B )).
has_rfuzzy_symbol(( A value B )) :- syntax_error(( A value B )).
has_rfuzzy_symbol(( :- set_prop A )) :- syntax_error(( :- set_prop A )).
has_rfuzzy_symbol(( :- default(A,B) )) :- syntax_error(( :- default(A,B) )).
has_rfuzzy_symbol(( :- default(A,B) => C )) :- syntax_error(( :- default(A,B) => C )).

syntax_error(A) :-
	write('ERROR: Not recognized syntax: '),
	write(A),
	nl.

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

has_credibility((H cred (Op, D))) :-
	debug_msg('FAIL: (H cred (Op, D)) ', (H cred (Op, D))),
	debug_nl.

is_aggregator(Op) :-
	(
	    (
		faggr(Op,_,_,_), !  % ???
	    ) 
	;
	    (
		write('Operator is not an agregator: '),
		write(Op),
		nl, !, fail
	    )
	).

trans_rule(H, Op1, D, Orig_Cls, (NH :- Cls_1, Cls_2)) :-
	debug_msg('trans_rule(H, Op1, D, Orig_Cls) ', (trans_rule(H, Op1, D, Orig_Cls))),
	nonvar(Op1), nonvar(D), nonvar(Orig_Cls),
	debug_msg('functor ', 	functor(Orig_Cls,Op2,1)),
	functor(Orig_Cls,Op2,1),
	is_aggregator(Op2), 
	is_aggregator(Op1), 
	arg(1, Orig_Cls, Argument),

	functor(H,F,Ar),
	A is Ar + 1,
	change_name('rule', F, NewF), % Change name
	save_predicate_info('rule', F, A),
	functor(NH,NewF,A),
	copy_args(Ar,NH,H),

	% Translate all predicates in the argument
	debug_msg('insert_credibility(Argument, Cls_1, Ret_X) ', (insert_credibility(Argument, Cls_1, Ret_X))),
	insert_credibility(Argument, Cls_1, Ret_X),
	build_cred(NH, A, Op2, Ret_X, Op1, D, Cls_2).

insert_credibility((R , R_Cls), (RH, Ret_Cls), [X | Ret_X]) :-
	insert_credibility(R, RH, [X]),
	insert_credibility(R_Cls, Ret_Cls, Ret_X).	

% insert_credibility(H, Argument, Ret_Cls)
insert_credibility(R, RH, [X]) :-
	functor(R, FR, A),
	AH is A + 1,
	functor(RH, FR, AH),
	copy_args(A, RH, R),
	arg(AH, RH, X).

insert_credibility(R, Cls, X) :-
	debug_msg('ERROR: insert_credibility(R, Cls, X) ', (insert_credibility(R, Cls, X))),
	!, fail.

build_cred(H, A, Op2, ListVar, Op1, D, Cls) :-
	debug_msg('build_cred(H, A, Op2, ListVar, Op1, D, Cls) ', build_cred(H, A, Op2, ListVar, Op1, D, Cls)),
	arg(A, H, Mu),
	faggr(Op1, _IAny,  MAny1, _FAny),
	faggr(Op2, _IAny2, MAny2, _FAny2),
	Cls = (
		  inject(ListVar,  MAny2, Mu2),
		  Mu2 .>=. 0, Mu2 .=<. 1,
		  inject([Mu2, D], MAny1, Mu),
		  Mu  .>=. 0, Mu  .=<. 1
	      ).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

trans_each_property(F, H, Arity, Actual, Properties, Cls) :-
	debug_msg('trans_each_property', 
	trans_each_property(F, H, Arity, Actual, Properties) ),
	trans_each_property_aux(F, H, Arity, Actual, Properties, Cls),
	debug_msg('trans_each_property', 
	trans_each_property(F, H, Arity, Actual, Properties, Cls) ),
	!. % Backtracking not allowed here.

trans_each_property_aux(F, H, Arity, Actual, (P/1, R), (PF, List_Out)) :-
	Actual < Arity, % Security conditions.

	functor(PF, P, 1), % Build functor.
	arg(1,PF,X),       % Argument of functor is X.
	arg(Actual, H, X), % Unify with Argument of functor.

	NewActual is Actual + 1, % Next values.
	trans_each_property_aux(F, H, Arity, NewActual, R, List_Out).

trans_each_property_aux(_F, H, Arity, Actual, P/1, PF) :-
	Actual = Arity, % Security conditions.

	functor(PF, P, 1), % Build functor.
	arg(1,PF,X),       % Argument of functor is X.
	arg(Actual, H, X). % Unify with Argument of functor.

trans_each_property_aux(F, _H, _Arity, _Actual, _Properties, PF) :-
	build_fail_functor(PF),
	rfuzzy_warning_msg(F, ' types are not right. Please fix it. ').

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

build_straight_lines(F, [(X1,V1),(X2,V2)], [Point1, Line, Point2]) :-
	debug_msg('build_straight_lines', (build_straight_lines(F, [(X1,V1),(X2,V2)], [Point1, Line, Point2]))),
	build_point(F, X1, V1, Point1),
	build_line(F, X1, V1, X2, V2, Line),
	build_point(F, X2, V2, Point2).

build_straight_lines(F, [(X1,V1),(X2,V2)|List], [Point, Line|Cls]) :-
	debug_msg('build_straight_lines', (build_straight_lines(F, [(X1,V1),(X2,V2)|List], [Point, Line|Cls]))),
	build_point(F, X1, V1, Point),
	build_line(F, X1, V1, X2, V2, Line),
	build_straight_lines(F, [(X2,V2)|List],Cls).

build_point(F, X, V, (H :- X1 .=. X, V1 .=. V)) :-
	debug_msg('build_point', build_point(F, X, V, (H :- (H :- X1 .=. X, V1 .=. V)))),
	functor(H, F, 2),
 	arg(1,H,X1), % Put the individual into the functor.
	arg(2,H,V1). % Put the true value into the functor.

build_line(F, X1, V1, X2, V2, (H :- X .>. X1, X .<. X2, Cls_V)) :-
	debug_msg('build_line', (build_line(F, X1, V1, X2, V2, (H :- X .>. X1, X .<. X2, Cls_V)))),
	functor(H, F, 2),
	arg(1,H,X),

	number(X1), number(X2),
	number(V1), number(V2),
	X1 < X2, 

	!, % Backtracking is not allowed here.
	evaluate_V(H, X1, V1, X2, V2, Cls_V).

evaluate_V(H, _X1, Vf, _X2, Vf, (V .=. Vf)) :-
	arg(2,H,V). % Put the true value into the functor.

evaluate_V(H, X1, V1, X2, V2, (Pend .=. ((V2-V1)/(X2-X1)), V .=. V1+Pend*(X-X1))) :-
	X2 - X1 > 0,
	V1 \= V2,
	arg(1,H,X),
	arg(2,H,V).


