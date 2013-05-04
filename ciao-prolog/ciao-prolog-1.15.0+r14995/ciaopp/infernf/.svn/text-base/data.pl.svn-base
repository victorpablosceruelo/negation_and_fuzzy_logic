:- data(user_dec/1).
:- data(trust_dec/4).

%% moved from nf.pl (PBC)
%% create_worst_case_types(Head, [ground(Varlist)]):-
%% % create_worst_case_types(Head, [gnd(Varlist)]):-
%%     closed_var_list(Head, Varlist).

create_worst_case_types(Head, Types) :-
	closed_var_list(Head, Varlist),
	assign_term_type_to_variables(Varlist, Types).

assign_term_type_to_variables([Var|R], [Prop|RT]) :-
	set_top_type(F),
	Prop =.. [F, Var],
	assign_term_type_to_variables(R, RT).
assign_term_type_to_variables([], []).

:- pred nf_get_trust_dec(+Pred, -Head, -InTypes, -OuTypes)

# "Get the type information of predicate @var{Pred}. If there is no
  type information, then a safe information is assumed. In the
  non-failure analysis it is assumed that all arguments are ground,
  both at call. In the determinism analysis it is assumed that all
  arguments are free, at call. The sucess type information is irrelevant in both analysis.
  In fact it shuld not be output.".

nf_get_trust_dec(Pred, Head, InTypes, OuTypes) :-
	trust_dec(Pred, Head, InTypes, OuTypes),
	!.
nf_get_trust_dec(F/A, Head, Types, Types) :-
	functor(Head, F, A),
	create_worst_case_types(Head, Types).
