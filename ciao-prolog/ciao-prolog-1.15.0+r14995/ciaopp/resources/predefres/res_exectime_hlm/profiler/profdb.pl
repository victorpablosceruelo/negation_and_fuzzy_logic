:- module(profdb, _, []).

:- data builtin_time_pred/2.

% step_time(Name, Constant, Min, Max, Relation, Error, PercentError),

% contains the coefficients of the cost model (may be more than 1)

:- data global_model_params/2.

:- data model_params/3.
:- data data_table/5.

cleanup_data_table_db :-
	retractall_fact(data_table(_, _, _, _, _)).
