:- module(_, _, [assertions]).

:- use_module(library(math)).
:- use_module(profcost(profconfig)).
:- use_module(profcost(caliresource(caliresource_basic))).


params_MRSS(Params, MRSS) :-
	params_RSS(Params, RSS),
	params_Rows(Params, Rows),
	params_Cols(Params, Cols),
	mrss(RSS, Rows, Cols, MRSS).

params_Covariance(Params, Covariance) :-
	params_XXT(Params, XXT),
	params_Cols(Params, Cols),
	params_MRSS(Params, MRSS),
	covariance(XXT, Cols, MRSS, Covariance).

params_Variance(Params, Variance) :-
	params_Covariance(Params, Covariance),
	variance(Covariance, Variance).

params_StdError(Params, StdError) :-
	params_Variance(Params, Variance),
	stderror(Variance, StdError).

params_TValue(Params, TValue) :-
	params_Solution(Params, Solution),
	params_StdError(Params, StdError),
	tvalue(StdError, Solution, TValue).

params_LowerBoundSolution(Params, LowerBoundSolution) :-
	confidence_interval(Z),
	params_BoundSolution(-Z, Params, LowerBoundSolution).

params_UpperBoundSolution(Params, LowerBoundSolution) :-
	confidence_interval(Z),
	params_BoundSolution(Z, Params, LowerBoundSolution).

params_BoundSolution(Z, Params, BoundSolution) :-
	params_Solution(Params, Solution),
	params_StdError(Params, StdError),
	bound_solution(StdError, Z, Solution, BoundSolution).
