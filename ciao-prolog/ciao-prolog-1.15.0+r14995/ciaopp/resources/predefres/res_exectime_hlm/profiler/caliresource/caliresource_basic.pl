:- module(_, _, [assertions]).

% to do: make this operations lazy

params_Rows(params(_Rows, _Cols, _XXT, _Solution, _RSS), _Rows).
params_Cols(params(_Rows, _Cols, _XXT, _Solution, _RSS), _Cols).
params_XXT(params(_Rows, _Cols, _XXT, _Solution, _RSS), _XXT).
params_Solution(params(_Rows, _Cols, _XXT, _Solution, _RSS), _Solution).
params_RSS(params(_Rows, _Cols, _XXT, _Solution, _RSS), _RSS).
