:- module(diff_equation, [], []).

:- reexport(infercost(diff_equation(diff_equ)), [solve_diff_equ/7, solve_typed_diff_equ/8]).
:- reexport(infercost(diff_equation(first_order)), [solve_fode/5]).
