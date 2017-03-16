:- module(_, [provided_analysis/1,
		is_complexity_analysis/1,
		is_single_complexity_analysis/1,
		is_size_analysis/1,
		is_time_analysis/1,
		is_ualb_complexity_analysis/3,
		ualb_complexity_to_analysis/2],
	    [assertions, ciaopp(analysis_register)]).

:- use_module(library(aggregates), [findall/3]).

:- comment(doc, "Edison Mera").

% WARNING: This file must be self-contained and can not import modules
% from ciaopp -- EMM.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of ciaopp(analysis_register) multifile hooks:
lazy_analysis(Analysis) :- provided_analysis(Analysis).

analysis_module(Analysis, ciaopp(infercost)) :-
	provided_analysis(Analysis).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

provided_analysis(size_ub).
provided_analysis(size_lb).
provided_analysis(size_ualb).
provided_analysis(size_o).
provided_analysis(steps_ub).
provided_analysis(steps_lb).
provided_analysis(steps_ualb).
provided_analysis(steps_o).

is_time_analysis(steps_lb).
is_time_analysis(steps_ub).
is_time_analysis(steps_o).

is_size_analysis(size_lb).
is_size_analysis(size_ub).
is_size_analysis(size_o).

is_single_complexity_analysis(Analysis) :-
	is_time_analysis(Analysis).
is_single_complexity_analysis(Analysis) :-
	is_size_analysis(Analysis).

ualb_complexity_to_analysis(steps_ualb, steps_lb).
ualb_complexity_to_analysis(steps_ualb, steps_ub).
ualb_complexity_to_analysis(size_ualb,  size_lb).
ualb_complexity_to_analysis(size_ualb,  size_ub).

is_ualb_complexity_analysis(A, Lower, Upper) :-
	findall(CA, ualb_complexity_to_analysis(A, CA), [Lower, Upper]).

is_complexity_analysis(Analysis) :-
	is_single_complexity_analysis(Analysis).
is_complexity_analysis(Analysis) :-
	is_ualb_complexity_analysis(Analysis, _, _).
