:- module(_, [provided_analysis/1], [assertions, ciaopp(analysis_register)]).

:- doc(author, "Edison Mera").

% WARNING: This file must be self-contained and must not import other modules
% from ciaopp -- EMM.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of ciaopp(analysis_register) multifile hooks:
lazy_analysis(Analysis) :- provided_analysis(Analysis).

analysis_module(Analysis, ciaopp(resources)) :-
	provided_analysis(Analysis).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

provided_analysis(resources).
provided_analysis(size). % Only to preserve old functionality - EMM
