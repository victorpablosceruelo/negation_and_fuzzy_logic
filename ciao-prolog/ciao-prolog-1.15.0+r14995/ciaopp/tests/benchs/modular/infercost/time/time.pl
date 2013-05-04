%
%  time.pl			Nai-Wei Lin			December, 1991
%
%  This file contains the procedures for performing the time
%  analysis for the predicates in the program in topologically sorted order.
%

%
%  Perform the time analysis for a strongly connected component.
%
:- module(time,[ time_analysis/9 ],[]).

:- use_module('..'(algebraic)).
:- use_module('..'(dependency)).
:- use_module('..'(diff_equation)).
:- use_module('..'(init)).
:- use_module('..'(size)).
:- use_module('..'(solution)).
:- use_module('..'(top)).

:- use_module('..'(database), 
	[ approximation/1,
          db_get/1,
          flag_is_not_fails/1,
          flag_is_possibly_fails/1,
          flag_is_covered/1,
%          flag_is_not_covered/1,
%          flag_set_not_fails/1,
          flag_set_possibly_fails/1,
%          flag_set_covered/1,
          flag_set_not_covered/1
	]).
% messages (must go after database) MH
:- use_module(library(messages), [debug_message/2]).

:- push_prolog_flag(multi_arity_warnings,off).

:- include(time_).

:- pop_prolog_flag(multi_arity_warnings).

%% Control version comment prompting for the file.
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "off"
%% End:

