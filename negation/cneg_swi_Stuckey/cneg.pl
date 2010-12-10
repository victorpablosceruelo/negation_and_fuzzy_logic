%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Package  CNEG is part of the constructive negation implementation.
%
% Original by Susana Munoz Hernandez.
% Modified by Victor Pablos Ceruelo
%
% Distributed without any warranty.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- if(prolog_load_context(module, 'cneg')).
:- module(cneg).
:- endif.

% Needed to be able to compile the modules.
:- use_module(cneg_tr).

% trans_sent/3 makes the transformation.
% See discussion on the SWI Prolog mailing list for December 9-10 2003.

%:- multifile
%        user:goal_expansion/2.
%:- dynamic
%        user:goal_expansion/2.
%
%user:goal_expansion( A, B ) :- 
%  trans_goal( A, B, 'unknown').

:- multifile
        user:term_expansion/2.
:- dynamic
        user:term_expansion/2.

user:term_expansion( Input, Output ) :-
	prolog_load_context(module, FileName),
	trans_sent( Input, Output, FileName).


