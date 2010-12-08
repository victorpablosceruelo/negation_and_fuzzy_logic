%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Package  CNEG is part of the constructive negation implementation.
%
% Original by Susana Munoz Hernandez.
% Modified by Victor Pablos Ceruelo
%
% Distributed without any warranty.
% Works on Ciao Prolog r11293.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- module(cneg,
	  [
	   trans_sent/3
	   ]).
% To be able to call Pred from cneg

% Needed to be able to compile the modules.
:- use_module(cneg_lib).
:- use_module(cneg_aux).  
% NO:  :- include(cneg_lib).  
:- use_module(cneg_diseq, 
	[
	    cneg_diseq/3, cneg_eq/2,
	    put_universal_quantification/1,
	    remove_universal_quantification/2,
	    keep_universal_quantification/1
	]).
:- use_module(cneg_tr).
%:- use_module(library(write), [write/1]).
%:- use_module(library(system), [getenvstr/2]).

% Re-export predicates to use them in console.
:- reexport(cneg_diseq,
	[
	    cneg_diseq/3, cneg_eq/2,
	    put_universal_quantification/1,
	    remove_universal_quantification/2,
	    keep_universal_quantification/1
	]).   
:- reexport(cneg_lib, [ cneg_lib_aux/3 ]).   
%:- export([main/0, main/1]).

% To access predicates from anywhere.
:- multifile cneg_processed_pred/4.
:- multifile cneg_dynamic_cl/6.
:- multifile cneg_static_cl/3.

% cneg_tr contains the code transformation needed by cneg_lib
%:- load_compilation_module(library('cneg/cneg_tr')). CUANDO SEA LIBRERIA
%:- load_compilation_module(.('cneg_tr')).

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

user:term_expansion( A, B ) :-
  trans_sent( A, B, 'unknown').


