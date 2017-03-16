:- module(polynom_comp,[polynom_current_assertion/1,polynom_message/7],[condcomp, assertions]).
% change note:
% - polynom.pl luthfi darmawan Sept 2008
% - renamed into polynom_comp.pl [luthfi darmawan July 2010]
% - the folder is combined together with algebraic
:- doc(filetype, documentation).

:- doc(title,"Polynomial Functions Comparison").  
:- doc(author,"Luthfi Darmawan").  

:- doc(summary,"This module provides procedure for comparing
   polynomial functions").
 
:- doc(module,"This module provides procedure for comparing polynomial
   functions and some other mathematical functions that can be
   approximated using polynomial functions. It also provides utility
   predicates for working with polynomials, e.g. normalization, and
   mathematic computation.").

:- reexport(infercost('algebraic/polynom_comp_gsl'),
	[
	    polynom_root_interval/3,
	    difference/3,
	    validate_polynom/2,
	    compute_safe_intervals/4
	]).

:- reexport(infercost('algebraic/normal_form_polynom'),
	[
	    polynomize/2
	]).

:- reexport(infercost('algebraic/math_polynom'),
	[
	    brute_eval_intervals/3,
	    eval_arith/3
	]).

:- data polynom_current_assertion/1.
:- data polynom_message/7.


