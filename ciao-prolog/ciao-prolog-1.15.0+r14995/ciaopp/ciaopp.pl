:- module(ciaopp, [], [condcomp, assertions, ciaopp_options]).
% TODO: Can I use the 'ciaopaths' package instead?
:- load_compilation_module(library(bundle_registry(bundle_registry_load))).

:- if(defined(mini_pp)).
% Experimental version of CiaoPP with reduced footprint.
%
% Enable by adding the declarations below to 'ciaopp_options.pl':
%   :- compilation_fact(mini_pp).
%
% TODO: Integrate with CiaoPP (i.e. keep a minimal driver and use lazy
% modules or dynamic loading of domains/transformations)
% TODO: Move inside ciaopp_template
:- reexport(ciaopp(mini_driver)).
:- reexport(ciaopp(mini_printer)).
:- else.
:- include(ciaopp(ciaopp_template)).
:- endif.
