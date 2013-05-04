%% When a module with a dynamic, exported predicate with no
%% static clauses is analyzed, CiaoPP raises internal error
%% messages. 
%% The error is not raised when the module is loaded but 
%% when the analysis is performed. The error appears in 
%% the most common domains (def, shfr, eterms).
%%
%% CiaoPP output:
%% ?- module(dyn_exported),analyze(def).
%% {Loading current module from /home/jcorreas/Systems/CiaoDE/trunk/ciaopp/bugs/Pending/dyn_exported.pl
%% {loaded in 2702.589 msec.}
%% }
%% {Analyzing /home/jcorreas/Systems/CiaoDE/trunk/ciaopp/bugs/Pending/dyn_exported.pl
%% {preprocessed for plai in 2.0 msec.}
%% {ERROR (clause_db): Internal Error: clause_locator: 1st arg has to be a valid 
%%  clause key
%% }
%% {ERROR (clause_db): Internal Error: clause_locator: 1st arg has to be a valid 
%%  clause key
%% }
%% {WARNING (fixpo_ops): Unknown predicate dyn_exported:pp/2 in clause 0}
%% {analyzed by plai using def in 7.999 msec.}
%% }
%% 
%% yes
%% ?-

:- module(_, [pp/2]).

:- dynamic pp/2.
