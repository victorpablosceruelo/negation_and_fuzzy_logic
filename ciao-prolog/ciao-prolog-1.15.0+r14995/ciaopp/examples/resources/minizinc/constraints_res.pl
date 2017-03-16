:- package(constraints_res).

:- use_package(resdefs).

% Definition of some constraint related resources.

:- resource constraints.
:- trust_default + (cost(ub, constraints, 0), cost(lb, constraints, 0)).
:- head_cost(ub,constraints,0).
:- literal_cost(ub,constraints,0).

:- resource numvars.
:- trust_default + (cost(ub, numvars, 0), cost(lb, numvars, 0)).
:- head_cost(ub,numvars,0).
:- literal_cost(ub,numvars,0).

:- resource alldiff.
:- trust_default + (cost(ub, alldiff, 0), cost(lb, alldiff, 0)).
:- head_cost(ub,alldiff,0).
:- literal_cost(ub,alldiff,0).

% Loads the definitions of the functions declared in head_cost and literal_cost 
% so that they can be executed during the resource analysis. 

% :- load_resource_module(resources(resources_basic)).
