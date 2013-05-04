 
:- module(pool_,_,[assertions]).

%% Common predicates that don't go anywhere else...

recorded_internal(_,_,_).

recorda_internal(_,_,_).
recordz_internal(_,_,_).

there_is_delay:- fail.

meta_call(_):- fail.
peel_meta_call(_,_,_).
build_meta_call(_,_,_,_).

del_output(_,_,_).
%primes(_).
sum_list_of_lists(_,_,_).

group_calls(_,_,_,_,_,_,_,_).
type_assignments_included(_,_,_,_).
type_assignments_incompatible(_,_,_,_).

compute_lub_general(_,_,_).
del_compute_lub(_,_,_).
fd_extend(_,_,_,_).
fr_extend(_,_,_,_).
output_interface(_,_,_,_,_).

% GPS
no_instantiate_builtin(_).
no_instantiate_builtin_but_change(_).

