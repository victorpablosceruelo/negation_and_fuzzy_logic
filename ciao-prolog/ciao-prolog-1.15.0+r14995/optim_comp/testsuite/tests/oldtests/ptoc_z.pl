:- module(_, _, []).

% 11-queens program (obtain all the solutions)

:- compilation_fact(use_ptoc).
%:- compilation_fact(use_ptoc_spec).

:- if(use_ptoc). 
:- '$ptoc_type'(intlist, (list ; atomic([]))).
:- endif.

:- if(use_ptoc). 
:- '$preddef'(no_attack_2/3, ptoc).
:- '$ptoc_prop'('ptoc_z:no_attack_2'/3, [call_types=[intlist, smallint, smallint], exit_types=[intlist, smallint, smallint]]).
:- '$ptoc_prop'('ptoc_z:no_attack_2'/3, [argmodes=[in,in,in]]).
%:- '$ptoc_prop'('ptoc_z:no_attack_2'/3, [argmems=[x(0),cvar,cvar]]).
:- '$ptoc_prop'('ptoc_z:no_attack_2'/3, [argderefs=[true,true,true]]).
:- '$ptoc_prop'('ptoc_z:no_attack_2'/3, [imp=semidet]).
%:- '$ptoc_prop'('ptoc_z:no_attack_2'/3, [should_trim_frame=no]).
:- endif.
no_attack_2([], _Queen, _Nb).
:- if(use_ptoc).
no_attack_2([Y|Ys], Queen, Nb) :-
	'$trust_type'(Y, smallint),
        A is Y + Nb,
	Queen =\= A,
        B is Y - Nb,
	Queen =\= B,
        Nb1 is Nb + 1,
        no_attack_2(Ys, Queen, Nb1).
:- else.
no_attack_2([Y|Ys], Queen, Nb) :-
        A is Y + Nb,
	Queen =\= A,
        B is Y - Nb,
	Queen =\= B,
        Nb1 is Nb + 1,
        no_attack_2(Ys, Queen, Nb1).
:- endif.
