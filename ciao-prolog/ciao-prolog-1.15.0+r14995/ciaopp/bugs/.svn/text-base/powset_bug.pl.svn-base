:- module(powset_bug, [powset/2], [assertions, regtypes]).

:- entry powset(A, B) : int_list * var.

:- regtype int_list/1.

int_list([]).
int_list([X|L]) :-
	int(X),
	int_list(L).

powset([], [[]]).
powset([X|L], P) :-
	powset(L, P0),
	append_elem(P0, X, P, P0).

append_elem([],    _X,          T, T).
append_elem([L|Ls], X, [[X|L]|Rs], T) :-
	append_elem(Ls, X, Rs, T).

% ciaopp ?- module(powset).
% {Loading current module from /home/edison/svn/CiaoDE/ciaopp/bugs/powset.pl
% {loaded in 4016.230585669615 msec.}
% }

% yes
% ciaopp ?- analyze(eterms).
% {Analyzing /home/edison/svn/CiaoDE/ciaopp/bugs/powset.pl
% {preprocessed for plai in 20.57272828899338 msec.}

% Ciao interruption (h for help)? a
% { Execution aborted }
% ciaopp ?- 

