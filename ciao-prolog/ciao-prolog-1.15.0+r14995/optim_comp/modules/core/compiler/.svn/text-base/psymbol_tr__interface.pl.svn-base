% Interface for compilation of 'psymbol' related code
% (execution of associated body, unification, creation, etc.)
:- interface psymbol_tr {
    :- '$statemodel'(single).

    {
    :- fluid wcode :: accum.

    :- multifile(tr_exec/1).
    :- multifile(tr_exec_as_semidet/1).
    :- multifile(tr_u_str_is/2). % compilation for u_str_is
    }.
    :- multifile(tr_exec_fun/2).
}.
