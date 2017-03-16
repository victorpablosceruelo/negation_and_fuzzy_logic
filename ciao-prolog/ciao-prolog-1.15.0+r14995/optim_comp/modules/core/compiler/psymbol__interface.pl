% A 'psymbol' (program symbol, which can be a predicate or function
% symbol, associated to a known or unknown module)
:- interface psymbol {
    :- '$statemodel'(single).

    :- multifile(get_with_worker/1).
    :- multifile(get_det_mode/1).
}.
