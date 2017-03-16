:- module(_1,[eval_polynom/3],[assertions,ciaopp(tests(resources)),library(resdefs(resources_decl)),nativeprops,basicmodes,regtypes]).

:- load_resource_module(eval_polynom_res).

:- resource error_propagation.

:- resource fpu_time.

:- head_cost(ub,fpu_time,0).

:- literal_cost(ub,fpu_time,'eval_polynom:call_fpu_time').

:- trust_default+cost(ub,fpu_time,0).

:- entry eval_polynom(A,B,C)
         : ( list(A,num), num(B), var(C) ).

:- true pred eval_polynom(A,B,C)
         : ( list(A,num), num(B), term(C) )
        => ( list(A,num), num(B), num(C) ).

:- true pred eval_polynom(A,B,C)
         : ( mshare([[C]]), var(C), ground([A,B]) )
        => ground([A,B,C]).

:- true pred eval_polynom(A,B,C)
         : ( list(A,num), num(B), var(C) )
        => ( list(A,num), num(B), num(C) )
         + ( not_fails, covered ).

:- true pred eval_polynom(A,B,C)
         : ( list(A,num), num(B), var(C) )
        => ( list(A,num), num(B), num(C), size(ub,A,length(A)), size(ub,B,int(B)), size(ub,C,bot) )
         + cost(ub,fpu_time,0).

eval_polynom([],_X,0).
eval_polynom([C|Cs],X,Value) :-
        eval_polynom(Cs,X,Value0),
        Value is C+X*Value0.


