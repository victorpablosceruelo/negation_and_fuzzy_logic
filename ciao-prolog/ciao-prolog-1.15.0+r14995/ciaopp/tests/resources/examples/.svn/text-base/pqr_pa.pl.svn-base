:- module(_1,_2,[assertions,regtypes,ciaopp(tests(resources)),library(resdefs(resources_decl)),nativeprops,basicmodes]).

:- load_resource_module(ciaopp(tests(resources(examples(pqr_rt))))).

:- resource res_steps.

:- resource res_steps2.

:- head_cost(ub,res_steps,'pqr:f_res_steps_delta').

:- literal_cost(ub,res_steps,0).

:- head_cost(ub,res_steps2,'pqr:f_res_steps2_delta').

:- literal_cost(ub,res_steps2,0).

:- entry p(X,Y)
         : ( gnd(X), gnd(Y) ).

:- true pred p(X,Y)
         : ( gnd(X), gnd(Y) )
        => ( gnd(X), gnd(Y) ).

:- true pred p(X,Y)
         : ground([X,Y])
        => ground([X,Y]).

:- true pred p(X,Y)
         : ( gnd(X), gnd(Y) )
        => ( gnd(X), gnd(Y) )
         + ( not_fails, covered ).

:- true pred p(X,Y)
         : ( gnd(X), gnd(Y) )
        => ( gnd(X), gnd(Y), size(ub,X,size(X)), size(ub,Y,size(Y)) )
         + ( cost(ub,res_steps,12), cost(ub,res_steps2,24) ).

:- true pred p(X,Y)
         : ( gnd(X), gnd(Y) )
        => ( gnd(X), gnd(Y), size_lb(X,size(X)), size_lb(Y,size(Y)), size_ub(X,size(X)), size_ub(Y,size(Y)) )
         + ( steps_lb(3), steps_ub(3) ).

p(X,Y) :-
        q(X),
        r(Y).

:- trust comp q(X)
         : gnd(X)
         + ( cost(ub,res_steps,6), cost(ub,res_steps2,12) ).

:- true pred q(X)
         : term(X)
        => term(X).

:- true pred q(X)
         : mshare([[X]])
        => mshare([[X]]).

:- true pred q(X)
         : term(X)
        => term(X)
         + ( not_fails, covered ).

:- true pred q(X)
         : term(X)
        => ( term(X), size(ub,X,size(X)) )
         + ( cost(ub,res_steps,inf), cost(ub,res_steps2,inf) ).

:- true pred q(X)
         : term(X)
        => ( term(X), size_lb(X,size(X)), size_ub(X,size(X)) )
         + ( steps_lb(1), steps_ub(1) ).

q(_1).

:- trust comp r(X)
         : gnd(X)
         + ( cost(ub,res_steps,5), cost(ub,res_steps2,10) ).

:- true pred r(X)
         : term(X)
        => term(X).

:- true pred r(X)
         : mshare([[X]])
        => mshare([[X]]).

:- true pred r(X)
         : term(X)
        => term(X)
         + ( not_fails, covered ).

:- true pred r(X)
         : term(X)
        => ( term(X), size(ub,X,size(X)) )
         + ( cost(ub,res_steps,inf), cost(ub,res_steps2,inf) ).

:- true pred r(X)
         : term(X)
        => ( term(X), size_lb(X,size(X)), size_ub(X,size(X)) )
         + ( steps_lb(1), steps_ub(1) ).

r(_1).


