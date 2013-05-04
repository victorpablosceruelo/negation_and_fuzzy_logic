:- module(_1,[p/2,delta_res_steps/2,call_res_steps/2],[assertions,basicmodes,regtypes,ciaopp(tests(resources)),library(resdefs(resources_decl)),nativeprops]).

:- resource res_steps.

:- literal_cost(ub,res_steps,'pqr_cm:call_res_steps').

:- load_resource_module(ciaopp(tests(resources(examples(pqr_cm))))).

:- trust comp p(X,Y)
         + ( head_cost(ub,res_steps,1), literal_cost(ub,res_steps,0) ).

:- entry p(X,Y)
         : ( nonvar(X), nonvar(Y) ).

:- true pred p(X,Y)
         : ( term(X), term(Y) )
        => ( term(X), term(Y) ).

:- true pred p(X,Y)
         : mshare([[X],[X,Y],[Y]])
        => mshare([[X],[X,Y],[Y]]).

:- true pred p(X,Y)
         : ( term(X), term(Y) )
        => ( term(X), term(Y) )
         + ( not_fails, covered ).

:- true pred p(X,Y)
         : ( term(X), term(Y) )
        => ( term(X), term(Y), size(ub,X,size(X)), size(ub,Y,size(Y)) )
         + cost(ub,res_steps,12).

:- true pred p(X,Y)
         : ( term(X), term(Y) )
        => ( term(X), term(Y), size_lb(X,size(X)), size_lb(Y,size(Y)), size_ub(X,size(X)), size_ub(Y,size(Y)) )
         + ( steps_lb(3), steps_ub(3) ).

p(X,Y) :-
        q(X),
        r(Y).

:- trust comp q(X)
         : nonvar(X)
         + cost(ub,res_steps,6).

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
         + cost(ub,res_steps,6).

:- true pred q(X)
         : term(X)
        => ( term(X), size_lb(X,size(X)), size_ub(X,size(X)) )
         + ( steps_lb(1), steps_ub(1) ).

q(_1).

:- trust comp r(X)
         : nonvar(X)
         + cost(ub,res_steps,5).

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
         + cost(ub,res_steps,5).

:- true pred r(X)
         : term(X)
        => ( term(X), size_lb(X,size(X)), size_ub(X,size(X)) )
         + ( steps_lb(1), steps_ub(1) ).

r(_1).

:- true pred delta_res_steps(_1,_2)
         : ( term(_1), term(_2) )
        => ( term(_1), rt1(_2) ).

:- true pred delta_res_steps(_1,_2)
         : mshare([[_1],[_1,_2],[_2]])
        => ( mshare([[_1]]), ground([_2]) ).

:- true pred delta_res_steps(_1,_2)
         : ( term(_1), term(_2) )
        => ( term(_1), rt1(_2) )
         + ( possibly_fails, not_covered ).

delta_res_steps(_1,1) :- !.

:- true pred call_res_steps(_1,_2)
         : ( term(_1), term(_2) )
        => ( term(_1), rt0(_2) ).

:- true pred call_res_steps(_1,_2)
         : mshare([[_1],[_1,_2],[_2]])
        => ( mshare([[_1]]), ground([_2]) ).

:- true pred call_res_steps(_1,_2)
         : ( term(_1), term(_2) )
        => ( term(_1), rt0(_2) )
         + ( possibly_fails, not_covered ).

call_res_steps(_1,0) :- !.


:- regtype rt0/1.

rt0(0).


:- regtype rt1/1.

rt1(1).


