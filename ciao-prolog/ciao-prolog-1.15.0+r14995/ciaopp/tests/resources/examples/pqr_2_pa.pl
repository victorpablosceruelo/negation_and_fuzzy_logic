:- module(_1,_2,[assertions,basicmodes,regtypes,ciaopp(tests(resources)),library(resdefs(resources_decl)),nativeprops]).

:- load_resource_module(ciaopp(tests(resources(examples(pqr_2_rt))))).

:- resource calls_to_r.

:- head_cost(ub,calls_to_r,'pqr_2:f_calls_to_r').

:- literal_cost(ub,calls_to_r,0).

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
         + cost(ub,calls_to_r,1).

:- true pred p(X,Y)
         : ( term(X), term(Y) )
        => ( term(X), term(Y), size_lb(X,size(X)), size_lb(Y,size(Y)), size_ub(X,size(X)), size_ub(Y,size(Y)) )
         + ( steps_lb(6), steps_ub(6) ).

p(X,Y) :-
        q(X),
        r(Y),
        a(X).

:- true pred q(_1)
         : term(_1)
        => term(_1).

:- true pred q(_1)
         : mshare([[_1]])
        => mshare([[_1]]).

:- true pred q(_1)
         : term(_1)
        => term(_1)
         + ( not_fails, covered ).

:- true pred q(_1)
         : term(_1)
        => ( term(_1), size(ub,_1,size(_1)) )
         + cost(ub,calls_to_r,0).

:- true pred q(_1)
         : term(_1)
        => ( term(_1), size_lb(_1,size(_1)), size_ub(_1,size(_1)) )
         + ( steps_lb(1), steps_ub(1) ).

q(_1).

%% %% :- trust pred r(X)
%% %%    : nonvar(X)
%% %%    + cost(ub,calls_to_r,1).

:- check calls r(X)
         : nonvar(X).

:- trust comp r(X)
         : nonvar(X)
         + cost(ub,calls_to_r,1).

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
         + cost(ub,calls_to_r,1).

:- true pred r(X)
         : term(X)
        => ( term(X), size_lb(X,size(X)), size_ub(X,size(X)) )
         + ( steps_lb(1), steps_ub(1) ).

r(_1).

:- true pred a(X)
         : term(X)
        => term(X).

:- true pred a(X)
         : mshare([[X]])
        => mshare([[X]]).

:- true pred a(X)
         : term(X)
        => term(X)
         + ( not_fails, covered ).

:- true pred a(X)
         : term(X)
        => ( term(X), size(ub,X,size(X)) )
         + cost(ub,calls_to_r,0).

:- true pred a(X)
         : term(X)
        => ( term(X), size_lb(X,size(X)), size_ub(X,size(X)) )
         + ( steps_lb(3), steps_ub(3) ).

a(X) :-
        q(X),
        s(X).

:- true pred s(_1)
         : term(_1)
        => term(_1).

:- true pred s(_1)
         : mshare([[_1]])
        => mshare([[_1]]).

:- true pred s(_1)
         : term(_1)
        => term(_1)
         + ( not_fails, covered ).

:- true pred s(_1)
         : term(_1)
        => ( term(_1), size(ub,_1,size(_1)) )
         + cost(ub,calls_to_r,0).

:- true pred s(_1)
         : term(_1)
        => ( term(_1), size_lb(_1,size(_1)), size_ub(_1,size(_1)) )
         + ( steps_lb(1), steps_ub(1) ).

s(_1).


