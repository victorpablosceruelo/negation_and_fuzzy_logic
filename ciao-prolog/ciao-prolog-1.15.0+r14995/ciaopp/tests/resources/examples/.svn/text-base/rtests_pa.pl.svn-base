:- module(_1,_2,[assertions,basicmodes,regtypes,ciaopp(tests(resources)),nativeprops]).

:- doc(author,"Edison Mera").

:- doc(module,"This module test the trust_default declarations
	that let us to configure default values for the resources when
	no analysis or trusted cost is available.").

:- resource res_1.

:- trust_default+cost(ub,res_1,0).

:- resource res_2.

:- trust_default+cost(ub,res_2,inf).

:- resource res_3.

:- literal_cost(ub,res_1,0).

:- literal_cost(ub,res_2,0).

:- literal_cost(ub,res_3,0).

:- impl_defined([s/1]).

%% %% :- trust pred p(X,Y)
%% %%    : ( nonvar(X), nonvar(Y) )
%% %%    + head_cost(ub,res_2,1).

%% %% :- trust pred p(X,Y)
%% %%    : ( nonvar(X), nonvar(Y) )
%% %%    + head_cost(ub,res_3,1).

:- entry p(X,Y)
         : ( nonvar(X), nonvar(Y) ).

:- check calls p(X,Y)
         : ( nonvar(X), nonvar(Y) ).

:- trust comp p(X,Y)
         : ( nonvar(X), nonvar(Y) )
         + head_cost(ub,res_2,1).

:- trust comp p(X,Y)
         : ( nonvar(X), nonvar(Y) )
         + head_cost(ub,res_3,1).

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
         + ( cost(ub,res_1,inf), cost(ub,res_2,inf), cost(ub,res_3,17) ).

p(X,Y) :-
        q(X),
        r(Y),
        s(X).

:- trust comp q(X)
         : nonvar(X)
         + cost(ub,res_1,6).

:- trust comp q(X)
         : nonvar(X)
         + cost(ub,res_2,6).

:- trust comp q(X)
         : nonvar(X)
         + cost(ub,res_3,6).

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
         + ( cost(ub,res_1,6), cost(ub,res_2,6), cost(ub,res_3,6) ).

q(_1).

:- trust comp r(X)
         : nonvar(X)
         + cost(ub,res_1,5).

:- trust comp r(X)
         : nonvar(X)
         + cost(ub,res_2,5).

:- trust comp r(X)
         : nonvar(X)
         + cost(ub,res_3,5).

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
         + ( cost(ub,res_1,5), cost(ub,res_2,5), cost(ub,res_3,5) ).

r(_1).


