:- module( _, [pred2/1 , pred1/1], [assertions , nativeprops , regtypes] ).


:- use_package(nativeprops).

:- new_declaration((resource)/1).

:- op(1170,fx,resource).

:- new_declaration((head_cost)/3).

:- op(1170,fx,head_cost).

:- new_declaration((literal_cost)/3).

:- op(1170,fx,literal_cost).

:- new_declaration((trust_default)/1).

:- op(1170,fx,trust_default).

:- new_declaration((granularity_resources)/1).

:- op(1170,fx,granularity_resources).

:- new_declaration((java_mode)/2).

:- op(1170,fx,java_mode).

:- new_declaration((java_measure)/2).

:- op(1170,fx,java_measure).

:- new_declaration(load_resource_module/1).

:- load_compilation_module(library(resdefs(resources_tr))).

:- add_sentence_trans(add_mod_to_directives/3, 820).

:- new_declaration((platform)/1).

:- op(1170,fx,platform).

:- new_declaration(compound_resource/2).

:- new_declaration(platform_constants/3).

:- use_package(library(resdefs(resources_decl))).

:- resource steps.

:- use_package(res_steps(res_steps_decl)).

:- head_cost(ub,steps,1).

:- head_cost(lb,steps,1).

:- literal_cost(ub,steps,0).

:- literal_cost(lb,steps,0).

:- trust_default+cost(ub,steps,0).

:- trust_default+cost(lb,steps,0).

:- use_package(res_steps(res_steps_assr)).

:- entry pred1(A)
         : int(A).

:- true pred pred1(A)
         : int(A)
        => rt0(A).

:- true pred pred1(A)
         : ground([A])
        => ground([A]).

:- true pred pred1(A)
         : int(A)
        => rt0(A)
         + ( possibly_fails, covered ).

:- true pred pred1(A)
         : int(A)
        => ( rt0(A), size(lb,A,int(A)) )
         + cost(lb,steps,1).

:- true pred pred1(A)
         : int(A)
        => ( rt0(A), size(ub,A,int(A)) )
         + cost(ub,steps,4).

:- true pred pred1(A)
         : int(A)
        => ( rt0(A), size_lb(A,int(A)), size_ub(A,int(A)) )
         + ( steps_lb(1), steps_ub(2) ).

pred1(A) :-
        multitype(A) .

:- entry pred2(A)
         : atm(A).

:- true pred pred2(A)
         : atm(A)
        => rt1(A).

:- true pred pred2(A)
         : ground([A])
        => ground([A]).

:- true pred pred2(A)
         : atm(A)
        => rt1(A)
         + ( possibly_fails, covered ).

:- true pred pred2(A)
         : atm(A)
        => ( rt1(A), size(lb,A,size(A)) )
         + cost(lb,steps,1).

:- true pred pred2(A)
         : atm(A)
        => ( rt1(A), size(ub,A,size(A)) )
         + cost(ub,steps,8).

:- true pred pred2(A)
         : atm(A)
        => ( rt1(A), size_lb(A,size(A)), size_ub(A,size(A)) )
         + ( steps_lb(1), steps_ub(2) ).

pred2(A) :-
        multitype(A) .

%% %% :- trust pred multitype(_1)
%% %%    : gnd(_1)
%% %%    + ( not_fails, is_det ).

%% %% :- trust pred multitype(_1)
%% %%    : int(_1)
%% %%    + ( cost(ub,steps,3), cost(lb,steps,2) ).

%% %% :- trust pred multitype(_1)
%% %%    : atm(_1)
%% %%    + ( cost(ub,steps,7), cost(lb,steps,5) ).

:- check calls multitype(_1)
         : ( gnd(_1); int(_1); atm(_1) ).

:- trust comp multitype(_1)
         : gnd(_1)
         + ( not_fails, is_det ).

:- trust comp multitype(_1)
         : int(_1)
         + ( cost(ub,steps,3), cost(lb,steps,2) ).

:- trust comp multitype(_1)
         : atm(_1)
         + ( cost(ub,steps,7), cost(lb,steps,5) ).

:- true pred multitype(_1)
         : atm_or_int(_1)
        => rt3(_1).

:- true pred multitype(_1)
         : ground([_1])
        => ground([_1]).

:- true pred multitype(_1)
         : atm_or_int(_1)
        => rt3(_1)
         + ( possibly_fails, not_covered ).

:- true pred multitype(_1)
         : atm_or_int(_1)
        => ( rt3(_1), size(lb,_1,size(_1)) )
         + cost(lb,steps,0).

:- true pred multitype(_1)
         : atm_or_int(_1)
        => ( rt3(_1), size(ub,_1,size(_1)) )
         + cost(ub,steps,1).

:- true pred multitype(_1)
         : atm_or_int(_1)
        => ( rt3(_1), size_lb(_1,size(_1)), size_ub(_1,size(_1)) )
         + ( steps_lb(0), steps_ub(1) ).

multitype(a).
multitype(1).


:- regtype rt3/1.

rt3(1).
rt3(a).


:- regtype rt0/1.

rt0(1).


:- regtype rt1/1.

rt1(a).


