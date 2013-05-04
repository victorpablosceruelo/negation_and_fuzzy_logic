:- module(_1,[main/1],[assertions,ciaopp(tests(resources)),predefres(res_steps),nativeprops,basicmodes,regtypes]).

:- doc(author,"Edison Mera").

:- doc(module,"Of course, the trust assertion is wrong.  What we
	are testing here is the warning messages that appears when
	analyzing this program.  Also the analyzer must terminate.").

:- entry main(_1)
         : var(_1).

:- true pred main(A)
         : term(A)
        => rt0(A).

:- true pred main(A)
         : ( mshare([[A]]), var(A) )
        => ground([A]).

:- true pred main(A)
         : var(A)
        => rt0(A)
         + ( not_fails, covered ).

:- true pred main(A)
         : var(A)
        => ( rt0(A), size(lb,A,0) )
         + cost(lb,steps,2).

:- true pred main(A)
         : var(A)
        => ( rt0(A), size(ub,A,bot) )
         + cost(ub,steps,2).

:- true pred main(A)
         : var(A)
        => ( rt0(A), size_lb(A,4), size_ub(A,4) )
         + ( steps_lb(2), steps_ub(2) ).

main(A) :-
        test1(A).

%% %% :- trust pred test1(A)
%% %%    + size_metric(A,length(A)).

:- trust comp test1(A)
         + size_metric(A,length(A)).

:- true pred test1(A)
         : term(A)
        => rt0(A).

:- true pred test1(A)
         : ( mshare([[A]]), var(A) )
        => ground([A]).

:- true pred test1(A)
         : var(A)
        => rt0(A)
         + ( not_fails, covered ).

:- true pred test1(A)
         : var(A)
        => ( rt0(A), size(lb,A,0) )
         + cost(lb,steps,1).

:- true pred test1(A)
         : var(A)
        => ( rt0(A), size(ub,A,inf) )
         + cost(ub,steps,1).

:- true pred test1(A)
         : var(A)
        => ( rt0(A), size_lb(A,4), size_ub(A,4) )
         + ( steps_lb(1), steps_ub(1) ).

test1(A) :-
        A=[1,2,3,4].


:- regtype rt0/1.

rt0([1,2,3,4]).


