:- module(_1,[count/1,mytest/0],[assertions,regtypes,ciaopp(tests(resources)),predefres(res_steps),nativeprops,basicmodes]).

:- doc(author,"Edison Mera").

:- doc(module,"This tests reverse counting and a call to
	@pred{count0/2} predicate with the input argument instantiated
	in mytest/0.").

:- entry count(_1)
         : num(_1).

:- true pred count(N)
         : num(N)
        => num(N).

:- true pred count(N)
         : ground([N])
        => ground([N]).

:- true pred count(N)
         : num(N)
        => num(N)
         + ( not_fails, covered ).

:- true pred count(N)
         : num(N)
        => ( num(N), size(lb,N,int(N)) )
         + cost(lb,steps,2).

:- true pred count(N)
         : num(N)
        => ( num(N), size(ub,N,int(N)) )
         + cost(ub,steps,int(N)+2).

:- true pred count(N)
         : num(N)
        => ( num(N), size_lb(N,int(N)), size_ub(N,int(N)) )
         + ( steps_lb(2), steps_ub(int(N)+2) ).

count(N) :-
        count0(N,0).

:- trust comp count0(A,B)
         + size_metric(B,void).

:- true pred count0(A,B)
         : ( num(A), num(B) )
        => ( num(A), num(B) ).

:- true pred count0(A,B)
         : ground([A,B])
        => ground([A,B]).

:- true pred count0(A,B)
         : ( num(A), num(B) )
        => ( num(A), num(B) )
         + ( not_fails, covered ).

:- true pred count0(A,B)
         : ( num(A), num(B) )
        => ( num(A), num(B), size(lb,A,int(A)), size(lb,B,0) )
         + cost(lb,steps,1).

:- true pred count0(A,B)
         : ( num(A), num(B) )
        => ( num(A), num(B), size(ub,A,int(A)), size(ub,B,0) )
         + cost(ub,steps,int(A)+1).

:- true pred count0(A,B)
         : ( num(A), num(B) )
        => ( num(A), num(B), size_lb(A,int(A)), size_lb(B,0), size_ub(A,int(A)), size_ub(B,0) )
         + ( steps_lb(1), steps_ub(int(A)+1) ).

count0(N,M) :-
        N>0,
        !,
        N1 is N-1,
        M1 is M+1,
        count0(N1,M1).
count0(_1,_2).

:- entry mytest.

:- true pred mytest.

:- true pred mytest.

:- true pred mytest
         + ( not_fails, covered ).

:- true pred mytest
         + cost(lb,steps,2).

:- true pred mytest
         + cost(ub,steps,7).

:- true pred mytest
         + ( steps_lb(2), steps_ub(7) ).

mytest :-
        count0(5,0).


