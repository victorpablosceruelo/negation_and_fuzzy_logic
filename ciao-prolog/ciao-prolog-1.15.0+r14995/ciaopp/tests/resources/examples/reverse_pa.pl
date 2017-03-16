:- module(_1,[reverse/2],[assertions,regtypes,ciaopp(tests(resources)),predefres(res_steps),nativeprops,basicmodes]).

:- doc(author,"Edison Mera").

:- doc(module,"This tests reverse2/3, which have a cumulative
	parameter.  To allow the analysis can work, we ignore it in
	the size analysis by the usage of the void keywork in the
	@pred{size_metric/2} property.").

:- entry reverse(X,Y)
         : ( var(Y), list(X,num) ).

:- true pred reverse(X,Y)
         : ( list(X,num), term(Y) )
        => ( list(X,num), list(Y,num) ).

:- true pred reverse(X,Y)
         : ( mshare([[Y]]), var(Y), ground([X]) )
        => ground([X,Y]).

:- true pred reverse(X,Y)
         : ( list(X,num), var(Y) )
        => ( list(X,num), list(Y,num) )
         + ( not_fails, covered ).

:- true pred reverse(X,Y)
         : ( list(X,num), var(Y) )
        => ( list(X,num), list(Y,num), size(lb,X,length(X)), size(lb,Y,0) )
         + cost(lb,steps,length(X)+2).

:- true pred reverse(X,Y)
         : ( list(X,num), var(Y) )
        => ( list(X,num), list(Y,num), size(ub,X,length(X)), size(ub,Y,bot) )
         + cost(ub,steps,length(X)+2).

:- true pred reverse(X,Y)
         : ( list(X,num), var(Y) )
        => ( list(X,num), list(Y,num), size_lb(X,length(X)), size_lb(Y,0), size_ub(X,length(X)), size_ub(Y,inf) )
         + ( steps_lb(length(X)+2), steps_ub(length(X)+2) ).

reverse(X,Y) :-
        reverse2(X,[],Y).

:- trust comp reverse2(A,B,C)
         + size_metric(B,void).

:- true pred reverse2(A,B,C)
         : ( list(A,num), list(B,num), term(C) )
        => ( list(A,num), list(B,num), list(C,num) ).

:- true pred reverse2(A,B,C)
         : ( mshare([[C]]), var(C), ground([A,B]) )
        => ground([A,B,C]).

:- true pred reverse2(A,B,C)
         : ( list(A,num), list(B,num), var(C) )
        => ( list(A,num), list(B,num), list(C,num) )
         + ( not_fails, covered ).

:- true pred reverse2(A,B,C)
         : ( list(A,num), list(B,num), var(C) )
        => ( list(A,num), list(B,num), list(C,num), size(lb,A,length(A)), size(lb,B,0), size(lb,C,0) )
         + cost(lb,steps,length(A)+1).

:- true pred reverse2(A,B,C)
         : ( list(A,num), list(B,num), var(C) )
        => ( list(A,num), list(B,num), list(C,num), size(ub,A,length(A)), size(ub,B,0), size(ub,C,bot) )
         + cost(ub,steps,length(A)+1).

:- true pred reverse2(A,B,C)
         : ( list(A,num), list(B,num), var(C) )
        => ( list(A,num), list(B,num), list(C,num), size_lb(A,length(A)), size_lb(B,0), size_lb(C,0), size_ub(A,length(A)), size_ub(B,0), size_ub(C,inf) )
         + ( steps_lb(length(A)+1), steps_ub(length(A)+1) ).

reverse2([],Y,Y).
reverse2([X|L],Y0,Y) :-
        reverse2(L,[X|Y0],Y).


