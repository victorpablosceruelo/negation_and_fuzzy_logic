:- module(_1,[powset/2],[assertions,regtypes,nativeprops,ciaopp(examples(resources(exectimehl))),basicmodes]).

:- doc(bug,"Currently the analyzer is unable to see that the 3th
	argument of append_elem/4 can be safely ignored. It has been
	marked as void but it is not being ignored when creating the
	adg. -- EMM").

:- entry powset(A,B)
         : ( list(A,int), var(B) ).

:- true pred powset(A,B)
         : ( list(A,int), term(B) )
        => ( list(A,int), rt144(B) ).

:- true pred powset(A,B)
         : ( mshare([[B]]), var(B), ground([A]) )
        => ground([A,B]).

:- true pred powset(A,B)
         : ( list(A,int), var(B) )
        => ( list(A,int), rt144(B) )
         + ( not_fails, covered ).

:- true pred powset(A,B)
         : ( list(A,int), var(B) )
        => ( list(A,int), rt144(B), size(lb,A,length(A)), size(lb,B,0) )
         + cost(lb,exectime_model4,881.3764275860481*length(A)+870.536829326745).

:- true pred powset(A,B)
         : ( list(A,int), var(B) )
        => ( list(A,int), rt144(B), size(ub,A,length(A)), size(ub,B,bot) )
         + cost(ub,exectime_model4,inf).

powset(A,B) :-
        powset2(A,[[]],B).

:- trust comp powset2(A,B,C)
         + size_metric(B,void).

:- true pred powset2(A,B,C)
         : ( list(A,int), rt144(B), term(C) )
        => ( list(A,int), rt144(B), rt144(C) ).

:- true pred powset2(A,B,C)
         : ( mshare([[C]]), var(C), ground([A,B]) )
        => ground([A,B,C]).

:- true pred powset2(A,B,C)
         : ( list(A,int), rt144(B), var(C) )
        => ( list(A,int), rt144(B), rt144(C) )
         + ( not_fails, covered ).

:- true pred powset2(A,B,C)
         : ( list(A,int), rt144(B), var(C) )
        => ( list(A,int), rt144(B), rt144(C), size(lb,A,length(A)), size(lb,B,0), size(lb,C,0) )
         + cost(lb,exectime_model4,881.3764275860481*length(A)+378.4291019696336).

:- true pred powset2(A,B,C)
         : ( list(A,int), rt144(B), var(C) )
        => ( list(A,int), rt144(B), rt144(C), size(ub,A,length(A)), size(ub,B,0), size(ub,C,bot) )
         + cost(ub,exectime_model4,inf).

powset2([],X,X).
powset2([X|L],P0,P) :-
        append_elem(P0,X,P1,P0),
        powset2(L,P1,P).

:- trust comp append_elem(A,B,C,D)
         + size_metric(C,void).

:- true pred append_elem(A,B,C,D)
         : ( list(A,nlist(int)), int(B), term(C), rt144(D) )
        => ( list(A,nlist(int)), int(B), rt144(C), rt144(D) ).

:- true pred append_elem(A,B,C,D)
         : ( mshare([[C]]), var(C), ground([A,B,D]) )
        => ground([A,B,C,D]).

:- true pred append_elem(A,B,C,D)
         : ( list(A,nlist(int)), int(B), var(C), rt144(D) )
        => ( list(A,nlist(int)), int(B), rt144(C), rt144(D) )
         + ( not_fails, covered ).

:- true pred append_elem(A,B,C,D)
         : ( list(A,nlist(int)), int(B), var(C), rt144(D) )
        => ( list(A,nlist(int)), int(B), rt144(C), rt144(D), size(lb,A,length(A)), size(lb,B,int(B)), size(lb,C,0), size(lb,D,length(D)) )
         + cost(lb,exectime_model4,691.0216228808929*length(A)+433.8555520315054).

:- true pred append_elem(A,B,C,D)
         : ( list(A,nlist(int)), int(B), var(C), rt144(D) )
        => ( list(A,nlist(int)), int(B), rt144(C), rt144(D), size(ub,A,length(A)), size(ub,B,int(B)), size(ub,C,0), size(ub,D,length(D)) )
         + cost(ub,exectime_model4,737.0203590950605*length(A)+469.5480920740583).

append_elem([],_X,T,T).
append_elem([L|Ls],X,[[X|L]|Rs],T) :-
        append_elem(Ls,X,Rs,T).


:- regtype rt145/1.

rt145([]).
rt145([A|B]) :-
        int(A),
        nlist(B,int).


:- regtype rt144/1.

rt144([A|B]) :-
        rt145(A),
        list(B,nlist(int)).


