:- module(_1,_2,[assertions,ciaopp(tests(resources)),predefres(res_steps),nativeprops,basicmodes,regtypes]).

:- doc(author,"Edison Mera").

:- doc(module,"This example tests the existence of strong
	connected components that have bidirectional dependencies, in
	this example, between mylist/1 and mylist2/1.").

:- entry mylist(_1)
         : gnd(_1).

:- true pred mylist(_1)
         : term(_1)
        => list(_1).

:- true pred mylist(_1)
         : mshare([[_1]])
        => mshare([[_1]]).

:- true pred mylist(_1)
         : term(_1)
        => list(_1)
         + ( possibly_fails, not_covered ).

:- true pred mylist(_1)
         : term(_1)
        => ( list(_1), size(lb,_1,length(_1)) )
         + cost(lb,steps,0).

:- true pred mylist(_1)
         : term(_1)
        => ( list(_1), size(ub,_1,length(_1)) )
         + cost(ub,steps,2*length(_1)+1).

:- true pred mylist(_1)
         : term(_1)
        => ( list(_1), size_lb(_1,length(_1)), size_ub(_1,length(_1)) )
         + ( steps_lb(0), steps_ub(2*length(_1)+1) ).

mylist([]).
mylist([_1|Xs]) :-
        mylist2(Xs).

:- true pred mylist2(X)
         : term(X)
        => list(X).

:- true pred mylist2(X)
         : mshare([[X]])
        => mshare([[X]]).

:- true pred mylist2(X)
         : term(X)
        => list(X)
         + ( possibly_fails, covered ).

:- true pred mylist2(X)
         : term(X)
        => ( list(X), size(lb,X,length(X)) )
         + cost(lb,steps,1).

:- true pred mylist2(X)
         : term(X)
        => ( list(X), size(ub,X,length(X)) )
         + cost(ub,steps,2*length(X)+2).

:- true pred mylist2(X)
         : term(X)
        => ( list(X), size_lb(X,length(X)), size_ub(X,length(X)) )
         + ( steps_lb(1), steps_ub(2*length(X)+2) ).

mylist2(X) :-
        mylist(X).

:- entry myappend(_1,_2,_3)
         : ( list(_1,gnd), list(_2,gnd), var(_3) ).

:- true pred myappend(_1,X,_2)
         : ( list(_1,gnd), list(X,gnd), term(_2) )
        => ( list(_1,gnd), list(X,gnd), list(_2,gnd) ).

:- true pred myappend(_1,X,_2)
         : ( mshare([[_2]]), var(_2), ground([_1,X]) )
        => ground([_1,X,_2]).

:- true pred myappend(_1,X,_2)
         : ( list(_1,gnd), list(X,gnd), var(_2) )
        => ( list(_1,gnd), list(X,gnd), list(_2,gnd) )
         + ( not_fails, covered ).

:- true pred myappend(_1,X,_2)
         : ( list(_1,gnd), list(X,gnd), var(_2) )
        => ( list(_1,gnd), list(X,gnd), list(_2,gnd), size(lb,_1,length(_1)), size(lb,X,length(X)), size(lb,_2,length(X)+length(_1)) )
         + cost(lb,steps,length(_1)+1).

:- true pred myappend(_1,X,_2)
         : ( list(_1,gnd), list(X,gnd), var(_2) )
        => ( list(_1,gnd), list(X,gnd), list(_2,gnd), size(ub,_1,length(_1)), size(ub,X,length(X)), size(ub,_2,length(X)+length(_1)) )
         + cost(ub,steps,length(_1)+1).

:- true pred myappend(_1,X,_2)
         : ( list(_1,gnd), list(X,gnd), var(_2) )
        => ( list(_1,gnd), list(X,gnd), list(_2,gnd), size_lb(_1,length(_1)), size_lb(X,length(X)), size_lb(_2,length(X)+length(_1)), size_ub(_1,length(_1)), size_ub(X,length(X)), size_ub(_2,length(X)+length(_1)) )
         + ( steps_lb(length(_1)+1), steps_ub(length(_1)+1) ).

myappend([],X,X).
myappend([X|Xs],Y,[X|Zs]) :-
        myappend(Xs,Y,Zs).


