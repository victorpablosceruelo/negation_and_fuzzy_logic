:- module(_1,[zebra/7],[assertions,nativeprops,regtypes,ciaopp(tests(resources)),predefres(res_steps),basicmodes]).

:- entry zebra(A,B,C,D,E,F,G)
         : ( mshare([[A],[B],[C],[D],[E],[F],[G]]), var(A), var(B), var(C), var(D), var(E), var(F), var(G) ).

:- true pred zebra(A,B,C,D,E,F,G)
         : ( term(A), term(B), term(C), term(D), term(E), term(F), term(G) )
        => ( rt38(A), rt38(B), rt38(C), rt38(D), rt7(E), rt38(F), rt38(G) ).

:- true pred zebra(A,B,C,D,E,F,G)
         : ( mshare([[A],[B],[C],[D],[E],[F],[G]]), var(A), var(B), var(C), var(D), var(E), var(F), var(G) )
        => ground([A,B,C,D,E,F,G]).

:- true pred zebra(A,B,C,D,E,F,G)
         : ( var(A), var(B), var(C), var(D), var(E), var(F), var(G) )
        => ( rt38(A), rt38(B), rt38(C), rt38(D), rt7(E), rt38(F), rt38(G) )
         + ( possibly_fails, covered ).

:- true pred zebra(A,B,C,D,E,F,G)
         : ( var(A), var(B), var(C), var(D), var(E), var(F), var(G) )
        => ( rt38(A), rt38(B), rt38(C), rt38(D), rt7(E), rt38(F), rt38(G), size(lb,A,0), size(lb,B,0), size(lb,C,0), size(lb,D,0), size(lb,E,0), size(lb,F,0), size(lb,G,0) )
         + cost(lb,steps,1).

:- true pred zebra(A,B,C,D,E,F,G)
         : ( var(A), var(B), var(C), var(D), var(E), var(F), var(G) )
        => ( rt38(A), rt38(B), rt38(C), rt38(D), rt7(E), rt38(F), rt38(G), size(ub,A,inf), size(ub,B,inf), size(ub,C,inf), size(ub,D,inf), size(ub,E,inf), size(ub,F,bot), size(ub,G,bot) )
         + cost(ub,steps,2048*(sum($(j),0,5,2*(exp(fact($(j)),-1)*fact(5)* $(j)))*sum($(j),0,5,exp(fact($(j)),-1)*fact(5)))+2048*(sum($(j),0,5,2*(exp(fact($(j)),-1)*fact(5)* $(j)))*exp(sum($(j),0,5,exp(fact($(j)),-1)*fact(5)),4))+2048*(sum($(j),0,5,2*(exp(fact($(j)),-1)*fact(5)* $(j)))*exp(sum($(j),0,5,exp(fact($(j)),-1)*fact(5)),3))+2048*(sum($(j),0,5,2*(exp(fact($(j)),-1)*fact(5)* $(j)))*exp(sum($(j),0,5,exp(fact($(j)),-1)*fact(5)),2))+2048*sum($(j),0,5,2*(exp(fact($(j)),-1)*fact(5)* $(j)))+2048*(sum($(j),0,5,2*(exp(fact($(j)),-1)*fact(5)))*sum($(j),0,5,exp(fact($(j)),-1)*fact(5)))+2048*(sum($(j),0,5,2*(exp(fact($(j)),-1)*fact(5)))*exp(sum($(j),0,5,exp(fact($(j)),-1)*fact(5)),4))+2048*(sum($(j),0,5,2*(exp(fact($(j)),-1)*fact(5)))*exp(sum($(j),0,5,exp(fact($(j)),-1)*fact(5)),3))+2048*(sum($(j),0,5,2*(exp(fact($(j)),-1)*fact(5)))*exp(sum($(j),0,5,exp(fact($(j)),-1)*fact(5)),2))+2048*sum($(j),0,5,2*(exp(fact($(j)),-1)*fact(5)))+2048*sum($(j),0,5,exp(fact($(j)),-1)*fact(5))+2048*exp(sum($(j),0,5,exp(fact($(j)),-1)*fact(5)),4)+2048*exp(sum($(j),0,5,exp(fact($(j)),-1)*fact(5)),3)+2048*exp(sum($(j),0,5,exp(fact($(j)),-1)*fact(5)),2)+4389).

zebra(Englishman,Spaniard,Japanese,Ukrainian,Norwegian,Zebra,Water) :-
        Englishman=Red,
        Spaniard=Dog,
        Green=Coffee,
        Ukrainian=Tea,
        to_the_right(Green,Ivory),
        Winston=Snails,
        Kool=Yellow,
        Milk=third,
        Norwegian=first,
        next_to(Fox,Chesterfield),
        next_to(Horse,Kool),
        Lucky=Juice,
        Japanese=Parliament,
        next_to(Norwegian,Blue),
        houses([Blue,Green,Red,Yellow,Ivory]),
        houses([Norwegian,Englishman,Spaniard,Japanese,Ukrainian]),
        houses([Dog,Zebra,Fox,Snails,Horse]),
        houses([Parliament,Kool,Lucky,Chesterfield,Winston]),
        houses([Milk,Juice,Water,Tea,Coffee]).

:- true pred houses(Prop)
         : rt521(Prop)
        => rt520(Prop).

:- true pred houses(Prop)
         : mshare([[Prop]])
        => ground([Prop]).

:- true pred houses(Prop)
         : rt521(Prop)
        => rt520(Prop)
         + ( possibly_fails, covered ).

:- true pred houses(Prop)
         : rt521(Prop)
        => ( rt520(Prop), size(lb,Prop,0) )
         + cost(lb,steps,1).

:- true pred houses(Prop)
         : rt521(Prop)
        => ( rt520(Prop), size(ub,Prop,5) )
         + cost(ub,steps,sum($(j),0,5,2*(exp(fact($(j)),-1)*fact(5)* $(j)))+sum($(j),0,5,2*(exp(fact($(j)),-1)*fact(5)))+1).

houses(Prop) :-
        domain(Prop,[first,second,third,fourth,fifth]).

:- true pred domain(_2,_1)
         : ( list(_2), list(_1,rt38) )
        => ( list(_2,rt38), list(_1,rt38) ).

:- true pred domain(_2,_1)
         : ( mshare([[_2]]), ground([_1]) )
        => ground([_2,_1]).

:- true pred domain(_2,_1)
         : ( list(_2), list(_1,rt38) )
        => ( list(_2,rt38), list(_1,rt38) )
         + ( possibly_fails, covered ).

:- true pred domain(_2,_1)
         : ( list(_2), list(_1,rt38) )
        => ( list(_2,rt38), list(_1,rt38), size(lb,_2,0), size(lb,_1,length(_1)) )
         + cost(lb,steps,1).

:- true pred domain(_2,_1)
         : ( list(_2), list(_1,rt38) )
        => ( list(_2,rt38), list(_1,rt38), size(ub,_2,length(_1)), size(ub,_1,length(_1)) )
         + cost(ub,steps,sum($(j),0,length(_1),2*(exp(fact($(j)),-1)*fact(length(_1))* $(j)))+sum($(j),0,length(_1),2*(exp(fact($(j)),-1)*fact(length(_1))))).

domain([],_1).
domain([X|Rest],Domain) :-
        select(X,Domain,NewDomain),
        domain(Rest,NewDomain).

:- true pred select(X,_1,R)
         : ( term(X), list(_1,rt38), term(R) )
        => ( rt38(X), rt177(_1), list(R,rt38) ).

:- true pred select(X,_1,R)
         : ( mshare([[X],[R]]), var(R), ground([_1]) )
        => ground([X,_1,R]).

:- true pred select(X,_1,R)
         : ( term(X), list(_1,rt38), var(R) )
        => ( rt38(X), rt177(_1), list(R,rt38) )
         + ( possibly_fails, not_covered ).

:- true pred select(X,_1,R)
         : ( term(X), list(_1,rt38), var(R) )
        => ( rt38(X), rt177(_1), list(R,rt38), size(lb,X,0), size(lb,_1,length(_1)), size(lb,R,length(_1)-1) )
         + cost(lb,steps,0).

:- true pred select(X,_1,R)
         : ( term(X), list(_1,rt38), var(R) )
        => ( rt38(X), rt177(_1), list(R,rt38), size(ub,X,bot), size(ub,_1,length(_1)), size(ub,R,length(_1)-1) )
         + cost(ub,steps,2*length(_1)).

select(X,[X|R],R).
select(X,[Y|R],[Y|Rest]) :-
        select(X,R,Rest).

:- true pred next_to(_1,_2)
         : ( term(_1), term(_2) )
        => ( rt38(_1), rt38(_2) ).

:- true pred next_to(_1,_2)
         : ( mshare([[_1],[_2]]), var(_2) )
        => ground([_1,_2]).

:- true pred next_to(_1,_2)
         : ( term(_1), var(_2) )
        => ( rt38(_1), rt38(_2) )
         + ( possibly_fails, not_covered ).

:- true pred next_to(_1,_2)
         : ( term(_1), var(_2) )
        => ( rt38(_1), rt38(_2), size(lb,_1,1), size(lb,_2,1) )
         + cost(lb,steps,0).

:- true pred next_to(_1,_2)
         : ( term(_1), var(_2) )
        => ( rt38(_1), rt38(_2), size(ub,_1,1), size(ub,_2,1) )
         + cost(ub,steps,8).

next_to(fifth,fourth).
next_to(fourth,fifth).
next_to(fourth,third).
next_to(third,fourth).
next_to(third,second).
next_to(second,third).
next_to(second,first).
next_to(first,second).

:- true pred to_the_right(_1,_2)
         : ( term(_1), term(_2) )
        => ( rt12(_1), rt13(_2) ).

:- true pred to_the_right(_1,_2)
         : ( mshare([[_1],[_2]]), var(_1), var(_2) )
        => ground([_1,_2]).

:- true pred to_the_right(_1,_2)
         : ( var(_1), var(_2) )
        => ( rt12(_1), rt13(_2) )
         + ( not_fails, covered ).

:- true pred to_the_right(_1,_2)
         : ( var(_1), var(_2) )
        => ( rt12(_1), rt13(_2), size(lb,_1,1), size(lb,_2,1) )
         + cost(lb,steps,1).

:- true pred to_the_right(_1,_2)
         : ( var(_1), var(_2) )
        => ( rt12(_1), rt13(_2), size(ub,_1,1), size(ub,_2,1) )
         + cost(ub,steps,4).

to_the_right(fifth,fourth).
to_the_right(fourth,third).
to_the_right(third,second).
to_the_right(second,first).


:- regtype rt38/1.

rt38(fifth).
rt38(first).
rt38(fourth).
rt38(second).
rt38(third).


:- regtype rt521/1.

rt521([A,B,C,D,E]) :-
        term(A),
        term(B),
        term(C),
        term(D),
        term(E).


:- regtype rt520/1.

rt520([A,B,C,D,E]) :-
        rt38(A),
        rt38(B),
        rt38(C),
        rt38(D),
        rt38(E).


:- regtype rt177/1.

rt177([A|B]) :-
        rt38(A),
        list(B,rt38).


:- regtype rt12/1.

rt12(fifth).
rt12(fourth).
rt12(second).
rt12(third).


:- regtype rt13/1.

rt13(first).
rt13(fourth).
rt13(second).
rt13(third).


:- regtype rt7/1.

rt7(first).


