:- module(_1,[safe/2],[assertions,nativeprops,ciaopp(tests(resources)),predefres(res_all),basicmodes,regtypes]).

:- doc(author,"Nai-Wei Lin").

:- doc(author,"Edison Mera").

:- doc(module,"This program plays the n-queens game.").

:- resource res_steps.

:- head_cost(ub,res_steps,1).

:- literal_cost(ub,res_steps,0).

:- head_cost(lb,res_steps,1).

:- literal_cost(ub,res_steps,0).

:- trust_default+cost(ub,res_steps,0).

:- trust_default+cost(lb,res_steps,0).

:- entry safe(_1,_2)
         : ( int(_1), var(_2) ).

:- true pred safe(N,Queens)
         : ( int(N), term(Queens) )
        => ( int(N), list(Queens,^(q(num,num))) ).

:- true pred safe(N,Queens)
         : ( mshare([[Queens]]), var(Queens), ground([N]) )
        => ground([N,Queens]).

:- true pred safe(N,Queens)
         : ( int(N), var(Queens) )
        => ( int(N), list(Queens,^(q(num,num))) )
         + ( possibly_fails, covered ).

:- true pred safe(N,Queens)
         : ( int(N), var(Queens) )
        => ( int(N), list(Queens,^(q(num,num))), size(lb,N,int(N)), size(lb,Queens,int(N)) )
         + ( cost(lb,giunif,0), cost(lb,gounif,0), cost(lb,nargs,2), cost(lb,res_steps,1), cost(lb,steps,1), cost(lb,viunif,1), cost(lb,vounif,1) ).

:- true pred safe(N,Queens)
         : ( int(N), var(Queens) )
        => ( int(N), list(Queens,^(q(num,num))), size(ub,N,int(N)), size(ub,Queens,int(N)) )
         + ( cost(ub,giunif,sum($(j),1,int(N),6*(exp(int(N),$(j))* $(j)))+3*int(N)+1), cost(ub,gounif,2*int(N)+1), cost(ub,nargs,sum($(j),1,int(N),4*(exp(int(N),$(j))* $(j)))+2*(exp(int(N)-1,-1)*exp(int(N),int(N)+1))-2*(exp(int(N)-1,-1)*int(N))+3*int(N)+5), cost(ub,res_steps,sum($(j),1,int(N),2*(exp(int(N),$(j))* $(j)))+exp(int(N)-1,-1)*exp(int(N),int(N)+1)-exp(int(N)-1,-1)*int(N)+int(N)+2), cost(ub,steps,sum($(j),1,int(N),2*(exp(int(N),$(j))* $(j)))+exp(int(N)-1,-1)*exp(int(N),int(N)+1)-exp(int(N)-1,-1)*int(N)+int(N)+2), cost(ub,viunif,sum($(j),1,int(N),7*(exp(int(N),$(j))* $(j)))-4*(exp(int(N)-1,-1)*exp(int(N),int(N)+1))+4*(exp(int(N)-1,-1)*int(N))+2*int(N)+2), cost(ub,vounif,2*(exp(int(N)-1,-1)*exp(int(N),int(N)+1))-2*(exp(int(N)-1,-1)*int(N))+3*int(N)+1) ).

:- true pred safe(N,Queens)
         : ( int(N), var(Queens) )
        => ( int(N), list(Queens,^(q(num,num))), size_lb(N,int(N)), size_lb(Queens,int(N)), size_ub(N,int(N)), size_ub(Queens,int(N)) )
         + ( steps_lb(1), steps_ub(sum($(j),1,int(N),2*(exp(int(N),$(j))* $(j)))+exp(int(N)-1,-1)*exp(int(N),int(N)+1)-exp(int(N)-1,-1)*int(N)+int(N)+2) ).

safe(N,Queens) :-
        extend(N,N,Queens).

:- true pred extend(M,_1,_2)
         : ( num(M), int(_1), term(_2) )
        => ( num(M), int(_1), list(_2,^(q(num,num))) ).

:- true pred extend(M,_1,_2)
         : ( mshare([[_2]]), var(_2), ground([M,_1]) )
        => ground([M,_1,_2]).

:- true pred extend(M,_1,_2)
         : ( num(M), int(_1), var(_2) )
        => ( num(M), int(_1), list(_2,^(q(num,num))) )
         + ( possibly_fails, not_covered ).

:- true pred extend(M,_1,_2)
         : ( num(M), int(_1), var(_2) )
        => ( num(M), int(_1), list(_2,^(q(num,num))), size(lb,M,int(M)), size(lb,_1,int(_1)), size(lb,_2,int(M)) )
         + ( cost(lb,giunif,0), cost(lb,gounif,0), cost(lb,nargs,0), cost(lb,res_steps,0), cost(lb,steps,0), cost(lb,viunif,0), cost(lb,vounif,0) ).

:- true pred extend(M,_1,_2)
         : ( num(M), int(_1), var(_2) )
        => ( num(M), int(_1), list(_2,^(q(num,num))), size(ub,M,int(M)), size(ub,_1,int(_1)), size(ub,_2,int(M)) )
         + ( cost(ub,giunif,sum($(j),1,int(M),6*(exp(int(_1),$(j))* $(j)))+3*int(M)+1), cost(ub,gounif,2*int(M)+1), cost(ub,nargs,sum($(j),1,int(M),4*(exp(int(_1),$(j))* $(j)))+2*(exp(int(_1)-1,-1)*exp(int(_1),int(M)+1))-2*(exp(int(_1)-1,-1)*int(_1))+3*int(M)+3), cost(ub,res_steps,sum($(j),1,int(M),2*(exp(int(_1),$(j))* $(j)))+exp(int(_1)-1,-1)*exp(int(_1),int(M)+1)-exp(int(_1)-1,-1)*int(_1)+int(M)+1), cost(ub,steps,sum($(j),1,int(M),2*(exp(int(_1),$(j))* $(j)))+exp(int(_1)-1,-1)*exp(int(_1),int(M)+1)-exp(int(_1)-1,-1)*int(_1)+int(M)+1), cost(ub,viunif,sum($(j),1,int(M),7*(exp(int(_1),$(j))* $(j)))-4*(exp(int(_1)-1,-1)*exp(int(_1),int(M)+1))+4*(exp(int(_1)-1,-1)*int(_1))+2*int(M)+1), cost(ub,vounif,2*(exp(int(_1)-1,-1)*exp(int(_1),int(M)+1))-2*(exp(int(_1)-1,-1)*int(_1))+3*int(M)) ).

:- true pred extend(M,_1,_2)
         : ( num(M), int(_1), var(_2) )
        => ( num(M), int(_1), list(_2,^(q(num,num))), size_lb(M,int(M)), size_lb(_1,int(_1)), size_lb(_2,int(M)), size_ub(M,int(M)), size_ub(_1,int(_1)), size_ub(_2,int(M)) )
         + ( steps_lb(0), steps_ub(sum($(j),1,int(M),2*(exp(int(_1),$(j))* $(j)))+exp(int(_1)-1,-1)*exp(int(_1),int(M)+1)-exp(int(_1)-1,-1)*int(_1)+int(M)+1) ).

extend(0,_1,[]).
extend(M,N,[q(M,Q)|Selected]) :-
        M>0,
        M1 is M-1,
        extend(M1,N,Selected),
        choose(N,Q),
        consistent(q(M,Q),Selected).

:- true pred consistent(_1,_2)
         : ( rt4(_1), list(_2,^(q(num,num))) )
        => ( rt4(_1), list(_2,^(q(num,num))) ).

:- true pred consistent(_1,_2)
         : ground([_1,_2])
        => ground([_1,_2]).

:- true pred consistent(_1,_2)
         : ( rt4(_1), list(_2,^(q(num,num))) )
        => ( rt4(_1), list(_2,^(q(num,num))) )
         + ( possibly_fails, covered ).

:- true pred consistent(_1,_2)
         : ( rt4(_1), list(_2,^(q(num,num))) )
        => ( rt4(_1), list(_2,^(q(num,num))), size(lb,_1,size(_1)), size(lb,_2,length(_2)) )
         + ( cost(lb,giunif,1), cost(lb,gounif,0), cost(lb,nargs,2), cost(lb,res_steps,1), cost(lb,steps,1), cost(lb,viunif,1), cost(lb,vounif,0) ).

:- true pred consistent(_1,_2)
         : ( rt4(_1), list(_2,^(q(num,num))) )
        => ( rt4(_1), list(_2,^(q(num,num))), size(ub,_1,size(_1)), size(ub,_2,length(_2)) )
         + ( cost(ub,giunif,6*length(_2)+1), cost(ub,gounif,0), cost(ub,nargs,4*length(_2)+2), cost(ub,res_steps,2*length(_2)+1), cost(ub,steps,2*length(_2)+1), cost(ub,viunif,7*length(_2)+1), cost(ub,vounif,0) ).

:- true pred consistent(_1,_2)
         : ( rt4(_1), list(_2,^(q(num,num))) )
        => ( rt4(_1), list(_2,^(q(num,num))), size_lb(_1,size(_1)), size_lb(_2,length(_2)), size_ub(_1,size(_1)), size_ub(_2,length(_2)) )
         + ( steps_lb(1), steps_ub(2*length(_2)+1) ).

consistent(_1,[]).
consistent(Q,[Q1|Rest]) :-
        noattack(Q,Q1),
        consistent(Q,Rest).

:- true pred noattack(_1,_2)
         : ( rt4(_1), rt4(_2) )
        => ( rt4(_1), rt4(_2) ).

:- true pred noattack(_1,_2)
         : ground([_1,_2])
        => ground([_1,_2]).

:- true pred noattack(_1,_2)
         : ( rt4(_1), rt4(_2) )
        => ( rt4(_1), rt4(_2) )
         + ( possibly_fails, not_covered ).

:- true pred noattack(_1,_2)
         : ( rt4(_1), rt4(_2) )
        => ( rt4(_1), rt4(_2), size(lb,_1,size(_1)), size(lb,_2,size(_2)) )
         + ( cost(lb,giunif,0), cost(lb,gounif,0), cost(lb,nargs,0), cost(lb,res_steps,0), cost(lb,steps,0), cost(lb,viunif,0), cost(lb,vounif,0) ).

:- true pred noattack(_1,_2)
         : ( rt4(_1), rt4(_2) )
        => ( rt4(_1), rt4(_2), size(ub,_1,size(_1)), size(ub,_2,size(_2)) )
         + ( cost(ub,giunif,5), cost(ub,gounif,0), cost(ub,nargs,2), cost(ub,res_steps,1), cost(ub,steps,1), cost(ub,viunif,4), cost(ub,vounif,0) ).

:- true pred noattack(_1,_2)
         : ( rt4(_1), rt4(_2) )
        => ( rt4(_1), rt4(_2), size_lb(_1,size(_1)), size_lb(_2,size(_2)), size_ub(_1,size(_1)), size_ub(_2,size(_2)) )
         + ( steps_lb(0), steps_ub(1) ).

noattack(q(X1,Y1),q(X2,Y2)) :-
        Y1=\=Y2,
        X is X1-X2,
        Y is Y1-Y2,
        Z is Y2-Y1,
        X=\=Y,
        X=\=Z.

:- true pred choose(N,M)
         : ( num(N), term(M) )
        => ( num(N), num(M) ).

:- true pred choose(N,M)
         : ( mshare([[M]]), var(M), ground([N]) )
        => ground([N,M]).

:- true pred choose(N,M)
         : ( num(N), var(M) )
        => ( num(N), num(M) )
         + ( possibly_fails, not_covered ).

:- true pred choose(N,M)
         : ( num(N), var(M) )
        => ( num(N), num(M), size(lb,N,int(N)), size(lb,M,1) )
         + ( cost(lb,giunif,0), cost(lb,gounif,0), cost(lb,nargs,0), cost(lb,res_steps,0), cost(lb,steps,0), cost(lb,viunif,0), cost(lb,vounif,0) ).

:- true pred choose(N,M)
         : ( num(N), var(M) )
        => ( num(N), num(M), size(ub,N,int(N)), size(ub,M,int(N)+1) )
         + ( cost(ub,giunif,4*int(N)), cost(ub,gounif,0), cost(ub,nargs,4*int(N)), cost(ub,res_steps,2*int(N)), cost(ub,steps,2*int(N)), cost(ub,viunif,2*int(N)), cost(ub,vounif,2*int(N)) ).

:- true pred choose(N,M)
         : ( num(N), var(M) )
        => ( num(N), num(M), size_lb(N,int(N)), size_lb(M,1), size_ub(N,int(N)), size_ub(M,int(N)+1) )
         + ( steps_lb(0), steps_ub(2*int(N)) ).

choose(N,N) :-
        N>0.
choose(N,M) :-
        N>0,
        N1 is N-1,
        choose(N1,M).


:- regtype rt4/1.

rt4(q(A,B)) :-
        num(A),
        num(B).


