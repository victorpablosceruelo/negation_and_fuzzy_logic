/*-----------------------------------------------------------------------------
Program: Symbolic differentiation
Author:  
Date:    
-----------------------------------------------------------------------------*/
:- module(deriv,[d/3],[assertions]).

:- entry d(X,Y,Z)
	: ( ground([X,Y])
	  , var(Z)
	  ).

goal(Y):- expression(X), d(X, x, Y).

d(U+V,X,DU+DV)			:-  !,d(U,X,DU), d(V,X,DV).
d(U-V,X,DU-DV)			:-  !,d(U,X,DU), d(V,X,DV).
d(U*V,X, DU*V+U*DV)		:-  !,d(U,X,DU), d(V,X,DV).
d(U/V,X,(DU*V-U*DV)/V^2)	:-  !,d(U,X,DU), d(V,X,DV).
d(U^N,X, DU*N*U^N1)		:-  !,integer(N), N1 is N-1, d(U,X,DU).
d(-U,X,-DU)			:-  !,d(U,X,DU).
d(exp(U),X,exp(U)*DU)		:-  !,d(U,X,DU).
d(log(U),X,DU/U)		:-  !,d(U,X,DU).
d(X,X,1) :- !.
d(_C,_X,0).

expression( Exp
          + Exp
          - Exp
          * Exp
          / Exp
          * Exp
          / Exp
          ) :- value(Exp).

value(((3*x + (4*exp(x^3)*log(x^2)) -2) / ( -(3*x) + 5/(exp(x^4)+2)))).
