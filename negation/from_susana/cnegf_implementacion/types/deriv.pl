:- module(deriv, [d/3], [assertions,regtypes]).
% :- module(deriv, [d/3], [assertions, types]).

% :- use_module(library(lists), [list/2, list1/2]).

:- entry d(X,Y,Z): (expression(X), ground(X), ground(Y), var(Z)).

 %% :- typedef expression::= 0;
 %%                          1;
 %%                          2;
 %%                          ^(-expression);
 %%                          ^log(expression);
 %%                          ^exp(expression);
 %%                          ^(expression^expression);
 %%                          ^(expression/expression);
 %%                          ^(expression-expression);
 %%                          ^(expression*expression);
 %%                          ^(expression+expression).

%% :- typedef expression ::=  num;
%%                           ^(-expression);
%%                           ^log(expression);
%%                           ^exp(expression);
%%                           ^(expression^expression);
%%                           ^(expression/expression);
%%                           ^(expression-expression);
%%                           ^(expression*expression);
%%                           ^(expression+expression).
    
:- regtype expression/1.

expression(X):- num(X).
expression(-(X)):- expression(X).
expression(log(X)):- expression(X).
expression(exp(X)):- expression(X).
expression(^(X,Y)):- expression(X), expression(Y). 
expression(/(X,Y)):- expression(X), expression(Y). 
expression(-(X,Y)):- expression(X), expression(Y). 
expression(*(X,Y)):- expression(X), expression(Y). 
expression(+(X,Y)):- expression(X), expression(Y). 

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


 %% expression( Exp
 %%           + Exp
 %%           - Exp
 %%           * Exp
 %%           / Exp
 %%           * Exp
 %%           / Exp
 %%           ) :- value(Exp).
 %% 
 %% value(((3*x + (4*exp(x^3)*log(x^2)) -2) / ( -(3*x) + 5/(exp(x^4)+2)))).







%% Control version comment prompting for the file.
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "off"
%% End:

