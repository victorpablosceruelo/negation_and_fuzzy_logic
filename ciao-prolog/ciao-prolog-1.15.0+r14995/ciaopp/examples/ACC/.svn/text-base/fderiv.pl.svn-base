:- module(_,[d/3],[assertions,regtypes,fsyntax]).

:- doc(title,"Symbolic Differentiation").

:- fun_eval d/2.

d(U+V,X)    :=  d(U,X) + d(V,X).
d(U-V,X)    :=  d(U,X) - d(V,X).
d(U*V,X)    :=  d(U,X) * V + U * d(V,X).
d(U/V,X)    := (d(U,X)*V - U*d(V,X)) / V^2.
% d(U^N,X)    :=  d(U,X) * N * U^N1 :- N1 is N - 1.
d(U^N,X)    :=  d(U,X) * N * U^N1 :- integer(N), N1 is N - 1.
d( -U,X)    := -d(U,X).
d(exp(U),X) :=  exp(U) * d(U,X).
d(log(U),X) :=  d(U,X) / U.
d(X,X)      :=  1.
d(~const,_) :=  0.
d(~number,_):=  0.

:- regtype const/1.

const := a | b | c.

