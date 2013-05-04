:- module(mmatf, [mmultiply/3], [fsyntax, assertions, nativeprops]).

:- entry mmultiply/3 : ground * ground * var.

:- fun_eval vmul/2.

vmul([],[])            := 0.
vmul([H1|T1], [H2|T2]) := H1 * H2 + vmul(T1,T2).

%% vmul(X,Y) := (X = [],      Y = [])      ? 0.
%%            | (X = [H1|T1], Y = [H2|T2]) ? H1 * H2 + vmul(T1,T2).

:- fun_eval multiply/2.

multiply([],_)          := [].
multiply([V0|Rest], V1) := [ vmul(V0,V1) | multiply(Rest, V1) ] .

:- fun_eval mmultiply/2.

mmultiply([],_)          := [].
mmultiply([V0|Rest], V1) := [ multiply(V1,V0) | mmultiply(Rest, V1) ] .


