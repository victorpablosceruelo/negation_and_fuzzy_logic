:- module(thresholds, [find_size_threshold/4], [assertions]). 

:- doc(author,"Pedro L@'{o}pez").  

:- use_module(library(lists), [append/3]). 

:- use_module(infercost(algebraic), 
	[
	    max/3, 
	    min/3, 
	    pow/3, 
	    logar/3, 
	    factorial/2 
	]).

:- use_module(g_utility, [find_value/3]). 

:- push_prolog_flag(multi_arity_warnings,off).


find_size_threshold(Expression, [(Var, Value)], GrainSize, SizeThreshold) :-
	evaluate_expression(Expression, [(Var, Value)], ExpValue),
        (
	    ExpValue >= GrainSize -> SizeThreshold is Value - 1
	;
	    (
		NewValue is Value + 1, 
		find_size_threshold(Expression, [(Var, NewValue)], GrainSize, 
                                  SizeThreshold)
	    )
	).

evaluate_expression(X, _, X):- 
        number(X),
        !.
evaluate_expression($(N), VarValues, Y) :-
        find_value(VarValues, $(N), Y).
evaluate_expression($(M, N), VarValues, Y) :-
	integer(N), 
        integer(M),
        !,
        find_value(VarValues, $(M, N), Y).
evaluate_expression(X, VarValues, Y) :-
	size_variable(X),
        !,
        find_value(VarValues, X, Y).
evaluate_expression(X+Y, VarValues, Z) :-
	evaluate_expression(X,VarValues,X1),
	evaluate_expression(Y,VarValues,Y1),
	Z is X1+Y1.
evaluate_expression(X-Y,VarValues,Z) :-
	evaluate_expression(X,VarValues,X1),
	evaluate_expression(Y,VarValues,Y1),
	Z is X1-Y1.
evaluate_expression(X*Y,VarValues,Z) :-
	evaluate_expression(X,VarValues,X1),
	evaluate_expression(Y,VarValues,Y1),
	Z is X1*Y1.
evaluate_expression(X/Y,VarValues,Z) :-
	evaluate_expression(X,VarValues,X1),
	evaluate_expression(Y,VarValues,Y1),
	Z is X1/Y1.
evaluate_expression(-X, VarValues, Y):-
        !,
        evaluate_expression(X,VarValues,X1),
	Y is X1 * -1.
evaluate_expression(X, VarValues, Y) :-
	functor(X, sum, 4),
        !,
	sum_evaluate_expression(X, VarValues, Y).

%%% Warning!, use functions of the new version.

evaluate_expression(exp(X,Y),VarValues,Z) :-
	evaluate_expression(X,VarValues,X1),
	evaluate_expression(Y,VarValues,Y1),
	pow(X1, Y1, Z).

evaluate_expression(log(X,Y),VarValues,Z) :-
	evaluate_expression(X,VarValues,X1),
	evaluate_expression(Y,VarValues,Y1),
	logar(X1, Y1, Z).

evaluate_expression(fact(X),VarValues,Y) :-
	evaluate_expression(X,VarValues,X1),
	factorial(X1, Y).

evaluate_expression(max(X,Y),VarValues,Z) :-
	evaluate_expression(X,VarValues,X1),
	evaluate_expression(Y,VarValues,Y1),
	max(X1, Y1, Z).

evaluate_expression(min(X,Y),VarValues,Z) :-
	evaluate_expression(X,VarValues,X1),
	evaluate_expression(Y,VarValues,Y1),
	min(X1, Y1, Z).

%% evaluate_expression(exp(X,Y),VarValues,Z) :-
%% 	evaluate_expression(X,VarValues,X1),
%% 	evaluate_expression(Y,VarValues,Y1),
%% 	Z is exp(X1,Y1).
%% 
%% evaluate_expression(log(X,Y),VarValues,Z) :-
%% 	evaluate_expression(X,VarValues,X1),
%% 	evaluate_expression(Y,VarValues,Y1),
%% 	Z is log(X1,Y1).
%% 
%% evaluate_expression(fact(X),VarValues,Y) :-
%% 	evaluate_expression(X,VarValues,X1),
%% 	factorial(X1, Y).
%% 
%% evaluate_expression(max(X,Y),VarValues,Z) :-
%% 	evaluate_expression(X,VarValues,X1),
%% 	evaluate_expression(Y,VarValues,Y1),
%% 	Z is max(X1,Y1).
%% 
%% evaluate_expression(min(X,Y),VarValues,Z) :-
%% 	evaluate_expression(X,VarValues,X1),
%% 	evaluate_expression(Y,VarValues,Y1),
%% 	Z is min(X1,Y1).

evaluate_expression(X,VarValues,Y) :-
	functor(X,prod,4),
	prod_evaluate_expression(X,VarValues,Y).

size_variable(arg(_,_)).
size_variable(arity(_)).
size_variable(head(_)).
size_variable(tail(_)).


prod_evaluate_expression(prod(Index,Low, Upp, Exp), VarValues, Res):-
	evaluate_expression(Low, VarValues, LowValue),
	evaluate_expression(Upp, VarValues, UppValue),
	prod_evaluate_expression(LowValue, UppValue, Index, Exp, VarValues,
                                 1, Res).

prod_evaluate_expression(LowValue, UppValue, _, _, _, Res, Res):-
	LowValue > UppValue, !.
prod_evaluate_expression(LowValue,UppValue,Index,Exp,VarValues,IRes,Res):-
	LowValue =< UppValue,
	!,
	append([(Index, LowValue)], VarValues, NewVarValues),
	evaluate_expression(Exp, NewVarValues, Prod), 
        NewIRes is IRes * Prod,
        NewLowValue is LowValue + 1,
        prod_evaluate_expression(NewLowValue,UppValue,Index,Exp,VarValues, 
                                 NewIRes,Res).

sum_evaluate_expression(sum(Index,Low, Upp, Exp), VarValues, Res):-
	evaluate_expression(Low, VarValues, LowValue),
	evaluate_expression(Upp, VarValues, UppValue),
	sum_evaluate_expression(LowValue,UppValue,Index, Exp,VarValues,0,Res).

sum_evaluate_expression(LowValue, UppValue, _, _, _, Res, Res):-
	LowValue > UppValue,
	!.

sum_evaluate_expression(LowValue,UppValue,Index,Exp,VarValues,IRes,Res):-
	LowValue =< UppValue,
	!,
	append([(Index, LowValue)], VarValues, NewVarValues),
	evaluate_expression(Exp, NewVarValues, Sum), 
        NewIRes is IRes + Sum,
        NewLowValue is LowValue + 1,
        sum_evaluate_expression(NewLowValue, UppValue, Index, Exp, VarValues, 
                                NewIRes, Res).

:- pop_prolog_flag(multi_arity_warnings).


