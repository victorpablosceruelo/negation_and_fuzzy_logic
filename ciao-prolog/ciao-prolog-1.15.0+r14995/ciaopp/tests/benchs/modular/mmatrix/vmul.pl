:- module(vmul,[vmul/3],[]).

vmul([],[],0).
vmul([H1|T1], [H2|T2], Result):- 
	vmul(T1,T2, Newresult), 
	Product is H1*H2,
	Result is Product+Newresult.


