:- module(ex2,[main/1],[assertions,regtypes]).

main(Res):- 
	L1 = [L2],
	q(L),
	p(L,L2),
	app([1,2|L1],L,Res),
	validlist(Res).

q([1]).
q([2|Xs]):- q(Xs).

p(X,Y):- r(X,Y).

r([],0).
r([_|_],1).

app([],Y,Y):- validlist(Y).
app([X|Xs],Y,[X|Zs]):-app(Xs,Y,Zs).

                
:- regtype validlist/1.

validlist([]).           
validlist([X|Xs]):-                                             
	valid(X),
	validlist(Xs).                                           

:- regtype valid/1.

valid(1).
valid(2).

%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "off"
%% End:

