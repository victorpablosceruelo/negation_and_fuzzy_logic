:- module(myapp_new2,[myapp/1],[]).

:- use_package(assertions).
:- use_package(regtypes).

myapp(Res):- 
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

valid(0).
valid(1).
valid(2).


:- regtype emtpylist/1.

emtpylist([]).

%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "off"
%% End:

