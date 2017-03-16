:- module(running_example,[main/1],[]).

:- use_package(assertions).
:- use_package(regtypes).

main(Res):- 
	L1 = [Num],
	q(L),
	p(L,Num),
	app([1,2|L1],L,Res),
	validlist(Res).

q([1]).
q([1|Xs]):- q(Xs).

p(X,Y):- add_list_ac(X,Y).

add_list_ac([],0).
add_list_ac([X|_Xs],Y):-
	Y is X + 1.

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


:- regtype emtpylist/1.

emtpylist([]).

%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "off"
%% End:

