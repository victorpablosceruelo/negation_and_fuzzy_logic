
:- module(proof7_sanchez,_,[.(cneg)]).

:- use_module(dist). 

symmetric(o). 
symmetric(f1(X)):- symmetric(X).
symmetric(f2(X,Y)):- mirror(X,Y).

mirror(o,o) .
mirror(f1(X),f1(Y)):- mirror(X,Y).
mirror(f2(X,Y),f2(Z,W)):- mirror(X,W),mirror(Y,Z).

has_duplicates([X|Y]):- member1(X,Y). 
has_duplicates([_|Y]):- has_duplicates(Y).

member1(X,[X|_]).
member1(X,[_|Y]):- member1(X,Y).

list_of_digits([ ]).
list_of_digits([X|Y]):- digit(X),list_of_digits(Y).

digit(1).
digit(2).
digit(3).

len([],0).
len([_|L],N):- len(L,N1), N is N1+1.

disjoint([],_).
disjoint([X|L1],L2):- cneg(member1(X,L2)), disjoint(L1,L2).

greater(s(_),0).
greater(s(X),s(Y)):- greater(X,Y).

insert_sort(X,[],[X]).
insert_sort(X,[Y|L],[X|[Y|L]]):- !, greater(X,Y).

 

