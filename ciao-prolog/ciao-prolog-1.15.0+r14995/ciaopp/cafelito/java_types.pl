


:- regtype byte/1
# "Prolog's equivalent of Java byte type, in the range -128 to 127".
byte(X):-
   basic_props:int(X),
   X >= -128,
   X =<  127.

:- regtype short/1
# "Prolog's equivalent of Java short type, in the range -32,768 to 32,767".
short(X):-
   basic_props:int(X),
   X >= -32.768,
   X =<  32.767.

int(X):-
   basic_props:int(X).

float(X):-
   flt(X).

double(X):-
   flt(X).

long(X):-
   flt(X). 	