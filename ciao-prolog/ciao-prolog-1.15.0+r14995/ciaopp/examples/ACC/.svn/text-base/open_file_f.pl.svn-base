:- module(_,[create_streams/2],[fsyntax,assertions,regtypes]).

:- entry create_streams(A,B) : list(A,int).

create_streams([])     := [].
create_streams([N|NL]) := [ ~safe_open(FName,write) | 
                            ~create_streams(NL)     ] :-
	app("/tmp/",~number_codes(N),FName).
% 	app("/tmp/../",~number_codes(N),FName).

% --------------------------------------------------------------

app([], L   ) := L.
app([X|Xs],L) := [X|~app(Xs,L)].

% --------------------------------------------------------------

:- check calls safe_open(FName,_,_) : safeName(FName).

safe_open(FName,Mode) := ~open(F,Mode) :- atom_codes(F,FName).

:- regtype safeName/1.  safeName("/tmp/"||L):-list(L,alphnum).

:- regtype alphnum/1.   alphnum := ~alph_code|~num_code.

:- regtype alph_code/1. alph_code := 0'a|0'b|0'c|0'd|0'e|0'f.

% --------------------------------------------------------------

