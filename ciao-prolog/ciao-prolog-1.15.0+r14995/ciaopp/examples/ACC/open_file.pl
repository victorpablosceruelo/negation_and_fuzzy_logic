:- module(open_file,[create_streams/2],[assertions,regtypes]).

:- entry create_streams(A,B) : list(A,num).

:- use_module(library(write)).

create_streams([],[]).
create_streams([N|NL],[F|FL]):-
        number_codes(N,ChInN), 
%	app("/tmp/../",ChInN,Fname),
	app("/tmp/",ChInN,Fname),
        safe_open(Fname,write,F), 
	create_streams(NL,FL).

:- check calls safe_open(Fname,Mode,Stream) : safe_name(Fname).

safe_open(Fname,Mode,Stream):-
	atom_codes(File,Fname),
	open(File,Mode,Stream).

:-  regtype  safe_name/1.

safe_name("/tmp/"||L) :- list(L,alphanum_code).

:- regtype alphanum_code/1.
alphanum_code(X):-
	alpha_code(X).
alphanum_code(X):-
	num_code(X).

:-  regtype  alpha_code/1.

%admissible_char(A):-  member(A,"abcdefghijklmnopqrstuvwzyz0123456789").
alpha_code(0'a).
alpha_code(0'b).
alpha_code(0'c).
alpha_code(0'd).
alpha_code(0'e).
alpha_code(0'f).
alpha_code(0'g).

app([],L,L).
app([X|Xs],L,[X|Y]):-
        app(Xs,L,Y).


