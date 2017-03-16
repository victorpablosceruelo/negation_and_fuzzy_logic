:- module(open_file,_,[assertions,regtypes]).

:- use_module(library(write)).
%:- use_module(library(lists),[append/3]).


:-  regtype  safe_name/1.

safe_name("/tmp/"||L) :- list(L,alphanum_code).

%% :- regtype list_alphanum_code/1.
%% 
%% list_alphanum_code([]).
%% list_alphanum_code([X|Xs]):-
%% 	alphanum_code(X),
%% 	list_alphanum_code(Xs).
%% 

:-  regtype  alphanum_code/1.
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

:-  regtype  num_code/1.

num_code(0'1).
num_code(0'2).
num_code(0'3).
num_code(0'4).
num_code(0'5).
num_code(0'6).
num_code(0'7).
num_code(0'8).
num_code(0'9).
num_code(0'0).

create_streams([],[]).
create_streams([N|NL],[F|FL]):-
        number_codes(N,Num),
	generate(Num,Fname),
 	X = [49],
 	generate(X,Fname),
        safe_open(Fname,write,F),
	create_streams(NL,FL).

%:- check calls safe_open(Fname,Mode,Stream) : (safe_name(Fname),file_mode(Mode)).
:- check calls safe_open(Fname,Mode,Stream) : safe_name(Fname).

safe_open(Fname,Mode,Stream):-
	atom_codes(File,Fname),
	open(File,Mode,Stream).

generate(Num,Fname):-
        app("/tmp/",Num,Fname).

app([],L,L).
app([X|Xs],L,[X|Y]):-
        app(Xs,L,Y).


%% :- regtype file_mode/1.
%% 
%% file_mode(read).
%% file_mode(write).
%% file_mode(append).
