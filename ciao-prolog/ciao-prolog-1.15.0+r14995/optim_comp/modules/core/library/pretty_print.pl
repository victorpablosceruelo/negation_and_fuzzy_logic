:- module(pretty_print,
	[ pretty_print/2,
	  pretty_print/3
	],
	[ assertions,
	  regtypes,
	  functions
	]).

:- use_module(library(operators)).
:- use_module(library(vndict)).
:- use_module(library(write)).

%% -----------------------------------------------------------------------
:- doc(title,"A simple pretty-printer for Ciao programs").

:- doc(author, "The CLIP Group").

:- doc(module,"This library module writes out to standard output a 
	clause or a list of clauses.").
%% -----------------------------------------------------------------------

:- pred pretty_print(Cls,Flags) : clauses * list(flag)
	# "Prints each clause in the list @var{Cls} after numbering its
	  variables.".

:- pred pretty_print(Cls,Flags,Ds) : clauses * list(flag) * varnamedict
	# "Prints each clause in the list @var{Cls} after using the 
	  corresponding variable names dictionary in @var{Ds} to
	  name its variables.".

:- doc(doinclude,clauses/1).

%% :- typedef clauses ::= [] ; [clause|clauses] ; clause .
:- regtype clauses/1.

clauses := [].
clauses := [~clause|~clauses].
clauses := ~clause.

:- doc(doinclude,clause/1).

%% :- typedef clause ::= clterm ; ^((clterm,any)) .
:- regtype clause/1.

clause := ~clterm.
clause := ^((~clterm,~term)).

:- doc(doinclude,clterm/1).

%% :- typedef clterm ::= ^clause(goal,body) ; ^directive(body)
%% 	            ; ^((goal:-body)) ; goal .
:- regtype clterm/1.

clterm := clause(~callable,~body).
clterm := directive(~body).
clterm := (~callable :- ~body).
clterm := ~callable.

:- doc(doinclude,body/1).
:- doc(body/1,"A well formed body, including cge expressions and
   &-concurrent expressions. The atomic goals may or may not have
   a key in the form @tt{^(goal:any)}, and may or may not be
   module qualified, but if they are it has to be in the form
   @tt{^(^(moddesc:goal):any)}.").

:- regtype body(X)
	# "@var{X} is a printable body.".

body(X):- body(X).

:- doc(doinclude,flag/1).
:- doc(flag/1,
	"A keyword @tt{ask/1} flags whether to output @em{asks} or
         @em{whens} and @tt{nl/1} whether to separate clauses with
	 a blank line or not.").
:- regtype flag(X)
	# "@var{X} is a flag for the pretty-printer.".
flag(ask(A)):- ok_ans(A).
flag(nl(B)):- ok_ans(B).

:- regtype ok_ans(X)
	# "@var{X} is an answer for yes/no questions.".
ok_ans(yes).
ok_ans(no).

% check hooks:
curr_hooks(L,A,B):- hooks(L,no,A,no,B).

hooks([],A,A,B,B).
hooks([X|Xs],A0,A,B0,B):-
	hook(X,A0,B0,A1,B1),
	hooks(Xs,A1,A,B1,B).

hook(ask(A),_A,B,A,B):- ok_ans(A).
hook(nl(B),A,_B,A,B):- ok_ans(B).

%% -----------------------------------------------------------------------

pretty_print(Cls,Hooks,Ds):-
	curr_hooks(Hooks,Key,Nl),
	pretty_printK(Cls,Ds,Key,Nl).

pretty_print(Cls,Hooks):-
	curr_hooks(Hooks,Key,Nl),
	pretty_printK(Cls,_nodict,Key,Nl).

pretty_printK([],_Ds,_Key,_Nl):- !.
pretty_printK([(Cl,_)|T],[D|Ds],Key,Nl) :- !,
	pretty_print0(Cl,D,Key,Nl),
	pretty_printK(T,Ds,Key,Nl).
pretty_printK([Cl|T],[D|Ds],Key,Nl) :- !,
	pretty_print0(Cl,D,Key,Nl),
	pretty_printK(T,Ds,Key,Nl).
pretty_printK((Cl,_),D,Key,Nl) :- !,
	pretty_print0(Cl,D,Key,Nl).
pretty_printK(Cl,D,Key,Nl) :- 
	pretty_print0(Cl,D,Key,Nl).

pretty_print0(Cl,D,K,N):- var(D), !,
	numbervars(Cl,0,_),
	pp(Cl,K),
	write('.'), nl,
	separator(N).
pretty_print0(Cl,D,K,N):-
	rename(Cl,D),
	pp(Cl,K),
	write('.'), nl,
	separator(N).

separator(yes):- nl.
separator(no).

pp(directive(D),_K):- !,
	write(':- '), 
	writeq(D).
pp((H :- B),K):- !,
	pp(clause(H,B),K).
pp(clause(H,true),_K):- !,
	writeq(H).
pp(clause(H,!),_K):- !,
	writeq(H),
 	write(' :- !').
pp(clause(H,B),K):- !,
	writeq(H),
	write(' :-'), nl,
	ppb(B,8,K).
pp(H,K):-
	pp(clause(H,true),K).

ppb((A,B),Tab,K) :- !,
	ppb(A,Tab,K),
	write(','), nl,
	ppb(B,Tab,K).
ppb('&'(A,B),Tab,K) :- !,
	ppc(A,Tab,K),
	write(' &'), nl,
	ppc(B,Tab,K).
ppb(('&'(A)),Tab,K) :- !,
	ppb(A,Tab,K),
	write(' &').
ppb(true(B),Tab,K) :- !,
	tab(Tab), write('true('), nl,
	NTab1 is Tab+4,
	NTab2 is Tab+7,
	ppc(B,NTab2,K), nl,
	tab(NTab1), write(')').
ppb((A->B;C),Tab,K) :- !,
	tab(Tab), write('('), nl,
	NTab1 is Tab+2,
	NTab2 is Tab+5,
	ppb(A,NTab1,K),
	write(' ->'), nl,
	ppb(B,NTab2,K), nl,
	tab(Tab), write(';'), nl,
	ppb(C,NTab2,K), nl,
	tab(Tab), write(')').
ppb((A->B),Tab,K) :- !,
	tab(Tab), write('('), nl,
	NTab1 is Tab+2,
	NTab2 is Tab+5,
	ppb(A,NTab1,K),
	write(' ->'), nl,
	ppb(B,NTab2,K), nl,
	tab(Tab), write(')').
ppb((A;B),Tab,K) :- !,
	tab(Tab), write('('), nl,
	NTab is Tab+5,
	ppb(A,NTab,K), nl,
	tab(Tab), write(';'), nl,
	ppb(B,NTab,K), nl,
	tab(Tab), write(')').
ppb('=>'(A,B),Tab,K) :- !,
	tab(Tab), write('('), nl,
	NTab is Tab+5,
	ppb(A,NTab,K), nl,
	tab(Tab), write('=>'), nl,
	ppb(B,NTab,K), nl,
	tab(Tab), write(')').
% Not anymore!!!
%% ppb(A:_,Tab,K) :- !,
%%  	ppg(A,Tab,K).
ppb(A,Tab,K) :-
 	ppg(A,Tab,K).

ppc('&'(A,B),Tab,K) :- !,
	ppc(A,Tab,K),
	write(' &'), nl,
	ppc(B,Tab,K).
ppc(X,Tab,K) :-
	functor(X,F,2),
%	( F=',' ; F='=>' ; F=';' ; F='->' ), !,
% for the rest, '(' is written by ppb itself:
	F=',' , !,
	tab(Tab), write('('), nl,
	NTab is Tab+1,
	ppb(X,NTab,K), nl,
	tab(Tab), write(')').
ppc(A,Tab,K) :-
	ppb(A,Tab,K).

% when/2 to a guarded ask
ppg(when(A,B),Tab,yes) :- !,
	ppb((ask(A)->B),Tab,ask),
	write(' & ').
% complex-ask with an &
ppg(ask(A,B),Tab,yes) :- !,
	tab(Tab),
	writeq(ask(A,B)),
	write(' & ').
% simple or qualified atomic goal
ppg(A,Tab,_K) :-
	functor(A,F,_),
	current_op(X,_,F),
	current_op(Y,_,','),
	X >= Y, !,
	tab(Tab),
	write('( '), writeq(A), write(' )').
ppg(A,Tab,_K) :-
	tab(Tab),
	writeq(A).
