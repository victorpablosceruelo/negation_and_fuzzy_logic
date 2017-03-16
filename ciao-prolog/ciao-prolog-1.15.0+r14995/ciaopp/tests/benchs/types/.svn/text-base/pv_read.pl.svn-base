%% Modified by -PL

:- module(pv_read, [read/2], [assertions]).
:- use_module(library(ttyout), [ttynl/0, ttyput/1]).

:- entry read(A, B) : var * var.

% WARNING!!
% replaced get0 by myget0

% replaced ' (' by lb_bracket_0 
% replaced '(' by lbracket_0
% replaced '[' by slbracket_0
% replaced '{' by llbracket_0
% replaced ')' by rbracket_0
% replaced ']' by srbracket_0
% replaced '}' by lrbracket_0
% replaced ',' by coma_0 
% replaced '|' by bar_0
% replaced '!' by cut_0
% replaced ';' by semcol_0 (semicolon)
% replaced '#' cua_0
% replaced '$' dol_0
% replaced '&' by and_0
% replaced '*' by times_0
% replaced '+' by plus_0
% replaced '-' by minus_0
% replaced '.' by point_0
% replaced '/' by slash_0
% replaced ':' by colon_0
% replaced '<' by less_0
% replaced '=' by equ_0
% replaced '>' by great_0
% replaced '?' by inter_0
% replaced '@' by at_0
% replaced '\' by bslash_0
% replaced '^' by hat_0
% replaced '`' by circ_0
% replaced '~' by mar_0
% replaced '{}' by pepe_0

% Commented -PL 
 %%  fail :- 3 = 4.

%% myget0(-1).
%% myget0(1).
%% myget0(2).
%% myget0(46).
myget0(X):- get_code(X).

 %% current_op(1,xfx,toto).
 %% current_op(2,xfx,titi).
 %% current_op(1,xfy,toto).
 %% current_op(2,xfy,titi).
 %% current_op(1,yfx,toto).
 %% current_op(2,yfx,titi).
 %% current_op(1,fx,toto).
 %% current_op(2,fx,titi).
 %% current_op(1,yf,toto).
 %% current_op(2,yf,titi).
 %% current_op(1,xf,toto).
 %% current_op(2,xf,titi).
 %% current_op(1,fy,toto).
 %% current_op(2,fy,titi).
% :- impl_defined(current_op/3).
:- use_module(library(operators), [current_op/3]).


read_fullstop(26, _, _) :- 
        display(cut_0_end_of_file_just_after_full_stop), 
        % display(message), 
        ttynl,
        %% Commented -PL display('! end of file just after full stop'), ttynl,
        fail.
read_fullstop(Ch, _, []) :-
        Ch =< 32.
read_fullstop(Ch, Dict, [atom(A)|Tokens]) :-
        read_symbol(Ch, _S, NextCh),
        getname(A),
        read_tokens(NextCh, Dict, Tokens).

read_tokens2(TokenList, Dictionary) :-
        read_tokens(32, Dict, TokenList),
        append(Dict, [], Dict), 
        Dictionary = Dict.
read_tokens2([atom(end_of_file)], []).        

read_tokens(-1, _, _) :-                
        2 = 3.                                        
read_tokens(Ch, Dict, Tokens) :-
        Ch =< 32,                                
        myget0(NextCh),                                
        read_tokens(NextCh, Dict, Tokens).
read_tokens(37, Dict, Tokens) :-                 
        myget0(Ch),                                
        Ch =\= 26,                                
        myget0(NextCh),
        read_tokens(NextCh, Dict, Tokens).
read_tokens(47, Dict, Tokens) :-                 
        myget0(NextCh),
        read_solidus3(NextCh, Dict, Tokens).
read_tokens(33, Dict, [atom(cut_0)|Tokens]) :-         
        myget0(NextCh),                                
        read_after_atom(NextCh, Dict, Tokens).        
% Commented out. -PL
 read_tokens(40, Dict, [ lb_bracket_0 | Tokens]) :-         
         myget0(NextCh),
         read_tokens(NextCh, Dict, Tokens).

read_tokens(41, Dict, [rbracket_0|Tokens]) :- 
        myget0(NextCh),
        read_tokens(NextCh, Dict, Tokens).
read_tokens(44, Dict, [coma_0|Tokens]) :- 
        myget0(NextCh),
        read_tokens(NextCh, Dict, Tokens).
read_tokens(59, Dict, [atom(semcol_0)|Tokens]) :-         
        myget0(NextCh),                                
        read_tokens(NextCh, Dict, Tokens).        
read_tokens(91, Dict, [slbracket_0|Tokens]) :- 
        myget0(NextCh),
        read_tokens(NextCh, Dict, Tokens).
read_tokens(93, Dict, [srbracket_0|Tokens]) :- 
        myget0(NextCh),
        read_tokens(NextCh, Dict, Tokens).
read_tokens(123, Dict, [llbracket_0|Tokens]) :- 
        myget0(NextCh),
        read_tokens(NextCh, Dict, Tokens).
read_tokens(124, Dict, [bar_0|Tokens]) :- 
        myget0(NextCh),
        read_tokens(NextCh, Dict, Tokens).
read_tokens(125, Dict, [lrbracket_0|Tokens]) :- 
        myget0(NextCh),
        read_tokens(NextCh, Dict, Tokens).
read_tokens(46, Dict, Tokens) :-                 
        myget0(NextCh),                                
        read_fullstop(NextCh, Dict, Tokens).
read_tokens(34, Dict, [string(S)|Tokens]) :-         
        read_string(S, 34, NextCh),
        read_tokens(NextCh, Dict, Tokens).
read_tokens(39, Dict, [atom(A)|Tokens]) :-         
        read_string(_S, 39, NextCh),
        getname(A),                                
        read_after_atom(NextCh, Dict, Tokens).
read_tokens(Ch, Dict, [var(Var,Name)|Tokens]) :-
        Ch >= 65, Ch =< 90,
        read_name(Ch, _S, NextCh),
        getname(Name),
        read_lookup(Dict, Name=Var),
        read_tokens(NextCh, Dict, Tokens).
read_tokens(Ch, Dict, [integer(I)|Tokens]) :-
        Ch >= 48, Ch =< 57,        
        read_integer(Ch, I, NextCh),
        read_tokens(NextCh, Dict, Tokens).
read_tokens(Ch, Dict, [atom(A)|Tokens]) :-
        Ch >= 97, Ch =< 122,                        
        read_name(Ch, _S, NextCh),
        getname(A),
        read_after_atom(NextCh, Dict, Tokens).
read_tokens(_Ch, Dict, [atom(A)|Tokens]) :-        
        myget0(AnotherCh),
        read_symbol(AnotherCh, _Chars, NextCh),        
        getname(A),
        read_after_atom(NextCh, Dict, Tokens).

getname('toto').
getname('titi').

read_after_atom(40, Dict, [lbracket_0|Tokens]) :- 
        myget0(NextCh),
        read_tokens(NextCh, Dict, Tokens).
read_after_atom(Ch, Dict, Tokens) :-
        read_tokens(Ch, Dict, Tokens).

read_string(Chars, Quote, NextCh) :-
        myget0(Ch),
        read_string4(Ch, Chars, Quote, NextCh).
read_string4(26, _, Quote, 26) :-
        display(cut_0_end_of_file_in), 
        % display(message),
        ttyput(Quote),
        % Commented display('! end of file in '), ttyput(Quote),
        display(token), ttyput(Quote), ttynl,
        fail.
read_string4(Quote, Chars, Quote, NextCh) :- 
        myget0(Ch),                                
        more_string(Ch, Quote, Chars, NextCh).
read_string4(Char, [Char|Chars], Quote, NextCh) :-
        read_string(Chars, Quote, NextCh).        

more_string(Quote, Quote, [Quote|Chars], NextCh) :- 
        read_string(Chars, Quote, NextCh).        
more_string(NextCh, _, [], NextCh).                


read_solidus3(42, Dict, Tokens) :- 
        myget0(Ch),
        read_solidus(Ch, NextCh),
        read_tokens(NextCh, Dict, Tokens).
read_solidus3(Ch, Dict, [atom(A)|Tokens]) :-
        read_symbol(Ch, _Chars, NextCh),                
        getname(A),
        read_tokens(NextCh, Dict, Tokens).

read_solidus(26, 26) :- 
        display(cut_0_end_of_file_in_slash_0_times_0_comment), 
        % display(message), 
        ttynl.
        % Commented -PL display('! end of file in /*comment'), ttynl. %*/

read_solidus(42, LastCh) :-
        myget0(NextCh),
        NextCh =\= 47,         
        read_solidus(NextCh, LastCh).
read_solidus(42, 32).
read_solidus(_, LastCh) :-
        myget0(NextCh),
        read_solidus(NextCh, LastCh).

read_name(Char, [Char|Chars], LastCh) :-
        Char >= 97, Char =< 122,        
        myget0(NextCh),
        read_name(NextCh, Chars, LastCh).
read_name(LastCh, [], LastCh).


read_symbol(Char, [Char|Chars], LastCh) :-
        check_special(Char), 
        myget0(NextCh),
        read_symbol(NextCh, Chars, LastCh).
read_symbol(LastCh, [], LastCh).

 % Commented out -PL
 %% check_special('#').
 %% check_special('$').
 %% check_special('&').
 %% check_special('*').
 %% check_special('+').
 %% check_special('-').
 %% check_special('.').
 %% check_special('/').
 %% check_special(':').
 %% check_special('<').
 %% check_special('=').
 %% check_special('>').
 %% check_special('?').
 %% check_special('@').
 %% check_special('\').
 %% check_special('^').
 %% check_special('`').
 %% check_special('~').

check_special(cua_0).
check_special(dol_0).
check_special(and_0).
check_special(times_0).
check_special(plus_0).
check_special(minus_0).
check_special(point_0).
check_special(slash_0).
check_special(colon_0).
check_special(less_0).
check_special(equ_0).
check_special(great_0).
check_special(inter_0).
check_special(at_0).
check_special(bslash_0).
check_special(hat_0).
check_special(circ_0).
check_special(mar_0).



read_integer(BaseChar, IntVal, NextCh) :-
        Base is BaseChar - 48,
        myget0(Ch),
        Ch =\= 26,
         Ch =\= 39, read_digits5(Ch, Base, 10, IntVal, NextCh).

read_digits(SoFar, Base, Value, NextCh) :-
        myget0(Ch),
        Ch =\= 26,
        read_digits5(Ch, SoFar, Base, Value, NextCh).

read_digits5(Digit, SoFar, Base, Value, NextCh) :-
        Digit >= 48, Digit =< 57,
        Next is SoFar*Base-48+Digit,
        read_digits(Next, Base, Value, NextCh).
read_digits5(LastCh, Value, _, Value, LastCh).

read_lookup([X|_], X).
read_lookup([_|T], X) :-
        read_lookup(T, X). 

append([],L,L).
append([H|L1],L2,[H|L3]) :- append(L1,L2,L3).


read(Answer, Variables) :-
    read_tokens2([Tokens|Ts],Variables),
    read4([Tokens|Ts], 1200, Term, LeftOver), 
    all_read(LeftOver),
    Answer = Term.

all_read([]).
all_read([X|Y]) :-
   syntax_error([operator,expected,after,expression], [X|Y]).

expect(Token, [Token|Rest], Rest).
expect(Token, S0, _) :-
   syntax_error([Token,or,operator,expected], S0).

prefixop(Op, Prec, Prec) :-
   current_op(Prec, fy, Op).
prefixop(Op, Prec, Less) :-
   current_op(Prec, fx, Op),
   Less is Prec-1.


postfixop(Op, Prec, Prec) :-
   current_op(Prec, yf, Op).
postfixop(Op, Less, Prec) :-
   current_op(Prec, xf, Op), Less is Prec-1.

infixop(Op, Less, Prec, Less) :-
   current_op(Prec, xfx, Op), Less is Prec-1.
infixop(Op, Less, Prec, Prec) :-
   current_op(Prec, xfy, Op), Less is Prec-1.
infixop(Op, Prec, Prec, Less) :-
   current_op(Prec, yfx, Op), Less is Prec-1.


ambigop(F, L1, O1, R1, L2, O2) :-
   postfixop(F, L2, O2),
   infixop(F, L1, O1, R1).

read4([Token|RestTokens], Precedence, Term, LeftOver) :-
   read5(Token, RestTokens, Precedence, Term, LeftOver).
read4([], _, _, _) :-
   syntax_error([expression,expected], []).

read5(var(Variable,_), [lbracket_0|S1], Precedence, Answer, S) :- 
   read4(S1, 999, Arg1, S2),
   read_args(S2, RestArgs, S3),
   exprtl0(S3, apply(Variable,[Arg1|RestArgs]), Precedence, Answer, S).

read5(var(Variable,_), S0, Precedence, Answer, S) :- 
   exprtl0(S0, Variable, Precedence, Answer, S).

read5(atom(minus_0), [integer(Integer)|S1], Precedence, Answer, S) :-
   Negative is -Integer,
   exprtl0(S1, Negative, Precedence, Answer, S).

read5(atom(Functor), S0, Precedence, Answer, S) :-
   prefixop(Functor, Prec, Right),
   after_prefix_op(Functor, Prec, Right, S0, Precedence, Answer, S).

read5(atom(Atom), S0, Precedence, Answer, S) :-
   exprtl0(S0, Atom, Precedence, Answer, S).

read5(integer(Integer), S0, Precedence, Answer, S) :- 
   exprtl0(S0, Integer, Precedence, Answer, S).

read5(slbracket_0, [srbracket_0|S1], Precedence, Answer, S) :- 
   exprtl0(S1, [], Precedence, Answer, S).

read5(slbracket_0, S1, Precedence, Answer, S) :- 
   read4(S1, 999, Arg1, S2),
   read_list(S2, RestArgs, S3),
   exprtl0(S3, [Arg1|RestArgs], Precedence, Answer, S).

read5(lbracket_0, S1, Precedence, Answer, S) :- 
   read4(S1, 1200, Term, S2),
   expect(rbracket_0, S2, S3),
   exprtl0(S3, Term, Precedence, Answer, S).

read5(lb_bracket_0, S1, Precedence, Answer, S) :- 
   read4(S1, 1200, Term, S2),
   expect(rbracket_0, S2, S3),
   exprtl0(S3, Term, Precedence, Answer, S).

read5(llbracket_0, [lrbracket_0|S1], Precedence, Answer, S) :- 
   exprtl0(S1, pepe_0, Precedence, Answer, S).

read5(llbracket_0, S1, Precedence, Answer, S) :- 
   read4(S1, 1200, Term, S2),
   expect(lrbracket_0, S2, S3),
   exprtl0(S3, set(Term), Precedence, Answer, S).

read5(string(List), S0, Precedence, Answer, S) :- 
   exprtl0(S0, List, Precedence, Answer, S).

read5(Token, S0, _, _, _) :-
   syntax_error([Token,cannot,start,an,expression], S0).

read_args([coma_0|S1], [Term|Rest], S) :- 
   read4(S1, 999, Term, S2),
   read_args(S2, Rest, S).
read_args([rbracket_0|S], [], S).
read_args([_X|_Y], _, _) :-
   syntax_error([coma_0_or_rbracket_0,expected,in,arguments], _S).
   % Commented -PL syntax_error([', or )',expected,in,arguments], S).


read_list([coma_0|S1], [Term|Rest], S) :- 
   read4(S1, 999, Term, S2),
   read_list(S2, Rest, S).
read_list([bar_0|S1], Rest, S) :- 
   read4(S1, 999, Rest, S2),
   expect(srbracket_0, S2, S).
read_list([srbracket_0|S], [], S).
read_list(S, _, _) :-
   % Commented 
   % syntax_error([', | or ]',expected,in,list], S).
   syntax_error([coma_0_bar_0_or_srbracket_0,expected,in,list], S).
   
after_prefix_op(Op, Oprec, _Aprec, S0, Precedence, _, _) :-
   Precedence < Oprec,
   syntax_error([prefix,operator,Op,in,context,
      with,precedence,Precedence], S0).

after_prefix_op(Op, Oprec, _Aprec, S0, Precedence, Answer, S) :-
   peepop(S0, S1),
   prefix_is_atom(S1, Oprec), 
   exprtl(S1, Oprec, Op, Precedence, Answer, S).

peepop([atom(F),lbracket_0|S1], [atom(F),lbracket_0|S1]) .
peepop([atom(F)|S1], [infixop(F,L,P,R)|S1]) :- infixop(F, L, P, R).
peepop([atom(F)|S1], [postfixop(F,L,P)|S1]) :- postfixop(F, L, P).
peepop(S0, S0).

prefix_is_atom([Token|_], Precedence) :-
   prefix_is_atom(Token, Precedence).

prefix_is_atom(infixop(_,L,_,_), P) :- L >= P.
prefix_is_atom(postfixop(_,L,_), P) :- L >= P.
prefix_is_atom(rbracket_0, _).
prefix_is_atom(srbracket_0, _).
prefix_is_atom(lrbracket_0, _).
prefix_is_atom(bar_0, P) :- 1100 >= P.
prefix_is_atom(coma_0, P) :- 1000 >= P.
prefix_is_atom([],  _).


exprtl0([atom(F)|S1], Term, Precedence, Answer, S) :-
   ambigop(F, L1, O1, R1, _L2, _O2),
   exprtl([infixop(F,L1,O1,R1)|S1], 0, Term, Precedence, Answer, S).

exprtl0([atom(F)|S1], Term, Precedence, Answer, S) :-
   ambigop(F, _L1, _O1, _R1, L2, O2),
   exprtl([postfixop(F,L2,O2) |S1], 0, Term, Precedence, Answer, S).

exprtl0([atom(F)|S1], Term, Precedence, Answer, S) :-
   infixop(F, L1, O1, R1),
   exprtl([infixop(F,L1,O1,R1)|S1], 0, Term, Precedence, Answer, S).
exprtl0([atom(F)|S1], Term, Precedence, Answer, S) :-
   postfixop(F, L2, O2),
   exprtl([postfixop(F,L2,O2) |S1], 0, Term, Precedence, Answer, S).

exprtl0([coma_0|S1], Term, Precedence, Answer, S) :-
   Precedence >= 1000,
   read4(S1, 1000, Next, S2),
   exprtl(S2, 1000, c(Term,Next), Precedence, Answer, S).

exprtl0([bar_0|S1], Term, Precedence, Answer, S) :-
   Precedence >= 1100,
   read4(S1, 1100, Next, S2),
   exprtl(S2, 1100, sc(Term,Next), Precedence, Answer, S).

exprtl0([Thing|S1], _, _, _, _) :-
   cant_follow_expr(Thing, Culprit),
   syntax_error([Culprit,follows,expression], [Thing|S1]).

exprtl0(S, Term, _, Term, S).


cant_follow_expr(atom(_),   atom).
cant_follow_expr(var(_,_),   variable).
cant_follow_expr(integer(_),   integer).
cant_follow_expr(string(_),   string).
cant_follow_expr(lb_bracket_0,      bracket).
cant_follow_expr(lbracket_0,      bracket).
cant_follow_expr(slbracket_0,      bracket).
cant_follow_expr(llbracket_0,      bracket).


exprtl([coma_0|S1], C, Term, Precedence, Answer, S) :-
   Precedence >= 1000, C < 1000,
   read4(S1, 1000, Next, S2),
   exprtl(S2, 1000, c(Term,Next), Precedence, Answer, S).

exprtl([bar_0|S1], C, Term, Precedence, Answer, S) :-
   Precedence >= 1100, C < 1100,
   read4(S1, 1100, Next, S2),
   exprtl(S2, 1100, sc(Term,Next), Precedence, Answer, S).

exprtl(S, _, Term, _, Term, S).

% syntax_error(Message, List) :-
%   fail.
% redefined -PL
% Begin added by -PL
%   This business of syntax errors is tricky.  When an error is detected,
%   we have to write out a message.  We also have to note how far it was
%   to the end of the input, and for this we are obliged to use the data-
%   base.  Then we fail all the way back to read(), and that prints the
%   input list with a marker where the error was noticed.  If subgoal_of
%   were available in compiled code we could use that to find the input
%   list without hacking the data base.  The really hairy thing is that
%   the original code noted a possible error and backtracked on, so that
%   what looked at first sight like an error sometimes turned out to be
%   a wrong decision by the parser.  This version of the parser makes
%   fewer wrong decisions, and my goal was to get it to do no backtracking
%   at all.  This goal has not yet been met, and it will still occasionally
%   report an error message and then decide that it is happy with the input
%   after all.  Sorry about that.

/*  Modified by D.S. Warren, Sep 10 1985, to print remainder of 
    token list only, and not use record/recorded */

% replaced '**' by two_times_0  
syntax_error(Message, List) :-
        nl, print(two_times_0),
        print_list(Message),nl,
	print(two_times_0_Tokens_skipped_colon_0),
        % print('** Tokens skipped:'),
	print_list(List), !.
%	fail.

print_list([]) :- nl.
print_list([Head|Tail]) :-
	tab(1),
        print_token(Head),
        print_list(Tail).

print_token(atom(X)) :- !, print(X).
print_token(X) :- print(X).

% End added by -PL

print(X):- display(X).

 %% syntax_error(Message, List):-
 %%   write_message(Message),
 %%   write_message(List).
 %% 
 %% write_message([]).
 %% write_message([H|L]) :- write(H), write_message(L).

