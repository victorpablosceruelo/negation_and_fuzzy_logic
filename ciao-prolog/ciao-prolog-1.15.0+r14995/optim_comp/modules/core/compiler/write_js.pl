:- module(_, [], [compiler(complang)]).

:- doc(title, "Writter of JavaScript programs from AST representation").
:- doc(author, "Jose F. Morales").

:- doc(module, "This module provides predicates for writing JavaScript
	programs from their AST representation.").

:- doc(bug, "Write the regular type for the AST of JavaScript programs so that
	it can appear in the documentation").

:- use_module(compiler(write_c_common)).
:- use_module(library(lists), [length/2, append/3]).
:- use_module(library(string_type(string_type_rt))).
:- use_module(compiler(errlog)).
:- use_module(compiler(meta_syntax)).

% ---------------------------------------------------------------------------

:- use_module(library(streams)).
:- use_module(compiler(open_and_protect)).

:- public class js_writer {
    :- constructor new_/0.
    new_.

    :- public to_stream/2.
%    :- pred to_stream(+Stream, +Code) :: stream * js_tree
%    # "It writes the @em{JavaScript} code represented by @var{Code} in
%       @var{Stream}.".
    to_stream(Stream, Code) :-
    	current_output(OldOutput),
    	set_output(Stream),
    	Ok = ( to_stream_2(Code) ? yes | no ),
    	set_output(OldOutput),
    	Ok = yes.

    :- public to_file/2.
    to_file(Name, Code) :-
        open_and_protect(Name, OutputStream, Ref),
%	name_variables(Code, Module),
	to_stream(OutputStream, Code),
        close(OutputStream),
        end_protect(Ref).
}.

% -----------------------------------------------------------------------

% TODO: Share with write_c.pl
to_stream_2([]) :- !.
to_stream_2([X|Xs]) :- !,
    	to_stream_3(X),
    	to_stream_2(Xs).
to_stream_2(X) :-
    	to_stream_3(X).

to_stream_3(X) :-
	o :: token_writer <- ~token_writer.new,
%	( catch(js_stats(X),_,fail) ->
	( js_stats(X) ->
	    true
	; errlog:bug(['write_js:js_stats failed for ', X]), fail
	),
	o.newline, o.flush_pending.

{
% TODO: Share with write_c.pl
:- fluid o :: token_writer.

% Emit a token
t(X) :- o.token(X).
tj(X) :- o.token(X), o.glue. % no space after
jt(X) :- o.glue, o.token(X). % no space before
jtj(X) :- o.glue, o.token(X), o.glue. % no space around
}.

% For a reference of JavaScript grammar, see
% https://developer.mozilla.org/en/JavaScript/Reference/Statements

{
:- fluid o :: token_writer.

% Statements
js_stats(X) :- ( X = [] ; X = [_|_] ), !, js_stats_seq(X).
js_stats(fundecl(Id, Args2, Code)) :- % function declaration
    	t('function'), js_id(Id), jtj('('), js_exprs(Args2), jt(')'), js_braces(Code).
js_stats(if(A,B)) :- !, % if-then
        t('if'), tj('('), js_expr(A), jt(')'), js_braces(B).
js_stats(simple_if(A,B)) :- !, % if-then (avoid braces if possible)
        t('if'), tj('('), js_expr(A), jt(')'),
	( B = [_] -> js_stats(B) ; js_braces(B) ).
js_stats(if(A,B,C)) :- !, % if-then-else
        t('if'), tj('('), js_expr(A), jt(')'),
	js_braces(B), t('else'),
	( no_braces(C) -> js_stats(C)
	; js_braces(C)
	).
js_stats(return(A)) :- !, % return statement
	t('return'), js_expr(A), jt(';').
js_stats(A <- B) :- !, % assignment 
	js_expr(A), t('='), js_expr(B), jt(';').
js_stats(vardecl(A)) :- !, % variable declaration
	t('var'), js_expr(A), jt(';').
js_stats(vardecl(A, B)) :- !, % variable declaration with initial value
	t('var'), js_expr(A), t('='), js_expr(B), jt(';').
js_stats(function(Head,Body)) :- !, % function definition
	t('function'), js_expr(Head), js_braces(Body).
js_stats(while(Cond,Body)) :- !, % while
	t('while'), tj('('), js_expr(Cond), jt(')'),
	js_braces(Body).
js_stats(switch(Expr,Cases)) :- !, % switch 
	t('switch'), tj('('), js_expr(Expr), jt(')'),
	js_braces(Cases).
js_stats(case(Label,Body)) :- !, % case (for switch statement)
	t('case'), js_expr(Label), jt(':'),
	js_stats(Body). % (no braces in Body)
js_stats(default(Body)) :- !, % default case (for switch statement)
	t('default'), jt(':'), js_stats(Body). % no braces in Body
js_stats(break) :- !, % break (for switch statement)
	t('break'), jt(';').
    % TODO: implement for, for each, label, continue
js_stats(Code) :- % other statements
    	% TODO: use only one string format
    	js_expr(Code),
	jt(';').

% A sequence of statements
js_stats_seq([]) :- !.
js_stats_seq([X]) :- !, js_stats(X).
js_stats_seq([X|Xs]) :-
        js_stats(X), o.newline, js_stats_seq(Xs).

% Code that may be between curly braces
js_braces([]) :- !, t('{}').
js_braces(X) :-
	t('{'), o.inner, o.newline,
	js_stats(X),
	o.outer, o.newline, t('}').

% JS expressions
js_expr(X) :- var(X), !, fail.
js_expr(X) :- X = ~mcall(Obj0, Method), !, % makes life easier for user!
    	js_expr(mb(Obj0, Method)).
js_expr(mb(op(Op), Args)) :- !, % (applicative syntax for operator)
    	js_expr(op(Op, Args)).
js_expr(mb(X, As)) :- ( As = [] ; As = [_|_] ), !, % (application)
    	js_expr(call(X, As)).
js_expr(mb(X, elem(I))) :- !, % X[I]
    	js_expr(X), jtj('['), js_expr(I), jt(']').
js_expr(mb(X, Method)) :- !,
    	js_expr(~paren(X)), jtj('.'), js_id(Method).
js_expr(paren(X)) :- !, % TODO: most of this should be automatic
   	tj('('), js_expr(X), jt(')').
js_expr(new(X, As)) :- !,
    	t('new'), js_expr(X), jtj('('), js_exprs(As), jt(')').
%    js_expr(new_noself(C, As)) :- !, % TODO: deprecate
%    	t('new'), js_expr(C), jtj('('), js_exprs(['null'|As]), jt(')').
js_expr(function(Xs, Body)) :- !, % a closure
    	t('function'), jtj('('), js_exprs(Xs), jt(')'), js_braces(Body).
js_expr(call(X, As)) :- !, % TODO: do not use
    	js_expr(X), jtj('('), js_exprs(As), jt(')').
js_expr(not(X)) :- !,
        % TODO: add priorities in operators so that paren is not
        % necessary here, and it can be moved to the operator table
    	tj('!'), js_expr(~paren(X)).
js_expr(E) :-
        % A term whose principal functor is an operator
        functor(E, N, A),
	js_op(N, Kind, _, _),
	op_kind_arity(Kind, A),
	!,
        E =.. [_|Xs],
        js_expr(op(N, Xs)).
js_expr(op(Op, Xs)) :-
	length(Xs, Arity),
        js_op(Op, Kind, Sep, JSOp),
	op_kind_arity(Kind, Arity),
	!,
        ( Kind = xfx, Xs = [A, B] ->
	    js_expr(A),
	    ( Sep = space -> t(JSOp) ; jtj(JSOp) ),
	    js_expr(B)
        ; Kind = fx, Xs = [A] ->
	    ( Sep = space -> t(JSOp) ; tj(JSOp) ),
	    js_expr(A)
        ; Kind = xf, Xs = [A] ->
	    js_expr(A),
	    ( Sep = space -> t(JSOp) ; jt(JSOp) )
	; throw(bad_op)
	).
js_expr([]) :- !, tj('['), jt(']'). % empty array
js_expr(X) :- atom(X), !, t(X). % TODO: duplicated in js_id/1
js_expr(X) :- number(X), !, t(X). % TODO: duplicated?
js_expr(X) :- is_string(X), !, % a native string
        js_expr(native_string(~string_codes(X))).
js_expr(native_string(Codes)) :- !,
    	t(Codes). % string literal
% ----------
% Special identifier names (avoid excessive atom creation)
js_expr(X) :-  is_js_id(X), !,
        js_id(X).
% ----------
% Unknown expression
js_expr(X) :-
        throw(unknown_js_expr(X)).

js_exprs([]) :- !.
js_exprs([A]) :- !, js_expr(A).
js_exprs([A|As]) :- js_expr(A), jt(','), js_exprs(As).
}.

:- static js_op/3.
% TODO: add priorities and associativity
% :- pred js_op(N, Kind, Sep, JSN) # "@var{N} represents a JavaScript operator,
%    such that:
%:
%   @var{Kind}: is the kind (fx | xf | xfx)
%   @var{Sep}: separation (none | space)
%   @var{JSN}: JS name of the opcode 
% ".
js_op('!=', xfx, space, '!=').
js_op('!==', xfx, space, '!==').
js_op('&', xfx, space, '&').
js_op('|', xfx, space, '|').
js_op('&&', xfx, space, '&&'). % TODO: use write_c bin_op syntax
js_op('||', xfx, space, '||'). % TODO: use write_c bin_op syntax
js_op('%', xfx, space, '%').
js_op('*', xfx, space, '*').
js_op('+', xfx, space, '+').
js_op('+=', xfx, space, '+=').
js_op('-', xfx, space, '-').
js_op('/', xfx, space, '/').
js_op('<', xfx, space, '<').
js_op('<<', xfx, space, '<<').
js_op('<=', xfx, space, '<=').
js_op('==', xfx, space, '==').
js_op('===', xfx, space, '===').
js_op('>', xfx, space, '>').
js_op('>=', xfx, space, '>=').
js_op('>>', xfx, space, '>>').
js_op('in', xfx, space, 'in').
js_op('instanceof', xfx, space, 'instanceof').
js_op('post++', xf, none, '++').
js_op('post--', xf, none, '--').
js_op('-', fx, none, '-').
js_op('!', fx, none, '!').
js_op('delete', fx, space, 'delete').
js_op('typeof', fx, space, 'typeof').

:- static op_kind_arity/2.
op_kind_arity(xfx, 2).
op_kind_arity(fx, 1).
op_kind_arity(xf, 1).

:- static no_braces/1.
% when we can avoid curly braces in emitted native code
no_braces(X) :- var(X), !, fail.
no_braces(if(_,_,_)).

% TODO: Add operator priority instead
% Add parenthesis if necessary
paren(X) := ( needs_paren(X) ? paren(X) | X ).
needs_paren(new(_,_)).
needs_paren(binary_op(_,_,_)). % TODO: this is binary_op
needs_paren(op(_,_)).
needs_paren(instanceof(_,_)).

{
:- fluid o :: token_writer.
% TODO: Many identifers here, simplify (move to jsgen)
js_id(id_l(Depth, X)) :- !, tj('$l'), t(Depth), jtj('_'), js_id(X). % for local ids
js_id(id_pred_entry(X)) :- !, tj('p$'), js_id(X).
js_id(id_s(X)) :- !, % (identifiers in the module system tables as strings)
	% TODO: kludge, escape in other way
	tj('"'),
	( X = id_functor(F,A) ->
	    t(inline(~escape_codes(~atom_codes(F)))), jtj('/'), t(A)
	; X = id_frame(id_functor(F,A), SaveArgs, Size) -> 
	    t(inline(~escape_codes(~atom_codes(F)))), jtj('/'), t(A), jtj('f'),
	    ( SaveArgs = yes -> jtj('x') ; true ), t(Size)
	; X = id_mod(M) ->
	    t(inline(~escape_codes(~atom_codes(M)))), jtj('/'), jtj('m')
	; js_id(X) % TODO: emit error?
	),
	jt('"').
js_id(id_trait(X)) :- !, tj('ta$'), js_id(X). % traits (for custom definitions)
js_id(id_natcode(X)) :- !, js_id(X), jt('$n').
js_id(id_frame(X, SaveArgs, Size)) :- !,
	js_id(X), jtj('$f'),
	( SaveArgs = yes -> jtj('x') ; true ), t(Size).
js_id(id_functor(F,A)) :- !,
	t(~encode_symbol_a(F/A)).
js_id(id_bb(X, N)) :- !, js_id(X), jtj('$b'), t(N).
js_id(id_mod(X)) :- !, % identifiers for classes/modules (Prolog side)
	tj('M'),
	( X == '\6\root' ->
	    t('__ROOT__')
	; t(~encode_symbol_a(X))
	).
js_id(id_param(N)) :- !, tj('z$'), t(N). % parameters (for traits)
js_id(id_cargmem(N)) :- !, tj('a'), t(N).
:- if(\+ defined(strarg_array)).
js_id(id_strarg(N)) :- !, tj('a'), t(N).
:- endif.
js_id(id_x(N)) :- !, tj('x'), t(N).
js_id(id_y(N)) :- !, tj('y'), t(N).
js_id(id_att(N)) :- !,
	tj('a$'),
	t(~encode_symbol_a(N)).
js_id(X) :- atom(X), !, t(X).
js_id(X) :-
	bug(invalid_id(X)), fail.
}.

is_js_id(X) :- atom(X), !.
is_js_id(id_l(_,_)).
is_js_id(id_pred_entry(_)).
is_js_id(id_s(_)).
is_js_id(id_trait(_)).
is_js_id(id_natcode(_)).
is_js_id(id_frame(_,_,_)).
is_js_id(id_functor(_,_)).
is_js_id(id_bb(_,_)).
is_js_id(id_mod(_)).
is_js_id(id_param(_)).
:- if(\+ defined(strarg_array)).
is_js_id(id_strarg(_)).
:- endif.
is_js_id(id_cargmem(_)).
is_js_id(id_x(_)).
is_js_id(id_y(_)).
is_js_id(id_att(_)).

encode_symbol_a(X) := ~encode_symbol_a(X, enc_js_id).

