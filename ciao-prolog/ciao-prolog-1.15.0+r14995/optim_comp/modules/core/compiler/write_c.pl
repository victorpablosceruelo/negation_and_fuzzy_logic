:- module(_, [], [compiler(complang)]).

:- doc(title, "Writter of C programs from AST representation").
:- doc(author, "Jose F. Morales").

:- doc(module, "This module provides predicates for writing C programs
	from their AST representation.").

:- doc(bug, "Write the regular type for the AST of C programs so that
	it can appear in the documentation").

:- doc(bug, "Add a layer to include methods to store type definitions,
       generate prototypes, etc.?").

:- use_module(compiler(errlog)).
:- use_module(compiler(list_common), [list_to_sum/2]).

% -----------------------------------------------------------------------------

:- use_module(library(streams)).
:- use_module(compiler(open_and_protect)).

:- public class c_writer {
    :- attr name_counter :: m_int.

    :- constructor new_/0.
    new_ :- ~name_counter = 0.

    :- public to_stream/2.
%    :- pred to_stream(+Stream, +Code) :: stream * c_tree
%    # "It writes the @em{C} code represented by @var{Code} in
%       @var{Stream}.".
    to_stream(Stream, Code) :-
    	current_output(OldOutput),
    	set_output(Stream),
    	Ok = ( to_stream_2(Code) ? yes | no ),
    	set_output(OldOutput),
    	Ok = yes.

    :- public name_variables/2.
    % Instantiate fresh variables in Code with names prefixed by Module
    name_variables(Code, Module) :-
    	name_variables_1(Code, ~encode_symbol_a(Module, enc_c_id)).

    name_variables_1(X, _Module) :- type(X, attv), !. % note: do nothing with attributed variables
    name_variables_1(X, Module) :- var(X), !,
    	X = q(Module, ~name_counter),
     	name_counter.inc(1).
    name_variables_1(X, Module) :- functor(X, _F, A), !,
    	name_variables_2(1, A, X, Module).
    name_variables_1(_, _).

    name_variables_2(I, N, X, Module) :-
    	I =< N, !,
    	I1 is I + 1,
    	arg(I, X, A),
    	name_variables_1(A, Module),
    	name_variables_2(I1, N, X, Module).
    name_variables_2(_, _, _, _). 

    :- public to_file/3.
    to_file(Name, Module, Code) :-
        open_and_protect(Name, OutputStream, Ref),
	name_variables(Code, Module),
	to_stream(OutputStream, Code),
        close(OutputStream),
        end_protect(Ref).
}.

% -----------------------------------------------------------------------------

to_stream_2([]) :- !.
to_stream_2([X|Xs]) :-
    	to_stream_3(X),
    	to_stream_2(Xs).

to_stream_3(X) :-
	o :: token_writer <- ~token_writer.new,
	( to_stream_4(X) ->
	    true
	; errlog:bug(['to_stream_4 failed for ', X]), fail
	),
	o.newline, o.flush_pending.

{
% TODO: Share with write_js.pl
:- fluid o :: token_writer.

% Emit a token
t(X) :- o.token(X).
tj(X) :- o.token(X), o.glue. % no space after
jt(X) :- o.glue, o.token(X). % no space before
jtj(X) :- o.glue, o.token(X), o.glue. % no space around
}.

{
:- fluid o :: token_writer.

to_stream_4(X) :- is_declaration(X), !,
	declaration(X).
to_stream_4(X) :-
	directive(X).

:- '$ctxprj'(is_declaration/1, []).
is_declaration(declare(_, _)).
is_declaration(declare(_, _, _)).
is_declaration(declare_type(_, _)).
is_declaration(declare_union(_, _)).
is_declaration(declare_struct(_, _)).

declaration(declare_type(Name, union(Name2))) :- !,
	declaration(declare(Name, typedef+union_id(Name2))).
declaration(declare_type(Name, struct(Name2))) :- !,
	declaration(declare(Name, typedef+struct_id(Name2))).
declaration(declare_union(Name, Decls)) :- !,
	declaration(declare('$empty', union(Name, Decls))).
declaration(declare_struct(Name, Decls)) :- !,
	declaration(declare('$empty', struct(Name, Decls))).
declaration(declare(Name, Type0, lambda(InNames, Code))) :-
	split_specifiers(Type0, StorageSpecifiers, AsmSpecifiers, Type),
	Type = function(InTypes, OutType),
	!,
	declare_function(Name, StorageSpecifiers, AsmSpecifiers, InTypes, OutType, InNames, Code).
declaration(declare(Name, Type, Value)) :- !,
	vdecl(Name, Type),
	t('='),
	variable_initializer(Value),
	jt(';').
declaration(declare(Name, Type)) :- !,
	vdecl(Name, Type),
	jt(';').
declaration(declare_type(Name, Type)) :- !,
	vdecl(Name, typedef+Type),
	jt(';').

declare_function(Name, StorageSpecifiers, AsmSpecifiers, InTypes, OutType, InNames, Code) :- !,
	zip_decls(InTypes, InNames, InDecls),
	Type = function_args(InDecls, OutType),
	vdecl0(Name, StorageSpecifiers, AsmSpecifiers, Type),
	% TODO: function code must be in a block (document, and move block conversion to user code)
	Code2 = ( Code = block(_) ? Code | block(Code) ),
	block(Code2).

% mix type with names
:- '$ctxprj'(zip_decls/3, []).
zip_decls([], [], []).
zip_decls([T|Ts], [N|Ns], [declare(N, T)|Ds]) :-
	zip_decls(Ts, Ns, Ds).

% Declaration of variables and functions
vdecl(Name, Type0) :-
	split_specifiers(Type0, StorageSpecifiers, AsmSpecifiers, Type),
	vdecl0(Name, StorageSpecifiers, AsmSpecifiers, Type).

vdecl0(Name, StorageSpecifiers, AsmSpecifiers, Type) :-
	% storage specifiers
	specifier_list(StorageSpecifiers),
	% reverse declaration
	revdecl(Type, Name, Rev),
	revdecl_prior(revdecl, Rev),
	% asm specifiers
	specifier_list(AsmSpecifiers).

:- '$ctxprj'(revdecl/3, []).
revdecl(Type0, Rev0, Rev) :-
	revdecl__2(Type0, Rev0, Type1, Rev1), !,
	revdecl(Type1, Rev1, Rev).
revdecl(Type, Rev, rev_var(Type, Rev)).

:- '$ctxprj'(revdecl__2/4, []).
revdecl__2(bitfield(Bits, Type), Rev, Type, rev_bitfield(Rev, Bits)) :- !.
revdecl__2(array(Type), Rev, Type, rev_array(Rev)) :- !.
revdecl__2(array(Type, Size), Rev, Type, rev_array(Rev, Size)) :- !.
revdecl__2(function(X, Type), Rev, Type, rev_function(Rev, X)) :- !.
revdecl__2(function_args(X, Type), Rev, Type, rev_function_args(Rev, X)) :- !.
revdecl__2(Type0, Rev, Type, rev_pointer(rev_var(S, Rev))) :-
	get_pointertype(Type0, S, Type).

:- '$ctxprj'(get_pointertype/3, []).
get_pointertype(X + Y, Y, Type) :-
	X = pointer(Type), !.
get_pointertype(X, '$empty', Type) :-
	X = pointer(Type), !.
get_pointertype(X + Y, X + Z, Type) :- !, 
	get_pointertype(Y, Z, Type).

:- '$ctxprj'(split_specifiers/4, []).
split_specifiers(Type, StorageSpecifiers, AsmSpecifiers, Z) :-
	split_specifiers__2(Type, StorageSpecifiers, [], AsmSpecifiers, [], Z0, []),
	( list_to_sum(Z0, Z) -> true ; Z = '$empty' ).

:- '$ctxprj'(split_specifiers__2/7, []).
split_specifiers__2(Xa + Xb, Y, Y0, Z, Z0, W, W0) :- !,
	split_specifiers__2(Xa, Y, Y1, Z, Z1, W, W1),
	split_specifiers__2(Xb, Y1, Y0, Z1, Z0, W1, W0).
split_specifiers__2(X, Y, Y0, Z, Z0, W, W0) :-
	( is_storage_specifier(X) ->
	    Y = [X|Y0],
	    Z = Z0,
	    W = W0
	; is_asm_specifier(X) ->
	    Y = Y0,
	    Z = [X|Z0],
	    W = W0
	; Y = Y0,
	  Z = Z0,
	  W = [X|W0]
	).

:- '$ctxprj'(is_storage_specifier/1, []).
is_storage_specifier(typedef).
is_storage_specifier(extern).
is_storage_specifier(static).
is_storage_specifier(auto).
is_storage_specifier(register).
is_storage_specifier(inline).

:- '$ctxprj'(is_asm_specifier/1, []).
is_asm_specifier(asm(_)).

% type expression
type_exp(Type) :- vdecl('$empty', Type).

vdecl_args(at_least(X)) :- !,
	vdecl_list(X), jt(','), t('...').
vdecl_args(X) :-
	vdecl_list(X).

vdecl_list([]) :- !.
vdecl_list([X]) :- !, vdecld(X).
vdecl_list([X|Xs]) :- vdecld(X), jt(','), vdecl_list(Xs).

vdecld(declare(Name, Type)) :-
	vdecl(Name, Type).

type_exp_list(at_least(X)) :- !,
	type_exp_list__2(X), jt(','), t('...').
type_exp_list(X) :-
	type_exp_list__2(X).

type_exp_list__2([]) :- !.
type_exp_list__2([X]) :- !, type_exp(X).
type_exp_list__2([X|Xs]) :- type_exp(X), jt(','), type_exp_list__2(Xs).

enumerator(X # Y) :- !,
	identifier(X), t('='), expr_prior(conditional, Y).
enumerator(X) :-
	identifier(X).

variable_initializer(aggregate(X)) :- !,
	aggregate_initializer_list(X).
variable_initializer(X) :-
	expr_prior(assignment, X), !.

% TODO: split aggregate for arrays and structures
aggregate_initializer_list(X) :-
	tj('{'),
	aggregate_initializer_list__2(X),
	jt('}').

aggregate_initializer_list__2([X]) :- !,
	aggregate_initializer(X).
aggregate_initializer_list__2([X|Xs]) :-
	aggregate_initializer(X), jt(','), aggregate_initializer_list__2(Xs).

aggregate_initializer(X # Y) :- !,
	designator(X), t('='), variable_initializer(Y).
aggregate_initializer(X) :-
	variable_initializer(X).

designator(element(Index)) :- !,
	tj('['), expr_prior(conditional, Index), jt(']').
designator(member(Member)) :-
	tj('.'), identifier(Member).

:- '$ctxprj'(is_block/1, []).
is_block(block(_)).

block(block(X)) :- block__2(X).

block__2([]) :- !,
	t('{'), t('}').
block__2(X) :-
	t('{'), o.inner, o.newline,
	statement_or_declaration_list(X),
	o.outer, o.newline, t('}').

statement_or_declaration_list([X]) :- !, statement_or_declaration(X).
statement_or_declaration_list([X|Xs]) :- statement_or_declaration(X), o.newline, statement_or_declaration_list(Xs).

statement_or_declaration(X) :- is_declaration(X), !,
	declaration(X).
statement_or_declaration(X) :-
	statement(X).

enumerator_list(X) :-
	tj('{'),
	enumerator_list__2(X),
	jt('}').

enumerator_list__2([X]) :- !, enumerator(X).
enumerator_list__2([X|Xs]) :- enumerator(X), jt(','), enumerator_list__2(Xs).

struct_variable_declaration_list(X) :-
	t('{'), o.inner, o.newline,
	struct_variable_declaration_list__2(X),
	o.outer, o.newline, t('}').

struct_variable_declaration_list__2([X]) :- !, struct_variable_declaration(X).
struct_variable_declaration_list__2([X|Xs]) :- struct_variable_declaration(X), o.newline, struct_variable_declaration_list__2(Xs).

struct_variable_declaration(declare(Name, Type)) :- !,
	vdecl(Name, Type),
	jt(';').

argument_expression_list([]) :- !.
argument_expression_list([X]) :- !, assignment_expression_item(X).
argument_expression_list([X|Xs]) :- assignment_expression_item(X), jt(','), argument_expression_list(Xs).

assignment_expression_item(X) :-
	expr_prior(assignment, X).

% priority(Id, Priority).
:- '$ctxprj'(priority/2, []).
priority(expr, 170).
priority(assignment, 160).
priority(conditional, 150).
%
priority(logical_OR, 140).
priority(logical_AND, 130).
priority(inclusive_OR, 120).
priority(exclusive_OR, 110).
priority(bitwise_AND, 100).
priority(equality, 90).
priority(relational, 80).
priority(shift, 70).
priority(additive, 60).
priority(multiplicative, 50).
priority(unary, 30).
priority(postfix, 20).
priority(primary, 10).
%
priority(revdecl, 20).
priority(arraydecl, 10).

% ---------------------------------------------------------------------------
% expr rules

:- '$ctxprj'(expr_kind/2, []).
:- discontiguous expr_kind/2.
:- discontiguous expr_rule/1.

expr(E) :- expr_prior(expr, E).

expr_prior(ExpectedPrec, E) :-
	functor(E, N0, 2),
	bin_op(N0, Prec0, Token, Assoc),
	!,
	prior_number(Prec0, Prec),
	op_ass(Assoc, PrecA, Prec, PrecB, in),
	%
	arg(1, E, A),
	arg(2, E, B),
	begin_quote(ExpectedPrec, Prec, Quoted),
	expr_prior(PrecA, A), t(Token),
	( Quoted = yes ->
	    expr_prior(PrecB, B), jt(')')
	; expr_prior(PrecB, B)
	).
expr_prior(ExpectedPrec, E) :-
	functor(E, N0, 1),
	unary_op(N0, Prec0, Token, Assoc),
	!,
	prior_number(Prec0, Prec),
	op_ass(Assoc, _, Prec, PrecB, pre),
	arg(1, E, B),
	begin_quote(ExpectedPrec, Prec, Quoted),
	tj(Token),
	( Quoted = yes ->
	    expr_prior(PrecB, B), jt(')')
	; expr_prior(PrecB, B)
	).
expr_prior(ExpectedPrec, E) :-
	expr_kind(E, ExprPrec),
	expr_prior0(ExpectedPrec, ExprPrec, E).

begin_quote(ExpectedPrec, Prec, Quoted) :-
	( less_priority(ExpectedPrec, Prec) ->
	    tj('('), Quoted = yes
	; Quoted = no
	).

expr_prior0(ExpectedPrec, ExprPrec, E) :-
	( less_priority(ExpectedPrec, ExprPrec) ->
	    tj('('), expr_rule(E), jt(')')
	; expr_rule(E)
	).

expr_kind((_, _), expr) :- !.
expr_rule((X, Y)) :- !, expr(X), jt(','), expr(Y).
%
expr_kind(_=_, assignment) :- !.
expr_rule(X=Y) :- !, expr_prior(unary, X), t('='), expr_prior(assignment, Y).
expr_kind(assign(_), assignment) :- !.
expr_rule(assign(X)) :- !,
	assign_expr(X, Y, Z, W),
	expr_prior(unary, Y), t(W), expr_prior(assignment, Z).
%
expr_kind(conditional(_, _, _), conditional) :- !.
expr_rule(conditional(X, Y, Z)) :- !, expr_prior(logical_OR, X), t('?'), expr(Y), t(':'), expr_prior(conditional, Z).
%
expr_kind(cast(_, _), unary) :- !.
expr_rule(cast(X, Y)) :- !, tj('('), type_exp(X), jtj(')'), expr_prior(unary, Y).
%
expr_kind(element(_, _), postfix) :- !.
expr_rule(element(Object, Index)) :- !, expr_prior(postfix, Object), jtj('['), expr(Index), jt(']').
expr_kind(call(_, _), postfix) :- !.
expr_rule(call(X, Y)) :- !, expr_prior(postfix, X), jtj('('), argument_expression_list(Y), jt(')').
expr_kind(member(_, _), postfix) :- !.
expr_rule(member(deref(Object), Member)) :- !, expr_prior(postfix, Object), jtj('->'), identifier(Member).
expr_rule(member(Object, Member)) :- !, expr_prior(postfix, Object), jtj('.'), identifier(Member).
expr_kind(post_inc(_), postfix) :- !.
expr_rule(post_inc(X)) :- !, expr_prior(postfix, X), jt('++').
expr_kind(post_dec(_), postfix) :- !.
expr_rule(post_dec(X)) :- !, expr_prior(postfix, X), jt('--').
expr_kind(aggregate(_, _), postfix) :- !.
expr_rule(aggregate(X, Y)) :- !, tj('('), type_exp(X), jtj(')'), aggregate_initializer_list(Y).
%
expr_kind(quoted(_), none) :- !.
expr_rule(quoted(X)) :- !, tj('('), expr(X), jt(')'). % note: extension (force parenthesis)
expr_kind(type_exp(_), none) :- !.
expr_rule(type_exp(X)) :- !, type_exp(X). % note: extended grammar (types as expressions)
expr_kind(block(_), none) :- !.
expr_rule(block(X)) :- !, block(block(X)). % note: extended grammar (blocks as expressions)
expr_kind(exprstat(_), none) :- !.
expr_rule(exprstat(X)) :- !, statement(X). % note: extended grammar (statements as expressions)
expr_kind(inline(_), none) :- !.
expr_rule(inline(X)) :- !, t(inline(X)). % note: extended grammar
expr_kind(string(_), none) :- !.
expr_rule(string(X)) :- !, string_literal(X).
expr_kind(X, none) :- is_identifier(X), !.
expr_rule(X) :- is_identifier(X), !, identifier(X).
expr_kind(X, none) :- number(X), !.
expr_rule(X) :- number(X), !, t(X).

:- '$ctxprj'(unary_op/4, []).
unary_op((++), unary, '++', fy).
unary_op((--), unary, '--', fy).
unary_op(address, unary, '&', fy).
unary_op(label_address, unary, '&&', fy).
unary_op(deref, unary, '*', fy).
unary_op((+), unary, '+', fy).
unary_op((-), unary, '-', fy).
unary_op((\), unary, '~', fy).
unary_op(logical_not, unary, '!', fy).

:- '$ctxprj'(bin_op/4, []).
bin_op(logical_or, logical_OR, '||', yfx).
bin_op(logical_and, logical_AND, '&&', yfx).
bin_op((\/), inclusive_OR, '|', yfx).
bin_op((#), exclusive_OR, '^', yfx).
bin_op((/\), bitwise_AND, '&', yfx).
bin_op((==), equality, '==', yfx).
bin_op((\==), equality, '!=', yfx).
bin_op((<), relational, '<', yfx).
bin_op((>), relational, '>', yfx).
bin_op((=<), relational, '<=', yfx).
bin_op((>=), relational, '>=', yfx).
bin_op((<<), shift, '<<', yfx).
bin_op((>>), shift, '>>', yfx).
bin_op((+), additive, '+', yfx).
bin_op((-), additive, '-', yfx).
bin_op((*), multiplicative, '*', yfx).
bin_op((/), multiplicative, '/', yfx).
bin_op((mod), multiplicative, '%', yfx).

:- '$ctxprj'(assign_expr/4, []).
assign_expr((X * Y), X, Y, '*=').
assign_expr((X / Y), X, Y, '/=').
assign_expr((X mod Y), X, Y, '%=').
assign_expr((X + Y), X, Y, '+=').
assign_expr((X - Y), X, Y, '-=').
assign_expr((X << Y), X, Y, '<<=').
assign_expr((X >> Y), X, Y, '>>=').
assign_expr((X /\ Y), X, Y, '&=').
assign_expr((X # Y), X, Y, '^=').
assign_expr((X \/ Y), X, Y, '|=').

% TODO: duplicated from library(operators)
:- '$ctxprj'(op_ass/5, []).
op_ass(fy, 0, Prec, Prec, pre).
op_ass(fx, 0, Prec, Less, pre) :- Less is Prec-1.
op_ass(yfx, Prec, Prec, Less, in) :- Less is Prec-1.
op_ass(xfy, Less, Prec, Prec, in) :- Less is Prec-1.
op_ass(xfx, Less, Prec, Less, in) :- Less is Prec-1.
op_ass(yf, Prec, Prec, 0, post).
op_ass(xf, Less, Prec, 0, post) :- Less is Prec-1.

% ---------------------------------------------------------------------------
% revdecl rules

:- '$ctxprj'(revdecl_kind/2, []).
:- discontiguous revdecl_kind/2.
:- discontiguous revdecl_rule/1.

revdecl_prior(ExpectedPrec, X) :- !,
	revdecl_kind(X, ExprPrec),
	( less_priority(ExpectedPrec, ExprPrec) ->
	    tj('('), revdecl_rule(X), jt(')')
	; revdecl_rule(X)
	).

% note: bitfield is only valid in structures
revdecl_kind(rev_bitfield(_, _), revdecl) :- !.
revdecl_rule(rev_bitfield(Rev, Bits)) :- !, revdecl_prior(revdecl, Rev), jtj(':'), expr_prior(conditional, Bits).
revdecl_kind(rev_pointer(_), revdecl) :- !.
revdecl_rule(rev_pointer(Rev)) :- !, tj('*'), revdecl_prior(revdecl, Rev).
revdecl_kind(rev_var(_, _), revdecl) :- !.
revdecl_rule(rev_var(Specifiers, Rev)) :- !, specifier_sum(Specifiers), revdecl_prior(revdecl, Rev).
revdecl_kind(rev_function_args(_, _), revdecl) :- !.
revdecl_rule(rev_function_args(Rev, X)) :- !, revdecl_prior(arraydecl, Rev), jtj('('), vdecl_args(X), jt(')').
revdecl_kind(rev_function(_, _), revdecl) :- !.
revdecl_rule(rev_function(Rev, X)) :- !, revdecl_prior(arraydecl, Rev), jtj('('), type_exp_list(X), jt(')').
%
revdecl_kind(rev_array(_), arraydecl) :- !.
revdecl_rule(rev_array(Rev)) :- !, revdecl_prior(arraydecl, Rev), jt('['), jt(']').
revdecl_kind(rev_array(_, _), arraydecl) :- !.
revdecl_rule(rev_array(Rev, Size)) :- !, revdecl_prior(arraydecl, Rev), jtj('['), expr_prior(assignment, Size), jt(']').
revdecl_kind('$empty', none) :- !.
revdecl_rule('$empty') :- !.
revdecl_kind(Rev, none) :- is_identifier(Rev), !.
revdecl_rule(Rev) :- is_identifier(Rev), !, identifier(Rev).

% Specifier rules
% note: it also includes storage class specifiers and type qualifiers
specifier(struct_id(X)) :- !, t(struct), identifier(X).
specifier(struct(X)) :- !, t(struct), struct_variable_declaration_list(X).
specifier(struct(X, Y)) :- !, t(struct), identifier(X), struct_variable_declaration_list(Y).
specifier(union_id(X)) :- !, t(union), identifier(X).
specifier(union(X)) :- !, t(union), struct_variable_declaration_list(X).
specifier(union(X, Y)) :- !, t(union), identifier(X), struct_variable_declaration_list(Y).
specifier(enum_id(X)) :- !, t(enum), identifier(X).
specifier(enum(X)) :- !, t(enum), enumerator_list(X).
specifier(enum(X, Y)) :- !, t(enum), identifier(X), enumerator_list(Y).
% extended grammar: asm specifier
specifier(asm(X)) :- !, t(asm), jtj('('), expr_prior(assignment, X), jt(')').
specifier(X) :- !, t(X).

% Statement rules
statement(comment(String)) :- !, t('/*'), t(inline(String)), t('*/').
statement(comment_msg(X)) :- !, t('/*'), t(msg(X)), t('*/').
statement(default(X)) :- !, o.outer, t(default), jt(':'), o.inner, o.newline, statement(X).
statement(label(X)) :- !, o.outer, identifier(X), jt(':'), o.inner.
statement(case(X, Y)) :- !, o.outer, t(case), expr_prior(conditional, X), jt(':'), o.inner, o.newline, statement(Y).
statement(if(X, Y)) :- !, t(if), tj('('), expr(X), jt(')'), statement(Y).
statement(if(X, Y, Z)) :- !, t(if), tj('('), expr(X), jt(')'), statement(Y), t(else), statement(Z).
statement(switch(X, Y)) :- !, t(switch), jtj('('), expr(X), jt(')'), block(block(Y)).
statement(while(X, Y)) :- !, t(while), tj('('), expr(X), jt(')'), statement(Y).
statement(do_while(X, Y)) :- !, t(do), statement(X), t(while), tj('('), expr(Y), jt(')'), jt(';').
statement(for(X, Y, Z, W)) :- !, t(for), tj('('), expr(X), jt(';'), expr(Y), jt(';'), expr(Z), jt(')'), statement(W).
statement(for2(Y, Z, W)) :- !, t(for), t('('), jt(';'), expr(Y), jt(';'), expr(Z), jt(')'), statement(W).
statement(goto(X)) :- !, t(goto), identifier(X), jt(';').
statement(continue) :- !, t(continue), jt(';').
statement(break) :- !, t(break), jt(';').
statement(return) :- !, t(return), jt(';').
statement(return(X)) :- !, t(return), expr(X), jt(';').
statement(X) :- is_block(X), !, block(X).
statement(X) :- !, expr(X), jt(';').

% Directive rules (preprocesor directives, comments, etc.)
directive(comment(String)) :- !, t('/*'), t(inline(String)), t('*/').
directive(comment_msg(X)) :- !, t('/*'), t(msg(X)), t('*/').
directive('@nl') :- !, true.
directive(local_include(X)) :- !, t('#include'), tj('"'), expr(X), jt('"').
directive(include(X)) :- !, t('#include'), tj('<'), expr(X), jt('>').
directive(define_void(X)) :- !, t('#define'), expr_prior(postfix, X).
directive(define(X, Y)) :- !, t('#define'), expr_prior(postfix, X), o.begmacro, expr_prior(primary, Y), o.endmacro.
directive(macro(X, Y)) :- !, t('#define'), expr_prior(postfix, X), o.inner, o.begmacro, o.newline, statement_or_declaration(Y), o.outer, o.endmacro.
directive(undefine(X)) :- !, t('#undef'), expr_prior(postfix, X).
directive(ifndef(X)) :- !, t('#if'), t('!'), jt('defined'), jtj('('), expr(X), jt(')').
directive(endif) :- !, t('#endif').
directive(inline(X)) :- !, t(inline(X)). % note: extended grammar

% A has less priority than B
:- '$ctxprj'(less_priority/2, []).
less_priority(A, B) :-
	Ap = ~prior_number(A),
	Bp = ~prior_number(B),
	Ap < Bp.

:- '$ctxprj'(prior_number/2, []).
prior_number(A) := ( number(A) ? A | ~priority(A) ).

% Write an identifier
identifier(X) :- is_identifier(X), t(X).

:- '$ctxprj'(is_identifier/1, []).
is_identifier(X) :- atom(X), !.
is_identifier(q(_, _)) :- !. % note: extension (qualified names)
is_identifier(p(_, _)) :- !. % note: extension (prefixed names)
is_identifier(type(_)) :- !. % note: extension (type names)

string_literal(X) :- t(X).

% Emit a specifier
specifier_sum(X+Y) :- !, specifier_sum(X), specifier_sum(Y).
specifier_sum('$empty') :- !.
specifier_sum(X) :- specifier(X).

specifier_list([]).
specifier_list([X|Xs]) :- specifier(X), specifier_list(Xs).

% Emit a type
emit_type(pointer(Type)) :- !,
	t(Type), t('*').
emit_type(array(Type)) :- !,
	t(Type), t('['), jt(']').
emit_type(Type) :-
	t(Type).
}.

% ---------------------------------------------------------------------------

:- use_module(compiler(write_c_common)).

:- public encode_symbol_a/2.
encode_symbol_a(X) := ~encode_symbol_a(X, enc_c_id).
