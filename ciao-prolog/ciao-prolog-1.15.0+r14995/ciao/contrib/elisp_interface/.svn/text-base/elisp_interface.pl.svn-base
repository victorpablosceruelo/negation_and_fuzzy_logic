:- module(elisp_interface, [], [dcg, fsyntax, assertions, regtypes, isomodes]).

:- doc(title, "Minimal elisp Interface to Emacs").
:- doc(author, "Jose F. Morales").

:- doc(module, "A miminal interface to Emacs. Currently, it supports:

   @begin{itemize}
   @item invokation and definition of custom elisp code from Prolog
   @item a primitive event-based loop for interaction through widgets
     (e.g., buttons)
   @end{itemize}

   See @tt{examples/test.pl} for an example of usage.

   @begin{alert}
   This is a proof-of-concept in a @bf{very alpha state}. Please, ask
   me if you are interested in completing it.
   @end{alert}
").

% ===========================================================================

:- use_module(library(read)).
:- use_module(library(write)).
:- use_module(library(strings)).

:- doc(section, "Interface to @tt{elisp}").

:- export(elisp_init/0).
:- pred elisp_init # "Initialize the elisp interface".
elisp_init :-
	prompt(_, '').

:- export(elisp_eval/2).
:- pred elisp_eval(+Expr, -Ret) :: elisp_expr * string # "Evaluates
   the elisp expression @var{Expr} and obtain its result in
   @var{Ret}".
elisp_eval(Expr) := Ret :-
	lispify(Expr, Str, []),
	% send to the emacs filter
	write_string(Str), nl,
	% wait for an answer
	get_line(Ret).
% TODO: we should parse Ret

:- export(elisp_eval/1).
:- pred elisp_eval(+Expr) :: elisp_expr # "Like
   @pred{elisp_eval}, but ignores the output".
elisp_eval(Expr) :- _ = ~elisp_eval(Expr).

:- export(elisp_event_wait/1).
elisp_event_wait := Ret :-
	write_string("SET_STATE_EVENT_WAIT"), nl,
	% wait for an answer
	get_line(Ret).
% TODO: we should parse Ret

% ---------------------------------------------------------------------------

:- export(elisp_defun/2).
:- pred elisp_defun(+Head, +Body) :: elisp_expr * elisp_expr
   # "Defines a function".
elisp_defun(Head, Body) :-
	Head =.. [Name|Args],
	elisp_eval(defun(Name, list(Args), seq(Body))).

% ---------------------------------------------------------------------------

:- doc(subsection, "From Terms to Lisp Expressions").

:- regtype elisp_expr/1.
elisp_expr(X) :- term(X). % TODO: define

:- pred lispify(+X, -Str, ?Str0) :: elisp_expr * string * string 
   # "@var{Str}-@var{Str0} is a string encoding the Lisp expression
     for @var{X}".

% Encode terms as Lisp expressions 
lispify(X) --> { var(X) }, !, { fail }.
lispify(X) --> { atom(X) }, !, name(X).
lispify(X) --> { number(X) }, !, number(X).
lispify(comment(Xs)) --> !, % for string comments
	lispify(string(Xs)).
lispify(string(Xs)) --> !,  % "..."
	lispify_string(Xs).      
lispify(list(Xs)) --> !,    % (x ... x)
	"(", lispify_args(Xs), ")". 
lispify(seq(Xs)) --> !,     % x ... x
	lispify_args(Xs).
lispify(quote(X)) --> !,    % 'x
	"'", lispify(X).
lispify(call(X)) --> { atom(X) }, !, % (x)
	"(", lispify(X), ")". 
lispify(X) -->              % (n x ... x)
	{ X =.. [N|Xs] },
	"(", name(N), " ", lispify_args(Xs), ")".

lispify_args([]) --> [].
lispify_args([X]) --> !, lispify(X).
lispify_args([X|Xs]) --> !, lispify(X), " ", lispify_args(Xs).

name(X) --> { atom_codes(X, Cs) }, codes(Cs).

number(X) --> { number_codes(X, Cs) }, codes(Cs).

codes([]) --> [].
codes([X|Xs]) --> [X], codes(Xs).

lispify_string(Xs) --> "\"", esc_codes(Xs), "\"".

% string-escaped codes
esc_codes([]) --> [].
esc_codes([X|Xs]) --> esc_code(X), esc_codes(Xs).

esc_code(0'") --> !, "\\\"".
esc_code(0'\\) --> !, "\\\\".
esc_code(X) --> [X].

% ===========================================================================

:- doc(bug, "We use standard input/output for communication with the
   emacs buffer. Using a socket or other stream would be nicer.").

:- doc(bug, "The same idea can be used to implement interfaces with
   other languages.").

