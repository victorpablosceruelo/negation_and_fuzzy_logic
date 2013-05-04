:- module(latexified_buffer, [], [dcg, fsyntax, assertions, regtypes, isomodes]).

:- doc(title, "LaTeXified Buffer with the elisp Interface").
:- doc(author, "Jose F. Morales").

:- doc(module, "This example inserts LaTeX formulas and TikZ pictures,
   described as Prolog terms, in an Emacs buffer. It uses the elisp
   interface.").

:- use_module(library(elisp_interface)).
:- use_module(.(latex_convert)).

:- export(main/0).
main :-
	elisp_init,
	elisp_eval(call('clear-image-cache')),
	latex_cache_reset,
	% Define some buttons
	layout,
%	hello_world,
	% Enter the event loop,
	event_loop.

layout :-
	% Define the button text
	elisp_defun('bhandler'(button), [
          'ciao-elisp-event'('button-label'(button))
        ]),
	% 
%	FaceName = 'ciao-elisp-button',
	FaceName = 'underline',
%	elisp_eval(defface(FaceName,
%	             quote(':background'(string("blue3"))),
%		     string("Face for buttons"),
%		     ':group'(quote('ciao-elisp-faces')))),
	% Define the button type
	elisp_eval('define-button-type'(
          quote('my-button'),
	  quote('action'), quote('bhandler'),
	  quote('face'), quote(FaceName),
	  quote('mouse-face'), quote(FaceName),
	  quote('follow-link'), t,
	  quote('help-echo'), string("Click button"))),
	% Insert the button
        elisp_eval('insert-text-button'(string("Insert new"), ':type', quote('my-button'))),
	elisp_insert("\n").

%:- export(elisp_insert/1).
:- pred elisp_insert(+Msg) :: string # "Invokes @tt{(insert Msg)}".
elisp_insert(Msg) :-
	elisp_eval(insert(string(Msg))).

% ---------------------------------------------------------------------------

event_loop :-
	Event = ~elisp_event_wait,
	( Event = "\"Insert new\"" ->
	    insert_new
	; true % ignore
	),
	event_loop.

:- data level/1.
set_level(N) :- retractall_fact(level(_)), assertz_fact(level(N)).
get_level(N) :- ( level(N0) -> N = N0 ; N = 0 ).

:- use_module(library(system)).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(format), [sformat/3]).

:- export(insert_new/0).
insert_new :-
	get_level(N0),
	N is N0 + 1,
	set_level(N),
%	math_to_string(sup(n,'\'')=frac(x,sqrt(sup(x,2)+sup(y,2))), Formula, []),
	I is N0 mod 6,
	( I = 0 ->
	    Latex = math(apply(sub('\\mathsf{d}',N), 'x,y')=sqrt(N,sup(x,N)+sup(y,N)))
	; I = 1 ->
	    prolog_to_tikz(sqrt(sup(x,2)+sup(y,2)), PrologToTikz),
	    Latex = tikz(PrologToTikz)
	; I = 2 ->
	    prolog_to_tikz(app([],[1,2,3]), PrologToTikz),
	    Latex = tikz(PrologToTikz)
	; I = 3 ->
	    prolog_to_tikz(app([1],[2,3]), PrologToTikz),
	    Latex = tikz(PrologToTikz)
	; I = 4 ->
	    prolog_to_tikz(app([1,2],[3]), PrologToTikz),
	    Latex = tikz(PrologToTikz)
	; I = 5 ->
	    prolog_to_tikz(app([1,2,3],[]), PrologToTikz),
	    Latex = tikz(PrologToTikz)
%	  Latex = tikz(
%            tikzpicture(['level/.style={sibling distance=60mm/#1}'], [
%   	        node(['circle,draw'], 'hello', [
%   	          child(node(['circle,draw'], '2', [])),
%   	          child(node(['circle,draw'], '3', []))
%                ])
%            ]))
	),
	insert_latex(Latex).

insert_latex(Latex) :-
	latex_to_png(Latex, Png),
	cache_dir(CacheDir),
	working_directory(Curr, Curr),
	FullPng = ~atom_concat([Curr, '/', CacheDir, '/', Png]),
	sformat(LatexS, "~w", [Latex]),
	elisp_eval(setq(myva, 'create-image'(string(~atom_codes(FullPng)), '\'png nil :ascent \'center)'))),
	elisp_eval('save-excursion'(
		     'goto-char'(call('point-max')),
		     'insert'(string("   ")),
		     'insert'(string(LatexS)),
		     'insert'(string("\n")),
		     'insert'(string("   ")),
		     'insert-image'(myva),
		     'insert'(string("\n\n")))).
% (image-size myva)
% (insert-image myva)
% (put-image myva (point))
% (remove-images 0 (point))

% "    \\node [circle,draw] {hello}\n"||
% "      child {\n"||
% "        node [circle,draw,fill=red] {2}\n"||
% "            child {node [circle,draw] {1}}\n"||
% "            child {node [circle,draw] {3}}\n"||
% "      }\n"||
% "      child {
% "        node [circle,draw] {6}\n"||
% "        child {node [circle,draw] {5}}\n"||
% "        child {node [circle,draw] {9}\n"||
% "          child {node [circle, draw] {7}} \n"||
% "          child [missing]\n"||
% "        }\n"||
% "      };\n"||

% Example: draw the tree from a term
prolog_to_tikz(X) := Y :-
	prolog_to_tikz_(X, Node),
	Y = tikzpicture(['level/.style={sibling distance=60mm/#1}'], [
   	        Node
            ]).

prolog_to_tikz_(X, Node) :-
	( number(X) ->
	    N = X, As = [], Opts = 'circle,draw,fill=LightSalmon1'
	; atom(X) ->
	    N = X, As = [], Opts = 'circle,draw'
	; X =.. [N|As], Opts = 'circle,draw,fill=MistyRose1'
	),
	Node = node([Opts], N, Children),
	prolog_to_tikz__(As, Children).

prolog_to_tikz__([], []).
prolog_to_tikz__([X|Xs], [Y|Ys]) :-
	prolog_to_tikz_(X, X1),
	Y = child(X1),
	prolog_to_tikz__(Xs, Ys).

% (clear-image-cache)
% (setq myva (create-image "~/Documents/svn/ciao-n/ciao/contrib/elisp_interface/examples/temp/out.png" 'png))
% (image-size myva)
% (insert-image myva)
% (put-image myva (point))
% (remove-images 0 (point))

